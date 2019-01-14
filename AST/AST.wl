BeginPackage["AST`"]

ParseString::usage = "ParseString[string] returns an AST by interpreting string as WL input. \
Note: If there are multiple expressions in string, then only the last expression is returned. \
ParseString[string, h] wraps the output with h and allows multiple expressions to be returned."

ParseFile::usage = "ParseFile[file] returns an AST by interpreting file as WL input."

TokenizeString::usage = "TokenizeString[string] returns a list of tokens by interpreting string as WL input."

TokenizeFile::usage = "TokenizeFile[file] returns a list of tokens by interpreting file as WL input."


ConcreteParseString::usage = "ConcreteParseString[string] returns a concrete syntax tree by interpreting string as WL input."

ConcreteParseFile::usage = "ConcreteParseFile[file] returns a concrete syntax tree by interpreting file as WL input."




ToInputFormString::usage = "ToInputFormString[concrete] returns a string representation of concrete."
ToFullFormString::usage = "ToFullFormString[abstract] returns a string representation of abstract."

ToNode
FromNode

DeclarationName



PrefixOperatorToSymbol
PostfixOperatorToSymbol
BinaryOperatorToSymbol
InfixOperatorToSymbol
TernaryOperatorsToSymbol
GroupOpenerToSymbol
GroupOpenerToCloser
GroupCloserToSymbol
GroupOpenerToMissingCloserSymbol
GroupCloserToMissingOpenerSymbol


SymbolToPrefixOperatorString
SymbolToPostfixOperatorString
SymbolToBinaryOperatorString
SymbolToInfixOperatorString
SymbolToTernaryOperatorString
SymbolToGroupPair
SymbolToTernaryPair



(*

There are some System symbols that are only created when expressions are parsed:

e.g., HermitianConjugate and ImplicitPlus are System symbols that do not exist until expressions
are parsed:

In[1]:= Names["HermitianConjugate"]
Out[1]= {}
In[2]:= ToExpression["a\\[HermitianConjugate]",InputForm,Hold]//FullForm
Out[2]//FullForm= Hold[ConjugateTranspose[a]]
In[3]:= Names["HermitianConjugate"]
Out[3]= {HermitianConjugate}

In[1]:= Names["ImplicitPlus"]
Out[1]= {}
In[2]:= ToExpression["a\\[ImplicitPlus]b",InputForm,Hold]//FullForm
Out[2]//FullForm= Hold[Plus[a,b]]
In[3]:= Names["ImplicitPlus"]
Out[3]= {ImplicitPlus}

These are not documented symbols, so they are apparently side-effects of parsing.

We want to avoid any confusion about this, so we introduce our own symbols here:
AST`PostfixHermitianConjugate and AST`BinaryImplicitPlus

Some examples of these System symbols that are introduced only after parsing:
HermitianConjugate
ImplicitPlus
InvisiblePrefixScriptBase
InvisiblePostfixScriptBase

*)

(*Token*)
Token


(*Character*)
WLCharacter


(* atom symbols *)
(*InternalEmpty*)
OptionalDefault
PatternBlank
PatternBlankSequence
PatternBlankNullSequence
OptionalDefaultPattern

(* operator symbols *)
PrefixLinearSyntaxBang
PrefixInvisiblePrefixScriptBase

PostfixHermitianConjugate
PostfixInvisiblePostfixScriptBase

BinarySlashSlash
BinaryAt
BinaryInvisibleApplication
BinaryAtAtAt

InfixImplicitPlus
ImplicitTimes
InfixInvisibleTimes
InfixTimes

TernaryTilde
TernarySlashColon

(* group symbols *)
(*List*)
GroupMissingOpenerList
GroupMissingCloserList

(*Association*)
GroupMissingOpenerAssociation
GroupMissingCloserAssociation

(*AngleBracket*)
GroupMissingOpenerAngleBracket
GroupMissingCloserAngleBracket

(*Ceiling*)
GroupMissingOpenerCeiling
GroupMissingCloserCeiling

(*Floor*)
GroupMissingOpenerFloor
GroupMissingCloserFloor

GroupDoubleBracket
GroupMissingOpenerDoubleBracket
GroupMissingCloserDoubleBracket

GroupSquare
GroupMissingOpenerSquare
GroupMissingCloserSquare

(*BracketingBar*)
GroupMissingOpenerBracketingBar
GroupMissingCloserBracketingBar

(*DoubleBracketingBar*)
GroupMissingOpenerDoubleBracketingBar
GroupMissingCloserDoubleBracketingBar

GroupParen
GroupMissingOpenerParen
GroupMissingCloserParen

GroupLinearSyntaxParen
GroupMissingOpenerLinearSyntaxParen
GroupMissingCloserLinearSyntaxParen



(* option symbols *)
Source
(*
Used to report f[,] or "\[Alpa]" as an option, e.g. SyntaxIssues -> {SyntaxIssue[], SyntaxIssue[]}
*)
SyntaxIssues
SyntaxIssue




(* node symbols *)
SymbolNode
StringNode
NumberNode

BlankNode
BlankSequenceNode
BlankNullSequenceNode
OptionalDefaultNode
PatternBlankNode
PatternBlankSequenceNode
PatternBlankNullSequenceNode
OptionalDefaultPatternNode

SlotNode
SlotSequenceNode
OutNode
(*InternalEmptyNode*)
PrefixNode
BinaryNode
TernaryNode
InfixNode
PostfixNode
GroupNode
CallNode

(*
InternalTokenNode represents a token in a linear syntax expression
When parsing linear syntax expressions, all tokens are simply kept unparsed
*)
InternalTokenNode

InternalAllNode
InternalDotNode
InternalNullNode
InternalOneNode

(* InternalMinusNode stop-gap *)
InternalMinusNode

FileNode
HoldNode

SyntaxErrorNode
CallMissingCloserNode


InternalInvalid




Begin["`Private`"]

Needs["AST`Abstract`"]
Needs["AST`DeclarationName`"]
Needs["AST`Node`"]
Needs["AST`Symbol`"]
Needs["AST`ToInputFormString`"]
Needs["AST`ToFullFormString`"]
Needs["AST`Utils`"]
Needs["PacletManager`"]


$exe = Module[{wlastResources, firstPair, listOfWLASTs},
			(*
			PacletResources["Resource", "wl-ast"] will return something like:
			{{Paclet[AST,0.7,<>],{/path/to/AST/ASTResources/MacOSX-x86-64/wl-ast}}}

			or if AST is not yet installed (i.e., it is currently in the process of building),
			then PacletResources["Resource", "wl-ast"] will return {}

			*)
			wlastResources = PacletResources["Resource", "wl-ast"];
			Which[
				wlastResources == {},
					None
				,
				Length[wlastResources] != 1,
					(*
					more than one paclet has "wl-ast" resources
					TODO: Message or Throw
					*)
					None
				,
				firstPair = wlastResources[[1]];
				firstPair[[1]]["Name"] != "AST",
					None
				,
				listOfWLASTs = firstPair[[2]];
				Length[listOfWLASTs] != 1,
					(*
					more than one wl-ast in AST paclet
					TODO: Message or Throw
					*)
					None
				,
				True,
					listOfWLASTs[[1]]
			]
		]



ConcreteParseString[s_String, h_:Automatic] :=
	concreteParseString[s, h]

TokenizeString[s_String] :=
	concreteParseString[s, List, "Tokenize"->True]


Options[concreteParseString] = {
	"Tokenize" -> False
}

concreteParseString[sIn_String, h_, OptionsPattern[]] :=
Catch[
Module[{s = sIn, res, out, actualAST, multiBytes, tokenize},

	tokenize = OptionValue["Tokenize"];

	(*
	RunProcess cannot handle strings > 2^16.
	This may be a system limitation
	*)
	If[StringLength[sIn] > 2^16,
		Throw[Failure["InputTooLarge", <|"TruncatedInput"->StringTake[s, 100]|>]]
	];

	(*
	RunProcess cannot handle characters > 255 yet
	bug 360669
	*)
	multiBytes = Select[ToCharacterCode[s], # > 255&];
	If[!empty[multiBytes],
		Throw[Failure["MultiByteCharactersNotAllowed", <|
			"Input"->s,
			"MultiByteCharacters"->(AST`Utils`escapeString[FromCharacterCode[#]]& /@ Take[multiBytes, UpTo[10]])|>]]
	];

	If[$exe === None,
		Throw[Failure["ExecutableNotFound", <|"Executable"->$exe|>]]
	];

	res = RunProcess[{
		$exe,
		Sequence@@If[tokenize, {"-format", "tokens"}, {}],
		"-noPrompt",
		"-nonInteractive"
		}, All, s];

	If[FailureQ[res],
		Throw[res]
	];

	res = handleResult[res, h];

	If[FailureQ[res],
		Throw[res]
	];

	res
]]

(*
may return:
a node
or Null if input was an empty string
*)
ParseString[s_String, h_:Automatic] :=
Module[{parse},
	parse = ConcreteParseString[s, h];

	ast = Abstract[parse];

	ast
]


(*
ConcreteParseFile[full_String] returns a FileNode AST or a Failure object
*)
ConcreteParseFile[full_String, h_:Automatic] :=
	concreteParseFile[full, h]

TokenizeFile[full_String] :=
	concreteParseFile[full, List, "Tokenize"->True]



Options[concreteParseFile] = {
	"Tokenize" -> False
}


concreteParseFile[file_String, hIn_, OptionsPattern[]] :=
Catch[
Module[{h, full, res, actualAST, tryString, actual, skipFirstLine = False, shebangWarn = False, opts, issues, tokenize},

	h = hIn;

	If[h === Automatic,
		h = Function[FileNode[File, {##}, <||>]]
	];

	tokenize = OptionValue["Tokenize"];

	(*
	We want to expand anything like ~ before passing to external process

	FindFile does a better job than AbsoluteFileName because it can handle things like "Foo`" also
	*)
	full = FindFile[file];
	If[FailureQ[full],
		Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
	];

	(*
	figure out if first line is special
	*)
	If[FileByteCount[full] > 0,
		Quiet[
			(*
			Importing a file containing only \n gives a slew of different messages and fails
			bug 363161
			Remove this Quiet when bug is resolved
			*)
			firstLine = Import[full, {"Lines", 1}];
			If[FailureQ[firstLine],
				firstLine = "";
			]
		];
		Which[
			StringMatchQ[firstLine, "(*!1"~~("A"|"B"|"C"|"D"|"H"|"I"|"N"|"O")~~"!*)mcm"],
			Throw[Failure["EncodedFile", <|"FileName"->full|>]]
			,
			StringStartsQ[firstLine, "#!"],
			skipFirstLine = True
			,
			StringStartsQ[firstLine, "#"],
			shebangWarn = True;
		];
	];

	If[$exe === None,
		Throw[Failure["ExecutableNotFound", <|"Executable"->$exe|>]]
	];

	res = RunProcess[{
			$exe,
			Sequence@@If[tokenize, {"-format", "tokens"}, {}],
			Sequence@@If[skipFirstLine, {"-skipFirstLine"}, {}],
			"-file", full
		}, All];

	If[FailureQ[res],
		Throw[res]
	];

	res = handleResult[res, h];

	If[FailureQ[res],
		If[res === $Failed,
			Throw[res]
		];
		res = Failure[res[[1]], Join[res[[2]], <|"FileName"->full|>]];
		Throw[res]
	];

	If[shebangWarn,
		opts = res[[3]];
		issues = Lookup[opts, SyntaxIssues, {}];
		AppendTo[issues, SyntaxIssue["Shebang", "# on first line looks like #! shebang", "Remark", <|Source->{{1, 1}, {1, 1}}|>]];
		AssociateTo[opts, SyntaxIssues -> issues];
		res[[3]] = opts;
	];

	res
]]

ParseFile[file_String, h_:Automatic] :=
Module[{parse, ast},

	parse = ConcreteParseFile[file, h];

	ast = Abstract[parse];

	ast
]


handleResult[res_Association, h_] :=
Catch[
Module[{input, ast},

	If[$Debug,
		If[!empty[res["StandardError"]],
			Print[res["StandardError"]]
		]
	];

	(*
	ExitCode may be None if the process crashed
	see bug 360670
	So make sure to test with =!=
	*)
	If[res["ExitCode"] =!= 0,
		Throw[Failure["ExitCode", <|"ExitCode"->res["ExitCode"]|>]]
	];

	input = res["StandardOutput"];

	If[$Debug,
		$LastStandardOutput = input;
	];

	(*
	work-around bug 363889
	*)
	If[$OperatingSystem == "Windows",
		input = StringReplace[input, "\r" -> ""];
	];

	(*
	The current implementation of AST uses ToExpression to convert a string from the wl-ast process
	to an expression.
	This may hit the depth limit in the WL parser and give errors. e.g.,
	ToExpression[StringJoin[{Table["f@", 255], "g"}]] evaluates fine
	ToExpression[StringJoin[{Table["f@", 256], "g"}]] gives ToExpression::sntx and returns $Failed

	Using a verbose way of expressing syntax makes this limit get hit earlier.
	*)
	Quiet[
	(*
	Put AST` on path even if it is not on path originally
	*)
	Block[{$ContextPath = {"AST`", "System`"}},
		If[h === Automatic,
			ast = ToExpression[input, InputForm];
			If[FailureQ[ast],
				Throw[Failure["ToExpressionFailed", <|"Result"->ast|>]]
			];
			,
			ast = ToExpression[input, InputForm, h];
			If[FailureQ[ast],
				Throw[Failure["ToExpressionFailed", <|"Result"->ast|>]]
			];
		]
	]];

	ast
]]





End[]

EndPackage[]
