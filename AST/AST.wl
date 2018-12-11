BeginPackage["AST`"]


(* functions *)

ParseString::usage = "ParseString[string] returns an AST by interpreting string as WL input. \
Note: If there are multiple expressions in string, then only the last expression is returned. \
ParseString[string, h] wraps the output with h and allows multiple expressions to be returned."

ParseFile::usage = "ParseFile[file] returns an AST by interpreting file as WL input."

TokenizeString::usage = "TokenizeString[string] returns a list of tokens by interpreting string as WL input."

TokenizeFile::usage = "TokenizeFile[file] returns a list of tokens by interpreting file as WL input."



ToInputFormString

ToNode


DeclarationName



PrefixOperatorToSymbol
PostfixOperatorToSymbol
BinaryOperatorToSymbol
InfixOperatorToSymbol
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



(* non-System symbols *)

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
AST`PostfixHermitianConjugate and AST`InfixImplicitPlus

*)

(*Token*)
Token

(* atoms *)
InternalMinus
InternalEmpty
OptionalDefault
PatternBlank
PatternBlankSequence
PatternBlankNullSequence

(* operators *)
BinarySlashSlash
BinaryAt
BinaryInvisibleApplication
BinaryAtAtAt
InfixImplicitTimes
InfixImplicitPlus
TernaryTilde
TernarySlashColon
LinearSyntaxBang
PostfixHermitianConjugate


(* groups *)
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



(* options *)
DerivativeOrder
Source
(*
Used to report f[,] or "\[Alpa]" as an option, e.g. SyntaxIssues -> {SyntaxIssue[], SyntaxIssue[]}
*)
SyntaxIssues
SyntaxIssue




(* nodes *)
SymbolNode
StringNode
NumberNode
SyntaxErrorNode
BlankNode
BlankSequenceNode
BlankNullSequenceNode
PatternBlankNode
PatternBlankSequenceNode
PatternBlankNullSequenceNode
OptionalDefaultNode
SlotNode
SlotSequenceNode
OutNode
InternalEmptyNode
PrefixNode
BinaryNode
TernaryNode
InfixNode
PostfixNode
GroupNode
CallNode
PartNode
InternalMinusNode

(*
InternalTokenNode represents a token in a linear syntax expression
When parsing a linear syntax expressions, all tokens are simply kept unparsed
*)
InternalTokenNode

FileNode

CallMissingCloserNode


InternalInvalid




Begin["`Private`"]

Needs["AST`DeclarationName`"]
Needs["AST`Node`"]
Needs["AST`Symbol`"]
Needs["AST`ToInputFormString`"]
Needs["AST`Utils`"]


packageDir = DirectoryName[FindFile["AST`"]]

exe = FileNameJoin[{packageDir, "ASTResources", $SystemID, "wl-ast"}]


ParseString[s_String, h_:Automatic] :=
	parseString[s, h]

TokenizeString[s_String] :=
	parseString[s, Automatic, "Tokenize"->True]


Options[parseString] = {
	"Tokenize" -> False
}

parseString[sIn_String, h_, OptionsPattern[]] :=
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
	If[Length[multiBytes] > 0,
		Throw[Failure["MultiByteCharactersNotAllowed", <|
			"Input"->s,
			"MultiByteCharacters"->AST`Utils`escapeString[FromCharacterCode[#]]& /@ Take[multiBytes, UpTo[10]]|>]]
	];

	res = RunProcess[{
		exe,
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
ParseFile[full_String] returns a FileNode AST or a Failure object
*)
ParseFile[full_String] :=
	parseFile[full]

TokenizeFile[full_String] :=
	parseFile[full, "Tokenize"->True]



Options[parseFile] = {
	"Tokenize" -> False
}


parseFile[fullIn_String, OptionsPattern[]] :=
Catch[
Module[{full, res, actualAST, tryString, actual, skipFirstLine = False, shebangWarn = False, opts, issues, tokenize},

	tokenize = OptionValue["Tokenize"];

	(*
	We want to expand anything like ~ before passing to external process
	*)
	full = AbsoluteFileName[fullIn];
	If[FailureQ[full],
		Throw[full]
	];

	If[FileType[full] =!= File,
		Throw[Failure["NotAFile", <|"FileName"->full|>]]
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
				Throw[Failure["CannotImportLines", <|"FileName"->full|>]]
			]
		];
		Which[
			firstLine == "(*!1N!*)mcm",
			Throw[Failure["EncodedFile", <|"FileName"->full|>]]
			,
			StringStartsQ[firstLine, "#!"],
			skipFirstLine = True
			,
			StringStartsQ[firstLine, "#"],
			shebangWarn = True;
		];
	];

	res = RunProcess[{
			exe,
			Sequence@@If[tokenize, {"-format", "tokens"}, {}],
			Sequence@@If[skipFirstLine, {"-skipFirstLine"}, {}],
			"-file", full
		}, All];

	If[FailureQ[res],
		Throw[res]
	];

	res = handleResult[res, Automatic];

	If[FailureQ[res],
		res = Failure[res[[1]], Join[res[[2]], <|"FileName"->full|>]];
		Throw[res]
	];

	If[shebangWarn,
		opts = res[[3]];
		issues = Lookup[opts, SyntaxIssues, {}];
		AppendTo[issues, SyntaxIssue["# on first line looks like #! shebang", "Remark", <|Source->{{1,1}, {1, 1}}|>]];
		AssociateTo[opts, SyntaxIssues -> issues];
		res[[3]] = opts;
	];

	res
]]

handleResult[res_Association, h_] :=
Catch[
Module[{input, ast},

	If[$Debug,
		Print[res]
	];

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

	(*
	work-around bug 363889
	*)
	If[$OperatingSystem == "Windows",
		input = StringReplace[input, "\r" -> ""];
	];

	(*
	Put AST` on path even if it is not on path originally
	*)
	Block[{$ContextPath = {"AST`", "System`"}},
		If[h === Automatic,
			ast = ToExpression[input, InputForm]
			,
			ast = ToExpression[input, InputForm, h]
		]
	];

	ast
]]






End[]

EndPackage[]
