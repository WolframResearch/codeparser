BeginPackage["AST`"]

(*
Parsing
*)
ParseString

ParseFile

ConcreteParseString

ConcreteParseFile

TokenizeString

TokenizeFile

ParseLeaf


(*
Boxes
*)
ConcreteParseBox



(*
ToString
*)
ToInputFormString

ToFullFormString

ToStandardFormBoxes

ToSourceCharacterString



(*
Nodes
*)
ToNode
FromNode

DeclarationName


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
BinaryAtAtAt

TernaryTilde
TernarySlashColon

Comma


(* group symbols *)
(*List*)

(*Association*)

(*AngleBracket*)

(*Ceiling*)

(*Floor*)

GroupDoubleBracket

GroupSquare

(*BracketingBar*)

(*DoubleBracketingBar*)

GroupParen

GroupLinearSyntaxParen

(* option symbols *)
Source
Synthesized

Comment
Metadata

(* node symbols *)
LeafNode
BoxNode
CodeNode
DirectiveNode
MultiBoxNode

PrefixNode
BinaryNode
TernaryNode
InfixNode
PostfixNode
GroupNode
CallNode
PrefixBinaryNode

StartOfLineNode
BlankNode
BlankSequenceNode
BlankNullSequenceNode
PatternBlankNode
PatternBlankSequenceNode
PatternBlankNullSequenceNode
OptionalDefaultPatternNode

FileNode
HoldNode


SyntaxErrorNode
GroupMissingCloserNode
GroupMissingOpenerNode
AbstractSyntaxErrorNode


InternalInvalid

BeginStaticAnalysisIgnore
EndStaticAnalysisIgnore


PackageNode
ContextNode
StaticAnalysisIgnoreNode



(*
Analysis
*)

(* property for SyntaxIssue *)
CodeActions
CodeAction
(*
CodeAction commands
*)
DeleteNode
ReplaceNode

ReplaceText


(*
Used to report f[,] or "\[Alpa]" as an option, e.g. SyntaxIssues -> {SyntaxIssue[], SyntaxIssue[]}
*)
SyntaxIssues
AbstractSyntaxIssues
SyntaxIssue








Begin["`Private`"]

Needs["AST`Abstract`"]
Needs["AST`Boxes`"]
Needs["AST`DeclarationName`"]
Needs["AST`Library`"]
Needs["AST`Node`"]
Needs["AST`ToString`"]
Needs["AST`Utils`"]
Needs["PacletManager`"]



loadAllFuncs[]


ConcreteParseString::usage = "ConcreteParseString[string] returns a concrete syntax tree by interpreting string as WL input."

ConcreteParseString[s_String, h_:Automatic] :=
	concreteParseString[s, h]

concreteParseString[sIn_String, hIn_, OptionsPattern[]] :=
Catch[
Module[{s, h, res},

	s = sIn;
	h = hIn;

	If[h === Automatic,
		(*
		The # here is { {exprs}, {issues}, {metadata} }

		Return the last aggregate node, or Null

		Simply drop any leftover syntax issues
		*)
		h = SelectFirst[Reverse[DeleteCases[#[[1]], LeafNode[Token`Comment | Token`WhiteSpace | Token`Newline | Token`LineContinuation, _, _]]], True&, Null]&
	];

	If[FailureQ[concreteParseStringFunc],
		Throw[concreteParseStringFunc]
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];
	(*
	in the event of an abort, force reload of functions
	This will fix the transient error that can happen when an abort occurs
	and the next use throws LIBRARY_FUNCTION_ERROR
	*)
	CheckAbort[
	res = concreteParseStringFunc[s];
	,
	loadAllFuncs[];
	Abort[]
	];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[Head[res] === LibraryFunctionError,
		Throw[Failure["LibraryFunctionError", <|"Result"->res|>]]
	];

	If[FailureQ[res],
		Throw[res]
	];

	h[res]
]]





ParseString::usage = "ParseString[string] returns an abstract syntax tree by interpreting string as WL input. \
Note: If there are multiple expressions in string, then only the last expression is returned. \
ParseString[string, h] wraps the output with h and allows multiple expressions to be returned. \
This is similar to how ToExpression operates."

(*
may return:
a node
or Null if input was an empty string
or something FailureQ if e.g., no permission to run wl-ast
*)
ParseString[s_String, h_:Automatic] :=
Catch[
Module[{cst, ast, agg},
	
	cst = ConcreteParseString[s, h];

	If[FailureQ[cst],
		Throw[cst]
	];

	agg = Aggregate[cst];

	ast = Abstract[agg];

	ast
]]




ConcreteParseFile::usage = "ConcreteParseFile[file] returns a concrete syntax tree by interpreting file as WL input."

Options[ConcreteParseFile] = {
	CharacterEncoding -> "UTF8"
}

(*
ConcreteParseFile[file_String] returns a FileNode AST or a Failure object
*)
ConcreteParseFile[file_String | File[file_String], h_:Automatic, opts:OptionsPattern[]] :=
	concreteParseFile[file, h, opts]


Options[concreteParseFile] = Options[ConcreteParseFile]

concreteParseFile[file_String, hIn_, OptionsPattern[]] :=
Catch[
Module[{h, encoding, full, res, skipFirstLine = False, shebangWarn = False, data, issues, firstLine, start, end, children},

	h = hIn;

	(*
	The <||> will be filled in with Source later
	The # here is { {exprs}, {issues}, {metadata} }
	*)
	If[hIn === Automatic,
		h = FileNode[File, #[[1]], <| SyntaxIssues -> #[[2]] |>]&
	];

	encoding = OptionValue[CharacterEncoding];
	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];


	(*
	We want to expand anything like ~ before passing to external process

	FindFile does a better job than AbsoluteFileName because it can handle things like "Foo`" also

	FindFile also fails if in sandbox mode
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
			(* special encoded file format *)
			StringMatchQ[firstLine, "(*!1"~~("A"|"B"|"C"|"D"|"H"|"I"|"N"|"O")~~"!*)mcm"],
			Throw[Failure["EncodedFile", <|"FileName"->full|>]]
			,
			(* wl script *)
			StringStartsQ[firstLine, "#!"],
			skipFirstLine = True
			,
			(* looks like a script; warn *)
			StringStartsQ[firstLine, "#"],
			shebangWarn = True;
		];
	];

	If[FailureQ[concreteParseFileFunc],
		Throw[concreteParseFileFunc]
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];
	CheckAbort[
	res = concreteParseFileFunc[full, skipFirstLine];
	,
	loadAllFuncs[];
	Abort[]
	];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[Head[res] === LibraryFunctionError,
		Throw[Failure["LibraryFunctionError", <|"Result"->res|>]]
	];

	If[FailureQ[res],
		If[res === $Failed,
			Throw[res]
		];
		res = Failure[res[[1]], Join[res[[2]], <|"FileName"->full|>]];
		Throw[res]
	];


	If[shebangWarn,
		issues = res[[2]];
		AppendTo[issues, SyntaxIssue["Shebang", "# on first line looks like #! shebang", "Remark", <|Source->{{1, 1}, {1, 1}}|>]];
		res[[2]] = issues;
	];

	res = h[res];

	(*
	Fill in Source for FileNode now
	*)
	If[hIn === Automatic,
		children = res[[2]];
		(* only fill in if there are actually children nodes to grab *)
		If[children =!= {},
			start = First[children][[3]][Source][[1]];
			end = Last[children][[3]][Source][[2]];
			data = res[[3]];
			AssociateTo[data, Source -> {start, end}];
			res[[3]] = data;
		];
	];

	res
]]




ParseFile::usage = "ParseFile[file] returns an abstract syntax tree by interpreting file as WL input."

Options[ParseFile] = {
	CharacterEncoding -> "UTF8"
}

ParseFile[file_String | File[file_String], h_:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{cst, ast, agg},

	cst = ConcreteParseFile[file, h, opts];

	If[FailureQ[cst],
		Throw[cst]
	];

	agg = Aggregate[cst];

	ast = Abstract[agg];

	ast
]]




TokenizeString::usage = "TokenizeString[string] returns a list of tokens by interpreting string as WL input."

TokenizeString[s_String] :=
	tokenizeString[s]

tokenizeString[sIn_String, OptionsPattern[]] :=
Catch[
Module[{s = sIn, res},

	If[FailureQ[tokenizeStringFunc],
		Throw[tokenizeStringFunc]
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];
	CheckAbort[
	res = tokenizeStringFunc[s];
	,
	loadAllFuncs[];
	Abort[]
	];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[Head[res] === LibraryFunctionError,
		Throw[Failure["LibraryFunctionError", <|"Result"->res|>]]
	];

	If[FailureQ[res],
		Throw[res]
	];

	res
]]







TokenizeFile::usage = "TokenizeFile[file] returns a list of tokens by interpreting file as WL input."

Options[TokenizeFile] = {
	CharacterEncoding -> "UTF8"
}

TokenizeFile[s_String | File[s_String], opts:OptionsPattern[]] :=
	tokenizeFile[s, opts]




Options[tokenizeFile] = Options[TokenizeFile]

tokenizeFile[sIn_String, OptionsPattern[]] :=
Catch[
Module[{s, encoding, res},

	s = sIn;

	encoding = OptionValue[CharacterEncoding];
	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	If[FailureQ[tokenizeFileFunc],
		Throw[tokenizeFileFunc]
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];
	CheckAbort[
	res = tokenizeFileFunc[s];
	,
	loadAllFuncs[];
	Abort[]
	];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[Head[res] === LibraryFunctionError,
		Throw[Failure["LibraryFunctionError", <|"Result"->res|>]]
	];

	If[FailureQ[res],
		Throw[res]
	];

	res
]]




ParseLeaf::usage = "ParseLeaf[str] returns a LeafNode by interpreting str as a leaf."

ParseLeaf[str_String] :=
	parseLeaf[str]

parseLeaf[strIn_String, OptionsPattern[]] :=
Catch[
Module[{str, res, leaf, data, exprs, issues},

	str = strIn;

	If[FailureQ[parseLeafFunc],
		Throw[parseLeafFunc]
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];
	(*
	in the event of an abort, force reload of functions
	This will fix the transient error that can happen when an abort occurs
	and the next use throws LIBRARY_FUNCTION_ERROR
	*)
	CheckAbort[
	res = parseLeafFunc[str];
	,
	loadAllFuncs[];
	Abort[]
	];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[Head[res] === LibraryFunctionError,
		Throw[Failure["LibraryFunctionError", <|"Result"->res|>]]
	];

	If[FailureQ[res],
		Throw[res]
	];

	exprs = res[[1]];
	issues = res[[2]];

	leaf = exprs[[1]];
	data = leaf[[3]];
	data[SyntaxIssues] = issues;
	leaf[[3]] = data;
	leaf
]]



End[]

EndPackage[]
