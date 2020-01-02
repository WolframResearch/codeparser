BeginPackage["AST`"]

(*
Parsing
*)
ParseString

ParseFile

ParseBytes

ConcreteParseString

ConcreteParseFile

ConcreteParseBytes

ParseLeaf

SafeString



(*
Tokenizing
*)
TokenizeString

TokenizeFile

TokenizeBytes



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

We want to avoid any confusion about this, so we make sure to use System` here

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
System`InvisiblePrefixScriptBase

System`HermitianConjugate
System`InvisiblePostfixScriptBase

PrefixNot2

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

(*
Shebang
*)

(* option symbols *)
Source
Synthesized

Comment
Metadata

Intra


(* node symbols *)
LeafNode
BoxNode
CodeNode
DirectiveNode

PrefixNode
BinaryNode
TernaryNode
InfixNode
PostfixNode
GroupNode
CallNode
PrefixBinaryNode

(*
StartOfLineNode
StartOfFileNode
*)
BlankNode
BlankSequenceNode
BlankNullSequenceNode
PatternBlankNode
PatternBlankSequenceNode
PatternBlankNullSequenceNode
OptionalDefaultPatternNode

FileNode
StringNode
HoldNode


SyntaxErrorNode
GroupMissingCloserNode
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
InsertNode
InsertNodeAfter
ReplaceNode
DeleteText
InsertText
InsertTextAfter
ReplaceText
DeleteTrivia
DeleteTriviaNode

(*
Used to report f[,] or "\[Alpa]" as an option, e.g. SyntaxIssues -> {SyntaxIssue[], SyntaxIssue[]}
*)
SyntaxIssues
AbstractSyntaxIssues
AbstractFormatIssues
SyntaxIssue
FormatIssue


SourceCharacter


Begin["`Private`"]

(*
Implementation of Abstract depends on Node (LHS of definitions)
So load Node before Abstract
*)
Needs["AST`Node`"]

Needs["AST`Abstract`"]
Needs["AST`Boxes`"]
Needs["AST`DeclarationName`"]
Needs["AST`Library`"]
Needs["AST`Quirks`"]
Needs["AST`Shims`"]
Needs["AST`ToString`"]
Needs["AST`Utils`"]


(*
This uses func := func = def idiom and is fast
*)
loadAllFuncs[]

setupQuirks[]

setupShims[]




ConcreteParseString::usage = "ConcreteParseString[string] returns a concrete syntax tree by interpreting string as WL input. \
ConcreteParseString[string, nodeFunc] applies nodeFunc to the result."

Options[ConcreteParseString] = {
	CharacterEncoding -> "UTF8"
}

ConcreteParseString[s_String, h_:Automatic, opts:OptionsPattern[]] :=
	concreteParseString[s, h, opts]


Options[concreteParseString] = Options[ConcreteParseString]

concreteParseString[sIn_String, hIn_, OptionsPattern[]] :=
Catch[
Module[{s, h, res, bytes, encoding},

	s = sIn;
	h = hIn;

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	bytes = ToCharacterCode[s, "UTF8"];

	(*
	The <||> will be filled in with Source later
	The # here is { {exprs}, {issues}, {metadata} }
	*)
	If[h === Automatic,
		h = StringNode[String, #[[1]], If[!empty[#[[2]]], <| SyntaxIssues -> #[[2]] |>, <||>]]&
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[concreteParseBytesFunc, bytes];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		Throw[res]
	];

	h[res]
]]





ParseString::usage = "ParseString[string] returns an abstract syntax tree by interpreting string as WL input. \
ParseString[string, nodeFunc] applies nodeFunc to the result."

Options[ParseString] = {

}

(*
may return:
a node
or Null if input was an empty string
or something FailureQ if e.g., no permission to run wl-ast
*)
ParseString[s_String, h_:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{cst, ast, agg},
	
	cst = ConcreteParseString[s, h, opts];

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
ConcreteParseFile[f:File[_String], h_:Automatic, opts:OptionsPattern[]] :=
	concreteParseFile[f, h, opts]


Options[concreteParseFile] = Options[ConcreteParseFile]

concreteParseFile[File[file_String], hIn_, OptionsPattern[]] :=
Catch[
Module[{h, encoding, full, res, data, start, end, children, bytes},

	h = hIn;

	encoding = OptionValue[CharacterEncoding];

	(*
	The <||> will be filled in with Source later
	The # here is { {exprs}, {issues}, {metadata} }
	*)
	If[h === Automatic,
		h = FileNode[File, #[[1]], If[!empty[#[[2]]], <| SyntaxIssues -> #[[2]] |>, <||>]]&
	];

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

	bytes = Import[full, "Byte"];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[concreteParseBytesFunc, bytes];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		If[res === $Failed,
			Throw[res]
		];
		res = Failure[res[[1]], Join[res[[2]], <|"FileName"->full|>]];
		Throw[res]
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

ParseFile[f:File[_String], h_:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{cst, ast, agg},

	cst = ConcreteParseFile[f, h, opts];

	If[FailureQ[cst],
		Throw[cst]
	];

	agg = Aggregate[cst];

	ast = Abstract[agg];

	ast
]]





ConcreteParseBytes::usage = "ConcreteParseBytes[bytes] returns a concrete syntax tree by interpreting bytes as WL input."

Options[ConcreteParseBytes] = {
	CharacterEncoding -> "UTF8"
}

(*
ConcreteParseBytes[bytes_List] returns a FileNode AST or a Failure object
*)
ConcreteParseBytes[bytes_List, h_:Automatic, opts:OptionsPattern[]] :=
	concreteParseBytes[bytes, h, opts]


Options[concreteParseBytes] = Options[ConcreteParseBytes]

concreteParseBytes[bytes_List, hIn_, OptionsPattern[]] :=
Catch[
Module[{h, encoding, res, data, start, end, children},

	h = hIn;

	encoding = OptionValue[CharacterEncoding];

	(*
	The <||> will be filled in with Source later
	The # here is { {exprs}, {issues}, {metadata} }
	*)
	If[h === Automatic,
		h = FileNode[File, #[[1]], If[!empty[#[[2]]], <| SyntaxIssues -> #[[2]] |>, <||>]]&
	];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[concreteParseBytesFunc, bytes];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		Throw[res]
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




ParseBytes::usage = "ParseBytes[bytes] returns an abstract syntax tree by interpreting bytes as WL input."

Options[ParseBytes] = {
	CharacterEncoding -> "UTF8"
}

ParseBytes[bytes_List, h_:Automatic, opts:OptionsPattern[]] :=
Catch[
Module[{cst, ast, agg},

	cst = ConcreteParseBytes[bytes, h, opts];

	If[FailureQ[cst],
		Throw[cst]
	];

	agg = Aggregate[cst];

	ast = Abstract[agg];

	ast
]]




TokenizeString::usage = "TokenizeString[string] returns a list of tokens by interpreting string as WL input."

Options[TokenizeString] = {
	CharacterEncoding -> "UTF8"
}

TokenizeString[s_String] :=
	tokenizeString[s]

TokenizeString[ss:{_String...}] :=
	tokenizeStringListable[ss]


Options[tokenizeString] = Options[TokenizeString]

tokenizeString[sIn_String, OptionsPattern[]] :=
Catch[
Module[{s, res, bytes, encoding},

	s = sIn;

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	bytes = ToCharacterCode[s, "UTF8"];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[tokenizeBytesFunc, bytes];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		Throw[res]
	];

	res
]]

Options[tokenizeStringListable] = Options[TokenizeString]

tokenizeStringListable[ssIn:{_String...}, OptionsPattern[]] :=
Catch[
Module[{ss, res, bytess, encoding},

	ss = ssIn;

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	bytess = ToCharacterCode[ss, "UTF8"];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		Throw[res]
	];

	res
]]







TokenizeFile::usage = "TokenizeFile[file] returns a list of tokens by interpreting file as WL input."

Options[TokenizeFile] = {
	CharacterEncoding -> "UTF8"
}

TokenizeFile[f:File[_String], opts:OptionsPattern[]] :=
	tokenizeFile[f, opts]

TokenizeFile[fs:{File[_String]...}, opts:OptionsPattern[]] :=
	tokenizeFileListable[fs, opts]



Options[tokenizeFile] = Options[TokenizeFile]

tokenizeFile[File[file_String], OptionsPattern[]] :=
Catch[
Module[{encoding, res, full, bytes},

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	full = FindFile[file];
	If[FailureQ[full],
		Throw[Failure["FindFileFailed", <|"FileName"->file|>]]
	];

	bytes = Import[full, "Byte"];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[tokenizeBytesFunc, bytes];
	
	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		Throw[res]
	];

	res
]]


Options[tokenizeFileListable] = Options[TokenizeFile]

tokenizeFileListable[fs:{File[_String]...}, OptionsPattern[]] :=
Catch[
Module[{encoding, res, fulls, bytess},

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	fulls = FindFile /@ fs;
	If[AnyTrue[fulls, FailureQ],
		Throw[Failure["FindFileFailed", <|"FileNames"->fs|>]]
	];

	bytess = Import[#, "Byte"]& /@ fulls;

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess];
	
	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		Throw[res]
	];

	res
]]






TokenizeBytes::usage = "TokenizeBytes[bytes] returns a list of tokens by interpreting bytes as WL input."

Options[TokenizeBytes] = {
	CharacterEncoding -> "UTF8"
}

TokenizeBytes[bytes:{_Integer...}, opts:OptionsPattern[]] :=
	tokenizeBytes[bytes, opts]

TokenizeBytes[bytess:{{_Integer...}...}, opts:OptionsPattern[]] :=
	tokenizeBytesListable[bytess, opts]



Options[tokenizeBytes] = Options[TokenizeBytes]

tokenizeBytes[bytes_List, OptionsPattern[]] :=
Catch[
Module[{encoding, res},

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[tokenizeBytesFunc, bytes];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		Throw[res]
	];

	res
]]


Options[tokenizeBytesListable] = Options[TokenizeBytes]

tokenizeBytesListable[bytess:{{_Integer...}...}, OptionsPattern[]] :=
Catch[
Module[{encoding, res},

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		Throw[res]
	];

	res
]]








ParseLeaf::usage = "ParseLeaf[str] returns a LeafNode by interpreting str as a leaf."

Options[ParseLeaf] = {
	"StringifyMode" -> 0
}

ParseLeaf[str_String, opts:OptionsPattern[]] :=
	parseLeaf[str, opts]


Options[parseLeaf] = Options[ParseLeaf]

parseLeaf[strIn_String, OptionsPattern[]] :=
Catch[
Module[{str, res, leaf, data, exprs, issues, stringifyMode},

	str = strIn;

	stringifyMode = OptionValue["StringifyMode"];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	$MathLinkTime = Quantity[0, "Seconds"];
	
	res = libraryFunctionWrapper[parseLeafFunc, str, stringifyMode];

	$MathLinkTime = Now - ($ConcreteParseStart + $ConcreteParseTime);

	If[FailureQ[res],
		Throw[res]
	];

	exprs = res[[1]];
	issues = res[[2]];

	leaf = exprs[[1]];

	If[!empty[issues],
		data = leaf[[3]];
		data[SyntaxIssues] = issues;
		leaf[[3]] = data;
	];

	leaf
]]








Format[fileNode:FileNode[_, children_, _], StandardForm] :=
	Interpretation[
		Row[{"FileNode", "[", "\[LeftSkeleton]", ToString[Length[children]], "\[RightSkeleton]", "]"}]
		,
		fileNode]

Format[fileNode:FileNode[_, children_, _], OutputForm] :=
	"FileNode[\[LeftSkeleton]" <> ToString[Length[children]] <> "\[RightSkeleton]]"




SafeString::usage = "SafeString[bytes] interprets bytes as UTF-8 and returns a \"safe\" string. Invalid \
sequences and surrogates are replaced with \[UnknownGlyph] and BOM is replaced with special character \\:e001."

SafeString[bytes:{_Integer...}] :=
Module[{res},
	res = libraryFunctionWrapper[safeStringFunc, bytes];
	res
]





End[]

EndPackage[]






(*
mapOffsetToLineCol[offset_, offsetLineMap_] :=
 
 Module[{line, lineOffset, col, prevLineOffset},
  line = 1;
  prevLineOffset = -1;
  lineOffset = offsetLineMap[[line]];
  While[offset >= lineOffset,
   prevLineOffset = lineOffset;
   line++;
   lineOffset = offsetLineMap[[line]];
   ];
  col = offset - prevLineOffset;
  {line, col}
  ]
*)
