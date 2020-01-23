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

ConcreteParseLeaf

SafeString

ExprTest



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
ErrorNode
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


ContainerNode


SyntaxErrorNode
GroupMissingCloserNode
GroupMissingCloserNeedsReparseNode
AbstractSyntaxErrorNode


InternalInvalid


PackageNode
ContextNode



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
Needs["AST`Error`"]
Needs["AST`Library`"]
Needs["AST`Quirks`"]
Needs["AST`Shims`"]
Needs["AST`ToString`"]
Needs["AST`Utils`"]


setupLibraries[]
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
Catch[
Module[{cst, bytes, encoding},

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	bytes = ToCharacterCode[s, "UTF8"];

	cst = concreteParseString[bytes, h, opts];

	If[FailureQ[cst],
		Throw[cst]
	];

	cst = cst /. {
		node_GroupMissingCloserNeedsReparseNode :> reparseMissingCloserNode[node, bytes],
		node:ErrorNode[Token`Error`UnterminatedComment, _, _] :> reparseUnterminatedCommentErrorNode[node, bytes]
	};

	cst
]]


Options[concreteParseString] = Options[ConcreteParseString]

concreteParseString[bytes_List, hIn_, OptionsPattern[]] :=
Catch[
Module[{h, res},

	h = hIn;

	(*
	The <||> will be filled in with Source later
	The # here is { {exprs}, {issues}, {metadata} }
	*)
	If[h === Automatic,
		h = ContainerNode[String, #[[1]], If[!empty[#[[2]] ], <| SyntaxIssues -> #[[2]] |>, <||>]]&
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[concreteParseBytesFunc, bytes];

	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

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
Catch[
Module[{cst, encoding, full, bytes},

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	(*
	We want to expand anything like ~ before passing to external process

	FindFile does a better job than AbsoluteFileName because it can handle things like "Foo`" also

	FindFile also fails if in sandbox mode
	*)
	full = FindFile[f];
	If[FailureQ[full],
		Throw[Failure["FindFileFailed", <|"FileName"->f|>]]
	];

	bytes = Import[full, "Byte"];

	cst = concreteParseFile[bytes, h, opts];

	If[FailureQ[cst],
		If[cst === $Failed,
			Throw[cst]
		];
		cst = Failure[cst[[1]], Join[cst[[2]], <|"FileName"->f|>]];
		Throw[cst]
	];

	cst = cst /. {
		node_GroupMissingCloserNeedsReparseNode :> reparseMissingCloserNode[node, bytes],
		node:ErrorNode[Token`Error`UnterminatedComment, _, _] :> reparseUnterminatedCommentErrorNode[node, bytes]
	};

	cst
]]


Options[concreteParseFile] = Options[ConcreteParseFile]

concreteParseFile[bytes_List, hIn_, OptionsPattern[]] :=
Catch[
Module[{h, res, data, start, end, children},

	h = hIn;

	(*
	The <||> will be filled in with Source later
	The # here is { {exprs}, {issues}, {metadata} }
	*)
	If[h === Automatic,
		h = ContainerNode[File, #[[1]], If[!empty[#[[2]] ], <| SyntaxIssues -> #[[2]] |>, <||>]]&
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[concreteParseBytesFunc, bytes];

	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

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
			start = First[children][[3, Key[Source], 1]];
			end = Last[children][[3, Key[Source], 2]];
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
Catch[
Module[{cst, encoding},

	encoding = OptionValue[CharacterEncoding];

	If[encoding =!= "UTF8",
		Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
	];

	cst = concreteParseBytes[bytes, h, opts];

	If[FailureQ[cst],
		Throw[cst]
	];

	cst = cst /. {
		node_GroupMissingCloserNeedsReparseNode :> reparseMissingCloserNode[node, bytes],
		node:ErrorNode[Token`Error`UnterminatedComment, _, _] :> reparseUnterminatedCommentErrorNode[node, bytes]
	};

	cst
]]


Options[concreteParseBytes] = Options[ConcreteParseBytes]

concreteParseBytes[bytes_List, hIn_, OptionsPattern[]] :=
Catch[
Module[{h, res, data, start, end, children},

	h = hIn;

	(*
	The <||> will be filled in with Source later
	The # here is { {exprs}, {issues}, {metadata} }
	*)
	If[h === Automatic,
		h = ContainerNode[Byte, #[[1]], If[!empty[#[[2]] ], <| SyntaxIssues -> #[[2]] |>, <||>]]&
	];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];

	res = libraryFunctionWrapper[concreteParseBytesFunc, bytes];

	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

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
			start = First[children][[3, Key[Source], 1]];
			end = Last[children][[3, Key[Source], 2]];
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

	res = libraryFunctionWrapper[tokenizeBytesFunc, bytes];

	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

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

	res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess];

	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

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

	res = libraryFunctionWrapper[tokenizeBytesFunc, bytes];
	
	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

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

	res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess];
	
	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

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

	res = libraryFunctionWrapper[tokenizeBytesFunc, bytes];

	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

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

	res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess];

	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

	If[FailureQ[res],
		Throw[res]
	];

	res
]]








ConcreteParseLeaf::usage = "ConcreteParseLeaf[str] returns a LeafNode by interpreting str as a leaf."

Options[ConcreteParseLeaf] = {
	"StringifyMode" -> 0
}

ConcreteParseLeaf[str_String, opts:OptionsPattern[]] :=
	concreteParseLeaf[str, opts]


Options[concreteParseLeaf] = Options[ConcreteParseLeaf]

concreteParseLeaf[strIn_String, OptionsPattern[]] :=
Catch[
Module[{str, res, leaf, data, exprs, issues, stringifyMode},

	str = strIn;

	stringifyMode = OptionValue["StringifyMode"];

	$ConcreteParseProgress = 0;
	$ConcreteParseStart = Now;
	$ConcreteParseTime = Quantity[0, "Seconds"];
	
	res = libraryFunctionWrapper[concreteParseLeafFunc, str, stringifyMode];

	$ConcreteParseProgress = 100;
	$ConcreteParseTime = Now - $ConcreteParseStart;

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








SafeString::usage = "SafeString[bytes] interprets bytes as UTF-8 and returns a \"safe\" string. Invalid \
sequences and surrogates are replaced with \[UnknownGlyph] and BOM is replaced with special character \\:e001."

SafeString[bytes:{_Integer...}] :=
Module[{res},
	res = libraryFunctionWrapper[safeStringFunc, bytes];
	res
]




ExprTest[] :=
Module[{p, e},
	
	p = libraryFunctionWrapper[exprTestFunc];

	e = AST`Library`Private`$exprCompiledLibFuns["Expr_FromPointer"][p];
	
	AST`Library`Private`$exprCompiledLibFuns["Expr_Release"][e];
	
	e
]




End[]

EndPackage[]
