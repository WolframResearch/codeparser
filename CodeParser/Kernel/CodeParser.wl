BeginPackage["CodeParser`"]

CodeParse

CodeConcreteParse

CodeConcreteParseBox

CodeConcreteParseLeaf

SafeString

ExprTest



CodeTokenize



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
PatternBlank
PatternBlankSequence
PatternBlankNullSequence
PatternOptionalDefault

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

InfixInequality

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

BlankNode
BlankSequenceNode
BlankNullSequenceNode
PatternBlankNode
PatternBlankSequenceNode
PatternBlankNullSequenceNode
PatternOptionalDefaultNode

SlotNode
SlotSequenceNode

OutNode

ContainerNode


SyntaxErrorNode
GroupMissingCloserNode
GroupMissingCloserNeedsReparseNode
AbstractSyntaxErrorNode


InternalInvalid


PackageNode
ContextNode
NewContextPathNode


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
EncodingIssue


SourceCharacter


Begin["`Private`"]

(*
Implementation of Abstract depends on Node (LHS of definitions)
So load Node before Abstract
*)
Needs["CodeParser`Node`"]

Needs["CodeParser`Abstract`"]
Needs["CodeParser`Boxes`"]
Needs["CodeParser`Definitions`"]
Needs["CodeParser`Error`"]
Needs["CodeParser`Library`"]
Needs["CodeParser`Quirks`"]
Needs["CodeParser`Shims`"]
Needs["CodeParser`ToString`"]
Needs["CodeParser`Utils`"]


(*
This uses func := func = def idiom and is fast
*)
loadAllFuncs[]

setupQuirks[]

setupShims[]



If[PacletFind["AST"] != {},
  Message[General::obspkg, "AST`"]
]



CodeConcreteParse::usage = "CodeConcreteParse[code] returns a concrete syntax tree by interpreting code as WL input."

Options[CodeConcreteParse] = {
  CharacterEncoding -> "UTF8",
  "SourceConvention" -> "LineColumn",
  ContainerNode -> Automatic
}

CodeConcreteParse[s_String, opts:OptionsPattern[]] :=
Catch[
Module[{csts},

  csts = CodeConcreteParse[{s}, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  csts[[1]]
]]

CodeConcreteParse[ss:{_String, _String...}, opts:OptionsPattern[]] :=
Catch[
Module[{csts, bytess, encoding},

  encoding = OptionValue[CharacterEncoding];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  bytess = ToCharacterCode[ss, "UTF8"];

  csts = concreteParseStringListable[bytess, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  csts =
    MapThread[Function[{cst, bytes},
      cst /. {
        node_GroupMissingCloserNeedsReparseNode :> reparseMissingCloserNode[node, bytes, opts],
        node:ErrorNode[Token`Error`UnterminatedComment, _, _] :> reparseUnterminatedCommentErrorNode[node, bytes, opts]
      }]
      ,
      {csts, bytess}
    ];

  csts
]]


Options[concreteParseStringListable] = Options[CodeConcreteParse]

concreteParseStringListable[bytess:{{_Integer...}...}, OptionsPattern[]] :=
Catch[
Module[{res, convention, container},

  convention = OptionValue["SourceConvention"];
  container = OptionValue[ContainerNode];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {metadata} }
  *)
  If[container === Automatic,
    container = ContainerNode[String, #[[1]], If[!empty[#[[2]]], <| SyntaxIssues -> #[[2]] |>, <||>]]&
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[concreteParseBytesListableFunc, bytess, convention];
  ];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res = container /@ res;

  res
]]





CodeParse::usage = "CodeParse[code] returns an abstract syntax tree by interpreting code as WL input."

Options[CodeParse] = {
  CharacterEncoding -> "UTF8",
  "SourceConvention" -> "LineColumn",
  ContainerNode -> Automatic
}

(*
may return:
a node
or Null if input was an empty string
or something FailureQ if e.g., no permission to run wl-codeparser
*)
CodeParse[s_String, opts:OptionsPattern[]] :=
Catch[
Module[{asts},

  asts = CodeParse[{s}, opts];

  If[FailureQ[asts],
    Throw[asts]
  ];

  asts[[1]]
]]

CodeParse[ss:{_String, _String...}, opts:OptionsPattern[]] :=
Catch[
Module[{csts, asts, aggs},
  
  csts = CodeConcreteParse[ss, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  aggs = Aggregate /@ csts;

  asts = Abstract /@ aggs;

  asts
]]






CodeConcreteParse[f:File[_String], opts:OptionsPattern[]] :=
Catch[
Module[{csts},

  csts = CodeConcreteParse[{f}, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  csts[[1]]
]]

CodeConcreteParse[fs:{File[_String], File[_String]...}, opts:OptionsPattern[]] :=
Catch[
Module[{csts, encoding, fulls, bytess},

  encoding = OptionValue[CharacterEncoding];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  (*
  We want to expand anything like ~ before passing to external process

  FindFile does a better job than AbsoluteFileName because it can handle things like "Foo`" also

  FindFile also fails if in sandbox mode
  *)
  fulls = FindFile /@ fs;
  If[AnyTrue[fulls, FailureQ],
    Throw[Failure["FindFileFailed", <|"FileNames"->fs|>]]
  ];

  bytess = Import[#, "Byte"]& /@ fulls;

  csts = concreteParseFileListable[bytess, opts];

  If[FailureQ[csts],
    If[csts === $Failed,
      Throw[csts]
    ];
    csts = Failure[csts[[1]], Join[csts[[2]], <|"FileNames"->fs|>]];
    Throw[csts]
  ];

  csts =
    MapThread[Function[{cst, bytes},
      cst /. {
        node_GroupMissingCloserNeedsReparseNode :> reparseMissingCloserNode[node, bytes, opts],
        node:ErrorNode[Token`Error`UnterminatedComment, _, _] :> reparseUnterminatedCommentErrorNode[node, bytes, opts]
      }]
      ,
      {csts, bytess}
    ];

  csts
]]



Options[concreteParseFileListable] = Options[CodeConcreteParse]

concreteParseFileListable[bytess:{{_Integer...}...}, OptionsPattern[]] :=
Catch[
Module[{res, convention, container, containerWasAutomatic},

  convention = OptionValue["SourceConvention"];
  container = OptionValue[ContainerNode];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {metadata} }
  *)
  If[container === Automatic,
    containerWasAutomatic = True;
    container = ContainerNode[File, #[[1]], If[!empty[#[[2]]], <| SyntaxIssues -> #[[2]] |>, <||>]]&
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[concreteParseBytesListableFunc, bytess, convention];
  ];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res = container /@ res;

  (*
  Fill in Source for FileNode now
  *)
  If[containerWasAutomatic,
    res = fillinSource /@ res
  ];

  res
]]



fillinSource[cstIn_] :=
Module[{cst, children, start, end, data},

  cst = cstIn;

  children = cst[[2]];
  (* only fill in if there are actually children nodes to grab *)
  If[children =!= {},
    start = First[children][[3, Key[Source], 1]];
    end = Last[children][[3, Key[Source], 2]];
    data = cst[[3]];
    AssociateTo[data, Source -> {start, end}];
    cst[[3]] = data;
  ];

  cst
]





CodeParse[f:File[_String], opts:OptionsPattern[]] :=
Catch[
Module[{asts},

  asts = CodeParse[{f}, opts];

  If[FailureQ[asts],
    Throw[asts]
  ];

  asts[[1]]
]]

CodeParse[fs:{File[_String], File[_String]...}, opts:OptionsPattern[]] :=
Catch[
Module[{csts, asts, aggs},

  csts = CodeConcreteParse[fs, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  aggs = Aggregate /@ csts;

  asts = Abstract /@ aggs;

  asts
]]





CodeConcreteParse[bytes:{_Integer, _Integer...}, opts:OptionsPattern[]] :=
Catch[
Module[{csts},

  csts = CodeConcreteParse[{bytes}, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  csts[[1]]
]]

CodeConcreteParse[bytess:{{_Integer, _Integer...}...}, opts:OptionsPattern[]] :=
Catch[
Module[{csts, encoding},

  encoding = OptionValue[CharacterEncoding];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  csts = concreteParseBytesListable[bytess, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  csts =
    MapThread[Function[{cst, bytes},
      cst /. {
        node_GroupMissingCloserNeedsReparseNode :> reparseMissingCloserNode[node, bytes, opts],
        node:ErrorNode[Token`Error`UnterminatedComment, _, _] :> reparseUnterminatedCommentErrorNode[node, bytes, opts]
      }]
      ,
      {csts, bytess}
    ];

  csts
]]



Options[concreteParseBytesListable] = Options[CodeConcreteParse]

concreteParseBytesListable[bytess:{{_Integer...}...}, OptionsPattern[]] :=
Catch[
Module[{res, convention, container},

  convention = OptionValue["SourceConvention"];
  container = OptionValue[ContainerNode];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {metadata} }
  *)
  If[container === Automatic,
    container = ContainerNode[Byte, #[[1]], If[!empty[#[[2]]], <| SyntaxIssues -> #[[2]] |>, <||>]]&
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[concreteParseBytesListableFunc, bytess, convention];
  ];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res = container /@ res;

  res
]]






CodeParse[bytes:{_Integer, _Integer...}, opts:OptionsPattern[]] :=
Catch[
Module[{asts},

  asts = CodeParse[{bytes}, opts];

  If[FailureQ[asts],
    Throw[asts]
  ];

  asts[[1]]
]]

CodeParse[bytess:{{_Integer, _Integer...}...}, opts:OptionsPattern[]] :=
Catch[
Module[{csts, asts, aggs},

  csts = CodeConcreteParse[bytess, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  aggs = Aggregate /@ csts;

  asts = Abstract /@ aggs;

  asts
]]






CodeConcreteParse[{}, opts:OptionsPattern[]] :=
Catch[
Module[{},

  {}
]]

CodeParse[{}, opts:OptionsPattern[]] :=
Catch[
Module[{},

  {}
]]






CodeTokenize::usage = "CodeTokenize[code] returns a list of tokens by interpreting code as WL input."

Options[CodeTokenize] = {
  CharacterEncoding -> "UTF8",
  "SourceConvention" -> "LineColumn"
}

CodeTokenize[s_String, opts:OptionsPattern[]] :=
Catch[
Module[{tokss},

  tokss = CodeTokenize[{s}, opts];

  If[FailureQ[tokss],
    Throw[tokss]
  ];

  tokss[[1]]
]]

CodeTokenize[ss:{_String, _String...}, opts:OptionsPattern[]] :=
Module[{tokss},

  tokss = tokenizeStringListable[ss, opts];

  tokss
]


Options[tokenizeStringListable] = Options[CodeTokenize]

tokenizeStringListable[ss:{_String...}, OptionsPattern[]] :=
Catch[
Module[{res, bytess, encoding, convention},

  encoding = OptionValue[CharacterEncoding];
  convention = OptionValue["SourceConvention"];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  bytess = ToCharacterCode[ss, "UTF8"];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess, convention];
  ];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res
]]








CodeTokenize[f:File[_String], opts:OptionsPattern[]] :=
Catch[
Module[{tokss},

  tokss = CodeTokenize[{f}, opts];

  If[FailureQ[tokss],
    Throw[tokss]
  ];

  tokss[[1]]
]]

CodeTokenize[fs:{File[_String], File[_String]...}, opts:OptionsPattern[]] :=
Module[{tokss},

  tokss = tokenizeFileListable[fs, opts];

  tokss
]



Options[tokenizeFileListable] = Options[CodeTokenize]

tokenizeFileListable[fs:{File[_String]...}, OptionsPattern[]] :=
Catch[
Module[{encoding, res, fulls, bytess, convention},

  encoding = OptionValue[CharacterEncoding];
  convention = OptionValue["SourceConvention"];

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

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess, convention];
  ];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res
]]







CodeTokenize[bytes:{_Integer, _Integer...}, opts:OptionsPattern[]] :=
Catch[
Module[{tokss},

  tokss = CodeTokenize[{bytes}, opts];

  If[FailureQ[tokss],
    Throw[tokss]
  ];

  tokss[[1]]
]]

CodeTokenize[bytess:{{_Integer, _Integer...}...}, opts:OptionsPattern[]] :=
Module[{tokss},

  tokss = tokenizeBytesListable[bytess, opts];

  tokss
]



Options[tokenizeBytesListable] = Options[CodeTokenize]

tokenizeBytesListable[bytess:{{_Integer...}...}, OptionsPattern[]] :=
Catch[
Module[{encoding, res, convention},

  encoding = OptionValue[CharacterEncoding];
  convention = OptionValue["SourceConvention"];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess, convention];
  ];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

	res
]]



CodeTokenize[{}, opts:OptionsPattern[]] :=
Catch[
Module[{},
  
  {}
]]






CodeConcreteParseLeaf::usage = "CodeConcreteParseLeaf[code] returns a LeafNode by interpreting code as a leaf."

Options[CodeConcreteParseLeaf] = {
  "StringifyMode" -> 0,
  "SourceConvention" -> "LineColumn"
}

CodeConcreteParseLeaf[str_String, opts:OptionsPattern[]] :=
	concreteParseLeaf[str, opts]


Options[concreteParseLeaf] = Options[CodeConcreteParseLeaf]

concreteParseLeaf[strIn_String, OptionsPattern[]] :=
Catch[
Module[{str, res, leaf, data, exprs, issues, stringifyMode, convention},

  str = strIn;

  stringifyMode = OptionValue["StringifyMode"];
  convention = OptionValue["SourceConvention"];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[concreteParseLeafFunc, str, stringifyMode, convention];
  ];

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

  e = CodeParser`Library`Private`$exprCompiledLibFuns["Expr_FromPointer"][p];

  CodeParser`Library`Private`$exprCompiledLibFuns["Expr_Release"][e];

  e
]




End[]

EndPackage[]
