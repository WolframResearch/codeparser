BeginPackage["CodeParser`"]

CodeParse

CodeConcreteParse

CodeConcreteParseBox

CodeConcreteParseLeaf

SafeString



CodeTokenize



CodeSyntaxQ

CodeSyntaxCSTQ

CodeStructuralSyntaxQ

CodeStructuralSyntaxCSTQ

CodeStructuralSyntaxAggQ



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

MemoizedSetDelayed
MemoizedTagSetDelayed

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

(*
Shebang
*)

(* option symbols *)
Source
SourceConvention
Synthesized

(*
Used in parsing boxes
*)
Comment
Metadata

Intra

(*
Used in parsing boxes
*)
TernaryOptionalPattern


(* node symbols *)
LeafNode
ErrorNode
UnterminatedTokenErrorNeedsReparseNode
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

CompoundNode

ContainerNode


SyntaxErrorNode
(*

what is the difference between GroupMissingCloserNode and UnterminatedGroupNode?


GroupMissingCloserNode is a parse that has been "recovered" but UnterminatedGroupNode has hit the end of the input


Parsing:

{ ) }

gets parsed as GroupMissingCloserNode[ { ] UnexpectedCloser[ ) ] UnexpectedCloser[ } ]


But just parsing:

{

gets parsed as UnterminatedGroupNode[ { ]

*)
GroupMissingCloserNode
UnterminatedGroupNode
UnterminatedGroupNeedsReparseNode
(*
GroupMissingOpenerNode is only used in Boxes
*)
GroupMissingOpenerNode
AbstractSyntaxErrorNode
CallMissingCloserNode
UnterminatedCallNode


(*
These are not (yet) used by the parser directly, but it is
good to have them in a central place
*)
QuaternaryNode
FragmentNode



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


ExprTest

GetMetadata


(*
Messages
*)
CodeParser


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
Needs["CodeParser`Scoping`"]
Needs["CodeParser`Shims`"]
Needs["CodeParser`ToString`"]
Needs["CodeParser`Utils`"]



CodeParser::old = "The old AST paclet has been renamed to CodeParser. Uninstall AST paclet from your system."

If[PacletFind["AST"] != {},
  Message[CodeParser::old]
]



$DefaultTabWidth = 1



(*
This uses func := func = def idiom and is fast
*)
loadAllFuncs[]

setupQuirks[]

setupShims[]



CodeConcreteParse::usage = "CodeConcreteParse[code] returns a concrete syntax tree by interpreting code as WL input. \
code can be a string, a file, or a list of bytes."

Options[CodeConcreteParse] = {
  CharacterEncoding -> "UTF8",
  SourceConvention -> "LineColumn",
  "TabWidth" :> $DefaultTabWidth,
  ContainerNode -> Automatic,
  "FileFormat" -> Automatic
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
Module[{csts, bytess, encoding, fileFormat, firstLineIsShebang},

  encoding = OptionValue[CharacterEncoding];
  fileFormat = OptionValue["FileFormat"];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  bytess = ToCharacterCode[ss, "UTF8"];

  Switch[fileFormat,
    "Script",
      firstLineIsShebang = True
    ,
    _,
      firstLineIsShebang = False
  ];

  csts = concreteParseStringListable[bytess, firstLineIsShebang, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  csts =
    MapThread[Function[{cst, bytes},
      
        Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

          UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
          UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

          cst
        ]
      ]
      ,
      {csts, bytess}
    ];

  csts
]]


Options[concreteParseStringListable] = Options[CodeConcreteParse]

concreteParseStringListable[bytess:{{_Integer...}...}, firstLineIsShebang_, OptionsPattern[]] :=
Catch[
Module[{res, convention, container, tabWidth},

  convention = OptionValue[SourceConvention];
  container = OptionValue[ContainerNode];
  tabWidth = OptionValue["TabWidth"];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {simple line conts}, {complex line conts}, {embedded newlines}, {embedded tabs} }
  *)
  If[container === Automatic,
    container = ContainerNode[String, #[[1]],
      <| If[!empty[#[[2]]], SyntaxIssues -> #[[2]], Nothing],
         If[!empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
         If[!empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
         If[!empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
         If[!empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing] |>
    ]&
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[concreteParseBytesListableFunc, bytess, convention, tabWidth, Boole[firstLineIsShebang]];
  ];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res = container /@ res;

  res
]]





CodeParse::usage = "CodeParse[code] returns an abstract syntax tree by interpreting code as WL input. \
code can be a string, a file, or a list of bytes."

Options[CodeParse] = {
  CharacterEncoding -> "UTF8",
  SourceConvention -> "LineColumn",
  "TabWidth" :> $DefaultTabWidth,
  ContainerNode -> Automatic,
  "FileFormat" -> Automatic
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
Module[{csts, encoding, fulls, bytess, fileFormat, firstLineIsShebang, exts},

  encoding = OptionValue[CharacterEncoding];
  fileFormat = OptionValue["FileFormat"];

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

  Switch[fileFormat,
    "Script",
      firstLineIsShebang = True
    ,
    Automatic,
      exts = FileExtension /@ fs;
      Which[
        AnyTrue[exts, (# == "wls")&],
          firstLineIsShebang = True
        ,
        True,
          firstLineIsShebang = False
      ]
    ,
    _,
      firstLineIsShebang = False
  ];

  (*
  Was:
  bytess = Import[#, "Byte"]& /@ fulls;

  but this is slow
  *)
  bytess = (Normal[ReadByteArray[#]] /. EndOfFile -> {})& /@ fulls;

  csts = concreteParseFileListable[bytess, firstLineIsShebang, opts];

  If[FailureQ[csts],
    If[csts === $Failed,
      Throw[csts]
    ];
    csts = Failure[csts[[1]], Join[csts[[2]], <|"FileNames"->fs|>]];
    Throw[csts]
  ];

  csts =
    MapThread[Function[{cst, bytes},
    
        Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

          UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
          UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

          cst
        ]
      ]
      ,
      {csts, bytess}
    ];

  csts
]]



Options[concreteParseFileListable] = Options[CodeConcreteParse]

concreteParseFileListable[bytess:{{_Integer...}...}, firstLineIsShebang_, OptionsPattern[]] :=
Catch[
Module[{res, convention, container, containerWasAutomatic, tabWidth},

  convention = OptionValue[SourceConvention];
  container = OptionValue[ContainerNode];
  tabWidth = OptionValue["TabWidth"];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {simple line conts}, {complex line conts}, {embedded newlines}, {embedded tabs} }
  *)
  If[container === Automatic,
    containerWasAutomatic = True;
    container = ContainerNode[File, #[[1]],
      <| If[!empty[#[[2]]], SyntaxIssues -> #[[2]], Nothing],
         If[!empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
         If[!empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
         If[!empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
         If[!empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing] |>
    ]&
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[concreteParseBytesListableFunc, bytess, convention, tabWidth, Boole[firstLineIsShebang]];
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
Module[{csts, encoding, fileFormat, firstLineIsShebang},

  encoding = OptionValue[CharacterEncoding];
  fileFormat = OptionValue["FileFormat"];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  Switch[fileFormat,
    "Script",
      firstLineIsShebang = True
    ,
    _,
      firstLineIsShebang = False
  ];

  csts = concreteParseBytesListable[bytess, firstLineIsShebang, opts];

  If[FailureQ[csts],
    Throw[csts]
  ];

  csts =
    MapThread[Function[{cst, bytes},
      
        Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

          UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
          UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

          cst
        ]

      ]
      ,
      {csts, bytess}
    ];

  csts
]]



Options[concreteParseBytesListable] = Options[CodeConcreteParse]

concreteParseBytesListable[bytess:{{_Integer...}...}, firstLineIsShebang_, OptionsPattern[]] :=
Catch[
Module[{res, convention, container, tabWidth},

  convention = OptionValue[SourceConvention];
  container = OptionValue[ContainerNode];
  tabWidth = OptionValue["TabWidth"];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {simple line conts}, {complex line conts}, {embedded newlines}, {embedded tabs} }
  *)
  If[container === Automatic,
    container = ContainerNode[Byte, #[[1]],
      <| If[!empty[#[[2]]], SyntaxIssues -> #[[2]], Nothing],
         If[!empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
         If[!empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
         If[!empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
         If[!empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing] |>
    ]&
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[concreteParseBytesListableFunc, bytess, convention, tabWidth, Boole[firstLineIsShebang]];
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






CodeTokenize::usage = "CodeTokenize[code] returns a list of tokens by interpreting code as WL input. \
code can be a string, a file, or a list of bytes."

Options[CodeTokenize] = {
  CharacterEncoding -> "UTF8",
  SourceConvention -> "LineColumn",
  "TabWidth" :> $DefaultTabWidth,
  "FileFormat" -> Automatic
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
Module[{res, bytess, encoding, convention, tabWidth},

  encoding = OptionValue[CharacterEncoding];
  convention = OptionValue[SourceConvention];
  tabWidth = OptionValue["TabWidth"];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  bytess = ToCharacterCode[ss, "UTF8"];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess, convention, tabWidth, Boole[False]];
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
Module[{encoding, res, fulls, bytess, convention, tabWidth, fileFormat, firstLineIsShebang, exts},

  encoding = OptionValue[CharacterEncoding];
  convention = OptionValue[SourceConvention];
  tabWidth = OptionValue["TabWidth"];
  fileFormat = OptionValue["FileFormat"];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  fulls = FindFile /@ fs;
  If[AnyTrue[fulls, FailureQ],
    Throw[Failure["FindFileFailed", <|"FileNames"->fs|>]]
  ];

  Switch[fileFormat,
    "Script",
      firstLineIsShebang = True
    ,
    Automatic,
      exts = FileExtension /@ fs;
      Which[
        AnyTrue[exts, (# == "wls")&],
          firstLineIsShebang = True
        ,
        True,
          firstLineIsShebang = False
      ]
    ,
    _,
      firstLineIsShebang = False
  ];

  (*
  Was:
  bytess = Import[#, "Byte"]& /@ fulls;

  but this is slow
  *)
  bytess = (Normal[ReadByteArray[#]] /. EndOfFile -> {})& /@ fulls;

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess, convention, tabWidth, Boole[firstLineIsShebang]];
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
Module[{encoding, res, convention, tabWidth},

  encoding = OptionValue[CharacterEncoding];
  convention = OptionValue[SourceConvention];
  tabWidth = OptionValue["TabWidth"];

  If[encoding =!= "UTF8",
    Throw[Failure["OnlyUTF8Supported", <|"CharacterEncoding"->encoding|>]]
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess, convention, tabWidth, Boole[False]];
  ];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];


  (*
  LineContinuation and EmbeddedNewline data is not returned here
  *)

	res
]]



CodeTokenize[{}, opts:OptionsPattern[]] :=
Catch[
Module[{},
  
  {}
]]






CodeConcreteParseLeaf::usage = "CodeConcreteParseLeaf[code] returns a LeafNode by interpreting code as a leaf. \
code can be a string."

Options[CodeConcreteParseLeaf] = {
  "StringifyMode" -> 0,
  SourceConvention -> "LineColumn",
  "TabWidth" :> $DefaultTabWidth
}

CodeConcreteParseLeaf[str_String, opts:OptionsPattern[]] :=
	concreteParseLeaf[str, opts]


Options[concreteParseLeaf] = Options[CodeConcreteParseLeaf]

concreteParseLeaf[strIn_String, OptionsPattern[]] :=
Catch[
Module[{str, res, leaf, data, exprs, stringifyMode, convention, tabWidth},

  str = strIn;

  stringifyMode = OptionValue["StringifyMode"];
  convention = OptionValue[SourceConvention];
  tabWidth = OptionValue["TabWidth"];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[concreteParseLeafFunc, str, stringifyMode, convention, tabWidth, Boole[False]];
  ];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  exprs = res[[1]];

  leaf = exprs[[1]];

  If[!empty[res[[2]]],
    data = leaf[[3]];
    data[SyntaxIssues] = res[[2]];
    leaf[[3]] = data;
  ];

  (*
  If[!empty[res[[3]]],
    data = leaf[[3]];
    data["SimpleLineContinuations"] = res[[3]];
    leaf[[3]] = data;
  ];

  If[!empty[res[[4]]],
    data = leaf[[3]];
    data["ComplexLineContinuations"] = res[[4]];
    leaf[[3]] = data;
  ];

  If[!empty[res[[5]]],
    data = leaf[[3]];
    data["EmbeddedNewlines"] = res[[5]];
    leaf[[3]] = data;
  ];

  If[!empty[res[[6]]],
    data = leaf[[3]];
    data["EmbeddedTabs"] = res[[6]];
    leaf[[3]] = data;
  ];
  *)
  
  leaf
]]








SafeString::usage = "SafeString[bytes] interprets bytes as UTF-8 and returns a \"safe\" string. \
Invalid sequences and surrogates are replaced with \\[UnknownGlyph] and BOM is replaced with special character \\:e001."

SafeString[bytes:{_Integer...}] :=
Module[{res},
  res = libraryFunctionWrapper[safeStringFunc, bytes];
  res
]





CodeSyntaxQ[code_] :=
  Module[{ast},
    ast = CodeParse[code];
    FreeQ[ast,
      ErrorNode |
      SyntaxErrorNode | AbstractSyntaxErrorNode |
      GroupMissingCloserNode | UnterminatedGroupNode |
      CallMissingCloserNode | UnterminatedCallNode] &&
    !MemberQ[Lookup[ast[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]
  ]

CodeSyntaxCSTQ[cst_] :=
  FreeQ[cst,
    ErrorNode |
    SyntaxErrorNode |
    GroupMissingCloserNode | UnterminatedGroupNode] &&
  !MemberQ[Lookup[cst[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]

CodeStructuralSyntaxQ[code_] :=
  Module[{ast},
    ast = CodeParse[code];
    FreeQ[ast,
      SyntaxErrorNode | AbstractSyntaxErrorNode |
      GroupMissingCloserNode | UnterminatedGroupNode |
      CallMissingCloserNode | UnterminatedCallNode] &&
    !MemberQ[Lookup[ast[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]
  ]

CodeStructuralSyntaxCSTQ[cst_] :=
  FreeQ[cst,
    SyntaxErrorNode |
    GroupMissingCloserNode | UnterminatedGroupNode] &&
  !MemberQ[Lookup[cst[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]

CodeStructuralSyntaxAggQ[agg_] :=
  FreeQ[agg,
    SyntaxErrorNode |
    GroupMissingCloserNode | UnterminatedGroupNode] &&
  !MemberQ[Lookup[agg[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]





ExprTest[] :=
Module[{p, e},

  p = libraryFunctionWrapper[exprTestFunc];

  e = CodeParser`Library`Private`$exprCompiledLibFuns["Expr_FromPointer"][p];

  CodeParser`Library`Private`$exprCompiledLibFuns["Expr_Release"][e];

  e
]

GetMetadata[expr_] :=
Module[{p, m},

  p = CodeParser`Library`Private`$exprCompiledLibFuns["Expr_Pointer"][expr];

  m = libraryFunctionWrapper[getMetadataFunc, p];

  m
]



End[]

EndPackage[]
