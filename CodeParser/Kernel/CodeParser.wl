(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["CodeParser`"]

CodeParse

CodeConcreteParse

(*
CodeConcreteParseBox exists because it is ambiguous whether or not the string "123" is supposed to
be interpreted as an integer or as a box
*)
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
System`MapApply

TernaryTilde
TernarySlashColon

(*
Used with boxes
*)
InfixTilde

InfixInequality

Comma

MemoizedSetDelayed
MemoizedUpSetDelayed
MemoizedTagSetDelayed

(* group symbols *)
(*List*)

(*Association*)

(*AngleBracket*)

(*Ceiling*)

(*Floor*)

GroupDoubleBracket

GroupSquare

GroupTypeSpecifier

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


(*
Position-related symbols
*)
Intra
(*After*)
CellIndex


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
ClauseNode



InternalInvalid


PackageNode
ContextNode
NewContextPathNode

CellNode

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
Needs["CodeParser`CodeAction`"]
Needs["CodeParser`Definitions`"]
Needs["CodeParser`Error`"]
Needs["CodeParser`Library`"]
Needs["CodeParser`Quirks`"]
Needs["CodeParser`Scoping`"]
Needs["CodeParser`Shims`"]
Needs["CodeParser`TopLevel`"]
Needs["CodeParser`ToString`"]
Needs["CodeParser`Utils`"]



CodeParser::old = "The old AST paclet has been renamed to CodeParser. Uninstall AST paclet from your system."

If[PacletFind["AST"] != {},
  Message[CodeParser::old]
]



(*
first line behavior may be:
NotScript:
Source is a string or something, so if #! is on first line, then do not treat special

Check:
Source is something like .wl file that is being treated as a script
Or source is .wl file that is NOT being treated as a script
#! may be present, or it might not

Script:
Source is a .wls file and there is definitely a #! on first line
*)
firstLineBehaviorPat = NotScript | Check | Script



(*
This uses func := func = def idiom and is fast
*)
loadAllFuncs[]

setupQuirks[]

setupShims[]



CodeConcreteParse::usage = "CodeConcreteParse[code] returns a concrete syntax tree by interpreting code as WL input. \
code can be a string, a File, or a list of bytes."

Options[CodeConcreteParse] = {
  CharacterEncoding -> "UTF-8",
  SourceConvention -> "LineColumn",
  "TabWidth" -> 1,
  "FileFormat" -> Automatic,
  (*
  more obscure options
  *)
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
  codeConcreteParse[ss, CodeConcreteParse, opts]

codeConcreteParse[ss:{_String, _String...}, func_, opts:OptionsPattern[]] :=
Catch[
Module[{csts, bytess, encoding, fileFormat, firstLineBehavior},

  encoding = OptionValue[func, {opts}, CharacterEncoding];
  fileFormat = OptionValue[func, {opts}, "FileFormat"];

  If[fileFormat === Automatic,
    fileFormat = "Unknown"
  ];

  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  bytess = ToCharacterCode[ss, "UTF-8"];

  Switch[fileFormat,
    "Package",
      firstLineBehavior = Check
    ,
    "Script",
      firstLineBehavior = Script
    ,
    _,
      firstLineBehavior = NotScript
  ];

  csts = concreteParseStringListable[bytess, firstLineBehavior, func, opts];

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


concreteParseStringListable[bytess:{{_Integer...}...}, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, convention, container, tabWidth},

  convention = OptionValue[func, {opts}, SourceConvention];
  container = OptionValue[func, {opts}, ContainerNode];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];

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
  res = libraryFunctionWrapper[concreteParseBytesListableFunc, bytess, convention, tabWidth, firstLineBehaviorToInteger[firstLineBehavior]];
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
code can be a string, a File, or a list of bytes."

Options[CodeParse] = {
  CharacterEncoding -> "UTF-8",
  SourceConvention -> "LineColumn",
  "TabWidth" -> 1,
  "FileFormat" -> Automatic,
  (*
  more obscure options
  *)
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
  
  csts = codeConcreteParse[ss, CodeParse, opts];

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
  codeConcreteParse[fs, CodeConcreteParse, opts]

codeConcreteParse[fs:{File[_String], File[_String]...}, func_, opts:OptionsPattern[]] :=
Catch[
Module[{csts, encoding, fulls, bytess, fileFormat, firstLineBehavior, exts},

  encoding = OptionValue[func, {opts}, CharacterEncoding];
  fileFormat = OptionValue[func, {opts}, "FileFormat"];

  If[fileFormat === Automatic,
    exts = FileExtension /@ fs;
    Which[
      AnyTrue[exts, (# == "wl" || # == "m")&],
        fileFormat = "Package"
      ,
      AnyTrue[exts, (# == "wls")&],
        fileFormat = "Script"
      ,
      AnyTrue[exts, (# == "nb")&],
        fileFormat = "Notebook"
      ,
      True,
        fileFormat = "Unknown"
    ]
  ];

  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  (*
  We want to expand anything like ~ before passing to external process

  FindFile does a better job than AbsoluteFileName because it can handle things like "Foo`" also

  FindFile also fails if in sandbox mode
  *)
  fulls = FindFile /@ fs;
  If[AnyTrue[fulls, FailureQ],
    Throw[Failure["FindFileFailed", <| "FileNames" -> fs |>]]
  ];

  Switch[fileFormat,
    "Package",
      firstLineBehavior = Check
    ,
    "Script",
      firstLineBehavior = Script
    ,
    _,
      firstLineBehavior = NotScript
  ];

  (*
  Was:
  bytess = Import[#, "Byte"]& /@ fulls;

  but this is slow
  *)
  bytess = (Normal[ReadByteArray[#]] /. EndOfFile -> {})& /@ fulls;

  csts = concreteParseFileListable[bytess, firstLineBehavior, func, opts];

  If[FailureQ[csts],
    If[csts === $Failed,
      Throw[csts]
    ];
    csts = Failure[csts[[1]], Join[csts[[2]], <| "FileNames" -> fs |>]];
    Throw[csts]
  ];

  csts = MapThread[#1[[0]][#1[[1]], #1[[2]], <| #1[[3]], "FileName" -> #2 |>]&, {csts, fulls}];

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


concreteParseFileListable[bytess:{{_Integer...}...}, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, convention, container, containerWasAutomatic, tabWidth},

  convention = OptionValue[func, {opts}, SourceConvention];
  container = OptionValue[func, {opts}, ContainerNode];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];

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
  res = libraryFunctionWrapper[concreteParseBytesListableFunc, bytess, convention, tabWidth, firstLineBehaviorToInteger[firstLineBehavior]];
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
Catch[
Module[{cst, children, first, last, start, end, data},

  cst = cstIn;

  children = cst[[2]];
  
  (* only fill in if there are actually children nodes to grab *)
  If[empty[children],
    Throw[cst]
  ];

  first = First[children];

  If[MissingQ[first],
    Throw[cst]
  ];

  last = Last[children];

  If[MissingQ[last],
    Throw[cst]
  ];

  start = first[[3, Key[Source], 1]];
  end = last[[3, Key[Source], 2]];
  data = cst[[3]];
  AssociateTo[data, Source -> {start, end}];
  cst[[3]] = data;

  cst
]]





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

  csts = codeConcreteParse[fs, CodeParse, opts];

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
  codeConcreteParse[bytess, CodeConcreteParse, opts]

codeConcreteParse[bytess:{{_Integer, _Integer...}...}, func_, opts:OptionsPattern[]] :=
Catch[
Module[{csts, encoding, fileFormat, firstLineBehavior},

  encoding = OptionValue[func, {opts}, CharacterEncoding];
  fileFormat = OptionValue[func, {opts}, "FileFormat"];

  If[fileFormat === Automatic,
    fileFormat = "Unknown"
  ];

  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  Switch[fileFormat,
    "Package",
      firstLineBehavior = Check
    ,
    "Script",
      firstLineBehavior = Script
    ,
    _,
      firstLineBehavior = NotScript
  ];

  csts = concreteParseBytesListable[bytess, firstLineBehavior, func, opts];

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



concreteParseBytesListable[bytess:{{_Integer...}...}, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, convention, container, tabWidth},

  convention = OptionValue[func, {opts}, SourceConvention];
  container = OptionValue[func, {opts}, ContainerNode];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];

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
  res = libraryFunctionWrapper[concreteParseBytesListableFunc, bytess, convention, tabWidth, firstLineBehaviorToInteger[firstLineBehavior]];
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

  csts = codeConcreteParse[bytess, CodeParse, opts];

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
code can be a string, a File, or a list of bytes."

Options[CodeTokenize] = {
  CharacterEncoding -> "UTF-8",
  SourceConvention -> "LineColumn",
  "TabWidth" -> 1,
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

  tokss = tokenizeStringListable[ss, CodeTokenize, opts];

  tokss
]


tokenizeStringListable[ss:{_String...}, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, bytess, encoding, convention, tabWidth},

  encoding = OptionValue[func, {opts}, CharacterEncoding];
  convention = OptionValue[func, {opts}, SourceConvention];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];

  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  bytess = ToCharacterCode[ss, "UTF-8"];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess, convention, tabWidth, firstLineBehaviorToInteger[NotScript]];
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

  tokss = tokenizeFileListable[fs, CodeTokenize, opts];

  tokss
]



tokenizeFileListable[fs:{File[_String]...}, func_, opts:OptionsPattern[]] :=
Catch[
Module[{encoding, res, fulls, bytess, convention, tabWidth, fileFormat, firstLineBehavior, exts},

  encoding = OptionValue[func, {opts}, CharacterEncoding];
  convention = OptionValue[func, {opts}, SourceConvention];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];
  fileFormat = OptionValue[func, {opts}, "FileFormat"];

  If[fileFormat === Automatic,
    exts = FileExtension /@ fs;
    Which[
      AnyTrue[exts, (# == "wl" || # == "m")&],
        fileFormat = "Package"
      ,
      AnyTrue[exts, (# == "wls")&],
        fileFormat = "Script"
      ,
      AnyTrue[exts, (# == "nb")&],
        fileFormat = "Notebook"
      ,
      True,
        fileFormat = "Unknown"
    ]
  ];
  
  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  fulls = FindFile /@ fs;
  If[AnyTrue[fulls, FailureQ],
    Throw[Failure["FindFileFailed", <| "FileNames" -> fs |>]]
  ];

  Switch[fileFormat,
    "Package",
      firstLineBehavior = Check
    ,
    "Script",
      firstLineBehavior = Script
    ,
    _,
      firstLineBehavior = NotScript
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
  res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess, convention, tabWidth, firstLineBehaviorToInteger[firstLineBehavior]];
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

  tokss = tokenizeBytesListable[bytess, CodeTokenize, opts];

  tokss
]



tokenizeBytesListable[bytess:{{_Integer...}...}, func_, opts:OptionsPattern[]] :=
Catch[
Module[{encoding, res, convention, tabWidth},

  encoding = OptionValue[func, {opts}, CharacterEncoding];
  convention = OptionValue[func, {opts}, SourceConvention];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];

  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[tokenizeBytesListableFunc, bytess, convention, tabWidth, firstLineBehaviorToInteger[NotScript]];
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

(*
StringifyMode:
0: normal
1: symbol segment or a quoted string (RHS of :: or #)
2: file

EncodingMode:
0: normal (strings, files, bytes)
1: boxes
Has the effect of disabling NonASCIICharacter issues for boxes
*)
Options[CodeConcreteParseLeaf] = {
  "StringifyMode" -> 0,
  SourceConvention -> "LineColumn",
  "TabWidth" -> 1,
  "EncodingMode" -> 0
}

CodeConcreteParseLeaf[str_String, opts:OptionsPattern[]] :=
  concreteParseLeaf[str, CodeConcreteParseLeaf, opts]


concreteParseLeaf[strIn_String, func_, opts:OptionsPattern[]] :=
Catch[
Module[{str, res, leaf, data, exprs, stringifyMode, convention, tabWidth, encodingMode},

  str = strIn;

  stringifyMode = OptionValue[func, {opts}, "StringifyMode"];
  convention = OptionValue[func, {opts}, SourceConvention];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];
  encodingMode = OptionValue[func, {opts}, "EncodingMode"];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  Block[{$StructureSrcArgs = parseConvention[convention]},
  res = libraryFunctionWrapper[concreteParseLeafFunc, str, stringifyMode, convention, tabWidth, firstLineBehaviorToInteger[NotScript], encodingMode];
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








SafeString::usage = "SafeString[bytes] interprets bytes as UTF-8 and returns a string of the decoded bytes if it is safe. \
Otherwise, Missing[\"UnsafeCharacterEncoding\"] is returned. \
A string is safe if there are no incomplete sequences, stray surrogates, or BOM present."

SafeString[bytes:{_Integer...}] :=
Catch[
Module[{res, safeStr},
  res = libraryFunctionWrapper[safeStringFunc, bytes];

  If[FailureQ[res],
    Throw[res]
  ];

  safeStr = res[[1]];

  safeStr
]]





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



firstLineBehaviorToInteger[NotScript] = 0
firstLineBehaviorToInteger[Check] = 1
firstLineBehaviorToInteger[Script] = 2 



End[]

EndPackage[]
