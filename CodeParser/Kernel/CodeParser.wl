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
Used with boxes
*)
GroupLinearSyntax

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
GroupMissingCloserNode
UnterminatedGroupNeedsReparseNode
(*
GroupMissingOpenerNode is only used in Boxes
*)
GroupMissingOpenerNode
AbstractSyntaxErrorNode
CallMissingCloserNode


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


setupQuirks[]

setupShims[]


(*
CodeConcreteParse
*)

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
  ContainerNode -> Automatic,
  "AlreadyHasEOFSentinel" -> False
}


CodeConcreteParse[s_String, opts:OptionsPattern[]] :=
  codeConcreteParse[s, CodeConcreteParse, opts]

codeConcreteParse[s_String, func_, opts:OptionsPattern[]] :=
Catch[
Module[{cst, bytes, encoding, fileFormat, firstLineBehavior},

  encoding = OptionValue[func, {opts}, CharacterEncoding];
  fileFormat = OptionValue[func, {opts}, "FileFormat"];

  If[fileFormat === Automatic,
    fileFormat = "Unknown"
  ];

  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  bytes = ByteArray[ToCharacterCode[s, "UTF-8"]];

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

  cst = concreteParseString[bytes, firstLineBehavior, func, opts];

  If[FailureQ[cst],
    Throw[cst]
  ];

  Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

    UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
    UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

    (
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::SelfAssignment:: *)
    cst = cst
    (* :!CodeAnalysis::EndBlock:: *)
    );
  ];
  
  cst
]]


concreteParseString[{}, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, container, containerWasAutomatic},

  container = OptionValue[func, {opts}, ContainerNode];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {simple line conts}, {complex line conts}, {embedded newlines}, {embedded tabs} }
  *)
  If[container === Automatic,
    containerWasAutomatic = True;
    container = ContainerNode[String, #[[1]],
      <| If[!FailureQ[#[[2]]] && !empty[#[[2]]], SyntaxIssues -> #[[2]], Nothing],
         If[!FailureQ[#[[3]]] && !empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
         If[!FailureQ[#[[4]]] && !empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
         If[!FailureQ[#[[5]]] && !empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
         If[!FailureQ[#[[6]]] && !empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing] |>
    ]&
  ];

  res = {{}, {}, {}, {}, {}, {}};

  res = container[res];

  (*
  Fill in Source for FileNode now
  *)
  If[containerWasAutomatic,
    res = fillinSource[res]
  ];

  res
]]

concreteParseString[bytes_ByteArray?ByteArrayQ, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, convention, container, tabWidth,
  containerWasAutomatic},

  convention = OptionValue[func, {opts}, SourceConvention];
  container = OptionValue[func, {opts}, ContainerNode];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {simple line conts}, {complex line conts}, {embedded newlines}, {embedded tabs} }
  *)
  If[container === Automatic,
    containerWasAutomatic = True;
    container = ContainerNode[String, #[[1]],
      <| If[!FailureQ[#[[2]]] && !empty[#[[2]]], SyntaxIssues -> #[[2]], Nothing],
         If[!FailureQ[#[[3]]] && !empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
         If[!FailureQ[#[[4]]] && !empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
         If[!FailureQ[#[[5]]] && !empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
         If[!FailureQ[#[[6]]] && !empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing] |>
    ]&
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  res = libraryFunctionWrapper[concreteParseBytesFunc, $ParserSession, bytes, sourceConventionToInteger[convention], tabWidth, firstLineBehaviorToInteger[firstLineBehavior], False];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res = container[res];

  (*
  Fill in Source for FileNode now
  *)
  If[containerWasAutomatic,
    res = fillinSource[res]
  ];

  res
]]


CodeConcreteParse[f:File[_String], opts:OptionsPattern[]] :=
  codeConcreteParse[f, CodeConcreteParse, opts]

codeConcreteParse[f:File[_String], func_, opts:OptionsPattern[]] :=
Catch[
Module[{cst, encoding, full, bytes, fileFormat, firstLineBehavior,
  ext},

  encoding = OptionValue[func, {opts}, CharacterEncoding];
  fileFormat = OptionValue[func, {opts}, "FileFormat"];

  If[fileFormat === Automatic,
    ext = FileExtension[f];
    Which[
      ext == "wl" || ext == "m",
        fileFormat = "Package"
      ,
      ext == "wls",
        fileFormat = "Script"
      ,
      ext == "nb",
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
  full = FindFile[f];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <| "File" -> f |>]]
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

  cst = concreteParseFile[full, firstLineBehavior, func, opts];

  If[FailureQ[cst],
    If[cst === $Failed,
      Throw[cst]
    ];
    cst = Insert[cst, "FileName" -> full, {2, -1}];
    Throw[cst]
  ];

  cst = Insert[cst, "FileName" -> full, {3, -1}];

  Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

    (*
    Was:
    bytes = Import[full, "Byte"];

    but this is slow
    *)
    bytes = ReadByteArray[full];

    UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
    UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

    (
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::SelfAssignment:: *)
    cst = cst
    (* :!CodeAnalysis::EndBlock:: *)
    );
  ];

  cst
]]

concreteParseFile[full_String, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, convention, container, containerWasAutomatic,
  tabWidth},

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
      <| If[!FailureQ[#[[2]]] && !empty[#[[2]]], SyntaxIssues -> #[[2]], Nothing],
         If[!FailureQ[#[[3]]] && !empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
         If[!FailureQ[#[[4]]] && !empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
         If[!FailureQ[#[[5]]] && !empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
         If[!FailureQ[#[[6]]] && !empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing] |>
    ]&
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  res = libraryFunctionWrapper[concreteParseFileFunc, $ParserSession, full, sourceConventionToInteger[convention], tabWidth, firstLineBehaviorToInteger[firstLineBehavior]];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res = container[res];

  (*
  Fill in Source for FileNode now
  *)
  If[containerWasAutomatic,
    res = fillinSource[res]
  ];

  res
]]


fillinSource[cstIn_] :=
Catch[
Module[{cst, children, first, last, start, end, data},

  cst = cstIn;

  children = cst[[2]];
  
  If[FailureQ[children],
    Throw[cst]
  ];

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


CodeConcreteParse[bytes:{_Integer...}, opts:OptionsPattern[]] :=
  codeConcreteParse[bytes, CodeConcreteParse, opts]


codeConcreteParse[bytesIn:{_Integer...}, func_, opts:OptionsPattern[]] :=
Catch[
Module[{cst, encoding, fileFormat, firstLineBehavior, bytes},

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

  bytes = ByteArray[bytesIn];

  cst = concreteParseBytes[bytes, firstLineBehavior, func, opts];

  If[FailureQ[cst],
    Throw[cst]
  ];

  Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

    UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
    UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

    (
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::SelfAssignment:: *)
    cst = cst
    (* :!CodeAnalysis::EndBlock:: *)
    );
  ];

  cst
]]


concreteParseBytes[{}, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, container, containerWasAutomatic},

  container = OptionValue[func, {opts}, ContainerNode];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {simple line conts}, {complex line conts}, {embedded newlines}, {embedded tabs} }
  *)
  If[container === Automatic,
    containerWasAutomatic = True;
    container = ContainerNode[Byte, #[[1]],
      <| If[!FailureQ[#[[2]]] && !empty[#[[2]]], SyntaxIssues -> #[[2]], Nothing],
         If[!FailureQ[#[[3]]] && !empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
         If[!FailureQ[#[[4]]] && !empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
         If[!FailureQ[#[[5]]] && !empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
         If[!FailureQ[#[[6]]] && !empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing] |>
    ]&
  ];

  res = {{}, {}, {}, {}, {}, {}};

  res = container[res];

  (*
  Fill in Source for FileNode now
  *)
  If[containerWasAutomatic,
    res = fillinSource[res]
  ];

  res
]]

concreteParseBytes[bytes_ByteArray?ByteArrayQ, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, convention, container, tabWidth, containerWasAutomatic,
  alreadyHasEOFSentinel},

  convention = OptionValue[func, {opts}, SourceConvention];
  container = OptionValue[func, {opts}, ContainerNode];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];
  alreadyHasEOFSentinel = OptionValue[func, {opts}, "AlreadyHasEOFSentinel"];

  If[!MatchQ[alreadyHasEOFSentinel, True | False],
    Throw[Failure["AlreadyHasEOFSentinelIsNotTrueOrFalse", <| "AlreadyHasEOFSentinel" -> alreadyHasEOFSentinel |>]]
  ];

  If[alreadyHasEOFSentinel && bytes[[-1]] != 255,
    Throw[Failure["EOFSentinelIsNotPresent", <||>]]
  ];

  (*
  The <||> will be filled in with Source later
  The # here is { {exprs}, {issues}, {simple line conts}, {complex line conts}, {embedded newlines}, {embedded tabs} }
  *)
  If[container === Automatic,
    containerWasAutomatic = True;
    container = ContainerNode[Byte, #[[1]],
      <| If[!FailureQ[#[[2]]] && !empty[#[[2]]], SyntaxIssues -> #[[2]], Nothing],
         If[!FailureQ[#[[3]]] && !empty[#[[3]]], "SimpleLineContinuations" -> #[[3]], Nothing],
         If[!FailureQ[#[[4]]] && !empty[#[[4]]], "ComplexLineContinuations" -> #[[4]], Nothing],
         If[!FailureQ[#[[5]]] && !empty[#[[5]]], "EmbeddedNewlines" -> #[[5]], Nothing],
         If[!FailureQ[#[[6]]] && !empty[#[[6]]], "EmbeddedTabs" -> #[[6]], Nothing] |>
    ]&
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  res = libraryFunctionWrapper[concreteParseBytesFunc, $ParserSession, bytes, sourceConventionToInteger[convention], tabWidth, firstLineBehaviorToInteger[firstLineBehavior], alreadyHasEOFSentinel];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res = container[res];

  (*
  Fill in Source for FileNode now
  *)
  If[containerWasAutomatic,
    res = fillinSource[res]
  ];
  
  res
]]


(*
CodeParse
*)

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
  ContainerNode -> Automatic,
  "AlreadyHasEOFSentinel" -> False
}


CodeParse[s_String, opts:OptionsPattern[]] :=
Catch[
Module[{cst, ast, agg},
  
  cst = codeConcreteParse[s, CodeParse, opts];

  agg = Aggregate[cst];

  ast = Abstract[agg];

  ast
]]


CodeParse[f:File[_String], opts:OptionsPattern[]] :=
Catch[
Module[{cst, ast, agg},

  cst = codeConcreteParse[f, CodeParse, opts];

  agg = Aggregate[cst];

  ast = Abstract[agg];

  ast
]]


CodeParse[bytes:{_Integer...}, opts:OptionsPattern[]] :=
Catch[
Module[{cst, ast, agg},

  cst = codeConcreteParse[bytes, CodeParse, opts];

  agg = Aggregate[cst];

  ast = Abstract[agg];

  ast
]]



(*
CodeTokenize
*)

CodeTokenize::usage = "CodeTokenize[code] returns a list of tokens by interpreting code as WL input. \
code can be a string, a File, or a list of bytes."

Options[CodeTokenize] = {
  CharacterEncoding -> "UTF-8",
  SourceConvention -> "LineColumn",
  "TabWidth" -> 1,
  "FileFormat" -> Automatic,
  "AlreadyHasEOFSentinel" -> False
}


CodeTokenize[s_String, opts:OptionsPattern[]] :=
Catch[
Module[{toks, encoding, bytes},

  encoding = OptionValue[CodeTokenize, {opts}, CharacterEncoding];

  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  bytes = ByteArray[ToCharacterCode[s, "UTF-8"]];

  toks = tokenizeString[bytes, NotScript, CodeTokenize, opts];

  If[FailureQ[toks],
    Throw[toks]
  ];

  Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

    UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
    UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

    (
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::SelfAssignment:: *)
    toks = toks
    (* :!CodeAnalysis::EndBlock:: *)
    );
  ];

  toks
]]


tokenizeString[{}, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
  {}

tokenizeString[bytes_ByteArray?ByteArrayQ, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, convention, tabWidth},

  convention = OptionValue[func, {opts}, SourceConvention];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  res = libraryFunctionWrapper[tokenizeBytesFunc, $ParserSession, bytes, sourceConventionToInteger[convention], tabWidth, firstLineBehaviorToInteger[firstLineBehavior], False];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res
]]


CodeTokenize[f:File[_String], opts:OptionsPattern[]] :=
Catch[
Module[{toks, encoding, full, bytes, fileFormat, firstLineBehavior,
  ext},

  encoding = OptionValue[CodeTokenize, {opts}, CharacterEncoding];

  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  fileFormat = OptionValue[CodeTokenize, {opts}, "FileFormat"];

  If[fileFormat === Automatic,
    ext = FileExtension[f];
    Which[
      ext == "wl" || ext == "m",
        fileFormat = "Package"
      ,
      ext == "wls",
        fileFormat = "Script"
      ,
      ext == "nb",
        fileFormat = "Notebook"
      ,
      True,
        fileFormat = "Unknown"
    ]
  ];

  full = FindFile[f];
  If[FailureQ[full],
    Throw[Failure["FindFileFailed", <| "File" -> f |>]]
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

  toks = tokenizeFile[full, firstLineBehavior, CodeTokenize, opts];

  If[FailureQ[toks],
    Throw[toks]
  ];

  Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

    bytes = ReadByteArray[full];

    UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
    UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

    (
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::SelfAssignment:: *)
    toks = toks
    (* :!CodeAnalysis::EndBlock:: *)
    );
  ];

  toks
]]

tokenizeFile[full_String, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, convention, tabWidth},

  convention = OptionValue[func, {opts}, SourceConvention];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  res = libraryFunctionWrapper[tokenizeFileFunc, $ParserSession, full, sourceConventionToInteger[convention], tabWidth, firstLineBehaviorToInteger[firstLineBehavior]];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res
]]


CodeTokenize[bytesIn:{_Integer...}, opts:OptionsPattern[]] :=
Catch[
Module[{toks, encoding, bytes},

  encoding = OptionValue[CodeTokenize, {opts}, CharacterEncoding];

  If[encoding =!= "UTF-8",
    Throw[Failure["OnlyUTF8Supported", <| "CharacterEncoding" -> encoding |>]]
  ];

  bytes = ByteArray[bytesIn];

  toks = tokenizeBytes[bytes, NotScript, CodeTokenize, opts];

  If[FailureQ[toks],
    Throw[toks]
  ];

  Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

    UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
    UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

    (
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::SelfAssignment:: *)
    toks = toks
    (* :!CodeAnalysis::EndBlock:: *)
    );
  ];

  toks
]]


tokenizeBytes[{}, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
  {}

tokenizeBytes[bytes_ByteArray?ByteArrayQ, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, convention, tabWidth, alreadyHasEOFSentinel},

  convention = OptionValue[func, {opts}, SourceConvention];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];
  alreadyHasEOFSentinel = OptionValue[func, {opts}, "AlreadyHasEOFSentinel"];

  If[!MatchQ[alreadyHasEOFSentinel, True | False],
    Throw[Failure["AlreadyHasEOFSentinelIsNotTrueOrFalse", <| "AlreadyHasEOFSentinel" -> alreadyHasEOFSentinel |>]]
  ];

  If[alreadyHasEOFSentinel && bytes[[-1]] != 255,
    Throw[Failure["EOFSentinelIsNotPresent", <||>]]
  ];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  res = libraryFunctionWrapper[tokenizeBytesFunc, $ParserSession, bytes, sourceConventionToInteger[convention], tabWidth, firstLineBehaviorToInteger[firstLineBehavior], alreadyHasEOFSentinel];

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



(*
CodeConcreteParseLeaf
*)

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


concreteParseLeaf[str_String, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, leaf, data, exprs, bytes},

  bytes = ByteArray[ToCharacterCode[str, "UTF-8"]];

  res = concreteParseLeafString[bytes, NotScript, CodeConcreteParseLeaf, opts];

  If[FailureQ[res],
    Throw[res]
  ];

  exprs = res[[1]];

  If[FailureQ[exprs],
    Throw[exprs]
  ];

  leaf = exprs[[1]];

  Block[{UnterminatedGroupNeedsReparseNode, UnterminatedTokenErrorNeedsReparseNode},

    UnterminatedGroupNeedsReparseNode[args___] := reparseUnterminatedGroupNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedGroupNode]]];
    UnterminatedTokenErrorNeedsReparseNode[args___] := reparseUnterminatedTokenErrorNode[{args}, bytes, FilterRules[{opts}, Options[reparseUnterminatedTokenErrorNode]]];

    (
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::SelfAssignment:: *)
    leaf = leaf
    (* :!CodeAnalysis::EndBlock:: *)
    );
  ];

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


concreteParseLeafString[{}, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res},

  res = {{Missing["EmptyInput"]}, {}, {}, {}, {}, {}};

  res
]]

concreteParseLeafString[bytes_ByteArray?ByteArrayQ, firstLineBehavior:firstLineBehaviorPat, func_, opts:OptionsPattern[]] :=
Catch[
Module[{res, stringifyMode, convention, tabWidth, encodingMode},

  stringifyMode = OptionValue[func, {opts}, "StringifyMode"];
  convention = OptionValue[func, {opts}, SourceConvention];
  tabWidth = OptionValue[func, {opts}, "TabWidth"];
  encodingMode = OptionValue[func, {opts}, "EncodingMode"];

  $ConcreteParseProgress = 0;
  $ConcreteParseStart = Now;
  $ConcreteParseTime = Quantity[0, "Seconds"];

  res = libraryFunctionWrapper[concreteParseLeafFunc, $ParserSession, bytes, stringifyMode, sourceConventionToInteger[convention], tabWidth, firstLineBehaviorToInteger[firstLineBehavior], encodingMode];

  $ConcreteParseProgress = 100;
  $ConcreteParseTime = Now - $ConcreteParseStart;

  If[FailureQ[res],
    Throw[res]
  ];

  res
]]


(*
SafeString
*)

SafeString::usage = "SafeString[bytes] interprets bytes as UTF-8 and returns a string of the decoded bytes if it is safe. \
Otherwise, Missing[\"UnsafeCharacterEncoding\"] is returned. \
A string is safe if there are no incomplete sequences, stray surrogates, or BOM present."

(*
ByteArray[{}] returns {}
*)
SafeString[{}] :=
  ""

SafeString[bytes:{_Integer...}] :=
  SafeString[ByteArray[bytes]]

(*
ReadByteArray[emptyFile] returns EndOfFile
*)
SafeString[EndOfFile] :=
  ""

SafeString[bytes_ByteArray?ByteArrayQ] :=
Catch[
Module[{res},

  res = libraryFunctionWrapper[safeStringFunc, $ParserSession, bytes];

  If[FailureQ[res],
    Throw[res]
  ];

  res = res[[1]];

  res
]]



CodeSyntaxQ[code_] :=
Module[{ast},
  ast = CodeParse[code];
  FreeQ[ast,
    ErrorNode |
    SyntaxErrorNode | AbstractSyntaxErrorNode |
    GroupMissingCloserNode | GroupMissingOpenerNode |
    CallMissingCloserNode |
    _Missing] &&
  !MemberQ[Lookup[ast[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]
]

CodeSyntaxCSTQ[cst_] :=
  FreeQ[cst,
    ErrorNode |
    SyntaxErrorNode |
    GroupMissingCloserNode | GroupMissingOpenerNode |
    _Missing] &&
  !MemberQ[Lookup[cst[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]

CodeStructuralSyntaxQ[code_] :=
Module[{ast},
  ast = CodeParse[code];
  FreeQ[ast,
    SyntaxErrorNode | AbstractSyntaxErrorNode |
    GroupMissingCloserNode | GroupMissingOpenerNode |
    CallMissingCloserNode |
    _Missing] &&
  !MemberQ[Lookup[ast[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]
]

CodeStructuralSyntaxCSTQ[cst_] :=
  FreeQ[cst,
    SyntaxErrorNode |
    GroupMissingCloserNode | GroupMissingOpenerNode |
    _Missing] &&
  !MemberQ[Lookup[cst[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]

CodeStructuralSyntaxAggQ[agg_] :=
  FreeQ[agg,
    SyntaxErrorNode |
    GroupMissingCloserNode | GroupMissingOpenerNode |
    _Missing] &&
  !MemberQ[Lookup[agg[[3]], SyntaxIssues, {}], EncodingIssue[_, _, "Fatal", _]]



firstLineBehaviorToInteger[NotScript] = 0
firstLineBehaviorToInteger[Check] = 1
firstLineBehaviorToInteger[Script] = 2


sourceConventionToInteger["LineColumn"] = 0
sourceConventionToInteger["SourceCharacterIndex"] = 1


End[]

EndPackage[]
