(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["CodeParser`Boxes`"]

parseBox



$ProbablyImplicitTimes
$PreserveRowBox

toBeSpliced
toBeSplicedDefinitely


Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`RowBox`"]
Needs["CodeParser`TokenEnum`"]
Needs["CodeParser`Utils`"]


CodeConcreteParse[nb_NotebookObject, opts:OptionsPattern[]] :=
  CodeConcreteParse[NotebookGet[nb], opts]


CodeConcreteParse[c_CellObject, opts:OptionsPattern[]] :=
  CodeConcreteParse[NotebookRead[c], opts]


CodeConcreteParse[Notebook[cells_, ___], opts:OptionsPattern[]] :=
Module[{parsed},
  parsed = MapIndexed[replaceContainerNode[CodeConcreteParse[#1, opts], #2]&, cells];
  ContainerNode[Notebook, parsed, <||>]
]

replaceContainerNode[ContainerNode[Cell, children_, data_], pos_] :=
  CellNode[Cell, children, <| data, CellIndex -> pos |>]

replaceContainerNode[other_, pos_] := other



CodeConcreteParse[Cell[BoxData[box_], _, ___], opts:OptionsPattern[]] :=
Catch[
Module[{parsed},
  parsed = CodeConcreteParseBox[box, FilterRules[{opts}, Options[CodeConcreteParseBox]]];
  
  If[FailureQ[parsed],
    Throw[parsed]
  ];

  (*
  just do simple thing for now and reset Box -> Cell
  FIXME: maybe in the future need to have a wrapper BoxDataNode or something
  *)
  parsed[[1]] = Cell;
  parsed
]]

CodeConcreteParse[c:Cell[___], opts:OptionsPattern[]] :=
  Failure["CannotParseCell", <| "Cell" -> c |>]


CodeConcreteParse[b_RowBox, opts:OptionsPattern[]] :=
  CodeConcreteParseBox[b, FilterRules[{opts}, Options[CodeConcreteParseBox]]]


(*

ParseBox accepts boxes, which are concrete syntax

and returns concrete syntax

The semantics of RowBox are here

But no other box has semantics.
Do not want to reimplement MakeExpression.

*)

CodeConcreteParseBox[boxs_List, opts:OptionsPattern[]] :=
Catch[
Module[{children},

  children = MapIndexed[parseBox[#1, {} ~Join~ #2]&, boxs];

  If[!FreeQ[children, toBeSpliced[_]],

    (*
    toBeSpliced[] is INTERNAL, make sure to handle it before it gets returned
    *)
    children = children //. {
      head_[tag_, {first___, toBeSpliced[children1_ /; FreeQ[children1, toBeSpliced[_]]], last___}, data_] :>
        head[tag, Flatten[{first, reparsePossibleImplicitTimes[tag, children1, data[Source]], last}], data]
      ,
      {first___, toBeSpliced[children1_ /; FreeQ[children1, toBeSpliced[_]]], last___} :>
        Flatten[{first, children1, last}]
      ,
      {first___, toBeSplicedDefinitely[children1_], last___} :>
        Flatten[{first, children1, last}]
    };
  ];

  If[AnyTrue[children, FailureQ],
    Throw[SelectFirst[children, FailureQ]]
  ];

  ContainerNode[Box, children, <||>]
]]


wrapToplevelCompoundExpression[ns:{LeafNode[Token`Newline, _, _]..}] := ns

wrapToplevelCompoundExpression[ns:{___, LeafNode[Token`Semi, _, _], ___}] :=
Module[{aggregatedChildren},

  aggregatedChildren = DeleteCases[ns, LeafNode[Token`Boxes`MultiWhitespace | Token`Newline | Token`Boxes`LineContinuation, _, _] | GroupNode[Comment, _, _]];

  If[MatchQ[aggregatedChildren, {___, LeafNode[Token`Semi, _, _]}],
    {InfixNode[CompoundExpression, ns ~Join~ {LeafNode[Token`Fake`ImplicitNull, "", <||>]}, <||>]}
    ,
    {InfixNode[CompoundExpression, ns, <||>]}
  ]
]

wrapToplevelCompoundExpression[ns_] := ns


CodeConcreteParseBox[box_, opts:OptionsPattern[]] :=
Catch[
Module[{children, child},

  child = parseBox[box, {}];

  If[!FreeQ[child, toBeSpliced[_]],

    (*
    toBeSpliced[] is INTERNAL, make sure to handle it before it gets returned
    *)
    If[MatchQ[child, toBeSpliced[_]],
      children = child[[1]]
      ,
      children = {child}
    ];

    children = children //. {
      head_[tag_, {first___, toBeSpliced[children1_ /; FreeQ[children1, toBeSpliced[_]]], last___}, data_] :>
        head[tag, Flatten[{first, reparsePossibleImplicitTimes[tag, children1, data[Source]], last}], data]
      ,
      {first___, toBeSpliced[children1_ /; FreeQ[children1, toBeSpliced[_]]], last___} :>
        Flatten[{first, children1, last}]
      ,
      {first___, toBeSplicedDefinitely[children1_], last___} :>
        Flatten[{first, children1, last}]
    };
    ,
    children = {child}
  ];

  If[AnyTrue[children, FailureQ],
    Throw[SelectFirst[children, FailureQ]]
  ];

  ContainerNode[Box, children, <||>]
]]


reparsePossibleImplicitTimes[Box | Comment, children_, pos_] :=
Module[{handledChildren, aggregatedChildren},
Block[{$ProbablyImplicitTimes = False},

  handledChildren = children;

  aggregatedChildren = DeleteCases[handledChildren, LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _] | GroupNode[Comment, _, _]];

  prbDispatch[aggregatedChildren, handledChildren, Null, pos] /. {toBeSpliced -> toBeSplicedDefinitely}
]]

(*
Just a line continuation and 1 other thing
*)
reparsePossibleImplicitTimes[tag_, children:{LeafNode[Token`Boxes`LineContinuation, _, _], _}, pos_] :=
Module[{handledChildren, aggregatedChildren},
Block[{$ProbablyImplicitTimes = False},

  handledChildren = children;

  aggregatedChildren = DeleteCases[handledChildren, LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _] | GroupNode[Comment, _, _]];

  prbDispatch[aggregatedChildren, handledChildren, Null, pos] /. {toBeSpliced -> toBeSplicedDefinitely}
]]

reparsePossibleImplicitTimes[tag_, children_, pos_] :=
Module[{handledChildren, aggregatedChildren},
Block[{$ProbablyImplicitTimes = True},

  handledChildren = children;

  aggregatedChildren = DeleteCases[handledChildren, LeafNode[Token`Boxes`MultiWhitespace | Token`Newline, _, _] | GroupNode[Comment, _, _]];

  prbDispatch[aggregatedChildren, handledChildren, Null, pos] /. {toBeSpliced -> toBeSplicedDefinitely}
]]



Options[parseBox] = {
  (*
  0: normal
  1: tag (RHS of :: or #)
  2: file
  *)
  "StringifyMode" -> 0
}


(*
This is reached from within 
*)

parseBox[Cell[d:BoxData[_], rest___], pos_] :=
Catch[
Module[{handledChildren},

  handledChildren = {parseBox[d, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest];

  If[AnyTrue[handledChildren, FailureQ],
    Throw[SelectFirst[handledChildren, FailureQ]]
  ];

  BoxNode[Cell, handledChildren, <| Source -> pos |>]
]]

parseBox[Cell[rest___], pos_] :=
Catch[
Module[{handledChildren},

  handledChildren = applyEvaluatedCodeNodesToRest[rest];

  If[AnyTrue[handledChildren, FailureQ],
    Throw[SelectFirst[handledChildren, FailureQ]]
  ];

  BoxNode[Cell, handledChildren, <| Source -> pos |>]
]]

parseBox[BoxData[a_], pos_] :=
Catch[
Module[{handledChildren},

  handledChildren = {parseBox[a, Append[pos, 1]]};

  If[AnyTrue[handledChildren, FailureQ],
    Throw[SelectFirst[handledChildren, FailureQ]]
  ];

  BoxNode[BoxData, handledChildren, <|Source->pos|>]
]]









Attributes[applyEvaluatedCodeNodesToRest] = {}

applyEvaluatedCodeNodesToRest[rest___] :=
Module[{},
  Function[arg,
    With[{assoc = <||>},
      CodeNode[Evaluated, arg, assoc]
    ]
  ] /@ {rest}
]

Attributes[applyUnevaluatedCodeNodesToRest] = {HoldAllComplete}

applyUnevaluatedCodeNodesToRest[rest___] := List @@ Map[Function[arg, With[{assoc = <||>}, CodeNode[Unevaluated, arg, assoc]], {HoldAllComplete}], HoldComplete[rest]]









(*
boxes that are just {Protected, ReadProtected}
*)

parseBox[ActionMenuBox[a_, rest___], pos_] :=
  BoxNode[ActionMenuBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[AdjustmentBox[a_, rest___], pos_] :=
  BoxNode[AdjustmentBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[AnimatorBox[rest___], pos_] :=
  BoxNode[AnimatorBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ButtonBox[a_, rest___], pos_] :=
  BoxNode[ButtonBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[CheckboxBox[rest___], pos_] :=
  BoxNode[CheckboxBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ColorSetterBox[rest___], pos_] :=
  BoxNode[ColorSetterBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ErrorBox[a_], pos_] :=
  BoxNode[ErrorBox, {parseBox[a, Append[pos, 1]]}, <|Source->pos|>]

(*
something like TraditionalForm where they are not valid StandardForm boxes

In fact, contents of FormBox can be VERY far away from standard StandardForm boxes
*)
parseBox[FormBox[rest___], pos_] :=
  BoxNode[FormBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[FractionBox[a_, b_, rest___], pos_] :=
  BoxNode[FractionBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <| Source -> pos |>]

parseBox[FrameBox[a_, rest___], pos_] :=
  BoxNode[FrameBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

(*
a is a List of Lists
*)
parseBox[GridBox[a_, rest___], pos_] :=
Block[{$PreserveRowBox = True},
  BoxNode[GridBox, {parseBoxPossibleList[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]
]

parseBox[InputFieldBox[rest___], pos_] :=
  BoxNode[InputFieldBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ItemBox[a_, rest___], pos_] :=
  BoxNode[ItemBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ListPickerBox[rest___], pos_] :=
  BoxNode[ListPickerBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[LocatorPaneBox[first_, a_, rest___], pos_] :=
  BoxNode[LocatorPaneBox, applyEvaluatedCodeNodesToRest[first] ~Join~ {parseBox[a, Append[pos, 2]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[NamespaceBox[first_, a_, rest___], pos_] :=
  BoxNode[NamespaceBox, applyEvaluatedCodeNodesToRest[first] ~Join~ {parseBox[a, Append[pos, 2]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[OpenerBox[rest___], pos_] :=
  BoxNode[OpenerBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[OverlayBox[rest___], pos_] :=
  BoxNode[OverlayBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[OverscriptBox[a_, b_, rest___], pos_] :=
  BoxNode[OverscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <| Source -> pos |>]

parseBox[PaneBox[a_, rest___], pos_] :=
  BoxNode[PaneBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PanelBox[a_, rest___], pos_] :=
  BoxNode[PanelBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PaneSelectorBox[rest___], pos_] :=
  BoxNode[PaneSelectorBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PopupMenuBox[rest___], pos_] :=
  BoxNode[PopupMenuBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ProgressIndicatorBox[rest___], pos_] :=
  BoxNode[ProgressIndicatorBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RadicalBox[a_, b_, rest___], pos_] :=
  BoxNode[RadicalBox, {parseBox[a, Append[pos, 1]]} ~Join~ {parseBox[b, Append[pos, 2]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <| Source -> pos |>]

parseBox[RadioButtonBox[rest___], pos_] :=
  BoxNode[RadioButtonBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RotationBox[a_, rest___], pos_] :=
  BoxNode[RotationBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RowBox[children_], pos_] :=
Catch[
Module[{handledChildren, aggregatedChildren},

  handledChildren = children;

  handledChildren = MapIndexed[parseBox[#1, Append[pos, 1] ~Join~ #2]&, handledChildren];
  
  If[AnyTrue[handledChildren, FailureQ],
    Throw[SelectFirst[handledChildren, FailureQ]]
  ];

  aggregatedChildren = DeleteCases[handledChildren, LeafNode[Token`Boxes`MultiWhitespace | Token`Newline | Token`Boxes`LineContinuation, _, _] | GroupNode[Comment, _, _]];

  If[Length[aggregatedChildren] == 1,
    If[TrueQ[$PreserveRowBox],
      Throw[BoxNode[RowBox, {handledChildren}, <| Source -> pos |>]]
      ,
      (*
      Make sure to return the concrete children
      *)
      Throw[toBeSpliced[handledChildren]]
    ]
  ];
  
  prbDispatch[aggregatedChildren, handledChildren, children, pos]
]]

parseBox[SetterBox[rest___], pos_] :=
  BoxNode[SetterBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Slider2DBox[rest___], pos_] :=
  BoxNode[Slider2DBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SliderBox[rest___], pos_] :=
  BoxNode[SliderBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SqrtBox[a_, rest___], pos_] :=
  BoxNode[SqrtBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <| Source -> pos |>]

parseBox[StyleBox[a_, rest___], pos_] :=
  BoxNode[StyleBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SubscriptBox[a_, b_, rest___], pos_] :=
  BoxNode[SubscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <| Source -> pos |>]

parseBox[SuperscriptBox[a_, b_, rest___], pos_] :=
  BoxNode[SuperscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <| Source -> pos |>]

parseBox[SubsuperscriptBox[a_, b_, c_, rest___], pos_] :=
  BoxNode[SubsuperscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]], parseBox[c, Append[pos, 3]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[TabViewBox[rest___], pos_] :=
  BoxNode[TabViewBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[TagBox[a_, rest___], pos_] :=
  BoxNode[TagBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

(*
too complicated to handle first arg as boxes

For example:
TemplateBox[{"\"https://www.wolframcloud.com/objects/user-b0c28e9f-876d-4478-9d8b-9e7d18a9ea81/nameFormatExample.wl\"", 
  "https://www.wolframcloud.com/objects/user-b0c28e9f-876d-4478-9d8b-9e7d18a9ea81/nameFormatExample.wl"}, "HyperlinkURL"]

The second arg is a link, but it's not like we can do ParseLeaf["https://www.wolframcloud.com/objects/user-b0c28e9f-876d-4478-9d8b-9e7d18a9ea81/nameFormatExample.wl"]

*)
parseBox[TemplateBox[rest___], pos_] :=
  BoxNode[TemplateBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[TogglerBox[rest___], pos_] :=
  BoxNode[TogglerBox, applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

(*
FullNotationPalette has something like:

TooltipBox[xxx, "\[EscapeKey]notation\[EscapeKey]. Notation template that parses and formats."]

2nd arg is a string, not a box
*)
parseBox[TooltipBox[a_, rest___], pos_] :=
  BoxNode[TooltipBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[UnderoverscriptBox[a_, b_, c_, rest___], pos_] :=
  BoxNode[UnderoverscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]], parseBox[c, Append[pos, 3]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[UnderscriptBox[a_, b_, rest___], pos_] :=
  BoxNode[UnderscriptBox, {parseBox[a, Append[pos, 1]], parseBox[b, Append[pos, 2]]} ~Join~ applyEvaluatedCodeNodesToRest[rest], <| Source -> pos |>]






(*
boxes that are HoldFirst
*)

parseBox[TableViewBox[rest___], pos_] :=
  BoxNode[TableViewBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]





(*
boxes that are HoldRest
*)

parseBox[DynamicWrapperBox[a_, rest___], pos_] :=
  BoxNode[DynamicWrapperBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]






(*
boxes that are HoldAll
*)

parseBox[Arrow3DBox[rest___], pos_] :=
  BoxNode[Arrow3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[BSplineCurve3DBox[rest___], pos_] :=
  BoxNode[BSplineCurve3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[BSplineCurveBox[rest___], pos_] :=
  BoxNode[BSplineCurveBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[BSplineSurface3DBox[rest___], pos_] :=
  BoxNode[BSplineSurface3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[ConicHullRegion3DBox[rest___], pos_] :=
  BoxNode[ConicHullRegion3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[CuboidBox[rest___], pos_] :=
  BoxNode[CuboidBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[CylinderBox[rest___], pos_] :=
  BoxNode[CylinderBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[DiskBox[rest___], pos_] :=
  BoxNode[DiskBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[DynamicBox[rest___], pos_] :=
  BoxNode[DynamicBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[DynamicModuleBox[rest___], pos_] :=
  BoxNode[DynamicModuleBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GeometricTransformation3DBox[rest___], pos_] :=
  BoxNode[GeometricTransformation3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Graphics3DBox[rest___], pos_] :=
  BoxNode[Graphics3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

(*
a may be a List

GraphicsBox is HoldAll, so cannot recurse into child boxes without evaling

parseBox[GraphicsBox[a_, rest___], pos_] :=
  BoxNode[GraphicsBox, {parseBoxPossibleListPossibleDirective[a, Append[pos, 1]]} ~Join~ applyCodeNodesToRest[rest], <|Source->pos|>]
*)
parseBox[GraphicsBox[rest___], pos_] :=
  BoxNode[GraphicsBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GraphicsComplex3DBox[rest___], pos_] :=
  BoxNode[GraphicsComplex3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GraphicsComplexBox[rest___], pos_] :=
  BoxNode[GraphicsComplexBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GraphicsGroup3DBox[rest___], pos_] :=
  BoxNode[GraphicsGroup3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[GraphicsGroupBox[rest___], pos_] :=
  BoxNode[GraphicsGroupBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[InsetBox[a_, rest___], pos_] :=
  BoxNode[InsetBox, {parseBox[a, Append[pos, 1]]} ~Join~ applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Line3DBox[rest___], pos_] :=
  BoxNode[Line3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[LineBox[rest___], pos_] :=
  BoxNode[LineBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Point3DBox[rest___], pos_] :=
  BoxNode[Point3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PointBox[rest___], pos_] :=
  BoxNode[PointBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[Polygon3DBox[rest___], pos_] :=
  BoxNode[Polygon3DBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[PolygonBox[rest___], pos_] :=
  BoxNode[PolygonBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RasterBox[rest___], pos_] :=
  BoxNode[RasterBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[RectangleBox[rest___], pos_] :=
  BoxNode[RectangleBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[SphereBox[rest___], pos_] :=
  BoxNode[SphereBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]

parseBox[TubeBox[rest___], pos_] :=
  BoxNode[TubeBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]





(*
boxes that are HoldAllComplete
*)

(*
InterpretationBox is HoldAllComplete
*)
parseBox[InterpretationBox[rest___], pos_] :=
  BoxNode[InterpretationBox, applyUnevaluatedCodeNodesToRest[rest], <|Source->pos|>]





























parseBoxPossibleList[l_List, pos_] := MapIndexed[parseBoxPossibleList[#, pos ~Join~ #2]&, l]

parseBoxPossibleList[box_, pos_] := parseBox[box, pos]


parseBox["=.", pos_] :=
  Sequence @@ {
    LeafNode[Token`Equal, "=", <| Source -> pos |>],
    LeafNode[Token`Dot, ".", <| Source -> pos |>]
  }


(*
FIXME: if there is ever integration with the build system, then remove this
hard-coded list and use the build system
*)
$mbWhitespace = {
"\[NonBreakingSpace]",
"\[ThickSpace]",
"\[ThinSpace]",
"\[VeryThinSpace]",
"\[MediumSpace]",
"\[NoBreak]",
"\[SpaceIndicator]",
"\[InvisibleSpace]",
"\[NegativeVeryThinSpace]",
"\[NegativeThinSpace]",
"\[NegativeMediumSpace]",
"\[NegativeThickSpace]",
"\[AutoSpace]",
"\[Continuation]",
"\[RoundSpaceIndicator]",
"\[PageBreakAbove]",
"\[PageBreakBelow]",
"\[DiscretionaryPageBreakAbove]",
"\[DiscretionaryPageBreakBelow]",
"\[AlignmentMarker]"
(*
do NOT add \[COMPATIBILITYNoBreak]
we want to route \[COMPATIBILITYNoBreak] through the actual parser so that issues are generated
*)
}

$whitespacePat = Alternatives @@ ({" " | "\t"} ~Join~ $mbWhitespace)


parseBox[str:"[", pos_] := LeafNode[Token`OpenSquare, str, <|Source -> pos|>]

parseBox[str:"/", pos_] := LeafNode[Token`Slash, str, <|Source -> pos|>]

parseBox[str:",", pos_] := LeafNode[Token`Comma, str, <|Source -> pos|>]

parseBox[str:"]", pos_] := LeafNode[Token`CloseSquare, str, <|Source -> pos|>]

parseBox[str:"{", pos_] := LeafNode[Token`OpenCurly, str, <|Source -> pos|>]

parseBox[str:"}", pos_] := LeafNode[Token`CloseCurly, str, <|Source -> pos|>]

parseBox[str:"=", pos_] := LeafNode[Token`Equal, str, <|Source -> pos|>]

parseBox[str:";", pos_] := LeafNode[Token`Semi, str, <|Source -> pos|>]

parseBox[str:"//", pos_] := LeafNode[Token`SlashSlash, str, <|Source -> pos|>]

parseBox[str:":=", pos_] := LeafNode[Token`ColonEqual, str, <|Source -> pos|>]

parseBox[str:">", pos_] := LeafNode[Token`Greater, str, <|Source -> pos|>]

parseBox[str:"^", pos_] := LeafNode[Token`Caret, str, <|Source -> pos|>]

parseBox[str:"+", pos_] := LeafNode[Token`Plus, str, <|Source -> pos|>]

parseBox[str:"#", pos_] := LeafNode[Token`Hash, str, <|Source -> pos|>]

parseBox[str:"##", pos_] := LeafNode[Token`HashHash, str, <|Source -> pos|>]

parseBox[str:"&", pos_] := LeafNode[Token`Amp, str, <|Source -> pos|>]

parseBox[str:"-", pos_] := LeafNode[Token`Minus, str, <|Source -> pos|>]

parseBox[str:"(", pos_] := LeafNode[Token`OpenParen, str, <|Source -> pos|>]

parseBox[str:")", pos_] := LeafNode[Token`CloseParen, str, <|Source -> pos|>]

parseBox[str:"%", pos_] := LeafNode[Token`Percent, str, <|Source -> pos|>]

parseBox[str:"/.", pos_] := LeafNode[Token`SlashDot, str, <|Source -> pos|>]

parseBox[str:"!", pos_] := LeafNode[Token`Bang, str, <|Source -> pos|>]

parseBox[str:"/@", pos_] := LeafNode[Token`SlashAt, str, <|Source -> pos|>]

parseBox[str:"+=", pos_] := LeafNode[Token`PlusEqual, str, <|Source -> pos|>]

parseBox[str:"<|", pos_] := LeafNode[Token`LessBar, str, <|Source -> pos|>]

parseBox[str:"|>", pos_] := LeafNode[Token`BarGreater, str, <|Source -> pos|>]

parseBox[str:"?", pos_] := LeafNode[Token`Question, str, <|Source -> pos|>]

parseBox[str:"<", pos_] := LeafNode[Token`Less, str, <|Source -> pos|>]

parseBox[str:"->", pos_] := LeafNode[Token`MinusGreater, str, <|Source -> pos|>]

parseBox[str:"||", pos_] := LeafNode[Token`BarBar, str, <|Source -> pos|>]

parseBox[str:"@", pos_] := LeafNode[Token`At, str, <|Source -> pos|>]

parseBox[str:".", pos_] := LeafNode[Token`Dot, str, <|Source -> pos|>]

parseBox[str:":", pos_] := LeafNode[Token`Colon, str, <|Source -> pos|>]

parseBox[str:"|", pos_] := LeafNode[Token`Bar, str, <|Source -> pos|>]

parseBox[str:"--", pos_] := LeafNode[Token`MinusMinus, str, <|Source -> pos|>]

parseBox[str:"*", pos_] := LeafNode[Token`Star, str, <|Source -> pos|>]

parseBox[str:"&&", pos_] := LeafNode[Token`AmpAmp, str, <|Source -> pos|>]


parseBox[str:"===", pos_] := LeafNode[Token`EqualEqualEqual, str, <|Source -> pos|>]

parseBox[str:"=!=", pos_] := LeafNode[Token`EqualBangEqual, str, <|Source -> pos|>]

parseBox[str:"<>", pos_] := LeafNode[Token`LessGreater, str, <|Source -> pos|>]

parseBox[str:"@@", pos_] := LeafNode[Token`AtAt, str, <|Source -> pos|>]

parseBox[str:">=", pos_] := LeafNode[Token`GreaterEqual, str, <|Source -> pos|>]

parseBox[str:"==", pos_] := LeafNode[Token`EqualEqual, str, <|Source -> pos|>]

parseBox[str:"<=", pos_] := LeafNode[Token`LessEqual, str, <|Source -> pos|>]

parseBox[str:"++", pos_] := LeafNode[Token`PlusPlus, str, <|Source -> pos|>]

parseBox[str:"..", pos_] := LeafNode[Token`DotDot, str, <|Source -> pos|>]

parseBox[str:"/;", pos_] := LeafNode[Token`SlashSemi, str, <|Source -> pos|>]

parseBox[str:"<<", pos_] := LeafNode[Token`LessLess, str, <|Source -> pos|>]

parseBox[str:"??", pos_] := LeafNode[Token`QuestionQuestion, str, <|Source -> pos|>]

parseBox[str:"~", pos_] := LeafNode[Token`Tilde, str, <|Source -> pos|>]

parseBox[str:"::", pos_] := LeafNode[Token`ColonColon, str, <|Source -> pos|>]

parseBox[str:":>", pos_] := LeafNode[Token`ColonGreater, str, <|Source -> pos|>]

parseBox[str:"@*", pos_] := LeafNode[Token`AtStar, str, <|Source -> pos|>]

parseBox[str:"@@@", pos_] := LeafNode[Token`AtAtAt, str, <|Source -> pos|>]

parseBox[str:"~~", pos_] := LeafNode[Token`TildeTilde, str, <|Source -> pos|>]


parseBox[str:";;", pos_] := LeafNode[Token`SemiSemi, str, <|Source -> pos|>]


(*
Common symbols that can be short-circuited
*)

parseBox[str:"a", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]

parseBox[str:"b", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]

parseBox[str:"c", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]

parseBox[str:"d", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]

parseBox[str:"f", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]

parseBox[str:"i", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]

parseBox[str:"t", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]

parseBox[str:"x", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]

parseBox[str:"y", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]


parseBox[str:"I", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]

parseBox[str:"N", pos_] := LeafNode[Symbol, str, <|Source -> pos|>]


(*
Common whitespace that can be short-circuited
*)
parseBox[str:"\n", pos_] := LeafNode[Token`Newline, str, <|Source -> pos|>]

parseBox[str:" ", pos_] := LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>]

parseBox[str:"  ", pos_] := LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>]

parseBox[str:"   ", pos_] := LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>]

parseBox[str:"    ", pos_] := LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>]

parseBox[str:"     ", pos_] := LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>]

parseBox[str:"      ", pos_] := LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>]

parseBox[str:"       ", pos_] := LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>]

parseBox[str:"        ", pos_] := LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>]



parseBox[str:"\t", pos_] := LeafNode[Token`Boxes`MultiWhitespace, str, <|Source -> pos|>]


(*
This has the added benefit of not generating an UnexpectedNewlineCharacter issue

NOTE: an UnexpectedNewlineCharacter issue is still generated if you call CodeConcreteParseLeaf directly:

CodeConcreteParseLeaf["\[IndentingNewLine]", "EncodingMode" -> 1]

FIXME: fix the behavior of CodeConcreteParseLeaf to generate an issue here
*)
parseBox[str:"\[IndentingNewLine]", pos_] := LeafNode[Token`Newline, str, <|Source -> pos|>]


(*
The Front End treats comments as a collection of code, and not a single token
*)
parseBox[str:"(*", pos_] := LeafNode[Token`Boxes`OpenParenStar, str, <| Source -> pos |>]

parseBox[str:"*)", pos_] := LeafNode[Token`Boxes`StarCloseParen, str, <| Source -> pos |>]

(*
parseBox[str:"\[LeftSkeleton]", pos_] := LeafNode[Token`Boxes`LongName`LeftSkeleton, str, <|Source -> pos|>]

parseBox[str:"\[RightSkeleton]", pos_] := LeafNode[Token`Boxes`LongName`RightSkeleton, str, <|Source -> pos|>]
*)

(*
interesting case where sending \( to library would return Token`Error`UnterminatedLinearSyntaxBlob

The FE treats \(\) as a group, so the openers and closers need to be their own tokens
*)
parseBox[str:"\\(", pos_] := LeafNode[Token`LinearSyntax`OpenParen, str, <| Source -> pos |>]

parseBox[str:"\\)", pos_] := LeafNode[Token`LinearSyntax`CloseParen, str, <| Source -> pos |>]

parseBox[str:"\\\n", pos_] := LeafNode[Token`Boxes`LineContinuation, str, <| Source -> pos |>]



parseBox[str:"_", pos_] := LeafNode[Token`Under, str, <| Source -> pos |>]

parseBox[str:"__", pos_] := LeafNode[Token`UnderUnder, str, <| Source -> pos |>]

parseBox[str:"___", pos_] := LeafNode[Token`UnderUnderUnder, str, <| Source -> pos |>]

parseBox[str:"_.", pos_] := LeafNode[Token`UnderDot, str, <| Source -> pos |>]


parseBox[str_String, pos_, OptionsPattern[]] :=
Catch[
Module[{data, issues, stringifyMode, oldLeafSrc, len, src, cases, containsQuote, parsed, origSrc},

  (*
  Bypass calling into ParseLeaf if only whitespace,
  we handle multiple whitespace characters here,
  because the FE treats  <space><space><space>  as a single token
  *)

  stringifyMode = OptionValue["StringifyMode"];

  containsQuote = StringContainsQ[str, "\""];

  Which[
    !containsQuote &&
      (*
      was originally using:
      (LetterCharacter | "$") ~~ (LetterCharacter | "$" | DigitCharacter)... 

      but LetterCharacter matches non-ASCII characters:
      In[8]:= StringMatchQ["\[Alpha]", LetterCharacter]

      Out[8]= True

      and I'm trying to only quickly test for ASCII here.
      Non-ASCII characters should go through the parser to generate NonASCIICharacter EncodingIssues
      *)
      StringMatchQ[str, RegularExpression["[a-zA-Z$][a-zA-Z$0-9]*"]] && stringifyMode == 0,
      Throw[LeafNode[Symbol, str, <| Source -> pos |>]]
    ,
    !containsQuote &&
      StringMatchQ[str, RegularExpression["[0-9]+"]] && stringifyMode == 0,
      Throw[LeafNode[Integer, str, <| Source -> pos |>]]
    ,
    (*
    Handle all of the CompoundNodes

    a_b

    Front End treats a_b as single token

    Split things like a_b into correct structures
    *)
    !containsQuote && StringContainsQ[str, "_"],
      Which[
        (cases = StringCases[str, RegularExpression["^(_|__|___)([^_]+)$"] :> {"$1", "$2"}]) != {},
          If[StringEndsQ[str, ":"],
            (*
            something like "_:"
            *)
            Throw[ErrorNode[Token`Error`OldFESyntax, parseBox[#, pos]& /@ cases[[1]], <| Source -> pos |>]]
          ];
          Throw[CompoundNode[underToOp[cases[[1, 1]]], parseBox[#, pos]& /@ cases[[1]], <| Source -> pos |>]]
        ,
        (cases = StringCases[str, RegularExpression["^([^_]+)(_\\.)$"] :> {"$1", "$2"}]) != {},
          Throw[CompoundNode[PatternOptionalDefault, parseBox[#, pos]& /@ cases[[1]], <| Source -> pos |>]]
        ,
        (cases = StringCases[str, RegularExpression["^([^_]+)(_|__|___)([^_]+)$"] :> {"$1", "$2", "$3"}]) != {},
          If[StringEndsQ[str, ":"],
            (*
            something like "a_:"
            *)
            Throw[ErrorNode[Token`Error`OldFESyntax, {
              parseBox[cases[[1, 1]], pos],
              CompoundNode[underToOp[cases[[1, 2]]], {parseBox[cases[[1, 2]], pos], parseBox[cases[[1, 3]], pos]}, <| Source -> pos |>]}, <| Source -> pos |>]]
          ];
          Throw[CompoundNode[underToPatternOp[cases[[1, 2]]], {
            parseBox[cases[[1, 1]], pos],
            CompoundNode[underToOp[cases[[1, 2]]], {parseBox[cases[[1, 2]], pos], parseBox[cases[[1, 3]], pos]}, <| Source -> pos |>]}, <| Source -> pos |>]]
        ,
        (cases = StringCases[str, RegularExpression["^([^_]+)(_|__|___)$"] :> {"$1", "$2"}]) != {},
          Throw[CompoundNode[underToPatternOp[cases[[1, 2]]], parseBox[#, pos]& /@ cases[[1]], <| Source -> pos |>]]
      ]
    ,
    (*
    I normally do not want to parse strings inside of parseBox, but it is possible to have the syntax:

    #"foo"

    and the FE treats this as a single token.
    *)
    StringStartsQ[str, "#"],
      Which[
        (cases = StringCases[str, RegularExpression["^(##)(\\d+)$"] :> {"$1", "$2"}]) != {},
          Throw[CompoundNode[SlotSequence, parseBox[#, pos]& /@ cases[[1]], <| Source -> pos |>]]
        ,
        (cases = StringCases[str, RegularExpression["^(#)(\\d+)$"] :> {"$1", "$2"}]) != {},
          Throw[CompoundNode[Slot, parseBox[#, pos]& /@ cases[[1]], <| Source -> pos |>]]
        ,
        (cases = StringCases[str, RegularExpression["^(#)([a-zA-Z\"].*)$"] :> {"$1", "$2"}]) != {},
          Throw[CompoundNode[Slot, {parseBox[cases[[1, 1]], pos], parseBox[cases[[1, 2]], pos, "StringifyMode" -> 1]}, <| Source -> pos |>]]
      ]
    ,
    StringStartsQ[str, "%"],
      Which[
        (cases = StringCases[str, RegularExpression["^(%)(\\d+)$"] :> {"$1", "$2"}]) != {},
          Throw[CompoundNode[Out, parseBox[#, pos]& /@ cases[[1]], <| Source -> pos |>]]
        ,
        StringMatchQ[str, "%" ~~ "%"..],
          Throw[LeafNode[Token`PercentPercent, str, <| Source -> pos |>]]
      ]
    ,
    StringStartsQ[str, "'"],
      Which[
        StringMatchQ[str, ("'")..],
          Throw[LeafNode[Token`Boxes`MultiSingleQuote, str, <| Source -> pos |>]]
      ]
    ,
    StringMatchQ[str, $whitespacePat..] && (stringifyMode == 0 || stringifyMode == 2),
      Throw[LeafNode[Token`Boxes`MultiWhitespace, str, <| Source -> pos |>]]
    ,
    StringMatchQ[str, Verbatim["*"].. ~~ ")"],
      (*
      The FE parses (***) as RowBox[{"(*", "***)"}], so need to handle variable-length *** ) token

      TODO: fe bug?
      *)
      Throw[LeafNode[Token`Boxes`StarCloseParen, str, <| Source -> pos |>]]
    ,
    (*
    Handle the simple case of a string with no backslashes

    Perfectly easy to parse here
    *)
    containsQuote && StringMatchQ[str, RegularExpression["\"[^\\\\]*\""]],
      Throw[LeafNode[String, str, <| Source -> pos |>]]
  ];

  (*
  EncodingMode:
  0: normal (strings, files, bytes)
  1: boxes
  Has the effect of disabling NonASCIICharacter issues for boxes
  *)
  parsed = CodeConcreteParseLeaf[str, "StringifyMode" -> stringifyMode, "EncodingMode" -> 1];

  If[FailureQ[parsed],
    Throw[parsed]
  ];

  If[MissingQ[parsed],
    Throw[parsed]
  ];

  data = parsed[[3]];
  (*
  result of CodeConcreteParseLeaf is using LineColumn-convention
  but we need to force using Position-convention
  so just set to pos
  *)
  origSrc = data[Source];
  data[Source] = pos;
  issues = Lookup[data, SyntaxIssues, {}];
  If[!empty[issues],
    (*
    also need to force issues to use Position-convention
    *)
    issues = replaceWithPositionConvention[#, pos, origSrc]& /@ issues;
    data[SyntaxIssues] = issues
  ];
  parsed[[3]] = data;

  If[parsed[[1]] == Whitespace,
    parsed[[1]] = Token`Boxes`MultiWhitespace
  ];

  parsed
]]


underToOp["_"] = Blank
underToOp["__"] = BlankSequence
underToOp["___"] = BlankNullSequence

underToPatternOp["_"] = PatternBlank
underToPatternOp["__"] = PatternBlankSequence
underToPatternOp["___"] = PatternBlankNullSequence


(*
Replaces the original LineColumn-convention with Position-convention
*)
replaceWithPositionConvention[(head:SyntaxIssue|FormatIssue|EncodingIssue)[tag_, msg_, severity_, dataIn_], pos_, leafSrc_] :=
Module[{data, actions, newSrc, oldSyntaxIssueSrc},

  data = dataIn;

  oldSyntaxIssueSrc = data[Source];

  newSrc = pos;

  If[!(oldSyntaxIssueSrc[[1, 2]] == leafSrc[[1, 2]] && oldSyntaxIssueSrc[[2, 2]] == leafSrc[[2, 2]]),
      (*
      this is some sub-part of the leaf
      The arguments in Intra[a, b] are appropriate for StringTake et al.
      *)
      newSrc = newSrc ~Join~ { Intra[oldSyntaxIssueSrc[[1, 2]], oldSyntaxIssueSrc[[2, 2]]-1] };
  ];

  data[Source] = newSrc;
  actions = data[CodeActions];
  If[!empty[actions],
      actions = replaceWithPositionConvention[#, newSrc]& /@ actions;
      data[CodeActions] = actions
  ];
  head[tag, msg, severity, data]
]

replaceWithPositionConvention[CodeAction[label_, command_, dataIn_], newSrc_] :=
Module[{data, src},
  data = dataIn;

  src = data[Source];
  (*src = pos ~Join~ {LineColumn[src]};*)
  src = newSrc;
  data[Source] = src;
  CodeAction[label, command, data]
]

parseBox[args___] :=
  Failure["Unhandled", <| "Function" -> parseBox, "Arguments" -> HoldForm[{args}] |>]


removeImplicits[node_] := DeleteCases[node, LeafNode[Token`Fake`ImplicitTimes, _, _], Infinity]


(*
We want to coalesce runs of whitespace because that is what the FE prefers
*)
coalesceWhitespace[node_] :=
Module[{poss, runs, coalesced},

  poss = Position[node, LeafNode[Whitespace, _, _]];

  (*
  runs of Whitespace
  *)
  runs = Split[poss, (
    Length[#1] == Length[#2] &&
    Most[#1] == Most[#2] &&
    Last[#1] + 1 == Last[#2])&];

  coalesced =
    Fold[
      Function[{nodeA, run},
        (*
        Delete all but the first whitespace in a run
        Replace the first whitespace with the coalesced
        *)
        ReplacePart[Delete[nodeA, Rest[run]], First[run] -> LeafNode[Token`Boxes`MultiWhitespace, StringJoin[#[[2]]& /@ Extract[node, run]], <||>]]
      ]
      ,
      node
      ,
      (*
      Earlier positions may affect later positions, so process in reverse order
      *)
      runs // Reverse
    ];

  coalesced
]


ToStandardFormBoxes::usage = "ToStandardFormBoxes[cst] converts cst to a box."

(*
{a, \n, b, \n, c} => {a, \n, b, \n, c}

top-level expressions separated by newlines are just returned in a list
*)
ToStandardFormBoxes[ContainerNode[Box, children:{_, LeafNode[Token`Newline, _, _], ___}, _]] :=
Block[{$RecursionLimit = Infinity},
Module[{res},
  res = Replace[children, {
    LeafNode[Token`Newline, str_, _] :> str,
    else_ :> toStandardFormBoxes[coalesceWhitespace[removeImplicits[else]]]
  }, {1}];

  Which[
    Length[res] == 1,
      res[[1]]
    ,
    True,
      res
  ]
]]

(*
{a, " ", b, " ", c} => RowBox[{a, " ", b, " ", c}]
*)
ToStandardFormBoxes[ContainerNode[Box, children_, _]] :=
Block[{$RecursionLimit = Infinity},
Module[{res},
  res = Replace[children, {
    else_ :> toStandardFormBoxes[coalesceWhitespace[removeImplicits[else]]]
  }, {1}];

  Which[
    Length[res] == 1,
      res[[1]]
    ,
    True,
      RowBox[res]
  ]
]]


toStandardFormBoxes[LeafNode[tok_, _, _]] /; tokenIsEmpty[tok] :=
  Nothing

toStandardFormBoxes[LeafNode[_, str_, _]] :=
  str


toStandardFormBoxes[ErrorNode[tok_, _, _]] /; tokenIsEmpty[tok] :=
  Nothing

toStandardFormBoxes[ErrorNode[_, str_, _]] :=
  str





toStandardFormBoxes[BoxNode[Cell, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[Cell @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BoxData, {a_}, _]] :=
Catch[
Module[{heldChildren},

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] }];

  With[{heldChildren = heldChildren}, ReleaseHold[BoxData @@ heldChildren]]
]]


(*
BoxNodes that may contain CodeNodes have to be handled individually
*)











(*
boxes that are just {Protected, ReadProtected}
*)

toStandardFormBoxes[BoxNode[ActionMenuBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[ActionMenuBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[AdjustmentBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[AdjustmentBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[AnimatorBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[AnimatorBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ButtonBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[ButtonBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[CheckboxBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[CheckboxBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ColorSetterBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ColorSetterBox @@ heldChildren]]
]]

toStandardFormBoxes[n:BoxNode[ErrorBox, {rest___}, _]] :=
  Failure["Unimplemented", <| "Function" -> toStandardFormBoxes, "Arguments" -> HoldForm[{n}] |>]

toStandardFormBoxes[BoxNode[FormBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[FormBox @@ heldChildren]]
]]

toStandardFormBoxes[n:BoxNode[FractionBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[FractionBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[FrameBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[FrameBox @@ heldChildren]]
]]

(*
a is a List of Lists
*)
toStandardFormBoxes[BoxNode[GridBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = Map[toStandardFormBoxes, a, {2}]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[GridBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[InputFieldBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[InputFieldBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ItemBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[ItemBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ListPickerBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ListPickerBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[LocatorPaneBox, {first_, a_, rest___}, _]] :=
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, HoldComplete]& /@ {first};
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = heldFirst ~Join~ { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[LocatorPaneBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[NamespaceBox, {first_, a_, rest___}, _]] :=
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, HoldComplete]& /@ {first};
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = heldFirst ~Join~ { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[NamespaceBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[OpenerBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[OpenerBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[OverlayBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[OverlayBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[OverscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[OverscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PaneBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[PaneBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PanelBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[PanelBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PaneSelectorBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PaneSelectorBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PopupMenuBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PopupMenuBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ProgressIndicatorBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ProgressIndicatorBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RadicalBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]},
    heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[RadicalBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RadioButtonBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[RadioButtonBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RotationBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[RotationBox @@ heldChildren]]
]]

(*
a is a List of boxes
*)
toStandardFormBoxes[BoxNode[RowBox, {a_}, _]] :=
Catch[
Module[{aBox, boxes},

  aBox = toStandardFormBoxes /@ a;

  boxes = {aBox};

  RowBox @@ boxes
]]

toStandardFormBoxes[BoxNode[SetterBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[SetterBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Slider2DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Slider2DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SliderBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[SliderBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SqrtBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]},
    heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SqrtBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[StyleBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[StyleBox @@ heldChildren]]
]]

toStandardFormBoxes[n:BoxNode[SubscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SubscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SuperscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SuperscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SubsuperscriptBox, {a_, b_, c_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b], cBox = toStandardFormBoxes[c]},
    heldChildren = { HoldComplete[aBox], HoldComplete[bBox], HoldComplete[cBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[SubsuperscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TabViewBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TabViewBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TagBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[TagBox @@ heldChildren]]
]]

(*
too complicated to handle first arg as boxes
*)
toStandardFormBoxes[BoxNode[TemplateBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TemplateBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TogglerBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TogglerBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TooltipBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]},
    heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[TooltipBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[UnderoverscriptBox, {a_, b_, c_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b], cBox = toStandardFormBoxes[c]},
    heldChildren = { HoldComplete[aBox], HoldComplete[bBox], HoldComplete[cBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[UnderoverscriptBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[UnderscriptBox, {a_, b_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a], bBox = toStandardFormBoxes[b]}, heldChildren = { HoldComplete[aBox], HoldComplete[bBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[UnderscriptBox @@ heldChildren]]
]]




(*
boxes that are HoldFirst
*)

toStandardFormBoxes[BoxNode[TableViewBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TableViewBox @@ heldChildren]]
]]





(*
boxes that are HoldRest
*)

toStandardFormBoxes[BoxNode[DynamicWrapperBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[DynamicWrapperBox @@ heldChildren]]
]]





(*
boxes that are HoldAll
*)

toStandardFormBoxes[BoxNode[Arrow3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Arrow3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BSplineCurve3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[BSplineCurve3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BSplineCurveBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[BSplineCurveBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[BSplineSurface3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[BSplineSurface3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[ConicHullRegion3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[ConicHullRegion3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[CuboidBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[CuboidBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[CylinderBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[CylinderBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[DiskBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[DiskBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[DynamicBox, {rest___}, _]] :=
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[DynamicBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[DynamicModuleBox, {rest___}, _]] :=
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[DynamicModuleBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[GeometricTransformation3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GeometricTransformation3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Graphics3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Graphics3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsComplex3DBox, {rest___}, _]] :=
Catch[
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, HoldComplete]& /@ {first};
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsComplex3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsComplexBox, {rest___}, _]] :=
Catch[
Module[{heldFirst, heldRest, heldChildren},
  heldFirst = Extract[#, {2}, HoldComplete]& /@ {first};
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsComplexBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsGroup3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsGroup3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[GraphicsGroupBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[GraphicsGroupBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[InsetBox, {a_, rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  With[{aBox = toStandardFormBoxes[a]}, heldChildren = { HoldComplete[aBox] } ~Join~ heldRest];

  With[{heldChildren = heldChildren}, ReleaseHold[InsetBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Line3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Line3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[LineBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[LineBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Point3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Point3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PointBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PointBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[Polygon3DBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[Polygon3DBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[PolygonBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[PolygonBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[RasterBox, {rest___}, _]] :=
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[RasterBox @@ heldChildren]]
]

toStandardFormBoxes[BoxNode[RectangleBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[RectangleBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[SphereBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[SphereBox @@ heldChildren]]
]]

toStandardFormBoxes[BoxNode[TubeBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[TubeBox @@ heldChildren]]
]]





(*
boxes that are HoldAllComplete
*)

toStandardFormBoxes[BoxNode[InterpretationBox, {rest___}, _]] :=
Catch[
Module[{heldRest, heldChildren},
  heldRest = Extract[#, {2}, HoldComplete]& /@ {rest};

  heldChildren = heldRest;

  With[{heldChildren = heldChildren}, ReleaseHold[InterpretationBox @@ heldChildren]]
]]















toStandardFormBoxes[n:CodeNode[Evaluated, code_, data_]] :=
  code

toStandardFormBoxes[n:CodeNode[Unevaluated, _, _]] :=
  Failure["CannotConvertUnevaluatedCodeNodeToStandardFormBoxes", <| "Node" -> n |>]


toStandardFormBoxes[l_List] := Map[toStandardFormBoxes, l]

toStandardFormBoxes[DirectiveNode[dir_, children_, _]] :=
  dir @@ children





toStandardFormBoxes[PrefixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]



toStandardFormBoxes[BinaryNode[Unset, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;

  nodeBoxes = nodeBoxes /. {most___, "=", "."} :> {most, "=."};

  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]


(*
Convert back to form that the FE likes

Single RowBox RowBox[{"a", ":", "b", ":", "c"}]
*)
toStandardFormBoxes[BinaryNode[Optional, nodes:{BinaryNode[Pattern, _, _], ___}, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ (nodes[[1]][[2]] ~Join~ nodes[[2;;]]);
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]


toStandardFormBoxes[BinaryNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]





toStandardFormBoxes[InfixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]


toStandardFormBoxes[TernaryNode[TagUnset, nodes:{
  _, LeafNode[Token`SlashColon, _, _], _, LeafNode[Token`Equal, _, _], LeafNode[Token`Dot, _, _]}, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;

  nodeBoxes = {nodeBoxes[[1]], nodeBoxes[[2]], nodeBoxes[[3]], "=."};

  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]


(*
Convert back to form that the FE likes

Single RowBox[{"a", "~", "b", "~", "c", "~", "d", "~", "e"}]
*)
toStandardFormBoxes[TernaryNode[TernaryTilde, nodes:{TernaryNode[TernaryTilde, _, _], ___}, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ (nodes[[1]][[2]] ~Join~ nodes[[2;;]]);
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]


toStandardFormBoxes[TernaryNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]


toStandardFormBoxes[PostfixNode[op_, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]

toStandardFormBoxes[CallNode[op_List, node_, data_]] :=
Catch[
Module[{opBoxes, nodeBox},
  opBoxes = toStandardFormBoxes /@ op;
  If[AnyTrue[opBoxes, FailureQ],
    Throw[SelectFirst[opBoxes, FailureQ]]
  ];
  nodeBox = toStandardFormBoxes[node];
  If[FailureQ[nodeBox],
    Throw[nodeBox]
  ];
  RowBox[opBoxes ~Join~ nodeBox[[1]]]
]]

(*
this is a convenience, and is not technically correct
FIXME: need a way to turn Aggregate syntax into boxes
*)
toStandardFormBoxes[CallNode[op_, node_, data_]] :=
Catch[
Module[{opBoxes, nodeBox},
  opBoxes = toStandardFormBoxes[op];
  If[AnyTrue[opBoxes, FailureQ],
    Throw[SelectFirst[opBoxes, FailureQ]]
  ];
  nodeBox = toStandardFormBoxes[node];
  If[FailureQ[nodeBox],
    Throw[nodeBox]
  ];
  RowBox[ {opBoxes} ~Join~ nodeBox[[1]]]
]]

toStandardFormBoxes[GroupNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]



toStandardFormBoxes[PrefixBinaryNode[Integrate, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[{First[nodeBoxes], RowBox[Rest[nodeBoxes]]}]
]]



toStandardFormBoxes[CompoundNode[Blank, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

toStandardFormBoxes[CompoundNode[BlankSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

toStandardFormBoxes[CompoundNode[BlankNullSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single a_ token
*)
toStandardFormBoxes[CompoundNode[PatternBlank, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single a__ token
*)
toStandardFormBoxes[CompoundNode[PatternBlankSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single a___ token
*)
toStandardFormBoxes[CompoundNode[PatternBlankNullSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single a_. token
*)
toStandardFormBoxes[CompoundNode[PatternOptionalDefault, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single #1 token
*)
toStandardFormBoxes[CompoundNode[Slot, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single ##1 token
*)
toStandardFormBoxes[CompoundNode[SlotSequence, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]

(*
Convert back to form that the FE likes

Single %123 token
*)
toStandardFormBoxes[CompoundNode[Out, nodes_, _]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  StringJoin[nodeBoxes]
]]


(*
nodes should be a list of strings
*)
toStandardFormBoxes[SyntaxErrorNode[tag_, nodes_, data_]] :=
Catch[
Module[{},
  RowBox[nodes]
]]

toStandardFormBoxes[GroupMissingCloserNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]

toStandardFormBoxes[GroupMissingOpenerNode[op_, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[nodeBoxes]
]]




toStandardFormBoxes[ContainerNode[File, nodes_, data_]] :=
Catch[
Module[{nodeBoxes},
  nodeBoxes = toStandardFormBoxes /@ nodes;
  If[AnyTrue[nodeBoxes, FailureQ],
    Throw[SelectFirst[nodeBoxes, FailureQ]]
  ];
  RowBox[Riffle[nodeBoxes, "\n"]]
]]





toStandardFormBoxes[ContainerNode[Hold, nodes_, data_]] :=
Module[{processed},
  
  processed = Riffle[nodes, LeafNode[Token`Comma, ",", <||>]];

  toStandardFormBoxes[CallNode[LeafNode[Symbol, "Hold", <||>],
                GroupNode[GroupSquare, {
                  LeafNode[Token`OpenSquare, "[", <||>] } ~Join~
                  { InfixNode[Comma, processed, <||>] } ~Join~
                  { LeafNode[Token`CloseSquare, "]", <||>] }, <||>], <||> ]]
]



toStandardFormBoxes[Missing["EmptyInput"]] = ""

toStandardFormBoxes[m_?MissingQ] = m

toStandardFormBoxes[f_?FailureQ] := f

toStandardFormBoxes[args___] :=
  Failure["Unhandled", <| "Function" -> toStandardFormBoxes, "Arguments" -> HoldForm[{args}] |>]



End[]

EndPackage[]
