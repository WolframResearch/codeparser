BeginPackage["CodeParser`Concretify`"]

Concretify

Begin["`Private`"]

Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["PacletManager`"] (* for PacletInformation *)


(*

Concretify[ast] converts ast to a CST

*)



(*
TODO: when targeting 12.1 as a minimum, then use paclet["AssetLocation", "Precedence"]
*)
location = "Location" /. PacletInformation["CodeParser"]

precedenceFile = FileNameJoin[{location, "Resources", "Generated", "Precedence.wl"}]

$precedence = Get[precedenceFile]

lookupPrecedence[prec_?FailureQ] := prec

lookupPrecedence[prec_] :=
  If[KeyExistsQ[$precedence, prec],
    $precedence[prec]
    ,
    Message[lookupPrecedence::notfound, prec];
    Missing["PrecedenceNotFound", prec]
  ]



Concretify[ast_] :=
Catch[
Module[{cst, agg, ast2, astToCompare, ast2ToCompare, str},

  cst = walk[ast];

  If[TrueQ[$DisableSanityChecking],
    Throw[cst]
  ];

  (*
  Sanity Checking
  *)
  
  agg = CodeParser`Abstract`Aggregate[cst];

  ast2 = CodeParser`Abstract`Abstract[agg];

  astToCompare = ast /. _Association -> <||>;
  ast2ToCompare = ast2 /. _Association -> <||>;

  If[astToCompare =!= ast2ToCompare,
    Throw[Failure["ConcretifySanityCheckFailed", <||>]]
  ];

  str = ToSourceCharacterString[cst];

  ast2 = CodeParse[str];

  ast2[[1]] = ast[[1]];

  ast2ToCompare = ast2 /. _Association -> <||>;

  If[astToCompare =!= ast2ToCompare,
    Throw[Failure["ConcretifySanityCheckFailed2", <||>]]
  ];

  cst
]]


(*
try to catch bad args by matching specific patterns here
*)
structure[Identity, _, _]["ctor"] = Identity

structure[ctor_Function, _, _]["ctor"] := ctor

structure[_, op_?FailureQ, _]["op"] := op

(*
maybe op matches:
LeafNode[Token`Equal, "=", <||>]
*)
structure[_, op:_[_, _, _], _]["op"] := op

(*
maybe op matches:
{LeafNode[Token`Equal, "=", <||>], LeafNode[Token`Dot, ".", <||>]}
*)
structure[_, op:{_[_, _, _], _[_, _, _]}, _]["op"] := op

structure[_, _, prec_Symbol]["prec"] := prec



precedenceGreater[a_?FailureQ, b_] := 
  a

precedenceGreater[a_, b_?FailureQ] := 
  b

precedenceGreater[a_, b_] := 
  lookupPrecedence[a] > lookupPrecedence[b]

precedenceLess[a_?FailureQ, b_] := 
  a

precedenceLess[a_, b_?FailureQ] := 
  b

precedenceLess[a_, b_] := 
  lookupPrecedence[a] < lookupPrecedence[b]

precedenceEqual[a_?FailureQ, b_] :=
  a

precedenceEqual[a_, b_?FailureQ] :=
  b

precedenceEqual[a_, b_] :=
  lookupPrecedence[a] == lookupPrecedence[b]

precedenceGreaterSpecialMinus[Precedence`Infix`Minus, Precedence`Prefix`Minus] = True

precedenceGreaterSpecialMinus[a_, b_] = False

precedenceGreaterSpecialMinus2[Precedence`Infix`Minus, Precedence`Star] = True

precedenceGreaterSpecialMinus2[a_, b_] = False

precedenceGreaterSpecialPlus[Precedence`Infix`Plus, Precedence`Prefix`Plus] = True

precedenceGreaterSpecialPlus[a_, b_] =  False

precedenceMin[a_, b_] :=
  MinimalBy[{a, b}, lookupPrecedence][[1]]



(*
infix
*)

nodeStructure[CallNode[LeafNode[Symbol, "Plus", _], {_, _, ___}, _]] =
  structure[InfixNode[Plus, #, <||>]&, LeafNode[Token`Plus, "+", <||>], Precedence`Infix`Plus]

nodeStructure[CallNode[LeafNode[Symbol, "Times", _], {LeafNode[Integer, "-1", _], LeafNode[Integer | Real | Rational, _, _]}, _]] =
  structure[InfixNode[Times, #, <||>]&, LeafNode[Token`Star, "*", <||>], Precedence`Star]

nodeStructure[CallNode[LeafNode[Symbol, "Times", _], {LeafNode[Integer, "-1", _], _}, _]] =
  structure[PrefixNode[Minus, #, <||>]&, LeafNode[Token`Minus, "-", <||>], Precedence`Prefix`Minus]

nodeStructure[CallNode[LeafNode[Symbol, "Times", _], _, _]] = 
  structure[InfixNode[Times, #, <||>]&, LeafNode[Token`Star, "*", <||>], Precedence`Star]

nodeStructure[CallNode[LeafNode[Symbol, "And", _], _, _]] = 
  structure[InfixNode[And, #, <||>]&, LeafNode[Token`AmpAmp, "&&", <||>], Precedence`AmpAmp]

nodeStructure[CallNode[LeafNode[Symbol, "Or", _], _, _]] = 
  structure[InfixNode[Or, #, <||>]&, LeafNode[Token`BarBar, "||", <||>], Precedence`BarBar]

nodeStructure[CallNode[LeafNode[Symbol, "Dot", _], _, _]] = 
  structure[InfixNode[Dot, #, <||>]&, LeafNode[Token`Dot, ".", <||>], Precedence`Dot]

nodeStructure[CallNode[LeafNode[Symbol, "Alternatives", _], _, _]] = 
  structure[InfixNode[Alternatives, #, <||>]&, LeafNode[Token`Bar, "|", <||>], Precedence`Bar]

nodeStructure[CallNode[LeafNode[Symbol, "CompoundExpression", _], _, _]] = 
  structure[InfixNode[CompoundExpression, #, <||>]&, LeafNode[Token`Semi, ";", <||>], Precedence`Semi]

nodeStructure[CallNode[LeafNode[Symbol, "StringJoin", _], _, _]] = 
  structure[InfixNode[StringJoin, #, <||>]&, LeafNode[Token`LessGreater, "<>", <||>], Precedence`LessGreater]

nodeStructure[CallNode[LeafNode[Symbol, "RightComposition", _], _, _]] = 
  structure[InfixNode[RightComposition, #, <||>]&, LeafNode[Token`SlashStar, "/*", <||>], Precedence`SlashStar]

nodeStructure[CallNode[LeafNode[Symbol, "Composition", _], _, _]] = 
  structure[InfixNode[Composition, #, <||>]&, LeafNode[Token`AtStar, "@*", <||>], Precedence`AtStar]

nodeStructure[CallNode[LeafNode[Symbol, "StringExpression", _], _, _]] = 
  structure[InfixNode[StringExpression, #, <||>]&, LeafNode[Token`TildeTilde, "~~", <||>], Precedence`TildeTilde]

nodeStructure[CallNode[LeafNode[Symbol, "NonCommutativeMultiply", _], _, _]] =
   structure[InfixNode[NonCommutativeMultiply, #, <||>]&, LeafNode[Token`StarStar, "**", <||>], Precedence`StarStar]

nodeStructure[CallNode[LeafNode[Symbol, "SameQ", _], _, _]] = 
  structure[InfixNode[SameQ, #, <||>]&, LeafNode[Token`EqualEqualEqual, "===", <||>], Precedence`EqualEqualEqual]

nodeStructure[CallNode[LeafNode[Symbol, "UnsameQ", _], _, _]] = 
  structure[InfixNode[UnsameQ, #, <||>]&, LeafNode[Token`EqualBangEqual, "=!=", <||>], Precedence`EqualBangEqual]

nodeStructure[CallNode[LeafNode[Symbol, "MessageName", _], _, _]] = 
  structure[InfixNode[MessageName, #, <||>]&, LeafNode[Token`ColonColon, "::", <||>], Precedence`ColonColon]



(*
infix inequality
*)

nodeStructure[CallNode[LeafNode[Symbol, "Less", _], _, _]] =
  structure[InfixNode[InfixInequality, #, <||>]&, LeafNode[Token`Less, "<", <||>], Precedence`Class`Inequality]

nodeStructure[CallNode[LeafNode[Symbol, "Greater", _], _, _]] =
  structure[InfixNode[InfixInequality, #, <||>]&, LeafNode[Token`Greater, ">", <||>], Precedence`Class`Inequality]

nodeStructure[CallNode[LeafNode[Symbol, "Equal", _], _, _]] =
  structure[InfixNode[InfixInequality, #, <||>]&, LeafNode[Token`EqualEqual, "==", <||>], Precedence`Class`Inequality]

nodeStructure[CallNode[LeafNode[Symbol, "LessEqual", _], _, _]] =
  structure[InfixNode[InfixInequality, #, <||>]&, LeafNode[Token`LessEqual, "<=", <||>], Precedence`Class`Inequality]

nodeStructure[CallNode[LeafNode[Symbol, "GreaterEqual", _], _, _]] =
  structure[InfixNode[InfixInequality, #, <||>]&, LeafNode[Token`GreaterEqual, ">=", <||>], Precedence`Class`Inequality]

nodeStructure[CallNode[LeafNode[Symbol, "Unequal", _], _, _]] =
  structure[InfixNode[InfixInequality, #, <||>]&, LeafNode[Token`BangEqual, "!=", <||>], Precedence`Class`Inequality]



(*
prefix
*)

nodeStructure[CallNode[LeafNode[Symbol, "PreIncrement", _], _, _]] =
  structure[PrefixNode[PreIncrement, #, <||>]&, LeafNode[Token`PlusPlus, "++", <||>], Precedence`Prefix`PlusPlus]

nodeStructure[CallNode[LeafNode[Symbol, "PreDecrement", _], _, _]] =
  structure[PrefixNode[PreDecrement, #, <||>]&, LeafNode[Token`MinusMinus, "--", <||>], Precedence`Prefix`MinusMinus]

nodeStructure[CallNode[LeafNode[Symbol, "Not", _], _, _]] =
  structure[PrefixNode[Not, #, <||>]&, LeafNode[Token`Bang, "!", <||>], Precedence`Prefix`Bang]

nodeStructure[CallNode[LeafNode[Symbol, "Plus", _], {_}, _]] =
  structure[PrefixNode[Plus, #, <||>]&, LeafNode[Token`Plus, "+", <||>], Precedence`Prefix`Plus]



(*
binary
*)

nodeStructure[CallNode[LeafNode[Symbol, "Power", _], _, _]] =
  structure[BinaryNode[Power, #, <||>]&, LeafNode[Token`Caret, "^", <||>], Precedence`Caret]

nodeStructure[CallNode[LeafNode[Symbol, "Optional", _], _, _]] =
  structure[BinaryNode[Optional, #, <||>]&, LeafNode[Token`Colon, ":", <||>], Precedence`Fake`OptionalColon]

nodeStructure[CallNode[LeafNode[Symbol, "Pattern", _], _, _]] =
  structure[BinaryNode[Pattern, #, <||>]&, LeafNode[Token`Colon, ":", <||>], Precedence`Fake`PatternColon]

nodeStructure[CallNode[LeafNode[Symbol, "Set", _], _, _]] =
  structure[BinaryNode[Set, #, <||>]&, LeafNode[Token`Equal, "=", <||>], Precedence`Equal]

nodeStructure[CallNode[LeafNode[Symbol, "Unset", _], _, _]] =
  structure[BinaryNode[Unset, #, <||>]&, {LeafNode[Token`Equal, "=", <||>], LeafNode[Token`Dot, ".", <||>]}, Precedence`Equal]

nodeStructure[CallNode[LeafNode[Symbol, "SetDelayed", _], _, _]] =
  structure[BinaryNode[SetDelayed, #, <||>]&, LeafNode[Token`ColonEqual, ":=", <||>], Precedence`ColonEqual]

nodeStructure[CallNode[LeafNode[Symbol, "PatternTest", _], _, _]] =
  structure[BinaryNode[PatternTest, #, <||>]&, LeafNode[Token`Question, "?", <||>], Precedence`Infix`Question]

nodeStructure[CallNode[LeafNode[Symbol, "Rule", _], _, _]] =
  structure[BinaryNode[Rule, #, <||>]&, LeafNode[Token`MinusGreater, "->", <||>], Precedence`MinusGreater]

nodeStructure[CallNode[LeafNode[Symbol, "RuleDelayed", _], _, _]] =
  structure[BinaryNode[RuleDelayed, #, <||>]&, LeafNode[Token`ColonGreater, ":>", <||>], Precedence`ColonGreater]

nodeStructure[CallNode[LeafNode[Symbol, "SubtractFrom", _], _, _]] =
  structure[BinaryNode[SubtractFrom, #, <||>]&, LeafNode[Token`MinusEqual, "-=", <||>], Precedence`MinusEqual]

nodeStructure[CallNode[LeafNode[Symbol, "AddTo", _], _, _]] =
  structure[BinaryNode[AddTo, #, <||>]&, LeafNode[Token`PlusEqual, "+=", <||>], Precedence`PlusEqual]

nodeStructure[CallNode[LeafNode[Symbol, "TimesBy", _], _, _]] =
  structure[BinaryNode[TimesBy, #, <||>]&, LeafNode[Token`StarEqual, "*=", <||>], Precedence`StarEqual]

nodeStructure[CallNode[LeafNode[Symbol, "DivideBy", _], _, _]] =
  structure[BinaryNode[DivideBy, #, <||>]&, LeafNode[Token`SlashEqual, "/=", <||>], Precedence`SlashEqual]

nodeStructure[CallNode[LeafNode[Symbol, "Map", _], _, _]] =
  structure[BinaryNode[Map, #, <||>]&, LeafNode[Token`SlashAt, "/@", <||>], Precedence`SlashAt]

nodeStructure[CallNode[LeafNode[Symbol, "MapAll", _], _, _]] =
  structure[BinaryNode[MapAll, #, <||>]&, LeafNode[Token`SlashSlashAt, "//@", <||>], Precedence`SlashSlashAt]

nodeStructure[CallNode[LeafNode[Symbol, "Condition", _], _, _]] =
  structure[BinaryNode[Condition, #, <||>]&, LeafNode[Token`SlashSemi, "/;", <||>], Precedence`SlashSemi]

nodeStructure[CallNode[LeafNode[Symbol, "ReplaceAll", _], _, _]] =
  structure[BinaryNode[ReplaceAll, #, <||>]&, LeafNode[Token`SlashDot, "/.", <||>], Precedence`SlashDot]

nodeStructure[CallNode[LeafNode[Symbol, "ReplaceRepeated", _], _, _]] =
  structure[BinaryNode[ReplaceRepeated, #, <||>]&, LeafNode[Token`SlashSlashDot, "//.", <||>], Precedence`SlashSlashDot]

nodeStructure[CallNode[LeafNode[Symbol, "Apply", _], _, _]] =
  structure[BinaryNode[Apply, #, <||>]&, LeafNode[Token`AtAt, "@@", <||>], Precedence`AtAt]

nodeStructure[CallNode[LeafNode[Symbol, "UpSet", _], _, _]] =
  structure[BinaryNode[UpSet, #, <||>]&, LeafNode[Token`CaretEqual, "^=", <||>], Precedence`CaretEqual]

nodeStructure[CallNode[LeafNode[Symbol, "TwoWayRule", _], _, _]] =
  structure[BinaryNode[TwoWayRule, #, <||>]&, LeafNode[Token`LessMinusGreater, "<->", <||>], Precedence`LessMinusGreater]

(*
nodeStructure[CallNode[LeafNode[Symbol,"Function",_],_,_]] =
  structure[BinaryNode[Function,#,<||>]&,LeafNode[Token`BarMinusGreater,"|->",<||>], Precedence`BarMinusGreater]
*)

nodeStructure[CallNode[LeafNode[Symbol, "ApplyTo", _], _, _]] =
  structure[BinaryNode[ApplyTo, #, <||>]&, LeafNode[Token`SlashSlashEqual, "//=", <||>], Precedence`SlashSlashEqual]

nodeStructure[CallNode[LeafNode[Symbol, "Span", _], {_, _}, _]] =
  structure[BinaryNode[Span, #, <||>]&, LeafNode[Token`SemiSemi, ";;", <||>], Precedence`SemiSemi]


(*
ternary
*)

nodeStructure[CallNode[LeafNode[Symbol, "Span", _], {_, _, _}, _]] =
  structure[TernaryNode[Span, #, <||>]&, LeafNode[Token`SemiSemi, ";;", <||>], Precedence`SemiSemi]


(*
postfix
*)

nodeStructure[CallNode[CallNode[LeafNode[Symbol, "Derivative", _], {LeafNode[Integer, "1", _]}, _], _, _]] =
  structure[PostfixNode[Derivative, #, <||>]&, LeafNode[Token`SingleQuote, "'", <||>], Precedence`SingleQuote]

nodeStructure[CallNode[LeafNode[Symbol, "Increment", _], _, _]] =
  structure[PostfixNode[Increment, #, <||>]&, LeafNode[Token`PlusPlus, "++", <||>], Precedence`Postfix`PlusPlus]

nodeStructure[CallNode[LeafNode[Symbol, "Decrement", _], _, _]] =
  structure[PostfixNode[Decrement, #, <||>]&, LeafNode[Token`MinusMinus, "--", <||>], Precedence`Postfix`MinusMinus]

nodeStructure[CallNode[LeafNode[Symbol, "Function", _], _, _]] =
  structure[PostfixNode[Function, #, <||>]&, LeafNode[Token`Amp, "&", <||>], Precedence`Amp]

nodeStructure[CallNode[LeafNode[Symbol, "Factorial", _], _, _]] =
  structure[PostfixNode[Factorial, #, <||>]&, LeafNode[Token`Bang, "!", <||>], Precedence`Postfix`Bang]

nodeStructure[CallNode[LeafNode[Symbol, "Repeated", _], _, _]] =
  structure[PostfixNode[Repeated, #, <||>]&, LeafNode[Token`DotDot, "..", <||>], Precedence`DotDot]

nodeStructure[CallNode[LeafNode[Symbol, "RepeatedNull", _], _, _]] =
  structure[PostfixNode[RepeatedNull, #, <||>]&, LeafNode[Token`DotDotDot, "...", <||>], Precedence`DotDotDot]

nodeStructure[CallNode[LeafNode[Symbol, "Factorial2", _], _, _]] =
  structure[PostfixNode[Factorial2, #, <||>]&, LeafNode[Token`BangBang, "!!", <||>], Precedence`Postfix`BangBang]



(*
Leafs
*)

nodeStructure[n : LeafNode[Symbol, str_, _]] :=
  structure[
    Failure["Unhandled structure", <| "n" -> n |>]&
    ,
    Failure["badop", <| "n" -> n |>]
    ,
    If[StringStartsQ[str, "-1"],
      Precedence`Prefix`Minus
      ,
      Precedence`Symbol
    ]
  ]

nodeStructure[n : LeafNode[Integer, str_, _]] :=
  structure[
    Failure["Unhandled structure", <| "n" -> n |>]&
    ,
    Failure["badop", <| "n" -> n |>]
    ,
    If[StringStartsQ[str, "-1"],
      Precedence`Prefix`Minus
      ,
      Precedence`Highest
    ]
  ]

nodeStructure[n : LeafNode[Real, str_, _]] :=
  structure[
    Failure["Unhandled structure", <| "n" -> n |>]&
    ,
    Failure["badop", <| "n" -> n |>]
    ,
    If[StringStartsQ[str, "-1"],
      Precedence`Prefix`Minus
      ,
      Precedence`Highest
    ]
  ]

nodeStructure[n : LeafNode[Rational, str_, _]] :=
  structure[
    Failure["Unhandled structure", <| "n" -> n |>]&
    ,
    Failure["badop", <| "n" -> n |>]
    ,
    If[StringStartsQ[str, "-1"],
      Precedence`Prefix`Minus
      ,
      Precedence`Highest
    ]
  ]

nodeStructure[n : CallNode[head:LeafNode[Symbol, "Part", _], children_, data_]] :=
  structure[Failure["Unhandled structure", <| "n" -> n |>]&, Failure["badop", <| "n" -> n |>], Precedence`Call]

nodeStructure[n : CallNode[head_, children_, data_]] :=
  structure[Failure["Unhandled structure", <| "n" -> n |>]&, Failure["badop", <| "n" -> n |>], Precedence`Call]

nodeStructure[a_] :=
  structure[Failure["Unhandled structure", <| "a" -> a |>]&, Failure["badop", <| "a" -> a |>], Failure["badprec", <| "a" -> a |>]]





precCTL[LeafNode[Symbol, _, _]] = Precedence`Symbol
precCTR[LeafNode[Symbol, _, _]] = Precedence`Symbol

precCTL[LeafNode[Integer | Real | Rational, _, _]] = Precedence`Highest
precCTR[LeafNode[Integer | Real | Rational, str /; StringStartsQ[str, "-"], _]] = Precedence`Prefix`Minus
precCTR[LeafNode[Integer | Real | Rational, _, _]] = Precedence`Highest

precCTL[LeafNode[String, _, _]] = Precedence`Highest
precCTR[LeafNode[String, _, _]] = Precedence`Highest

precCTL[LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder | Token`UnderDot, _, _]] = Precedence`Highest
precCTR[LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder | Token`UnderDot, _, _]] = Precedence`Highest

precCTL[LeafNode[Token`LinearSyntaxBlob, _, _]] = Precedence`Highest
precCTR[LeafNode[Token`LinearSyntaxBlob, _, _]] = Precedence`Highest

precCTL[CompoundNode[Blank | BlankSequence | BlankNullSequence, _, _]] = Precedence`Highest
precCTR[CompoundNode[Blank | BlankSequence | BlankNullSequence, _, _]] = Precedence`Highest

precCTL[CompoundNode[PatternBlank | PatternBlankSequence | PatternBlankNullSequence, _, _]] = Precedence`Highest
precCTR[CompoundNode[PatternBlank | PatternBlankSequence | PatternBlankNullSequence, _, _]] = Precedence`Highest

precCTL[LeafNode[Token`Hash | Token`HashHash, _, _]] = Precedence`Highest
precCTR[LeafNode[Token`Hash | Token`HashHash, _, _]] = Precedence`Highest

precCTL[CompoundNode[Slot | SlotSequence, _, _]] = Precedence`Highest
precCTR[CompoundNode[Slot | SlotSequence, _, _]] = Precedence`Highest

precCTL[GroupNode[_, _, _]] = Precedence`Highest
precCTR[GroupNode[_, _, _]] = Precedence`Highest


precCTL[BinaryNode[Power, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Caret]
precCTR[BinaryNode[Power, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Caret]

precCTL[BinaryNode[Divide, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Slash]
precCTR[BinaryNode[Divide, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Slash]

precCTL[BinaryNode[Set, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Equal]
precCTR[BinaryNode[Set, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Equal]

precCTL[BinaryNode[Unset, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Fake`EqualDot]
precCTR[BinaryNode[Unset, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Fake`EqualDot]

precCTL[BinaryNode[Pattern, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Fake`PatternColon]
precCTR[BinaryNode[Pattern, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Fake`PatternColon]

precCTL[BinaryNode[Optional, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Fake`OptionalColon]
precCTR[BinaryNode[Optional, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Fake`OptionalColon]

precCTL[BinaryNode[Map, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SlashAt]
precCTR[BinaryNode[Map, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SlashAt]

precCTL[BinaryNode[Apply, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`AtAt]
precCTR[BinaryNode[Apply, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`AtAt]

precCTL[BinaryNode[UpSet, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`CaretEqual]
precCTR[BinaryNode[UpSet, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`CaretEqual]

precCTL[BinaryNode[SubtractFrom, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`MinusEqual]
precCTR[BinaryNode[SubtractFrom, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`MinusEqual]

precCTL[BinaryNode[TimesBy, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`StarEqual]
precCTR[BinaryNode[TimesBy, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`StarEqual]

precCTL[BinaryNode[MapAll, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SlashSlashAt]
precCTR[BinaryNode[MapAll, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SlashSlashAt]

precCTL[BinaryNode[Rule, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`MinusGreater]
precCTR[BinaryNode[Rule, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`MinusGreater]

precCTL[BinaryNode[ReplaceAll, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SlashDot]
precCTR[BinaryNode[ReplaceAll, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SlashDot]

precCTL[BinaryNode[RuleDelayed, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`ColonGreater]
precCTR[BinaryNode[RuleDelayed, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`ColonGreater]

precCTL[BinaryNode[AddTo, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`PlusEqual]
precCTR[BinaryNode[AddTo, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`PlusEqual]

precCTL[BinaryNode[SetDelayed, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`ColonEqual]
precCTR[BinaryNode[SetDelayed, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`ColonEqual]

precCTL[BinaryNode[TwoWayRule, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`LessMinusGreater]
precCTR[BinaryNode[TwoWayRule, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`LessMinusGreater]

precCTL[BinaryNode[PatternTest, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Infix`Question]
precCTR[BinaryNode[PatternTest, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Infix`Question]

precCTL[BinaryNode[Condition, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SlashSemi]
precCTR[BinaryNode[Condition, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SlashSemi]

precCTL[BinaryNode[ReplaceRepeated, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SlashSlashDot]
precCTR[BinaryNode[ReplaceRepeated, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SlashSlashDot]

precCTL[BinaryNode[DivideBy, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SlashEqual]
precCTR[BinaryNode[DivideBy, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SlashEqual]

precCTL[BinaryNode[ApplyTo, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SlashSlashEqual]
precCTR[BinaryNode[ApplyTo, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SlashSlashEqual]

precCTL[BinaryNode[Span, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SemiSemi]
precCTR[BinaryNode[Span, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SemiSemi]

precCTL[TernaryNode[Span, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SemiSemi]
precCTR[TernaryNode[Span, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SemiSemi]

precCTL[InfixNode[Times, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Star]
precCTR[InfixNode[Times, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Star]

precCTL[InfixNode[Plus, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Infix`Plus]
precCTR[InfixNode[Plus, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Infix`Plus]

precCTL[InfixNode[Dot, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Dot]
precCTR[InfixNode[Dot, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Dot]

precCTL[InfixNode[InfixInequality, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Class`Inequality]
precCTR[InfixNode[InfixInequality, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Class`Inequality]

precCTL[InfixNode[StringJoin, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`LessGreater]
precCTR[InfixNode[StringJoin, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`LessGreater]

precCTL[InfixNode[Alternatives, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Bar]
precCTR[InfixNode[Alternatives, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Bar]

precCTL[InfixNode[Composition, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`AtStar]
precCTR[InfixNode[Composition, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`AtStar]

precCTL[InfixNode[RightComposition, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SlashStar]
precCTR[InfixNode[RightComposition, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`SlashStar]

precCTL[InfixNode[NonCommutativeMultiply, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`StarStar]
precCTR[InfixNode[NonCommutativeMultiply, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`StarStar]

precCTL[InfixNode[And, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`AmpAmp]
precCTR[InfixNode[And, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`AmpAmp]

precCTL[InfixNode[Or, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`BarBar]
precCTR[InfixNode[Or, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`BarBar]

precCTL[InfixNode[StringExpression, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`TildeTilde]
precCTR[InfixNode[StringExpression, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`TildeTilde]

precCTL[InfixNode[UnsameQ, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`EqualBangEqual]
precCTR[InfixNode[UnsameQ, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`EqualBangEqual]

precCTL[InfixNode[SameQ, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`EqualEqualEqual]
precCTR[InfixNode[SameQ, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`EqualEqualEqual]

precCTL[InfixNode[CompoundExpression, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Semi]
precCTR[InfixNode[CompoundExpression, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`Semi]

precCTL[InfixNode[MessageName, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`ColonColon]
precCTR[InfixNode[MessageName, {first_, ___}, _]] := precedenceMin[precCTR[first], Precedence`ColonColon]

precCTL[PrefixNode[Plus, _, _]] = Precedence`Prefix`Plus
precCTR[PrefixNode[Plus, _, _]] = Precedence`Prefix`Plus

precCTL[PrefixNode[PreIncrement, _, _]] = Precedence`Prefix`PlusPlus
precCTR[PrefixNode[PreIncrement, _, _]] = Precedence`Prefix`PlusPlus

precCTL[PrefixNode[PreDecrement, _, _]] = Precedence`Prefix`MinusMinus
precCTR[PrefixNode[PreDecrement, _, _]] = Precedence`Prefix`MinusMinus

precCTL[PrefixNode[Minus, _, _]] = Precedence`Prefix`Minus
precCTR[PrefixNode[Minus, _, _]] = Precedence`Prefix`Minus

precCTL[PrefixNode[Not, _, _]] = Precedence`Prefix`Bang
precCTR[PrefixNode[Not, _, _]] = Precedence`Prefix`Bang

precCTL[PostfixNode[Factorial, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Postfix`Bang]
precCTR[PostfixNode[Factorial, {first_, ___}, _]] = Precedence`Highest

precCTL[PostfixNode[Factorial2, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Postfix`BangBang]
precCTR[PostfixNode[Factorial2, {first_, ___}, _]] = Precedence`Highest

precCTL[PostfixNode[Increment, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Postfix`PlusPlus]
precCTR[PostfixNode[Increment, {first_, ___}, _]] = Precedence`Highest

precCTL[PostfixNode[Decrement, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Postfix`MinusMinus]
precCTR[PostfixNode[Decrement, {first_, ___}, _]] = Precedence`Highest

precCTL[PostfixNode[Function, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`Amp]
precCTR[PostfixNode[Function, {first_, ___}, _]] = Precedence`Highest

precCTL[PostfixNode[Repeated, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`DotDot]
precCTR[PostfixNode[Repeated, {first_, ___}, _]] = Precedence`Highest

precCTL[PostfixNode[RepeatedNull, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`DotDotDot]
precCTR[PostfixNode[RepeatedNull, {first_, ___}, _]] = Precedence`Highest

precCTL[PostfixNode[Derivative, {first_, ___}, _]] := precedenceMin[precCTL[first], Precedence`SingleQuote]
precCTR[PostfixNode[Derivative, {first_, ___}, _]] = Precedence`Highest

precCTL[CallNode[{h_}, _, _]] := precedenceMin[precCTL[h], Precedence`Call]
precCTR[CallNode[{h_}, _, _]] := precedenceMin[precCTR[h], Precedence`Call]

precCTL[PrefixNode[PrefixLinearSyntaxBang, _, _]] = Precedence`LinearSyntax`Bang
precCTR[PrefixNode[PrefixLinearSyntaxBang, _, _]] = Precedence`LinearSyntax`Bang

precCTL[ErrorNode[_, _, _]] = Precedence`Highest
precCTR[ErrorNode[_, _, _]] = Precedence`Highest

precCTL[cst_?FailureQ] := cst
precCTR[cst_?FailureQ] := cst

precCTL[cst_] :=
  Failure["unhandled precCTL: ", <| "cst" -> cst |>]

precCTR[cst_] :=
  Failure["unhandled precCTR: ", <| "cst" -> cst |>]


(*
used mainly with juxtaposing
*)
firstPrec[LeafNode[Symbol, _, _]] = Precedence`Symbol

firstPrec[LeafNode[Integer | Real | Rational, _, _]] = Precedence`Highest

firstPrec[LeafNode[String, _, _]] = Precedence`Highest

firstPrec[LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder | Token`UnderDot, _, _]] = Precedence`Highest

firstPrec[CompoundNode[Blank | BlankSequence | BlankNullSequence, _, _]] = Precedence`Highest

firstPrec[CompoundNode[PatternBlank | PatternBlankSequence | PatternBlankNullSequence, _, _]] = Precedence`Highest

firstPrec[BinaryNode[_, {first_, ___}, _]] :=
  firstPrec[first]

firstPrec[InfixNode[_, {first_, ___}, _]] :=
  firstPrec[first]

firstPrec[n : PrefixNode[_, _, _]] :=
  precCTL[n]

firstPrec[PostfixNode[_, {first_, ___}, _]] :=
  firstPrec[first]

firstPrec[CallNode[{h_, ___}, _, _]] :=
  firstPrec[h]

firstPrec[GroupNode[_, _, _]] = Precedence`Highest

firstPrec[ErrorNode[_, _, _]] = Precedence`Lowest

firstPrec[cst_] :=
  Failure["unhandled firstPrec: ", <| "cst" -> cst |>]


(*
used mainly with juxtaposing
*)
lastPrec[LeafNode[Symbol, _, _]] = Precedence`Symbol

lastPrec[LeafNode[Integer | Real | Rational, _, _]] = Precedence`Highest

lastPrec[LeafNode[String, _, _]] = Precedence`Highest

lastPrec[LeafNode[Token`Under | Token`UnderUnder | Token`UnderUnderUnder | Token`UnderDot, _, _]] = Precedence`Highest

lastPrec[CompoundNode[Blank | BlankSequence | BlankNullSequence, _, _]] = Precedence`Highest

lastPrec[CompoundNode[PatternBlank | PatternBlankSequence | PatternBlankNullSequence, _, _]] = Precedence`Highest

lastPrec[BinaryNode[_, {___, last_}, _]] :=
  lastPrec[last]

lastPrec[InfixNode[_, {___, last_}, _]] :=
  lastPrec[last]

lastPrec[n : PostfixNode[_, _, _]] :=
  precCTL[n]

lastPrec[PrefixNode[_, {___, last_}, _]] :=
  lastPrec[last]

lastPrec[CallNode[_, _, _]] = Precedence`Highest

lastPrec[GroupNode[_, _, _]] = Precedence`Highest

lastPrec[ErrorNode[_, _, _]] = Precedence`Lowest

lastPrec[cst_] :=
  Failure["unhandled lastPrec: ", <| "cst" -> cst |>]



okToJuxtapose[Precedence`Infix`Plus, Precedence`Prefix`Plus] = False

okToJuxtapose[Precedence`Infix`Plus, Precedence`Prefix`PlusPlus] = False

okToJuxtapose[Precedence`Prefix`Plus, Precedence`Prefix`PlusPlus] = False

(*
There is no Precedence`Prefix`Plus Precedence`Prefix`Plus
*)

(*
not ambiguous, but needed to prevent syntax errors
*)
okToJuxtapose[Precedence`Amp, Precedence`AmpAmp] = False

okToJuxtapose[Precedence`Amp, Precedence`Amp] = False

(*
really just for Integer, Real, Rational
*)
okToJuxtapose[Precedence`Highest, Precedence`Dot] = False

okToJuxtapose[Precedence`Highest, Precedence`DotDot] = False

okToJuxtapose[Precedence`Highest, Precedence`DotDotDot] = False

(*
really just for 123` + 45
*)
okToJuxtapose[Precedence`Highest, Precedence`Infix`Plus] = False

okToJuxtapose[Precedence`Highest, Precedence`Infix`Minus] = False

okToJuxtapose[Precedence`Dot, Precedence`Highest] = False

okToJuxtapose[Precedence`SlashDot, Precedence`Highest] = False

okToJuxtapose[Precedence`SlashSlashDot, Precedence`Highest] = False

okToJuxtapose[Precedence`DotDot, Precedence`Dot] = False

okToJuxtapose[Precedence`DotDot, Precedence`DotDotDot] = False


okToJuxtapose[Precedence`Postfix`Bang, Precedence`Postfix`Bang] = False

okToJuxtapose[Precedence`Postfix`Bang, Precedence`Postfix`BangBang] = False

okToJuxtapose[Precedence`Postfix`Bang, Precedence`Equal] = False

(*
really just for a! == b
*)
okToJuxtapose[Precedence`Postfix`Bang, Precedence`Class`Inequality] = False

okToJuxtapose[Precedence`Infix`Minus, Precedence`Prefix`Minus] = False

okToJuxtapose[Precedence`Infix`Minus, Precedence`Prefix`MinusMinus] = False

okToJuxtapose[Precedence`Prefix`Minus, Precedence`Prefix`Minus] = False

okToJuxtapose[Precedence`Prefix`Minus, Precedence`Prefix`MinusMinus] = False


okToJuxtapose[a_, b_] = True



(*
infix
*)

walk[n : CallNode[LeafNode[Symbol, "Times", _], children:{LeafNode[Integer, "-1", _], LeafNode[Integer | Real | Rational, _, _]}, _]] :=
Module[{struct, ctor, op},
  struct = structure[InfixNode[Times, #, <||>]&, LeafNode[Token`Star, "*", <||>], Precedence`Star];
  ctor = struct["ctor"];
  op = struct["op"];
  (* prec = struct["prec"]; *)
  ctor[Flatten[{children[[1]]} ~Join~ {op} ~Join~ {children[[2]]}]]
]

walk[n : CallNode[LeafNode[Symbol, "Times", _], children:{LeafNode[Integer, "-1", _], _}, _]] :=
Module[{struct, ctor, op, prec},
  (*
  treat as prefix -, but only if child is not Integer | Real | Rational

  e.g.,
  -1 2 should stay -1 2
  *)
  struct = structure[PrefixNode[Minus, #, <||>]&, LeafNode[Token`Minus, "-", <||>], Precedence`Prefix`Minus];
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{op, Function[{c},
    Module[{walked},
      walked = walk[c];
      Which[
        (*
        allow "-(a+b)" to work
        *)
        precedenceGreater[prec, precCTL[walked]],
          {paren[walked]}
        ,
        precedenceEqual[prec, precCTL[walked]],
          {paren[walked]}
        ,
        !okToJuxtapose[prec, firstPrec[walked]],
          {spaceBefore[walked]}
        ,
        True,
          {walked}
      ]
    ]
  ][children[[2]]]}]]
]

walk[n : CallNode[LeafNode[Symbol, "Times", _], {a_, CallNode[LeafNode[Symbol, "Power", _], {b_, LeafNode[Integer, "-1", _]}, _]}, _]] :=
Module[{struct, ctor, op, prec},
  (*
  treat as binary /
  *)
  struct = structure[BinaryNode[Divide, #, <||>]&, LeafNode[Token`Slash, "/", <||>], Precedence`Slash];
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{Function[{c},
        Module[{walked},
          walked = walk[c];
          Which[
            precedenceGreater[prec, precCTL[walked]],
              paren[walked]
            ,
            !okToJuxtapose[lastPrec[walked], prec],
              spaceAfter[walked]
            ,
            True,
              walked
          ]
        ]
      ][a]} ~Join~ {op} ~Join~ {Function[{c},
      Module[{walked},
        walked = walk[c];
        Which[
          precedenceGreater[prec, precCTL[walked]],
            paren[walked]
          ,
          precedenceEqual[prec, precCTL[walked]],
            paren[walked]
          ,
          !okToJuxtapose[prec, firstPrec[walked]],
            spaceBefore[walked]
          ,
          True,
            walked
        ]
      ]
    ][b]}]]
]

walk[n : CallNode[LeafNode[Symbol, "Times", _], children : {_, _, ___}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[
    Flatten[
      {Function[{c},
            Module[{walked},
              walked = walk[c];
              Which[
                precedenceGreater[prec, precCTL[walked]],
                  paren[walked]
                ,
                (*
                needs to use the precedenceEqualNoPostfix function, 
                e.g., to keep "- -2 b" correct, which parses as Times[Times[-1,-2],b]
                *)
                precedenceEqual[prec, precCTL[walked]],
                  paren[walked]
                ,
                !okToJuxtapose[lastPrec[walked], prec],
                  spaceAfter[walk[c]]
                ,
                True,
                  walked
              ]
            ]
          ][First[children]]} ~Join~
          (
            Function[{c},
                Module[{walked},
                  walked = walk[c];
                  {
                    op
                    ,
                    Which[
                      precedenceGreater[prec, precCTL[walked]],
                        paren[walked]
                      ,
                      (*
                      Needs to use precedenceEqual
                      to have "a * - - 2 * b" work
                      *)
                      precedenceEqual[prec, precCTL[walked]],
                        paren[walked]
                      ,
                      !okToJuxtapose[prec, firstPrec[walked]] && !okToJuxtapose[lastPrec[walked], prec],
                        spaceBoth[walked]
                      ,
                      !okToJuxtapose[prec, firstPrec[walked]],
                        spaceBefore[walked]
                      ,
                      !okToJuxtapose[lastPrec[walked], prec],
                        spaceAfter[walked]
                      ,
                      True,
                        walked
                    ]
                  }
                ]
              ] /@ children[[2 ;; -2]]
          ) ~Join~ (Function[{c},
          Module[{walked},
            walked = walk[c];
            {
              op
              ,
              Which[
                precedenceGreater[prec, precCTL[walked]],
                  paren[walked]
                ,
                (*
                Needs to use precedenceEqual
                to have "a * - - 2" work
                *)
                precedenceEqual[prec, precCTL[walked]],
                  paren[walked]
                ,
                !okToJuxtapose[prec, firstPrec[walked]],
                  spaceBefore[walked]
                ,
                True,
                  walked
              ]
            }
          ]
        ][Last[children]])
    ]
  ]
]

walk[n : CallNode[LeafNode[Symbol, "Plus", _], children : {_, _, ___}, _]] :=
Module[{structs},
  structs =
    Function[{c},
        Which[
          MatchQ[c, CallNode[LeafNode[Symbol, "Times", _], {LeafNode[Integer, "-1", _], LeafNode[Integer | Real | Rational, _, _]}, _]],
            structure[Identity, LeafNode[Token`Plus, "+", <||>], Precedence`Infix`Plus]
          ,
          MatchQ[c, CallNode[LeafNode[Symbol, "Times", _], {LeafNode[Integer, "-1", _], _}, _]],
            structure[PrefixNode[Minus, #, <||>]&, LeafNode[Token`Minus, "-", <||>], Precedence`Infix`Minus]
          ,
          MatchQ[c, LeafNode[Integer | Real | Rational, str_ /; StringStartsQ[str, "-"], _]],
            structure[Identity, LeafNode[Token`Minus, "-", <||>], Precedence`Prefix`Minus]
          ,
          True,
            structure[Identity, LeafNode[Token`Plus, "+", <||>], Precedence`Infix`Plus]
        ]
      ] /@ Rest[children];
  InfixNode[Plus, #, <||>]&[
    Flatten[
      {Function[{c},
              Module[{walked},
                walked = walk[c];
                Which[
                  precedenceLess[precCTR[walked], First[structs]["prec"]],
                    paren[walked]
                  ,
                  precedenceEqual[precCTR[walked], First[structs]["prec"]],
                    paren[walked]
                  ,
                  precCTR[walked] === Precedence`Prefix`Plus && First[structs]["prec"] === Precedence`Infix`Plus,
                    (*
                    allow  Plus[args]+1  to work
                    *)
                    paren[walked]
                  ,
                  !okToJuxtapose[lastPrec[walked], First[structs]["prec"]],
                    spaceAfter[walked]
                  ,
                  True,
                    walked
                ]
              ]
            ][First[children]]} ~Join~
            MapThread[
              Function[{structPair, c},
                Module[{walked},
                  Switch[structPair[[1]]["prec"],
                    Precedence`Infix`Minus,
                      walked = walk[c[[2, 2]]];
                    ,
                    Precedence`Prefix`Minus,
                      walked = walk[c][[2, 2]];
                    ,
                    _,
                      walked = walk[c]
                  ];
                  {
                    structPair[[1]]["op"]
                    ,
                    Which[
                      precedenceGreater[structPair[[1]]["prec"], precCTL[walked]],
                        paren[walked]
                      ,
                      precedenceEqual[structPair[[1]]["prec"], precCTL[walked]],
                        paren[walked]
                      ,
                      (*
                      allow "a - - -2 - b" to work
                      *)
                      precedenceGreaterSpecialMinus[structPair[[1]]["prec"], firstPrec[walked]],
                        paren[walked]
                      ,
                      (*
                      allow  1 - (a b) + 2  to work
                      *)
                      structPair[[1]]["prec"] === Precedence`Infix`Minus && precCTL[walked] === Precedence`Star,
                        paren[walked]
                      ,
                      (*
                      allow "a + (+b)" to work
                      *)
                      precedenceGreaterSpecialPlus[Last[structs]["prec"], precCTL[walked]],
                        paren[walked]
                      ,
                      !okToJuxtapose[structPair[[1]]["prec"], firstPrec[walked]] && !okToJuxtapose[lastPrec[walked], structPair[[2]]["prec"]],
                        spaceBoth[walked]
                      ,
                      !okToJuxtapose[structPair[[1]]["prec"], firstPrec[walked]],
                        spaceBefore[walked]
                      ,
                      !okToJuxtapose[lastPrec[walked], structPair[[2]]["prec"]],
                        spaceAfter[walked]
                      ,
                      True,
                        walked
                    ]
                  }
                ]
              ]
              ,
              {Partition[structs, 2, 1], children[[2 ;; -2]]}
            ] ~Join~ {Last[structs]["op"]} ~Join~ {Function[{c},
          Module[{walked},
            Switch[Last[structs]["prec"],
              Precedence`Infix`Minus,
                walked = walk[c[[2, 2]]];
              ,
              Precedence`Prefix`Minus,
                walked = walk[c][[2, 2]];
              ,
              _,
                walked = walk[c]
            ];
            Which[
              precedenceGreater[Last[structs]["prec"], precCTL[walked]],
                paren[walked]
              ,
              precedenceEqual[Last[structs]["prec"], precCTL[walked]],
                paren[walked]
              ,
              (*
              allow "x - Times[y, z]" to work
              *)
              precedenceGreaterSpecialMinus2[Last[structs]["prec"], precCTL[walked]],
                paren[walked]
              ,
              (*
              allow "a - - -2" to work
              *)
              precedenceGreaterSpecialMinus[Last[structs]["prec"], firstPrec[walked]],
                paren[walked]
              ,
              (*
              allow "a + (+b)" to work
              *)
              precedenceGreaterSpecialPlus[Last[structs]["prec"], precCTL[walked]],
                paren[walked]
              ,
              !okToJuxtapose[Last[structs]["prec"], firstPrec[walked]],
                spaceBefore[walked]
              ,
              True,
                walked
            ]
          ]
        ][Last[children]]}
    ]
  ]
]

(*
handle implicit Null in a;
*)
walk[n:CallNode[LeafNode[Symbol, "CompoundExpression", _], children : {_, _, ___}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[
    Flatten[
      {Function[{c},
            Module[{walked},
              walked = walk[c];
              Which[
                precedenceLess[precCTR[walked], prec],
                  paren[walked]
                ,
                precedenceEqual[precCTR[walked], prec],
                  paren[walked]
                ,
                !okToJuxtapose[lastPrec[walked], prec],
                  spaceAfter[walked]
                ,
                True,
                  walked
              ]
            ]
          ][First[children]]} ~Join~
          (
            Function[{c},
                Module[{walked},
                  walked = walk[c];
                  {
                    op
                    ,
                    Which[
                      MatchQ[walked, LeafNode[Symbol, "Null", _]],
                        spaceAfter[LeafNode[Token`Fake`ImplicitNull, "", <||>]]
                      ,
                      precedenceGreater[prec, precCTL[walked]],
                        paren[walked]
                      ,
                      precedenceEqual[prec, precCTL[walked]],
                        paren[walked]
                      ,
                      !okToJuxtapose[prec, firstPrec[walked]] && !okToJuxtapose[lastPrec[walked], prec],
                        spaceBoth[walked]
                      ,
                      !okToJuxtapose[prec, firstPrec[walked]],
                        spaceBefore[walked]
                      ,
                      !okToJuxtapose[lastPrec[walked], prec],
                        spaceAfter[walked]
                      ,
                      True,
                        walked
                    ]
                  }
                ]
              ] /@ children[[2 ;; -2]]
          ) ~Join~ (Function[{c},
          Module[{walked},
            walked = walk[c];
            {
              op
              ,
              Which[
                MatchQ[walked, LeafNode[Symbol, "Null", _]],
                  LeafNode[Token`Fake`ImplicitNull, "", <||>]
                ,
                precedenceGreater[prec, precCTL[walked]],
                  paren[walked]
                ,
                precedenceEqual[prec, precCTL[walked]],
                  paren[walked]
                ,
                !okToJuxtapose[prec, firstPrec[walked]],
                  spaceBefore[walked]
                ,
                True,
                  walked
              ]
            }
          ]
        ][Last[children]])
    ]
  ]
]

walk[
  n :
    CallNode[
      LeafNode[
        Symbol
        ,
        "And" | "Or" | "Alternatives" | "Dot" |
        "StringJoin" | "RightComposition" |
        "Composition" | "StringExpression" |
        "NonCommutativeMultiply" | "SameQ" | "UnsameQ" |
        "MessageName" |
        (* inequality *)
        "Less" | "Greater" | "Equal" | "LessEqual" |
        "GreaterEqual" | "Unequal"
        ,
        _
      ]
      ,
      children : {_, _, ___}
      ,
      _
    ]
] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[
    Flatten[
      {Function[{c},
            Module[{walked},
              walked = walk[c];
              Which[
                precedenceLess[precCTR[walked], prec],
                  paren[walked]
                ,
                precedenceEqual[precCTR[walked], prec],
                  paren[walked]
                ,
                !okToJuxtapose[lastPrec[walked], prec],
                  spaceAfter[walked]
                ,
                True,
                  walked
              ]
            ]
          ][First[children]]} ~Join~
          (
            Function[{c},
                Module[{walked},
                  walked = walk[c];
                  {
                    op
                    ,
                    Which[
                      precedenceGreater[prec, precCTL[walked]],
                        paren[walked]
                      ,
                      precedenceEqual[prec, precCTL[walked]],
                        paren[walked]
                      ,
                      !okToJuxtapose[prec, firstPrec[walked]] && !okToJuxtapose[lastPrec[walked], prec],
                        spaceBoth[walked]
                      ,
                      !okToJuxtapose[prec, firstPrec[walked]],
                        spaceBefore[walked]
                      ,
                      !okToJuxtapose[lastPrec[walked], prec],
                        spaceAfter[walked]
                      ,
                      True,
                        walked
                    ]
                  }
                ]
              ] /@ children[[2 ;; -2]]
          ) ~Join~ (Function[{c},
          Module[{walked},
            walked = walk[c];
            {
              op
              ,
              Which[
                precedenceGreater[prec, precCTL[walked]],
                  paren[walked]
                ,
                precedenceEqual[prec, precCTL[walked]],
                  paren[walked]
                ,
                !okToJuxtapose[prec, firstPrec[walked]],
                  spaceBefore[walked]
                ,
                True,
                  walked
              ]
            }
          ]
        ][Last[children]])
    ]
  ]
]



(*
binary
*)

walk[n : CallNode[LeafNode[Symbol, "Unset", _], children : {_}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{Function[{c},
        Module[{walked},
          walked = walk[c];
          Which[
            precedenceGreater[prec, precCTL[walked]],
              paren[walked]
            ,
            !okToJuxtapose[lastPrec[walked], prec],
              spaceAfter[walked]
            ,
            True,
              walked
          ]
        ]
      ][children[[1]]]} ~Join~ {op}]]
]

(*
handle Pattern specially
*)
walk[n : CallNode[LeafNode[Symbol, "Pattern", _], children : {LeafNode[Symbol, _, _], _}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{Function[{c},
        Module[{walked},
          walked = walk[c];
          Which[
            precedenceGreater[prec, precCTL[walked]],
              paren[walked]
            ,
            !okToJuxtapose[lastPrec[walked], prec],
              spaceAfter[walked]
            ,
            True,
              walked
          ]
        ]
      ][children[[1]]]} ~Join~ {op} ~Join~ {Function[{c},
      Module[{walked},
        walked = walk[c];
        Which[
          precedenceGreater[prec, precCTL[walked]],
            paren[walked]
          ,
          precedenceEqual[prec, precCTL[walked]],
            paren[walked]
          ,
          !okToJuxtapose[prec, firstPrec[walked]],
            spaceBefore[walked]
          ,
          True,
            walked
        ]
      ]
    ][children[[2]]]}]]
]

(*
handle Optional specially
*)
walk[n : CallNode[LeafNode[Symbol, "Optional", _], children : {CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, _, _], _}, _], _}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{Function[{c},
        Module[{walked},
          walked = walk[c];
          Which[
            precedenceGreater[prec, precCTL[walked]],
              paren[walked]
            ,
            !okToJuxtapose[lastPrec[walked], prec],
              spaceAfter[walked]
            ,
            True,
              walked
          ]
        ]
      ][children[[1]]]} ~Join~ {op} ~Join~ {Function[{c},
      Module[{walked},
        walked = walk[c];
        Which[
          precedenceGreater[prec, precCTL[walked]],
            paren[walked]
          ,
          precedenceEqual[prec, precCTL[walked]],
            paren[walked]
          ,
          !okToJuxtapose[prec, firstPrec[walked]],
            spaceBefore[walked]
          ,
          True,
            walked
        ]
      ]
    ][children[[2]]]}]]
]

(*
Left associative
*)
walk[n : CallNode[LeafNode[Symbol, "Condition" | "PatternTest" | "ReplaceAll" | "ReplaceRepeated" | "Span", _], children : {_, _}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{Function[{c},
        Module[{walked},
          walked = walk[c];
          Which[
            precedenceGreater[prec, precCTL[walked]],
              paren[walked]
            ,
            !okToJuxtapose[lastPrec[walked], prec],
              spaceAfter[walked]
            ,
            True,
              walked
          ]
        ]
      ][children[[1]]]} ~Join~ {op} ~Join~ {Function[{c},
      Module[{walked},
        walked = walk[c];
        Which[
          precedenceGreater[prec, precCTL[walked]],
            paren[walked]
          ,
          precedenceEqual[prec, precCTL[walked]],
            paren[walked]
          ,
          !okToJuxtapose[prec, firstPrec[walked]],
            spaceBefore[walked]
          ,
          True,
            walked
        ]
      ]
    ][children[[2]]]}]]
]
  
(*
Right associative
*)
walk[n : CallNode[LeafNode[Symbol, "AddTo" | "Apply" |  "ApplyTo" | "DivideBy" | "Map" | "MapAll" | "Power" | "Rule" | "RuleDelayed" | "Set" | "SetDelayed" | "SubtractFrom" | "TimesBy" | "TwoWayRule" | "UpSet", _], children : {_, _}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{Function[{c},
        Module[{walked},
          walked = walk[c];
          Which[
            precedenceGreater[prec, precCTL[walked]],
              paren[walked]
            ,
            precedenceEqual[prec, precCTL[walked]],
              paren[walked]
            ,
            !okToJuxtapose[lastPrec[walked], prec],
              spaceAfter[walked]
            ,
            True,
              walked
          ]
        ]
      ][children[[1]]]} ~Join~ {op} ~Join~ {Function[{c},
      Module[{walked},
        walked = walk[c];
        Which[
          precedenceGreater[prec, precCTL[walked]],
            paren[walked]
          ,
          !okToJuxtapose[prec, firstPrec[walked]],
            spaceBefore[walked]
          ,
          True,
            walked
        ]
      ]
    ][children[[2]]]}]]
]


(*
ternary
*)

walk[
  n :
    CallNode[
      LeafNode[
        Symbol
        ,
        "Span"
        ,
        _
      ]
      ,
      children : {_, _, _}
      ,
      _
    ]
] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[
    Flatten[
      {Function[{c},
            Module[{walked},
              walked = walk[c];
              Which[
                precedenceLess[precCTR[walked], prec],
                  paren[walked]
                ,
                precedenceEqual[precCTR[walked], prec],
                  paren[walked]
                ,
                !okToJuxtapose[lastPrec[walked], prec],
                  spaceAfter[walked]
                ,
                True,
                  walked
              ]
            ]
          ][First[children]]} ~Join~
          (
            Function[{c},
                Module[{walked},
                  walked = walk[c];
                  {
                    op
                    ,
                    Which[
                      precedenceGreater[prec, precCTL[walked]],
                        paren[walked]
                      ,
                      precedenceEqual[prec, precCTL[walked]],
                        paren[walked]
                      ,
                      !okToJuxtapose[prec, firstPrec[walked]] && !okToJuxtapose[lastPrec[walked], prec],
                        spaceBoth[walked]
                      ,
                      !okToJuxtapose[prec, firstPrec[walked]],
                        spaceBefore[walked]
                      ,
                      !okToJuxtapose[lastPrec[walked], prec],
                        spaceAfter[walked]
                      ,
                      True,
                        walked
                    ]
                  }
                ]
              ] /@ children[[2 ;; -2]]
          ) ~Join~ (Function[{c},
          Module[{walked},
            walked = walk[c];
            {
              op
              ,
              Which[
                precedenceGreater[prec, precCTL[walked]],
                  paren[walked]
                ,
                precedenceEqual[prec, precCTL[walked]],
                  paren[walked]
                ,
                !okToJuxtapose[prec, firstPrec[walked]],
                  spaceBefore[walked]
                ,
                True,
                  walked
              ]
            }
          ]
        ][Last[children]])
    ]
  ]
]


(*
postfix
*)

walk[n : CallNode[CallNode[LeafNode[Symbol, "Derivative", _], {LeafNode[Integer, "1", _]}, _], children : {_}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{Function[{c},
    Module[{walked},
      walked = walk[c];
      Which[
        precedenceGreater[prec, precCTL[walked]],
          paren[walked]
        ,
        !okToJuxtapose[lastPrec[walked], prec],
          spaceAfter[walked]
        ,
        True,
          walked
      ]
    ]
  ][children[[1]]], op}]]
]

walk[n : CallNode[LeafNode[Symbol, "Increment" | "Function" | "Factorial" | "Decrement" | "Factorial2" | "Repeated" | "RepeatedNull", _], children : {_}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{Function[{c},
    Module[{walked},
      walked = walk[c];
      Which[
        precedenceGreater[prec, precCTL[walked]],
          paren[walked]
        ,
        !okToJuxtapose[lastPrec[walked], prec],
          spaceAfter[walked]
        ,
        True,
          walked
      ]
    ]
  ][children[[1]]], op}]]
]



(*
prefix
*)

walk[n : CallNode[LeafNode[Symbol, "Plus", _], children : {_}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{op, Function[{c},
    Module[{walked},
      walked = walk[c];
      Which[
        (*
        allow "+(a*b)" to work
        *)
        precedenceGreater[prec, precCTL[walked]],
          paren[walked]
        ,
        !okToJuxtapose[prec, firstPrec[walked]],
          spaceBefore[walked]
        ,
        True,
          walked
      ]
    ]
  ][children[[1]]]}]]
]

walk[n : CallNode[LeafNode[Symbol, "PreIncrement" | "Not" | "PreDecrement", _], children : {_}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  ctor[Flatten[{op, Function[{c},
    Module[{walked},
      walked = walk[c];
      Which[
        precedenceGreater[prec, precCTL[walked]],
          paren[walked]
        ,
        !okToJuxtapose[prec, firstPrec[walked]],
          spaceBefore[walked]
        ,
        True,
          walked
      ]
    ]
  ][children[[1]]]}]]
]



(*
group
*)

walk[n:CallNode[LeafNode[Symbol, "List", _], {}, _]] :=
  curly[{}]

walk[n:CallNode[LeafNode[Symbol, "List", _], {child_}, _]] :=
  curly[{walk[child]}]

walk[n:CallNode[LeafNode[Symbol, "List", _], children_, _]] :=
  curly[{InfixNode[Comma, Riffle[walk /@ children, LeafNode[Token`Comma, ",", <||>]], <||>]}]

walk[n:CallNode[LeafNode[Symbol, "Association", _], {}, _]] :=
  assoc[{}]

walk[n:CallNode[LeafNode[Symbol, "Association", _], {child_}, _]] :=
  assoc[{walk[child]}]

walk[n:CallNode[LeafNode[Symbol, "Association", _], children_, _]] :=
  assoc[{InfixNode[Comma, Riffle[walk /@ children, LeafNode[Token`Comma, ",", <||>]], <||>]}]



walk[n : CallNode[LeafNode[Symbol, "Blank", _], {}, _]] :=
  LeafNode[Token`Under, "_", <||>]

walk[n : CallNode[LeafNode[Symbol, "BlankSequence", _], {}, _]] :=
  LeafNode[Token`UnderUnder, "__", <||>]

walk[n : CallNode[LeafNode[Symbol, "BlankNullSequence", _], {}, _]] :=
  LeafNode[Token`UnderUnderUnder, "___", <||>]

walk[n : CallNode[LeafNode[Symbol, "Blank", _], {s : LeafNode[Symbol, _, _]}, _]] :=
  CompoundNode[Blank, {LeafNode[Token`Under, "_", <||>], s}, <||>]

walk[n : CallNode[LeafNode[Symbol, "BlankSequence", _], {s : LeafNode[Symbol, _, _]}, _]] :=
  CompoundNode[BlankSequence, {LeafNode[Token`UnderUnder, "__", <||>], s}, <||>]

walk[n : CallNode[LeafNode[Symbol, "BlankNullSequence", _], {s : LeafNode[Symbol, _, _]}, _]] :=
  CompoundNode[BlankNullSequence, {LeafNode[Token`UnderUnderUnder, "___", <||>], s}, <||>]

walk[n : CallNode[LeafNode[Symbol, "Pattern", _], {a : LeafNode[Symbol, _, _], CallNode[LeafNode[Symbol, "Blank", _], {}, _]}, _]] :=
  CompoundNode[PatternBlank, {a, LeafNode[Token`Under, "_", <||>]}, <||>]

walk[n : CallNode[LeafNode[Symbol, "Pattern", _], {a : LeafNode[Symbol, _, _], CallNode[LeafNode[Symbol, "BlankSequence", _], {}, _]}, _]] :=
  CompoundNode[PatternBlankSequence, {a, LeafNode[Token`UnderUnder, "__", <||>]}, <||>]

walk[n : CallNode[LeafNode[Symbol, "Pattern", _], {a : LeafNode[Symbol, _, _], CallNode[LeafNode[Symbol, "BlankNullSequence", _], {}, _]}, _]] :=
  CompoundNode[PatternBlankNullSequence, {a, LeafNode[Token`UnderUnderUnder, "___", <||>]}, <||>]

walk[n : CallNode[LeafNode[Symbol, "Pattern", _], {a : LeafNode[Symbol, _, _], CallNode[LeafNode[Symbol, "Blank", _], {b : LeafNode[Symbol, _, _]}, _]}, _]] :=
  CompoundNode[PatternBlank, {a, CompoundNode[Blank, {LeafNode[Token`Under, "_", <||>], b}, <||>]}, <||>]

walk[n : CallNode[LeafNode[Symbol, "Pattern", _], {a : LeafNode[Symbol, _, _], CallNode[LeafNode[Symbol, "BlankSequence", _], {b : LeafNode[Symbol, _, _]}, _]}, _]] :=
  CompoundNode[PatternBlankSequence, {a, CompoundNode[BlankSequence, {LeafNode[Token`UnderUnder, "__", <||>], b}, <||>]}, <||>]

walk[n : CallNode[LeafNode[Symbol, "Pattern", _], {a : LeafNode[Symbol, _, _], CallNode[LeafNode[Symbol, "BlankNullSequence", _], {b : LeafNode[Symbol, _, _]}, _]}, _]] :=
  CompoundNode[PatternBlankNullSequence, {a, CompoundNode[BlankNullSequence, {LeafNode[Token`UnderUnderUnder, "___", <||>], b}, <||>]}, <||>]


walk[n : CallNode[LeafNode[Symbol, "Slot", _], {LeafNode[Integer, "1", _]}, _]] :=
  LeafNode[Token`Hash, "#", <||>]

walk[n : CallNode[LeafNode[Symbol, "Slot", _], {i:LeafNode[Integer, _, _]}, _]] :=
  CompoundNode[Slot, {LeafNode[Token`Hash, "#", <||>], i}, <||>]

walk[n : CallNode[LeafNode[Symbol, "SlotSequence", _], {LeafNode[Integer, "1", _]}, _]] :=
  LeafNode[Token`HashHash, "##", <||>]

walk[n : CallNode[LeafNode[Symbol, "SlotSequence", _], {i:LeafNode[Integer, _, _]}, _]] :=
  CompoundNode[SlotSequence, {LeafNode[Token`HashHash, "##", <||>], i}, <||>]


walk[n : CallNode[LeafNode[Symbol, "Part", _], {first_}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  CallNode[Flatten[{Function[{h},
    Module[{walked},
      walked = walk[h];
      Which[
        precedenceLess[precCTR[walked], prec],
          paren[walked]
        ,
        !okToJuxtapose[lastPrec[walked], prec],
          spaceAfter[walked]
        ,
        True,
          walked
      ]
    ]
  ][first]}], doubleSquare[{}], <||>]
]

walk[n : CallNode[LeafNode[Symbol, "Part", _], {first_, second_}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  CallNode[Flatten[{Function[{h},
    Module[{walked},
      walked = walk[h];
      Which[
        precedenceLess[precCTR[walked], prec],
          paren[walked]
        ,
        !okToJuxtapose[lastPrec[walked], prec],
          spaceAfter[walked]
        ,
        True,
          walked
      ]
    ]
  ][first]}], doubleSquare[{walk[second]}], <||>]
]

walk[n : CallNode[LeafNode[Symbol, "Part", _], {first_, rest___}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  CallNode[Flatten[{Function[{h},
    Module[{walked},
      walked = walk[h];
      Which[
        precedenceLess[precCTR[walked], prec],
          paren[walked]
        ,
        !okToJuxtapose[lastPrec[walked], prec],
          spaceAfter[walked]
        ,
        True,
          walked
      ]
    ]
  ][first]}], doubleSquare[{InfixNode[Comma, Riffle[walk /@ {rest}, LeafNode[Token`Comma, ",", <||>]], <||>]}], <||>]
]


walk[n : CallNode[head_, {}, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  CallNode[Flatten[{Function[{h},
    Module[{walked},
      walked = walk[h];
      Which[
        precedenceLess[precCTR[walked], prec],
          paren[walked]
        ,
        !okToJuxtapose[lastPrec[walked], prec],
          spaceAfter[walked]
        ,
        True,
          walked
      ]
    ]
  ][head]}], square[{}], <||>]
]

walk[n : CallNode[head_, children_, _]] :=
Module[{struct = nodeStructure[n], ctor, op, prec},
  ctor = struct["ctor"];
  op = struct["op"];
  prec = struct["prec"];
  CallNode[Flatten[{Function[{h},
    Module[{walked},
      walked = walk[h];
      Which[
        precedenceLess[precCTR[walked], prec],
          paren[walked]
        ,
        !okToJuxtapose[lastPrec[walked], prec],
          spaceAfter[walked]
        ,
        True,
          walked
      ]
    ]
  ][head]}], square[{InfixNode[Comma, Riffle[walk /@ children, LeafNode[Token`Comma, ",", <||>]], <||>]}], <||>]
]


(*
concretifying, so need to introduce newlines
*)
walk[ContainerNode[type_, children_, _]] :=
  ContainerNode[type,
    Riffle[walk /@ children, LeafNode[Token`Newline, $systemNewline, <||>]], <||>]


walk[PackageNode[{ctxt_}, children_, _]] :=
  Sequence @@ (
    {CallNode[{LeafNode[Symbol, "BeginPackage", <||>]}, square[{walk[ctxt]}], <||>]} ~Join~
    (walk /@ children) ~Join~
    {CallNode[{LeafNode[Symbol, "EndPackage", <||>]}, square[{}], <||>]})

walk[PackageNode[{ctxt_, needs_}, children_, _]] :=
  Sequence @@ (
    {CallNode[{LeafNode[Symbol, "BeginPackage", <||>]}, square[{InfixNode[Comma, {walk[ctxt], LeafNode[Token`Comma, ",", <||>], walk[needs]}, <||>]}], <||>]} ~Join~
    (walk /@ children) ~Join~
    {CallNode[{LeafNode[Symbol, "EndPackage", <||>]}, square[{}], <||>]})

walk[ContextNode[{ctxt_}, children_, _]] :=
  Sequence @@ (
    {CallNode[{LeafNode[Symbol, "Begin", <||>]}, square[{walk[ctxt]}], <||>]} ~Join~
    (walk /@ children) ~Join~
    {CallNode[{LeafNode[Symbol, "End", <||>]}, square[{}], <||>]})

walk[NewContextPathNode[{ctxt_}, children_, _]] :=
  Sequence @@ (
    {CallNode[{LeafNode[Symbol, "System`Private`NewContextPath", <||>]}, square[{walk[ctxt]}], <||>]} ~Join~
    (walk /@ children) ~Join~
    {CallNode[{LeafNode[Symbol, "System`Private`RestoreContextPath", <||>]}, square[{}], <||>]})

walk[n : LeafNode[tag_, str_, data_]] :=
Module[{struct, ctor, op, prec},
  If[StringStartsQ[str, "-"],
    (* treat as prefix - *)
    struct = structure[PrefixNode[Minus, #, <||>]&, LeafNode[Token`Minus, "-", <||>], Precedence`Prefix`Minus];
    ctor = struct["ctor"];
    op = struct["op"];
    prec = struct["prec"];
    ctor[Flatten[{op, Function[{c},
      walk[c]
    ][LeafNode[tag, StringDrop[str, 1], data]]}]]
    ,
    (* do nothing *)
    n
  ]
]

walk[n:PrefixNode[PrefixLinearSyntaxBang, _, _]] :=
  n

walk[n:ErrorNode[_, _, _]] :=
  n

walk[AbstractSyntaxErrorNode[AbstractSyntaxError`CommaTopLevel, children_, _]] :=
  InfixNode[Comma, Riffle[walk /@ children, LeafNode[Token`Comma, ",", <||>]], <||>]

walk[n:AbstractSyntaxErrorNode[_, _, _]] :=
  n

walk[f_?FailureQ] :=
  f

walk[args___] :=
  Failure["Unhandled", <| "Function" -> walk, "Arguments" -> HoldForm[{args}] |>]



paren[n_] :=
  GroupNode[GroupParen, {LeafNode[Token`OpenParen, "(", <||>]} ~Join~ {n} ~Join~ {LeafNode[Token`CloseParen, ")", <||>]}, <||>]

square[children_List] :=
  GroupNode[GroupSquare, {LeafNode[Token`OpenSquare, "[", <||>]} ~Join~ children ~Join~ {LeafNode[Token`CloseSquare, "]", <||>]}, <||>]

doubleSquare[children_List] :=
  GroupNode[GroupSquare, {LeafNode[Token`OpenSquare, "[", <||>]} ~Join~
  {GroupNode[GroupSquare, {LeafNode[Token`OpenSquare, "[", <||>]} ~Join~
  children ~Join~
  {LeafNode[Token`CloseSquare, "]", <||>]}, <||>]} ~Join~
  {LeafNode[Token`CloseSquare, "]", <||>]}, <||>]

curly[children_List] :=
  GroupNode[List, {LeafNode[Token`OpenCurly, "{", <||>]} ~Join~ children ~Join~ {LeafNode[Token`CloseCurly, "}", <||>]}, <||>]

assoc[children_List] :=
  GroupNode[Association, {LeafNode[Token`LessBar, "<|", <||>]} ~Join~ children ~Join~ {LeafNode[Token`BarGreater, "|>", <||>]}, <||>]



spaceBefore[n_] :=
  {LeafNode[Whitespace, " ", <||>], n}

spaceAfter[n_] :=
  {n, LeafNode[Whitespace, " ", <||>]}

spaceBoth[n_] :=
  {LeafNode[Whitespace, " ", <||>], n, LeafNode[Whitespace, " ", <||>]}



End[]

EndPackage[]
