(*

The token enum

*)
<|
Token`Unknown -> 0,
Token`EOF -> Next,
Token`Comment -> Next,
Token`Symbol -> Next,
Token`String -> Next,
Token`Integer -> Next,
Token`Real -> Next,
Token`Newline -> Next,
Token`WhiteSpace -> Next,

(* errors *)
Token`Error`First -> Next,
Token`Error`Unknown -> Token`Error`First,

(*
a ~b
*)
Token`Error`ExpectedTilde -> Next,

(*
a /: b
*)
Token`Error`ExpectedSet -> Next,

(*
a ^:
*)
Token`Error`ExpectedEqual -> Next,

(*
Any weird unhandled escaped characters
*)
Token`Error`UnhandledCharacter -> Next,
Token`Error`ExpectedDigitOrAlpha -> Next,
Token`Error`ExpectedAlphaOrDollar -> Next,

(*
1:2
*)
Token`Error`ExpectedSymbol -> Next,
Token`Error`UnterminatedComment -> Next,
Token`Error`UnterminatedString -> Next,
Token`Error`InvalidBase -> Next,
Token`Error`ExpectedAccuracy -> Next,
Token`Error`ExpectedExponent -> Next,

(*
a::
*)
Token`Error`EmptyString -> Next,
Token`Error`Rest -> Next,
Token`Error`Internal -> Next,
Token`Error`End -> Next,

Token`Operator`First -> Next,
Token`Operator`Unknown -> Token`Operator`First,
(* 1 character operators *)
Token`Operator`Dot -> Next,
Token`Operator`Colon -> Next,
Token`Operator`OpenParen -> Next,
Token`Operator`CloseParen -> Next,
Token`Operator`OpenSquare -> Next,
Token`Operator`CloseSquare -> Next,
Token`Operator`Comma -> Next,
Token`Operator`OpenCurly -> Next,
Token`Operator`CloseCurly -> Next,
Token`Operator`Equal -> Next,
Token`Operator`Bang -> Next,
Token`Operator`Under -> Next,
Token`Operator`Less -> Next,
Token`Operator`Greater -> Next,
Token`Operator`Minus -> Next,
Token`Operator`Bar -> Next,
Token`Operator`Semi -> Next,
Token`Operator`Hash -> Next,
Token`Operator`Amp -> Next,
Token`Operator`Slash -> Next,
Token`Operator`At -> Next,
Token`Operator`Plus -> Next,
Token`Operator`Tilde -> Next,
Token`Operator`Star -> Next,
Token`Operator`Caret -> Next,
Token`Operator`SingleQuote -> Next,
Token`Operator`Percent -> Next,
Token`Operator`Question -> Next,

(* 2 character operators *)
Token`Operator`DotDot -> Next,
Token`Operator`ColonColon -> Next,
Token`Operator`ColonEqual -> Next,
Token`Operator`ColonGreater -> Next,
Token`Operator`EqualEqual -> Next,
Token`Operator`UnderUnder -> Next,
Token`Operator`UnderDot -> Next,
Token`Operator`LessBar -> Next,
Token`Operator`LessLess -> Next,
Token`Operator`LessGreater -> Next,
Token`Operator`LessEqual -> Next,
Token`Operator`GreaterGreater -> Next,
Token`Operator`GreaterEqual -> Next,
Token`Operator`MinusGreater -> Next,
Token`Operator`MinusMinus -> Next,
Token`Operator`MinusEqual -> Next,
Token`Operator`BarBar -> Next,
Token`Operator`BarGreater -> Next,
Token`Operator`SemiSemi -> Next,
Token`Operator`AmpAmp -> Next,
Token`Operator`SlashAt -> Next,
Token`Operator`SlashSemi -> Next,
Token`Operator`SlashDot -> Next,
Token`Operator`SlashSlash -> Next,
Token`Operator`SlashColon -> Next,
Token`Operator`SlashEqual -> Next,
Token`Operator`SlashStar -> Next,
Token`Operator`AtAt -> Next,
Token`Operator`AtStar -> Next,
Token`Operator`PlusPlus -> Next,
Token`Operator`PlusEqual -> Next,
Token`Operator`TildeTilde -> Next,
Token`Operator`StarEqual -> Next,
Token`Operator`StarStar -> Next,
Token`Operator`CaretEqual -> Next,
Token`Operator`HashHash -> Next,
Token`Operator`BangEqual -> Next,
Token`Operator`BangBang -> Next,

(* 3 character operators *)
Token`Operator`DotDotDot -> Next,
Token`Operator`EqualEqualEqual -> Next,
Token`Operator`EqualBangEqual -> Next,
Token`Operator`UnderUnderUnder -> Next,
Token`Operator`SlashSlashDot -> Next,
Token`Operator`AtAtAt -> Next,
Token`Operator`LessMinusGreater -> Next,
Token`Operator`SlashSlashAt -> Next,
Token`Operator`CaretColonEqual -> Next,
Token`Operator`GreaterGreaterGreater -> Next,

(* Linear syntax operators *)
Token`Operator`LinearSyntax`Bang -> Next,
Token`Operator`LinearSyntax`OpenParen -> Next,
Token`Operator`LinearSyntax`Star -> Next,
Token`Operator`LinearSyntax`CloseParen -> Next,
Token`Operator`LinearSyntax`At -> Next,
Token`Operator`LinearSyntax`Caret -> Next,
Token`Operator`LinearSyntax`Under -> Next,
Token`Operator`LinearSyntax`Percent -> Next,
Token`Operator`LinearSyntax`Plus -> Next,
Token`Operator`LinearSyntax`Backtick -> Next,
Token`Operator`LinearSyntax`Slash -> Next,
Token`Operator`LinearSyntax`Amp -> Next,
Token`Operator`LinearSyntax`Space -> Next,

(* Fake operators *)
(* implicit times operator *)
Token`Operator`Fake`ImplicitTimes -> Next,
(* A colon operator that is sym:pat, COLON operator is not used, it is ambiguous *)
Token`Operator`Fake`PatternColon -> Next,
(* A colon operator that is pat:val, COLON operator is not used, it is ambiguous *)
Token`Operator`Fake`OptionalColon -> Next,
(* Not used, but needed for sanity checks *)
Token`Operator`Fake`EqualDot -> Next,

Token`Operator`End -> Next,

Nothing
|>