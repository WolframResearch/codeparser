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

Token`First -> Next,
(* 1 character tokens *)
Token`Dot -> Next, (* . *)
Token`Colon -> Next, (* : *)
Token`OpenParen -> Next, (* ( *)
Token`CloseParen -> Next, (* ) *)
Token`OpenSquare -> Next, (* [ *)
Token`CloseSquare -> Next, (* ] *)
Token`Comma -> Next, (* , *)
Token`OpenCurly -> Next, (* { *)
Token`CloseCurly -> Next, (* } *)
Token`Equal -> Next, (* = *)
Token`Bang -> Next, (* ! *)
Token`Under -> Next, (* _ *)
Token`Less -> Next, (* < *)
Token`Greater -> Next, (* > *)
Token`Minus -> Next, (* - *)
Token`Bar -> Next, (* | *)
Token`Semi -> Next, (* ; *)
Token`Hash -> Next, (* # *)
Token`Amp -> Next, (* & *)
Token`Slash -> Next, (* / *)
Token`At -> Next, (* @ *)
Token`Plus -> Next, (* + *)
Token`Tilde -> Next, (* ~ *)
Token`Star -> Next, (* * *)
Token`Caret -> Next, (* ^ *)
Token`SingleQuote -> Next, (* ' *)
Token`Percent -> Next, (* % *)
Token`Question -> Next, (* ? *)

(* 2 character tokens *)
Token`DotDot -> Next, (* .. *)
Token`ColonColon -> Next, (* :: *)
Token`ColonEqual -> Next, (* := *)
Token`ColonGreater -> Next, (* :> *)
Token`EqualEqual -> Next, (* == *)
Token`UnderUnder -> Next, (* __ *)
Token`UnderDot -> Next, (* _. *)
Token`LessBar -> Next, (* <| *)
Token`LessLess -> Next, (* << *)
Token`LessGreater -> Next, (* <> *)
Token`LessEqual -> Next, (* <= *)
Token`GreaterGreater -> Next, (* >> *)
Token`GreaterEqual -> Next, (* >= *)
Token`MinusGreater -> Next, (* -> *)
Token`MinusMinus -> Next, (* -- *)
Token`MinusEqual -> Next, (* -= *)
Token`BarBar -> Next, (* || *)
Token`BarGreater -> Next, (* |> *)
Token`SemiSemi -> Next, (* ;; *)
Token`AmpAmp -> Next, (* && *)
Token`SlashAt -> Next, (* /@ *)
Token`SlashSemi -> Next, (* /; *)
Token`SlashDot -> Next, (* /. *)
Token`SlashSlash -> Next, (* // *)
Token`SlashColon -> Next, (* /: *)
Token`SlashEqual -> Next, (* /= *)
Token`SlashStar -> Next, (* /* *)
Token`AtAt -> Next, (* @@ *)
Token`AtStar -> Next, (* @* *)
Token`PlusPlus -> Next, (* ++ *)
Token`PlusEqual -> Next, (* += *)
Token`TildeTilde -> Next, (* ~~ *)
Token`StarEqual -> Next, (* *= *)
Token`StarStar -> Next, (* ** *)
Token`CaretEqual -> Next, (* ^= *)
Token`HashHash -> Next, (* ## *)
Token`BangEqual -> Next, (* != *)
Token`BangBang -> Next, (* !! *)

(* 3 character tokens *)
Token`DotDotDot -> Next, (* ... *)
Token`EqualEqualEqual -> Next, (* === *)
Token`EqualBangEqual -> Next, (* =!= *)
Token`UnderUnderUnder -> Next, (* ___ *)
Token`SlashSlashDot -> Next, (* //. *)
Token`AtAtAt -> Next, (* @@@ *)
Token`LessMinusGreater -> Next, (* <-> *)
Token`SlashSlashAt -> Next, (* //@ *)
Token`CaretColonEqual -> Next, (* ^:= *)
Token`GreaterGreaterGreater -> Next, (* >>> *)

(* Linear syntax tokens *)
Token`LinearSyntax`Bang -> Next, (* \! *)
Token`LinearSyntax`OpenParen -> Next, (* \( *)
Token`LinearSyntax`Star -> Next, (* \* *)
Token`LinearSyntax`CloseParen -> Next, (* \) *)
Token`LinearSyntax`At -> Next, (* \@ *)
Token`LinearSyntax`Caret -> Next, (* \^ *)
Token`LinearSyntax`Under -> Next, (* \_ *)
Token`LinearSyntax`Percent -> Next, (* \% *)
Token`LinearSyntax`Plus -> Next, (* \+ *)
Token`LinearSyntax`Backtick -> Next, (* \` *)
Token`LinearSyntax`Slash -> Next, (* \/ *)
Token`LinearSyntax`Amp -> Next, (* \& *)
Token`LinearSyntax`Space -> Next, (* \<space> *)

(* Fake tokens *)
(* implicit times operator *)
Token`Fake`ImplicitTimes -> Next,
(* A colon operator that is sym:pat, COLON operator is not used, it is ambiguous *)
Token`Fake`PatternColon -> Next,
(* A colon operator that is pat:val, COLON operator is not used, it is ambiguous *)
Token`Fake`OptionalColon -> Next,
(* Not used, but needed for sanity checks *)
Token`Fake`EqualDot -> Next,

Token`End -> Next,

Nothing
|>