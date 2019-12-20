(*

The token enum

This is a complete enumeration of all tokens in Wolfram Language

*)
<|
Token`Unknown -> 0,
Token`EndOfFile -> Next,
Token`Symbol -> Next,
Token`String -> Next,
Token`Integer -> Next,
Token`Real -> Next,

(* trivia *)
Token`Comment -> Next,
Token`Newline -> Next,
Token`Whitespace -> Next,
Token`LineContinuation -> Next,

(* errors *)
Token`Error`ExpectedEqual -> Next,
(*
Order of First appearing here is important.
During generation, values that are not Next are removed
*)
Token`Error`First -> Token`Error`ExpectedEqual,
Token`Error`UnhandledDot -> Next,
Token`Error`UnhandledCharacter -> Next,
Token`Error`ExpectedLetterlike -> Next,
Token`Error`UnterminatedComment -> Next,
Token`Error`UnterminatedString -> Next,
Token`Error`ExpectedAccuracy -> Next,
Token`Error`ExpectedExponent -> Next,
Token`Error`EmptyString -> Next,
Token`Error`Aborted -> Next,
Token`Error`ExpectedOperand -> Next,
Token`Error`UnrecognizedDigit -> Next,
Token`Error`ExpectedDigit -> Next,
Token`Error`UninterpretableCharacter -> Next,
Token`Error`UnsupportedCharacter -> Next,
Token`Error`InvalidBase -> Next,
Token`Error`End -> Next,

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
Token`QuestionQuestion -> Next, (* ?? *)
Token`EqualDot -> Next, (* =. *)

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

(* implicit Times operator in  a b  *)
Token`Fake`ImplicitTimes -> Next,

(* implicit  Null  in  a;  and  f[1,]  *)
Token`Fake`ImplicitNull -> Next,
(* implicit  1  in  ;;b  *)
Token`Fake`ImplicitOne -> Next,
(* implicit  All  in  a;;  *)
Token`Fake`ImplicitAll -> Next,


(*
Used when parsing boxes

The front end treats  ( *  and  * )  as tokens
(broken up here so as to not mess up the comment)
*)
Token`Boxes`OpenParenStar -> Next,
Token`Boxes`StarCloseParen -> Next,
(*
The front end treats ''' as a single token
*)
Token`Boxes`MultiSingleQuote -> Next,

(*
Parsing f.m as a leaf from the front end (from example input such as <<f.m)
*)
(*Token`Other -> Next,*)

Nothing
|>
