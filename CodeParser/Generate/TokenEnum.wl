(* ::Package::"Tags"-><|"SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["CodeParser`Generate`TokenEnum`"]

(* Used by Generate/RowBox.wl *)
GroupOpenerToCloser


Begin["`Private`"]


GroupOpenerToCloser[Token`OpenCurly] = Closer`CloseCurly
GroupOpenerToCloser[Token`LessBar] = Closer`BarGreater
GroupOpenerToCloser[Token`OpenSquare] = Closer`CloseSquare
GroupOpenerToCloser[Token`OpenParen] = Closer`CloseParen
GroupOpenerToCloser[Token`ColonColonOpenSquare] = Closer`CloseSquare

GroupOpenerToCloser[Token`LongName`LeftAngleBracket] = Closer`LongName`RightAngleBracket
GroupOpenerToCloser[Token`LongName`LeftCeiling] = Closer`LongName`RightCeiling
GroupOpenerToCloser[Token`LongName`LeftFloor] = Closer`LongName`RightFloor
GroupOpenerToCloser[Token`LongName`LeftDoubleBracket] = Closer`LongName`RightDoubleBracket
GroupOpenerToCloser[Token`LongName`LeftBracketingBar] = Closer`LongName`RightBracketingBar
GroupOpenerToCloser[Token`LongName`LeftDoubleBracketingBar] = Closer`LongName`RightDoubleBracketingBar
GroupOpenerToCloser[Token`LongName`LeftAssociation] = Closer`LongName`RightAssociation
GroupOpenerToCloser[Token`LongName`OpenCurlyQuote] = Closer`LongName`CloseCurlyQuote
GroupOpenerToCloser[Token`LongName`OpenCurlyDoubleQuote] = Closer`LongName`CloseCurlyDoubleQuote


End[]

EndPackage[]
