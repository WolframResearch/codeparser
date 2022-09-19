
Needs["CodeParser`"]
Needs["CodeParser`Concretify`"]
Needs["CodeParser`Utils`"]


Test[
    ToSourceCharacterString[Concretify[CodeParse["a ^ b"]]]
    ,
    "a^b"
	,
	TestID->"Concretify-20220308-Z0H4G1"
]

Test[
    ToSourceCharacterString[Concretify[CodeParse["a + b"]]]
    ,
    "a+b"
	,
	TestID->"Concretify-20220308-D5X7L1"
]

Test[
    ToSourceCharacterString[Concretify[CodeParse["a + b ^ (c * d)"]]]
    ,
    "a+b^(c*d)"
	,
	TestID->"Concretify-20220308-I8G7E4"
]

Test[
    ToSourceCharacterString[Concretify[CodeParse["a ^ b ^ c"]]]
    ,
    "a^b^c"
	,
	TestID->"Concretify-20220308-J7K6G5"
]

Test[
    ToSourceCharacterString[Concretify[CodeParse["a ^ (b ^ c)"]]]
    ,
    "a^b^c"
	,
	TestID->"Concretify-20220308-G6S4J4"
]

Test[
    ToSourceCharacterString[Concretify[CodeParse["(a ^ b) ^ c"]]]
    ,
    "(a^b)^c"
	,
	TestID->"Concretify-20220308-I3Q9E0"
]

Test[
    ToSourceCharacterString[Concretify[CodeParse["+b"]]]
    ,
    "+b"
	,
	TestID->"Concretify-20220308-K4S6N5"
]

Test[
    ToSourceCharacterString[Concretify[CodeParse["a + +b"]]]
    ,
    "a+b"
	,
	TestID->"Concretify-20220308-E8L3Y8"
]

Test[
    ToSourceCharacterString[Concretify[CodeParse["a + (+b)"]]]
    ,
    "a+(+b)"
	,
	TestID->"Concretify-20220308-B8R2W4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["Plus[a, b//Plus]"]]]
    ,
    "a+(+b)"
	,
	TestID->"Concretify-20220308-G4Q5R0"
]

Test[
    ToSourceCharacterString[Concretify[CodeParse["a + (+b) + (+c)"]]]
    ,
    "a+(+b)+(+c)"
	,
	TestID->"Concretify-20220505-O4Y5T8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["Plus[a, b//Plus, c//Plus]"]]]
    ,
    "a+(+b)+(+c)"
	,
	TestID->"Concretify-20220505-T9Z0J2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a++ + b"]]]
    ,
    "a+++b"
	,
	TestID->"Concretify-20220308-W1G2D8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a + ++b"]]]
    ,
    "a+ ++b"
	,
	TestID->"Concretify-20220308-Z6F5P4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a& || b"]]]
    ,
    "a&||b"
	,
	TestID->"Concretify-20220308-S0T4T2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a || b& || c"]]]
    ,
    "a||b&||c"
	,
	TestID->"Concretify-20220308-G5Z5I3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a || b&"]]]
    ,
    "a||b&"
	,
	TestID->"Concretify-20220308-L0X1Z7"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a& && b"]]]
    ,
    "a& &&b"
	,
	TestID->"Concretify-20220308-K8P0T2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a // -1"]]]
    ,
    "(-1)[a]"
	,
	TestID->"Concretify-20220308-W3R9E5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["-a"]]]
    ,
    "-a"
	,
	TestID->"Concretify-20220308-F7D2K9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["-a b"]]]
    ,
    "-a*b"
	,
	TestID->"Concretify-20220308-O4M5K9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["b/c"]]]
    ,
    "b/c"
	,
	TestID->"Concretify-20220308-I6Q4P5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a b/c"]]]
    ,
    "a*b/c"
	,
	TestID->"Concretify-20220308-N3G8I5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a-b/c"]]]
    ,
    "a-b/c"
	,
	TestID->"Concretify-20220308-P7F8B3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a - b"]]]
    ,
    "a-b"
	,
	TestID->"Concretify-20220308-Y3Z2B0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a - - b"]]]
    ,
    "a-(-b)"
	,
	TestID->"Concretify-20220308-G1X7Y7"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a!"]]]
    ,
    "a!"
	,
	TestID->"Concretify-20220308-K4X5G2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a!!"]]]
    ,
    "a!!"
	,
	TestID->"Concretify-20220308-C8B8F2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a*b! = 1"]]]
    ,
    "a*b! =1"
	,
	TestID->"Concretify-20220308-L4C0P0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a @ b c"]]]
    ,
    "a[b]*c"
	,
	TestID->"Concretify-20220308-Y5A5P9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["- -a"]]]
    ,
    "-(-a)"
	,
	TestID->"Concretify-20220308-X6G2U8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["- -a!"]]]
    ,
    "-(-a!)"
	,
	TestID->"Concretify-20220308-I9V6P9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["- -a'"]]]
    ,
    "-(-a')"
	,
	TestID->"Concretify-20220308-N4O0M9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a! !!b"]]]
    ,
    "a! !!*b"
	,
	TestID->"Concretify-20220308-B2R7U2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["-1 2"]]]
    ,
    "-1*2"
	,
	TestID->"Concretify-20220308-H7Y6S4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a | -1 2"]]]
    ,
    "a|-1*2"
	,
	TestID->"Concretify-20220308-V2U4R1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["- - 2"]]]
    ,
    "-1*-2"
	,
	TestID->"Concretify-20220308-W4E2B9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["- \\[Minus] 2 a"]]]
    ,
    "(-1*-2)*a"
	,
	TestID->"Concretify-20220308-Y4C0O1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a /. 2"]]]
    ,
    "a/. 2"
	,
	TestID->"Concretify-20220308-R7A4U8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["-1  a  /  b"]]]
    ,
    "-(a/b)"
	,
	TestID->"Concretify-20220308-R5N0Y7"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["+ - -1"]]]
    ,
    "+(-1*-1)"
	,
	TestID->"Concretify-20220308-I7F8R0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a -> b &"]]]
    ,
    "a->b&"
	,
	TestID->"Concretify-20220308-L4A1V4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a -> (b &)"]]]
    ,
    "a->(b&)"
	,
	TestID->"Concretify-20220308-G9R7S0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a . 1 . 2"]]]
    ,
    "a. 1 . 2"
	,
	TestID->"Concretify-20220308-Z0M1V8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["- -2 b"]]]
    ,
    "(-1*-2)*b"
	,
	TestID->"Concretify-20220308-Z6Q3M8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["2 + -1 3"]]]
    ,
    "2 +-1*3"
	,
	TestID->"Concretify-20220308-H2Q4D2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["f'[x]"]]]
    ,
    "f'[x]"
	,
	TestID->"Concretify-20220308-K5A3S3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["#"]]]
    ,
    "#"
	,
	TestID->"Concretify-20220425-D4D9L2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["##"]]]
    ,
    "##"
	,
	TestID->"Concretify-20220425-K7W0R4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a /. (b /. c)"]]]
    ,
    "a/.(b/.c)"
	,
	TestID->"Concretify-20220425-K4Q4E7"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["c -> (f&[])"]]]
    ,
    "c->(f&[])"
	,
	TestID->"Concretify-20220425-D9X7C9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["c && (f&[])"]]]
    ,
    "c&&(f&[])"
	,
	TestID->"Concretify-20220425-D5C2Q2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a + b + (c + d)"]]]
    ,
    "a+b+(c+d)"
	,
	TestID->"Concretify-20220425-E7P7X4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a + b - (c + d)"]]]
    ,
    "a+b-(c+d)"
	,
	TestID->"Concretify-20220425-P6M3Q3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["Pattern[#, _Symbol]"]]]
    ,
    "Pattern[#,_Symbol]"
	,
	TestID->"Concretify-20220425-R0L2P8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["(a | b) | c"]]]
    ,
    "(a|b)|c"
	,
	TestID->"Concretify-20220425-P5N9T8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["x - Times[y, z]"]]]
    ,
    "x-(y*z)"
	,
	TestID->"Concretify-20220426-R0V8Z4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a @@ Composition[f[#]&, g]"]]]
    ,
    "a@@(f[#]&@*g)"
	,
	TestID->"Concretify-20220426-B0N7J8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a_ ..."]]]
    ,
    "a_ ..."
	,
	TestID->"Concretify-20220426-W0W2H4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["Plus[args]+1"]]]
    ,
    "(+args)+1"
	,
	TestID->"Concretify-20220426-H0B1G9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["(a = b) =!= c"]]]
    ,
    "(a=b)=!=c"
	,
	TestID->"Concretify-20220426-I2S2H7"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["Optional[a_?b, c]"]]]
    ,
    "Optional[a_?b,c]"
	,
	TestID->"Concretify-20220426-F8R4S0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a :> ApplyTo[b, c]"]]]
    ,
    "a:>(b//=c)"
	,
	TestID->"Concretify-20220426-P0Y3J6"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["1 - (a b) + 2"]]]
    ,
    "1 -(a*b)+2"
	,
	TestID->"Concretify-20220426-F7A9D3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["(a..)..."]]]
    ,
    "a.. ..."
	,
	TestID->"Concretify-20220426-Z8G3B5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a - 3"]]]
    ,
    "a-3"
	,
	TestID->"Concretify-20220501-L0M4X1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;Null"]]]
    ,
    "a;"
	,
	TestID->"Concretify-20220503-C4Q1Y2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;Null;Null"]]]
    ,
    "a; ;"
	,
	TestID->"Concretify-20220503-T9I0B2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a,"]]]
    ,
    "a,Null"
	,
	TestID->"Concretify-20220507-Y5P3E3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a+"]]]
    ,
    "a+"
	,
	TestID->"Concretify-20220507-Z6K5G0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["\!\(\*StyleBox[\"@\", \"foo\"]\)aa"]]]
    ,
    "\!\(\*StyleBox[\"@\", \"foo\"]\)*aa"
	,
	TestID->"Concretify-20220507-S4K6R1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["\
BeginPackage[\"Foo`\"]
EndPackage[]
"]]]
    ,
    "\
BeginPackage[\"Foo`\"]" <> $systemNewline <> "\
EndPackage[]"
	,
	TestID->"Concretify-20220614-F0A1F4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["\
Begin[\"Foo`\"]
End[]
"]]]
    ,
    "\
Begin[\"Foo`\"]" <> $systemNewline <> "\
End[]"
	,
	TestID->"Concretify-20220614-N4T1I5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a=."]]]
    ,
    "a=."
	,
	TestID->"Concretify-20220809-G0E2O5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a[[]]"]]]
	,
	"a[[]]"
	,
	TestID->"Concretify-20220917-T5M2N2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a[[1]]"]]]
	,
	"a[[1]]"
	,
	TestID->"Concretify-20220917-F3J2S5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a[[1,2]]"]]]
	,
	"a[[1,2]]"
	,
	TestID->"Concretify-20220917-A3T3O2"
]









Test[
	ToSourceCharacterString[Concretify[CodeParse[";; ;;"]]]
	,
	"(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-E7L2P8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;"]]]
	,
	"(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-H7A3J2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;"]]]
	,
	"a;;All"
	,
	TestID->"Concretify-20220917-W8X5N9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;b"]]]
	,
	"a;;b"
	,
	TestID->"Concretify-20220917-Y4C5W5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;b"]]]
	,
	"1;;b"
	,
	TestID->"Concretify-20220917-Y3Y7F1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;;;"]]]
	,
	"(a;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-O3K5T9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;a;;"]]]
	,
	"(1;;a)*(1;;All)"
	,
	TestID->"Concretify-20220917-K3L6G8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;a"]]]
	,
	"1;;All;;a"
	,
	TestID->"Concretify-20220917-P4H0G3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;a;;b"]]]
	,
	"1;;a;;b"
	,
	TestID->"Concretify-20220917-Y8A2N2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;;;b"]]]
	,
	"a;;All;;b"
	,
	TestID->"Concretify-20220917-W3Z3S2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;b;;c"]]]
	,
	"a;;b;;c"
	,
	TestID->"Concretify-20220917-T4K8O8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;"]]]
	,
	"(1;;All)*(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-N5W8F1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;b;;c;;"]]]
	,
	"(1;;b;;c)*(1;;All)"
	,
	TestID->"Concretify-20220917-Z2M2U1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;b;;c;;d"]]]
	,
	"(a;;b;;c)*(1;;d)"
	,
	TestID->"Concretify-20220917-U6Z3Z1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;;;;;"]]]
	,
	"(a;;All)*(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-G3A5U6"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;a;;;;"]]]
	,
	"(1;;a)*(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-M8I2B3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;a;;"]]]
	,
	"(1;;All;;a)*(1;;All)"
	,
	TestID->"Concretify-20220917-M3V3H7"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;a"]]]
	,
	"(1;;All)*(1;;All;;a)"
	,
	TestID->"Concretify-20220917-G5I0S8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;;;"]]]
	,
	"(1;;All)*(1;;All)*(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-G2Q3F4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;;;;;;;"]]]
	,
	"(a;;All)*(1;;All)*(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-Z6Z4T1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;a;;;;;;"]]]
	,
	"(1;;a)*(1;;All)*(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-D8Q1A8"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;a;;;;"]]]
	,
	"(1;;All;;a)*(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-U0M1P2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;a;;"]]]
	,
	"(1;;All)*(1;;All;;a)*(1;;All)"
	,
	TestID->"Concretify-20220917-J7E9D4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;;;a"]]]
	,
	"(1;;All)*(1;;All)*(1;;All;;a)"
	,
	TestID->"Concretify-20220917-I3D7O9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;;;;;b"]]]
	,
	"(a;;All)*(1;;All;;b)"
	,
	TestID->"Concretify-20220917-H2T9N9"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;a;;;;b"]]]
	,
	"(1;;a)*(1;;All;;b)"
	,
	TestID->"Concretify-20220917-T8S5U6"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;a;;b"]]]
	,
	"(1;;All;;a)*(1;;b)"
	,
	TestID->"Concretify-20220917-L7T9R0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;a"]]]
	,
	"(1;;All)*(1;;All;;a)"
	,
	TestID->"Concretify-20220917-K1D0M7"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;;;b"]]]
	,
	"(1;;All)*(1;;All)*(1;;All;;b)"
	,
	TestID->"Concretify-20220917-T1T8Y4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;;;;;;;b"]]]
	,
	"(a;;All)*(1;;All)*(1;;All;;b)"
	,
	TestID->"Concretify-20220917-L5F6R3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;a;;;;;;b"]]]
	,
	"(1;;a)*(1;;All)*(1;;All;;b)"
	,
	TestID->"Concretify-20220917-W4Q6S4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;a;;;;b"]]]
	,
	"(1;;All;;a)*(1;;All;;b)"
	,
	TestID->"Concretify-20220917-M5R6S7"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;a;;b"]]]
	,
	"(1;;All)*(1;;All;;a)*(1;;b)"
	,
	TestID->"Concretify-20220917-G6Z1J0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;;;a"]]]
	,
	"(1;;All)*(1;;All)*(1;;All;;a)"
	,
	TestID->"Concretify-20220917-A2T5N5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;;;b"]]]
	,
	"(1;;All)*(1;;All)*(1;;All;;b)"
	,
	TestID->"Concretify-20220917-J9D8J2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;c;;;;;;b"]]]
	,
	"(a;;c)*(1;;All)*(1;;All;;b)"
	,
	TestID->"Concretify-20220917-J5H0O7"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;a;;c;;;;b"]]]
	,
	"(1;;a;;c)*(1;;All;;b)"
	,
	TestID->"Concretify-20220917-W5A0B6"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;a;;c;;b"]]]
	,
	"(1;;All;;a)*(1;;c;;b)"
	,
	TestID->"Concretify-20220917-G7I0M1"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["c;;;;;;a;;b"]]]
	,
	"(c;;All)*(1;;All;;a)*(1;;b)"
	,
	TestID->"Concretify-20220917-L1C0M2"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["{ ;;\n;; }"]]]
	,
	"{(1;;All)*(1;;All)}"
	,
	TestID->"Concretify-20220917-W3H2W6"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["{ ;;\n;;a }"]]]
	,
	"{1;;All;;a}"
	,
	TestID->"Concretify-20220917-A8P1E5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;;;;;;;"]]]
	,
	"(1;;All)*(1;;All)*(1;;All)*(1;;All)"
	,
	TestID->"Concretify-20220917-G6R3O0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;b;;c;;d;;e"]]]
	,
	"(a;;b;;c)*(1;;d;;e)"
	,
	TestID->"Concretify-20220917-S5H7T6"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;b;;c;;d;;e;;f"]]]
	,
	"(a;;b;;c)*(1;;d;;e)*(1;;f)"
	,
	TestID->"Concretify-20220917-A7D1V0"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;b;;c;;d;;e;;f;;g"]]]
	,
	"(a;;b;;c)*(1;;d;;e)*(1;;f;;g)"
	,
	TestID->"Concretify-20220917-U0Q5Q3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;b;;c;;d;;e;;f;;g;;h"]]]
	,
	"(a;;b;;c)*(1;;d;;e)*(1;;f;;g)*(1;;h)"
	,
	TestID->"Concretify-20220917-N5C8T5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;;;;"]]]
	,
	"(a;;All)*(1;;All);"
	,
	TestID->"Concretify-20220917-Q4R1J4"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["b;;a;;;"]]]
	,
	"(b;;a)*(1;;All);"
	,
	TestID->"Concretify-20220917-I7G4K5"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse[";;a;;;"]]]
	,
	"(1;;a)*(1;;All);"
	,
	TestID->"Concretify-20220917-F0L3H3"
]

Test[
	ToSourceCharacterString[Concretify[CodeParse["a;;!"]]]
	,
	"a;;(!)"
	,
	TestID->"Concretify-20220917-F6A2V8"
]





