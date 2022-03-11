
Needs["CodeParser`"]

Test[
	SafeString[ToCharacterCode["1+1"]]
	,
	"1+1"
	,
	TestID->"SafeString-20200103-U8A6X2"
]

(*
Invalid sequences
*)
Test[
	SafeString[{206}]
	,
	Missing["UnsafeCharacterEncoding_IncompleteUTF8Sequence"]
	,
	TestID->"SafeString-20200103-K0M0B9"
]

(*
High surrogates
*)
Test[
	(*
	UTF-8 for 0xd800
	*)
	SafeString[{237, 160, 128}]
	,
	Missing["UnsafeCharacterEncoding_StraySurrogate"]
	,
	TestID->"SafeString-20200103-Z8W9G3"
]

(*
Low surrogates
*)
Test[
	(*
	UTF-8 for 0xdc00
	*)
	SafeString[{237, 176, 128}]
	,
	Missing["UnsafeCharacterEncoding_StraySurrogate"]
	,
	TestID->"SafeString-20200103-G7F2O6"
]



(*
BOM
*)
Test[
	(*
	UTF-8 for 0xfeff
	*)
	SafeString[{239, 187, 191}]
	,
	Missing["UnsafeCharacterEncoding_BOM"]
	,
	TestID->"SafeString-20200103-V9G4Y6"
]











