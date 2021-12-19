
Needs["CodeParser`"]

Test[
	CodeParse[{65, 16^^ed, 16^^a0, 16^^80, 65}]
	,
	ContainerNode[Byte, {Missing[
   "UnsafeCharacterEncoding"]}, <|SyntaxIssues -> {EncodingIssue[
     "StraySurrogate", "Stray surrogate.", 
     "Fatal", <|Source -> {{1, 2}, {1, 3}}, 
      ConfidenceLevel -> 1.|>]}|>]
	,
	TestID->"Unsafe-20211223-G8M5U2"
]


Test[
	CodeParse[{16^^E1, 16^^A0, 16^^C0}]
	,
	ContainerNode[Byte, {Missing[
   "UnsafeCharacterEncoding"]}, <|SyntaxIssues -> {EncodingIssue[
     "IncompleteSequence", "Incomplete sequence.", 
     "Fatal", <|Source -> {{1, 1}, {1, 2}}, ConfidenceLevel -> 1.|>], 
    EncodingIssue["IncompleteSequence", "Incomplete sequence.", 
     "Fatal", <|Source -> {{1, 2}, {1, 3}}, 
      ConfidenceLevel -> 1.|>]}|>]
	,
	TestID->"Unsafe-20211223-B6H1C5"
]


Test[
	CodeParse[{16^^C0 , 16^^80}]
	,
	ContainerNode[Byte, {Missing[
   "UnsafeCharacterEncoding"]}, <|SyntaxIssues -> {EncodingIssue[
     "IncompleteSequence", "Incomplete sequence.", 
     "Fatal", <|Source -> {{1, 1}, {1, 2}}, ConfidenceLevel -> 1.|>], 
    EncodingIssue["IncompleteSequence", "Incomplete sequence.", 
     "Fatal", <|Source -> {{1, 2}, {1, 3}}, 
      ConfidenceLevel -> 1.|>]}|>]
	,
	TestID->"Unsafe-20211224-A8O4H2"
]


