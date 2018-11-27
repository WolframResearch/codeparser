BeginPackage["AST`"]


(* functions *)

ParseString
ParseFile


TokenizeString
TokenizeFile



ToInputFormString

ToNode


DeclarationName



PrefixOperatorToSymbol
PostfixOperatorToSymbol
InfixOperatorToSymbol
GroupOpenerToSymbol
GroupOpenerToCloser
GroupCloserToSymbol
GroupOpenerToMissingCloserSymbol
GroupCloserToMissingOpenerSymbol


SymbolToPrefixOperatorString
SymbolToInfixOperatorString
SymbolToPostfixOperatorString
SymbolToGroupPair
SymbolToTernaryOperatorPair



(* non-System symbols *)

(*Token*)
Token

(* atoms *)
InternalMinus
InternalEmpty
OptionalDefault
PatternBlank
PatternBlankSequence
PatternBlankNullSequence

(* operators *)
BinarySlashSlash
BinaryAt
BinaryInvisibleApplication
BinaryAtAtAt
InfixImplicitTimes
TernaryTilde
TernarySlashColon
LinearSyntaxBang
PostfixHermitianConjugate


(* groups *)
(*List*)
GroupMissingOpenerList
GroupMissingCloserList

(*Association*)
GroupMissingOpenerAssociation
GroupMissingCloserAssociation

(*AngleBracket*)
GroupMissingOpenerAngleBracket
GroupMissingCloserAngleBracket

(*Ceiling*)
GroupMissingOpenerCeiling
GroupMissingCloserCeiling

(*Floor*)
GroupMissingOpenerFloor
GroupMissingCloserFloor

GroupDoubleBracket
GroupMissingOpenerDoubleBracket
GroupMissingCloserDoubleBracket

GroupSquare
GroupMissingOpenerSquare
GroupMissingCloserSquare

(*BracketingBar*)
GroupMissingOpenerBracketingBar
GroupMissingCloserBracketingBar

(*DoubleBracketingBar*)
GroupMissingOpenerDoubleBracketingBar
GroupMissingCloserDoubleBracketingBar

GroupParen
GroupMissingOpenerParen
GroupMissingCloserParen

GroupLinearSyntaxParen
GroupMissingOpenerLinearSyntaxParen
GroupMissingCloserLinearSyntaxParen



(* options *)
DerivativeOrder
Source
(*
Used to report f[,] or "\[Alpa]" as an option, e.g. SyntaxIssues -> {SyntaxIssue[], SyntaxIssue[]}
*)
SyntaxIssues
SyntaxIssue




(* nodes *)
SymbolNode
StringNode
NumberNode
SyntaxErrorNode
BlankNode
BlankSequenceNode
BlankNullSequenceNode
PatternBlankNode
PatternBlankSequenceNode
PatternBlankNullSequenceNode
OptionalDefaultNode
SlotNode
SlotSequenceNode
OutNode
InternalEmptyNode
PrefixNode
BinaryNode
TernaryNode
InfixNode
PostfixNode
GroupNode
CallNode
PartNode
InternalMinusNode

(*
InternalTokenNode represents a token in a linear syntax expression
When parsing a linear syntax expressions, all tokens are simply kept unparsed
*)
InternalTokenNode

FileNode






Begin["`Private`"]

Needs["AST`DeclarationName`"]
Needs["AST`Node`"]
Needs["AST`Symbol`"]
Needs["AST`ToInputFormString`"]
Needs["AST`Utils`"]


packageDir = DirectoryName[FindFile["AST`"]]

exe = FileNameJoin[{packageDir, "ASTResources", $SystemID, "wl-ast"}]


ParseString[s_String] :=
	parseString[s]

TokenizeString[s_String] :=
	parseString[s, "Tokenize"->True]


Options[parseString] = {
	"Tokenize" -> False
}

parseString[sIn_String, OptionsPattern[]] :=
Catch[
Module[{s = sIn, res, out, actualAST, multiBytes, tokenize},

	tokenize = OptionValue["Tokenize"];

	If[StringContainsQ[sIn, "\n"],
		Throw[Failure["NewlinesNotYetSupported", <|"Input"->sIn|>]]
	];

	(*
	RunProcess cannot handle strings > 2^16.
	This may be a system limitation
	*)
	If[StringLength[sIn] > 2^16,
		Throw[Failure["InputTooLarge", <|"TruncatedInput"->StringTake[s, 100]|>]]
	];

	(*
	RunProcess cannot handle characters > 255 yet
	bug 360669
	*)
	multiBytes = Select[ToCharacterCode[s], # > 255&];
	If[Length[multiBytes] > 0,
		Throw[Failure["MultiByteCharactersNotAllowed", <|
			"Input"->s,
			"MultiByteCharacters"->AST`Utils`escapeString[FromCharacterCode[#]]& /@ Take[multiBytes, UpTo[10]]|>]]
	];

	res = RunProcess[{exe, Sequence@@If[tokenize, {"-format", "tokens"}, {}], "-noPrompt"}, All, s];

	res = handleResult[res];

	If[FailureQ[res],
		Throw[res]
	];

	res
]]





(*
ParseFile[full_String] returns a FileNode AST or a Failure object
*)
ParseFile[full_String] :=
	parseFile[full]

TokenizeFile[full_String] :=
	parseFile[full, "Tokenize"->True]



Options[parseFile] = {
	"Tokenize" -> False
}


parseFile[full_String, OptionsPattern[]] :=
Catch[
Module[{res, actualAST, tryString, actual, skipFirstLine = False, shebangWarn = False, opts, issues, tokenize},

	tokenize = OptionValue["Tokenize"];

	If[FileType[full] =!= File,
		Throw[Failure["NotAFile", <|"FileName"->full|>]]
	];

	If[FileByteCount[full] > 0,
		firstLine = Import[full, {"Lines", 1}];
		Which[
			firstLine === "(*!1N!*)mcm",
			Throw[Failure["EncodedFile", <|"FileName"->full|>]]
			,
			StringStartsQ[firstLine, "#!"],
			skipFirstLine = True
			,
			StringStartsQ[firstLine, "#"],
			shebangWarn = True;
		];
	];

	res = RunProcess[{exe, Sequence@@If[tokenize, {"-format", "tokens"}, {}], If[skipFirstLine, "-skipFirstLine", Nothing], "-file", full}, All];

	res = handleResult[res];

	If[FailureQ[res],
		res = Failure[res[[1]], Join[res[[2]], <|"FileName"->full|>]];
		Throw[res]
	];

	If[shebangWarn,
		opts = res[[3]];
		issues = Lookup[opts, SyntaxIssues, {}];
		AppendTo[issues, SyntaxIssue["# on first line looks like #! shebang", "Remark", <|Source->{{1,1}, {1, 1}}|>]];
		AssociateTo[opts, SyntaxIssues -> issues];
		res[[3]] = opts;
	];

	res
]]

handleResult[res_] :=
Catch[
Module[{ast},

	If[$Debug,
		Print[res]
	];

	If[!empty[res["StandardError"]],
		Print[res["StandardError"]]
	];

	(*
	ExitCode may be None if the process crashed
	see bug 360670
	So make sure to test with =!=
	*)
	If[res["ExitCode"] =!= 0,
		Throw[Failure["ExitCode", <|"ExitCode"->res["ExitCode"]|>]]
	];

	(*
	Put AST` on path even if it is not on path originally
	*)
	Block[{$ContextPath = {"AST`", "System`"}},
		ast = ToExpression[res["StandardOutput"], InputForm]
	];

	ast
]]






End[]

EndPackage[]
