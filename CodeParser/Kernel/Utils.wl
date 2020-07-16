BeginPackage["CodeParser`Utils`"]

escapeString

(*
char
*)

empty

SourceMemberQ

SourceMemberIntersectingQ

SourceMemberQFunction

contiguousQ


removeIgnoredNodes


normalizeTokens

removeSimpleLineContinuation

removeRemainingSimpleLineContinuation

removeComplexLineContinuations

convertEmbeddedNewlines

convertEmbeddedTabs

(*
Functions copied from CodeFormatter

FIXME: when a dependency on CodeFormatter is created, then use those functions and remove this section

*)
replaceTabs

tabReplacementFunc



Begin["`Private`"]

Needs["CodeParser`"]



$DefaultNewline = "\n"

$DefaultTabWidth = 4



escapeString[s_] :=
	ToString[s, InputForm, CharacterEncoding -> "ASCII"]


(*
char["A"] evaluate to 65
Only string literals of a single character may be given as arguments
*)
(*
Attributes[char] = {HoldAll}
char[s_String] /; StringLength[s] == 1 := ToCharacterCode[s][[1]]

char[args___] := (Message[char::unhandled, Hold[args]];$Failed)
*)


empty[l_List] := Length[l] == 0

empty[a_Association] := Length[a] == 0

empty[s_String] := s == ""





(*
Construct a SourceMemberQFunction to be used later
*)

SourceMemberQ[srcs_] := SourceMemberQFunction[srcs]

(*
Define what SourceMemberQFunction should do
*)
SourceMemberQFunction[srcs_][cursor_] := SourceMemberQ[srcs, cursor]






spanPat = {_Integer, _Integer}

srcPat = {spanPat, spanPat}


(*
test that a cursor Source is a Member of any of a List of Sources

This tests for membership of ANY src

This does NOT test for membership of ALL srcs

*)
SourceMemberQ[srcs:{srcPat...}, cursor:srcPat] :=
	AnyTrue[srcs, SourceMemberQ[#, cursor]&]

SourceMemberQ[srcs:{srcPat...}, cursor:spanPat] :=
	AnyTrue[srcs, SourceMemberQ[#, cursor]&]


(*
test that a cursor Source is a Member of a src Source
*)
SourceMemberQ[src:srcPat, {cursor1:spanPat, cursor2:spanPat}] :=
	SourceMemberQ[src, cursor1] && SourceMemberQ[src, cursor2]

SourceMemberQ[src:srcPat, cursor:spanPat] :=
	SourceMemberQ[src, cursor]

(*
Do the actual work

SourceMemberQ[{{1,3},{2,0}}, {1,4}] => True
SourceMemberQ[{{1,3},{2,0}}, {2,4}] => False
*)
SourceMemberQ[{{srcLine1_Integer, srcCol1_Integer}, {srcLine2_Integer, srcCol2_Integer}}, {cursorLine_Integer, cursorCol_Integer}] :=
Which[
	(* not in-between the lines of the spec, so no *)
	!(srcLine1 <= cursorLine <= srcLine2),
		False
	,
	(* everything is on 1 line, so now test cols *)
	cursorLine == srcLine1 == srcLine2,
		srcCol1 <= cursorCol <= srcCol2
	,
	(* on srcLine1, so test that cursor comes after srcCol1 *)
	cursorLine == srcLine1,
		srcCol1 <= cursorCol
	,
	(* on srcLine2, so test that cursor comes before srcCol2 *)
	cursorLine == srcLine2,
		cursorCol <= srcCol2
	,
	(* exclusively in-between start and end, so yes *)
	True,
		True
]




(*
Also handle Position Sources
*)
SourceMemberQ[{srcInts___Integer}, cursorPos:{_Integer...}] := MatchQ[cursorPos, {srcInts, ___}]




(*

contiguousQ

input: src1:{{line,col}, {line,col}}   src2:{{line,col}, {line,col}}

*)

contiguousQ[srcs_List] := And @@ contiguousQ @@@ Partition[srcs, 2, 1]

(*
LineCol-style
*)
contiguousQ[{{_, _}, {line_, col1_}}, {{line_, col2_}, {_, _}}] := col1 == col2

(*
Position-style
*)
contiguousQ[{_Integer..., idx1_Integer}, {_Integer..., idx2_Integer}] := idx1 + 1 == idx2

contiguousQ[_, _] := False




expandSrc[{{line_, col1_}, {line_, col2_}}] :=
  <|line -> {col1, col2}|>

expandSrc[{{line1_, col1_}, {line2_, col2_}}] :=
  <|
    line1 -> {col1, Infinity}, 
    Table[l -> {1, Infinity}, {l, line1 + 1, line2 - 1}], 
    line2 -> {1, col2}
  |>

intervalTest[{_}] :=
  False
intervalTest[{int1_, int2_}] := 
   IntervalIntersection[Interval[int1], Interval[int2]] =!= Interval[]

SourceMemberIntersectingQ[
  src1:{{_Integer, _Integer}, {_Integer, _Integer}},
  src2:{{_Integer, _Integer}, {_Integer, _Integer}}] :=
  Catch[
  Module[{expanded1, expanded2},
    expanded1 = expandSrc[src1];
    expanded2 = expandSrc[src2];
    Or @@ Merge[{expanded1, expanded2}, intervalTest]
  ]]




(*
Used by CodeInspector for removing nodes ignored because of BeginStaticAnalysisIgnore[]
*)

removeIgnoredNodes[Null, _SourceMemberQFunction] := Null

removeIgnoredNodes[l_LeafNode, _SourceMemberQFunction] := l

(* optimization *)
removeIgnoredNodes[node_, SourceMemberQFunction[{}]] :=
  node

removeIgnoredNodes[node_[tag_, childrenIn_, dataIn_], ignoredNodesSrcMemberFunc_SourceMemberQFunction] :=
Module[{children, data, syntaxIssues, abstractSyntaxIssues},

  children = childrenIn;
  data = dataIn;

  children = DeleteCases[children, n_ /; ignoredNodesSrcMemberFunc[n[[3, Key[Source]]]]];
  children = removeIgnoredNodes[#, ignoredNodesSrcMemberFunc]& /@ children;
  
  syntaxIssues = Lookup[data, SyntaxIssues, {}];
  If[syntaxIssues != {},
    syntaxIssues = DeleteCases[syntaxIssues, n_ /; ignoredNodesSrcMemberFunc[n[[4, Key[Source]]]]];
    data[SyntaxIssues] = syntaxIssues;
  ];

  abstractSyntaxIssues = Lookup[data, AbstractSyntaxIssues, {}];
  If[abstractSyntaxIssues != {},
    abstractSyntaxIssues = DeleteCases[abstractSyntaxIssues, n_ /; ignoredNodesSrcMemberFunc[n[[4, Key[Source]]]]];
    data[AbstractSyntaxIssues] = abstractSyntaxIssues;
  ];

  node[tag, children, data]
]



(*

"normalize" the ast by doing some operations:

Remove line continuations from leafs

Remove embedded newlines from strings

*)
Options[normalizeTokens] = {
  "FormatOnly" -> False,
  "Newline" :> $DefaultNewline,
  "TabWidth" :> $DefaultTabWidth
}

normalizeTokens[astIn_, OptionsPattern[]] :=
  Module[{ast, data, tokStartLocs, simpleLineContinuations, complexLineContinuations,
    embeddedNewlines, grouped, poss, tuples, mapSpecs, formatOnly, newline, embeddedTabs},

    ast = astIn;

    formatOnly = OptionValue["FormatOnly"];
    newline = OptionValue["Newline"];
    tabWidth = OptionValue["TabWidth"];

    data = ast[[3]];

    If[KeyExistsQ[data, "SimpleLineContinuations"] ||
      KeyExistsQ[data, "ComplexLineContinuations"] ||
      KeyExistsQ[data, "EmbeddedNewlines"] ||
      KeyExistsQ[data, "EmbeddedTabs"],

      (*
      -5 is where LeafNodes[] are
      *)
      poss = Position[ast, LeafNode[_, _, _], {-5}];

      tokStartLocs = #[[3, Key[Source], 1]]& /@ Extract[ast, poss];

      (*
      Group by starting SourceLocation
      *)
      grouped = GroupBy[Transpose[{tokStartLocs, poss}, {2, 1}], #[[1]]&];

      If[KeyExistsQ[data, "SimpleLineContinuations"],

        simpleLineContinuations = data["SimpleLineContinuations"];

        mapSpecs = Map[
          Function[{contLoc},

            tuples = grouped[contLoc];

            (*
            The token associated with this location may have been abstracted away and now missing
            *)
            If[!MissingQ[tuples],

              tuples
              ,
              Nothing
            ]
          ]
          ,
          simpleLineContinuations
        ];

        mapSpecs = Flatten[mapSpecs, 1];

        (*
        There may be more than 1 simple line continuation, so use FixedPoint
        *)
        ast = MapAt[FixedPoint[removeSimpleLineContinuation, #]&, ast, mapSpecs[[All, 2]]];

        KeyDropFrom[data, "SimpleLineContinuations"]
      ];

      (*
      If formatting, then leave complex line continuations alone
      If abstracting, then remove them
      *)
      If[!formatOnly,

        If[KeyExistsQ[data, "ComplexLineContinuations"],

          complexLineContinuations = data["ComplexLineContinuations"];

          mapSpecs = Map[
            Function[{contLoc},

              tuples = grouped[contLoc];

              (*
              The token associated with this location may have been abstracted away and now missing
              *)
              If[!MissingQ[tuples],

                tuples
                ,
                Nothing
              ]
            ]
            ,
            complexLineContinuations
          ];

          mapSpecs = Flatten[mapSpecs, 1];

          ast = MapAt[removeComplexLineContinuations, ast, mapSpecs[[All, 2]]];

          KeyDropFrom[data, "ComplexLineContinuations"]
        ];
      ];

      If[KeyExistsQ[data, "EmbeddedNewlines"],

        embeddedNewlines = data["EmbeddedNewlines"];

        mapSpecs = Map[
          Function[{newlineLoc},

            tuples = grouped[newlineLoc];

            (*
            The token associated with this location may have been abstracted away and now missing
            *)
            If[!MissingQ[tuples],

              tuples
              ,
              Nothing
            ]
          ]
          ,
          embeddedNewlines
        ];

        mapSpecs = Flatten[mapSpecs, 1];

        ast = MapAt[convertEmbeddedNewlines[#, "FormatOnly" -> formatOnly, "Newline" -> newline]&, ast, mapSpecs[[All, 2]]];

        KeyDropFrom[data, "EmbeddedNewlines"]
      ];

      If[KeyExistsQ[data, "EmbeddedTabs"],

        embeddedTabs = data["EmbeddedTabs"];

        mapSpecs = Map[
          Function[{newlineLoc},

            tuples = grouped[newlineLoc];

            (*
            The token associated with this location may have been abstracted away and now missing
            *)
            If[!MissingQ[tuples],

              tuples
              ,
              Nothing
            ]
          ]
          ,
          embeddedTabs
        ];

        mapSpecs = Flatten[mapSpecs, 1];

        ast = MapAt[convertEmbeddedTabs[#, "TabWidth" -> tabWidth, "Newline" -> newline]&, ast, mapSpecs[[All, 2]]];

        KeyDropFrom[data, "EmbeddedTabs"]
      ];

      ast[[3]] = data;

    ];

    ast
  ]



(*
Remove both external and internal line continuations

Do not need to worry about preceding backslashes or anything like that because
this is NOT called on multiline strings or multiline comments
*)
removeSimpleLineContinuation[LeafNode[tag_, s_String, data_]] :=
  LeafNode[tag, StringReplace[s, StartOfString ~~ "\\" ~~ ("\n" | "\r\n" | "\r") ~~ WhitespaceCharacter... -> ""], data]

(*
removeSimpleLineContinuation is called by FixedPoint, so provide an escape hatch
*)
removeSimpleLineContinuation[___] :=
  $Failed

(*
Remove remaining external line continuation

LineColumn convention
*)
removeRemainingSimpleLineContinuation[LeafNode[tag_, s_String, data:KeyValuePattern[Source -> {{_ , _}, {_, _}}]]] :=
  Module[{cases, ws, rest},

    cases = StringCases[s, StartOfString ~~ "\\" ~~ ("\n" | "\r\n" | "\r") ~~ ws : (WhitespaceCharacter ...) ~~ rest___ :> {ws, rest}];

    If[cases == {},
      LeafNode[tag, s, data]
      ,
      {ws, rest} = cases[[1]];
      LeafNode[tag, rest, <|data, Source -> {{data[[Key[Source], 1, 1]] + 1, StringLength[ws] + 1}, data[[Key[Source], 2]]}|>]
    ]
  ]

removeRemainingSimpleLineContinuation[LeafNode[tag_, s_String, data_]] :=
  LeafNode[tag, StringReplace[s, StartOfString ~~ "\\" ~~ ("\n" | "\r\n" | "\r") ~~ WhitespaceCharacter... -> ""], data]

(*
removeRemainingSimpleLineContinuation is called by FixedPoint, so provide an escape hatch
*)
removeRemainingSimpleLineContinuation[___] :=
  $Failed


(*
Strip \<newline> line continuations from str

Must handle \\<newline> correctly, DO NOT STRIP!

Strip \\\<newline>, DO NOT STRIP \\\\<newline>, etc.

It may seem like a RegularExpression with a positive lookbehind construct would work here,
but lookbehinds need to be fixed width.

So cannot use a regex.

*)
removeComplexLineContinuations[LeafNode[tag : String | Token`Comment, str_, data_]] :=
  Module[{continuationPoss, backslashCount, onePastLastPos, pos, takeSpec},

    continuationPoss = StringPosition[str, "\n"|"\r"];

    (*
    all newlines with an odd number of leading backslashes = line continuations
    *)
    continuationPoss = Map[
      Function[{newlinePos},
        pos = newlinePos[[1]];
        onePastLastPos = NestWhile[# - 1 &, pos - 1, # >= 1 && StringTake[str, {#}] == "\\" &];
        backslashCount = pos - onePastLastPos - 1;
        If[OddQ[backslashCount],
          {pos - 1, pos}
          ,
          Nothing
        ]
      ]
      ,
      continuationPoss
    ];

    (* make sure to include both characters in \r\n *)
    continuationPoss = Map[
      Function[{contPos},
        pos = contPos[[2]];
        If[StringTake[str, {pos}] == "\r" && pos + 1 <= StringLength[str] && StringTake[str, {pos + 1}] == "\n",
          {contPos[[1]], pos + 1}
          ,
          contPos
        ]
      ]
      ,
      continuationPoss
    ];

    (*

    This used to be:

    LeafNode[tag, StringReplacePart[str, "", continuationPoss], data]

    but that is VERY slow for removing substrings

    So convert to a Take spec and use StringTake and StringJoin
    *)

    takeSpec = {#[[1, 2]] + 1, #[[2, 1]] - 1}& /@
      Partition[{{Null, 0}} ~Join~ continuationPoss ~Join~ {{StringLength[str] + 1, Null}}, 2, 1];

    LeafNode[tag, StringJoin[StringTake[str, takeSpec]], data]
  ]

isWhitespaceChar[c_String] := c == " " || c == "\t"


(*
It is not convenient to have actual <newline> characters embedded in strings
*)

Options[convertEmbeddedNewlines] = {
  "FormatOnly" -> False,
  "Newline" :> $DefaultNewline
}

convertEmbeddedNewlines[LeafNode[tag : String | Token`Comment, str_, data_], OptionsPattern[]] :=
  Module[{formatOnly, newline, escapedNewline},

    formatOnly = OptionValue["FormatOnly"];
    newline = OptionValue["Newline"];

    If[formatOnly,
      (*
      Formatting, so keep embedded, but still canonicalize newline
      *)
      LeafNode[tag, StringReplace[str, {"\n" -> newline, "\r\n" -> newline, "\r" -> newline}], data]
      ,
      (*
      Abstracting, so completely escape embedded newlines
      *)
      Switch[newline,
        "\n",
          escapedNewline = "\\n";
        ,
        "\r\n",
          escapedNewline = "\\r\\n";
        ,
        "\r",
          escapedNewline = "\\r";
      ];
      LeafNode[tag, StringReplace[str, {"\n" -> escapedNewline, "\r\n" -> escapedNewline, "\r" -> escapedNewline}], data]
    ]
  ]


Options[convertEmbeddedTabs] = {
  "TabWidth" :> $DefaultTabWidth,
  "Newline" :> $DefaultNewline
}

convertEmbeddedTabs[LeafNode[tag : String | Token`Comment, str_, data_], OptionsPattern[]] :=
  Module[{tabWidth, newline, startingColumn},

    tabWidth = OptionValue["TabWidth"];
    newline = OptionValue["Newline"];

    Switch[data,
      KeyValuePattern[Source -> {{_, _}, {_, _}}],
        (*
        LineColumn convention
        *)
        startingColumn = data[[Key[Source], 1, 2]];
        LeafNode[tag, replaceTabs[str, startingColumn, newline, tabWidth], data]
      ,
      _,
        (*
        Any other convention

        replace with " "; we don't know anything about columns
        *)
        startingColumn = 0;
        LeafNode[tag, replaceTabs[str, startingColumn, newline, tabWidth], data]
    ]
  ]


(*
Functions copied from CodeFormatter

FIXME: when a dependency on CodeFormatter is created, then use those functions and remove this section

*)


(*
Memoizing function that returns the number of spaces that a tab should be replaced with

tabReplacementFunc[1, 4] => "    "
tabReplacementFunc[2, 4] => "   "
tabReplacementFunc[3, 4] => "  "
tabReplacementFunc[4, 4] => " "
tabReplacementFunc[5, 4] => "    "
tabReplacementFunc[6, 4] => "   "
tabReplacementFunc[7, 4] => "  "
tabReplacementFunc[8, 4] => " "
tabReplacementFunc[9, 4] => "    "

*)
tabReplacementFunc[col_Integer, tabWidth_Integer] :=
  tabReplacementFunc[col, tabWidth] =
  StringJoin[Table[" ", Mod[1 - col, tabWidth, 1]]]


replaceTabs[str_String, startingColumn_Integer, newline_String, tabWidth_Integer] :=
Module[{lines},
  lines = StringSplit[str, newline, All];
  (*
  Pad the first line with the correct number of characters from its origin
  *)
  lines[[1]] = StringJoin[Table["!", startingColumn - 1], lines[[1]]];
  lines = Map[
    Function[{line},
      (*
      for each line, accumulate a string by replacing each tab with its equivalent spaces,
      working from left to right
      *)
      NestWhile[
        Function[{lineAccum},
          With[{pos = StringPosition[lineAccum, "\t"][[1, 1]]},
            StringReplacePart[lineAccum, tabReplacementFunc[pos, tabWidth], {pos, pos}]]
        ]
        ,
        line
        ,
        StringContainsQ[#, "\t"]&
      ]
    ]
    ,
    lines
  ];
  (*
  Remove padding
  *)
  lines[[1]] = StringDrop[lines[[1]], startingColumn - 1];
  StringJoin[Riffle[lines, newline]]
]



End[]

EndPackage[]
