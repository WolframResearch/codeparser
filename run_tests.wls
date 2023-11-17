#!/usr/bin/env wolframscript

$builtPacletDir = FileNameJoin[{Directory[], "build", "paclet", "CodeParser"}];

If[!FileExistsQ[$builtPacletDir],
	Throw["Failed"];
	Exit[-1];
];


Print[
	"Loading CodeParser: ", PacletDirectoryLoad[$builtPacletDir]
]

Needs["CodeParser`"]



Needs["MUnit`"]

createSuccessLogger[] := With[{logger = Unique[]},
  Module[{success = True},
   logger /: LogFatal[logger, _] := success = False;
   logger /: LogFailure[logger, _] := success = False;
   logger /: LogMessagesFailure[logger, _] := success = False;
   logger /: LogError[logger, _] := success = False;
   logger /: LogFatal[logger, _] := success = False;
   logger /: TestRunSucceededQ[logger] := success;
   logger
   ]
  ]

successLogger = createSuccessLogger[]

Quiet @ EchoTiming @ TestRun[
	"Tests/TestSuite.mt",
	Loggers :> {VerbosePrintLogger[], successLogger}
]

If[TrueQ[TestRunSucceededQ[successLogger]],
  Exit[0]
  ,
  Exit[1]
]
