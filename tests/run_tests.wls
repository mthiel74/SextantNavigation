#!/usr/bin/env wolframscript
(* Runs every tests/*_tests.wl file, prints a summary, exits nonzero on failure. *)
repo = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repo, "wolfram", "CelestialNavigation.wl"}]];

testFiles = FileNames["*_tests.wl", FileNameJoin[{repo, "tests"}]];
allTests = Flatten[Get /@ testFiles];
report = TestReport[allTests];
Print["Tests run: ", report["TestsSucceededCount"] + report["TestsFailedCount"]];
Print["Succeeded: ", report["TestsSucceededCount"]];
Print["Failed:    ", report["TestsFailedCount"]];
If[report["TestsFailedCount"] > 0,
  Print["FAILURES:"];
  Print[#["Input"]] & /@ report["TestsFailed"];
  Exit[1]
];
Print["ALL TESTS PASSED"];
