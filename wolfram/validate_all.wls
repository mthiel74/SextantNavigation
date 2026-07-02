#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  validate_all.wls  --  [NETWORK] consolidated ephemeris validation.

    Item 19 of the scientific-improvements campaign. Runs the two
    authoritative cross-checks of the project's SELF-CONTAINED ephemeris
    against public-domain external references, and writes a combined
    residual figure + a short validation report.

      1. SUN  : our cnSunGP / cnSunGHA  vs  JPL Horizons (NASA/JPL, public
                domain) apparent declination over the voyage window.
                Delegates to wolfram/fetch_horizons.wls.
      2. STARS: our embedded J2000 nav-star catalogue (data/nav_stars.csv)
                vs an authoritative catalogue (Hipparcos via VizieR / SIMBAD).
                Delegates to wolfram/fetch_stars.wls.

    This script REQUIRES network access and is NOT part of the offline
    rebuild path. Run it on a connected machine; commit the resulting
    docs/images/ephemeris_residuals.png + data/horizons_sun.csv (Sun) and
    the certified data/nav_stars.csv (stars). If the network is
    unreachable it fails loud and exits non-zero -- it never fabricates
    validation data.

    Usage:  wolframscript -file wolfram/validate_all.wls
*)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
wolframDir = FileNameJoin[{repoRoot, "wolfram"}];

runStage[label_, script_] := Module[{path, res, code},
  path = FileNameJoin[{wolframDir, script}];
  If[! FileExistsQ[path],
    Print["[validate_all] MISSING ", script]; Return[<|"ok" -> False, "label" -> label|>]];
  Print["\n========================================================"];
  Print["[validate_all] ", label, "  ->  ", script];
  Print["========================================================"];
  res = RunProcess[{"wolframscript", "-file", path}];
  code = res["ExitCode"];
  Print[res["StandardOutput"]];
  If[res["StandardError"] =!= "", Print["STDERR: ", res["StandardError"]]];
  <|"ok" -> (code === 0), "label" -> label, "exit" -> code|>
];

Print["[validate_all] Consolidated ephemeris validation (NETWORK required)."];

sun  = runStage["Sun GP vs JPL Horizons",        "fetch_horizons.wls"];
star = runStage["Star catalogue vs Hipparcos",   "fetch_stars.wls"];

Print["\n========================================================"];
Print["[validate_all] SUMMARY"];
Print["========================================================"];
Print["  Sun  (Horizons): ", If[sun["ok"], "PASS", "FAIL/UNREACHABLE (exit " <> ToString[sun["exit"]] <> ")"]];
Print["  Stars (Hipparcos): ", If[star["ok"], "PASS", "FAIL/UNREACHABLE (exit " <> ToString[star["exit"]] <> ")"]];

If[sun["ok"] && star["ok"],
  Print["\n[validate_all] Both cross-checks completed. ",
        "See docs/images/ephemeris_residuals.png and the certified ",
        "data/nav_stars.csv / data/horizons_sun.csv."];
  Print["[validate_all] DONE"];
  ,
  Print["\n[validate_all] One or more stages did not complete -- most likely ",
        "no network in this environment. Re-run on a connected machine. ",
        "No validation data was fabricated."];
  Exit[1]
];
