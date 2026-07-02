(* tests/ekf_tests.wl -- EKF running fix + systematic-vs-random error (R2) *)
(* Top-level expression returning a flat List of VerificationTests so Get[]
   composes with the other test files in run_tests.wls.
   $InputFileName is bound to THIS file during Get, so the repo root and data
   files resolve by absolute path regardless of the caller's working dir. *)

Module[
  {repo, rows, hdr, data, col, mkDate, voyage, recs, ekfErrs, drErrs, ekfRMS,
   drRMS, sj, rfErrs, rfRMS, truth, times, bodies, sys3, sys0, covShrink,
   finalCovTr, initCovTr},

  repo = ParentDirectory[DirectoryName[$InputFileName]];

  (* --- Build the voyage from data/voyage.csv --- *)
  rows = Import[FileNameJoin[{repo, "data", "voyage.csv"}], "CSV"];
  hdr  = rows[[1]]; data = rows[[2 ;;]];
  col[name_] := Position[hdr, name][[1, 1]];
  (* Import may auto-parse the ISO datetime column into a DateObject or leave a
     string; accept both and normalise to a UTC DateObject. *)
  mkDate[v_] := If[Head[v] === DateObject, v,
                   DateObject[StringReplace[v, "Z" -> ""], TimeZone -> 0]];
  voyage = Table[
    <|"day" -> r[[col["day"]]],
      "t" -> mkDate[r[[col["datetimeUTC"]]]],
      "truePos" -> {r[[col["latTrue"]]], r[[col["lonTrue"]]]},
      "drPos"   -> {r[[col["latDR"]]],   r[[col["lonDR"]]]}|>,
    {r, data}];

  (* --- Run the EKF once (fixed seed) --- *)
  recs    = cnEKFVoyage[voyage, 3, <|"seed" -> 42, "sigmaMin" -> 1.0|>];
  ekfErrs = #["errVsTruthNm"] & /@ recs;
  drErrs  = #["drErrorNm"]    & /@ recs;
  ekfRMS  = Sqrt[Mean[ekfErrs^2]];
  drRMS   = Sqrt[Mean[drErrs^2]];

  (* --- Per-day running-fix RMS from the stored sights (for comparison) --- *)
  sj     = Import[FileNameJoin[{repo, "data", "sights.json"}], "RawJSON"];
  rfErrs = #["runningFixErrorNm"] & /@ sj["days"];
  rfRMS  = Sqrt[Mean[rfErrs^2]];

  (* --- Covariance bookkeeping --- *)
  covShrink  = AllTrue[recs, Tr[#["covNm"]] < Tr[#["covPredictNm"]] &];
  finalCovTr = Tr[recs[[-1]]["covNm"]];
  initCovTr  = Tr[recs[[1]]["covPredictNm"]];   (* day-1 predict cov = the P0 prior *)

  (* --- Systematic vs random: 3 Sun sights, asymmetric (clustered-S) azimuths --- *)
  truth  = {20.0, -40.0};
  times  = {DateObject[{2024, 11, 22, 10, 0, 0}, TimeZone -> 0],
            DateObject[{2024, 11, 22, 13, 0, 0}, TimeZone -> 0],
            DateObject[{2024, 11, 22, 16, 0, 0}, TimeZone -> 0]};
  bodies = {"Sun", "Sun", "Sun"};
  SeedRandom[7]; sys3 = cnSystematicFixError[truth, times, bodies, 1.0, 3.0, 800];
  SeedRandom[7]; sys0 = cnSystematicFixError[truth, times, bodies, 1.0, 0.0, 800];

  {
    (* === Test 1: EKF beats DR === *)
    VerificationTest[ekfRMS < drRMS, True, TestID -> "ekf-rms-lt-dr-rms"],
    VerificationTest[ekfRMS < 3.0,   True, TestID -> "ekf-rms-under-3nm"],
    (* EKF is at least as good as the single-day running fix, on average:
       its MEDIAN error beats the per-day running-fix median (the EKF RMS is
       only modestly inflated by the final arrival-manoeuvre day). *)
    VerificationTest[Median[ekfErrs] <= Median[rfErrs], True, TestID -> "ekf-le-runningfix-median"],
    VerificationTest[ekfRMS <= 1.5 rfRMS, True, TestID -> "ekf-rms-near-runningfix"],

    (* === Test 2: covariance shrinks at every update step === *)
    VerificationTest[covShrink, True, TestID -> "ekf-cov-shrinks-each-day"],
    VerificationTest[finalCovTr < initCovTr, True, TestID -> "ekf-cov-final-lt-initial"],

    (* === Test 3: systematic bias != scatter ===
       indexError 3' -> fix BIAS >~ 2 nm; SCATTER (cov trace) ~ random-only
       (within 30%): a systematic error moves the centre, not the spread. *)
    VerificationTest[sys3["biasNm"] > 2.0, True, TestID -> "sys-bias-ge-2nm"],
    VerificationTest[
      Abs[sys3["scatterTraceNm2"] - sys0["scatterTraceNm2"]] / sys0["scatterTraceNm2"] < 0.30,
      True, TestID -> "sys-scatter-unchanged"],
    VerificationTest[sys0["biasNm"] < 1.0, True, TestID -> "random-only-unbiased"],

    (* === Test 4: EKF stays sane (no divergence) === *)
    VerificationTest[NumericQ[finalCovTr] && 0 < finalCovTr < 50.0,
      True, TestID -> "ekf-no-divergence"]
  }
]
