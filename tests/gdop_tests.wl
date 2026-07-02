(* gdop_tests.wl  -- GDOP optimal star selection (Item 4)
   Observer: pos = {15, -40}  (15 deg N, 40 deg W, South Atlantic)
   Time:     t = 2025-Jun-21 22:00 UTC  (nautical twilight, Sun at -11 deg)
   At this pos/time 19 navigational stars are visible above 15 deg.

   Clustered reference triplet:  {Kochab, Alkaid, Alioth}
     azimuths ~ 6.9 deg, 7.8 deg, 354.4 deg  (13 deg wedge)  -> GDOP ~ 5.45

   Optimal triplet found by cnBestStarTriplet:
     {Vega, Sabik, Alioth}  azimuths ~ 53 deg, 116 deg, 354 deg  -> GDOP ~ 1.155
*)

(* --- shared setup (evaluated once at Get time) --- *)
$gdopPos  = {15, -40};
$gdopTime = DateObject[{2025, 6, 21, 22, 0, 0}, TimeZone -> 0];
$gdopBest = cnBestStarTriplet[$gdopPos, $gdopTime];     (* cached Association *)

(* --- tests --- *)
{
  (* 1. At least 6 navigational stars visible above 15 deg *)
  VerificationTest[
    Length[cnVisibleStars[$gdopPos, $gdopTime]] >= 6,
    True,
    TestID -> "gdop-visibility-count"],

  (* 2. GDOP formula: three azimuths 120 deg apart -> Sqrt[4/3] ~ 1.1547 *)
  VerificationTest[
    Abs[cnGDOPFromAzimuths[{0., 120., 240.}] - Sqrt[4./3.]] < 0.01,
    True,
    TestID -> "gdop-optimal-formula"],

  (* 3. Clustered raw azimuths (10,20,30 deg) give GDOP > 3 *)
  VerificationTest[
    cnGDOPFromAzimuths[{10., 20., 30.}] > 3.0,
    True,
    TestID -> "gdop-clustered-raw-large"],

  (* 4. cnFixGDOP with real clustered stars gives GDOP > 3 *)
  VerificationTest[
    cnFixGDOP[$gdopPos, $gdopTime, {"Kochab", "Alkaid", "Alioth"}] > 3.0,
    True,
    TestID -> "gdop-clustered-triplet-large"],

  (* 5. Best triplet GDOP is near the theoretical minimum (< 1.4) *)
  VerificationTest[
    $gdopBest["gdop"] < 1.4,
    True,
    TestID -> "gdop-best-triplet-near-optimal"],

  (* 6. Best triplet GDOP is less than the clustered reference triplet GDOP *)
  VerificationTest[
    $gdopBest["gdop"] < cnFixGDOP[$gdopPos, $gdopTime, {"Kochab", "Alkaid", "Alioth"}],
    True,
    TestID -> "gdop-best-beats-clustered"],

  (* 7. Monte Carlo corroboration: best triplet yields smaller empirical CEP
        (50th-percentile position error) than the clustered triplet.
        20 seeded trials per group; GDOP ratio ~ 4.7 makes the ordering robust.
        Errors computed in nm in the local tangent plane. *)
  VerificationTest[
    Module[{bestStars, clusterStars, trialFix, bestErrs, clustErrs},
      bestStars    = $gdopBest["stars"];
      clusterStars = {"Kochab", "Alkaid", "Alioth"};
      trialFix[stars_, seed_] := Module[{bodies, lops},
        SeedRandom[seed];
        bodies = {"Star", #} & /@ stars;
        lops = Table[
          cnReduceSightBody[
            cnGenerateSightBody[$gdopPos, $gdopTime, body, <|"sigmaMin" -> 1.0|>],
            $gdopTime, $gdopPos, body],
          {body, bodies}];
        cnFix[lops]
      ];
      bestErrs  = Table[
        With[{fix = trialFix[bestStars,    200 + k]},
          Sqrt[((fix[[1]] - $gdopPos[[1]]) 60.)^2 +
               ((fix[[2]] - $gdopPos[[2]]) Cos[$gdopPos[[1]] Degree] 60.)^2]],
        {k, 20}];
      clustErrs = Table[
        With[{fix = trialFix[clusterStars, 200 + k]},
          Sqrt[((fix[[1]] - $gdopPos[[1]]) 60.)^2 +
               ((fix[[2]] - $gdopPos[[2]]) Cos[$gdopPos[[1]] Degree] 60.)^2]],
        {k, 20}];
      Quantile[bestErrs, 0.5] < Quantile[clustErrs, 0.5]
    ],
    True,
    TestID -> "gdop-montecarlo-cep-ordering"]
}
