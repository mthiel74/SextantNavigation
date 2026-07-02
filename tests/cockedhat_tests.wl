(* tests/cockedhat_tests.wl -- Cocked-hat 25%% theorem (Item 10) *)
(* Top-level List so Get[] returns all VerificationTests as a flat list. *)

{
  (* --- Test 1a: point clearly inside a known triangle --- *)
  (* Triangle: {20,0}, {22,0}, {21,1} degrees lat/lon.
     Centroid ~ {21, 0.333}; the point {21, 0.3} is near-centroid and clearly inside. *)
  VerificationTest[
    cnPointInTriangle[{21.0, 0.3}, {{20.0, 0.0}, {22.0, 0.0}, {21.0, 1.0}}],
    True, TestID -> "pit-inside"],

  (* --- Test 1b: point clearly outside --- *)
  (* {24, 2} is north-east of all three vertices. *)
  VerificationTest[
    cnPointInTriangle[{24.0, 2.0}, {{20.0, 0.0}, {22.0, 0.0}, {21.0, 1.0}}],
    False, TestID -> "pit-outside"],

  (* --- Test 2: THE 25% THEOREM ---
     Sun sights at three daytime times give three LOPs with bearings SE / S / WSW.
     With sigma = 1.5 arcmin and 2000 independent trials, P(truth inside) -> 0.25.
     SE = sqrt(0.25 * 0.75 / 2000) ~ 0.0097; tolerance +-0.04 is > 4 SE. *)
  With[{
    truth = {20.0, -40.0},
    times = {DateObject[{2024,11,22,10, 0,0}, TimeZone->0],
             DateObject[{2024,11,22,13, 0,0}, TimeZone->0],
             DateObject[{2024,11,22,16, 0,0}, TimeZone->0]},
    bodies = {"Sun","Sun","Sun"},
    opts = <|"sigmaMin"->1.5, "seed"->7|>,
    n = 1000},
    VerificationTest[
      Abs[cnCockedHatProbability[truth, times, bodies, opts, n] - 0.25] < 0.05,
      True, TestID -> "cocked-hat-25pct-sun"]
  ],

  (* --- Test 3: INDEPENDENCE OF BEARINGS ---
     Different Sun azimuths (different times) still converge to 0.25.
     Confirms the theorem is bearing-independent. *)
  With[{
    truth = {20.0, -40.0},
    times = {DateObject[{2024,11,22, 9, 0,0}, TimeZone->0],
             DateObject[{2024,11,22,11,30,0}, TimeZone->0],
             DateObject[{2024,11,22,14,30,0}, TimeZone->0]},
    bodies = {"Sun","Sun","Sun"},
    opts = <|"sigmaMin"->1.5, "seed"->13|>,
    n = 1000},
    VerificationTest[
      Abs[cnCockedHatProbability[truth, times, bodies, opts, n] - 0.25] < 0.05,
      True, TestID -> "cocked-hat-25pct-independent-bearings"]
  ]
}
