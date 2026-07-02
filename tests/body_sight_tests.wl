{
  (* --- Test 1: Zero-noise Sirius sight reduces through truth --- *)
  (* truePos = {15.0, -40.0}, ap = {14.5, -39.5} is ~41 nm from truth.
     With sigmaMin=0 the LOP must pass through truth, so |interceptNm| <= AP-truth + 1 nm *)
  VerificationTest[
    Module[{truePos = {15.0, -40.0},
            t = DateObject[{2024, 3, 20, 21, 30, 0}, TimeZone -> 0],
            ap = {14.5, -39.5},
            hs, lop},
      hs = cnGenerateSightBody[truePos, t, {"Star","Sirius"}, <|"sigmaMin"->0|>];
      lop = cnReduceSightBody[hs, t, ap, {"Star","Sirius"}];
      Abs[lop["interceptNm"]] < 42
    ],
    True, TestID -> "star-zero-noise-sirius"],

  (* --- Test 2: No SD for stars (star Ho differs from Sun Ho by ~16') --- *)
  (* At h=35 deg: Sun Ho = hs - dip - refraction + SD + parallax;
     Star Ho = hs - dip - refraction (no SD, no parallax).
     Difference = SD + parallax = 16 + 0.15*cos(35) ~ 16.12'.
     Assert within 1' of 16.0' i.e. between 15 and 17'. *)
  VerificationTest[
    Module[{h = 35.0},
      Abs[(cnObservedAltitudeBody[h, 2, 0, "Sun"] -
           cnObservedAltitudeBody[h, 2, 0, {"Star","Sirius"}]) * 60 - 16.0] < 1.0
    ],
    True, TestID -> "star-no-sd"],

  (* --- Test 3: 3-star fix recovers truth with zero noise --- *)
  (* Stars: Sirius (58 deg), Rigel (58 deg), Procyon (73 deg) from {15.0,-40.0}
     at 2024-03-20 21:30 UTC. Zero noise => fix should converge to < 0.5 nm of truth. *)
  VerificationTest[
    Module[{truePos = {15.0, -40.0},
            t = DateObject[{2024, 3, 20, 21, 30, 0}, TimeZone -> 0],
            ap = {15.1, -40.1},
            stars = {{"Star","Sirius"}, {"Star","Rigel"}, {"Star","Procyon"}},
            lops, fix, errNm},
      lops = Table[
        cnReduceSightBody[
          cnGenerateSightBody[truePos, t, s, <|"sigmaMin"->0|>],
          t, ap, s],
        {s, stars}];
      fix = cnFix[lops];
      errNm = QuantityMagnitude[GeoDistance[GeoPosition[truePos], GeoPosition[fix]] /
                                Quantity[1, "NauticalMiles"]];
      errNm < 0.5
    ],
    True, TestID -> "three-star-fix"],

  (* --- Test 4: Backward compatibility — cnGenerateSight/cnReduceSight still work --- *)
  (* AP {20.5,-30.5} is ~47 nm from truth {20.0,-30.0}.
     Zero-noise Sun sight must reduce to intercept < 60 nm. *)
  VerificationTest[
    Module[{truePos = {20.0, -30.0},
            t = DateObject[{2024, 6, 21, 12, 0, 0}, TimeZone->0],
            ap = {20.5, -30.5},
            hs, lop},
      hs = cnGenerateSight[truePos, t, <|"sigmaMin"->0|>];
      lop = cnReduceSight[hs, t, ap];
      Abs[lop["interceptNm"]] < 60
    ],
    True, TestID -> "backward-compat-sun"]
}
