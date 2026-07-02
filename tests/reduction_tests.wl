(* tests/reduction_tests.wl — Sight reduction tests (Task 4) *)
(* Single expression so Get[] returns all VerificationTests as one flat list. *)

Join[

 (* --- Dynamic tests anchored on live ephemeris --- *)
 With[{t = DateObject[{2024, 11, 15, 14, 30, 0}, TimeZone -> 0]},
  Module[{gp = cnSunGP[t], truth = {20.0, -40.0}, ho, ap, ca},
   ho = cnSunAltitude[truth, t];          (* true altitude at the true position *)
   ap = {21.0, -41.0};                    (* assumed position 1 deg away *)
   ca = cnComputedAltitude[ap, gp];
   {
    (* Hc at the AP is a valid altitude *)
    VerificationTest[0 < ca[[1]] < 90, True, TestID -> "hc-range"],
    (* Azimuth in 0..360 *)
    VerificationTest[0 <= ca[[2]] < 360, True, TestID -> "zn-range"],
    (* Independent hand check of the altitude formula at the AP *)
    VerificationTest[
      Abs[ca[[1]] - ArcSin[Sin[ap[[1]] Degree] Sin[gp[[1]] Degree] +
          Cos[ap[[1]] Degree] Cos[gp[[1]] Degree] Cos[(ap[[2]] - gp[[2]]) Degree]]/Degree] < 1*^-6,
      True, TestID -> "hc-formula"],
    (* Sanity: computed altitude AT the true position gives small intercept vs Ho.
       Tolerance 1.5 nm accommodates the ~0.01 deg accuracy of the low-precision
       GP formula relative to WL's SunPosition ephemeris. *)
    VerificationTest[Abs[cnIntercept[ho, cnComputedAltitude[truth, gp][[1]]]] < 1.5, True, TestID -> "intercept-zero-at-truth"],
    (* LOP closest point lies ~|intercept| nm from the AP.
       Parentheses around "// QuantityMagnitude" are required: // has lower
       precedence than -, so without them WL parses the subtraction before applying
       QuantityMagnitude. *)
    VerificationTest[
      Module[{lop = cnLOP[ap, gp, ho]},
        Abs[(GeoDistance[GeoPosition[ap], GeoPosition[lop["point"]]]/
            Quantity[1, "NauticalMiles"] // QuantityMagnitude)
          - Abs[lop["interceptNm"]]] < 0.5],
      True, TestID -> "lop-offset"]
   }]],

 (* --- Step 5: Synthetic self-consistency regression (Task 4) ---
    SYNTHETIC — not a published observation.
    I could not confidently reproduce a specific Bowditch / H.O. 229 worked example
    from memory without risk of fabrication; a labelled synthetic anchor is more honest.

    Inputs: AP = {35N, 45W}, GP = {15N, 0} → LHA = Mod[-45 - 0, 360] = 315 deg (body east).
    Expected Hc and Zn are derived analytically here from the same defining formulae
    used in cnComputedAltitude, so the test is a numerical self-consistency anchor:
    it guards against typos, sign errors, and unit mistakes in the implementation.
    Sin[315 deg] < 0 → body east of meridian → Zn stays as ArcCos result (not 360-Z).
    Tolerances: 1e-6 deg on Hc and Zn. *)
 Module[{ap = {35.0, -45.0}, gp = {15.0, 0.0},
         lat = 35.0, dec = 15.0, lha = 315.0,
         hcExp, cosZn, znExp, res},
   hcExp = ArcSin[Sin[lat Degree] Sin[dec Degree] +
                  Cos[lat Degree] Cos[dec Degree] Cos[lha Degree]] / Degree;
   cosZn = (Sin[dec Degree] - Sin[hcExp Degree] Sin[lat Degree]) /
           (Cos[hcExp Degree] Cos[lat Degree]);
   znExp = ArcCos[Clip[cosZn, {-1, 1}]] / Degree;
   If[Sin[lha Degree] > 0, znExp = 360 - znExp];
   znExp = Mod[znExp, 360];
   res = cnComputedAltitude[ap, gp];
   {
     VerificationTest[Abs[res[[1]] - hcExp] < 1*^-6, True,
       TestID -> "synthetic-regression-hc"],
     VerificationTest[Abs[res[[2]] - znExp] < 1*^-6, True,
       TestID -> "synthetic-regression-zn"]
   }]

]
