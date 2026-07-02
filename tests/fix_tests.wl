(* tests/fix_tests.wl — Fix, advance, running fix, cocked hat (Task 5); sight generation + MC (Task 6) *)
(* Top-level Join[] so Get[] returns all VerificationTests as one flat list. *)

Join[

 (* --- Task 5: Fix, advance, running fix, cocked hat --- *)
 With[{t = DateObject[{2024, 11, 15, 14, 30, 0}, TimeZone -> 0]},
  Module[{truth = {20.0, -40.0}, gp1, gp2, gp3, mk},
   (* Three synthetic bodies with good cut angles via offset GPs. *)
   gp1 = {0.0, -40.0}; gp2 = {20.0, -10.0}; gp3 = {-10.0, -70.0};
   (* mk builds a noise-free LOP: uses cnAltitudeFromGP for BOTH Ho and Hc
      (same formula), guaranteeing each LOP passes through truth to within the
      linearization error of the intercept method. *)
   mk[gp_] := Module[{ho, ap},
     ho = cnAltitudeFromGP[truth, gp];
     ap = truth + {0.4, -0.5};
     cnLOP[ap, gp, ho]];
   {
    (* fix-recovers-truth: perfect (noise-free) LOPs intersect at truth within 0.5 nm.
       FIXES vs brief:
         (a) removed stray ] that appeared after QuantityMagnitude before "< 0.5"
         (b) removed spurious trailing // (# &) (identity no-op, was a copy artifact)
         (c) wrapped "// QuantityMagnitude" in parens: // has lower precedence than /
             and <, so without parens WL applies QuantityMagnitude to the boolean,
             not to the Quantity. *)
    VerificationTest[
      (GeoDistance[GeoPosition[truth],
                   GeoPosition[cnFix[{mk[gp1], mk[gp2], mk[gp3]}]]]/
        Quantity[1, "NauticalMiles"] // QuantityMagnitude) < 0.5,
      True, TestID -> "fix-recovers-truth"],

    (* advance-distance: advancing an LOP 60 nm north moves its point ~60 nm.
       FIX vs brief: wrapped "// QuantityMagnitude" in parens so the subtraction
       of 60 happens AFTER QuantityMagnitude is applied — without parens WL
       parses "//QuantityMagnitude - 60" as "// (QuantityMagnitude - 60)" which
       tries to apply a non-function to the Quantity argument. *)
    VerificationTest[
      Abs[(GeoDistance[GeoPosition[mk[gp1]["point"]],
           GeoPosition[cnAdvanceLOP[mk[gp1], 0.0, 60.0]["point"]]]/
           Quantity[1, "NauticalMiles"] // QuantityMagnitude) - 60] < 1,
      True, TestID -> "advance-distance"],

    (* cocked-hat-zero: cocked hat of perfect (noise-free) LOPs has ~0 area.
       Tolerance 1.0 nm^2 (generous for linearization residuals near truth). *)
    VerificationTest[
      cnCockedHat[{mk[gp1], mk[gp2], mk[gp3]}]["areaNm2"] < 1.0,
      True, TestID -> "cocked-hat-zero"]
   }]],

 (* --- Task 6: Sight generation + Monte-Carlo error model --- *)
 With[{t = DateObject[{2024, 11, 15, 14, 30, 0}, TimeZone -> 0], truth = {20.0, -40.0}},
  {
   (* zero-error sight reduces to an LOP passing through the truth.
      FIX vs brief: wrapped "(... // QuantityMagnitude) + 1" in parens before the
      comparison: // has lower precedence than <, so without outer parens WL would
      apply QuantityMagnitude to the comparison boolean. *)
   VerificationTest[
     Module[{hs = cnGenerateSight[truth, t, <|"sigmaMin" -> 0|>], lop},
       lop = cnReduceSight[hs, t, truth + {0.3, -0.4}, <||>];
       Abs[lop["interceptNm"]] < (GeoDistance[GeoPosition[truth],
         GeoPosition[truth + {0.3, -0.4}]]/
         Quantity[1, "NauticalMiles"] // QuantityMagnitude) + 1],
     True, TestID -> "zero-error-through-truth"],
   (* Monte-Carlo CEP grows with sigma: 2' noise gives CEP within 0.5..6 nm *)
   VerificationTest[
     0.5 < cnMonteCarloFix[truth, {t, DateObject[{2024, 11, 15, 16, 0, 0}, TimeZone -> 0]},
             <|"sigmaMin" -> 2.0, "seed" -> 1|>, 200]["cep"] < 6.0,
     True, TestID -> "mc-cep-range"]
  }]

]
