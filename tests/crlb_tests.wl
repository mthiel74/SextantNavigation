(* crlb_tests.wl -- Fisher information and Cramer-Rao lower bound (Item 11)
   Three tests:
     1. Optimal geometry  : 3 azimuths 120 deg apart -> cov = (2/3)I, semi-axes = Sqrt[2/3]
     2. MC efficiency     : Sun fix (3 sights, sigma=1.5') → empirical cov eigenvalues and
                            CEP within 25% of CRLB (n=1500 seeded trials)
     3. GDOP cross-check  : Sqrt[Tr[cnCRLBCovariance[az,1]]] = cnGDOPFromAzimuths[az]
*)

{
  (* ── Test 1: Optimal geometry ────────────────────────────────────────────────
     Three azimuths 120 deg apart: u_i sum gives M = (3/2)I.
     F = (1/1^2)(3/2)I  =>  C = (2/3)I  =>  both semi-axes = Sqrt[2/3] ~ 0.8165 nm.
     Tolerance 1e-3 (well within floating-point rounding). *)
  VerificationTest[
    Module[{cov, ell},
      cov = cnCRLBCovariance[{0., 120., 240.}, 1.0];
      ell = cnErrorEllipse[cov];
      Max[Abs[ell["semiMajorNm"] - Sqrt[2./3.]],
          Abs[ell["semiMinorNm"] - Sqrt[2./3.]]] < 1*^-3
    ],
    True,
    TestID -> "crlb-optimal-geometry"],

  (* ── Test 2: MC efficiency ───────────────────────────────────────────────────
     Three Sun sights at {20 N, 40 W} on 2024-Nov-15 at 10:00, 14:00, 17:00 UTC.
     sigma = 1.5 arcmin.  n = 1500 seeded trials.
     Empirical covariance eigenvalues within 25% of CRLB eigenvalues;
     empirical CEP (Quantile[errs, 0.5]) within 25% of CRLB CEP. *)
  VerificationTest[
    Module[{truePos, t1, t2, t3, times, sigma,
            mcResult, azimuths, cov, crlbEll, mcEll,
            crlbEigens, mcEigens, eigOK, cepOK},
      truePos = {20., -40.};
      t1 = DateObject[{2024, 11, 15, 10, 0, 0}, TimeZone -> 0];
      t2 = DateObject[{2024, 11, 15, 14, 0, 0}, TimeZone -> 0];
      t3 = DateObject[{2024, 11, 15, 17, 0, 0}, TimeZone -> 0];
      times  = {t1, t2, t3};
      sigma  = 1.5;
      (* MC run — seeded for reproducibility *)
      mcResult = cnMonteCarloFix[truePos, times,
                   <|"sigmaMin" -> sigma, "seed" -> 42|>, 1500];
      (* CRLB using same azimuths as the MC reduction path *)
      azimuths = cnComputedAltitude[truePos, cnSunGP[#]][[2]] & /@ times;
      cov      = cnCRLBCovariance[azimuths, sigma];
      crlbEll  = cnErrorEllipse[cov];
      mcEll    = cnErrorEllipse[mcResult["covNm"]];
      (* Compare eigenvalues (variance, nm^2) in descending order *)
      crlbEigens = Sort[{crlbEll["semiMajorNm"]^2, crlbEll["semiMinorNm"]^2}, Greater];
      mcEigens   = Sort[{mcEll["semiMajorNm"]^2,   mcEll["semiMinorNm"]^2},   Greater];
      eigOK = Max[Abs[mcEigens / crlbEigens - 1.]] < 0.25;
      cepOK = Abs[mcResult["cep"] / crlbEll["cep"] - 1.] < 0.25;
      eigOK && cepOK
    ],
    True,
    TestID -> "crlb-mc-efficiency"],

  (* ── Test 3: GDOP cross-check ────────────────────────────────────────────────
     At sigma=1 arcmin, Sqrt[Tr[C]] = Sqrt[Tr[M^{-1}]] = cnGDOPFromAzimuths.
     cnCRLBCovariance[az,1] = M^{-1} because F = (1/1^2)M = M.
     Agreement to machine precision (< 1e-10). *)
  VerificationTest[
    Module[{az = {30., 150., 270.}},
      Abs[Sqrt[Tr[cnCRLBCovariance[az, 1.0]]] - cnGDOPFromAzimuths[az]] < 1*^-10
    ],
    True,
    TestID -> "crlb-gdop-crosscheck"]
}
