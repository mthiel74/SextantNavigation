(* proper_motion_tests.wl — A2: stellar proper motion (J2000 -> date)
   Strategy: isolate PM effect by comparing GP positions at the SAME epoch (t2024)
   computed (a) via cnBodyGPFor (applies PM then precesses) vs
             (b) via cnBodyGP[cnStarRADec[name], t] (precesses J2000 coords, no PM).
   The difference between (a) and (b) is purely the proper motion shift.

   Hipparcos proper motions in the CSV (arcsec/yr):
     Arcturus:         pmRA = -1.093, pmDec = -1.999  -> total ~2.278 "/yr
     Vega:             pmRA =  0.201, pmDec =  0.286  -> total ~0.350 "/yr
     Rigil Kentaurus:  pmRA = -3.678, pmDec =  0.482  -> total ~3.710 "/yr
     Polaris:          pmRA =  0.044, pmDec = -0.012  -> total ~0.046 "/yr

   dt at t2024 (2024-07-02 12:00 UTC from J2000.0): ~24.50 years.

   Expected PM shifts (deg) at t2024:
     Arcturus:         total ~0.01551 deg (= 55.8")
     Vega:             total ~0.00238 deg (=  8.6")
     Polaris:          total ~0.00031 deg (=  1.1")
     Rigil Kentaurus:  total ~0.02527 deg (= 90.9")
*)

(* Helper: angular separation in degrees between two {dec, lonGP} GP points *)
pmGPSep[{dec1_, lon1_}, {dec2_, lon2_}] :=
  ArcCos[Clip[Sin[dec1 Degree] Sin[dec2 Degree] +
              Cos[dec1 Degree] Cos[dec2 Degree] Cos[(lon1 - lon2) Degree],
              {-1, 1}]] / Degree;

{
  (* 1. Arcturus GP shift due to PM: ~55.8" = ~0.0155 deg
        Compare: (a) cnBodyGPFor (with PM) vs (b) cnBodyGP[J2000 coords] (without PM)
        at the SAME epoch t2024. *)
  VerificationTest[
    Module[{t2024 = DateObject[{2024, 7, 2, 12, 0, 0}, TimeZone -> 0],
            gpPM, gpNoPM, shiftDeg},
      gpPM   = cnBodyGPFor[{"Star", "Arcturus"}, t2024];
      gpNoPM = cnBodyGP[cnStarRADec["Arcturus"], t2024];
      shiftDeg = pmGPSep[gpPM, gpNoPM];
      0.010 < shiftDeg < 0.025],
    True,
    TestID -> "pm-arcturus-gp-shift-magnitude"],

  (* 2. Arcturus declination shift due to PM is close to pmDec * dt / 3600.
        Expected: -1.999 * 24.5016 / 3600 ~ -0.01361 deg.
        Test: |shift - expected| < 0.003 deg (tolerance covers small cross-terms). *)
  VerificationTest[
    Module[{t2024 = DateObject[{2024, 7, 2, 12, 0, 0}, TimeZone -> 0],
            gpPM, gpNoPM, dt, pmDec, expectedDecShift, actualDecShift},
      gpPM   = cnBodyGPFor[{"Star", "Arcturus"}, t2024];
      gpNoPM = cnBodyGP[cnStarRADec["Arcturus"], t2024];
      dt = (JulianDate[t2024] - 2451545.0) / 365.25;   (* ~ 24.50 yr *)
      pmDec = cnLoadStars[][ "Arcturus"]["pmDec"];        (* -1.999 arcsec/yr *)
      expectedDecShift = pmDec * dt / 3600.0;            (* ~ -0.01361 deg *)
      actualDecShift   = gpPM[[1]] - gpNoPM[[1]];
      Abs[actualDecShift - expectedDecShift] < 0.003],
    True,
    TestID -> "pm-arcturus-dec-shift-matches-expected"],

  (* 3. Vega shifts much less than Arcturus (total PM ~ 0.350 vs 2.278 "/yr).
        Expected Vega shift: ~0.00238 deg; Arcturus: ~0.0155 deg.
        Test: Vega shift < Arcturus shift / 3.0 (safely below, ratio ~ 0.154). *)
  VerificationTest[
    Module[{t2024 = DateObject[{2024, 7, 2, 12, 0, 0}, TimeZone -> 0],
            arctShift, vegaShift},
      arctShift = pmGPSep[cnBodyGPFor[{"Star","Arcturus"}, t2024],
                           cnBodyGP[cnStarRADec["Arcturus"], t2024]];
      vegaShift = pmGPSep[cnBodyGPFor[{"Star","Vega"}, t2024],
                           cnBodyGP[cnStarRADec["Vega"], t2024]];
      vegaShift < arctShift / 3.0 && vegaShift < 0.005],
    True,
    TestID -> "pm-vega-shifts-less-than-arcturus"],

  (* 4. Polaris: total PM ~0.046 "/yr -> shift over 24.5 yr ~1.1" = 0.0003 deg.
        Test: shift < 0.002 deg (well above sensor noise floor, safely below Arcturus). *)
  VerificationTest[
    Module[{t2024 = DateObject[{2024, 7, 2, 12, 0, 0}, TimeZone -> 0]},
      pmGPSep[cnBodyGPFor[{"Star","Polaris"}, t2024],
               cnBodyGP[cnStarRADec["Polaris"], t2024]] < 0.002],
    True,
    TestID -> "pm-polaris-negligible-shift"],

  (* 5. Rigil Kentaurus: largest PM in catalogue (~3.71 "/yr -> 0.025 deg over 24.5 yr).
        Test: shift > 0.018 deg (comfortably above Arcturus's ~0.0155 deg). *)
  VerificationTest[
    Module[{t2024 = DateObject[{2024, 7, 2, 12, 0, 0}, TimeZone -> 0]},
      pmGPSep[cnBodyGPFor[{"Star","Rigil Kentaurus"}, t2024],
               cnBodyGP[cnStarRADec["Rigil Kentaurus"], t2024]] > 0.018],
    True,
    TestID -> "pm-rigil-kentaurus-largest-shift"],

  (* 6. GP self-consistency: body at its own PM-adjusted GP is at the zenith.
        Tests that cnBodyGPFor still produces a valid GP after PM application. *)
  VerificationTest[
    Module[{t2024 = DateObject[{2024, 7, 2, 12, 0, 0}, TimeZone -> 0],
            gp},
      gp = cnBodyGPFor[{"Star", "Arcturus"}, t2024];
      Abs[cnAltitudeFromGP[gp, gp] - 90] < 1*^-4],
    True,
    TestID -> "pm-gp-self-consistency-arcturus"],

  VerificationTest[
    Module[{t2024 = DateObject[{2024, 7, 2, 12, 0, 0}, TimeZone -> 0],
            gp},
      gp = cnBodyGPFor[{"Star", "Sirius"}, t2024];
      Abs[cnAltitudeFromGP[gp, gp] - 90] < 1*^-4],
    True,
    TestID -> "pm-gp-self-consistency-sirius"],

  (* 7. Zero-noise 3-star fix roundtrip: generate and reduce use the same PM path
        via cnBodyGPFor, so the roundtrip should close to < 0.5 nm. *)
  VerificationTest[
    Module[{truePos = {15.0, -40.0},
            t = DateObject[{2024, 7, 2, 12, 0, 0}, TimeZone -> 0],
            ap = {15.1, -40.1},
            stars = {{"Star","Sirius"}, {"Star","Arcturus"}, {"Star","Vega"}},
            lops, fix, errNm},
      lops = Table[
        cnReduceSightBody[
          cnGenerateSightBody[truePos, t, s, <|"sigmaMin"->0|>],
          t, ap, s],
        {s, stars}];
      fix = cnFix[lops];
      errNm = QuantityMagnitude[GeoDistance[GeoPosition[truePos], GeoPosition[fix]] /
                                Quantity[1, "NauticalMiles"]];
      errNm < 0.5],
    True,
    TestID -> "pm-three-star-fix-roundtrip"]
}
