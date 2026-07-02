(* tests/historical_tests.wl -- external/documented validation (R2) *)
(* Bare List of VerificationTests so Get[] composes with the other test files
   in run_tests.wls.  Part A reproduces a worked reduction and checks it both
   internally (independent 3-D vector formula -> exact) and externally (Wolfram
   SunPosition -> agreement < 1').  Part B reconstructs the James Caird leg and
   checks recovered-landfall error < 10 nm.  All numbers recomputed here with a
   fixed seed so the file is self-contained. *)

Module[
  {tA, gpA, decA, ghaA, apA, hcA, znA, hoA, pA, vecAltAz, hcV, znV,
   dHcVec, dZnVec, spA, spAlt, spAz, hcApp, dAltExt, dAzExt,
   elephant, khb, depart, nDays, sigma, gcDistNm, slerp, results,
   truthTrack, fixTrack, errs, fixRMS, maxErr, landfallFix, landfallErr},

  (* ===== PART A ===== *)
  tA   = DateObject[{1916, 5, 2, 15, 0, 0}, TimeZone -> 0];
  gpA  = cnSunGP[tA];
  decA = gpA[[1]]; ghaA = Mod[-gpA[[2]], 360];
  apA  = {-57.0, -46.0};
  {hcA, znA} = cnComputedAltitude[apA, gpA];
  hoA = 17.6500; pA = cnIntercept[hoA, hcA];

  (* independent 3-D direction-cosine (ENU) derivation of altitude/azimuth *)
  vecAltAz[{lat_, lon_}, {dec_, lonGP_}] := Module[{up, east, north, b},
    up    = {Cos[lat Degree] Cos[lon Degree], Cos[lat Degree] Sin[lon Degree], Sin[lat Degree]};
    east  = {-Sin[lon Degree], Cos[lon Degree], 0};
    north = {-Sin[lat Degree] Cos[lon Degree], -Sin[lat Degree] Sin[lon Degree], Cos[lat Degree]};
    b     = {Cos[dec Degree] Cos[lonGP Degree], Cos[dec Degree] Sin[lonGP Degree], Sin[dec Degree]};
    {ArcSin[b . up]/Degree, Mod[ArcTan[b . north, b . east]/Degree, 360]}];
  {hcV, znV} = vecAltAz[apA, gpA];
  dHcVec = Abs[hcA - hcV];                  (* degrees *)
  dZnVec = Abs[znA - znV];

  (* external authority: Wolfram SunPosition (apparent, refracted) *)
  spA   = SunPosition[GeoPosition[apA], tA];
  spAlt = QuantityMagnitude[UnitConvert[spA[[2]], "AngularDegrees"]];
  spAz  = QuantityMagnitude[UnitConvert[spA[[1]], "AngularDegrees"]];
  hcApp = hcA + cnRefraction[hcA]/60;
  dAltExt = Abs[hcApp - spAlt] 60;          (* arcmin *)
  dAzExt  = Abs[znA - spAz] 60;

  (* ===== PART B ===== *)
  elephant = {-61.05, -55.21};
  khb      = {-54.17, -37.30};
  depart   = DateObject[{1916, 4, 24}, TimeZone -> 0];
  nDays    = 16; sigma = 2.0;
  gcDistNm = QuantityMagnitude[
    GeoDistance[GeoPosition[elephant], GeoPosition[khb]]/Quantity[1, "NauticalMiles"]];
  slerp[p0_, p1_, f_] := Module[{v0, v1, om, v},
    v0 = {Cos[p0[[1]] Degree] Cos[p0[[2]] Degree], Cos[p0[[1]] Degree] Sin[p0[[2]] Degree], Sin[p0[[1]] Degree]};
    v1 = {Cos[p1[[1]] Degree] Cos[p1[[2]] Degree], Cos[p1[[1]] Degree] Sin[p1[[2]] Degree], Sin[p1[[1]] Degree]};
    om = ArcCos[Clip[v0 . v1, {-1, 1}]];
    v  = (Sin[(1 - f) om] v0 + Sin[f om] v1)/Sin[om];
    {ArcSin[v[[3]]]/Degree, ArcTan[v[[1]], v[[2]]]/Degree}];

  SeedRandom[1916];
  results = Table[
    Module[{f, tp, date, noon, lops, fix, err},
      f = k/nDays; tp = slerp[elephant, khb, N@f];
      date = DatePlus[depart, {k, "Day"}];
      noon = cnLANTimeUTC[tp[[2]], date];
      lops = Table[
        Module[{ti, hs},
          ti = DatePlus[noon, {off, "Hour"}];
          hs = cnGenerateSightBody[tp, ti, "Sun", <|"sigmaMin" -> sigma|>];
          cnReduceSightBody[hs, ti, tp, "Sun"]], {off, {-3., 0., 3.}}];
      fix = cnFix[lops];
      err = QuantityMagnitude[GeoDistance[GeoPosition[tp], GeoPosition[fix]]/Quantity[1, "NauticalMiles"]];
      <|"truePos" -> tp, "fix" -> fix, "errNm" -> err|>],
    {k, 0, nDays}];
  fixTrack    = #["fix"]   & /@ results;
  errs        = #["errNm"] & /@ results;
  fixRMS      = Sqrt[Mean[errs^2]];
  maxErr      = Max[errs];
  landfallFix = fixTrack[[-1]];
  landfallErr = QuantityMagnitude[
    GeoDistance[GeoPosition[landfallFix], GeoPosition[khb]]/Quantity[1, "NauticalMiles"]];

  {
    (* === Part A: reduction trigonometry is internally exact === *)
    VerificationTest[dHcVec < 1.*^-6, True, TestID -> "partA-Hc-exact-vs-vector"],
    VerificationTest[dZnVec < 1.*^-6, True, TestID -> "partA-Zn-exact-vs-vector"],

    (* === Part A: full Sun pipeline matches an independent authority < 1' === *)
    VerificationTest[dAltExt < 1.0, True, TestID -> "partA-alt-vs-sunposition-under-1arcmin"],
    VerificationTest[dAzExt  < 1.0, True, TestID -> "partA-az-vs-sunposition-under-1arcmin"],

    (* === Part A: intercept computes correctly (Ho > Hc => Toward, +) === *)
    VerificationTest[pA > 0 && Abs[pA - (hoA - hcA) 60] < 1.*^-9,
      True, TestID -> "partA-intercept-toward"],

    (* === Part B: documented great-circle leg is ~700-800 nm === *)
    VerificationTest[650 < gcDistNm < 850, True, TestID -> "partB-leg-length-realistic"],

    (* === Part B: recovered landfall is within a realistic error === *)
    VerificationTest[landfallErr < 10.0, True, TestID -> "partB-landfall-under-10nm"],

    (* === Part B: the method tracks the whole documented leg, not just the end === *)
    VerificationTest[fixRMS < 6.0,  True, TestID -> "partB-track-rms-under-6nm"],
    VerificationTest[maxErr < 10.0, True, TestID -> "partB-max-daily-under-10nm"]
  }
]
