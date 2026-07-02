(* tests/lunar_tests.wl — Lunar-distance method: GMT (hence longitude) without a chronometer.
   The Moon moves ~0.5 deg/hour against the Sun, so the cleared geocentric Moon-Sun distance
   is a celestial clock.  Verifies:
     0. cnMoonPosition agrees with JPL/Wolfram apparent geocentric RA/Dec to <1 arcmin,
        and HP is in the physical 54-61' range.
     1. Self-consistency: D_geo(t0) inverted from t0+2h recovers t0 to << 1 min.
     2. Clearing: a simulated topocentric distance (parallax+refraction added) cleared by
        cnClearLunarDistance returns the geocentric distance to < 0.2 arcmin.
     3. Rate sanity: d(D)/dt is ~0.45 deg/hr (assert 0.4-0.55).
     4. End-to-end longitude: simulate a 1' lunar observation, clear + invert to GMT,
        then a Sun altitude sight (Sun on the prime vertical) gives longitude within ~0.5 deg
        of truth — the historical lunar accuracy, ~30-60x worse than the chronometer (Item 12).
*)

Join[

  (* ── Test 0b: Meeus Example 47.a (1992-04-12 0h) — self-proving offline anchor ─ *)
  (* Meeus, Astronomical Algorithms, Ex. 47.a: apparent geocentric RA = 134.68847 deg,
     Dec = +13.768368 deg, distance = 368409.7 km. Our abridged-ELP cnMoonPosition
     reproduces it to ~0.002', proving the lunar theory against a primary reference. *)
  {VerificationTest[
     With[{m = cnMoonPosition[DateObject[{1992, 4, 12, 0, 0, 0}, TimeZone -> 0]]},
       Abs[m[[1]] - 134.68847] < 0.002 && Abs[m[[2]] - 13.768368] < 0.002 &&
       Abs[m[[3]] - 368409.7] < 5.0],
     True, TestID -> "moon-meeus-47a"]},

  (* ── Test 0: Moon-position accuracy and HP sanity ───────────────────────────── *)
  (* Reference (JPL/Wolfram apparent geocentric, TETE) for 2024-06-15 12:00 UTC:
     RA = 188.19498 deg, Dec = -3.14062 deg.  cnMoonPosition omits lunar aberration,
     so the residual is ~0.5'; assert < 1' on both coordinates. *)
  With[{tRef = DateObject[{2024, 6, 15, 12, 0, 0}, TimeZone -> 0],
        raRef = 188.19498, decRef = -3.14062},
    Module[{mp, dRaMin, dDecMin},
      mp = cnMoonPosition[tRef];
      dRaMin  = (mp[[1]] - raRef) Cos[decRef Degree] * 60;
      dDecMin = (mp[[2]] - decRef) * 60;
      {
        VerificationTest[Abs[dRaMin] < 1.0,  True, TestID -> "lunar-moonpos-ra-1arcmin"],
        VerificationTest[Abs[dDecMin] < 1.0, True, TestID -> "lunar-moonpos-dec-1arcmin"],
        (* HP in the physical range 54-61 arcmin (0.90-1.02 deg) *)
        VerificationTest[54.0 < cnMoonHP[tRef] * 60 < 61.0, True, TestID -> "lunar-moon-hp-range"],
        (* cnMoonGP declination equals cnMoonPosition declination *)
        VerificationTest[Abs[cnMoonGP[tRef][[1]] - mp[[2]]] < 1*^-6, True, TestID -> "lunar-moongp-dec-matches"]
      }
    ]
  ],

  (* ── Test 1: self-consistency (recover GMT to << 1 minute) ──────────────────── *)
  With[{t0 = DateObject[{2024, 6, 13, 18, 0, 0}, TimeZone -> 0]},
    Module[{dGeo, tRec, errSec},
      dGeo = cnLunarDistanceGeocentric[t0];
      tRec = cnLunarDistanceGMT[dGeo, DatePlus[t0, {2, "Hour"}]];
      errSec = Abs[AbsoluteTime[tRec] - AbsoluteTime[t0]];
      {
        VerificationTest[errSec < 60.0, True, TestID -> "lunar-self-consistency-1min"],
        (* and in fact essentially exact (< 1 s) since forward and inverse share the ephemeris *)
        VerificationTest[errSec < 1.0, True, TestID -> "lunar-self-consistency-1sec"]
      }
    ]
  ],

  (* ── Test 3: rate sanity (Moon-Sun distance changes ~0.5 deg/hr) ─────────────── *)
  With[{t0 = DateObject[{2024, 6, 13, 18, 0, 0}, TimeZone -> 0]},
    Module[{rate},
      rate = Abs[cnLunarDistanceGeocentric[DatePlus[t0, {1, "Hour"}]] - cnLunarDistanceGeocentric[t0]];
      {
        VerificationTest[0.4 < rate < 0.55, True, TestID -> "lunar-rate-half-deg-per-hour"]
      }
    ]
  ],

  (* ── Test 2: clearing returns the geocentric distance ───────────────────────── *)
  (* Observer {35 N, 40 W}; both bodies ~45 deg up at 2024-06-13 18:00 UTC.  Simulate a
     topocentric center-to-center observation: the azimuth difference A is computed from the
     geocentric altitudes, the apparent altitudes are found by a fixed-point that inverts
     the clearing conventions (geocentric = apparent + parallax - refraction), and the
     observed distance follows.  Clearing should return D_geo to < 0.2 arcmin. *)
  With[{pos = {35.0, -40.0}, t0 = DateObject[{2024, 6, 13, 18, 0, 0}, TimeZone -> 0]},
    Module[{appAltMoon, appAltSun, mGP, sGP, hpM, hMg, hSg, dGeo, cosA, hMa, hSa, dObs, dClr},
      appAltMoon[hg_, hp_] := Module[{h = hg}, Do[h = hg - ArcSin[Sin[hp Degree] Cos[h Degree]]/Degree + cnRefraction[h]/60., 5]; h];
      appAltSun[hg_]       := Module[{h = hg}, Do[h = hg - 0.002443 Cos[h Degree] + cnRefraction[h]/60., 5]; h];
      mGP = cnMoonGP[t0]; sGP = cnSunGP[t0]; hpM = cnMoonHP[t0];
      hMg = cnAltitudeFromGP[pos, mGP];  hSg = cnAltitudeFromGP[pos, sGP];
      dGeo = cnLunarDistanceGeocentric[t0];
      cosA = (Cos[dGeo Degree] - Sin[hMg Degree] Sin[hSg Degree]) / (Cos[hMg Degree] Cos[hSg Degree]);
      hMa = appAltMoon[hMg, hpM];  hSa = appAltSun[hSg];
      dObs = ArcCos[Clip[Sin[hMa Degree] Sin[hSa Degree] + Cos[hMa Degree] Cos[hSa Degree] cosA, {-1, 1}]]/Degree;
      dClr = cnClearLunarDistance[dObs, hMa, hSa, hpM];
      {
        (* the topocentric correction is sizeable (Moon parallax dominant): D_obs - D_geo > 0.3 deg *)
        VerificationTest[(dObs - dGeo) > 0.3, True, TestID -> "lunar-clearing-correction-nonzero"],
        VerificationTest[Abs[dClr - dGeo] * 60 < 0.2, True, TestID -> "lunar-clearing-0.2arcmin"]
      }
    ]
  ],

  (* ── Test 4: end-to-end longitude (historical lunar accuracy ~0.5 deg) ───────── *)
  (* Truth {35 N, 40 W} at 2024-06-13 18:00 UTC.  Sun bears ~269 deg (prime vertical) ->
     altitude gives longitude well.  Add 1' Gaussian noise to the lunar distance (a realistic
     cleared-lunar error), recover GMT, then solve a Sun altitude sight at known latitude.
     Expect ~0.5 deg longitude error: 1' distance / (0.45 deg/hr) ~ 2 min GMT -> ~0.5 deg lon.
     This is ~30-60x worse than the chronometer (Item 12: 4 s -> 1 nm), the historical point. *)
  With[{truePos = {35.0, -40.0}, tTrue = DateObject[{2024, 6, 13, 18, 0, 0}, TimeZone -> 0]},
    Module[{appAltMoon, appAltSun, mGP, sGP, hpM, hMg, hSg, dGeo, cosA, hMa, hSa, dObs,
            dNoisy, dClr, tRec, gmtErrMin, hs, ho, sGPrec, lonSol, lonErr},
      appAltMoon[hg_, hp_] := Module[{h = hg}, Do[h = hg - ArcSin[Sin[hp Degree] Cos[h Degree]]/Degree + cnRefraction[h]/60., 5]; h];
      appAltSun[hg_]       := Module[{h = hg}, Do[h = hg - 0.002443 Cos[h Degree] + cnRefraction[h]/60., 5]; h];
      mGP = cnMoonGP[tTrue]; sGP = cnSunGP[tTrue]; hpM = cnMoonHP[tTrue];
      hMg = cnAltitudeFromGP[truePos, mGP];  hSg = cnAltitudeFromGP[truePos, sGP];
      dGeo = cnLunarDistanceGeocentric[tTrue];
      cosA = (Cos[dGeo Degree] - Sin[hMg Degree] Sin[hSg Degree]) / (Cos[hMg Degree] Cos[hSg Degree]);
      hMa = appAltMoon[hMg, hpM];  hSa = appAltSun[hSg];
      dObs = ArcCos[Clip[Sin[hMa Degree] Sin[hSa Degree] + Cos[hMa Degree] Cos[hSa Degree] cosA, {-1, 1}]]/Degree;
      SeedRandom[42];
      dNoisy = dObs + RandomVariate[NormalDistribution[0, 1.0/60.]];   (* 1 arcmin lunar noise *)
      dClr = cnClearLunarDistance[dNoisy, hMa, hSa, hpM];
      tRec = cnLunarDistanceGMT[dClr, DatePlus[tTrue, {1.5, "Hour"}]];
      gmtErrMin = Abs[AbsoluteTime[tRec] - AbsoluteTime[tTrue]] / 60.;
      (* Sun altitude sight at the true time; reduce with the recovered GMT and known latitude *)
      hs = cnGenerateSight[truePos, tTrue, <|"sigmaMin" -> 0|>];
      ho = cnObservedAltitude[hs, 2.0, 0.0, "Lower"];
      sGPrec = cnSunGP[tRec];
      lonSol = lon /. FindRoot[cnAltitudeFromGP[{truePos[[1]], lon}, sGPrec] - ho, {lon, -35.0, -45.0}];
      lonErr = Abs[lonSol - truePos[[2]]];
      {
        (* GMT recovered to a few minutes from 1' lunar noise *)
        VerificationTest[gmtErrMin < 5.0, True, TestID -> "lunar-e2e-gmt-few-min"],
        (* longitude recovered to ~0.5 deg — the historical lunar accuracy *)
        VerificationTest[lonErr < 1.0, True, TestID -> "lunar-e2e-longitude-half-deg"],
        (* and it IS the historical regime: error well above the chronometer floor (< 0.05 deg) *)
        VerificationTest[lonErr > 0.1, True, TestID -> "lunar-e2e-worse-than-chronometer"]
      }
    ]
  ]

]
