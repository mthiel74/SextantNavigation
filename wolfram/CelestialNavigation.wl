(* ::Package:: *)
BeginPackage["CelestialNavigation`"];

cnVersion::usage = "cnVersion[] gives the package version string.";

cnDip::usage = "cnDip[heightMeters] gives the dip of the horizon in arcminutes (positive).";
cnRefraction::usage = "cnRefraction[hApparentDeg] gives atmospheric refraction in arcminutes (positive), Bennett's formula.";
cnRefractionSaemundsson::usage = "cnRefractionSaemundsson[hDeg] gives atmospheric refraction in arcminutes from the true altitude hDeg using Saemundsson (1986): 1.02 cot(h + 10.3/(h+5.11)).";
cnRefractionSimple::usage = "cnRefractionSimple[hDeg] gives atmospheric refraction in arcminutes using the simple/Smart model: 0.96 cot(h). Valid well above the horizon; diverges near 0\[Degree].";
cnRefractionPT::usage = "cnRefractionPT[hDeg, pressureHPa, tempC] gives Bennett refraction scaled by the air-density factor (P/1010)*(283/(273+T)). Standard conditions P=1010 hPa, T=10\[Degree]C reproduce cnRefraction[h].";
cnSunSemidiameter::usage = "cnSunSemidiameter[] gives the mean solar semi-diameter in arcminutes (16.0).";
cnSunSemidiameterAt::usage = "cnSunSemidiameterAt[t] gives the solar semi-diameter in arcminutes at UTC time t, scaled by (1/r) where r is the Sun-Earth distance in AU from the low-precision formula r = 1.00014 - 0.01671 Cos[g] - 0.00014 Cos[2g] (g = mean anomaly). Ranges ~15.7-16.3'.  cnSunSemidiameter[] (constant 16.0) is retained for backward compatibility.";
cnSunParallax::usage = "cnSunParallax[hDeg] gives parallax in altitude in arcminutes.";
cnObservedAltitude::usage = "cnObservedAltitude[hsDeg, heightMeters, indexErrorMin, limb] gives the observed altitude Ho in degrees. limb is \"Lower\", \"Upper\", or \"Center\".";

cnSunAltitude::usage = "cnSunAltitude[{latDeg, lonDeg}, time] gives the true geometric Sun altitude in degrees. time is a DateObject with TimeZone -> 0.";
cnSunAzimuth::usage = "cnSunAzimuth[{latDeg, lonDeg}, time] gives the true Sun azimuth in degrees (0-360, from North). time is a DateObject with TimeZone -> 0.";
cnSunGP::usage = "cnSunGP[time] gives {decDeg, lonGPDeg}, the sub-solar point (GP): latitude = solar declination, longitude where Sun is overhead (East positive, -180..180). time is a DateObject with TimeZone -> 0.";

cnComputedAltitude::usage = "cnComputedAltitude[{latAP, lonAP}, {decGP, lonGP}] gives {HcDeg, ZnDeg}, the calculated altitude and azimuth at the assumed position for a body whose GP is {decGP, lonGP}. Uses the Marcq St-Hilaire intercept method with east-positive LHA convention.";
cnIntercept::usage = "cnIntercept[hoDeg, hcDeg] gives the intercept in nautical miles ((Ho-Hc)*60). Positive = Toward.";
cnLOP::usage = "cnLOP[{latAP,lonAP}, {decGP,lonGP}, hoDeg] gives an Association with keys AP, Hc, Zn, Ho, interceptNm, point {lat,lon}, bearingDeg for the line of position.";

cnAltitudeFromGP::usage = "cnAltitudeFromGP[{latDeg,lonDeg}, {decDeg,lonGPDeg}] gives the geometric altitude in degrees of a body whose GP is {decDeg,lonGPDeg} seen from {latDeg,lonDeg}. Uses the spherical law of cosines. Same formula as cnComputedAltitude so noise-free tests close exactly.";
cnFix::usage = "cnFix[lops] gives {latDeg,lonDeg}, the least-squares LOP intersection in a local tangent plane (nm). lops is a list of associations from cnLOP.";
cnAdvanceLOP::usage = "cnAdvanceLOP[lop, courseDeg, distNm] returns a copy of the LOP association with \"point\" advanced by distNm along courseDeg (transferred position line for a running fix).";
cnRunningFix::usage = "cnRunningFix[items] gives {latDeg,lonDeg}. Each element of items is <|\"lop\"->, \"course\"->, \"distance\"->|>; LOPs are advanced then fixed by cnFix.";
cnCockedHat::usage = "cnCockedHat[lops] gives <|\"vertices\"->{{lat,lon}..}, \"areaNm2\"->_|>, the triangle of three pairwise two-LOP intersections and its area in nm\[CenterDot]nm.";

cnGenerateSight::usage = "cnGenerateSight[truePos, t, opts] generates a simulated sextant altitude Hs (degrees) by computing the true altitude via cnAltitudeFromGP and inverting the sight corrections, then adding Gaussian noise. opts: \"heightMeters\", \"indexErrorMin\", \"limb\", \"sigmaMin\" (noise in arcmin, default 1.0), \"seed\".";
cnReduceSight::usage = "cnReduceSight[hs, t, ap, opts] applies cnObservedAltitude to the sextant altitude Hs, then builds and returns a cnLOP association from assumed position ap using cnSunGP[t].";
cnMonteCarloFix::usage = "cnMonteCarloFix[truePos, times, opts, n] runs n Monte-Carlo trials — each generating sights at every time in times and computing a running fix — and returns <|\"fixes\"->{{lat,lon}..}, \"errorsNm\"->{..}, \"cep\"->_, \"covNm\"->_|>. opts: \"sigmaMin\", \"seed\".";

cnLoadStars::usage = "cnLoadStars[] returns an Association name -> <|\"raDeg\", \"decDeg\", \"mag\"|> of all 58 navigational stars, loaded from data/nav_stars.csv relative to the package file. Result is cached after first call.";
cnStarRADec::usage = "cnStarRADec[name] returns {raDeg, decDeg} (J2000.0) for the named navigational star.";
cnBodyGP::usage = "cnBodyGP[{raDeg, decDeg}, t] returns {decDeg, lonGPDeg} — the geographic position of a body with J2000.0 coordinates {raDeg,decDeg} at UTC time t. Coordinates are first precessed from J2000.0 to t using IAU-1976 precession. GHA = Mod[cnGMST[t] - precessed_raDeg, 360]; lonGP = Mod[-GHA + 180, 360] - 180.";
cnPrecess::usage = "cnPrecess[{raDeg, decDeg}, t] precesses J2000.0 equatorial coordinates to the epoch of DateObject t using IAU-1976 precession.";

cnBodyGPFor::usage = "cnBodyGPFor[body, t] returns {decDeg, lonGPDeg} for body at UTC time t. body is \"Sun\" (uses cnSunGP) or {\"Star\", name} (uses cnBodyGP[cnStarRADec[name], t]).";
cnObservedAltitudeBody::usage = "cnObservedAltitudeBody[hsDeg, heightMeters, indexErrorMin, body] gives Ho applying body-appropriate corrections. body is \"Sun\" (Lower limb, SD=16', HP active) or {\"Star\", name} (no SD, no parallax).";
cnGenerateSightBody::usage = "cnGenerateSightBody[truePos, t, body, opts] generates a simulated sextant altitude Hs for body (\"Sun\" or {\"Star\",name}). Inverts body-appropriate corrections and adds Gaussian noise (sigmaMin, default 1.0). opts: \"heightMeters\", \"indexErrorMin\", \"sigmaMin\", \"seed\".";
cnReduceSightBody::usage = "cnReduceSightBody[hs, t, ap, body, opts] reduces a sextant altitude Hs to a LOP using body-appropriate corrections and GP. body is \"Sun\" or {\"Star\",name}. opts: \"heightMeters\", \"indexErrorMin\".";

cnGDOPFromAzimuths::usage = "cnGDOPFromAzimuths[azimuths] gives the GDOP scalar from a list of body azimuths in degrees. Builds the 2x2 Fisher information matrix M = Sum[u_i u_i^T] where u_i = {Sin[Az_i Degree], Cos[Az_i Degree]}, and returns Sqrt[Tr[Inverse[M]]]. For three azimuths 120 deg apart, GDOP = Sqrt[4/3] ~ 1.155 (the optimum). Returns 9999 for near-singular (clustered) configurations.";

cnVisibleStars::usage = "cnVisibleStars[pos, t] or cnVisibleStars[pos, t, minAltDeg] returns a list of navigational star names whose computed altitude at pos exceeds minAltDeg (default 15 deg) at UTC time t. pos = {latDeg, lonDeg}.";

cnFixGDOP::usage = "cnFixGDOP[pos, t, starNames] gives the GDOP scalar for a position fix from the named navigational stars at observer pos and UTC time t. Computes each star's azimuth via cnComputedAltitude and returns cnGDOPFromAzimuths of those azimuths. Returns 9999 for near-singular configurations.";

cnBestStarTriplet::usage = "cnBestStarTriplet[pos, t] or cnBestStarTriplet[pos, t, minAltDeg] returns <|\"stars\"->{n1,n2,n3}, \"gdop\"->_, \"azimuths\"->{Az1,Az2,Az3}|> for the visible-star triplet that minimises GDOP at observer pos and UTC time t. Searches all C(n,3) triplets of the n visible stars above minAltDeg (default 15 deg). If more than 20 stars are visible, restricts the search to the 20 brightest (lowest magnitude) to keep C(n,3) under ~1140. pos = {latDeg, lonDeg}.";

cnPointInTriangle::usage = "cnPointInTriangle[pt, {v1,v2,v3}] returns True if {latDeg,lonDeg} pt lies inside the triangle with {latDeg,lonDeg} vertices v1,v2,v3. Projects all four points to a local East/North tangent plane (nm) centred on the vertex centroid, then uses the sign-of-cross-products test (all three edge-to-point cross products have the same sign iff the point is interior).";

cnCockedHatContains::usage = "cnCockedHatContains[truePos, lops] returns True if {latDeg,lonDeg} truePos lies inside the cocked-hat triangle formed by the three pairwise LOP intersections of lops (three LOP associations from cnLOP or cnReduceSightBody).";

cnCockedHatProbability::usage = "cnCockedHatProbability[truePos, t, bodies, opts, n] runs n Monte-Carlo trials and returns the empirical fraction of trials in which truePos lies inside the cocked-hat triangle formed by noisy LOPs for each body. Each trial generates independent Gaussian noise for each sight (sigmaMin from opts, default 1.5 arcmin). t is a single DateObject applied to all bodies, or a list of DateObjects (one per body). bodies is a list of body specs accepted by cnGenerateSightBody (\"Sun\" or {\"Star\",name}). The 25%% theorem (Daniels 1951) predicts this fraction converges to 0.25 for any three bodies with distinct bearings. opts keys: sigmaMin (default 1.5), seed. Seed is applied once before the loop; individual sight calls are NOT re-seeded.";

cnFisherMatrix::usage = "cnFisherMatrix[azimuthsDeg, sigmaMin] gives the 2x2 Fisher information matrix F = (1/sigmaMin^2) Sum_i u_i u_i^T where u_i = {Sin[Zn_i Degree], Cos[Zn_i Degree]}. sigmaMin is the sight standard deviation in arcminutes (1 arcmin = 1 nm along the LOP normal).";

cnCRLBCovariance::usage = "cnCRLBCovariance[azimuthsDeg, sigmaMin] gives the Cramer-Rao lower bound position covariance matrix (nm^2) = Inverse[cnFisherMatrix[azimuthsDeg, sigmaMin]]. Its eigenvalues give the minimum achievable position variance along each principal axis.";

cnErrorEllipse::usage = "cnErrorEllipse[cov] returns <|\"semiMajorNm\", \"semiMinorNm\", \"orientDeg\", \"cep\"|> for the error ellipse of a 2x2 position covariance (nm^2). orientDeg is the bearing from North of the major semi-axis (clockwise, mod 180). CEP ~ 0.59*(semiMajor+semiMinor) is the 50%% circular error probable.";

cnFixCRLB::usage = "cnFixCRLB[truePos, bodies, sigmaMin] returns <|\"fisherMatrix\", \"covariance\", \"ellipse\", \"azimuths\"|> — the CRLB analysis of a celestial fix. bodies is a list of {bodySpec, time} pairs where bodySpec is \"Sun\" or {\"Star\", name}. sigmaMin is the sight standard deviation in arcminutes.";

cnLongitudeErrorPerSecond::usage = "cnLongitudeErrorPerSecond[latDeg] gives the longitude position error in nautical miles per second of chronometer error: 0.25*Cos[latDeg Degree]. Derivation: Earth rotates 15 deg/hour = 0.25 arcmin of longitude per second; 1 arcmin of longitude = cos(lat) nm on the ground, so the sensitivity is 0.25*cos(lat) nm/s. At the equator, 4 seconds = 1 nautical mile — the navigator's rule that motivated Harrison's chronometer and the 1714 Longitude Act.";

cnChronometerFixError::usage = "cnChronometerFixError[truePos, t, bodies, timeErrSec, opts] computes the position error and its east/west vs north/south split when all sights are reduced with a chronometer offset of timeErrSec seconds. Sights are generated at the true times (zero sextant noise by default) and reduced using the wrong time t_i + timeErrSec — the navigator's GP is therefore shifted. Returns <|\"totalNm\"->, \"eastNm\"->, \"northNm\"->, \"fix\"->|>. t may be a single DateObject (same for all bodies) or a list. bodies is a list of body specs (\"Sun\" or {\"Star\",name}). opts keys: heightMeters, indexErrorMin, sigmaMin (default 0, isolates clock bias).";

cnNoonLatitude::usage = "cnNoonLatitude[hoDeg, decDeg, bearing] gives the observer latitude in degrees from a meridian altitude sight. hoDeg is the observed altitude at Local Apparent Noon (degrees); decDeg is the Sun declination (degrees); bearing is \"S\" (Sun bears south, observer north of sub-solar point) or \"N\" (Sun bears north, observer south of sub-solar point). Formula: zenith distance z = 90 - Ho; \"S\" -> lat = dec + z; \"N\" -> lat = dec - z. Immune to chronometer error because only the maximum altitude value is needed, not the absolute time.";

cnMeridianAltitude::usage = "cnMeridianAltitude[latDeg, decDeg] gives {altDeg, bearing} — the geometric meridian (maximum) altitude in degrees and the bearing of the Sun at Local Apparent Noon. altDeg = 90 - Abs[latDeg - decDeg]; bearing is \"S\" when lat > dec (Sun south of observer) and \"N\" when lat <= dec (Sun north of observer). Use as a helper to generate noon sights and resolve the bearing automatically.";

cnLANTimeUTC::usage = "cnLANTimeUTC[lonDeg, dateObj] gives the approximate UTC DateObject (TimeZone -> 0) of Local Apparent Noon at longitude lonDeg (East positive). Formula: UTC_LAN = 12:00 - lonDeg/15 hours. Accuracy ~15 minutes without an equation-of-time correction (EoT varies -16 to +14 min across the year); sufficient for planning when to take noon sights. For a more accurate value add the equation of time, but the simple formula is standard for DR purposes.";

(* --- Lunar-distance method (Item: longitude without a chronometer) --- *)

cnMoonPosition::usage = "cnMoonPosition[t] gives {raDeg, decDeg, distanceKm, hpDeg}, the Moon's apparent geocentric right ascension and declination (degrees, equator/equinox of date including nutation), geocentric distance (km), and equatorial horizontal parallax (degrees), at UTC DateObject t. Self-contained offline implementation of the abridged ELP lunar theory (Meeus, Astronomical Algorithms ch. 47; full 60-term longitude/distance table 47.A and 60-term latitude table 47.B). Reproduces Meeus Example 47.a (1992-04-12) to ~0.002\[Prime] (see tests/lunar_tests.wl); accuracy ~0.5\[Prime] vs full JPL apparent positions (omits lunar aberration), well within the ~1-2\[Prime] navigation tolerance.";

cnMoonGP::usage = "cnMoonGP[t] gives {decDeg, lonGPDeg}, the Moon's geographic position (sub-lunar point): latitude = declination, longitude where the Moon is overhead (East positive, -180..180), at UTC DateObject t. Uses cnMoonPosition and GMST (GHA = Mod[cnGMST[t] - raMoon, 360]).";

cnMoonHP::usage = "cnMoonHP[t] gives the Moon's equatorial horizontal parallax in degrees at UTC DateObject t (~0.95\[Degree] = 57\[Prime] mean). HP = ArcSin[6378.14 / distanceKm].";

cnLunarDistanceGeocentric::usage = "cnLunarDistanceGeocentric[t] gives the geocentric (center-of-Earth) angular distance in degrees between the Moon and the Sun at UTC DateObject t — the great-circle angle between cnMoonGP[t] and cnSunGP[t]. This is the 'clock in the sky': it changes ~0.5\[Degree]/hour as the Moon moves against the Sun, so inverting it yields GMT.";

cnClearLunarDistance::usage = "cnClearLunarDistance[dObsDeg, moonAltDeg, sunAltDeg, moonHPDeg] (optional 5th arg sunHPDeg, default 0.002443\[Degree]=8.8\[DoublePrime]) clears a topocentric (observed, center-to-center) Moon-Sun distance to the geocentric distance in degrees. moonAltDeg, sunAltDeg are the APPARENT (refracted, topocentric) altitudes. Rigorous spherical clearing: the azimuth difference between the two bodies is invariant under refraction and parallax (both act along the vertical circle), so cos(A) is found from the apparent altitudes and observed distance, then the geocentric distance is rebuilt from the geocentric altitudes (apparent + parallax - refraction; Moon parallax P = ArcSin[Sin[HP]Cos[h]] dominates, Sun's HP ~8.8\[DoublePrime]). Refraction is Bennett (cnRefraction).";

cnLunarDistanceGMT::usage = "cnLunarDistanceGMT[dGeoDeg, tApprox] gives the UTC DateObject (TimeZone -> 0) at which cnLunarDistanceGeocentric equals dGeoDeg, found by inverting the monotonic distance-vs-time relation near tApprox (secant FindRoot over a few-hour window). With a cleared lunar distance as dGeoDeg, this recovers Greenwich Mean Time without a chronometer — the 18th-century method of Maskelyne's lunar tables, rival to Harrison's H4. Conditioning degrades near syzygy (new/full Moon), where the Moon-Sun distance rate slows; tApprox should be within a few hours of the true time for the inversion to converge.";

(* --- Recursive Bayesian / Extended Kalman Filter running fix (R2) --- *)

cnEKFVoyage::usage = "cnEKFVoyage[voyage, sightsPerDay, opts] runs a 2-state (position) Extended Kalman Filter over a voyage, fusing dead-reckoning motion (PREDICT) with the day's celestial sights (UPDATE) into one evolving estimate with a shrinking covariance. voyage is a list of day-associations <|\"day\"->_, \"t\"->DateObject, \"truePos\"->{lat,lon}, \"drPos\"->{lat,lon}|> ordered in time. sightsPerDay is the number of Sun sights taken each day (default 3), spread symmetrically over \[PlusMinus]sightHoursSpread hours about the noon time t (distinct times give distinct azimuths, hence a well-conditioned fix). PREDICT: the estimate is advanced by the DR position increment drPos[k]-drPos[k-1] and the covariance is inflated by the process-noise Q=processNoiseNm^2 I (per day). UPDATE (per sight): the measurement is the observed altitude Ho of the body with GP g; the predicted measurement is cnComputedAltitude[est,g] giving {Hc,Zn}; the innovation Ho-Hc (in arcmin = nm) is the Marcq St-Hilaire intercept; the 1x2 measurement Jacobian is H={Sin[Zn],Cos[Zn]} in local E/N nm coords (1 arcmin altitude <-> 1 nm along Zn); measurement noise R=sigmaMin^2. Standard EKF: S=H.P.H'+R, K=P.H'/S, est+=K*innovation, P=(I-K.H).P (symmetrised). The local E/N tangent plane is recentred on the current estimate each step (nm <-> lat/lon as in cnFix). Returns a list of per-day associations <|\"day\",\"t\",\"estPos\",\"truePos\",\"drPos\",\"covNm\" (2x2 after updates),\"covPredictNm\" (2x2 after predict, before updates),\"errVsTruthNm\",\"drErrorNm\"|>. opts keys: sigmaMin (1.0), indexErrorMin (0.0, a systematic bias applied to generated sights but NOT corrected in reduction), processNoiseNm (4.0, per-day DR uncertainty std), initialPos (Automatic -> voyage[[1]] drPos), initialCovNm (20.0, initial position std), sightHoursSpread (3.0), body (\"Sun\"), heightMeters (2.0), seed (Automatic; seeds once before the run, sights are NOT re-seeded).";

cnSystematicFixError::usage = "cnSystematicFixError[truePos, t, bodies, sigmaMin, indexErrorMin, n] runs n Monte-Carlo fixes with a CONSTANT index error (a systematic bias) of indexErrorMin arcmin added to EVERY sight (generated with the bias, reduced WITHOUT correcting it, as a navigator unaware of the error would). Demonstrates that a systematic error moves the CENTRE of the fix distribution (bias) without enlarging its SPREAD (scatter): the fix covariance is translation-invariant, so it equals the random-only (indexErrorMin=0) covariance, while the mean is shifted by ~the index error along the net azimuth direction. This is the counterpoint to the cocked-hat 25%% theorem: a tight cocked hat does NOT prove accuracy. t is a single DateObject (same for all bodies) or a list (one per body). bodies is a list of body specs (\"Sun\" or {\"Star\",name}). Returns <|\"fixes\"->{{lat,lon}..}, \"biasVecNm\"->{E,N} mean offset from truth, \"biasNm\"->magnitude, \"scatterCov\"->2x2 covariance (nm^2), \"scatterTraceNm2\"->Tr, \"indexErrorMin\"->_|>. Seed externally with SeedRandom for reproducibility.";

(* --- Bayesian posterior over position (R3) --- *)

cnPosteriorGrid::usage = "cnPosteriorGrid[sights, {{latMin,latMax},{lonMin,lonMax}}, nGrid, sigmaMin] evaluates the full Bayesian posterior probability surface over an nGrid x nGrid position grid. Each sight is <|\"body\"->(\"Sun\"|{\"Star\",name}), \"t\"->DateObject, \"Ho\"->obsAltDeg|>. With a flat (uniform) prior the log-posterior at a grid point x is Sum_i -0.5 ((Ho_i - cnAltitudeFromGP[x, GP_i]) 60 / sigmaMin)^2 (residual converted from degrees to arcmin; sigmaMin is the sight standard deviation in arcmin, equivalently the LOP-normal sd in nm). The unnormalised log-posterior is exponentiated (after subtracting its max for numerical stability) and normalised to sum to 1 over the grid. One sight yields a fuzzy BAND (the line of position as a likelihood ridge), two a BLOB, three or more a tight PEAK. A deliberate constant systematic (e.g. uncorrected index error baked into the Ho values) shifts the peak OFF the true position, visualising bias honestly. Returns <|\"lats\"->(nGrid lat axis), \"lons\"->(nGrid lon axis), \"logPosterior\"->(unnormalised 2D array, rows=lat, cols=lon), \"posterior\"->(normalised 2D array), \"mapEstimate\"->{lat,lon} of the maximum-a-posteriori grid cell|>.";

Begin["`Private`"];

(* Capture the package directory ONCE at load time while $InputFileName is set.
   cnLoadStars uses this so it resolves the CSV correctly even when the package
   is Get-loaded from a notebook or other directory. *)
cnPackageDir = DirectoryName[$InputFileName];

cnVersion[] := "0.1.0";

cnDip[h_?NumericQ] := 1.76 Sqrt[h];                         (* arcmin, h in metres *)
cnRefraction[hApp_?NumericQ] := Cot[(hApp + 7.31/(hApp + 4.4)) Degree]; (* Bennett, arcmin *)
cnRefractionSaemundsson[h_?NumericQ] := 1.02 Cot[(h + 10.3/(h + 5.11)) Degree]; (* Saemundsson 1986, arcmin, from TRUE altitude *)
cnRefractionSimple[h_?NumericQ] := 0.96 Cot[h Degree]; (* Simple/Smart model, arcmin; diverges near horizon *)
cnRefractionPT[h_?NumericQ, p_?NumericQ, t_?NumericQ] := cnRefraction[h] * (p/1010) * (283/(273 + t)); (* Bennett scaled for P,T *)
cnSunSemidiameter[] := 16.0;                                 (* arcmin, mean *)
(* A5: Date-varying solar semi-diameter.  r = Earth-Sun distance in AU from the
   same low-precision mean-anomaly formula used by cnSunGP.  SD = 16.0 / r
   (SD scales inversely with distance: larger at perihelion, smaller at aphelion).
   g = mean anomaly in degrees.  Range: ~15.74' (aphelion, early July) to
   ~16.27' (perihelion, early January). *)
cnSunSemidiameterAt[t_] := Module[{jd, d, g, r},
  jd = JulianDate[t];
  d  = jd - 2451545.0;
  g  = Mod[357.528 + 0.9856003 d, 360];      (* mean anomaly, degrees *)
  r  = 1.00014 - 0.01671 Cos[g Degree] - 0.00014 Cos[2 g Degree];  (* AU *)
  16.0 / r
];
cnSunParallax[h_?NumericQ] := 0.15 Cos[h Degree];           (* arcmin *)
cnObservedAltitude[hs_?NumericQ, height_?NumericQ, indexErr_?NumericQ, limb_String] :=
  Module[{sd = cnSunSemidiameter[], limbSign},
    limbSign = limb /. {"Lower" -> 1, "Upper" -> -1, "Center" -> 0};
    hs - indexErr/60 - cnDip[height]/60 - cnRefraction[hs]/60
       + limbSign sd/60 + cnSunParallax[hs]/60
  ];

(* --- Ephemeris wrappers (Task 3) --- *)

(* Convert a Quantity angular value to a plain degree number *)
cnDeg[q_] := QuantityMagnitude[UnitConvert[q, "AngularDegrees"]];

(* True geometric Sun altitude in degrees at observer position *)
cnSunAltitude[{lat_?NumericQ, lon_?NumericQ}, t_] :=
  cnDeg[SunPosition[GeoPosition[{lat, lon}], t][[2]]];

(* True Sun azimuth in degrees (0-360, measured from North) *)
cnSunAzimuth[{lat_?NumericQ, lon_?NumericQ}, t_] :=
  Mod[cnDeg[SunPosition[GeoPosition[{lat, lon}], t][[1]]], 360];

(* Greenwich Mean Sidereal Time in degrees.
   (A4: previously named cnGAST; renamed to cnGMST because the equation of
   equinoxes (~1") is omitted — this is GMST, not GAST.)
   Uses IAU-1982 numeric GMST formula (AstroValue["GreenwichSiderealTime"] is
   unavailable without a Wolfram Server connection).
   jd: Julian Date, d: days from J2000.0 (JD 2451545.0). *)
cnGMST[t_] := Module[{jd, d, gmstHours},
  jd = JulianDate[t];
  d = jd - 2451545.0;
  gmstHours = Mod[18.697375 + 24.065709824279 d, 24];
  gmstHours * 15.0
];

(* --- Navigational star catalogue (Item 1) --- *)

(* Memoized: load once from CSV located via cnPackageDir (captured at load time).
   cnPackageDir is bound when the package is Get-loaded — $InputFileName is the
   package file at that moment — so this works correctly regardless of the calling
   context's working directory (e.g. a notebook FrontEnd where $InputFileName is
   unset at call time). *)
cnLoadStars::nofile = "Navigational star catalogue not found: `1`. Check that data/nav_stars.csv is present relative to the package.";
cnLoadStars[] := cnLoadStars[] = Module[{csvPath, rows, assoc},
  csvPath = FileNameJoin[{cnPackageDir, "..", "data", "nav_stars.csv"}];
  If[!FileExistsQ[csvPath],
    Message[cnLoadStars::nofile, csvPath]; Abort[]];
  rows = Import[csvPath, "CSV"];
  (* rows[[1]] is the header; rows[[2;;]] are data.
     Columns: name, raDeg, decDeg, magnitude, pmRA (arcsec/yr mu_alpha*cos delta), pmDec (arcsec/yr) *)
  assoc = Association @ Table[
    rows[[i, 1]] -> <|"raDeg" -> rows[[i, 2]], "decDeg" -> rows[[i, 3]],
                      "mag" -> rows[[i, 4]],
                      "pmRA" -> rows[[i, 5]], "pmDec" -> rows[[i, 6]]|>,
    {i, 2, Length[rows]}
  ];
  assoc
];

cnStarRADec::notfound = "Star `1` not found in the navigational-star catalogue.";
cnStarRADec[name_String] := Module[{cat, entry},
  cat = cnLoadStars[];
  entry = cat[name];
  If[MissingQ[entry], Message[cnStarRADec::notfound, name]; Return[$Failed]];
  {entry["raDeg"], entry["decDeg"]}
];

(* IAU-1976 precession: precess J2000.0 equatorial coordinates {raDeg, decDeg}
   to the epoch of DateObject t.
   T = Julian centuries from J2000.0; zetaA, zA, thetaA are Lieske (1977) angles in arcsec,
   converted to degrees.  ArcTan[B,A] = atan2(A,B) in Wolfram Language. *)
cnPrecess[{raDeg_?NumericQ, decDeg_?NumericQ}, t_] := Module[
  {T, zetaA, zA, thetaA, ra0, dec0, A, B, C, raP, decP},
  T = (JulianDate[t] - 2451545.0) / 36525.0;
  zetaA = (2306.2181 T + 0.30188 T^2 + 0.017998 T^3) / 3600.0;  (* degrees *)
  zA    = (2306.2181 T + 1.09468 T^2 + 0.018203 T^3) / 3600.0;
  thetaA = (2004.3109 T - 0.42665 T^2 - 0.041833 T^3) / 3600.0;
  ra0  = raDeg  Degree;
  dec0 = decDeg Degree;
  A = Cos[dec0] Sin[ra0 + zetaA Degree];
  B = Cos[thetaA Degree] Cos[dec0] Cos[ra0 + zetaA Degree] - Sin[thetaA Degree] Sin[dec0];
  C = Sin[thetaA Degree] Cos[dec0] Cos[ra0 + zetaA Degree] + Cos[thetaA Degree] Sin[dec0];
  raP  = Mod[ArcTan[B, A] / Degree + zA, 360];
  decP = ArcSin[C] / Degree;
  {raP, decP}
];

cnBodyGP[{raDeg_?NumericQ, decDeg_?NumericQ}, t_] := Module[{prec, gha, lonGP},
  prec = cnPrecess[{raDeg, decDeg}, t];
  gha = Mod[cnGMST[t] - prec[[1]], 360];
  lonGP = Mod[-gha + 180, 360] - 180;
  {prec[[2]], lonGP}
];

(* Sun's Greenwich Hour Angle in degrees (0-360), derived from low-precision
   solar coordinates (Astronomical Almanac, Section C, ~0.01 deg accuracy).
   IMPORTANT: ArcTan[x,y] in WL = atan2(y,x); RA uses
   ArcTan[Cos[lam], Cos[eps]*Sin[lam]] to match atan2(cos(eps)*sin(lam), cos(lam)). *)
cnSunGHA[t_] := Module[{jd, d, L, g, lam, eps, raDeg},
  jd = JulianDate[t];
  d = jd - 2451545.0;
  L   = Mod[280.460 + 0.9856474 d, 360];           (* mean longitude, deg *)
  g   = Mod[357.528 + 0.9856003 d, 360];           (* mean anomaly, deg *)
  lam = L + 1.915 Sin[g Degree] + 0.020 Sin[2 g Degree]; (* ecliptic lon, deg *)
  eps = 23.439 - 0.0000004 d;                       (* obliquity, deg *)
  raDeg = ArcTan[Cos[lam Degree], Cos[eps Degree] Sin[lam Degree]] / Degree;
  Mod[cnGMST[t] - Mod[raDeg, 360], 360]
];

(* Sub-solar point (geographic position of the Sun):
   {declinationDeg, lonGPDeg} where lonGP is in -180..180, East positive. *)
cnSunGP[t_] := Module[{jd, d, L, g, lam, eps, decDeg, ghaDeg, lonGP},
  jd = JulianDate[t];
  d = jd - 2451545.0;
  L   = Mod[280.460 + 0.9856474 d, 360];
  g   = Mod[357.528 + 0.9856003 d, 360];
  lam = L + 1.915 Sin[g Degree] + 0.020 Sin[2 g Degree];
  eps = 23.439 - 0.0000004 d;
  decDeg = ArcSin[Sin[eps Degree] Sin[lam Degree]] / Degree;
  ghaDeg = cnSunGHA[t];
  lonGP = Mod[-ghaDeg + 180, 360] - 180;
  {decDeg, lonGP}
];

(* --- Sight reduction — Marcq St-Hilaire intercept method (Task 4) --- *)

(* Computed altitude Hc and azimuth Zn at an assumed position (AP) for a body
   whose geographic position is {decGP, lonGP}.
   LHA = lonAP - lonGP (east-positive convention; Mod to 0..360 gives traditional
   west-measured LHA since GHA = Mod[-lonGP, 360] and LHA = GHA + lonAP).
   Azimuth quadrant rule: Sin[LHA] > 0 (LHA in 0..180) means body is to the west
   of the observer's meridian → Zn = 360 - Z. *)
cnComputedAltitude[{lat_, lon_}, {dec_, lonGP_}] := Module[{lha, hc, zn, cosZn},
  lha = Mod[lon - lonGP, 360];                                   (* east-positive LHA *)
  hc = ArcSin[Sin[lat Degree] Sin[dec Degree] +
       Cos[lat Degree] Cos[dec Degree] Cos[lha Degree]] / Degree;
  cosZn = (Sin[dec Degree] - Sin[hc Degree] Sin[lat Degree]) /
          (Cos[hc Degree] Cos[lat Degree]);
  zn = ArcCos[Clip[cosZn, {-1, 1}]] / Degree;
  (* LHA in 0..180 → body to the west → Zn = 360 - Z; else Zn = Z *)
  If[Sin[lha Degree] > 0, zn = 360 - zn];
  {hc, Mod[zn, 360]}
];

(* Intercept in nautical miles: positive = Toward (Ho > Hc means closer) *)
cnIntercept[ho_, hc_] := (ho - hc) 60;

(* Line of position: AP shifted interceptNm along Zn gives the closest LOP point.
   GeoDestination returns GeoPosition[{lat, lon}] with plain degree values;
   [[1, ;;2]] extracts {lat, lon} safely even if a trailing elevation is present. *)
cnLOP[ap_, gp_, ho_] := Module[{hc, zn, p, dest, pt},
  {hc, zn} = cnComputedAltitude[ap, gp];
  p = cnIntercept[ho, hc];
  dest = GeoDestination[GeoPosition[ap],
           {Quantity[p, "NauticalMiles"], Quantity[zn, "AngularDegrees"]}];
  pt = dest[[1, ;; 2]];
  <|"AP" -> ap, "Hc" -> hc, "Zn" -> zn, "Ho" -> ho,
    "interceptNm" -> p, "point" -> pt, "bearingDeg" -> Mod[zn + 90, 360]|>
];

(* --- Fix, advance, running fix, cocked hat (Task 5) --- *)

(* Geometric altitude in degrees using the spherical law of cosines.
   Identical formula to cnComputedAltitude so noise-free LOPs close exactly. *)
cnAltitudeFromGP[{lat_, lon_}, {dec_, lonGP_}] :=
  ArcSin[Sin[lat Degree] Sin[dec Degree] +
    Cos[lat Degree] Cos[dec Degree] Cos[(lon - lonGP) Degree]] / Degree;

(* Simultaneous fix — least-squares intersection of LOPs in a local tangent plane.
   Each LOP is linearised as the line through "point" normal to "bearingDeg".
   Coordinate system: origin at mean of LOP points, axes East and North in nm. *)
cnFix[lops_List] := Module[{p0, toXY, fromXY, lines, sol},
  p0 = Mean[#["point"] & /@ lops];
  (* Convert geographic {lat,lon} to local {E, N} in nautical miles. *)
  toXY[{lat_, lon_}] := {(lon - p0[[2]]) Cos[p0[[1]] Degree] 60,
                          (lat - p0[[1]]) 60};
  (* Inverse: local {E, N} back to {lat, lon}. *)
  fromXY[{xx_, yy_}] := {p0[[1]] + yy/60,
                          p0[[2]] + xx/(60 Cos[p0[[1]] Degree])};
  (* For each LOP, build normal n to the line and RHS d = n . pt.
     Bearing b is the direction of the LOP; normal is {Cos[b], -Sin[b]} in {E,N}. *)
  lines = Function[lopArg,
    Module[{pt = toXY[lopArg["point"]], brg = lopArg["bearingDeg"] Degree, n},
      n = {Cos[brg], -Sin[brg]};
      {n, n . pt}]] /@ lops;
  sol = LeastSquares[lines[[All, 1]], lines[[All, 2]]];
  fromXY[sol]
];

(* Advance (transfer) a position line by dist nm along course.
   Returns a new LOP association with "point" moved; all other keys unchanged.
   Uses the same GeoDestination extraction as cnLOP (Task 4): [[1, ;;2]]. *)
cnAdvanceLOP[lop_, course_, dist_] := Module[{dest, newpt},
  dest = GeoDestination[GeoPosition[lop["point"]],
           {Quantity[dist, "NauticalMiles"],
            Quantity[course, "AngularDegrees"]}];
  newpt = dest[[1, ;; 2]];
  <|lop, "point" -> newpt|>
];

(* Running fix: advance each LOP to a common time, then take the simultaneous fix.
   Each element of items: <|"lop" -> assoc, "course" -> deg, "distance" -> nm|> *)
cnRunningFix[items_List] :=
  cnFix[cnAdvanceLOP[#["lop"], #["course"], #["distance"]] & /@ items];

(* Cocked hat: three pairwise two-LOP intersections and the area of the resulting triangle.
   Area is computed in a local nm tangent plane centred on the first vertex. *)
cnCockedHat[lops_List] := Module[{pairs, verts, ref, pts2d},
  pairs = Subsets[lops, {2}];
  verts = cnFix /@ pairs;               (* three {lat,lon} vertices *)
  ref = verts[[1]];
  (* Project vertices to local {E, N} nm plane centred on first vertex. *)
  pts2d = Function[{lat, lon},
    {(lon - ref[[2]]) Cos[ref[[1]] Degree] 60,
     (lat - ref[[1]]) 60}] @@@ verts;
  (* Triangle area via shoelace (det) formula; works with floating-point coordinates. *)
  <|"vertices" -> verts,
    "areaNm2"  -> 0.5 Abs[Det[{pts2d[[2]] - pts2d[[1]],
                                pts2d[[3]] - pts2d[[1]]}]]|>
];

(* --- Sight generation, reduction, and Monte-Carlo error model (Task 6) --- *)

(* Generate a simulated sextant altitude Hs for the Sun.
   TRUE altitude is computed from the ALMANAC GP (cnAltitudeFromGP[truePos, cnSunGP[t]])
   — NOT from SunPosition — so that zero-noise sights reduce EXACTLY through truePos
   (both generate and reduce use the same ephemeris, eliminating the ~0.5 nm systematic
   mismatch that would arise if SunPosition were used here but cnSunGP used in reduction).
   The corrections are then INVERTED (approximating cnRefraction and cnSunParallax at htrue
   rather than hs, which introduces a sub-arcsecond error), and Gaussian noise is added. *)
cnGenerateSight[truePos_, t_, opts_:<||>] := Module[
  {o = Join[<|"heightMeters"->2.0,"indexErrorMin"->0.0,"limb"->"Lower",
              "sigmaMin"->1.0,"seed"->Automatic|>, opts],
   htrue, hs, err},
  (* True altitude from the almanac GP — same ephemeris the navigator uses *)
  htrue = cnAltitudeFromGP[truePos, cnSunGP[t]];
  (* Invert corrections: find hs such that cnObservedAltitude[hs,...] recovers htrue.
     Approximation: evaluate cnRefraction and cnSunParallax at htrue (not hs). *)
  hs = htrue + o["indexErrorMin"]/60 + cnDip[o["heightMeters"]]/60
       + cnRefraction[htrue]/60
       - (o["limb"] /. {"Lower"->1,"Upper"->-1,"Center"->0}) cnSunSemidiameter[]/60
       - cnSunParallax[htrue]/60;
  If[o["seed"] =!= Automatic, SeedRandom[o["seed"]]];
  err = If[o["sigmaMin"] == 0, 0, RandomVariate[NormalDistribution[0, o["sigmaMin"]/60]]];
  hs + err
];

(* Reduce a sextant altitude to a Line of Position using the almanac GP. *)
cnReduceSight[hs_, t_, ap_, opts_:<||>] := Module[
  {o = Join[<|"heightMeters"->2.0,"indexErrorMin"->0.0,"limb"->"Lower"|>, opts], ho},
  ho = cnObservedAltitude[hs, o["heightMeters"], o["indexErrorMin"], o["limb"]];
  cnLOP[ap, cnSunGP[t], ho]
];

(* Monte-Carlo fix distribution.
   For each of n trials: generate one sight per time in times, reduce to LOPs,
   compute the simultaneous fix, and record the error from truePos.
   The "seed" option seeds the global RNG once before the loop; it is stripped
   before being passed down to cnGenerateSight to prevent re-seeding on every
   sight call (which would make all sights identical within a trial). *)
cnMonteCarloFix[truePos_, times_List, opts_:<||>, n_:500] := Module[
  {o = Join[<|"sigmaMin"->1.0,"seed"->Automatic|>, opts],
   sightOpts, fixes, errs, p0, toXY},
  If[o["seed"] =!= Automatic, SeedRandom[o["seed"]]];
  (* Drop "seed" so cnGenerateSight uses global RNG state, not a fixed re-seed *)
  sightOpts = KeyDrop[o, "seed"];
  fixes = Table[
    cnFix[cnReduceSight[cnGenerateSight[truePos, #, sightOpts], #, truePos, sightOpts] & /@ times],
    {n}];
  errs = QuantityMagnitude[
           GeoDistance[GeoPosition[truePos], GeoPosition[#]] / Quantity[1,"NauticalMiles"]
         ] & /@ fixes;
  p0 = truePos;
  toXY[{lat_,lon_}] := {(lon - p0[[2]]) Cos[p0[[1]] Degree] 60, (lat - p0[[1]]) 60};
  <|"fixes"    -> fixes,
    "errorsNm" -> errs,
    "cep"      -> Quantile[errs, 0.5],
    "covNm"    -> Covariance[toXY /@ fixes]|>
];

(* --- Generalized sight engine for Sun and stars (Item 3) --- *)

(* GP dispatcher: routes to the correct ephemeris function by body type. *)
cnBodyGPFor["Sun", t_] := cnSunGP[t];

(* A2: Apply linear proper motion (Hipparcos mu_alpha*cos(delta) and mu_delta,
   both in arcsec/yr) BEFORE precession.  dt is Julian years from J2000.0.
   pmRA = mu_alpha * cos(delta), so Delta_alpha = pmRA * dt / cos(delta).
   Delta_delta = pmDec * dt.  Divide by 3600 to convert arcsec -> degrees.
   Fail loud if the star name is not in the catalogue. *)
cnBodyGPFor[{"Star", name_String}, t_] := Module[
  {star, raDeg, decDeg, pmRA, pmDec, dt, raAdj, decAdj},
  star = cnLoadStars[][name];
  If[MissingQ[star],
    Message[cnStarRADec::notfound, name]; Return[$Failed]];
  raDeg  = star["raDeg"];
  decDeg = star["decDeg"];
  pmRA   = star["pmRA"];
  pmDec  = star["pmDec"];
  dt = (JulianDate[t] - 2451545.0) / 365.25;    (* Julian years from J2000.0 *)
  raAdj  = raDeg  + pmRA  * dt / (3600.0 Cos[decDeg Degree]);
  decAdj = decDeg + pmDec * dt / 3600.0;
  cnBodyGP[{raAdj, decAdj}, t]
];

(* Body-appropriate corrections:
   Sun  — Lower limb default (SD=16', parallax active), dip and refraction apply.
   Star — no SD, no parallax (point source at infinite distance); dip and refraction only. *)
cnObservedAltitudeBody[hs_?NumericQ, height_?NumericQ, indexErr_?NumericQ, "Sun"] :=
  cnObservedAltitude[hs, height, indexErr, "Lower"];

cnObservedAltitudeBody[hs_?NumericQ, height_?NumericQ, indexErr_?NumericQ, {"Star", _String}] :=
  hs - indexErr/60 - cnDip[height]/60 - cnRefraction[hs]/60;

(* Generate a simulated sextant altitude Hs for a generic body.
   TRUE altitude is computed from the almanac GP via cnBodyGPFor so that zero-noise
   sights reduce exactly through truePos (same ephemeris path in generate and reduce).
   Corrections are inverted at htrue (sub-arcsecond approximation error). *)
cnGenerateSightBody[truePos_, t_, body_, opts_:<||>] := Module[
  {o = Join[<|"heightMeters"->2.0,"indexErrorMin"->0.0,"sigmaMin"->1.0,"seed"->Automatic|>, opts],
   htrue, hs, err},
  htrue = cnAltitudeFromGP[truePos, cnBodyGPFor[body, t]];
  hs = If[body === "Sun",
    (* Sun: invert Lower-limb corrections (SD subtracted from hs to increase it) *)
    htrue + o["indexErrorMin"]/60 + cnDip[o["heightMeters"]]/60
            + cnRefraction[htrue]/60 - cnSunSemidiameter[]/60 - cnSunParallax[htrue]/60,
    (* Star: no SD, no parallax *)
    htrue + o["indexErrorMin"]/60 + cnDip[o["heightMeters"]]/60 + cnRefraction[htrue]/60
  ];
  If[o["seed"] =!= Automatic, SeedRandom[o["seed"]]];
  err = If[o["sigmaMin"] == 0, 0, RandomVariate[NormalDistribution[0, o["sigmaMin"]/60]]];
  hs + err
];

(* Reduce a sextant altitude to a LOP using body-appropriate corrections and GP. *)
cnReduceSightBody[hs_, t_, ap_, body_, opts_:<||>] := Module[
  {o = Join[<|"heightMeters"->2.0,"indexErrorMin"->0.0|>, opts], ho},
  ho = cnObservedAltitudeBody[hs, o["heightMeters"], o["indexErrorMin"], body];
  cnLOP[ap, cnBodyGPFor[body, t], ho]
];

(* --- Optimal star selection via geometric dilution of precision (Item 4) --- *)

(* GDOP from raw azimuth list (degrees).
   Each sight constrains position along the body's azimuth Zn.
   The Fisher information matrix is M = Sum_i u_i u_i^T where
   u_i = {Sin[Zn_i Degree], Cos[Zn_i Degree]} (E,N unit vector toward GP).
   Position covariance ~ sigma^2 M^{-1}; GDOP = Sqrt[Tr[M^{-1}]].
   For 3 azimuths exactly 120 deg apart: M = (3/2) Identity => GDOP = Sqrt[4/3] ~ 1.155.
   Clustered azimuths produce a near-singular M; Check catches Inverse::sing
   and returns the sentinel value 9999. *)
cnGDOPFromAzimuths[azimuths_List] := Module[{us, M},
  us = {Sin[# Degree], Cos[# Degree]} & /@ azimuths;
  M = Sum[Outer[Times, us[[i]], us[[i]]], {i, Length[us]}];
  Check[Sqrt[Tr[Inverse[M]]], 9999.]
];

(* Visible navigational stars: those with computed altitude >= minAltDeg. *)
cnVisibleStars[pos_, t_, minAltDeg_:15] :=
  Select[Keys[cnLoadStars[]],
    cnComputedAltitude[pos, cnBodyGPFor[{"Star", #}, t]][[1]] >= minAltDeg &
  ];

(* GDOP for a named set of stars at observer position and UTC time. *)
cnFixGDOP[pos_, t_, starNames_List] := Module[{azimuths},
  azimuths = cnComputedAltitude[pos, cnBodyGPFor[{"Star", #}, t]][[2]] & /@ starNames;
  cnGDOPFromAzimuths[azimuths]
];

(* Best triplet of visible stars minimising GDOP.
   Computes alt+az for all 58 catalogue stars in one pass, keeps those above
   minAltDeg, then searches all C(n,3) triplets.  If more than 20 are visible,
   restricts to the 20 brightest (lowest magnitude) so C(n,3) <= 1140 — in
   practice n <= 25 for this catalogue, so the restriction rarely fires.
   Returns <|"stars"->{n1,n2,n3}, "gdop"->_, "azimuths"->{Az1,Az2,Az3}|>. *)
cnBestStarTriplet[pos_, t_, minAltDeg_:15] := Module[
  {allStars, altAzData, visible, candidates, azMap, triplets,
   bestGDOP, bestTrip, bestAz, azs, g, trip},
  allStars = Keys[cnLoadStars[]];
  (* Single pass: compute alt and az for every catalogue star *)
  altAzData = Association[
    # -> cnComputedAltitude[pos, cnBodyGPFor[{"Star", #}, t]] & /@ allStars];
  visible = Select[allStars, altAzData[#][[1]] >= minAltDeg &];
  (* Restrict to brightest 20 if more than 20 visible *)
  candidates = If[Length[visible] > 20,
    Take[SortBy[visible, cnLoadStars[][#]["mag"] &], 20],
    visible
  ];
  azMap = Association[# -> altAzData[#][[2]] & /@ candidates];
  triplets = Subsets[candidates, {3}];
  bestGDOP = Infinity; bestTrip = None; bestAz = None;
  Do[
    trip = triplets[[i]];
    azs = azMap /@ trip;
    g = cnGDOPFromAzimuths[azs];
    If[g < bestGDOP, bestGDOP = g; bestTrip = trip; bestAz = azs],
    {i, Length[triplets]}
  ];
  <|"stars" -> bestTrip, "gdop" -> bestGDOP, "azimuths" -> bestAz|>
];

(* --- Cocked-hat 25% theorem: containment test and Monte-Carlo (Item 10) --- *)

(* Point-in-triangle test using a local tangent plane.
   Projects all four {lat,lon} points to local East/North nm coordinates centred
   on the mean of the three vertices, then applies the sign-of-cross-products test:
   for each directed edge v_i -> v_{i+1}, compute the z-component of the 2-D cross
   product (edge) x (pt - v_i).  If all three have the same sign the point is inside
   (works for both CW and CCW vertex orderings).
   Points exactly on an edge return True (all three values are 0). *)
cnPointInTriangle[pt_, {v1_, v2_, v3_}] := Module[
  {p0, toXY, pXY, t1, t2, t3, d1, d2, d3},
  p0 = Mean[{v1, v2, v3}];
  toXY[{lat_, lon_}] := {(lon - p0[[2]]) Cos[p0[[1]] Degree] 60,
                          (lat - p0[[1]]) 60};
  pXY = toXY[pt];
  t1 = toXY[v1]; t2 = toXY[v2]; t3 = toXY[v3];
  d1 = (t2[[1]]-t1[[1]]) (pXY[[2]]-t1[[2]]) - (t2[[2]]-t1[[2]]) (pXY[[1]]-t1[[1]]);
  d2 = (t3[[1]]-t2[[1]]) (pXY[[2]]-t2[[2]]) - (t3[[2]]-t2[[2]]) (pXY[[1]]-t2[[1]]);
  d3 = (t1[[1]]-t3[[1]]) (pXY[[2]]-t3[[2]]) - (t1[[2]]-t3[[2]]) (pXY[[1]]-t3[[1]]);
  Not[(d1 < 0 || d2 < 0 || d3 < 0) && (d1 > 0 || d2 > 0 || d3 > 0)]
];

(* Containment test: build the cocked-hat triangle and test truePos against it. *)
cnCockedHatContains[truePos_, lops_] := Module[{hat},
  hat = cnCockedHat[lops];
  cnPointInTriangle[truePos, hat["vertices"]]
];

(* Monte-Carlo estimate of P(truth inside cocked hat).
   Mirrors the structure of cnMonteCarloFix: seed once at top, strip "seed" key
   before passing opts into sight generation so individual sights are NOT re-seeded
   (re-seeding would make all sights in a trial identical — correlated noise).
   t may be a single DateObject (same time for all bodies) or a list of DateObjects.
   Uses truePos as the assumed position (AP) for each sight reduction, which keeps
   the LOPs unbiased (they all pass near truePos in the zero-noise limit).
   Returns the empirical fraction as a Machine real. *)
cnCockedHatProbability[truePos_, t_, bodies_List, opts_:<||>, n_:500] := Module[
  {o = Join[<|"sigmaMin"->1.5,"seed"->Automatic|>, opts],
   sightOpts, times, count},
  If[o["seed"] =!= Automatic, SeedRandom[o["seed"]]];
  sightOpts = KeyDrop[o, "seed"];
  times = If[ListQ[t], t, ConstantArray[t, Length[bodies]]];
  count = Total @ Table[
    Boole @ cnCockedHatContains[truePos,
      MapThread[Function[{body, ti},
        cnReduceSightBody[
          cnGenerateSightBody[truePos, ti, body, sightOpts],
          ti, truePos, body, sightOpts]],
        {bodies, times}]],
    {n}];
  N[count / n]
];

(* --- Fisher information matrix and Cramér–Rao lower bound (Item 11) --- *)

(* F = (1/σ²) Σ u_i u_i^T, u_i = {Sin[Zn_i°], Cos[Zn_i°]} in (E,N) nm space.
   Each sight constrains position along the body azimuth with noise σ nm.
   Outer[Times, u, u] gives the 2×2 rank-1 update u u^T. *)
cnFisherMatrix[azimuthsDeg_List, sigmaMin_?NumericQ] := Module[{us},
  us = {Sin[# Degree], Cos[# Degree]} & /@ azimuthsDeg;
  (1/sigmaMin^2) Sum[Outer[Times, us[[i]], us[[i]]], {i, Length[us]}]
];

(* CRLB covariance C = F^{-1} in nm^2; position error covariance floor. *)
cnCRLBCovariance[azimuthsDeg_List, sigmaMin_?NumericQ] :=
  Inverse[cnFisherMatrix[azimuthsDeg, sigmaMin]];

(* Eigen-decompose a 2×2 position covariance to extract the error ellipse.
   Eigensystem returns {eigenvalues, eigenvectors}; Ordering sorts descending
   so order[[1]] is the major axis.
   orientDeg: bearing from North (clockwise) of the major semi-axis, mod 180
              (an axis has no preferred direction, hence mod 180 not 360).
   ArcTan[x,y] in WL = atan2(y,x); bearing from N = atan2(vE,vN) = ArcTan[vN,vE]. *)
cnErrorEllipse[cov_] := Module[
  {eig, vals, vecs, order, major, minor, majorVec, orientDeg},
  eig = Eigensystem[N[cov]];
  vals = eig[[1]]; vecs = eig[[2]];
  order = Ordering[vals, All, Greater];
  major = Sqrt[vals[[order[[1]]]]];
  minor = Sqrt[vals[[order[[2]]]]];
  majorVec = vecs[[order[[1]]]];
  (* majorVec = {vE, vN}; bearing from N = atan2(vE,vN) = ArcTan[vN, vE] *)
  orientDeg = Mod[ArcTan[majorVec[[2]], majorVec[[1]]] / Degree, 180];
  <|"semiMajorNm" -> major, "semiMinorNm" -> minor,
    "orientDeg"   -> orientDeg,
    "cep"         -> 0.59 (major + minor)|>
];

(* Compute CRLB for a specific fix: get body azimuths from geometry, return full analysis.
   bodies: list of {bodySpec, time} pairs. bodySpec is "Sun" or {"Star",name}. *)
cnFixCRLB[truePos_, bodies_, sigmaMin_?NumericQ] := Module[{azimuths, cov},
  azimuths = cnComputedAltitude[truePos, cnBodyGPFor[#[[1]], #[[2]]]][[2]] & /@ bodies;
  cov = cnCRLBCovariance[azimuths, sigmaMin];
  <|"fisherMatrix" -> cnFisherMatrix[azimuths, sigmaMin],
    "covariance"   -> cov,
    "ellipse"      -> cnErrorEllipse[cov],
    "azimuths"     -> azimuths|>
];

(* --- Chronometer-error longitude sensitivity (Item 12) --- *)

(* Longitude position error (nm) per second of chronometer error.
   Earth rotates 15 deg/hour = 0.25 arcmin longitude per second.
   One arcmin of longitude = cos(lat) nautical miles, so:
   sensitivity = 0.25 * cos(lat) nm/s.
   At the equator: 4 s * 0.25 nm/s = 1.0 nm — the "4-second rule".
   Independent of longitude; latitude determines the scale of meridians. *)
cnLongitudeErrorPerSecond[latDeg_?NumericQ] := 0.25 Cos[latDeg Degree];

(* Position-bias from a clock offset: generate at true time, reduce with wrong time.
   The clock error shifts every body's GP westward by 15*timeErrSec/3600 degrees
   (Earth rotates east; GP appears to move west).  All LOPs are therefore biased
   consistently in the east-west direction, producing a nearly pure longitude error
   in the fix — exactly the vulnerability that motivated Harrison's H4 chronometer. *)
cnChronometerFixError[truePos_, t_, bodies_List, timeErrSec_?NumericQ, opts_:<||>] :=
  Module[
    {o = Join[<|"heightMeters" -> 2.0, "indexErrorMin" -> 0.0, "sigmaMin" -> 0.0|>, opts],
     times, tWrong, lops, fix, p0, toXY, fixXY, eastNm, northNm, totalNm},
    (* Normalize to lists *)
    times  = If[ListQ[t], t, ConstantArray[t, Length[bodies]]];
    (* Wrong time: navigator's clock reads t + timeErrSec *)
    tWrong = DatePlus[#, {timeErrSec, "Second"}] & /@ times;
    (* Sights generated at TRUE time; reduced with WRONG time → biased GP *)
    lops = MapThread[
      Function[{body, tTrue, tBad},
        cnReduceSightBody[
          cnGenerateSightBody[truePos, tTrue, body, o],
          tBad, truePos, body, o]],
      {bodies, times, tWrong}];
    fix = cnFix[lops];
    (* Project fix error into local East/North nm tangent plane *)
    p0 = truePos;
    toXY[{lat_, lon_}] := {(lon - p0[[2]]) Cos[p0[[1]] Degree] 60,
                            (lat - p0[[1]]) 60};
    fixXY   = toXY[fix];
    eastNm  = fixXY[[1]];
    northNm = fixXY[[2]];
    totalNm = Sqrt[eastNm^2 + northNm^2];
    <|"totalNm" -> totalNm, "eastNm" -> eastNm, "northNm" -> northNm, "fix" -> fix|>
  ];

(* --- Meridian altitude / noon-sight latitude (Item 15) --- *)

(* Latitude from meridian altitude (the noon sight).
   At LAN the navigational triangle collapses: Ho is the maximum solar altitude
   and the Sun bears due north or south.  The zenith distance z = 90 - Ho.
   bearing = "S": Sun south of observer (lat > dec) → lat = dec + z
   bearing = "N": Sun north of observer (lat ≤ dec) → lat = dec - z
   No longitude or time information is required — only the altitude peak — so
   the result is immune to chronometer error (contrast longitude from sights,
   which suffers 0.25*cos(lat) nm/s from clock bias). *)
cnNoonLatitude[hoDeg_?NumericQ, decDeg_?NumericQ, bearing_String] :=
  Module[{z = 90.0 - hoDeg},
    Which[
      bearing === "S", decDeg + z,
      bearing === "N", decDeg - z,
      True, Message[cnNoonLatitude::bearing, bearing]; $Failed
    ]
  ];
cnNoonLatitude::bearing = "bearing must be \"S\" (Sun south) or \"N\" (Sun north); got `1`.";

(* Geometric meridian altitude and bearing.
   Returns {altDeg, bearing} where altDeg = 90 - |lat - dec| is the maximum
   altitude the Sun reaches as it crosses the observer's meridian, and
   bearing = "S" when lat > dec (Sun south), "N" when lat ≤ dec (Sun north).
   Use as a helper: feed altDeg as Ho into cnNoonLatitude with the same bearing
   to recover lat exactly (round-trip). *)
cnMeridianAltitude[latDeg_?NumericQ, decDeg_?NumericQ] :=
  {90.0 - Abs[latDeg - decDeg],
   If[latDeg > decDeg, "S", "N"]};

(* Approximate UTC of Local Apparent Noon at a given longitude.
   The Sun crosses the observer's meridian (hour angle = 0) when the Greenwich
   Hour Angle of the Sun equals the observer's west longitude, i.e., when
   GHA_Sun = 360 - lonDeg (for east-positive lon).  In mean-sun terms this
   corresponds to local mean noon = 12:00 minus the east longitude in hours.
   UTC_LAN ≈ 12:00 - lonDeg / 15  (hours)
   This ignores the equation of time (EoT), which varies ±16 min across the year.
   For a planning estimate (when to watch for the Sun's maximum altitude) this
   simple formula is standard DR practice.  The returned DateObject carries
   TimeZone -> 0 so it can be passed directly to cnSunGP and similar functions.
   dateObj provides the calendar date; only year/month/day are used. *)
cnLANTimeUTC[lonDeg_?NumericQ, dateObj_] :=
  Module[{y, m, d, lanHour, lanH, lanMin, lanSec},
    {y, m, d} = DateList[dateObj][[{1, 2, 3}]];
    lanHour = 12.0 - lonDeg / 15.0;         (* decimal UTC hours *)
    lanH    = IntegerPart[lanHour];
    lanMin  = IntegerPart[(lanHour - lanH) * 60];
    lanSec  = ((lanHour - lanH) * 60 - lanMin) * 60;
    DateObject[{y, m, d, lanH, lanMin, lanSec}, TimeZone -> 0]
  ];

(* --- Lunar-distance method (longitude without a chronometer) --- *)

(* Abridged ELP lunar theory — Meeus, Astronomical Algorithms, ch. 47.
   Table 47.A: {D, M, Mp, F, Sigma_l (1e-6 deg), Sigma_r (1e-3 km)}.  Sigma_l multiplies
   Sin, Sigma_r multiplies Cos of the argument (D*D + M*M + Mp*Mp + F*F).  Terms with a
   nonzero solar mean anomaly M are scaled by E (or E^2 if |M|=2) for the eccentricity of
   Earth's orbit. *)
cnMoonTblA = {
{0,0,1,0,6288774,-20905355},{2,0,-1,0,1274027,-3699111},{2,0,0,0,658314,-2955968},
{0,0,2,0,213618,-569925},{0,1,0,0,-185116,48888},{0,0,0,2,-114332,-3149},
{2,0,-2,0,58793,246158},{2,-1,-1,0,57066,-152138},{2,0,1,0,53322,-170733},
{2,-1,0,0,45758,-204586},{0,1,-1,0,-40923,-129620},{1,0,0,0,-34720,108743},
{0,1,1,0,-30383,104755},{2,0,0,-2,15327,10321},{0,0,1,2,-12528,0},
{0,0,1,-2,10980,79661},{4,0,-1,0,10675,-34782},{0,0,3,0,10034,-23210},
{4,0,-2,0,8548,-21636},{2,1,-1,0,-7888,24208},{2,1,0,0,-6766,30824},
{1,0,-1,0,-5163,-8379},{1,1,0,0,4987,-16675},{2,-1,1,0,4036,-12831},
{2,0,2,0,3994,-10445},{4,0,0,0,3861,-11650},{2,0,-3,0,3665,14403},
{0,1,-2,0,-2689,-7003},{2,0,-1,2,-2602,0},{2,-1,-2,0,2390,10056},
{1,0,1,0,-2348,6322},{2,-2,0,0,2236,-9884},{0,1,2,0,-2120,5751},
{0,2,0,0,-2069,0},{2,-2,-1,0,2048,-4950},{2,0,1,-2,-1773,4130},
{2,0,0,2,-1595,0},{4,-1,-1,0,1215,-3958},{0,0,2,2,-1110,0},
{3,0,-1,0,-892,3258},{2,1,1,0,-810,2616},{4,-1,-2,0,759,-1897},
{0,2,-1,0,-713,-2117},{2,2,-1,0,-700,2354},{2,1,-2,0,691,0},
{2,-1,0,-2,596,0},{4,0,1,0,549,-1423},{0,0,4,0,537,-1117},
{4,-1,0,0,520,-1571},{1,0,-2,0,-487,-1739},{2,1,0,-2,-399,0},
{0,0,2,-2,-381,-4421},{1,1,1,0,351,0},{3,0,-2,0,-340,0},
{4,0,-3,0,330,0},{2,-1,2,0,327,0},{0,2,1,0,-323,1165},
{1,1,-1,0,299,0},{2,0,3,0,294,0},{2,0,-1,-2,0,8752}};
(* Table 47.B: {D, M, Mp, F, Sigma_b (1e-6 deg)} — ecliptic latitude. *)
cnMoonTblB = {
{0,0,0,1,5128122},{0,0,1,1,280602},{0,0,1,-1,277693},{2,0,0,-1,173237},
{2,0,-1,1,55413},{2,0,-1,-1,46271},{2,0,0,1,32573},{0,0,2,1,17198},
{2,0,1,-1,9266},{0,0,2,-1,8822},{2,-1,0,-1,8216},{2,0,-2,-1,4324},
{2,0,1,1,4200},{2,1,0,-1,-3359},{2,-1,-1,1,2463},{2,-1,0,1,2211},
{2,-1,-1,-1,2065},{0,1,-1,-1,-1870},{4,0,-1,-1,1828},{0,1,0,1,-1794},
{0,0,0,3,-1749},{0,1,-1,1,-1565},{1,0,0,1,-1491},{0,1,1,1,-1475},
{0,1,1,-1,-1410},{0,1,0,-1,-1344},{1,0,0,-1,-1335},{0,0,3,1,1107},
{4,0,0,-1,1021},{4,0,-1,1,833},{0,0,1,-3,777},{4,0,-2,1,671},
{2,0,0,-3,607},{2,0,2,-1,596},{2,-1,1,-1,491},{2,0,-2,1,-451},
{0,0,3,-1,439},{2,0,2,1,422},{2,0,-3,-1,421},{2,1,-1,1,-366},
{2,1,0,1,-351},{4,0,0,1,331},{2,-1,1,1,315},{2,-2,0,-1,302},
{0,0,1,3,-283},{2,1,1,-1,-229},{1,1,0,-1,223},{1,1,0,1,223},
{0,1,-2,-1,-220},{2,1,-1,-1,-220},{1,0,1,1,-185},{2,-1,-2,-1,181},
{0,1,2,1,-177},{4,0,-2,-1,176},{4,-1,-1,-1,166},{1,0,1,-1,-164},
{4,0,1,-1,132},{1,0,-1,-1,-119},{4,-1,0,-1,115},{2,-2,0,1,107}};

cnMoonPosition[t_] := Module[
  {jd, T, Lp, D, M, Mp, F, A1, A2, A3, E, sl, sr, sb, lam, bet, Delta,
   omega, lSun, dpsi, deps, eps0, eps, raDeg, decDeg, hpDeg, eMul},
  jd = JulianDate[t];
  T  = (jd - 2451545.0)/36525.0;
  Lp = Mod[218.3164477 + 481267.88123421 T - 0.0015786 T^2 + T^3/538841 - T^4/65194000, 360];   (* mean longitude *)
  D  = Mod[297.8501921 + 445267.1114034 T - 0.0018819 T^2 + T^3/545868 - T^4/113065000, 360];   (* mean elongation *)
  M  = Mod[357.5291092 + 35999.0502909 T - 0.0001536 T^2 + T^3/24490000, 360];                  (* Sun mean anomaly *)
  Mp = Mod[134.9633964 + 477198.8675055 T + 0.0087414 T^2 + T^3/69699 - T^4/14712000, 360];     (* Moon mean anomaly *)
  F  = Mod[93.2720950 + 483202.0175233 T - 0.0036539 T^2 - T^3/3526000 + T^4/863310000, 360];   (* argument of latitude *)
  A1 = Mod[119.75 + 131.849 T, 360];
  A2 = Mod[53.09 + 479264.290 T, 360];
  A3 = Mod[313.45 + 481266.484 T, 360];
  E  = 1 - 0.002516 T - 0.0000074 T^2;
  eMul[m_] := Switch[Abs[m], 0, 1, 1, E, _, E^2];
  sl = Sum[row[[5]] eMul[row[[2]]] Sin[(row[[1]] D + row[[2]] M + row[[3]] Mp + row[[4]] F) Degree], {row, cnMoonTblA}];
  sr = Sum[row[[6]] eMul[row[[2]]] Cos[(row[[1]] D + row[[2]] M + row[[3]] Mp + row[[4]] F) Degree], {row, cnMoonTblA}];
  sb = Sum[row[[5]] eMul[row[[2]]] Sin[(row[[1]] D + row[[2]] M + row[[3]] Mp + row[[4]] F) Degree], {row, cnMoonTblB}];
  sl += 3958 Sin[A1 Degree] + 1962 Sin[(Lp - F) Degree] + 318 Sin[A2 Degree];
  sb += -2235 Sin[Lp Degree] + 382 Sin[A3 Degree] + 175 Sin[(A1 - F) Degree] +
        175 Sin[(A1 + F) Degree] + 127 Sin[(Lp - Mp) Degree] - 115 Sin[(Lp + Mp) Degree];
  lam   = Lp + sl/1000000.0;          (* geometric ecliptic longitude, deg *)
  bet   = sb/1000000.0;               (* ecliptic latitude, deg *)
  Delta = 385000.56 + sr/1000.0;      (* geocentric distance, km *)
  (* Nutation in longitude/obliquity (principal terms) for the apparent place. *)
  omega = Mod[125.04452 - 1934.136261 T, 360];
  lSun  = Mod[280.4665 + 36000.7698 T, 360];
  dpsi = (-17.20 Sin[omega Degree] - 1.32 Sin[2 lSun Degree] - 0.23 Sin[2 Lp Degree] + 0.21 Sin[2 omega Degree])/3600.0;
  deps = (9.20 Cos[omega Degree] + 0.57 Cos[2 lSun Degree] + 0.10 Cos[2 Lp Degree] - 0.09 Cos[2 omega Degree])/3600.0;
  lam  = lam + dpsi;                  (* apparent ecliptic longitude *)
  eps0 = 23.4392911 - 0.0130041667 T - 1.638889*^-7 T^2 + 5.036111*^-7 T^3;  (* mean obliquity *)
  eps  = eps0 + deps;
  raDeg  = Mod[ArcTan[Cos[lam Degree], Sin[lam Degree] Cos[eps Degree] - Tan[bet Degree] Sin[eps Degree]]/Degree, 360];
  decDeg = ArcSin[Sin[bet Degree] Cos[eps Degree] + Cos[bet Degree] Sin[eps Degree] Sin[lam Degree]]/Degree;
  hpDeg  = ArcSin[6378.14/Delta]/Degree;
  {raDeg, decDeg, Delta, hpDeg}
];

cnMoonGP[t_] := Module[{mp, raDeg, decDeg, gha, lonGP},
  mp = cnMoonPosition[t];
  raDeg = mp[[1]]; decDeg = mp[[2]];
  gha = Mod[cnGMST[t] - raDeg, 360];
  lonGP = Mod[-gha + 180, 360] - 180;
  {decDeg, lonGP}
];

cnMoonHP[t_] := cnMoonPosition[t][[4]];

(* Geocentric Moon-Sun angular distance = great-circle angle between the two GPs.
   (The GP direction from geocenter is exactly the body direction; lat = dec, lon = -GHA.) *)
cnLunarDistanceGeocentric[t_] := Module[{m = cnMoonGP[t], s = cnSunGP[t]},
  ArcCos[Clip[Sin[m[[1]] Degree] Sin[s[[1]] Degree] +
    Cos[m[[1]] Degree] Cos[s[[1]] Degree] Cos[(m[[2]] - s[[2]]) Degree], {-1, 1}]] / Degree
];

(* Clear a topocentric (observed) Moon-Sun distance to the geocentric distance.
   moonAlt, sunAlt are APPARENT (refracted, topocentric) altitudes in degrees.
   The azimuth difference A between the two bodies is invariant under refraction and
   parallax (both act along the vertical circle), so cos A is computed from the apparent
   altitudes and the observed distance, then the geocentric distance is rebuilt from the
   geocentric altitudes (apparent + parallax - refraction). *)
cnClearLunarDistance[dObs_?NumericQ, moonAlt_?NumericQ, sunAlt_?NumericQ,
                     moonHP_?NumericQ, sunHP_:0.002443] := Module[
  {rM, pM, rS, pS, hMg, hSg, cosA},
  rM = cnRefraction[moonAlt]/60.;                              (* deg *)
  pM = ArcSin[Sin[moonHP Degree] Cos[moonAlt Degree]]/Degree;  (* Moon parallax in altitude, deg *)
  rS = cnRefraction[sunAlt]/60.;
  pS = sunHP Cos[sunAlt Degree];                               (* small-angle Sun parallax, deg *)
  hMg = moonAlt - rM + pM;                                     (* geocentric Moon altitude *)
  hSg = sunAlt - rS + pS;                                      (* geocentric Sun altitude *)
  cosA = (Cos[dObs Degree] - Sin[moonAlt Degree] Sin[sunAlt Degree]) /
         (Cos[moonAlt Degree] Cos[sunAlt Degree]);
  ArcCos[Clip[Sin[hMg Degree] Sin[hSg Degree] + Cos[hMg Degree] Cos[hSg Degree] cosA, {-1, 1}]] / Degree
];

(* Invert the monotonic distance-vs-time relation to recover GMT.
   Two starting points drive a secant FindRoot (the objective is a black-box numeric
   function, so no symbolic derivative is available). *)
cnLunarDistanceGMT[dGeo_?NumericQ, tApprox_] := Module[{t0, f, sol, x},
  t0 = AbsoluteTime[tApprox];
  f[xx_?NumericQ] := cnLunarDistanceGeocentric[FromAbsoluteTime[t0 + xx, TimeZone -> 0]] - dGeo;
  sol = x /. FindRoot[f[x], {x, 0., 1800.}];
  FromAbsoluteTime[t0 + sol, TimeZone -> 0]
];

(* --- Recursive Bayesian / Extended Kalman Filter running fix (R2) --- *)

(* 2-state position EKF.  State x = {lat, lon} (deg), covariance P in nm^2 in a
   local East/North tangent plane re-centred on the current estimate each step.
   PREDICT: advance by the DR increment (drPos[k]-drPos[k-1]); inflate P by Q.
   UPDATE : per sight, innovation = (Ho - Hc)*60 nm (the Marcq St-Hilaire
   intercept); Jacobian H = {Sin[Zn], Cos[Zn]} (1 arcmin altitude = 1 nm along Zn);
   R = sigmaMin^2.  K = P H' / (H P H' + R); x += K*innov; P = (I - K H) P.
   Sights are generated from truePos at distinct times (distinct azimuths) so the
   per-day update is well-conditioned (same idealisation as cnMonteCarloFix). *)
cnEKFVoyage[voyage_List, sightsPerDay_Integer:3, opts_:<||>] := Module[
  {o, sigmaMin, ie, Q, R, body, height, spread, sightOpts, initPos, P0,
   offsets, estPos, P, records, day, t, truePos, drNow, drPrev, Ppred,
   noonUTC, ti, gp, hsBody, Ho, hc, zn, innov, Hvec, Sval, Kvec, dx, errNm, drErrNm},
  o = Join[<|"sigmaMin"->1.0, "indexErrorMin"->0.0, "processNoiseNm"->4.0,
             "initialPos"->Automatic, "initialCovNm"->20.0, "sightHoursSpread"->3.0,
             "body"->"Sun", "heightMeters"->2.0, "seed"->Automatic|>, opts];
  sigmaMin = o["sigmaMin"]; ie = o["indexErrorMin"];
  Q = o["processNoiseNm"]^2 IdentityMatrix[2];
  R = sigmaMin^2;
  body = o["body"]; height = o["heightMeters"]; spread = o["sightHoursSpread"];
  If[o["seed"] =!= Automatic, SeedRandom[o["seed"]]];
  sightOpts = <|"sigmaMin"->sigmaMin, "indexErrorMin"->ie, "heightMeters"->height|>;
  initPos = If[o["initialPos"] === Automatic, voyage[[1]]["drPos"], o["initialPos"]];
  P0 = o["initialCovNm"]^2 IdentityMatrix[2];
  offsets = If[sightsPerDay <= 1, {0.}, N @ Subdivide[-spread, spread, sightsPerDay - 1]];
  estPos = N @ initPos; P = N @ P0; records = {};
  Do[
    day = voyage[[k]]; t = day["t"]; truePos = day["truePos"]; drNow = day["drPos"];
    (* PREDICT: advance estimate by the DR increment, inflate covariance by Q *)
    If[k > 1,
      drPrev = voyage[[k-1]]["drPos"];
      estPos = estPos + (drNow - drPrev);
      P = P + Q;
    ];
    Ppred = P;
    (* UPDATE: sightsPerDay Sun sights spread about LOCAL apparent noon (so the
       Sun is high and the azimuths fan S over morning/noon/afternoon, giving a
       well-conditioned fix at every longitude along the voyage). *)
    noonUTC = cnLANTimeUTC[truePos[[2]], t];
    Do[
      ti = DatePlus[noonUTC, {offsets[[s]], "Hour"}];
      gp = cnBodyGPFor[body, ti];
      hsBody = cnGenerateSightBody[truePos, ti, body, sightOpts];
      (* navigator reduces assuming ZERO index error (the systematic bias is unknown) *)
      Ho = cnObservedAltitudeBody[hsBody, height, 0.0, body];
      {hc, zn} = cnComputedAltitude[estPos, gp];
      innov = (Ho - hc) 60.;                       (* nm, = intercept *)
      Hvec = {Sin[zn Degree], Cos[zn Degree]};      (* E/N, points along Zn *)
      Sval = Hvec . P . Hvec + R;
      Kvec = (P . Hvec)/Sval;
      dx = Kvec innov;                              (* {E, N} nm correction *)
      estPos = {estPos[[1]] + dx[[2]]/60.,
                estPos[[2]] + dx[[1]]/(60. Cos[estPos[[1]] Degree])};
      P = (IdentityMatrix[2] - Outer[Times, Kvec, Hvec]) . P;
      P = (P + Transpose[P])/2;                     (* keep symmetric *)
    , {s, Length[offsets]}];
    errNm  = QuantityMagnitude[GeoDistance[GeoPosition[truePos], GeoPosition[estPos]]/Quantity[1,"NauticalMiles"]];
    drErrNm = QuantityMagnitude[GeoDistance[GeoPosition[truePos], GeoPosition[drNow]]/Quantity[1,"NauticalMiles"]];
    AppendTo[records, <|"day"->day["day"], "t"->t, "estPos"->estPos, "truePos"->truePos,
      "drPos"->drNow, "covNm"->P, "covPredictNm"->Ppred,
      "errVsTruthNm"->errNm, "drErrorNm"->drErrNm|>];
  , {k, Length[voyage]}];
  records
];

(* Monte-Carlo fix distribution WITH a constant index error on every sight.
   Generate WITH indexErrorMin, reduce WITHOUT correcting it: the bias leaks into
   Ho as a common altitude offset, shifting the fix mean (bias) while leaving the
   fix covariance (scatter) unchanged (covariance is translation-invariant).
   Bias = mean offset from truth (E/N nm); scatter = covariance (nm^2). *)
cnSystematicFixError[truePos_, t_, bodies_List, sigmaMin_?NumericQ,
                     indexErrorMin_?NumericQ, n_:500] := Module[
  {times, fixes, p0, toXY, fixXY, mean, cov},
  times = If[ListQ[t], t, ConstantArray[t, Length[bodies]]];
  fixes = Table[
    cnFix[MapThread[Function[{body, ti},
      cnReduceSightBody[
        cnGenerateSightBody[truePos, ti, body,
          <|"sigmaMin"->sigmaMin, "indexErrorMin"->indexErrorMin|>],
        ti, truePos, body, <|"indexErrorMin"->0.0|>]],
      {bodies, times}]],
    {n}];
  p0 = truePos;
  toXY[{lat_, lon_}] := {(lon - p0[[2]]) Cos[p0[[1]] Degree] 60, (lat - p0[[1]]) 60};
  fixXY = toXY /@ fixes;
  mean = Mean[fixXY];
  cov  = Covariance[fixXY];
  <|"fixes"->fixes, "biasVecNm"->mean, "biasNm"->Norm[mean],
    "scatterCov"->cov, "scatterTraceNm2"->Tr[cov], "indexErrorMin"->indexErrorMin|>
];

(* --- Bayesian posterior over position (R3) --- *)

(* Full posterior probability surface over a position grid, flat prior.
   Each sight contributes a Gaussian log-likelihood on its altitude residual
   (Ho - geometric altitude at the grid point), the residual converted from
   degrees to arcmin so sigmaMin (arcmin) is the natural noise scale
   (1 arcmin altitude <-> 1 nm along the LOP normal).  GPs are computed ONCE
   per sight (not per grid cell).  The log-posterior is exponentiated after
   subtracting its max (numerical stability) and normalised to sum to 1.
   Array convention: rows index lat (lats), columns index lon (lons) — the
   orientation ArrayPlot/MatrixPlot expect (DataReversed flips lat to point up). *)
cnPosteriorGrid[sights_List, {{latMin_, latMax_}, {lonMin_, lonMax_}},
                nGrid_Integer, sigmaMin_?NumericQ] := Module[
  {lats, lons, gps, hos, logpost, maxLog, post, total, idx, mapEst},
  lats = N @ Subdivide[latMin, latMax, nGrid - 1];
  lons = N @ Subdivide[lonMin, lonMax, nGrid - 1];
  gps  = cnBodyGPFor[#["body"], #["t"]] & /@ sights;     (* one GP per sight *)
  hos  = #["Ho"] & /@ sights;
  (* Unnormalised log-posterior (flat prior): rows = lat, cols = lon. *)
  logpost = Table[
    Sum[
      ((hos[[k]] - cnAltitudeFromGP[{lat, lon}, gps[[k]]]) 60. / sigmaMin)^2,
      {k, Length[sights]}] (-0.5),
    {lat, lats}, {lon, lons}];
  maxLog = Max[logpost];
  (* Exp of far-from-peak cells underflows to 0 (negligible probability) — the
     desired behaviour; quiet the harmless machine-underflow warning, then Chop
     the resulting denormalised tinies to exact 0 so downstream consumers (e.g.
     Raster rendering) don't re-trigger the underflow message.  Chopping before
     normalising keeps Total[post,2] exactly 1. *)
  post   = Chop[Quiet[Exp[logpost - maxLog], General::munfl], 1*^-12];  (* max ~ 1 *)
  total  = Total[post, 2];
  post   = post / total;                 (* normalised: Total[post,2] == 1 *)
  idx    = First @ Position[post, Max[post], {2}, 1];
  mapEst = {lats[[idx[[1]]]], lons[[idx[[2]]]]};
  <|"lats" -> lats, "lons" -> lons,
    "logPosterior" -> logpost, "posterior" -> post, "mapEstimate" -> mapEst|>
];

End[];
EndPackage[];
