{
  (* Test 1: Zero-time identity — at J2000.0 epoch precession is the identity *)
  VerificationTest[
    cnPrecess[{101.29, -16.72}, DateObject[{2000, 1, 1, 12, 0, 0}, TimeZone -> 0]],
    {101.29, -16.72},
    SameTest -> (Max[Abs[#1 - #2]] < 1*^-3 &),
    TestID -> "prec-zero-time-identity"],

  (* Test 2: Rotation invariance — precession preserves angular separation *)
  VerificationTest[
    Module[{sirius  = {101.2917, -16.7161},
            vega    = {279.2347,  38.7837},
            t       = DateObject[{2024, 1, 1, 12, 0, 0}, TimeZone -> 0],
            angSep, s2, v2, sepJ2000, sepPrec},
      angSep[{ra1_, dec1_}, {ra2_, dec2_}] :=
        ArcCos[Sin[dec1 Degree] Sin[dec2 Degree] +
               Cos[dec1 Degree] Cos[dec2 Degree] Cos[(ra1 - ra2) Degree]] / Degree;
      sepJ2000 = angSep[sirius, vega];
      s2 = cnPrecess[sirius, t];
      v2 = cnPrecess[vega, t];
      sepPrec = angSep[s2, v2];
      Abs[sepPrec - sepJ2000] < 1*^-4],
    True, TestID -> "prec-rotation-invariance"],

  (* Test 3: Magnitude sanity — Aldebaran precession J2000→2024 is 0.2..0.45 deg *)
  VerificationTest[
    Module[{aldebaran = {68.9802, 16.5093},
            t         = DateObject[{2024, 1, 1, 12, 0, 0}, TimeZone -> 0],
            prec, shift},
      prec  = cnPrecess[aldebaran, t];
      shift = ArcCos[Sin[aldebaran[[2]] Degree] Sin[prec[[2]] Degree] +
                     Cos[aldebaran[[2]] Degree] Cos[prec[[2]] Degree]
                       Cos[(aldebaran[[1]] - prec[[1]]) Degree]] / Degree;
      0.2 < shift < 0.45],
    True, TestID -> "prec-aldebaran-magnitude"],

  (* Test 4a: GP self-consistent — after cnBodyGP precesses, GP lat differs from J2000 dec *)
  VerificationTest[
    Module[{t     = DateObject[{2024, 6, 15, 12, 0, 0}, TimeZone -> 0],
            radec, gp, precessed},
      radec    = cnStarRADec["Sirius"];
      gp       = cnBodyGP[radec, t];
      precessed = cnPrecess[radec, t];
      Abs[gp[[1]] - radec[[2]]] > 0.001],
    True, TestID -> "prec-gp-lat-differs-j2000"],

  (* Test 4b: body at its own GP is at the zenith (altitude 90 deg) *)
  VerificationTest[
    Module[{t     = DateObject[{2024, 6, 15, 12, 0, 0}, TimeZone -> 0],
            radec, gp},
      radec = cnStarRADec["Sirius"];
      gp    = cnBodyGP[radec, t];
      Abs[cnAltitudeFromGP[gp, gp] - 90] < 1*^-3],
    True, TestID -> "prec-gp-zenith"]
}
