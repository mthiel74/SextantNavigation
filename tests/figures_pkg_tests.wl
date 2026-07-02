(* figures_pkg_tests.wl -- asserts every sxFig...[] in SextantFigures` returns *)
(* a valid graphics expression. Bare List of VerificationTest, picked up by    *)
(* tests/run_tests.wls (which has already loaded CelestialNavigation.wl).       *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..", "wolfram", "SextantFigures.wl"}]];

sxFigTestHeadsQ[e_] := MatchQ[e,
  _Graphics | _GeoGraphics | _Image | _Legended | _Graphics3D |
  _Row | _Column | _GraphicsRow | _GraphicsColumn | _Grid | _Labeled | _Panel];

{
  (* figures.wls-derived (9) *)
  VerificationTest[sxFigTestHeadsQ[sxFigAltitudeCircle[]],       True, TestID -> "figpkg-AltitudeCircle"],
  VerificationTest[sxFigTestHeadsQ[sxFigIntercept[]],            True, TestID -> "figpkg-Intercept"],
  VerificationTest[sxFigTestHeadsQ[sxFigCockedHat[]],            True, TestID -> "figpkg-CockedHat"],
  VerificationTest[sxFigTestHeadsQ[sxFigRunningFix[]],           True, TestID -> "figpkg-RunningFix"],
  VerificationTest[sxFigTestHeadsQ[sxFigCorrectionsWaterfall[]], True, TestID -> "figpkg-CorrectionsWaterfall"],
  VerificationTest[sxFigTestHeadsQ[sxFigSubsolarTrack[]],        True, TestID -> "figpkg-SubsolarTrack"],
  VerificationTest[sxFigTestHeadsQ[sxFigErrorEllipse[]],         True, TestID -> "figpkg-ErrorEllipse"],
  VerificationTest[sxFigTestHeadsQ[sxFigCutAngle[]],             True, TestID -> "figpkg-CutAngle"],
  VerificationTest[sxFigTestHeadsQ[sxFigCelestialVsGPS[]],       True, TestID -> "figpkg-CelestialVsGPS"],

  (* single-figure scripts (18) *)
  VerificationTest[sxFigTestHeadsQ[sxFigPZXTriangle[]],          True, TestID -> "figpkg-PZXTriangle"],
  VerificationTest[sxFigTestHeadsQ[sxFigCelestialSphere[]],      True, TestID -> "figpkg-CelestialSphere"],
  VerificationTest[sxFigTestHeadsQ[sxFigEquationOfTime[]],       True, TestID -> "figpkg-EquationOfTime"],
  VerificationTest[sxFigTestHeadsQ[sxFigHorizonDip[]],           True, TestID -> "figpkg-HorizonDip"],
  VerificationTest[sxFigTestHeadsQ[sxFigRefraction[]],           True, TestID -> "figpkg-Refraction"],
  VerificationTest[sxFigTestHeadsQ[sxFigCockedHatTheorem[]],     True, TestID -> "figpkg-CockedHatTheorem"],
  VerificationTest[sxFigTestHeadsQ[sxFigCRLB[]],                 True, TestID -> "figpkg-CRLB"],
  VerificationTest[sxFigTestHeadsQ[sxFigChronometer[]],          True, TestID -> "figpkg-Chronometer"],
  VerificationTest[sxFigTestHeadsQ[sxFigNoonSight[]],            True, TestID -> "figpkg-NoonSight"],
  VerificationTest[sxFigTestHeadsQ[sxFigStarSelection[]],        True, TestID -> "figpkg-StarSelection"],
  VerificationTest[sxFigTestHeadsQ[sxFigStarChart[]],            True, TestID -> "figpkg-StarChart"],
  VerificationTest[sxFigTestHeadsQ[sxFigErrorBudget[]],          True, TestID -> "figpkg-ErrorBudget"],
  VerificationTest[sxFigTestHeadsQ[sxFigSpeeds[]],               True, TestID -> "figpkg-Speeds"],
  VerificationTest[sxFigTestHeadsQ[sxFigTwilightReplay[]],       True, TestID -> "figpkg-TwilightReplay"],
  VerificationTest[sxFigTestHeadsQ[sxFigEphemerisValidation[]],  True, TestID -> "figpkg-EphemerisValidation"],
  VerificationTest[sxFigTestHeadsQ[sxFigLunarDistance[]],        True, TestID -> "figpkg-LunarDistance"],
  VerificationTest[sxFigTestHeadsQ[sxFigEKF[]],                  True, TestID -> "figpkg-EKF"],
  VerificationTest[sxFigTestHeadsQ[sxFigHistorical[]],           True, TestID -> "figpkg-Historical"],

  (* Bayesian posterior (R3) *)
  VerificationTest[sxFigTestHeadsQ[sxFigPosterior[]],            True, TestID -> "figpkg-Posterior"]
}
