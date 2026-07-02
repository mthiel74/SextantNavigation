(* interactive_tests.wl -- asserts every sxManip...[] in SextantInteractive`    *)
(* constructs a Manipulate object (R3).  Bare List of VerificationTest, picked   *)
(* up by tests/run_tests.wls (which has already loaded CelestialNavigation.wl).  *)
(* The Manipulate controls are inert headless, but the object must build without *)
(* error; full initial-frame rasterization is verified separately (slow to run   *)
(* inside the unit suite).                                                       *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..", "wolfram", "SextantInteractive.wl"}]];

{
  VerificationTest[Head[sxManipIntercept[]],     Manipulate, TestID -> "interactive-Intercept"],
  VerificationTest[Head[sxManipPosterior[]],     Manipulate, TestID -> "interactive-Posterior"],
  VerificationTest[Head[sxManipSightPlanning[]], Manipulate, TestID -> "interactive-SightPlanning"]
}
