(* path_tests.wl — A1: cnLoadStars path fix + fail-loud guards
   Tests that:
   1. cnLoadStars works correctly regardless of working directory at call time
      (simulated by checking cnPackageDir is the package dir, not cwd).
   2. The catalogue has at least 57 stars (already tested in stars_tests but repeated here
      for belt-and-suspenders coverage of the path fix).
   3. An unknown star name fails loud (returns $Failed, does NOT produce a large expression).
*)
{
  (* 1. cnPackageDir was captured at load time and points to the wolfram/ directory
        (where CelestialNavigation.wl lives), not to the current working directory. *)
  VerificationTest[
    FileExistsQ[FileNameJoin[{CelestialNavigation`Private`cnPackageDir,
                              "..", "data", "nav_stars.csv"}]],
    True,
    TestID -> "path-packagedir-correct"],

  (* 2. Catalogue loads and has at least 57 stars, verifying path resolution works. *)
  VerificationTest[
    Length[cnLoadStars[]] >= 57,
    True,
    TestID -> "path-loadstars-count"],

  (* 3. cnLoadStars now loads pmRA and pmDec columns. Verify they are numeric for Arcturus. *)
  VerificationTest[
    With[{star = cnLoadStars[]},
      NumericQ[star["Arcturus"]["pmRA"]] && NumericQ[star["Arcturus"]["pmDec"]]],
    True,
    TestID -> "path-pm-columns-loaded"],

  (* 4. Unknown star returns $Failed (loud failure, not symbolic fan-out).
        Quiet suppresses the expected message; Check catches $Failed.
        The result should be $Failed, not a large unevaluated expression. *)
  VerificationTest[
    Quiet[cnStarRADec["NoSuchStarXYZ"]],
    $Failed,
    TestID -> "path-unknown-star-returns-failed"],

  (* 5. cnBodyGPFor with unknown star also returns $Failed, not symbolic garbage.
        The expression should be $Failed (or at most a short expression), not
        a multi-hundred-token symbolic blob. *)
  VerificationTest[
    With[{result = Quiet[cnBodyGPFor[{"Star", "NoSuchStarXYZ"},
                    DateObject[{2024, 6, 21, 12, 0, 0}, TimeZone -> 0]]]},
      result === $Failed],
    True,
    TestID -> "path-bodygpfor-unknown-star-fails-loud"]
}
