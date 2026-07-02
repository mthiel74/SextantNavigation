(* posterior_tests.wl -- Bayesian posterior over position, cnPosteriorGrid (R3).
   Bare List of VerificationTest, picked up by tests/run_tests.wls (which has
   already loaded CelestialNavigation.wl).

   Shared scenario: true position {20 N, 40 W}, three noise-free Sun sights on
   2024-Nov-22 at 10:00, 13:00, 16:00 UTC.  Ho is the exact geometric altitude
   from the almanac GP (no noise, no corrections), so a zero-residual peak sits
   on the true position.  Grid box is centred on truth with an ODD node count
   (61) so the true position is exactly a grid node — the MAP equals truth to
   machine precision in the noise-free limit.

   Four tests:
     1. Normalisation : posterior sums to 1 over the grid.
     2. MAP accuracy  : 3 noise-free sights -> MAP within ~1 nm of truth.
     3. Single ridge  : 1 sight -> posterior variance along the LOP >> across it.
     4. Sharpening    : entropy strictly decreases as sights 1 -> 2 -> 3 are added.
*)

{
  (* -- Test 1: normalisation (sums to ~1) ---------------------------------- *)
  VerificationTest[
    Module[{truePos, ts, mk, sights, box, r},
      truePos = {20., -40.};
      ts = {DateObject[{2024,11,22,10,0,0}, TimeZone->0],
            DateObject[{2024,11,22,13,0,0}, TimeZone->0],
            DateObject[{2024,11,22,16,0,0}, TimeZone->0]};
      mk[t_] := <|"body"->"Sun", "t"->t,
                  "Ho"->cnAltitudeFromGP[truePos, cnSunGP[t]]|>;
      sights = mk /@ ts;
      box = {{19.75, 20.25}, {-40.25, -39.75}};
      r = cnPosteriorGrid[sights, box, 61, 1.5];
      Abs[Total[r["posterior"], 2] - 1.] < 1*^-10
    ],
    True,
    TestID -> "posterior-normalised"],

  (* -- Test 2: MAP within ~1 nm of truth (3 noise-free sights) ------------- *)
  VerificationTest[
    Module[{truePos, ts, mk, sights, box, r, map, errNm},
      truePos = {20., -40.};
      ts = {DateObject[{2024,11,22,10,0,0}, TimeZone->0],
            DateObject[{2024,11,22,13,0,0}, TimeZone->0],
            DateObject[{2024,11,22,16,0,0}, TimeZone->0]};
      mk[t_] := <|"body"->"Sun", "t"->t,
                  "Ho"->cnAltitudeFromGP[truePos, cnSunGP[t]]|>;
      sights = mk /@ ts;
      box = {{19.75, 20.25}, {-40.25, -39.75}};
      r = cnPosteriorGrid[sights, box, 61, 1.5];
      map = r["mapEstimate"];
      errNm = QuantityMagnitude[
        GeoDistance[GeoPosition[truePos], GeoPosition[map]] /
          Quantity[1, "NauticalMiles"]];
      errNm < 1.0
    ],
    True,
    TestID -> "posterior-map-accuracy"],

  (* -- Test 3: one sight yields a ridge (along-LOP variance >> across) ------ *)
  VerificationTest[
    Module[{truePos, t, sight, box, r, lats, lons, p, toEN, pts, w, mu, cen,
            cov, ev},
      truePos = {20., -40.};
      t = DateObject[{2024,11,22,13,0,0}, TimeZone->0];
      sight = <|"body"->"Sun", "t"->t,
                "Ho"->cnAltitudeFromGP[truePos, cnSunGP[t]]|>;
      box = {{19.5, 20.5}, {-40.5, -39.5}};
      r = cnPosteriorGrid[{sight}, box, 61, 1.5];
      lats = r["lats"]; lons = r["lons"]; p = r["posterior"];
      (* posterior-weighted covariance in local E/N nm; eigenratio = ridge aspect *)
      toEN[la_, lo_] := {(lo - truePos[[2]]) Cos[truePos[[1]] Degree] 60,
                          (la - truePos[[1]]) 60};
      pts = Flatten[Table[toEN[lats[[i]], lons[[j]]],
              {i, Length[lats]}, {j, Length[lons]}], 1];
      w   = Flatten[p];
      mu  = w . pts;
      cen = (# - mu) & /@ pts;
      cov = Sum[w[[k]] Outer[Times, cen[[k]], cen[[k]]], {k, Length[w]}];
      ev  = Sort[Eigenvalues[cov], Greater];
      ev[[1]] / ev[[2]] > 10.
    ],
    True,
    TestID -> "posterior-single-ridge"],

  (* -- Test 4: adding sights sharpens (entropy decreases) ------------------ *)
  VerificationTest[
    Module[{truePos, ts, mk, sights, box, ent, e1, e2, e3},
      truePos = {20., -40.};
      ts = {DateObject[{2024,11,22,10,0,0}, TimeZone->0],
            DateObject[{2024,11,22,13,0,0}, TimeZone->0],
            DateObject[{2024,11,22,16,0,0}, TimeZone->0]};
      mk[t_] := <|"body"->"Sun", "t"->t,
                  "Ho"->cnAltitudeFromGP[truePos, cnSunGP[t]]|>;
      sights = mk /@ ts;
      box = {{19.5, 20.5}, {-40.5, -39.5}};
      ent[ss_] := Module[{pp = Flatten[cnPosteriorGrid[ss, box, 61, 1.5]["posterior"]]},
        pp = Select[pp, # > 0 &];
        -Total[pp Log[pp]]];
      e1 = ent[sights[[;; 1]]];
      e2 = ent[sights[[;; 2]]];
      e3 = ent[sights[[;; 3]]];
      e1 > e2 > e3
    ],
    True,
    TestID -> "posterior-sharpening"]
}
