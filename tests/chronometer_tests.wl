(* tests/chronometer_tests.wl — Chronometer-error longitude sensitivity (Item 12)
   Verifies:
     1. The "4-second rule": cnLongitudeErrorPerSecond[0] = 0.25 nm/s → 4 s = 1 nm (equator).
     2. cos(latitude) scaling: cnLongitudeErrorPerSecond[60] = 0.125 nm/s.
     3. Clock bias produces east/west (longitude) error, not north/south: the headline
        fact behind the Longitude Problem. |eastNm| >> |northNm| for multi-Sun fix.
     4. Linearity: doubling the clock error doubles the longitude error; zero error → zero bias.
*)

Join[

  (* ── Test 1: 4-second rule at the equator ────────────────────────────────── *)
  (* cnLongitudeErrorPerSecond[0] must equal exactly 0.25 nm/s (Cos[0] = 1).
     4 s * 0.25 nm/s = 1.0 nm — the navigator's rule. *)
  {
    VerificationTest[
      Abs[cnLongitudeErrorPerSecond[0] - 0.25] < 1*^-6,
      True, TestID -> "chrono-4sec-rule-equator"]
  },

  (* ── Test 2: cos(latitude) scaling at 60 degrees ─────────────────────────── *)
  (* cos(60°) = 0.5 → sensitivity = 0.125 nm/s. *)
  {
    VerificationTest[
      Abs[cnLongitudeErrorPerSecond[60] - 0.125] < 1*^-6,
      True, TestID -> "chrono-cos-lat-60"]
  },

  (* ── Tests 3–6: Clock bias is longitude-only ─────────────────────────────── *)
  (* Truth {15 N, 40 W}. Sun sighted at three well-spread times for distinct
     azimuths (~E, ~S, ~W) on 2024-Nov-15.  All reduced with +30 s clock offset.
     Expected total error ≈ 0.25 * 30 * cos(15°) ≈ 7.24 nm, almost entirely east. *)
  With[
    {truePos  = {15.0, -40.0},
     times    = {DateObject[{2024, 11, 15, 10, 0, 0}, TimeZone -> 0],
                 DateObject[{2024, 11, 15, 15, 0, 0}, TimeZone -> 0],
                 DateObject[{2024, 11, 15, 19, 0, 0}, TimeZone -> 0]},
     bodies   = {"Sun", "Sun", "Sun"},
     bias30   = 30.0},
    Module[{r0, r30, r60},
      r0  = cnChronometerFixError[truePos, times, bodies, 0.0];
      r30 = cnChronometerFixError[truePos, times, bodies, bias30];
      r60 = cnChronometerFixError[truePos, times, bodies, 2 bias30];
      {
        (* Test 3a: longitude (east) dominates latitude (north) by factor > 5 *)
        VerificationTest[
          Abs[r30["eastNm"]] > 5 * Abs[r30["northNm"]],
          True, TestID -> "chrono-east-dominates"],

        (* Test 3b: total error within 25% of analytical prediction *)
        VerificationTest[
          0.75 * 0.25 * 30.0 * Cos[15 Degree] < r30["totalNm"] <
          1.25 * 0.25 * 30.0 * Cos[15 Degree],
          True, TestID -> "chrono-total-error-ballpark"],

        (* Test 4a: zero clock error → zero position bias (< 0.05 nm, numerical floor) *)
        VerificationTest[
          r0["totalNm"] < 0.05,
          True, TestID -> "chrono-zero-err-zero-bias"],

        (* Test 4b: doubling the clock error doubles the east error (within 10%).
           Ratio r60/r30 should be ~2, i.e., 1.8 < ratio < 2.2. *)
        VerificationTest[
          1.8 < Abs[r60["eastNm"] / r30["eastNm"]] < 2.2,
          True, TestID -> "chrono-linear-east"]
      }
    ]
  ]

]
