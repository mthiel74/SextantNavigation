(* tests/noon_tests.wl — Meridian altitude / noon-sight latitude (Item 15)
   Tests:
     1. Round-trip South case: lat=40, dec=-10 → Hmer=40°, bearing "S"; recover lat=40.
     2. Round-trip North case: lat=-5, dec=+15 → Hmer=70°, bearing "N"; recover lat=-5.
     3. Auto-bearing correctness: cnMeridianAltitude picks the right bearing and altitude
        for several (lat,dec) pairs spanning both hemispheres.
     4. Chronometer immunity: a 60 s clock error shifts declination by ~1 arcmin/hour * 1 min
        ≈ 0.001° — latitude error must be < 0.01°.  Contrast: longitude from sights
        suffers 0.25*cos(lat) nm/s = 0.25*cos(40°) ≈ 0.19 nm per second.
*)

Join[

  (* ── Test 1: Round-trip South case ───────────────────────────────────────── *)
  (* lat=40 N, dec=-10 → Sun is south. Hmer = 90 - |40 - (-10)| = 40°.
     cnNoonLatitude[40, -10, "S"] = -10 + (90-40) = -10 + 50 = 40. *)
  {
    VerificationTest[
      Module[{hmer, brg},
        {hmer, brg} = cnMeridianAltitude[40.0, -10.0];
        Abs[hmer - 40.0] < 1*^-9 && brg === "S"
      ],
      True, TestID -> "noon-south-mer-alt"],

    VerificationTest[
      Abs[cnNoonLatitude[40.0, -10.0, "S"] - 40.0] < 1*^-9,
      True, TestID -> "noon-south-roundtrip"]
  },

  (* ── Test 2: Round-trip North case ───────────────────────────────────────── *)
  (* lat=-5, dec=+15 → Sun is north. Hmer = 90 - |-5-15| = 70°.
     cnNoonLatitude[70, 15, "N"] = 15 - (90-70) = 15 - 20 = -5. *)
  {
    VerificationTest[
      Module[{hmer, brg},
        {hmer, brg} = cnMeridianAltitude[-5.0, 15.0];
        Abs[hmer - 70.0] < 1*^-9 && brg === "N"
      ],
      True, TestID -> "noon-north-mer-alt"],

    VerificationTest[
      Abs[cnNoonLatitude[70.0, 15.0, "N"] - (-5.0)] < 1*^-9,
      True, TestID -> "noon-north-roundtrip"]
  },

  (* ── Test 3: Auto-bearing round-trips for varied (lat,dec) pairs ─────────── *)
  (* For each pair, cnMeridianAltitude gives {hmer, brg}; feeding those into
     cnNoonLatitude should recover lat to machine precision. *)
  With[
    {pairs = {{40.0, 23.4}, {-33.0, 23.4}, {0.0, -10.0},
              {51.5, -20.0}, {-10.0, -20.0}, {20.0, 20.0}}},
    Module[{results},
      results = Table[
        Module[{lat = pairs[[i, 1]], dec = pairs[[i, 2]], hmer, brg, recovered},
          {hmer, brg} = cnMeridianAltitude[lat, dec];
          recovered = cnNoonLatitude[hmer, dec, brg];
          Abs[recovered - lat] < 1*^-9
        ],
        {i, Length[pairs]}
      ];
      {
        VerificationTest[And @@ results, True,
          TestID -> "noon-auto-bearing-roundtrip"]
      }
    ]
  ],

  (* ── Test 4: Chronometer immunity ────────────────────────────────────────── *)
  (* At lat=40 N on 2024-Nov-15, compute:
     (a) true meridian altitude via cnMeridianAltitude using the true declination
         at the approximate LAN time,
     (b) declination from cnSunGP at LAN + 60 s (1-minute clock error).
     Use the true meridian altitude with the wrong declination in cnNoonLatitude;
     the latitude error is purely |decWrong - decTrue| since the formula is:
       latRecovered = decWrong + (90 - hoTrue)  [for "S" bearing]
                    = decWrong + (lat - decTrue)
     → latErr = |decWrong - decTrue| ≈ 0.0000023°/s × 60 s ≈ 0.00014°.
     Contrast: the same 60 s clock error shifts longitude by
     0.25*cos(40°)*60 ≈ 11.5 nm — five orders of magnitude larger in nm impact. *)
  With[
    {lat   = 40.0,
     lon   = -20.0,
     date  = DateObject[{2024, 11, 15}, TimeZone -> 0]},
    Module[{tLAN, decTrue, hoTrue, brg, tWrong, decWrong, latRecovered, latErr},
      tLAN       = cnLANTimeUTC[lon, date];
      decTrue    = cnSunGP[tLAN][[1]];          (* true declination at LAN *)
      (* True meridian altitude (Sun exactly on meridian) from formula *)
      {hoTrue, brg} = cnMeridianAltitude[lat, decTrue];
      (* Wrong time: +60 s clock error shifts only declination, not Ho *)
      tWrong     = DatePlus[tLAN, {60, "Second"}];
      decWrong   = cnSunGP[tWrong][[1]];        (* shifted declination *)
      (* Noon latitude using exact Ho but wrong dec — simulates clock-biased navigator *)
      latRecovered = cnNoonLatitude[hoTrue, decWrong, brg];
      latErr     = Abs[latRecovered - lat];
      {
        VerificationTest[latErr < 0.01,
          True, TestID -> "noon-chrono-immunity-60s"]
      }
    ]
  ]

]
