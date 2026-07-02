#!/usr/bin/env wolframscript
(* ================================================================
   sights.wls — Task 10: Per-day Sun sights (running fix) +
   mid-voyage twilight star fix.

   Produces: data/sights.json

   NOTE: Star GPs are SYNTHETIC (illustrative only).
   AstroValue / AstroPosition star ephemeris requires a Wolfram
   Server connection which is unavailable offline. The three star
   GPs below are chosen to give well-separated azimuths (~42°,
   ~168°, ~268°) from the mid-voyage position. They do NOT
   correspond to real star positions at this epoch.

   Run: /usr/local/bin/wolframscript -file wolfram/sights.wls
   ================================================================ *)

(* ===== Locate repo root from this script's own path ===== *)
repoBase   = ParentDirectory[DirectoryName[$InputFileName]];
pkgFile    = FileNameJoin[{repoBase, "wolfram", "CelestialNavigation.wl"}];
voyageFile = FileNameJoin[{repoBase, "data", "voyage.csv"}];
outputFile = FileNameJoin[{repoBase, "data", "sights.json"}];

(* ===== Load package ===== *)
Get[pkgFile];

(* ===== Load voyage data ===== *)
rawCSV = Import[voyageFile, "CSV"];
header = First[rawCSV];
rows   = Rest[rawCSV];
voyage = Association[Thread[header -> #]] & /@ rows;
Print["Loaded ", Length[voyage], " voyage days."];

(* ===== Helpers ===== *)

(* Round a number to n decimal places as a machine-precision float *)
rd[x_?NumericQ, n_:4] := N[Round[N[x], 10^-n]];

(* Format a DateObject as ISO 8601 UTC string.
   DateList[t] converts to $TimeZone (local); DateString with TimeZone->0 forces UTC. *)
dtToStr[t_] :=
  DateString[t,
    {"Year", "-", "Month", "-", "Day", "T",
     "Hour24", ":", "Minute", ":", "Second"},
    TimeZone -> 0] <> "Z";

(* Serialise a LOP association to JSON-safe nested structure *)
serializeLOP[lop_] := <|
  "AP"          -> Map[rd[#, 4] &, lop["AP"]],
  "Hc"          -> rd[lop["Hc"], 4],
  "Zn"          -> rd[lop["Zn"], 4],
  "Ho"          -> rd[lop["Ho"], 4],
  "interceptNm" -> rd[lop["interceptNm"], 3],
  "point"       -> Map[rd[#, 4] &, lop["point"]],
  "bearingDeg"  -> rd[lop["bearingDeg"], 4]
|>;

(* Advance {lat,lon} by dist nm along course deg using GeoDestination *)
geoAdvance[pos_, courseDeg_, distNm_] :=
  GeoDestination[GeoPosition[pos],
    {Quantity[distNm, "NauticalMiles"],
     Quantity[courseDeg, "AngularDegrees"]}][[1, ;; 2]];

(* ===== Base date: midnight UTC on the voyage start day ===== *)
baseDate = DateObject[{2024, 11, 15, 0, 0, 0}, TimeZone -> 0];

(* ===== Process each day ===== *)
Print["Computing per-day Sun sights and running fixes..."];

dayRecords = Table[
  Module[{
    v, dayIdx, latTrue, lonTrue, latDR, lonDR, course, speed,
    truePos, drPos,
    noonHrs, dayMidnight,
    t1, t2, t3,
    pos1, pos2, pos3,
    dr1, dr2, dr3,
    hs1, hs2, hs3, lop1, lop2, lop3,
    rfItems, rf, rfError
  },

  v       = voyage[[i]];
  dayIdx  = v["day"];       (* integer 0..23 *)
  latTrue = N[v["latTrue"]];
  lonTrue = N[v["lonTrue"]];
  latDR   = N[v["latDR"]];
  lonDR   = N[v["lonDR"]];
  course  = N[v["courseDeg"]];
  speed   = N[v["speedKts"]];   (* 5.0 knots throughout *)

  truePos = {latTrue, lonTrue};
  drPos   = {latDR, lonDR};

  (* Local apparent noon UTC:
     LMT noon (LMT = UTC + lon/15) → UTC_noon = 12 - lon/15.
     lon is negative for West → UTC_noon > 12:00.
     Example: lon=-15.43 → 13.03 h; lon=-61 → 16.06 h *)
  noonHrs = 12.0 - lonTrue / 15.0;

  (* Midnight UTC for this voyage day *)
  dayMidnight = DatePlus[baseDate, {dayIdx, "Days"}];

  (* Three sight times: morning = noon-3h, LAN = noon, afternoon = noon+3h *)
  t1 = DatePlus[dayMidnight, {noonHrs - 3.0, "Hours"}];
  t2 = DatePlus[dayMidnight, {noonHrs,       "Hours"}];
  t3 = DatePlus[dayMidnight, {noonHrs + 3.0, "Hours"}];

  (* True ship positions at each sight time.
     The CSV gives the noon position; t1 is 3 h before noon (ship was 15 nm
     back along the reverse course) and t3 is 3 h after noon (15 nm forward).
     Generating sights at the correct positions is essential for a valid
     running fix: if all sights were generated at the noon position, the
     advanced LOPs would not close at a single point, inflating errors by
     ~10–15 nm systematically. *)
  pos1 = geoAdvance[truePos, Mod[course + 180.0, 360.0], speed * 3.0]; (* 3 h back  *)
  pos2 = truePos;                                                          (* noon      *)
  pos3 = geoAdvance[truePos, course, speed * 3.0];                       (* 3 h ahead *)

  (* DR (assumed) positions at each sight time — same logic *)
  dr1 = geoAdvance[drPos, Mod[course + 180.0, 360.0], speed * 3.0];
  dr2 = drPos;
  dr3 = geoAdvance[drPos, course, speed * 3.0];

  (* Generate simulated sights at the correct ship position (unique seed per sight) *)
  hs1 = cnGenerateSight[pos1, t1, <|"sigmaMin" -> 1.0, "seed" -> dayIdx * 100 + 1|>];
  hs2 = cnGenerateSight[pos2, t2, <|"sigmaMin" -> 1.0, "seed" -> dayIdx * 100 + 2|>];
  hs3 = cnGenerateSight[pos3, t3, <|"sigmaMin" -> 1.0, "seed" -> dayIdx * 100 + 3|>];

  (* Reduce each sight at the DR position for that sight time *)
  lop1 = cnReduceSight[hs1, t1, dr1];
  lop2 = cnReduceSight[hs2, t2, dr2];
  lop3 = cnReduceSight[hs3, t3, dr3];

  (* Running fix: advance lop1 and lop2 to t3 (the afternoon sight time).
     t1 is 6 h before t3 → distance = speed × 6 nm.
     t2 is 3 h before t3 → distance = speed × 3 nm.
     lop3 is already at t3 → distance = 0.
     cnRunningFix advances each LOP and then calls cnFix. *)
  rfItems = {
    <|"lop" -> lop1, "course" -> course, "distance" -> speed * 6.0|>,
    <|"lop" -> lop2, "course" -> course, "distance" -> speed * 3.0|>,
    <|"lop" -> lop3, "course" -> course, "distance" -> 0.0|>
  };
  rf = cnRunningFix[rfItems];

  (* Error vs true position at t3 (the time of the running fix) *)
  rfError = QuantityMagnitude[
    GeoDistance[GeoPosition[pos3], GeoPosition[rf]] / Quantity[1, "NauticalMiles"]
  ];

  Print["  Day ", dayIdx, ": RF error = ",
        ToString[N[Round[rfError, 0.01]]], " nm  (LAN UTC ", dtToStr[t2], ")"];

  <|
    "day"               -> dayIdx,
    "datetimeNoonUTC"   -> dtToStr[v["datetimeUTC"]],
    "truePos"           -> Map[rd[#, 4] &, truePos],
    "drPos"             -> Map[rd[#, 4] &, drPos],
    "sightTimesUTC"     -> {dtToStr[t1], dtToStr[t2], dtToStr[t3]},
    "runningFix"        -> Map[rd[#, 4] &, rf],
    "runningFixErrorNm" -> rd[rfError, 3],
    "lops"              -> Map[serializeLOP, {lop1, lop2, lop3}]
  |>
  ],
  {i, 1, Length[voyage]}
];

(* ===== Summary statistics ===== *)
(* NOTE: Star fixes use real navigational star catalogue and live in
   data/twilight_fixes.json (generated by wolfram/twilight_replay.wls). *)
errors        = #["runningFixErrorNm"] & /@ dayRecords;
meanRFError   = N[Mean[errors]];
medianRFError = N[Median[errors]];

Print[""];
Print["=== SUMMARY ==="];
Print["Mean running-fix error:   ", ToString[N[Round[meanRFError,   0.01]]], " nm"];
Print["Median running-fix error: ", ToString[N[Round[medianRFError, 0.01]]], " nm"];
Print["Max running-fix error:    ", ToString[N[Round[Max[errors],   0.01]]], " nm"];

(* ===== Assemble and export JSON ===== *)
Print[""];
Print["Writing ", outputFile, " ..."];

output = <|
  "note" -> ("Per-day Sun running fixes. " <>
             "Star fixes are in data/twilight_fixes.json (real star catalogue, " <>
             "generated by wolfram/twilight_replay.wls). " <>
             "Generated by wolfram/sights.wls."),
  "summary" -> <|
    "nDays"              -> Length[voyage],
    "meanRunningFixNm"   -> rd[meanRFError,   3],
    "medianRunningFixNm" -> rd[medianRFError, 3],
    "maxRunningFixNm"    -> rd[N[Max[errors]], 3]
  |>,
  "days" -> dayRecords
|>;

(* Ensure data/ directory exists *)
Quiet[CreateDirectory[DirectoryName[outputFile]]];

Export[outputFile, output, "JSON"];
Print["Written: ", outputFile];
Print["Done."];
