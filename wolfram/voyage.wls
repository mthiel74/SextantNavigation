#!/usr/bin/env wolframscript
(* voyage.wls
   Atlantic great-circle voyage: Las Palmas → Rodney Bay, Saint Lucia
   Produces data/voyage.csv with columns:
     day, datetimeUTC, latTrue, lonTrue, latDR, lonDR, courseDeg, speedKts
*)

(* ── Constants ──────────────────────────────────────────────────────────── *)
start    = GeoPosition[{28.13,  -15.43}];   (* Las Palmas, Canary Islands *)
endPos   = GeoPosition[{14.07,  -60.95}];   (* Rodney Bay, Saint Lucia    *)
startTime = DateObject[{2024, 11, 15, 12, 0, 0}, TimeZone -> 0];

speedKts  = 5.0;
dailyNm   = speedKts * 24.0;               (* 120 nm per day *)

(* DR error model: -2° CCW course bias steers slightly to port (southward for WSW
   route, ~4.2 nm/day off-track).  A 0.3-kt NE-set current in direction course+90°
   (NNW for WSW heading) pushes ~7.2 nm/day northward.  These partially oppose,
   producing a net ~3 nm/day northward drift → realistic ~70 nm divergence over
   the passage — well within "plausible tens-of-nm" for a 22-day DR run. *)
biasAngle  = -2.0;                          (* degrees CCW from intended course *)
driftNmDay = 0.3 * 24.0;                   (* 7.2 nm/day perpendicular (NNW)  *)

(* ── Great-circle distance and number of days ───────────────────────────── *)
totalNm = QuantityMagnitude[UnitConvert[GeoDistance[start, endPos], "NauticalMiles"]];
Print["Total GC distance: ", N[totalNm, 6], " nm"];

nDays = Ceiling[totalNm / dailyNm];        (* number of daily legs; rows = nDays+1 *)
Print["Days at sea: ", nDays, "  (rows: ", nDays + 1, ")"];

(* Initial GC bearing — GeoDestination follows the geodesic from start *)
initBearing = GeoDirection[start, endPos];

(* ── Build true great-circle track (nDays+1 positions) ─────────────────── *)
trueTrack = Table[
  d = Min[i * dailyNm, totalNm];
  If[d >= totalNm,
    endPos,
    GeoDestination[start, {Quantity[d, "NauticalMiles"], initBearing}]
  ],
  {i, 0, nDays}
];

(* Course for each leg: bearing from waypoint i to waypoint i+1.
   For the last waypoint, reuse the previous leg's course (vessel has arrived). *)
courses = Table[
  If[i < nDays,
    Mod[QuantityMagnitude[GeoDirection[trueTrack[[i + 1]], trueTrack[[i + 2]]]], 360.0],
    Mod[QuantityMagnitude[GeoDirection[trueTrack[[nDays]], trueTrack[[nDays + 1]]]], 360.0]
  ],
  {i, 0, nDays}
];                                          (* courses[[i+1]] = course on day i *)

(* ── Build DR track (nDays+1 positions) ─────────────────────────────────── *)
drTrack = FoldList[
  Function[{pos, i},
    Module[{course, biasedCourse, perpDir, afterSail, afterDrift},
      course       = courses[[i + 1]];           (* course for leg i *)
      biasedCourse = Mod[course + biasAngle, 360.0];
      perpDir      = Mod[course + 90.0, 360.0];  (* port/north for WSW route *)
      afterSail    = GeoDestination[pos,
                       {Quantity[dailyNm, "NauticalMiles"],
                        Quantity[biasedCourse, "AngularDegrees"]}];
      afterDrift   = GeoDestination[afterSail,
                       {Quantity[driftNmDay, "NauticalMiles"],
                        Quantity[perpDir, "AngularDegrees"]}];
      afterDrift
    ]
  ],
  start,
  Range[0, nDays - 1]                       (* advance nDays times from start *)
];                                           (* nDays+1 GeoPosition objects     *)

(* ── Helpers ─────────────────────────────────────────────────────────────── *)
extractCoords[gp_] := gp[[1]];             (* GeoPosition[{lat,lon}] → {lat,lon} *)

isoUTC[dayOffset_] :=
  DateString[
    DatePlus[DateObject[{2024, 11, 15, 12, 0, 0}, TimeZone -> 0],
             Quantity[dayOffset, "Days"]],
    "ISODateTime"] <> "Z";

(* ── Build CSV rows ─────────────────────────────────────────────────────── *)
rows = Table[
  Module[{truePt, drPt, tc, dc},
    truePt = trueTrack[[i + 1]];
    drPt   = drTrack[[i + 1]];
    tc = extractCoords[truePt];
    dc = extractCoords[drPt];
    {i, isoUTC[i],
     N[tc[[1]], 8], N[tc[[2]], 8],
     N[dc[[1]], 8], N[dc[[2]], 8],
     N[courses[[i + 1]], 6], speedKts}
  ],
  {i, 0, nDays}
];

(* ── Export ──────────────────────────────────────────────────────────────── *)
repoDir = DirectoryName[DirectoryName[$InputFileName]];
dataDir = FileNameJoin[{repoDir, "data"}];
If[!DirectoryQ[dataDir], CreateDirectory[dataDir]];
csvPath = FileNameJoin[{dataDir, "voyage.csv"}];

header = {"day", "datetimeUTC", "latTrue", "lonTrue",
          "latDR", "lonDR", "courseDeg", "speedKts"};
Export[csvPath, Prepend[rows, header], "CSV"];
Print["CSV written: ", csvPath];

(* ── Acceptance checks ───────────────────────────────────────────────────── *)
Print["Row count (data rows): ", Length[rows]];

firstTrue = extractCoords[trueTrack[[1]]];
lastTrue  = extractCoords[trueTrack[[-1]]];
firstDR   = extractCoords[drTrack[[1]]];
lastDR    = extractCoords[drTrack[[-1]]];

Print["True first: lat=", N[firstTrue[[1]], 6], " lon=", N[firstTrue[[2]], 6]];
Print["True last:  lat=", N[lastTrue[[1]],  6], " lon=", N[lastTrue[[2]],  6]];

drDiv = QuantityMagnitude[UnitConvert[
  GeoDistance[GeoPosition[lastTrue], GeoPosition[lastDR]], "NauticalMiles"]];
Print["DR last:    lat=", N[lastDR[[1]], 6], " lon=", N[lastDR[[2]], 6]];
Print["DR vs truth divergence (endpoint): ", N[drDiv, 5], " nm"];
