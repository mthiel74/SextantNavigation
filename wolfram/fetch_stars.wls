#!/usr/bin/env wolframscript
(* fetch_stars.wls — Refresh data/nav_stars.csv from Wolfram StarData.
   Run on a networked machine to validate/update the embedded coordinates.

   EMBEDDED COORDINATES NOTE:
   The 10 anchor stars (Sirius, Vega, Polaris, Arcturus, Betelgeuse, Rigel,
   Canopus, Capella, Antares, Spica) were verified against textbook J2000.0
   values from the Hipparcos catalogue and standard nautical almanac sources.
   The other 48 stars carry best-knowledge J2000.0 values accurate to arcminute
   level; this script validates and replaces them with StarData values.

   Usage:
     /usr/local/bin/wolframscript -file wolfram/fetch_stars.wls
*)

Print["fetch_stars.wls: checking Wolfram connectivity..."];

(* Test connectivity with a 5-second timeout. *)
testResult = TimeConstrained[StarData["Sirius", "EquatorialCoordinates"], 5, $Failed];
If[testResult === $Failed || FailureQ[testResult] || MissingQ[testResult],
  Print["ERROR: Could not reach Wolfram servers (or StarData returned no result)."];
  Print["Embedded nav_stars.csv was not updated."];
  Exit[1]
];

Print["Connected. Fetching J2000.0 coordinates for 58 navigational stars..."];

stars = {
  "Acamar", "Achernar", "Acrux", "Adhara", "Aldebaran",
  "Alioth", "Alkaid", "Al Na'ir", "Alnilam", "Alphard",
  "Alphecca", "Alpheratz", "Altair", "Ankaa", "Antares",
  "Arcturus", "Atria", "Avior", "Bellatrix", "Betelgeuse",
  "Canopus", "Capella", "Deneb", "Denebola", "Diphda",
  "Dubhe", "Elnath", "Eltanin", "Enif", "Fomalhaut",
  "Gacrux", "Gienah", "Hadar", "Hamal", "Kaus Australis",
  "Kochab", "Markab", "Menkar", "Menkent", "Miaplacidus",
  "Mirfak", "Nunki", "Peacock", "Polaris", "Pollux",
  "Procyon", "Rasalhague", "Regulus", "Rigel", "Rigil Kentaurus",
  "Sabik", "Schedar", "Shaula", "Sirius", "Spica",
  "Suhail", "Vega", "Zubenelgenubi"
};

(* Build rows. StarData returns EquatorialCoordinates as {ra, dec} in radians
   (as Quantity[..., "Radians"]) for some versions, or as angle Quantities.
   We convert explicitly to degrees. *)
toDegreesFromQuantity[q_] := QuantityMagnitude[UnitConvert[q, "AngularDegrees"]];

rows = {"name,raDeg,decDeg,magnitude"};
failed = {};

Do[
  coords = TimeConstrained[StarData[s, "EquatorialCoordinates"], 10, $Failed];
  mag    = TimeConstrained[StarData[s, "ApparentMagnitude"], 10, $Failed];
  If[coords === $Failed || FailureQ[coords] || MissingQ[coords],
    AppendTo[failed, s];
    Print["  WARNING: no data for ", s],
    Module[{ra, dec, raDeg, decDeg, magVal},
      ra  = coords[[1]];
      dec = coords[[2]];
      raDeg  = If[QuantityQ[ra],  toDegreesFromQuantity[ra],  ra  / Degree];
      decDeg = If[QuantityQ[dec], toDegreesFromQuantity[dec], dec / Degree];
      (* RA from StarData is in [0,360]; keep it that way. *)
      raDeg  = Mod[raDeg, 360];
      magVal = If[mag === $Failed || FailureQ[mag] || MissingQ[mag], "NA",
                  ToString[NumberForm[N[mag], {4, 2}]]];
      AppendTo[rows,
        StringRiffle[{s, ToString[NumberForm[raDeg, {8, 4}]],
                         ToString[NumberForm[decDeg, {8, 4}]],
                         magVal}, ","]]
    ]
  ],
  {s, stars}
];

If[Length[failed] > 0,
  Print["WARNING: ", Length[failed], " stars had no data: ", StringRiffle[failed, ", "]]
];

outPath = FileNameJoin[{DirectoryName[$InputFileName], "..", "data", "nav_stars.csv"}];
Export[outPath, StringRiffle[rows, "\n"], "Text"];
Print["Written ", Length[rows] - 1, " stars to ", outPath];
Print["DONE. Review diffs against anchor values before committing."];
