#!/usr/bin/env wolframscript
(* fetch_horizons.wls — JPL Horizons ephemeris validation
   Queries the JPL Horizons API for Sun astrometric RA/DEC (geocentric),
   computes GHA via the same GMST used in CelestialNavigation.wl, writes
   data/horizons_sun.csv, and plots declination + GHA residuals vs the
   low-precision package formulas.

   JPL Horizons data: NASA/JPL, public domain. *)

(* ── 0. Setup ─────────────────────────────────────────────────────────── *)
scriptDir  = DirectoryName[$InputFileName];
repoRoot   = ParentDirectory[scriptDir];
packagePath = FileNameJoin[{scriptDir, "CelestialNavigation.wl"}];
dataDir    = FileNameJoin[{repoRoot, "data"}];
imgDir     = FileNameJoin[{repoRoot, "docs", "images"}];

(* Load the package (provides cnSunGP, used for dec comparison) *)
If[Check[Get[packagePath], $Failed] === $Failed,
  Print["ERROR: failed to load package ", packagePath];
  Exit[1]];

(* Create output directories if absent (guard each level) *)
If[!DirectoryQ[dataDir], CreateDirectory[dataDir]];
If[!DirectoryQ[FileNameJoin[{repoRoot, "docs"}]],
  CreateDirectory[FileNameJoin[{repoRoot, "docs"}]]];
If[!DirectoryQ[imgDir], CreateDirectory[imgDir]];

(* ── 1. Query JPL Horizons API ────────────────────────────────────────── *)
(* COMMAND='10' = Sun, CENTER='500@399' = Earth geocenter
   EPHEM_TYPE=OBSERVER, QUANTITIES='2' = apparent RA/DEC, referred to the
   true equator and equinox OF DATE. This matches the package's low-precision
   Almanac formula, which is itself an apparent-position-of-date approximation
   (precession folded in via the mean-longitude rate, obliquity of date).
   Using '1' (astrometric J2000 ICRF) would inject a ~250" precession
   systematic that masks the formula's true ~10-40" accuracy. *)
horizonsURL = "https://ssd.jpl.nasa.gov/api/horizons.api";
queryParams = <|
  "format"     -> "text",
  "COMMAND"    -> "'10'",
  "OBJ_DATA"   -> "'NO'",
  "MAKE_EPHEM" -> "'YES'",
  "EPHEM_TYPE" -> "'OBSERVER'",
  "CENTER"     -> "'500@399'",
  "START_TIME" -> "'2024-Nov-15 12:00'",
  "STOP_TIME"  -> "'2024-Dec-08 12:00'",
  "STEP_SIZE"  -> "'1 d'",
  "QUANTITIES" -> "'2'"
|>;

Print["[fetch_horizons] Querying JPL Horizons API (timeout 60 s)…"];
resp = Quiet[
  Check[
    URLRead[
      HTTPRequest[<|"URL" -> horizonsURL, "Query" -> queryParams|>],
      {"StatusCode", "Body"},
      TimeConstraint -> 60
    ],
    $Failed
  ],
  {URLRead::invhttp, URLRead::timeout, General::stop}
];

If[resp === $Failed,
  Print["ERROR: Network request failed (connection error or timeout)."];
  Print["DONE_WITH_CONCERNS: network unavailable in this environment; ",
        "run fetch_horizons.wls where Horizons is reachable."];
  Exit[1]
];

{statusCode, body} = resp;

If[statusCode =!= 200,
  Print["ERROR: HTTP status ", statusCode, " (expected 200)."];
  Print["Response body (first 800 chars):\n",
        StringTake[body, UpTo[800]]];
  Exit[1]
];

Print["[fetch_horizons] HTTP ", statusCode, " OK. Parsing ephemeris table…"];

(* ── 2. Parse the Horizons text table ────────────────────────────────── *)
soePos = StringPosition[body, "$$SOE"];
eoePos = StringPosition[body, "$$EOE"];
If[Length[soePos] == 0 || Length[eoePos] == 0,
  Print["ERROR: $$SOE/$$EOE markers not found in response."];
  Print["Body (first 1000 chars):\n", StringTake[body, UpTo[1000]]];
  Exit[1]
];

tableText = StringTake[body, {soePos[[1, 2]] + 1, eoePos[[1, 1]] - 1}];
rawLines  = Select[StringSplit[tableText, "\n"], StringLength[StringTrim[#]] > 5 &];
Print["[fetch_horizons] Found ", Length[rawLines], " ephemeris lines."];

(* Observer table format (QUANTITIES='1'):
     2024-Nov-15 12:00 [flags]  HH MM SS.ss  ±DD MM SS.s  ...
   flags are optional (e.g. "*m" for lighting condition).
   Regex: match date/time, then skip optional flags, then capture RA HMS and DEC DMS. *)
linePattern = RegularExpression[
  "(\\d{4}-\\w{3}-\\d{2} \\d{2}:\\d{2}).*?" <>
  "(\\d{1,2}) (\\d{2}) (\\d{2}\\.\\d+)\\s+" <>
  "([+\\-]?\\d{1,3}) (\\d{2}) (\\d{1,2}\\.\\d+)"
];

(* Month name → number *)
monthNum = <|"Jan"->1,"Feb"->2,"Mar"->3,"Apr"->4,"May"->5,"Jun"->6,
             "Jul"->7,"Aug"->8,"Sep"->9,"Oct"->10,"Nov"->11,"Dec"->12|>;

horizonsDateObject[str_String] := Module[
  {dp, tp},
  dp = StringSplit[str, " "];           (* {"2024-Nov-15", "12:00"} *)
  tp = StringSplit[dp[[2]], ":"];       (* {"12", "00"} *)
  With[{ymd = StringSplit[dp[[1]], "-"]},
    DateObject[{ToExpression[ymd[[1]]], monthNum[ymd[[2]]],
                ToExpression[ymd[[3]]], ToExpression[tp[[1]]],
                ToExpression[tp[[2]]], 0}, TimeZone -> 0]
  ]
];

horizonsDateISO[str_String] := Module[
  {dp, tp, ymd},
  dp = StringSplit[str, " "];
  tp = StringSplit[dp[[2]], ":"];
  ymd = StringSplit[dp[[1]], "-"];
  StringJoin[ymd[[1]], "-",
    IntegerString[monthNum[ymd[[2]]], 10, 2], "-",
    ymd[[3]], "T", dp[[2]], ":00Z"]
];

parseLine[line_String] := Module[
  {m, dateStr, raH, raM, raS, decDStr, decM, decS,
   raDeg, decSign, decDeg},
  m = StringCases[line, linePattern ->
    {"$1","$2","$3","$4","$5","$6","$7"}, 1];
  If[Length[m] == 0, Return[$Failed]];
  {dateStr, raH, raM, raS, decDStr, decM, decS} = m[[1]];
  {raH, raM, raS, decM, decS} =
    ToExpression /@ {raH, raM, raS, decM, decS};
  decSign = If[StringStartsQ[decDStr, "-"], -1, 1];
  decDeg  = decSign (Abs[ToExpression[decDStr]] + decM/60.0 + decS/3600.0);
  raDeg   = 15.0 (raH + raM/60.0 + raS/3600.0);
  {dateStr, raDeg, decDeg}
];

parsed = Select[parseLine /@ rawLines, # =!= $Failed &];
nRows = Length[parsed];
If[nRows == 0,
  Print["ERROR: No rows parsed from ephemeris table."];
  Print["First 3 raw lines:\n", StringJoin[Riffle[rawLines[[;;Min[3,Length[rawLines]]]], "\n"]]];
  Exit[1]
];
Print["[fetch_horizons] Parsed ", nRows, " rows."];

(* ── 3. Compute GMST-based GHA from Horizons RA ──────────────────────── *)
(* Replicate package's cnGAST formula so GHA comparison isolates only the
   RA formula error, not any GMST discrepancy.
   GHA_horizons = GMST_pkg(t) − RA_horizons  (degrees, mod 360) *)
pkgGMST[t_] := Module[{d},
  d = JulianDate[t] - 2451545.0;
  Mod[18.697375 + 24.065709824279 d, 24] * 15.0
];

(* Package low-precision GHA (replicates cnSunGHA) *)
pkgGHA[t_] := Module[{d, L, g, lam, eps, raDeg},
  d   = JulianDate[t] - 2451545.0;
  L   = Mod[280.460 + 0.9856474  d, 360];
  g   = Mod[357.528 + 0.9856003  d, 360];
  lam = L + 1.915 Sin[g Degree] + 0.020 Sin[2 g Degree];
  eps = 23.439 - 0.0000004 d;
  raDeg = ArcTan[Cos[lam Degree], Cos[eps Degree] Sin[lam Degree]] / Degree;
  Mod[pkgGMST[t] - Mod[raDeg, 360], 360]
];

(* Build result table:
   {isoDateStr, horizonsDecDeg, horizonsRAdeg, horizonsGHAdeg} *)
rows = Table[
  {dateStr, hRA, hDec} = parsed[[i]];
  t      = horizonsDateObject[dateStr];
  isoStr = horizonsDateISO[dateStr];
  hGHA   = Mod[pkgGMST[t] - hRA, 360];
  {isoStr, hDec, hRA, hGHA}
, {i, nRows}];

(* ── 4. Write data/horizons_sun.csv ──────────────────────────────────── *)
csvPath = FileNameJoin[{dataDir, "horizons_sun.csv"}];
csvData = Prepend[rows, {"datetimeUTC", "decDeg", "raDeg", "ghaDeg"}];
Export[csvPath, csvData, "CSV"];
Print["[fetch_horizons] Wrote ", csvPath];

(* ── 5. Compute residuals: package − Horizons ────────────────────────── *)
residuals = Table[
  {isoStr, hDec, hRA, hGHA} = rows[[i]];
  t = horizonsDateObject[parsed[[i, 1]]];
  {pkgDec, _} = cnSunGP[t];
  pGHA        = pkgGHA[t];
  decRes  = (pkgDec - hDec) * 3600.0;          (* arcsec *)
  (* Wrap GHA residual to ±180° before converting to arcsec *)
  ghaRes  = (Mod[pGHA - hGHA + 180, 360] - 180) * 3600.0;
  {horizonsDateObject[parsed[[i,1]]], decRes, ghaRes}
, {i, nRows}];

decResids = residuals[[All, 2]];
ghaResids = residuals[[All, 3]];

medDec = Median[Abs[decResids]];
maxDec = Max[Abs[decResids]];
medGHA = Median[Abs[ghaResids]];
maxGHA = Max[Abs[ghaResids]];

Print[""];
Print["── Declination residuals (package − JPL Horizons, arcsec) ──"];
Print["   Median |residual|: ", NumberForm[medDec, {6, 2}], "\""];
Print["   Max    |residual|: ", NumberForm[maxDec, {6, 2}], "\""];
Print["── GHA residuals (arcsec) ──"];
Print["   Median |residual|: ", NumberForm[medGHA, {6, 2}], "\""];
Print["   Max    |residual|: ", NumberForm[maxGHA, {6, 2}], "\""];
Print[""];

(* ── 6. Plot and save figure ─────────────────────────────────────────── *)
dateTimes = residuals[[All, 1]];
decData   = Transpose[{dateTimes, decResids}];
ghaData   = Transpose[{dateTimes, ghaResids}];

plt = DateListPlot[
  {decData, ghaData},
  PlotLegends  -> Placed[{"Declination", "GHA"}, {0.15, 0.85}],
  PlotLabel    -> Style[
    "Ephemeris Residuals: Low-Precision Package \[Minus] JPL Horizons\n" <>
    "Geocentric, Astrometric (J2000), 2024-Nov-15 to 2024-Dec-08",
    14, Bold],
  FrameLabel   -> {
    Style["Date (UTC)", 12],
    Style["Residual (arcseconds)", 12]},
  Frame        -> True,
  GridLines    -> Automatic,
  GridLinesStyle -> Directive[LightGray, Dashed],
  PlotStyle    -> {Directive[Blue, Thick], Directive[Red, Thick, Dashed]},
  ImageSize    -> 1200,
  AspectRatio  -> 1/3,
  PlotRange    -> All
];

pngPath = FileNameJoin[{imgDir, "ephemeris_residuals.png"}];
Export[pngPath, plt, "PNG", ImageResolution -> 150];
Print["[fetch_horizons] Saved figure: ", pngPath];

(* ── 7. Summary ──────────────────────────────────────────────────────── *)
Print[""];
Print["[fetch_horizons] DONE. Summary:"];
Print["  CSV rows:                ", nRows];
Print["  Dec residual median:     ", NumberForm[medDec, {6, 2}], "\"  (expect <~40\")"];
Print["  Dec residual max:        ", NumberForm[maxDec, {6, 2}], "\""];
Print["  GHA residual median:     ", NumberForm[medGHA, {6, 2}], "\""];
Print["  GHA residual max:        ", NumberForm[maxGHA, {6, 2}], "\""];
