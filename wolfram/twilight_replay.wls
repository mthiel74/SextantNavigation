#!/usr/bin/env wolframscript
(* twilight_replay.wls  --  Item 16: Real twilight star-fix replay, Atlantic crossing
   Writes:   data/twilight_fixes.json
   Exports:  docs/images/fig_twilight_replay.png
   Offline. Light theme. ImageSize -> 1600. No Inset in frameless Graphics.
   Named chars only (\[Degree] etc). *)

$CharacterEncoding = "UTF-8";

repoBase    = ParentDirectory[DirectoryName[$InputFileName]];
pkgFile     = FileNameJoin[{repoBase, "wolfram",        "CelestialNavigation.wl"}];
voyageFile  = FileNameJoin[{repoBase, "data",           "voyage.csv"}];
sightsFile  = FileNameJoin[{repoBase, "data",           "sights.json"}];
jsonOutFile = FileNameJoin[{repoBase, "data",           "twilight_fixes.json"}];
pngOutFile  = FileNameJoin[{repoBase, "docs", "images", "fig_twilight_replay.png"}];

Get[pkgFile];
Print["Package loaded. cnVersion = ", cnVersion[]];

(* ---- load voyage CSV ---- *)
rawCSV = Import[voyageFile, "CSV"];
header = First[rawCSV];
rows   = Rest[rawCSV];
voyage = Association[Thread[header -> #]] & /@ rows;
Print["Loaded ", Length[voyage], " voyage days."];

(* ---- load sights.json for Sun running-fix errors ---- *)
fixJSON[x_List] := If[Length[x] > 0 && AllTrue[x, MatchQ[#, _Rule] &],
  Association[fixJSON /@ x], fixJSON /@ x];
fixJSON[x_Rule] := Rule[x[[1]], fixJSON[x[[2]]]];
fixJSON[x_]     := x;
sightsData = fixJSON[Import[sightsFile, "JSON"]];
sunErrMap  = Association[#["day"] -> #["runningFixErrorNm"] & /@ sightsData["days"]];

(* ---- helpers ---- *)
baseDate = DateObject[{2024, 11, 15, 0, 0, 0}, TimeZone -> 0];
dtToStr[t_] := DateString[t,
  {"Year","-","Month","-","Day","T","Hour24",":","Minute",":","Second"},
  TimeZone -> 0] <> "Z";
rd[x_?NumericQ, n_:4] := N[Round[N[x], 10^-n]];
geoDist[p1_, p2_] := QuantityMagnitude[
  GeoDistance[GeoPosition[p1], GeoPosition[p2]] / Quantity[1, "NauticalMiles"]];

(* ---- colours (light theme) ---- *)
colBlue  = RGBColor[0.25, 0.45, 0.95];
colNavy  = RGBColor[0.13, 0.29, 0.53];
colWarm  = RGBColor[0.85, 0.50, 0.20];
colGreen = RGBColor[0.20, 0.68, 0.35];
colGrid  = GrayLevel[0.85];
colLabel = GrayLevel[0.12];

(* ==========================================================
   MAIN LOOP: evening twilight star fix per voyage day
   ========================================================== *)
Print["Computing twilight fixes..."];
twilightRecs = {};
skipped      = {};

Do[
  v       = voyage[[i]];
  dayIdx  = v["day"];
  truePos = {N[v["latTrue"]], N[v["lonTrue"]]};
  drPos   = {N[v["latDR"]],   N[v["lonDR"]]};

  (* LAN UTC hour; evening twilight ~4-6 h after LAN *)
  noonHrs     = 12.0 - truePos[[2]] / 15.0;
  dayMidnight = DatePlus[baseDate, {dayIdx, "Days"}];

  (* Scan for time when sun altitude ~= -9 deg (nautical twilight mid-point).
     Scan LAN+2h .. LAN+9h in 5-min steps. *)
  scanDts = Range[2.0, 9.0, 1/12.0];
  scanTs  = DatePlus[dayMidnight, {noonHrs + #, "Hours"}] & /@ scanDts;
  sunAlts = cnAltitudeFromGP[truePos, cnSunGP[#]] & /@ scanTs;
  bestIdx = First[Ordering[Abs[sunAlts + 9.0], 1]];
  twT     = scanTs[[bestIdx]];
  twAlt   = sunAlts[[bestIdx]];

  (* Skip if sun never gets close enough to -9 deg *)
  If[Abs[twAlt + 9.0] > 4.5,
    AppendTo[skipped, <|"day" -> dayIdx,
      "reason" -> "Sun alt at closest scan = " <> ToString[N[twAlt, 3]] <> "\[Degree]"|>];
    Continue[]
  ];

  (* Visible navigational stars and best GDOP triplet.
     Use drPos (the navigator's assumed position) for star selection — this is
     what a real navigator does, and avoids azimuth instability when a star's
     declination nearly equals the TRUE latitude but not the DR latitude. *)
  visible = cnVisibleStars[drPos, twT, 15];
  If[Length[visible] < 3,
    AppendTo[skipped, <|"day" -> dayIdx,
      "reason" -> ToString[Length[visible]] <> " stars visible (need >=3)"|>];
    Continue[]
  ];

  trip = cnBestStarTriplet[drPos, twT, 15];
  If[trip["gdop"] > 9000,
    AppendTo[skipped, <|"day" -> dayIdx, "reason" -> "Degenerate GDOP"|>];
    Continue[]
  ];

  stars = trip["stars"];
  gdop  = trip["gdop"];
  azs   = trip["azimuths"];

  (* Generate 3 star sights at truePos (sigma=1'), reduce at drPos *)
  lops = Table[
    Module[{body = {"Star", stars[[si]]}, hs},
      hs = cnGenerateSightBody[truePos, twT, body,
             <|"sigmaMin" -> 1.0, "seed" -> dayIdx * 100 + si|>];
      cnReduceSightBody[hs, twT, drPos, body]
    ], {si, 3}];

  fix   = cnFix[lops];
  errNm = geoDist[truePos, fix];
  cht   = cnCockedHat[lops];

  If[errNm > 20.0,
    Print["WARNING day ", dayIdx, ": fix error = ", N[errNm, 3], " nm -- check ephemeris"]
  ];

  Print["  Day ", dayIdx, ": ", StringRiffle[stars, ", "],
        "  GDOP=", N[Round[gdop, 0.001], 4],
        "  err=", N[Round[errNm, 0.01]], " nm",
        "  sunAlt=", N[Round[twAlt, 0.1]], "\[Degree]"];

  AppendTo[twilightRecs, <|
    "day"              -> dayIdx,
    "twilightUTC"      -> dtToStr[twT],
    "sunAltDeg"        -> rd[twAlt, 3],
    "nVisible"         -> Length[visible],
    "chosenStars"      -> stars,
    "gdop"             -> rd[gdop, 4],
    "azimuths"         -> Map[rd[#, 3] &, azs],
    "fix"              -> Map[rd[#, 5] &, fix],
    "errorNm"          -> rd[errNm, 3],
    "cockedHatAreaNm2" -> rd[cht["areaNm2"], 4]
  |>],
  {i, 1, Length[voyage]}
];

nValid   = Length[twilightRecs];
meanErr  = N[Mean[#["errorNm"] & /@ twilightRecs]];
meanGDOP = N[Mean[#["gdop"]    & /@ twilightRecs]];

Print[""];
Print["===== SANITY CHECK ====="];
Print["Valid twilight fixes: ", nValid, " / ", Length[voyage]];
Print["Mean twilight-fix error: ", N[Round[meanErr,  0.01]], " nm  (expect ~0.5-3 nm)"];
Print["Mean GDOP:               ", N[Round[meanGDOP, 0.01]], "  (expect ~1.1-2)"];
If[Length[skipped] > 0,
  Print["Skipped days: ", Length[skipped]];
  Do[Print["  Day ", s["day"], ": ", s["reason"]], {s, skipped}]
];

allChosen = Flatten[#["chosenStars"] & /@ twilightRecs];
starFreq  = ReverseSortBy[Tally[allChosen], Last];
Print["Top stars (appearances in optimal triplet):"];
Do[Print["  ", sf[[1]], " x", sf[[2]]], {sf, Take[starFreq, Min[8, Length[starFreq]]]}];

(* ---- write JSON ---- *)
jsonOut = <|
  "note"    -> ("Per-day evening twilight star fixes along the Atlantic crossing. "<>
                "Uses real navigational star catalogue + cnBestStarTriplet for optimal selection. "<>
                "Generated by wolfram/twilight_replay.wls (Item 16)."),
  "summary" -> <|
    "nDaysValid"        -> nValid,
    "meanTwilightFixNm" -> rd[meanErr,  3],
    "meanGDOP"          -> rd[meanGDOP, 3],
    "topStars"          -> Take[starFreq, Min[5, Length[starFreq]]]
  |>,
  "days" -> twilightRecs
|>;
Export[jsonOutFile, jsonOut, "JSON"];
Print["Wrote: ", jsonOutFile];

(* ==========================================================
   FIGURE
   ========================================================== *)
Print["Building figure..."];

(* ---- Panel A data ---- *)
validDays  = #["day"]     & /@ twilightRecs;
twErrors   = #["errorNm"] & /@ twilightRecs;
sunErrors  = sunErrMap /@ validDays;   (* Sun RF error for the same days *)
meanSunErr = N[Mean[sunErrors]];

(* ---- Panel A: error comparison ListLinePlot ---- *)
panelA = ListLinePlot[
  {Transpose[{validDays, twErrors}],
   Transpose[{validDays, sunErrors}]},
  PlotStyle -> {
    Directive[colBlue, AbsoluteThickness[2.5]],
    Directive[colWarm,  AbsoluteThickness[2.5], Dashing[{0.022, 0.012}]]
  },
  PlotMarkers -> {
    {Style["\[FilledCircle]", colBlue, 10], Scaled[0.015]},
    {Style["\[FilledSquare]", colWarm, 10], Scaled[0.015]}
  },
  Frame      -> True,
  FrameStyle -> Directive[GrayLevel[0.40], AbsoluteThickness[0.9]],
  FrameLabel -> {
    {Style["Fix error (nm)", FontFamily -> "Helvetica", FontSize -> 18, colNavy], None},
    {Style["Voyage day", FontFamily -> "Helvetica", FontSize -> 18, colNavy], None}
  },
  FrameTicks -> {
    {Automatic, None},
    {Table[{d, Style[ToString[d], FontFamily -> "Helvetica", FontSize -> 14, colLabel]},
       {d, 0, 23, 2}], None}
  },
  FrameTicksStyle -> Directive[FontSize -> 14],
  PlotRange   -> {{-0.5, 23.5}, {0, Automatic}},
  GridLines   -> {Range[0, 23, 2], Automatic},
  GridLinesStyle -> Directive[GrayLevel[0.88], AbsoluteThickness[0.6]],
  PlotLabel   -> Style[
    "Twilight Star Fix vs Sun Running Fix \[LongDash] Atlantic Crossing Nov\[Dash]Dec 2024",
    FontFamily -> "Helvetica", FontSize -> 19, Bold, colNavy],
  PlotLegends -> Placed[
    LineLegend[
      {Directive[colBlue, AbsoluteThickness[2.5]],
       Directive[colWarm,  AbsoluteThickness[2.5], Dashing[{0.022, 0.012}]]},
      {"Twilight star fix (3-star, real catalogue, optimal GDOP)",
       "Sun running fix (3 sights/day, 6 h spread)"},
      LegendFunction -> (Framed[#,
        FrameStyle -> GrayLevel[0.88], Background -> White, RoundingRadius -> 4] &),
      LabelStyle -> Directive[FontFamily -> "Helvetica", FontSize -> 15, colNavy]
    ], {0.66, 0.86}],
  Epilog -> {
    (* Twilight mean line *)
    {colBlue, Opacity[0.7], AbsoluteThickness[1.1], Dashing[{0.018, 0.008}],
     Line[{{-0.5, meanErr}, {23.5, meanErr}}]},
    {colBlue, Text[
      Style["Mean \[Star] fix: " <> ToString[NumberForm[N[meanErr, 3], {3, 2}]] <> " nm",
        FontFamily -> "Helvetica", FontSize -> 14, colBlue],
      {0.5, meanErr + 0.13}, {-1, -1}]},
    (* Sun mean line *)
    {colWarm, Opacity[0.7], AbsoluteThickness[1.1], Dashing[{0.018, 0.008}],
     Line[{{-0.5, meanSunErr}, {23.5, meanSunErr}}]},
    {colWarm, Text[
      Style["Mean \[Sun] fix: " <> ToString[NumberForm[N[meanSunErr, 3], {3, 2}]] <> " nm",
        FontFamily -> "Helvetica", FontSize -> 14, colWarm],
      {0.5, meanSunErr + 0.13}, {-1, 1}]},
    (* GDOP annotation bottom-right *)
    {GrayLevel[0.35], Text[
      Style["Mean GDOP = " <> ToString[NumberForm[N[meanGDOP, 3], {3, 2}]],
        FontFamily -> "Helvetica", FontSize -> 14, Italic, GrayLevel[0.35]],
      {22.5, 0.18}, {1, -1}]}
  },
  Background -> White,
  ImageSize  -> {1000, 480}
];

(* ---- Panel B: polar sky chart for the day with median GDOP ---- *)
medGDOP = N[Median[#["gdop"] & /@ twilightRecs]];
repRec  = twilightRecs[[First[Ordering[Abs[#["gdop"] - medGDOP] & /@ twilightRecs, 1]]]];
repDay  = repRec["day"];
repStars = repRec["chosenStars"];

(* Recompute twilight time for the representative day *)
repVoyRow   = SelectFirst[voyage, #["day"] == repDay &];
repPosTrue  = {N[repVoyRow["latTrue"]], N[repVoyRow["lonTrue"]]};
noonHrsRep  = 12.0 - repPosTrue[[2]] / 15.0;
dayMidRep   = DatePlus[baseDate, {repDay, "Days"}];
scanTsRep   = DatePlus[dayMidRep, {noonHrsRep + #, "Hours"}] & /@ Range[2.0, 9.0, 1/12.0];
sunAltsRep  = cnAltitudeFromGP[repPosTrue, cnSunGP[#]] & /@ scanTsRep;
twTrep      = scanTsRep[[First[Ordering[Abs[sunAltsRep + 9.0], 1]]]];

allStarData = cnLoadStars[];
repDrRow    = SelectFirst[voyage, #["day"] == repDay &];
repDrPos    = {N[repDrRow["latDR"]], N[repDrRow["lonDR"]]};
visRep      = cnVisibleStars[repDrPos, twTrep, 15];
repAzs      = repRec["azimuths"];

(* Helper: star XY position in polar alt-az (r=(90-alt)/90, N up, E right)
   Uses repDrPos (the navigator's AP) — same position used for star selection. *)
starXYrep[name_] := Module[{hcaz},
  hcaz = cnComputedAltitude[repDrPos, cnBodyGPFor[{"Star", name}, twTrep]];
  With[{r = (90. - hcaz[[1]]) / 90., az = hcaz[[2]]},
    {r Sin[az Degree], r Cos[az Degree]}]
];

(* Grid *)
altCircPrims = Table[
  With[{r = (90. - a) / 90.},
    {colGrid, AbsoluteThickness[0.6], Circle[{0., 0.}, r]}],
  {a, {15, 30, 45, 60, 75}}];

azLinePrims = Table[
  With[{az = a Degree},
    {colGrid, AbsoluteThickness[0.5], Line[{{0., 0.}, {Sin[az], Cos[az]}}]}],
  {a, 0, 330, 30}];

horizCirc = {GrayLevel[0.5], AbsoluteThickness[1.4], Circle[{0., 0.}, 1.]};

minAltRing = {Directive[colBlue, Opacity[0.35], AbsoluteThickness[1.1], Dashing[{0.013, 0.008}]],
  Circle[{0., 0.}, (90. - 15.) / 90.]};

compassLbls = {
  {colLabel, Text[Style["N", FontFamily -> "Helvetica", FontSize -> 15, Bold], {0.,  1.13}]},
  {colLabel, Text[Style["E", FontFamily -> "Helvetica", FontSize -> 15, Bold], { 1.13, 0.}]},
  {colLabel, Text[Style["S", FontFamily -> "Helvetica", FontSize -> 15, Bold], {0., -1.13}]},
  {colLabel, Text[Style["W", FontFamily -> "Helvetica", FontSize -> 15, Bold], {-1.13, 0.}]}};

altLbls = Table[
  With[{r = (90. - a) / 90.},
    {colLabel, Text[Style[ToString[a] <> "\[Degree]",
      FontFamily -> "Helvetica", FontSize -> 11], {r + 0.04, 0.03}, {-1, -1}]}],
  {a, {30, 45, 60}}];

(* Best-triplet azimuth spokes *)
spokePrims = Table[
  With[{az = az0 Degree},
    {colWarm, Opacity[0.65], AbsoluteThickness[1.2], Dashing[{0.015, 0.010}],
     Line[{{0., 0.}, {Sin[az], Cos[az]}}]}],
  {az0, repAzs}];

(* Star dots *)
dotR[mag_] := Max[0.011, 0.035 - 0.008 mag];
starDotPrims = Table[
  Module[{xy = starXYrep[s], mag = allStarData[s]["mag"], r, inBest},
    inBest = MemberQ[repStars, s];
    r = dotR[mag];
    {If[inBest, colWarm, colBlue],
     Disk[xy, If[inBest, r * 1.45, r]]}],
  {s, visRep}];

(* Star labels: chosen stars + bright ones (mag < 1.5) *)
labelSet = Union[repStars, Select[visRep, allStarData[#]["mag"] < 1.5 &]];
starLblPrims = Table[
  Module[{xy = starXYrep[s], inBest = MemberQ[repStars, s]},
    With[{col = If[inBest, colWarm, colLabel],
          dir = Normalize[xy + {0.001, 0.}]},
      {col, Text[Style[s, FontFamily -> "Helvetica", FontSize -> 12,
        If[inBest, Bold, Plain]],
        xy + 0.07 dir, {-1, 0}]}]],
  {s, labelSet}];

(* Gap annotations between chosen-triplet spokes *)
sortedAz  = Sort[N[repAzs]];
gaps      = Mod[Differences[Append[sortedAz, sortedAz[[1]] + 360.]], 360.];
gapMidAzs = Table[Mod[sortedAz[[k]] + gaps[[k]] / 2., 360.], {k, 3}];
gapAnnotPrims = Table[
  With[{az = gapMidAzs[[k]] Degree, g = gaps[[k]]},
    If[g < 200.,
      {colWarm, Text[Style[ToString[Round[g, 0.1]] <> "\[Degree]",
        FontFamily -> "Helvetica", FontSize -> 13, Bold],
        0.38 {Sin[az], Cos[az]}]},
      Nothing]],
  {k, 3}];

(* Titles using Text (not Inset) *)
titleB1 = Text[Style[
  "Day " <> ToString[repDay] <> " \[LongDash] Evening Twilight Sky",
  FontFamily -> "Helvetica", FontSize -> 18, Bold, colNavy], {0., 1.31}];

titleB2 = Text[Style[
  StringRiffle[repStars, ", "],
  FontFamily -> "Helvetica", FontSize -> 14, Bold, colWarm], {0., 1.215}];

titleB3 = Text[Style[
  "GDOP = " <> ToString[NumberForm[N[repRec["gdop"], 3], {3, 2}]] <>
  "   err = " <> ToString[NumberForm[N[repRec["errorNm"], 3], {3, 2}]] <> " nm",
  FontFamily -> "Helvetica", FontSize -> 14, colNavy], {0., -1.27}];

panelB = Graphics[
  {White, Rectangle[{-1.45, -1.45}, {1.45, 1.45}],
   azLinePrims, altCircPrims, minAltRing, horizCirc,
   spokePrims, starDotPrims, gapAnnotPrims, starLblPrims,
   compassLbls, altLbls,
   titleB1, titleB2, titleB3},
  PlotRange    -> {{-1.45, 1.45}, {-1.36, 1.36}},
  Background   -> White,
  ImageSize    -> {880, 840},
  ImagePadding -> {{25, 25}, {50, 50}}
];

(* ---- Combine panels and export (stacked vertically for legibility) ---- *)
combined = GraphicsGrid[
  {{panelA}, {panelB}},
  Spacings   -> {0, 12},
  Background -> White,
  ImageSize  -> 1000
];

img = Rasterize[combined, Background -> White, ImageSize -> {1000, Automatic}];
img = ImagePad[ImagePad[img, -3], 3, White];
Export[pngOutFile, img, "PNG"];
Print["Exported: ", pngOutFile];

(* ---- verify ---- *)
dims = ImageDimensions[img];
sz   = FileByteCount[pngOutFile];
Print["Image dimensions: ", dims, " pixels"];
Print["File size: ", sz, " bytes  (", Round[sz / 1024.], " KB)"];

pv1 = PixelValue[img, {8, 8}];
pv2 = PixelValue[img, {dims[[1]] - 8, dims[[2]] - 8}];
pv3 = PixelValue[img, {Round[dims[[1]] / 2], Round[dims[[2]] / 2]}];
Print["PixelValue top-left:     ", N[pv1, 3]];
Print["PixelValue bottom-right: ", N[pv2, 3]];
Print["PixelValue centre:       ", N[pv3, 3]];
If[Min[pv1[[{1, 2, 3}]]] > 0.90 && Min[pv2[[{1, 2, 3}]]] > 0.90,
  Print["Background check OK: corners are white"],
  Print["WARNING: corners not white -- check rendering"]];
Print["Done."];
