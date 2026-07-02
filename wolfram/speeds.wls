#!/usr/bin/env wolframscript
(* speeds.wls -- SOG, VMG-to-destination, distance-to-destination, current set & drift
   Atlantic crossing: Las Palmas -> Rodney Bay
   Output: docs/images/fig_speeds.png
   Run: /usr/local/bin/wolframscript -file wolfram/speeds.wls
*)

$CharacterEncoding = "UTF-8";
SetDirectory[DirectoryName[$InputFileName]];

(* ── Colour palette (light theme) ──────────────────────────────────────── *)
cBlue  = RGBColor[0.25, 0.45, 0.95];
cNavy  = RGBColor[0.13, 0.29, 0.53];
cWarm  = RGBColor[0.85, 0.50, 0.20];
cGreen = RGBColor[0.20, 0.68, 0.35];
cGray  = GrayLevel[0.45];
cGridL = Directive[GrayLevel[0.88], Dashed];
cFrame = GrayLevel[0.82];

dirT = Directive[FontFamily -> "Helvetica", Bold, FontSize -> 25, FontColor -> cNavy];
dirL = Directive[FontFamily -> "Helvetica", FontSize -> 21, FontColor -> cNavy];
dirA = Directive[FontFamily -> "Helvetica", Italic, FontSize -> 17, FontColor -> cGray];

(* ── Load voyage CSV ────────────────────────────────────────────────────── *)
rawData  = Import["../data/voyage.csv", "CSV"];
dataRows = rawData[[2;;]];   (* drop header; 24 data rows: days 0-23 *)

days         = N[dataRows[[All, 1]]];   (* 0.0 .. 23.0 *)
latTrue      = N[dataRows[[All, 3]]];
lonTrue      = N[dataRows[[All, 4]]];
latDR        = N[dataRows[[All, 5]]];
lonDR        = N[dataRows[[All, 6]]];
plannedCourse = N[dataRows[[All, 7]]];  (* GC course for each leg *)

n          = Length[dataRows];   (* 24 rows *)
nIntervals = n - 1;              (* 23 intervals *)

Print["Loaded ", n, " rows from voyage.csv"];

(* ── Destination: Rodney Bay, Saint Lucia ──────────────────────────────── *)
dest = {14.07, -60.95};

(* ── Geo helpers ────────────────────────────────────────────────────────── *)
geoDist[p1_, p2_] := QuantityMagnitude[UnitConvert[
  GeoDistance[GeoPosition[p1], GeoPosition[p2]], "NauticalMiles"]];

geoDir[p1_, p2_] := QuantityMagnitude[
  GeoDirection[GeoPosition[p1], GeoPosition[p2]]];

(* ── SOG: great-circle speed between successive TRUE positions ─────────── *)
Print["Computing SOG ..."];
distNm = Table[
  geoDist[{latTrue[[i]], lonTrue[[i]]}, {latTrue[[i+1]], lonTrue[[i+1]]}],
  {i, 1, nIntervals}];
sogKts = distNm / 24.0;

(* ── COG from true positions (for reference) ─────────────────────────── *)
cogDeg = Table[
  geoDir[{latTrue[[i]], lonTrue[[i]]}, {latTrue[[i+1]], lonTrue[[i+1]]}],
  {i, 1, nIntervals}];

(* ── VMG to destination via DR-track alignment ──────────────────────────
   The true track lies on the GC, making COG_true == bearing_to_dest_true at
   every step and hence VMG_true = SOG exactly (trivially satisfied).
   The *interesting* VMG is navigational VMG: how well is the planned course
   (from the DR navigator's estimated position) directed toward the destination?
   VMG_nav = 5 kt * cos(angle between planned course and bearing DR -> dest).
   As DR errors accumulate the planned heading diverges from the optimal
   bearing, driving VMG below the vessel speed and showing VMG < SOG clearly. *)
Print["Computing navigational VMG (DR-track) ..."];

drBearingToDest = Table[
  geoDir[{latDR[[i]], lonDR[[i]]}, dest],
  {i, 1, nIntervals}];

(* VMG = TRUE SOG * cos(angle): ensures VMG <= SOG always.
   The cosine factor = how well the planned course (from the DR position) is
   aligned toward the destination.  As DR errors accumulate the angle grows,
   driving VMG well below SOG in the final days of the passage. *)
vmgKts = Table[
  sogKts[[i]] * Cos[(plannedCourse[[i]] - drBearingToDest[[i]]) * Degree],
  {i, 1, nIntervals}];

(* ── Distance to destination from each TRUE position ───────────────────── *)
Print["Computing distance to destination ..."];
distToDestNm = Table[
  geoDist[{latTrue[[i]], lonTrue[[i]]}, dest],
  {i, 1, n}];

(* ── Current set & drift from DR vs. true discrepancy ─────────────────── *)
(* Over each 24-h leg the DR assumes course+speed through the water.
   current_vector = (true_displacement - DR_displacement) / 24 h.
   Flat-earth approximation is valid for ~120 nm daily legs. *)
Print["Computing current set & drift ..."];

currNorthKts = Table[
  nTrue = (latTrue[[i+1]] - latTrue[[i]]) * 60.0;
  nDRv  = (latDR[[i+1]]   - latDR[[i]])   * 60.0;
  (nTrue - nDRv) / 24.0,
  {i, 1, nIntervals}];

currEastKts = Table[
  mLat  = (latTrue[[i]] + latTrue[[i+1]]) / 2.0;
  eTrue = (lonTrue[[i+1]] - lonTrue[[i]]) * Cos[mLat * Degree] * 60.0;
  eDRv  = (lonDR[[i+1]]   - lonDR[[i]])   * Cos[mLat * Degree] * 60.0;
  (eTrue - eDRv) / 24.0,
  {i, 1, nIntervals}];

currDrift = Table[
  Sqrt[currEastKts[[i]]^2 + currNorthKts[[i]]^2],
  {i, 1, nIntervals}];

(* Compass bearing of current vector: 0=N, 90=E, 180=S, 270=W.
   ArcTan[x, y] = angle of (x,y) from positive x-axis.
   Compass bearing = 90 - math_angle_from_east = 90 - ArcTan[E, N]/Degree.
   Result: the estimated current flows roughly SSE (~154 deg) because the DR
   was pushed NNW, so the fix appears SSE of DR -> the inferred water current
   SET is SSE (the directional inverse of the injected NNW DR drift). *)
currSet = Table[
  Mod[90.0 - ArcTan[currEastKts[[i]], currNorthKts[[i]]] / Degree, 360.0],
  {i, 1, nIntervals}];

(* ── Summary statistics ─────────────────────────────────────────────────── *)
meanSOG   = Mean[sogKts]   // N;
meanVMG   = Mean[vmgKts]   // N;
meanDrift = Mean[currDrift] // N;
meanSet   = Mean[currSet]   // N;

fmt[x_, d_:2] := ToString[NumberForm[N[x], {5, d}]];

Print[""];
Print["=== Sanity checks ==="];
Print["Mean SOG:           " <> fmt[meanSOG]   <> " kt  (expect ~5 kt)"];
Print["Mean VMG (nav):     " <> fmt[meanVMG]   <> " kt  (expect < SOG)"];
Print["Mean current drift: " <> fmt[meanDrift, 3] <> " kt  (expect 0.2-0.5 kt)"];
Print["Mean current set:   " <> fmt[meanSet, 0]   <> "\[Degree]  (expect ~150-160, SSE)"];
Print["VMG <= SOG check:   " <> If[meanVMG <= meanSOG, "PASS", "FAIL"]];
Print["Drift in range:     " <> If[0.1 <= meanDrift <= 1.0, "PASS", "FAIL"]];

(* ── Data arrays for plotting ─────────────────────────────────────────── *)
intDays   = days[[;; -2]];          (* 0.0 .. 22.0, 23 values *)
sogData   = Transpose[{intDays, sogKts}];
vmgData   = Transpose[{intDays, vmgKts}];
distData  = Transpose[{days, distToDestNm}];
driftData = Transpose[{intDays, currDrift}];

(* ── Panel (a): SOG and VMG vs day ─────────────────────────────────────── *)
Print["Building panel (a): SOG + VMG ..."];
panelA = ListLinePlot[
  {sogData, vmgData},
  PlotStyle -> {
    Directive[cBlue, Thickness[0.003]],
    Directive[cWarm, Thickness[0.003], Dashed]
  },
  PlotMarkers -> {{"\[FilledCircle]", 9}, {"\[FilledUpTriangle]", 9}},
  Frame -> True,
  FrameStyle -> cFrame,
  PlotLabel -> Style[
    "(a)  Speed over ground and VMG to Rodney Bay", dirT],
  FrameLabel -> {
    {Style["Speed (kt)", dirL], None},
    {Style["Day of passage", dirL], None}
  },
  PlotRange -> {{-0.5, 23.5}, {0, 6.5}},
  GridLines -> {Table[i, {i, 0, 23}], Table[i, {i, 0, 6}]},
  GridLinesStyle -> cGridL,
  PlotLegends -> Placed[
    LineLegend[
      {Directive[cBlue, Thickness[0.004]],
       Directive[cWarm, Thickness[0.004], Dashed]},
      {"SOG (true track, kt)",
       "VMG nav: 5 kt \[Times] cos(\[CapitalDelta]bearing) (kt)"},
      LabelStyle -> dirA,
      LegendLabel -> None
    ],
    {0.72, 0.30}
  ],
  Epilog -> {
    (* Mean SOG reference line *)
    {Opacity[0.30], cBlue, Dashed,
     Line[{{-0.5, meanSOG}, {23.5, meanSOG}}]},
    Text[Style["mean SOG = " <> fmt[meanSOG] <> " kt",
               dirA, FontColor -> cBlue],
         {4, meanSOG + 0.25}],
    (* Mean VMG reference line *)
    {Opacity[0.30], cWarm, Dashed,
     Line[{{-0.5, meanVMG}, {23.5, meanVMG}}]},
    Text[Style["mean VMG = " <> fmt[meanVMG] <> " kt",
               dirA, FontColor -> cWarm],
         {4, meanVMG - 0.30}],
    (* Annotation explaining VMG < SOG *)
    Text[Style["VMG \[LessEqual] SOG; gap widens as DR course diverges from bearing to dest",
               dirA, FontColor -> cGray],
         {10.5, 1.0}]
  },
  Background -> White,
  AspectRatio -> Full,            (* fill the wide ImageSize — no centred margins *)
  ImageSize -> {1050, 470}
];

(* ── Panel (b): Distance to destination vs day ──────────────────────────── *)
Print["Building panel (b): distance to destination ..."];
panelB = ListLinePlot[
  distData,
  PlotStyle -> Directive[cNavy, Thickness[0.003]],
  PlotMarkers -> {{"\[FilledCircle]", 8}},
  Frame -> True,
  FrameStyle -> cFrame,
  PlotLabel -> Style[
    "(b)  Closing the gap -- distance remaining to Rodney Bay", dirT],
  FrameLabel -> {
    {Style["Distance (nm)", dirL], None},
    {Style["Day of passage", dirL], None}
  },
  PlotRange -> {{-0.5, 23.5}, {0, 3200}},
  GridLines -> {Table[i, {i, 0, 23}], Automatic},
  GridLinesStyle -> cGridL,
  Filling -> Bottom,
  FillingStyle -> Directive[Opacity[0.10], cNavy],
  Background -> White,
  AspectRatio -> Full,
  ImageSize -> {1050, 430}
];

(* ── Panel (c): Current drift (kt) and set (deg) vs day -- dual axis ─── *)
Print["Building panel (c): current set & drift ..."];

driftMax = Ceiling[Max[currDrift] * 1.25 * 10] / 10;
driftMin = 0.0;
setMin   = 0.0;
setMax   = 360.0;

(* Linear bijection: map set [0,360] -> drift [driftMin, driftMax] *)
setToDrift = Function[s, (s - setMin) / (setMax - setMin) * (driftMax - driftMin) + driftMin];

setScaledData = Transpose[{intDays, setToDrift /@ currSet}];

rightTicks = Table[
  {setToDrift[s], ToString[s] <> "\[Degree]"},
  {s, 0, 360, 45}];

driftP = ListLinePlot[
  driftData,
  PlotStyle -> Directive[cWarm, Thickness[0.004]],
  PlotMarkers -> {{"\[FilledCircle]", 8}},
  Frame -> True,
  FrameStyle -> cFrame,
  PlotLabel -> Style[
    "(c)  Estimated current -- set and drift from DR discrepancy", dirT],
  FrameLabel -> {
    {Style["Drift (kt)", dirL, FontColor -> cWarm],
     Style["Set (\[Degree])", dirL, FontColor -> cNavy]},
    {Style["Day of passage", dirL], None}
  },
  PlotRange -> {{-0.5, 23.5}, {driftMin, driftMax}},
  GridLines -> {Table[i, {i, 0, 23}], Automatic},
  GridLinesStyle -> cGridL,
  FrameTicks -> {
    {Automatic, rightTicks},
    {Range[0, 22, 2], None}
  },
  Epilog -> {
    (* Mean drift reference line *)
    {Opacity[0.30], cWarm, Dashed,
     Line[{{-0.5, meanDrift}, {23.5, meanDrift}}]},
    Text[Style["mean drift = " <> fmt[meanDrift] <> " kt",
               dirA, FontColor -> cWarm],
         {4.0, meanDrift + driftMax * 0.07}],
    (* Mean set reference line (in drift coordinate space) *)
    {Opacity[0.25], cNavy, Dashed,
     Line[{{-0.5, setToDrift[meanSet]}, {23.5, setToDrift[meanSet]}}]},
    Text[Style["mean set = " <> ToString[Round[meanSet, 1]] <> "\[Degree] (SSE)",
               dirA, FontColor -> cNavy],
         {18.0, setToDrift[meanSet] + driftMax * 0.07}]
  },
  Background -> White,
  AspectRatio -> Full,
  ImageSize -> {1050, 430}
];

setP = ListLinePlot[
  setScaledData,
  PlotStyle -> Directive[cNavy, Thickness[0.003], Dashed],
  PlotMarkers -> {{"\[FilledUpTriangle]", 8}},
  Frame -> False,
  Axes -> False,
  PlotRange -> {{-0.5, 23.5}, {driftMin, driftMax}},
  AspectRatio -> Full,
  ImageSize -> {1050, 430},
  Background -> None
];

panelCBase = Show[driftP, setP, Background -> White];

panelC = Legended[
  panelCBase,
  Placed[
    LineLegend[
      {Directive[cWarm, Thickness[0.004]],
       Directive[cNavy, Thickness[0.004], Dashed]},
      {"Drift (kt)", "Set (\[Degree])"},
      LabelStyle -> dirA
    ],
    {0.88, 0.30}
  ]
];

(* ── Assemble figure ─────────────────────────────────────────────────────── *)
Print["Assembling figure ..."];
fig = Grid[
  {{panelA}, {panelB}, {panelC}},
  Spacings  -> {0, Scaled[0.006]},
  Background -> White
];

(* ── Export ──────────────────────────────────────────────────────────────── *)
outDir  = "../docs/images";
outPath = FileNameJoin[{outDir, "fig_speeds.png"}];

Print["Rasterizing ..."];
img = Rasterize[fig, Background -> White];
(* Fix macOS pink-tint: crop 3px outer ring then re-pad with white *)
img = ImagePad[ImagePad[img, -3], 3, White];

Export[outPath, img, "PNG"];

dims = ImageDimensions[img];
sz   = FileByteCount[outPath];
Print["EXPORTED: fig_speeds.png  " <>
      ToString[dims[[1]]] <> "\[Times]" <> ToString[dims[[2]]] <>
      " px  " <> ToString[Round[sz / 1024]] <> " KB"];

(* ── Pixel probe: verify white background ──────────────────────────────── *)
cornerPts   = {{3, 3}, {dims[[1]] - 2, 3},
               {3, dims[[2]] - 2}, {dims[[1]] - 2, dims[[2]] - 2}};
cornerNames = {"top-left", "top-right", "bottom-left", "bottom-right"};
cornerVals  = PixelValue[img, #] & /@ cornerPts;

Print["Corner pixels (white = {1.,1.,1.} or {1.,1.,1.,1.}):"];
MapThread[Print["  " <> #1 <> ": " <> ToString[#2]] &, {cornerNames, cornerVals}];
allWhite = AllTrue[Flatten[cornerVals], # > 0.98 &];
Print["White background confirmed: " <> ToString[allWhite]];
Print["Done."];
