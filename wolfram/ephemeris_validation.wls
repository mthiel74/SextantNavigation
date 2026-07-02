#!/usr/bin/env wolframscript
(* ephemeris_validation.wls
   Offline ephemeris-validation figure for the CelestialNavigation package.
   Produces: docs/images/fig_ephemeris_validation.png

   Three panels (light theme, white bg):
     A  Sun altitude residual vs Wolfram SunPosition over 2024 (arcmin, geometric)
     B  Proper-motion shift J2000->2024 per navigational star (arcmin), sorted
     C  IAU-1976 precession shift J2000->2024 per star (arcmin), sorted

   NOTE: SunPosition returns apparent altitude (includes atmospheric refraction).
   We subtract the engine's Bennett refraction correction to recover the
   geometric Wolfram altitude before differencing — isolating the ~0.1' almanac
   error from the ~1-4' refraction.

   DO NOT edit CelestialNavigation.wl or any test files.
   Runs fully offline. No AstroValue / AstroPosition calls.
*)

$CharacterEncoding = "UTF-8";

SetDirectory[DirectoryName[$InputFileName]];
Get["CelestialNavigation.wl"];
Print["Package loaded: CelestialNavigation ", cnVersion[]];

If[!DirectoryQ["../docs/images"], CreateDirectory["../docs/images"]];
outPath = "../docs/images/fig_ephemeris_validation.png";

(* ── Suppress expected warnings ── *)
Off[DateListPlot::ldata];

(* ── Light-theme colours (matches figures.wls palette) ── *)
cBg    = White;
cText  = RGBColor[0.13, 0.29, 0.53];   (* navy *)
cBlue  = RGBColor[0.25, 0.45, 0.95];   (* accent blue *)
cNavy  = RGBColor[0.13, 0.29, 0.53];   (* navy *)
cWarm  = RGBColor[0.85, 0.50, 0.20];   (* warm orange *)
cMid   = GrayLevel[0.55];
cGrid  = Directive[GrayLevel[0.88], Dashed];
cFrame = GrayLevel[0.82];

(* ── Shared font directives ── *)
fTitle = Directive[FontFamily -> "Helvetica", FontSize -> 18, Bold, FontColor -> cText];
fLabel = Directive[FontFamily -> "Helvetica", FontSize -> 18, FontColor -> cText];
fAnnot = Directive[FontFamily -> "Helvetica", FontSize -> 16, FontColor -> cMid];
fSmall = Directive[FontFamily -> "Helvetica", FontSize -> 15, FontColor -> cText];
fTiny  = Directive[FontFamily -> "Helvetica", FontSize -> 12, FontColor -> cText];

(* ── Number formatting helper (avoids NumberForm rendering issues in scripts) ──
   IMPORTANT: use 10.0^(-d) not 10^(-d).  WL evaluates 10^(-d) as the exact
   rational 1/10^d; Round[float, exact_rational] then returns an exact rational
   (e.g. 69/500) whose ToString is a fraction string.  Machine-float dx keeps
   the result as a Machine real with decimal ToString. *)
fmt[x_, d_:2] := ToString[Round[N[x], 10.0^(-d)]];

(* ═══════════════════════════════════════════════════════════════
   PANEL A  Sun altitude residual vs Wolfram SunPosition  (2024)
   ─────────────────────────────────────────────────────────────
   Comparison: geometric vs geometric.
   cnAltitudeFromGP[loc, cnSunGP[t]]   = our geometric altitude (from almanac).
   SunPosition(apparent) - refraction   = Wolfram's geometric altitude.
   Residual = (ours - Wolfram geometric) in arcmin.
   All three test locations near lon=0 so Sun is above horizon at UTC noon.
   ═══════════════════════════════════════════════════════════════ *)
Print["Panel A: computing Sun altitude residuals over 2024..."];

startDate = DateObject[{2024, 1, 1, 12, 0, 0}, TimeZone -> 0];
dates2024 = Table[DatePlus[startDate, {5(k-1), "Day"}], {k, 1, 73}];

(* Locations near prime meridian — all have Sun well above horizon at UTC noon *)
testLocs  = {{55.0, -5.0}, {10.0, -5.0}, {-40.0, -5.0}};
locLabels = {"55\[Degree]N 5\[Degree]W (N Atlantic)", "10\[Degree]N 5\[Degree]W (Gulf of Guinea)", "40\[Degree]S 5\[Degree]W (S Atlantic)"};
locColors = {cBlue, cNavy, cWarm};

(* Wolfram apparent altitude in degrees (SunPosition works offline) *)
wSunAppar[loc_, t_] :=
  QuantityMagnitude[UnitConvert[SunPosition[GeoPosition[loc], t][[2]], "AngularDegrees"]];

residualsByLoc = Table[
  Module[{loc = testLocs[[li]], pairs = {}, ourAlt, wAppar, wGeom, t},
    Do[
      t       = dates2024[[ki]];
      ourAlt  = cnAltitudeFromGP[loc, cnSunGP[t]];          (* geometric *)
      wAppar  = wSunAppar[loc, t];                            (* apparent (with refraction) *)
      (* Back-compute Wolfram geometric by subtracting refraction.
         Uses the engine's own Bennett formula for apples-to-apples. *)
      wGeom   = wAppar - cnRefraction[wAppar] / 60.0;
      If[ourAlt > 5.0 && wGeom > 5.0,
        AppendTo[pairs, {t, (ourAlt - wGeom) * 60.0}]       (* arcmin *)
      ],
      {ki, Length[dates2024]}
    ];
    pairs
  ],
  {li, Length[testLocs]}
];

allResVals = Flatten[Select[residualsByLoc, Length[#] > 0 &][[All, All, 2]]];
medianRes  = Median[allResVals];
maxAbsRes  = Max[Abs[allResVals]];

Print["Sun altitude residual (geometric vs geometric)  median=",
  fmt[medianRes, 3], "'   |max|=", fmt[maxAbsRes, 3], "'"];

annotA = "median " <> fmt[medianRes, 2] <> "\[Prime]   |max| " <>
         fmt[maxAbsRes, 2] <> "\[Prime]";

(* Filter to only the non-empty location series *)
validSeries = Select[
  Transpose[{residualsByLoc, locColors, locLabels}],
  Length[#[[1]]] > 0 &
];
resData  = validSeries[[All, 1]];
resCols  = validSeries[[All, 2]];
resLbls  = validSeries[[All, 3]];

panelA = DateListPlot[
  resData,
  Joined      -> True,
  PlotStyle   -> Thread[Directive[#, Thickness[0.003]] & /@ resCols],
  PlotMarkers -> {{\[FilledCircle], 5}, {\[FilledSquare], 5}, {\[FilledDiamond], 5}},
  PlotLegends -> Placed[
    LineLegend[
      resCols,
      Style[#, fSmall] & /@ resLbls,
      LegendMarkers  -> {{\[FilledCircle], 10}, {\[FilledSquare], 10}, {\[FilledDiamond], 10}},
      LegendFunction -> (Framed[#, FrameStyle -> cFrame, Background -> White,
                                   FrameMargins -> 6, RoundingRadius -> 3] &)
    ],
    {0.56, 0.82}
  ],
  Frame        -> True,
  FrameStyle   -> cFrame,
  FrameLabel   -> {
    Style["UTC date (2024, sampled every 5 days)", fLabel],
    Style["Residual: our Hc \[Minus] Wolfram (arcmin)", fLabel]
  },
  PlotLabel -> Style[
    "Panel A \[LongDash] Sun altitude residual vs Wolfram  |  " <> annotA,
    fTitle
  ],
  GridLines      -> {None, {0}},
  GridLinesStyle -> {None, Directive[GrayLevel[0.5], Dashed, Thickness[0.001]]},
  Background     -> cBg,
  PlotRangePadding -> {{Scaled[0.01], Scaled[0.01]}, {Scaled[0.15], Scaled[0.20]}},
  ImagePadding   -> {{100, 25}, {62, 60}},
  ImageSize      -> {1040, 340}
];

(* ═══════════════════════════════════════════════════════════════
   PANEL B  Proper-motion shift J2000 -> 2024  (arcmin)
   ─────────────────────────────────────────────────────────────
   pmRA = mu_alpha*cos(delta) in arcsec/yr (from nav_stars.csv).
   Total on-sky shift (small-angle) = sqrt(pmRA^2 + pmDec^2) * dt yr / 60 arcmin.
   dt = 24 yr (J2000.0 to 2024.0).
   ═══════════════════════════════════════════════════════════════ *)
Print["Panel B: computing proper-motion shifts..."];

stars = cnLoadStars[];
dtYr  = 24.0;   (* years: J2000.0 to 2024.0 *)

pmData = SortBy[
  KeyValueMap[Function[{n, s},
    {n, Sqrt[s["pmRA"]^2 + s["pmDec"]^2] * dtYr / 60.0}   (* arcmin *)
  ], stars],
  -#[[2]] &    (* descending *)
];

pmNames  = pmData[[All, 1]];
pmVals   = pmData[[All, 2]];
nStars   = Length[pmData];

top3 = pmData[[;; 3]];
Print["Top-3 PM movers (arcmin/24yr):  ",
  StringRiffle[
    (ToString[#[[1]]] <> " " <> fmt[#[[2]], 2] <> "'") & /@ top3, "   "
  ]
];

(* Color: top-3 warm, next-7 blue, rest mid-gray *)
pmBarColors = Table[
  Which[i <= 3, cWarm, i <= 10, cBlue, True, GrayLevel[0.72]],
  {i, nStars}
];

(* Label only top-12 (rotated vertical so adjacent names don't overlap) *)
pmBarLabels = Table[
  If[i <= 12, Rotate[Style[pmNames[[i]], fTiny], Pi/2], ""],
  {i, nStars}
];

top3str = StringRiffle[
  (pmData[[#, 1]] <> " " <> fmt[pmData[[#, 2]], 2] <> "\[Prime]") & /@ {1,2,3},
  "   "
];

panelB = BarChart[
  pmVals,
  ChartStyle  -> pmBarColors,
  ChartLabels -> Placed[pmBarLabels, Below],
  Frame        -> True,
  FrameStyle   -> cFrame,
  FrameLabel   -> {
    Style["Navigational star (sorted by shift)", fLabel],
    Style["Proper-motion shift (arcmin)", fLabel]
  },
  PlotLabel -> Style[
    "Panel B \[LongDash] Proper-motion shift J2000\[Rule]2024 (A2 fix)",
    fTitle
  ],
  GridLines      -> {None, Automatic},
  GridLinesStyle -> cGrid,
  Background     -> cBg,
  PlotRangePadding -> {Scaled[0.02], {Scaled[0.02], Scaled[0.20]}},
  ImagePadding   -> {{72, 14}, {150, 58}},
  ImageSize      -> {505, 400}
];

(* ═══════════════════════════════════════════════════════════════
   PANEL C  Precession shift J2000 -> 2024  (arcmin)
   ─────────────────────────────────────────────────────────────
   Angular separation (on the sphere) between the J2000.0 unit vector
   and the IAU-1976 precessed-to-2024.5 unit vector for each star.
   Expected: ~17-20' (IAU-1976 precession accumulates ~50"/yr in ecliptic
   longitude; 24 yr -> ~1200" -> ~20').
   ═══════════════════════════════════════════════════════════════ *)
Print["Panel C: computing precession shifts..."];

epoch2024 = DateObject[{2024, 6, 15, 12, 0, 0}, TimeZone -> 0];

precData = SortBy[
  KeyValueMap[Function[{n, s},
    Module[{ra0, dec0, prec, r0, r1, sep},
      ra0  = s["raDeg"];  dec0 = s["decDeg"];
      prec = cnPrecess[{ra0, dec0}, epoch2024];
      r0   = {Cos[dec0 Degree] Cos[ra0 Degree],
              Cos[dec0 Degree] Sin[ra0 Degree],
              Sin[dec0 Degree]};
      r1   = {Cos[prec[[2]] Degree] Cos[prec[[1]] Degree],
              Cos[prec[[2]] Degree] Sin[prec[[1]] Degree],
              Sin[prec[[2]] Degree]};
      sep  = ArcCos[Clip[r0 . r1, {-1, 1}]] / Degree * 60.0;   (* arcmin *)
      {n, sep}
    ]
  ], stars],
  -#[[2]] &
];

precVals   = precData[[All, 2]];
medianPrec = Median[precVals];
maxPrec    = Max[precVals];
Print["Precession shift:  median=", fmt[medianPrec, 2], "'   max=", fmt[maxPrec, 2], "'"];

precBarLabels = Table[
  If[i <= 10, Rotate[Style[precData[[i, 1]], fTiny], Pi/2], ""],
  {i, Length[precData]}
];

precBarColors = Table[
  Which[i <= 5, cNavy, i <= 15, cBlue, True, GrayLevel[0.72]],
  {i, Length[precData]}
];

panelC = BarChart[
  precVals,
  ChartStyle  -> precBarColors,
  ChartLabels -> Placed[precBarLabels, Below],
  Frame        -> True,
  FrameStyle   -> cFrame,
  FrameLabel   -> {
    Style["Navigational star (sorted by shift)", fLabel],
    Style["Precession shift J2000\[Rule]2024.5 (arcmin)", fLabel]
  },
  PlotLabel -> Style[
    "Panel C \[LongDash] IAU-1976 precession shift",
    fTitle
  ],
  GridLines      -> {None, Automatic},
  GridLinesStyle -> cGrid,
  Background     -> cBg,
  PlotRangePadding -> {Scaled[0.02], {Scaled[0.02], Scaled[0.20]}},
  ImagePadding   -> {{72, 14}, {150, 58}},
  ImageSize      -> {505, 400}
];

(* ═══════════════════════════════════════════════════════════════
   Caption
   ═══════════════════════════════════════════════════════════════ *)
captionStr =
  "Engine accuracy validated offline (no network calls).  " <>
  "Panel A: Sun altitude residual = our low-precision Almanac C geometric Hc minus " <>
  "Wolfram SunPosition apparent altitude back-corrected to geometric using Bennett refraction. " <>
  "Median " <> fmt[medianRes, 2] <> "\[Prime] (~" <> fmt[Abs[medianRes], 1] <> "\[Prime]) is the low-precision-almanac floor; " <>
  "a full external check vs JPL Horizons is in fetch_horizons.wls (network).  " <>
  "Panel B: proper-motion shift corrects up to ~1.5 nm for high-\[Mu] stars " <>
  "(Rigil Kentaurus " <> fmt[top3[[1,2]], 2] <> "\[Prime], Arcturus " <> fmt[top3[[2,2]], 2] <> "\[Prime]) \[LongDash] now applied before precession (A2 fix).  " <>
  "Panel C: IAU-1976 precession shifts star coordinates " <>
  "~" <> fmt[medianPrec, 0] <> "\[Prime] over 24 yr \[LongDash] both corrections matter for sub-nautical-mile star fixes.";

captionPanel = Framed[
  Text[Style[captionStr,
    Directive[FontFamily -> "Helvetica", FontSize -> 16,
              FontColor -> GrayLevel[0.30], FontSlant -> Italic]]],
  FrameStyle    -> None,
  Background    -> GrayLevel[0.95],
  FrameMargins  -> {{20, 20}, {14, 14}},
  RoundingRadius -> 4,
  ImageSize     -> {1030, Automatic}
];

(* ═══════════════════════════════════════════════════════════════
   Assemble and export
   ═══════════════════════════════════════════════════════════════ *)
Print["Assembling figure..."];

fig = Framed[
  Column[
    {
      panelA,
      Row[{panelB, Spacer[18], panelC}],
      captionPanel
    },
    Alignment -> Left,
    Spacings  -> 1
  ],
  Background   -> White,
  FrameStyle   -> None,
  FrameMargins -> {{18, 18}, {18, 18}}
];

Print["Rasterizing and exporting..."];

(* Rasterize at 1:1 (72 DPI default) so logical pts ≈ pixels; ~1600 px wide *)
img = Rasterize[fig, Background -> White];

(* Pink-border crop fix (mirrors exportFig in figures.wls) *)
img = ImagePad[ImagePad[img, -3], 3, White];

Export[outPath, img, "PNG"];

(* ── Diagnostics ── *)
dims = ImageDimensions[img];
szKB = Round[FileByteCount[outPath] / 1024];
Print["EXPORTED: ", outPath];
Print["  dims: ", dims[[1]], "\[Times]", dims[[2]], " px  |  size: ", szKB, " KB"];

(* White-bg pixel check: sample top-left corner *)
cornerPx = PixelValue[img, {5, 5}];
Print["  corner pixel RGB (should be {1,1,1}): ", cornerPx];

(* ── Final sanity summary ── *)
Print[""];
Print["=== SANITY CHECK ==="];
Print["Sun altitude residual (geometric vs geometric):"];
Print["  median = ", fmt[medianRes, 3], "'   |max| = ", fmt[maxAbsRes, 3], "'"];
Print["Top-3 PM movers (arcmin over 24 yr):"];
Do[Print["  ", pmData[[i,1]], "  =  ", fmt[pmData[[i,2]], 2], "'"], {i, 3}];
Print["Precession:  median = ", fmt[medianPrec, 2], "'   max = ", fmt[maxPrec, 2], "'"];
Print["Figure:  ", dims[[1]], " x ", dims[[2]], " px  |  ", szKB, " KB"];
Print["White bg: ", cornerPx];
Print["=================="];
