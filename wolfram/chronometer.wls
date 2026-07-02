#!/usr/bin/env wolframscript
(* chronometer.wls — Figure: chronometer-error longitude sensitivity (Item 12)
   Left  : longitude error (nm) vs clock error (s) for latitudes 0, 30, 45, 60 deg.
   Right : east-error and north-error vs clock bias for a 3-Sun fix (lat 15 N).
   Output: docs/images/fig_chronometer.png
*)

$CharacterEncoding = "UTF-8";

repo = DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repo, "wolfram", "CelestialNavigation.wl"}]];

outPath = FileNameJoin[{repo, "docs", "images", "fig_chronometer.png"}];

(* ── Palette ─────────────────────────────────────────────────────────────── *)
blue   = RGBColor[0.122, 0.467, 0.706];
green  = RGBColor[0.173, 0.627, 0.173];
orange = RGBColor[0.890, 0.467, 0.008];
red    = RGBColor[0.839, 0.153, 0.157];
gray   = GrayLevel[0.55];
lw     = Directive[AbsoluteThickness[2.2]];

(* ═══════════════════════════════════════════════════════════════════════════
   LEFT PANEL — sensitivity formula: 0.25 cos(lat) nm/s for 4 latitudes
   ═══════════════════════════════════════════════════════════════════════════ *)
lats  = {0, 30, 45, 60};
cols  = {blue, green, orange, red};
names = {"0\[Degree] (equator)", "30\[Degree]", "45\[Degree]", "60\[Degree]"};

(* Label positions: near the right end of each curve, inside the frame *)
labelPos = Table[{57, 0.25 Cos[lat Degree] * 57 + 0.55}, {lat, lats}];

leftPlot = Show[
  Plot[
    Evaluate[Table[0.25 Cos[lat Degree] t, {lat, lats}]],
    {t, 0, 60},
    PlotRange   -> {{0, 60}, {0, 17}},
    PlotStyle   -> (Directive[lw, #] & /@ cols),
    Frame       -> True,
    FrameLabel  -> {
      {Style["Longitude error  (nm)", 19], None},
      {Style["Chronometer error  (s)", 19],
       Style["Chronometer \[LongDash] Longitude Sensitivity", 21, Bold]}},
    FrameStyle  -> Directive[Black, AbsoluteThickness[1]],
    FrameTicksStyle -> Directive[FontSize -> 16],
    GridLines   -> Automatic,
    GridLinesStyle -> Directive[GrayLevel[0.85]],
    Background  -> White,
    ImageSize   -> {1000, 560}
  ],
  (* Curve labels placed at end of each line — no Inset[] *)
  Graphics[
    MapThread[
      Function[{pos, col, name},
        {col, Text[Style[name, FontSize -> 18, FontFamily -> "Arial",
                         FontWeight -> Bold],
                   pos, {1, 0}]}],
      {labelPos, cols, names}]
  ],
  (* "4 s = 1 nm" annotation on the equator line *)
  Graphics[{
    {Directive[Black, AbsolutePointSize[7]], Point[{4, 1.0}]},
    Text[Style["4 s = 1 nm", FontSize -> 18, FontFamily -> "Arial",
               FontWeight -> Bold],
         {4, 1.0}, {-0.15, 1.5}]
  }]
];

(* ═══════════════════════════════════════════════════════════════════════════
   RIGHT PANEL — fix error components vs clock bias (3 Sun sights, lat 15 N)
   ═══════════════════════════════════════════════════════════════════════════ *)
truePos = {15.0, -40.0};
sunTimes = {DateObject[{2024, 11, 15, 10, 0, 0}, TimeZone -> 0],
            DateObject[{2024, 11, 15, 15, 0, 0}, TimeZone -> 0],
            DateObject[{2024, 11, 15, 19, 0, 0}, TimeZone -> 0]};
bodies   = {"Sun", "Sun", "Sun"};

biasVals = Range[-60, 60, 5];
Print["Computing fix errors for ", Length[biasVals], " clock-bias values..."];
results  = cnChronometerFixError[truePos, sunTimes, bodies, #] & /@ biasVals;
eastPts  = Transpose[{biasVals, #["eastNm"] & /@ results}];
northPts = Transpose[{biasVals, #["northNm"] & /@ results}];

rightPlot = Show[
  ListLinePlot[
    {eastPts, northPts},
    PlotStyle   -> {Directive[lw, blue], Directive[lw, red, Dashing[{0.015, 0.01}]]},
    PlotRange   -> {{-60, 60}, {-17, 17}},
    Frame       -> True,
    FrameLabel  -> {
      {Style["Fix error  (nm)", 19], None},
      {Style["Clock bias  (s)", 19],
       Style["Fix Error vs Clock Bias  (lat 15\[Degree]N, 3-Sun fix)", 21, Bold]}},
    FrameStyle  -> Directive[Black, AbsoluteThickness[1]],
    FrameTicksStyle -> Directive[FontSize -> 16],
    GridLines   -> {Range[-60, 60, 20], Range[-15, 15, 5]},
    GridLinesStyle -> Directive[GrayLevel[0.85]],
    Background  -> White,
    ImageSize   -> {1000, 560}
  ],
  (* Zero-line reference *)
  Graphics[{Directive[gray, AbsoluteThickness[1], Dashing[{0.02, 0.01}]],
            Line[{{-60, 0}, {60, 0}}]}],
  (* Curve labels *)
  Graphics[{
    {blue, Text[Style["East (longitude)", FontSize -> 19, FontFamily -> "Arial"],
                {-28, -10.5}, {0, 0}]},
    {red,  Text[Style["North (latitude)", FontSize -> 19, FontFamily -> "Arial"],
                {-28, -2.2}, {0, 0}]}
  }]
];

(* ═══════════════════════════════════════════════════════════════════════════
   COMPOSE
   ═══════════════════════════════════════════════════════════════════════════ *)
caption = "Harrison\[CloseCurlyQuote]s H4 chronometer (1759) solved the Longitude Problem by keeping " <>
          "time to \[PlusMinus]5 s/month \[LongDash] limiting longitude error to \[PlusMinus]1.2 nm (equator).";

fig = Column[
  {
    GraphicsColumn[{leftPlot, rightPlot}, Spacings -> 12, Background -> White,
                   ImageSize -> 1000],
    Style[caption, FontSize -> 17, FontFamily -> "Arial",
          FontColor -> GrayLevel[0.25], TextAlignment -> Center]
  },
  Alignment -> Center,
  Background -> White,
  ImageSize  -> 1000
];

Print["Exporting to ", outPath, " ..."];
Export[outPath, fig, "PNG", Background -> White, ImageSize -> 1000];

(* ── Verify ──────────────────────────────────────────────────────────────── *)
img  = Import[outPath];
dims = ImageDimensions[img];
sz   = FileByteCount[outPath];
corner = PixelValue[img, {1, 1}, "Byte"];

Print["Dimensions : ", dims[[1]], " x ", dims[[2]], " px"];
Print["File size  : ", Round[sz / 1024], " KB"];
Print["Top-left px: ", corner, "  (255,255,255 = white)"];
If[sz > 2*^6, Print["WARNING: file >2 MB (", Round[sz/1024], " KB)"]];
