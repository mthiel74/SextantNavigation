#!/usr/bin/env wolframscript
(* equation_of_time.wls — Item 8: Equation of Time and Analemma
   Exports docs/images/fig_equation_of_time.png
   Run: /usr/local/bin/wolframscript -file wolfram/equation_of_time.wls

   EoT derivation
   ==============
   cnSunGP[t] = {decDeg, lonGP}, sub-solar longitude (deg, east+, -180..180).
   At 12:00:00 UTC the mean Sun is on the Greenwich meridian by definition.
   The apparent Sun at longitude lonGP (east positive) at that moment is:
     lonGP > 0 (east of Greenwich): Sun has not yet crossed the meridian;
       apparent noon at Greenwich comes AFTER clock noon → EoT < 0.
     lonGP < 0 (west of Greenwich): Sun already past the meridian;
       apparent noon at Greenwich was BEFORE clock noon → EoT > 0.
   Rate: 360° = 24 h = 1440 min → 1° = 4 min.
   GHA_signed = -lonGP   (GHA_apparent at 12:00 UTC, signed east-positive)
   EoT (min) = GHA_signed × 4 = -lonGP × 4.

   Sanity: lonGP ≈ -4° in early Nov → EoT ≈ +16 min (max).
           lonGP ≈ +3.5° in mid-Feb → EoT ≈ -14 min (min).
*)

$CharacterEncoding = "UTF-8";

(* ── Load package ──────────────────────────────────────────────────── *)
repo    = DirectoryName[$InputFileName];
pkgPath = FileNameJoin[{repo, "CelestialNavigation.wl"}];
Get[pkgPath];

(* ── Light-theme colour palette ────────────────────────────────────── *)
accBlue = RGBColor[0.25, 0.45, 0.95];
warmOg  = RGBColor[0.85, 0.50, 0.20];
navy    = RGBColor[0.13, 0.29, 0.53];
midGray = GrayLevel[0.48];
green   = RGBColor[0.15, 0.60, 0.25];

(* ── Compute EoT over 2024 (leap year, 366 days), every 2 days ─────── *)
year = 2024;
t0   = DateObject[{year, 1, 1, 12, 0, 0}, TimeZone -> 0];
doys = Range[1, 366, 2];

Print["Computing EoT for ", Length[doys], " dates..."];
rawData = Table[
  Module[{t, gp},
    t  = DatePlus[t0, Quantity[d - 1, "Days"]];
    gp = cnSunGP[t];
    {d, -gp[[2]] * 4.0, gp[[1]]}   (* {day, EoT_min, dec_deg} *)
  ],
  {d, doys}
];

doyVals = rawData[[All, 1]];
eotVals = rawData[[All, 2]];
decVals = rawData[[All, 3]];

(* ── Extrema ─────────────────────────────────────────────────────── *)
iMax = First[Ordering[eotVals, -1]];
iMin = First[Ordering[eotVals,  1]];

eotMax = eotVals[[iMax]]; doyMax = doyVals[[iMax]];
eotMin = eotVals[[iMin]]; doyMin = doyVals[[iMin]];

(* Day-of-year → approximate date label (2024 is a leap year) *)
doy2label[d_] :=
  Module[{mlen, cum, m, dy},
    mlen = {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    cum  = FoldList[Plus, 0, mlen];
    m    = First[Select[Range[12], cum[[#]] < d <= cum[[# + 1]] &, 1]];
    dy   = d - cum[[m]];
    {"Jan","Feb","Mar","Apr","May","Jun",
     "Jul","Aug","Sep","Oct","Nov","Dec"}[[m]] <> " " <> ToString[dy]
  ];

Print["EoT maximum: +", Round[eotMax, 0.1], " min  day ", doyMax,
      "  (~", doy2label[doyMax], ")"];
Print["EoT minimum: ",  Round[eotMin, 0.1], " min  day ", doyMin,
      "  (~", doy2label[doyMin], ")"];

(* ── Zero crossings (linear interpolation between adjacent samples) ── *)
pairs  = Partition[Transpose[{doyVals, eotVals}], 2, 1];
crossPairs = Select[pairs, Sign[#[[1,2]]] * Sign[#[[2,2]]] < 0 &];
zeroDoys = Map[
  Function[pr,
    Module[{d1 = pr[[1,1]], e1 = pr[[1,2]], d2 = pr[[2,1]], e2 = pr[[2,2]]},
      d1 + (d2 - d1) * Abs[e1] / (Abs[e1] + Abs[e2])
    ]
  ],
  crossPairs
];
Print["Zero crossings: ", Map[doy2label[Round[#]]&, zeroDoys]];

(* ── Month starts and mid-month anchors (2024 leap year) ──────────── *)
monthStarts  = {1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336};
monthLabels  = {"Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec"};
monthMidDoys = {16, 47, 76, 107, 137, 168, 198, 229, 260, 290, 321, 351};

(* ── LEFT PANEL: EoT vs day-of-year ───────────────────────────────── *)
xTicks = Table[
  {monthStarts[[i]],
   Style[monthLabels[[i]], 11, FontFamily -> "Helvetica", FontColor -> navy]},
  {i, 12}
];
yTicksEoT = Table[
  {v, Style[ToString[v], 11, FontFamily -> "Helvetica", FontColor -> navy]},
  {v, Range[-16, 18, 2]}
];

maxLbl = "+" <> ToString[Round[eotMax, 0.1]] <> " min (~" <> doy2label[doyMax] <> ")";
minLbl = ToString[Round[eotMin, 0.1]] <> " min (~" <> doy2label[doyMin] <> ")";

zeroCrossDots = Map[
  {midGray, PointSize[0.009], Point[{#, 0}]}&,
  zeroDoys
];

epiLeft = Join[
  {
    (* Max dot and label *)
    accBlue, PointSize[0.014], Point[{doyMax, eotMax}],
    Text[Style[maxLbl, 9, FontFamily -> "Helvetica", FontColor -> navy],
         {doyMax, eotMax + 1.5}, {0, -1}],
    (* Min dot and label *)
    warmOg, PointSize[0.014], Point[{doyMin, eotMin}],
    Text[Style[minLbl, 9, FontFamily -> "Helvetica", FontColor -> warmOg],
         {doyMin, eotMin - 1.8}, {0, 1}]
  },
  zeroCrossDots
];

leftPanel = ListLinePlot[
  Transpose[{doyVals, eotVals}],
  PlotStyle     -> {Thick, accBlue},
  Frame         -> True,
  FrameStyle    -> Directive[GrayLevel[0.75], Thin],
  FrameLabel    -> {
    None,
    Style["Equation of Time (minutes)", 12, FontFamily -> "Helvetica",
          FontColor -> navy]
  },
  PlotLabel     -> Style["Equation of Time — 2024", 13, Bold,
                         FontFamily -> "Helvetica", FontColor -> navy],
  FrameTicks    -> {{yTicksEoT, None}, {xTicks, None}},
  Background    -> White,
  GridLines     -> {monthStarts, {0}},
  GridLinesStyle -> Directive[GrayLevel[0.88]],
  PlotRange     -> {{1, 366}, {-17, 20}},
  ImagePadding  -> {{65, 25}, {55, 55}},
  Epilog        -> epiLeft
];

(* ── RIGHT PANEL: Analemma ─────────────────────────────────────────── *)
(* 2024: Spring equinox Mar 20 = day 80, Summer solstice Jun 20 = day 172,
         Autumn equinox Sep 22 = day 266, Winter solstice Dec 21 = day 356 *)
seasonDoys   = {80, 172, 266, 356};
seasonNames  = {"Spring Equinox", "Summer Solstice", "Autumn Equinox", "Winter Solstice"};
seasonColors = {green, RGBColor[0.82, 0.62, 0.05], warmOg, accBlue};
(* label offsets {right/left, up/down} in data coords, then anchor *)
seasonPosOff = {{-1.5, -2.5}, {-4.0, +1.5}, {+1.5, +2.5}, {+1.5, -2.5}};
seasonAnch   = {{1, 1}, {1, -1}, {-1, 1}, {-1, 1}};

seasonIdxs = Map[First[Ordering[Abs[doyVals - #]]]&, seasonDoys];
seasonEoTs = eotVals[[seasonIdxs]];
seasonDecs = decVals[[seasonIdxs]];
seasonPts  = Transpose[{seasonEoTs, seasonDecs}];

seasonEpi = Flatten[Table[
  {
    seasonColors[[i]], PointSize[0.016], Point[seasonPts[[i]]],
    Text[Style[seasonNames[[i]], 9, Bold, FontFamily -> "Helvetica",
               FontColor -> seasonColors[[i]]],
         seasonPts[[i]] + seasonPosOff[[i]], seasonAnch[[i]]]
  },
  {i, 4}
], 1];

(* Month midpoint labels on the curve *)
monthIdxs = Map[First[Ordering[Abs[doyVals - #]]]&, monthMidDoys];
monthPts  = Transpose[{eotVals[[monthIdxs]], decVals[[monthIdxs]]}];

monthEpi = Flatten[Table[
  {midGray, PointSize[0.007], Point[monthPts[[i]]],
   Text[Style[monthLabels[[i]], 8, FontFamily -> "Helvetica",
              FontColor -> midGray],
        monthPts[[i]] + {0.4, 0.5}, {-1, -1}]},
  {i, 12}
], 1];

epiRight = Join[seasonEpi, monthEpi];

yTicksDec = Table[
  {v, Style[ToString[v] <> "\[Degree]", 11, FontFamily -> "Helvetica",
            FontColor -> navy]},
  {v, Range[-24, 24, 6]}
];
xTicksEoT = Table[
  {v, Style[ToString[v], 11, FontFamily -> "Helvetica", FontColor -> navy]},
  {v, Range[-14, 16, 4]}
];

rightPanel = ListLinePlot[
  Transpose[{eotVals, decVals}],
  PlotStyle     -> {Thick, navy},
  Frame         -> True,
  FrameStyle    -> Directive[GrayLevel[0.75], Thin],
  FrameLabel    -> {
    Style["EoT (min): apparent \[Minus] mean solar time", 12,
          FontFamily -> "Helvetica", FontColor -> navy],
    Style["Solar Declination (\[Degree])", 12,
          FontFamily -> "Helvetica", FontColor -> navy]
  },
  PlotLabel     -> Style["Analemma: Sun's Position at Clock Noon", 13, Bold,
                         FontFamily -> "Helvetica", FontColor -> navy],
  FrameTicks    -> {{yTicksDec, None}, {xTicksEoT, None}},
  Background    -> White,
  GridLines     -> {{0}, {0}},
  GridLinesStyle -> Directive[GrayLevel[0.72], Dashed],
  PlotRange     -> {{-17, 20}, {-25.5, 25.5}},
  AspectRatio   -> 1.25,
  ImagePadding  -> {{70, 20}, {55, 55}},
  Epilog        -> epiRight
];

(* ── Caption ────────────────────────────────────────────────────────── *)
caption = Style[
  "Equation of Time: apparent minus mean solar time (" <>
  "\[Minus]14 to +16 min over the year, from orbital eccentricity + axial tilt). " <>
  "The apparent Sun can cross your meridian up to ~16 min off clock noon — " <>
  "navigators apply this correction to determine Local Apparent Noon (LAN). " <>
  "The analemma (right) is the figure-8 traced by the Sun's noon position " <>
  "over a year, uniting EoT and declination.",
  11, Italic, FontFamily -> "Helvetica", FontColor -> GrayLevel[0.35]
];

(* ── Assemble and export ─────────────────────────────────────────────── *)
row = GraphicsRow[
  {leftPanel, rightPanel},
  ImageSize  -> 1600,
  Background -> White,
  Spacings   -> 30
];

fig = Column[
  {row, Spacer[4], caption},
  Alignment -> Center
];

outDir  = FileNameJoin[{repo, "..", "docs", "images"}];
outPath = FileNameJoin[{outDir, "fig_equation_of_time.png"}];

img  = Rasterize[fig, Background -> White];
img  = ImagePad[ImagePad[img, -3], 3, White];
Export[outPath, img, "PNG"];

dims    = ImageDimensions[img];
sz      = FileByteCount[outPath];
topLeft = ImageValue[img, {1, 1}];

Print["BG pixel (expect {1,1,1}): ", topLeft];
Print["EXPORTED: ", outPath];
Print["Dims: ", dims[[1]], " \[Times] ", dims[[2]], " px"];
Print["Size: ", Round[sz / 1024], " KB  (", Round[N[sz / 1024.0^2], 0.01], " MB)"];
