#!/usr/bin/env wolframscript
(* noon_sight.wls — Figure: Noon-sight latitude along the voyage (Item 15)
   Loads the package + voyage.csv.  For each day's LAN:
     1. Computes the true meridian altitude at the true position.
     2. Adds realistic ~1' Gaussian noise (seed fixed for reproducibility).
     3. Recovers latitude via cnNoonLatitude.
   Plots true vs noon-sight latitude (two near-coincident lines) and the
   latitude error in nautical miles on a secondary panel.
   Exports docs/images/fig_noon_sight.png (white background, 1600 px wide).
*)

$CharacterEncoding = "UTF-8";

(* ── Load package ──────────────────────────────────────────────────────────── *)
repo = DirectoryName[$InputFileName];   (* wolfram/ *)
pkgPath = FileNameJoin[{repo, "CelestialNavigation.wl"}];
Get[pkgPath];

(* ── Load voyage data ──────────────────────────────────────────────────────── *)
csvPath = FileNameJoin[{repo, "..", "data", "voyage.csv"}];
rawRows = Import[csvPath, "CSV"];
dataRows = rawRows[[2 ;;]];
(* Columns: day, datetimeUTC, latTrue, lonTrue, latDR, lonDR, courseDeg, speedKts *)
dayNums = dataRows[[All, 1]];
latTrue = dataRows[[All, 3]];
lonTrue = dataRows[[All, 4]];
nRows = Length[dataRows];

(* ── Simulate noon sights with ~1' noise ───────────────────────────────────── *)
SeedRandom[42];
sigmaMin = 1.0;   (* arcmin noise *)

noonLatitudes = Table[
  Module[{lat, lon, date, tLAN, decT, hoMer, brg, noise, hoObs},
    lat  = latTrue[[i]];
    lon  = lonTrue[[i]];
    date = DateObject[{2024, 11, 15 + dayNums[[i]]}, TimeZone -> 0];
    tLAN = cnLANTimeUTC[lon, date];
    decT = cnSunGP[tLAN][[1]];
    {hoMer, brg} = cnMeridianAltitude[lat, decT];
    noise = RandomVariate[NormalDistribution[0, sigmaMin / 60.0]];
    hoObs = hoMer + noise;
    cnNoonLatitude[hoObs, decT, brg]
  ],
  {i, nRows}
];

(* ── Latitude errors in nautical miles ─────────────────────────────────────── *)
errNm = (noonLatitudes - latTrue) * 60.0;   (* 1 deg lat = 60 nm *)

(* ── Build the figure ───────────────────────────────────────────────────────── *)
xVals = dayNums;

(* Panel 1: true latitude vs noon-sight latitude *)
latPlot = ListLinePlot[
  {Transpose[{xVals, latTrue}], Transpose[{xVals, noonLatitudes}]},
  PlotStyle -> {{Thick, Darker[Blue]}, {Thick, Dashed, Darker[Orange]}},
  PlotLegends -> Placed[
    LineLegend[
      {Directive[Thick, Darker[Blue]], Directive[Thick, Dashed, Darker[Orange]]},
      {"True latitude", "Noon-sight latitude"},
      LegendMarkerSize -> 28,
      LabelStyle -> Directive[16, Black, FontFamily -> "Helvetica"],
      LegendFunction -> (Framed[#, Background -> White,
                                  FrameStyle -> GrayLevel[0.7]] &)
    ],
    {0.72, 0.24}
  ],
  PlotRange -> All,
  Frame -> True,
  FrameLabel -> {None, Style["Latitude (\[Degree])", 17, Black]},
  FrameTicksStyle -> 15,
  GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.85], Dashed],
  Background -> White,
  ImageSize -> {1000, 380},
  ImagePadding -> {{80, 25}, {12, 22}}
];

(* Panel 2: latitude error in nautical miles *)
errPlot = ListLinePlot[
  Transpose[{xVals, errNm}],
  PlotStyle -> {Thick, Darker[Red]},
  PlotRange -> All,
  Frame -> True,
  FrameLabel -> {
    Style["Day of voyage", 17, Black],
    Style["Lat error (nm)", 17, Black]
  },
  FrameTicksStyle -> 15,
  GridLines -> {{}, {0}},
  GridLinesStyle -> Directive[GrayLevel[0.7], Dashed],
  Background -> White,
  Epilog -> {
    Text[
      Style[
        "Latitude (noon sight): time-insensitive",
        14, Black, FontFamily -> "Helvetica"
      ],
      Scaled[{0.97, 0.90}], {1, 1}
    ],
    Text[
      Style[
        "Longitude: 0.25\[Prime]/s \[RightArrow] needs a chronometer",
        14, GrayLevel[0.35], FontFamily -> "Helvetica"
      ],
      Scaled[{0.97, 0.78}], {1, 1}
    ]
  },
  ImageSize -> {1000, 340},
  ImagePadding -> {{80, 25}, {58, 12}}
];

(* Combine panels vertically *)
titleRow = Style[
  "Noon-Sight Latitude Along the Voyage",
  22, Bold, Black, FontFamily -> "Helvetica"
];
subtitleRow = Pane[
  Style[
    "Meridian altitude (noon sight) recovers latitude with ~1\[Prime] sextant noise; " <>
    "immune to chronometer error (only max altitude needed, not time).",
    15, GrayLevel[0.3], FontFamily -> "Helvetica"
  ],
  980
];

fig = Column[
  {titleRow, subtitleRow, Spacer[6], latPlot, errPlot},
  Alignment -> Left,
  Spacings -> {0, 0.4}
];

(* ── Export ─────────────────────────────────────────────────────────────────── *)
outDir = FileNameJoin[{repo, "..", "docs", "images"}];
If[!DirectoryQ[outDir], CreateDirectory[outDir]];
outPath = FileNameJoin[{outDir, "fig_noon_sight.png"}];

raster = Rasterize[fig, RasterSize -> 1000, Background -> White, ImageResolution -> 144];

(* Probe top-left corner pixel for white background *)
topLeft = ImageValue[raster, {1, 1}];
Print["BG pixel (expect {1,1,1}): ", topLeft];

Export[outPath, raster, "PNG"];

(* Report dimensions and file size *)
dims = ImageDimensions[raster];
fsize = FileByteCount[outPath];
Print["Exported: ", outPath];
Print["Dims: ", dims[[1]], " x ", dims[[2]], " px"];
Print["Size: ", N[fsize / 1024.0, 4], " KB  (", N[fsize / 1024.0^2, 4], " MB)"];
Print["Noon-latitude RMS error (nm): ", N[Sqrt[Mean[errNm^2]], 4]];
Print["Max |error| (nm): ", N[Max[Abs[errNm]], 4]];
