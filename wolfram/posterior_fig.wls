#!/usr/bin/env wolframscript
(* posterior_fig.wls — Bayesian position-posterior figure (band -> blob -> peak,
   plus a systematic-bias panel).  Output: docs/images/fig_posterior.png

   This standalone renderer replicates the four-panel posterior figure directly
   from CelestialNavigation.wl primitives (cnPosteriorGrid / cnAltitudeFromGP /
   cnSunGP).  It is deliberately DECOUPLED from wolfram/SextantFigures.wl so the
   PNG can be re-rendered independently.  Data, scenario, title and caption are
   identical to the package figure; only the canvas width and font sizes are
   tuned so every label/caption stays legible when the PNG is embedded at
   ~620 px wide in the notebook. *)

$CharacterEncoding = "UTF-8";

repo = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repo, "wolfram", "CelestialNavigation.wl"}]];

outPath = FileNameJoin[{repo, "docs", "images", "fig_posterior.png"}];

(* ── Light-theme palette ─────────────────────────────────────────────────── *)
navy   = RGBColor[0.13, 0.29, 0.53];
green  = RGBColor[0.13, 0.62, 0.30];
orange = RGBColor[0.92, 0.49, 0.06];
textC  = navy;
cf = Function[v, Blend[{White, RGBColor[0.80, 0.89, 0.98],
                        RGBColor[0.35, 0.58, 0.92], navy}, v^0.6]];

(* Scenario: 20 N, 40 W; three noise-free Sun sights spread over the day. *)
truth = {20.0, -40.0};
ts = {DateObject[{2024, 11, 22, 10, 0, 0}, TimeZone -> 0],
      DateObject[{2024, 11, 22, 13, 0, 0}, TimeZone -> 0],
      DateObject[{2024, 11, 22, 16, 0, 0}, TimeZone -> 0]};
mk[t_] := <|"body" -> "Sun", "t" -> t,
            "Ho" -> cnAltitudeFromGP[truth, cnSunGP[t]]|>;
ieMin = 10.0;
mkBias[t_] := <|"body" -> "Sun", "t" -> t,
                "Ho" -> cnAltitudeFromGP[truth, cnSunGP[t]] + ieMin/60.|>;

box = {{19.5, 20.5}, {-40.5, -39.5}};
{{latMin, latMax}, {lonMin, lonMax}} = box;
ng  = 180;     (* heatmap resolution *)
sig = 1.5;     (* sextant sigma, arcmin *)

(* One framed heatmap panel + true-position ring + MAP cross. *)
makePanel[sights_, panelTitle_] := Module[{r, post, map, dLon, dLat},
  r    = cnPosteriorGrid[sights, box, ng, sig];
  post = r["posterior"]; map = r["mapEstimate"];
  dLon = 0.035 (lonMax - lonMin); dLat = 0.035 (latMax - latMin);
  ArrayPlot[post,
    DataRange     -> {{lonMin, lonMax}, {latMin, latMax}},
    DataReversed  -> True,
    ColorFunction -> cf, ColorFunctionScaling -> True,
    Epilog -> {
      {green, AbsoluteThickness[2.5], Circle[{truth[[2]], truth[[1]]}, {dLon, dLat}]},
      {green, AbsolutePointSize[5], Point[{truth[[2]], truth[[1]]}]},
      {orange, AbsoluteThickness[3],
        Line[{{map[[2]] - dLon, map[[1]]}, {map[[2]] + dLon, map[[1]]}}],
        Line[{{map[[2]], map[[1]] - dLat}, {map[[2]], map[[1]] + dLat}}]}
    },
    PlotLabel  -> Style[panelTitle, FontFamily -> "Helvetica", FontSize -> 15,
                        Bold, FontColor -> textC],
    Frame      -> True,
    FrameStyle -> Directive[GrayLevel[0.6], 13, FontFamily -> "Helvetica"],
    FrameTicks -> {{Automatic, None}, {Automatic, None}},
    FrameLabel -> {
      {Style["lon (\[Degree]E)", FontSize -> 15, FontColor -> textC], None},
      {Style["lat (\[Degree]N)", FontSize -> 15, FontColor -> textC], None}},
    PlotRange  -> {{lonMin, lonMax}, {latMin, latMax}},
    AspectRatio -> 1,
    Background  -> White,
    ImagePadding -> {{46, 8}, {34, 8}},
    ImageSize   -> 250
  ]
];

p1 = makePanel[mk /@ ts[[;; 1]],  "1 sight \[LongDash] band (LOP)"];
p2 = makePanel[mk /@ ts[[;; 2]],  "2 sights \[LongDash] blob"];
p3 = makePanel[mk /@ ts[[;; 3]],  "3 sights \[LongDash] peak"];
p4 = makePanel[mkBias /@ ts[[;; 3]], "3 sights + 10\[Prime] index error"];

row = GraphicsRow[{p1, p2, p3, p4}, Spacings -> 6,
        Background -> White, ImageSize -> 1010];

titleStr = "Bayesian Position Posterior  \[LongDash]  " <>
  "Band \[Rule] Blob \[Rule] Peak as Sights Accumulate  \[LongDash]  20\[Degree]N, 40\[Degree]W";
captStr =
  "Normalised posterior P(position | sights) on a flat prior, " <>
  "Gaussian sight likelihood (\[Sigma] = 1.5\[Prime]).  " <>
  "Green ring = true position;  orange cross = MAP estimate.  " <>
  "One sight is a fuzzy BAND (the line of position is a likelihood ridge); " <>
  "each added sight sharpens it (BLOB \[Rule] PEAK), the honest analogue of a shrinking error ellipse.  " <>
  "Rightmost panel: a constant uncorrected 10\[Prime] index error leaves the peak just as tight " <>
  "but shifts it OFF the truth \[LongDash] a confident, precise, and WRONG fix (systematic bias is invisible to scatter).";

fig = Column[{
    Style[titleStr, FontFamily -> "Helvetica", FontSize -> 21, Bold, FontColor -> textC],
    row,
    Style[captStr, FontFamily -> "Helvetica", FontSize -> 16.5, FontColor -> GrayLevel[0.25]]
  },
  Alignment -> Center, Spacings -> {0, {0.8, 0.8}}, Background -> White];

img = Rasterize[fig, ImageResolution -> 144, Background -> White];
Export[outPath, img, "PNG"];

{w, h} = ImageDimensions[Import[outPath]];
sz     = FileByteCount[outPath];
Print["Output:     ", outPath];
Print["Dimensions: ", w, " x ", h, " pixels"];
Print["File size:  ", Round[sz / 1024], " KB"];
