#!/usr/bin/env wolframscript
(* cockedhat.wls -- Cocked-hat 25% theorem: Monte-Carlo demonstration figure *)
(* Run: /usr/local/bin/wolframscript -file wolfram/cockedhat.wls             *)
(* Item 10 of scientific-improvements campaign.                               *)

$CharacterEncoding = "UTF-8";
SetDirectory[DirectoryName[$InputFileName]];
Get["CelestialNavigation.wl"];

If[!DirectoryQ["../docs/images"], CreateDirectory["../docs/images"]];
outDir  = "../docs/images";
outPath = FileNameJoin[{outDir, "fig_cocked_hat_theorem.png"}];

(* ── Colour palette (light theme, matching figures.wls) ─────────────────── *)
blue  = RGBColor[0.25, 0.45, 0.95];
green = RGBColor[0.20, 0.68, 0.35];
red   = RGBColor[0.82, 0.20, 0.20];
navy  = RGBColor[0.13, 0.29, 0.53];
mid   = GrayLevel[0.45];
gridC = Directive[GrayLevel[0.88], Dashed];
frameC = GrayLevel[0.85];
bg    = White;
textC = RGBColor[0.13, 0.29, 0.53];
titleDir = Directive[FontFamily->"Helvetica", FontSize->20, Bold, FontColor->textC];
labelDir = Directive[FontFamily->"Helvetica", FontSize->20, FontColor->textC];

(* ── Helpers: pixel-exact image ops ─────────────────────────────────────── *)
(* Always ImageResize after Rasterize to defeat Retina 2x pixel density.    *)
toImg[expr_, w_] := ImageResize[Rasterize[expr, Background -> bg], w];

(* Render the text on a canvas the FULL target width w (not a fixed 500 px) so
   long titles/captions are not clipped before the final resize. *)
txtImg[txt_, w_, h_, fsize_, bold_:False] := ImageResize[
  Rasterize[
    Graphics[
      {Text[Style[txt, Directive[FontFamily->"Helvetica", FontSize->fsize,
                                  If[bold, Bold, Plain], FontColor->textC]],
             Scaled[{0.5, 0.5}]]},
      Background -> bg, ImageSize -> {w, h}, PlotRangeClipping -> False],
    Background -> bg],
  w];

(* ── Simulation parameters ───────────────────────────────────────────────── *)
truth    = {20.0, -40.0};
times    = {DateObject[{2024,11,22,10, 0,0}, TimeZone->0],
            DateObject[{2024,11,22,13, 0,0}, TimeZone->0],
            DateObject[{2024,11,22,16, 0,0}, TimeZone->0]};
bodies   = {"Sun", "Sun", "Sun"};
sightOpts = <|"sigmaMin" -> 1.5|>;

(* ── Run Monte-Carlo (seed once; do NOT re-seed per sight) ──────────────── *)
nFig = 500;
Print["Running ", nFig, " Monte-Carlo trials (seed 2024)..."];
SeedRandom[2024];

trials = Table[
  Module[{lops, hat, verts, inside},
    lops = MapThread[Function[{body, ti},
      cnReduceSightBody[
        cnGenerateSightBody[truth, ti, body, sightOpts],
        ti, truth, body, sightOpts]],
      {bodies, times}];
    hat    = cnCockedHat[lops];
    verts  = hat["vertices"];
    inside = cnCockedHatContains[truth, lops];
    <|"verts" -> verts, "inside" -> inside|>
  ],
  {nFig}
];

insideFrac = N[Total[Boole[#["inside"]] & /@ trials] / nFig];
Print["Empirical P(inside): ", NumberForm[insideFrac, {4,3}], "  (expected 0.25)"];

(* ── Tangent-plane projection centred on truth ───────────────────────────── *)
toXY[{lat_, lon_}] := {(lon - truth[[2]]) Cos[truth[[1]] Degree] 60,
                        (lat - truth[[1]]) 60};

(* ═══════════════════════════════════════════════════════════════════════════
   LEFT PANEL: 3 x 4 grid of 12 example triangles
   ─ 3 green (inside) + 9 red (outside) selected from the run.
   ─ GraphicsGrid — no Inset of typeset content.
   ═══════════════════════════════════════════════════════════════════════════ *)
insideT  = Select[trials, #["inside"] &];
outsideT = Select[trials, ! #["inside"] &];
nIn      = Min[3, Length[insideT]];
nOut     = Min[9, Length[outsideT]];
display12 = RandomSample[Join[Take[insideT, nIn], Take[outsideT, nOut]], nIn + nOut];

rng = 6.5;   (* half-range in nm *)

makeMini[t_] := Module[{verts, inside, pts2d, col},
  verts  = t["verts"];
  inside = t["inside"];
  pts2d  = toXY /@ verts;
  col    = If[inside, green, red];
  Graphics[{
    {FaceForm[Opacity[0.20, col]], EdgeForm[None], Polygon[pts2d]},
    {col, AbsoluteThickness[1.8],
     Line[{pts2d[[1]], pts2d[[2]], pts2d[[3]], pts2d[[1]]}]},
    {GrayLevel[0.65], AbsolutePointSize[5], Point /@ pts2d},
    {col,   AbsolutePointSize[11], Point[{0., 0.}]},
    {White, AbsolutePointSize[5],  Point[{0., 0.}]}
  },
  PlotRange   -> {{-rng, rng}, {-rng, rng}},
  Background  -> bg,
  Frame       -> True,
  FrameStyle  -> Directive[col, AbsoluteThickness[2.5]],
  FrameTicks  -> None,
  ImageSize   -> {185, 185},
  ImagePadding -> {{3,3},{3,3}}
  ]
];

miniPanels = makeMini /@ display12;

leftGrid = GraphicsGrid[
  Partition[miniPanels, 4],
  Spacings   -> {3, 3},
  Background -> bg,
  ImageSize  -> {560, 430}
];

(* ── Left panel column: title + grid + caption, all at leftW pixels wide ── *)
leftW = 560;

leftTitleImg  = txtImg["~12 example trials from the simulation", leftW, 34, 20, True];
leftSubImg    = txtImg[
  "GREEN = truth inside  |  RED = truth outside\n|  dot marks true position",
  leftW, 52, 17];
leftGridImg   = toImg[leftGrid, leftW];
leftCaptImg   = txtImg[
  "Expect ~25% green triangles  |  Daniels 1951: exactly 1/4\n" <>
  "for any 3 independent, symmetric, zero-mean bearings",
  leftW, 52, 17];

leftCol = ImageAssemble[
  {{leftTitleImg}, {leftGridImg}, {leftSubImg}, {leftCaptImg}},
  Background -> bg];

(* ═══════════════════════════════════════════════════════════════════════════
   RIGHT PANEL: running P(inside) converging to 0.25
   ─ Shaded +-2 SE band as Polygon in Epilog (data coordinates).
   ─ All labels via Text[] primitives in Epilog (no Inset of typeset content).
   ═══════════════════════════════════════════════════════════════════════════ *)
insideVec = Boole[#["inside"]] & /@ trials;
ns        = N @ Range[1, nFig];
runFrac   = N @ Table[Total[Take[insideVec, i]] / i, {i, 1, nFig}];

(* +-2 SE band polygon: upper edge left-to-right, lower edge right-to-left *)
sePlus  = 0.25 + 2 Sqrt[0.25 * 0.75 / ns];
seMinus = 0.25 - 2 Sqrt[0.25 * 0.75 / ns];
bandPts = Join[
  Transpose[{ns, sePlus}],
  Reverse[Transpose[{ns, seMinus}]]];

rightW = 1250 - leftW - 4;   (* 4 px gap *)

rightPlot = ListLinePlot[
  Transpose[{ns, runFrac}],
  PlotStyle   -> Directive[blue, AbsoluteThickness[2.5]],
  Frame       -> True,
  FrameLabel  -> {
    Style["Number of trials  n", labelDir],
    Style["Running P(truth inside cocked hat)", labelDir]
  },
  FrameStyle  -> Directive[frameC, AbsoluteThickness[1]],
  FrameTicksStyle -> Directive[FontFamily->"Helvetica", FontSize->15, FontColor->textC],
  GridLines   -> {Automatic, {0.15, 0.20, 0.25, 0.30, 0.35}},
  GridLinesStyle -> gridC,
  PlotRange   -> {{0, nFig}, {0.05, 0.65}},
  Background  -> bg,
  PlotLabel   -> Style[
    "Convergence of running P(inside)  |  \[Sigma] = 1.5\[CloseCurlyQuote] per sight  |  n = " <> ToString[nFig],
    titleDir],
  ImageSize   -> {rightW, 640},
  ImagePadding -> {{98, 28}, {82, 74}},
  Epilog -> {
    (* Shaded +-2 SE band *)
    {Opacity[0.18, red], Polygon[bandPts]},
    (* 0.25 dashed reference line *)
    {Directive[navy, Dashed, AbsoluteThickness[2]],
     Line[{{1., 0.25}, {N[nFig], 0.25}}]},
    (* P = 1/4 label *)
    Text[Style["P = 1/4 = 0.25  (Daniels 1951)",
               Directive[FontFamily->"Helvetica", FontSize->19, Bold, FontColor->navy]],
         {nFig * 0.40, 0.263}, {Left, Bottom}],
    (* +-2 SE band label *)
    Text[Style["\[PlusMinus]2 SE",
               Directive[FontFamily->"Helvetica", FontSize->18, FontColor->red]],
         {nFig * 0.06, 0.25 + 2 Sqrt[0.25*0.75/(nFig*0.06)] + 0.02}, {Left, Bottom}],
    (* Final value *)
    Text[Style["n=" <> ToString[nFig] <> ":  P = " <>
               ToString[NumberForm[insideFrac, {4,3}]],
               Directive[FontFamily->"Helvetica", FontSize->19, Bold, FontColor->blue]],
         {nFig * 0.80, insideFrac + 0.05}, {Center, Bottom}],
    (* Counter-intuitive warning *)
    Text[Style["A tight cocked hat does NOT mean a reliable fix!",
               Directive[FontFamily->"Helvetica", FontSize->18, Italic, FontColor->mid]],
         {nFig * 0.50, 0.60}, {Center, Bottom}]
  }
];

(* ═══════════════════════════════════════════════════════════════════════════
   ASSEMBLE two panels side by side then add title rows.
   All pixel widths enforced via ImageResize after Rasterize.
   ═══════════════════════════════════════════════════════════════════════════ *)
tgtW = 1250;
gapW = 4;

leftFull  = leftCol;                         (* already exact leftW wide    *)
rightFull = toImg[rightPlot, rightW];        (* forced to exact rightW wide *)

(* Match heights by padding the shorter panel at the bottom *)
lh = ImageDimensions[leftFull][[2]];
rh = ImageDimensions[rightFull][[2]];
maxH = Max[lh, rh];
leftPad  = ImagePad[leftFull,  {{0,0},{0, Max[0, maxH-lh]}}, bg];
rightPad = ImagePad[rightFull, {{0,0},{0, Max[0, maxH-rh]}}, bg];
gapStrip = ConstantImage[bg, {gapW, maxH}];

bodyRow = ImageAssemble[{{leftPad, gapStrip, rightPad}}, Background -> bg];
(* Confirm body width *)
bodyW = ImageDimensions[bodyRow][[1]];

(* Title strips at tgtW = 1600 px *)
mainTitle = txtImg[
  "The Cocked-Hat 25% Theorem  \[LongDash]  " <>
  "P(truth inside triangle) = 1/4  regardless of bearings or triangle size",
  tgtW, 44, 20, True];
subTitle = txtImg[
  "Three Sun sights at daytime azimuths SE / S / WSW  |  Daniels 1951  |  " <>
  "A tight cocked hat does NOT guarantee a reliable fix\n" <>
  "Theorem assumes INDEPENDENT symmetric errors, well-separated bearings, NO common systematic bias\n" <>
  "(a constant index error or refraction offset shifts all three LOPs together and breaks the 1/4 rule)",
  tgtW, 90, 15];

(* Resize bodyRow to tgtW if body width drifted slightly from rounding *)
bodyFinal = If[bodyW === tgtW, bodyRow,
               ImageResize[bodyRow, tgtW]];

fullImg = ImageAssemble[
  {{mainTitle}, {subTitle}, {bodyFinal}},
  Background -> bg];

(* Crop the 2-px macOS pink perimeter ring, re-pad white (from figures.wls) *)
fullImg = ImagePad[ImagePad[fullImg, -3], 3, White];

Export[outPath, fullImg, "PNG"];
dims = ImageDimensions[fullImg];
sz   = FileByteCount[outPath];
Print["EXPORTED: fig_cocked_hat_theorem.png  ",
      dims[[1]], "\[Times]", dims[[2]], " px  ", Round[sz/1024], " KB"];

(* Pixel probe: confirm white background at corner *)
cornerPx = ImageValue[fullImg, {5, 5}];
Print["Corner pixel (expect {1.,1.,1.}): ", cornerPx];
Print["Body row assembled at: ", bodyW, " px (target ", tgtW, ")"];
