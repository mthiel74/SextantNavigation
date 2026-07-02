#!/usr/bin/env wolframscript
(* refraction.wls — Item 7: Atmospheric refraction models figure
   Outputs: docs/images/fig_refraction.png
   Two panels:
     Left:  Refraction R (arcmin) vs altitude 0-60 deg, three models
     Right: Position error (nm) from model uncertainty vs altitude *)

$CharacterEncoding = "UTF-8";

repo = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repo, "wolfram", "CelestialNavigation.wl"}]];

outFile = FileNameJoin[{repo, "docs", "images", "fig_refraction.png"}];

(* ---- Light theme palette ---- *)
bgColor   = White;
axColor   = GrayLevel[0.2];
colBennett    = RGBColor[0.122, 0.467, 0.706];  (* blue *)
colSaemundsson = RGBColor[0.839, 0.153, 0.157]; (* red *)
colSimple     = RGBColor[0.172, 0.627, 0.172];  (* green *)
colPTenv      = RGBColor[0.580, 0.404, 0.741];  (* purple *)

altMin = 0.5; altMax = 60.0;
alts = Range[altMin, altMax, 0.25];

(* Clip refraction to max 50' for plotting (Simple diverges near 0) *)
rBennett     = Clip[cnRefraction[#], {0, 50}] & /@ alts;
rSaemundsson = Clip[cnRefractionSaemundsson[#], {0, 50}] & /@ alts;
rSimple      = Clip[cnRefractionSimple[#], {0, 50}] & /@ alts;

ptsBennett     = Transpose[{alts, rBennett}];
ptsSaemundsson = Transpose[{alts, rSaemundsson}];
ptsSimple      = Transpose[{alts, rSimple}];

(* Panel 1: Refraction vs altitude *)
(* Annotation for ~34' near-horizon value *)
annotLine = {GrayLevel[0.6], Dashed, Thickness[0.003],
             Line[{{altMin, 34}, {altMax, 34}}]};
annotText = Text[
  Style["~34\[Prime] \[TildeEqual] Sun diameter", FontSize -> 11,
        FontColor -> GrayLevel[0.5], FontFamily -> "Helvetica"],
  {28, 35.5}];

panel1 = Graphics[
  {
    annotLine, annotText,
    {colBennett, Thickness[0.004],
     Line[ptsBennett]},
    {colSaemundsson, Thickness[0.004], Dashed,
     Line[ptsSaemundsson]},
    {colSimple, Thickness[0.004], Dotted,
     Line[ptsSimple]}
  },
  Frame -> True,
  FrameStyle -> axColor,
  FrameLabel -> {
    Style["Altitude h (\[Degree])", FontSize -> 13, FontColor -> axColor],
    Style["Refraction R (\[Prime])", FontSize -> 13, FontColor -> axColor]
  },
  PlotRange -> {{altMin, altMax}, {0, 50}},
  GridLines -> {Range[0, 60, 10], Range[0, 50, 10]},
  GridLinesStyle -> Directive[GrayLevel[0.88], Thin],
  Background -> bgColor,
  AspectRatio -> 1,
  PlotRangePadding -> {{Scaled[0.02], Scaled[0.02]}, {Scaled[0.02], Scaled[0.05]}}
];

(* Panel 2: Position error (nm) from model uncertainty vs altitude
   We show two uncertainty sources:
   (a) |Saemundsson - Simple| as arcmin (model disagreement at low alt)
   (b) P,T envelope swing: range over {P=980/1040, T=-20/35} minus standard *)
altsPosErr = Range[1.0, 60.0, 0.5];

modelGapNm = (Abs[cnRefractionSaemundsson[#] - cnRefractionSimple[#]]) & /@ altsPosErr;
(* P,T swing: max over {(980,-20),(980,35),(1040,-20),(1040,35)} - standard *)
ptSwingNm = Function[h,
  Max[
    Abs[cnRefractionPT[h, 980, -20]  - cnRefraction[h]],
    Abs[cnRefractionPT[h, 980, 35]   - cnRefraction[h]],
    Abs[cnRefractionPT[h, 1040, -20] - cnRefraction[h]],
    Abs[cnRefractionPT[h, 1040, 35]  - cnRefraction[h]]
  ]
] /@ altsPosErr;

ptsModelGap = Transpose[{altsPosErr, modelGapNm}];
ptsPTSwing  = Transpose[{altsPosErr, ptSwingNm}];

(* Mark the 10-deg threshold *)
threshLine = {GrayLevel[0.6], Dashed, Thickness[0.003],
              Line[{{10, 0}, {10, 100}}]};
threshText = Text[
  Style["10\[Degree] threshold", FontSize -> 11,
        FontColor -> GrayLevel[0.5], FontFamily -> "Helvetica"],
  {13, 45}];

panel2 = Graphics[
  {
    threshLine, threshText,
    {colBennett, Thickness[0.004],
     Line[ptsModelGap]},
    {colPTenv, Thickness[0.004], Dashed,
     Line[ptsPTSwing]}
  },
  Frame -> True,
  FrameStyle -> axColor,
  FrameLabel -> {
    Style["Altitude h (\[Degree])", FontSize -> 13, FontColor -> axColor],
    Style["Position error (nm)", FontSize -> 13, FontColor -> axColor]
  },
  PlotRange -> {{1, 60}, {0, 85}},
  GridLines -> {Range[0, 60, 10], Range[0, 80, 10]},
  GridLinesStyle -> Directive[GrayLevel[0.88], Thin],
  Background -> bgColor,
  AspectRatio -> 1,
  PlotRangePadding -> {{Scaled[0.02], Scaled[0.02]}, {Scaled[0.02], Scaled[0.05]}}
];

(* Legend as a Graphics object *)
legend1 = Graphics[{
  {colBennett, Thickness[0.06], Line[{{0,0.82},{0.18,0.82}}]},
  Text[Style["Bennett (canonical)", FontSize -> 11, FontColor -> axColor,
             FontFamily -> "Helvetica"], {0.22, 0.82}, {-1, 0}],
  {colSaemundsson, Dashed, Thickness[0.06], Line[{{0,0.55},{0.18,0.55}}]},
  Text[Style["Saemundsson (from true alt)", FontSize -> 11, FontColor -> axColor,
             FontFamily -> "Helvetica"], {0.22, 0.55}, {-1, 0}],
  {colSimple, Dotted, Thickness[0.06], Line[{{0,0.28},{0.18,0.28}}]},
  Text[Style["Simple/Smart (0.96 cot h)", FontSize -> 11, FontColor -> axColor,
             FontFamily -> "Helvetica"], {0.22, 0.28}, {-1, 0}]
}, PlotRange -> {{0,1},{0,1}}, AspectRatio -> 1/3,
   ImageSize -> {380, 80}, Background -> bgColor];

legend2 = Graphics[{
  {colBennett, Thickness[0.06], Line[{{0,0.72},{0.18,0.72}}]},
  Text[Style["|Saemundsson \[Minus] Simple| (model gap)", FontSize -> 11,
             FontColor -> axColor, FontFamily -> "Helvetica"], {0.22, 0.72}, {-1, 0}],
  {colPTenv, Dashed, Thickness[0.06], Line[{{0,0.28},{0.18,0.28}}]},
  Text[Style["P,T swing (\[PlusMinus]30 hPa, \[PlusMinus]25\[Degree]C)", FontSize -> 11,
             FontColor -> axColor, FontFamily -> "Helvetica"], {0.22, 0.28}, {-1, 0}]
}, PlotRange -> {{0,1},{0,1}}, AspectRatio -> 1/3,
   ImageSize -> {380, 65}, Background -> bgColor];

title1 = Graphics[{
  Text[Style["Atmospheric Refraction Models", Bold, FontSize -> 14,
             FontColor -> axColor, FontFamily -> "Helvetica"], {0.5, 0.5}]
}, PlotRange -> {{0,1},{0,1}}, AspectRatio -> 1/8,
   ImageSize -> {420, 35}, Background -> bgColor];

title2 = Graphics[{
  Text[Style["Position Error vs Sight Altitude", Bold, FontSize -> 14,
             FontColor -> axColor, FontFamily -> "Helvetica"], {0.5, 0.5}]
}, PlotRange -> {{0,1},{0,1}}, AspectRatio -> 1/8,
   ImageSize -> {420, 35}, Background -> bgColor];

col1 = Column[{title1, Show[panel1, ImageSize -> 420], legend1},
              Alignment -> Left, Spacings -> 4];
col2 = Column[{title2, Show[panel2, ImageSize -> 420], legend2},
              Alignment -> Left, Spacings -> 4];

combined = Row[{col1, Spacer[30], col2}];

img = Rasterize[combined, "Image", ImageResolution -> 150, Background -> bgColor];

(* Verify white background *)
{w, h} = ImageDimensions[img];
cornerPixels = {
  PixelValue[img, {1, 1}],
  PixelValue[img, {w, 1}],
  PixelValue[img, {1, h}],
  PixelValue[img, {w, h}]
};
Print["Corner pixel values (should be ~1.0 for white): ", cornerPixels];

Export[outFile, img, "PNG"];
fileSize = FileByteCount[outFile];
{pw, ph} = ImageDimensions[img];
Print["Output: ", outFile];
Print["Dimensions: ", pw, " x ", ph, " px"];
Print["File size: ", Round[fileSize/1024], " KB"];
Print["DONE"];
