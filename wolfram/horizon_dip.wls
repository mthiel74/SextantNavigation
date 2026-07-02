#!/usr/bin/env wolframscript
(* horizon_dip.wls — Item 9: Dip of the horizon and height of eye
   Outputs: docs/images/fig_horizon_dip.png
   Two panels:
     Left:  2D geometric diagram — Earth curve, eye at height h, tangent
            line to sea horizon, true horizontal, dip angle, distance label
     Right: Quantitative plots — dip (arcmin) and horizon distance (nm)
            vs height of eye 0–25 m, with real-world cases and sensitivity note *)

$CharacterEncoding = "UTF-8";

repo = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repo, "wolfram", "CelestialNavigation.wl"}]];

outFile = FileNameJoin[{repo, "docs", "images", "fig_horizon_dip.png"}];

(* ---- Sanity checks ---- *)
d2m   = N[cnDip[2],   4];
d20m  = N[cnDip[20],  4];
dist20 = N[2.07 * Sqrt[20.0], 4];
Print["Sanity: dip@2 m  = ", d2m,   "\[Prime]  (expect ~2.49\[Prime])"];
Print["Sanity: dip@20 m = ", d20m,  "\[Prime]  (expect ~7.87\[Prime])"];
Print["Sanity: horizon@20 m = ", dist20, " nm  (expect ~9.3 nm)"];

(* ---- Palette ---- *)
bgColor = White;
axColor = GrayLevel[0.18];
colBlue = RGBColor[0.25, 0.45, 0.95];
colNavy = RGBColor[0.13, 0.29, 0.53];
colWarm = RGBColor[0.85, 0.50, 0.20];
colSky  = RGBColor[0.87, 0.93, 0.98];
colSea  = RGBColor[0.22, 0.50, 0.72];   (* surface arc *)
colEarth = RGBColor[0.18, 0.40, 0.58];  (* ocean floor fill *)

(* ================================================================
   PANEL 1 — GEOMETRIC DIAGRAM
   ================================================================
   Diagram coordinate system:
     Earth centre at {0, 0}, diagram radius Rd = 1
     Observer foot (nadir) at {0, Rd} = top of Earth
     Observer eye at {0, d} where d = Rd + hd (hd exaggerated)
*)

Rd = 1.0;
hd = 0.22;    (* exaggerated; true ratio h/R ~ 0.0002 for h = 1.5 m *)
d  = Rd + hd; (* observer distance from Earth centre *)

(* Tangent contact angle: cos(alpha) = Rd / d *)
alpha = ArcCos[Rd / d];   (* ~ 34.7 deg in diagram *)

(* Tangent / horizon point on surface, to the left *)
Hx = -Rd * Sin[alpha];
Hy =  Rd * Cos[alpha];
horizPt  = {Hx, Hy};
eyePt    = {0.0, d};
nadirPt  = {0.0, Rd};

(* Direction from eye to sea horizon *)
dirSight = Normalize[horizPt - eyePt];

(* True horizontal (leftward) *)
dirHoriz = {-1.0, 0.0};

(* Dip angle between true horizontal and sight-to-horizon *)
dipAngleDiag = ArcCos[Dot[dirHoriz, dirSight]];  (* radians *)

(* ---- Earth arc and fill ---- *)
arcHalf = 78 * Degree;
arcPts = Table[
  {Rd * Cos[t], Rd * Sin[t]},
  {t, Pi/2 - arcHalf, Pi/2 + arcHalf, 0.8 * Degree}
];
yBtm = -0.16;
earthFillPoly = Polygon[Join[arcPts,
  {{Last[arcPts][[1]], yBtm}, {First[arcPts][[1]], yBtm}}]];

(* ---- Key lines ---- *)
sightEnd   = eyePt + 1.07 * (horizPt - eyePt);
horizLeft  = eyePt + {-0.90, 0.0};
horizRight = eyePt + {0.30, 0.0};

(* ---- Height-of-eye marker ---- *)
hMarkX = 0.08;
tickLen = 0.032;

(* ---- Earth-radius marker (dashed, lower-right quadrant) ---- *)
rAngle = -50 * Degree;  (* from top: point down-right *)
rPtOnSurf = {Rd * Sin[-rAngle], Rd * Cos[-rAngle]};   (* on surface *)

(* ---- Dip angle arc ---- *)
dipArcR = 0.175;
sightAngle = ArcTan[dirSight[[1]], dirSight[[2]]];  (* angle from +x, CCW; third quadrant *)
(* Short arc: CW from sightAngle down to -Pi *)
dipArcPts = Table[
  eyePt + dipArcR * {Cos[t], Sin[t]},
  {t, sightAngle, -Pi, -0.5 * Degree}
];
dipLabelAngle = (sightAngle + (-Pi)) / 2;
dipLabelOff   = dipArcR + 0.10;
dipLabelPos   = eyePt + dipLabelOff * {Cos[dipLabelAngle], Sin[dipLabelAngle]};

(* ---- Right-angle box at tangent point ---- *)
rDirH = Normalize[horizPt];                          (* radius direction *)
tDirH = {-rDirH[[2]], rDirH[[1]]};                  (* 90 deg CCW: tangent direction *)
sqSz  = 0.042;
boxPts = {horizPt + sqSz*rDirH,
          horizPt + sqSz*rDirH + sqSz*tDirH,
          horizPt + sqSz*tDirH};

(* ---- Label positions ---- *)
sightMidPos   = (eyePt + horizPt) / 2 + {-0.02, 0.09};
hLabelPos     = {hMarkX + 0.065, (Rd + d) / 2};
rLabelPos     = 0.55 * rPtOnSurf + {0.10, 0.02};
earthLabelPos = {0.0, 0.58};

(* ---- Build diagram Graphics ---- *)
diag = Graphics[
  {
    (* Sky background *)
    {colSky, Rectangle[{-1.38, Rd - 0.002}, {0.55, 1.60}]},
    (* Ocean fill *)
    {colEarth, earthFillPoly},
    (* Earth surface arc *)
    {colSea, Thickness[0.007], Line[arcPts]},

    (* True horizontal *)
    {colBlue, Dashing[{0.015, 0.010}], Thickness[0.0045],
     Line[{horizLeft, horizRight}]},
    (* Sight line to horizon *)
    {colWarm, Thickness[0.0055],
     Line[{eyePt, sightEnd}]},

    (* Height marker: vertical bar *)
    {GrayLevel[0.35], Thickness[0.003],
     Line[{{hMarkX, Rd}, {hMarkX, d}}]},
    {GrayLevel[0.35], Thickness[0.003],
     Line[{{hMarkX - tickLen, Rd}, {hMarkX + tickLen, Rd}}]},
    {GrayLevel[0.35], Thickness[0.003],
     Line[{{hMarkX - tickLen, d}, {hMarkX + tickLen, d}}]},

    (* Earth radius dashed marker *)
    {GrayLevel[0.55], Dashing[{0.01, 0.008}], Thickness[0.003],
     Line[{{0.0, 0.0}, rPtOnSurf}]},

    (* Dip angle arc *)
    {colWarm, Thickness[0.006], Line[dipArcPts]},

    (* Right-angle box at tangent point *)
    {GrayLevel[0.35], Thickness[0.003], Line[boxPts]},

    (* Eye dot *)
    {colBlue, PointSize[0.024], Point[eyePt]},
    (* Horizon dot *)
    {colNavy, PointSize[0.019], Point[horizPt]},
    (* Earth centre dot *)
    {GrayLevel[0.6], PointSize[0.012], Point[{0.0, 0.0}]},

    (* ----- Text labels (no Inset) ----- *)

    (* "True horizontal" label *)
    Text[Style["True horizontal", FontSize -> 11, FontColor -> colBlue,
               FontFamily -> "Helvetica", FontSlant -> Italic],
         {-0.82, d + 0.045}, {-1, 0}],

    (* "Sea horizon" *)
    Text[Style["Sea horizon", FontSize -> 11, FontColor -> colSea,
               FontFamily -> "Helvetica"],
         horizPt + {-0.06, -0.09}, {1, 0}],

    (* Dip angle "d" with prime *)
    Text[Style["dip  d", Bold, FontSize -> 13, FontColor -> colWarm,
               FontFamily -> "Helvetica"],
         dipLabelPos, {0, 0}],

    (* Height label *)
    Text[Style["h", FontSize -> 13, FontColor -> GrayLevel[0.25],
               FontFamily -> "Helvetica", FontSlant -> Italic],
         hLabelPos, {-1, 0}],

    (* R label *)
    Text[Style["R", FontSize -> 13, FontColor -> GrayLevel[0.50],
               FontFamily -> "Helvetica", FontSlant -> Italic],
         rLabelPos, {0, 0}],

    (* Distance label along sight *)
    Text[Style["D \[TildeEqual] 2.07 \[Times] \[Sqrt]h  nm",
               FontSize -> 11, FontColor -> colWarm,
               FontFamily -> "Helvetica"],
         sightMidPos, {0, 0}],

    (* Eye label *)
    Text[Style["Eye", FontSize -> 11, FontColor -> colBlue,
               FontFamily -> "Helvetica"],
         eyePt + {0.07, 0.035}, {-1, 0}],

    (* Earth centre label *)
    Text[Style["C", FontSize -> 11, FontColor -> GrayLevel[0.60],
               FontFamily -> "Helvetica", FontSlant -> Italic],
         {0.0, 0.0} + {0.06, -0.06}, {-1, 0}],

    (* Ocean label *)
    Text[Style["Ocean", FontSize -> 11, FontColor -> GrayLevel[0.88],
               FontFamily -> "Helvetica"],
         earthLabelPos, {0, 0}],

    (* Not to scale notice *)
    Text[Style["(curvature greatly exaggerated \[LongDash] not to scale)",
               FontSize -> 9, FontColor -> GrayLevel[0.52],
               FontFamily -> "Helvetica", FontSlant -> Italic],
         {-0.42, yBtm + 0.04}, {0, 0}]
  },
  PlotRange -> {{-1.38, 0.55}, {yBtm, 1.60}},
  Background -> bgColor,
  AspectRatio -> Full,
  ImageSize -> {500, 500}
];

(* ================================================================
   PANEL 2 — QUANTITATIVE PLOTS
   ================================================================ *)

heights  = Range[0.1, 25.0, 0.1];
dipVals  = cnDip /@ heights;          (* arcmin *)
distVals = 2.07 * Sqrt[heights];      (* nm *)

(* Real-world cases: {label, h_m, dip_label_y_offset, dist_label_y_offset} *)
cases = {
  {"Dinghy (~1 m)",        1.0,  0.28,  0.18},
  {"Yacht deck (~3 m)",    3.0,  0.28,  0.20},
  {"Ship bridge (~20 m)", 20.0, -0.70, -0.55}
};

(* Sensitivity: d(1.76*sqrt(h))/dh = 0.88/sqrt(h) arcmin/m *)
sens2m = 0.88 / Sqrt[2.0];   (* arcmin per metre at h = 2 m *)
sens2mStr = ToString[Round[sens2m, 0.01]];

(* Dip plot primitives *)
dipCasePrims = Flatten[Map[Function[c,
  {{colWarm, PointSize[0.022], Point[{c[[2]], cnDip[c[[2]]]}]},
   Text[Style[c[[1]], FontSize -> 10, FontColor -> colWarm,
              FontFamily -> "Helvetica"],
        {c[[2]] + 0.5, cnDip[c[[2]]] + c[[3]]}, {-1, 0}]}
], cases], 1];

panel2a = Graphics[
  Join[
    {{colBlue, Thickness[0.006], Line[Transpose[{heights, dipVals}]]}},
    dipCasePrims,
    {
      (* Formula box *)
      Text[Style["dip(\[Prime]) = 1.76 \[Times] \[Sqrt]h  (h in m)",
                 FontSize -> 11, FontColor -> colBlue,
                 FontFamily -> "Helvetica", FontSlant -> Italic],
           {0.6, 8.8}, {-1, 1}],
      (* Sensitivity note *)
      Text[Style[
        "Sensitivity: \[PartialD]dip/\[PartialD]h = 0.88/\[Sqrt]h \[Prime]/m\n" <>
        "At h = 2 m: 1 m error \[RightArrow] +" <> sens2mStr <>
        "\[Prime] in dip \[TildeEqual] +" <> sens2mStr <> " nm fix error",
        FontSize -> 10, FontColor -> colWarm,
        FontFamily -> "Helvetica"],
        {12.5, 1.8}, {-1, 0}]
    }
  ],
  Frame -> True,
  FrameStyle -> axColor,
  FrameLabel -> {
    Style["Height of eye  h  (m)", FontSize -> 13, FontColor -> axColor],
    Style["Dip  (\[Prime])", FontSize -> 13, FontColor -> axColor]
  },
  PlotRange -> {{0, 25}, {0, 9.2}},
  GridLines -> {Range[0, 25, 5], Range[0, 9, 1]},
  GridLinesStyle -> Directive[GrayLevel[0.88], Thin],
  Background -> bgColor,
  AspectRatio -> 0.60,
  PlotRangePadding -> {{Scaled[0.02], Scaled[0.02]}, {Scaled[0.02], Scaled[0.04]}}
];

(* Distance plot primitives *)
distCasePrims = Flatten[Map[Function[c,
  {{colNavy, PointSize[0.022], Point[{c[[2]], 2.07*Sqrt[c[[2]]]}]},
   Text[Style[c[[1]], FontSize -> 10, FontColor -> colNavy,
              FontFamily -> "Helvetica"],
        {c[[2]] + 0.5, 2.07*Sqrt[c[[2]]] + c[[4]]}, {-1, 0}]}
], cases], 1];

panel2b = Graphics[
  Join[
    {{colNavy, Thickness[0.006], Line[Transpose[{heights, distVals}]]}},
    distCasePrims,
    {
      Text[Style["D (nm) = 2.07 \[Times] \[Sqrt]h  (h in m)",
                 FontSize -> 11, FontColor -> colNavy,
                 FontFamily -> "Helvetica", FontSlant -> Italic],
           {0.6, 10.1}, {-1, 1}]
    }
  ],
  Frame -> True,
  FrameStyle -> axColor,
  FrameLabel -> {
    Style["Height of eye  h  (m)", FontSize -> 13, FontColor -> axColor],
    Style["Distance to horizon  (nm)", FontSize -> 13, FontColor -> axColor]
  },
  PlotRange -> {{0, 25}, {0, 10.6}},
  GridLines -> {Range[0, 25, 5], Range[0, 10, 2]},
  GridLinesStyle -> Directive[GrayLevel[0.88], Thin],
  Background -> bgColor,
  AspectRatio -> 0.60,
  PlotRangePadding -> {{Scaled[0.02], Scaled[0.02]}, {Scaled[0.02], Scaled[0.04]}}
];

(* ---- Title bars ---- *)
makeTitle[str_] := Graphics[
  {Text[Style[str, Bold, FontSize -> 14, FontColor -> axColor,
              FontFamily -> "Helvetica"], {0.5, 0.5}]},
  PlotRange -> {{0, 1}, {0, 1}},
  Background -> bgColor,
  ImageSize -> {490, 32},
  AspectRatio -> Full
];

titleLeft  = makeTitle["Geometry of Dip of the Horizon"];
titleRight = makeTitle["Dip and Horizon Distance vs Eye Height"];

(* ---- Assemble ---- *)
leftCol = Column[
  {titleLeft, Show[diag, ImageSize -> {490, 490}]},
  Alignment -> Left, Spacings -> 3
];

rightCol = Column[
  {titleRight,
   Show[panel2a, ImageSize -> {490, 285}],
   Spacer[6],
   Show[panel2b, ImageSize -> {490, 285}]},
  Alignment -> Left, Spacings -> 3
];

combined = Row[{leftCol, Spacer[28], rightCol}];

(* ---- Rasterize ---- *)
img = Rasterize[combined, "Image", ImageResolution -> 150, Background -> bgColor];
img = ColorConvert[img, "RGB"];

(* Resize to ~1600 px wide *)
{pw, ph} = ImageDimensions[img];
If[pw =!= 1600,
  img = ImageResize[img, {1600, Automatic}];
  {pw, ph} = ImageDimensions[img];
];

(* Verify white background (corner pixel probe) *)
cornerPx = {
  PixelValue[img, {1,  1}],
  PixelValue[img, {pw, 1}],
  PixelValue[img, {1,  ph}],
  PixelValue[img, {pw, ph}]
};
Print["Corner pixel values (expect ~{1,1,1} white): ", N[cornerPx, 3]];

Export[outFile, img, "PNG"];
fileSize = FileByteCount[outFile];
Print["Output: ", outFile];
Print["Dimensions: ", pw, " x ", ph, " px"];
Print["File size: ", Round[fileSize / 1024], " KB"];
Print["DONE"];
