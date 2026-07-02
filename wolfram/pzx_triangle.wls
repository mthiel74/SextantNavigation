#!/usr/bin/env wolframscript
(* pzx_triangle.wls -- Navigational (PZX) triangle on the celestial sphere    *)
(* Item 5: spherical-trig foundation of the altitude/azimuth formula           *)
(* Run: /usr/local/bin/wolframscript -file wolfram/pzx_triangle.wls            *)

$CharacterEncoding = "UTF-8";
SetDirectory[DirectoryName[$InputFileName]];

If[!DirectoryQ["../docs/images"], CreateDirectory["../docs/images"]];
outFile = "../docs/images/fig_pzx_triangle.png";

(* ── Colours (LIGHT theme) ─────────────────────────────────────────────────── *)
bgCol     = White;
sphereCol = RGBColor[0.87, 0.91, 0.97];
navyCol   = RGBColor[0.13, 0.29, 0.53];   (* PZ side, P vertex, LHA label *)
accentCol = RGBColor[0.25, 0.45, 0.95];   (* ZX side, Z vertex, Zn label  *)
warmCol   = RGBColor[0.85, 0.50, 0.20];   (* PX side, X vertex             *)
subtleCol = GrayLevel[0.66];
textCol   = RGBColor[0.10, 0.15, 0.30];

(* ── Example parameters ─────────────────────────────────────────────────────── *)
phi   = 30. Degree;   (* observer latitude  *)
delta = 15. Degree;   (* body declination   *)
lha   = 40. Degree;   (* local hour angle   *)

(* Spherical law of cosines → altitude *)
sinH = Sin[phi]*Sin[delta] + Cos[phi]*Cos[delta]*Cos[lha];
H    = ArcSin[sinH];
Hdeg = H / Degree;

Print["=== PZX Triangle Example ==="];
Print["phi=30\[Degree]  delta=15\[Degree]  LHA=40\[Degree]  => H = ",
      NumberForm[Hdeg, {6, 3}], "\[Degree]"];
Print["co-latitude  PZ = ", 90-30, "\[Degree]"];
Print["co-declination PX = ", 90-15, "\[Degree]"];
Print["co-altitude  ZX = ", NumberForm[90-Hdeg, {5, 2}], "\[Degree]"];

(* ── Vertex positions (unit sphere) ─────────────────────────────────────────── *)
(* Coord system: z = North Celestial Pole, x = observer meridian on equator,    *)
(*               y = East; LHA increases westward => body offset toward -y.      *)
pVec = {0., 0., 1.};
zVec = {Cos[phi], 0., Sin[phi]};
xVec = {Cos[delta]*Cos[lha], -Cos[delta]*Sin[lha], Sin[delta]};

(* Sanity check: Z.X should equal sinH *)
Print["Z\[CenterDot]X = ", N[xVec . zVec, 6], "  sinH = ", N[sinH, 6],
      "  match? ", Abs[xVec . zVec - sinH] < 1.*^-10];

(* ── Great-circle arc via SLERP ─────────────────────────────────────────────── *)
slerp[a_, b_, t_] := Module[{om = ArcCos[Clip[a . b, {-1., 1.}]]},
  If[om < 1.*^-9, a, (Sin[(1-t)*om]*a + Sin[t*om]*b) / Sin[om]]
];

gcArc[a_, b_, n_:80] := Table[N[slerp[a, b, k/n]], {k, 0, n}];

(* Full great circle given its pole normal *)
gcFull[norm_, n_:120] := Module[{ax, u, v},
  ax = If[Abs[norm . {0,0,1}] < 0.9, {0.,0.,1.}, {1.,0.,0.}];
  u  = Normalize[Cross[norm, ax]];
  v  = Normalize[Cross[norm, u]];
  Table[Cos[2 Pi k/n]*u + Sin[2 Pi k/n]*v, {k, 0, n}]
];

(* Small arc indicating the angle at vertex v between edges to a and b *)
angleArc[v_, a_, b_, r_:0.20, n_:50] := Module[{ta, tb},
  ta = Normalize[a - (a . v)*v];
  tb = Normalize[b - (b . v)*v];
  Map[(v + r * #)&, gcArc[ta, tb, n]]
];

(* ── Build great circles and arcs ──────────────────────────────────────────── *)
equPts  = gcFull[{0.,0.,1.}];           (* celestial equator  *)
horPts  = gcFull[zVec];                  (* observer horizon   *)
merPts  = gcFull[{0.,1.,0.}];           (* observer meridian  *)

arcPZ   = gcArc[pVec, zVec];            (* co-latitude side   *)
arcPX   = gcArc[pVec, xVec];            (* co-declination side*)
arcZX   = gcArc[zVec, xVec];            (* co-altitude side   *)

angAtP  = angleArc[pVec, zVec, xVec, 0.20];   (* LHA angle at P *)
angAtZ  = angleArc[zVec, pVec, xVec, 0.20];   (* Azimuth at Z   *)

(* ── Label anchor points (midpoint of arc = Normalize[a+b], scaled out) ──── *)
midPZ = Normalize[pVec + zVec] * 1.15 + {0., -0.10, 0.};
midPX = Normalize[pVec + xVec] * 1.18 + {0., -0.06, 0.};
midZX = Normalize[zVec + xVec] * 1.17 + {0.,  0.12, 0.};

(* Angle label positions (just outside the angle arcs) *)
angPpos = pVec + {0.08, -0.33, -0.22};
angZpos = zVec + {-0.16, 0.22, 0.16};

(* ── Side label strings ───────────────────────────────────────────────────── *)
colatStr = "PZ = 90\[Degree]\[Minus]\[Phi] = 60\[Degree]";
codeStr  = "PX = 90\[Degree]\[Minus]\[Delta] = 75\[Degree]";
coaltStr = "ZX = 90\[Degree]\[Minus]H \[TildeTilde] " <>
           ToString[NumberForm[Round[90 - Hdeg, 0.1], {4, 1}]] <> "\[Degree]";

(* ── Style helper ─────────────────────────────────────────────────────────── *)
lbl[str_, col_, sz_:11] :=
  Style[str, FontFamily -> "Helvetica", FontSize -> sz, FontColor -> col];

(* ── Graphics3D primitives ───────────────────────────────────────────────── *)
prims = {
  (* Translucent sphere — light blue *)
  {Opacity[0.16], sphereCol, Sphere[{0.,0.,0.}, 1.]},
  {Opacity[1.]},

  (* Celestial equator — subtle dashed *)
  {Directive[subtleCol, Dashed, AbsoluteThickness[1.2]], Line[equPts]},

  (* Observer's meridian — very faint *)
  {Directive[GrayLevel[0.80], Dashed, AbsoluteThickness[0.7]], Line[merPts]},

  (* Observer's horizon — subtle dashed, slightly darker *)
  {Directive[GrayLevel[0.56], Dashed, AbsoluteThickness[1.2]], Line[horPts]},

  (* PZ = co-latitude, navy *)
  {Directive[navyCol, AbsoluteThickness[3.0]], Line[arcPZ]},

  (* PX = co-declination, warm orange *)
  {Directive[warmCol, AbsoluteThickness[3.0]], Line[arcPX]},

  (* ZX = co-altitude, accent blue *)
  {Directive[accentCol, AbsoluteThickness[3.0]], Line[arcZX]},

  (* Angle indicator at P (LHA) *)
  {Directive[navyCol, Opacity[0.75], AbsoluteThickness[1.8]], Line[angAtP]},

  (* Angle indicator at Z (Zn) *)
  {Directive[accentCol, Opacity[0.75], AbsoluteThickness[1.8]], Line[angAtZ]},

  {Opacity[1.]},

  (* Vertex dots *)
  {navyCol,   Sphere[pVec, 0.030]},
  {accentCol, Sphere[zVec, 0.030]},
  {warmCol,   Sphere[xVec, 0.030]},

  (* Vertex labels — large, bold *)
  Text[lbl["P", navyCol, 20],   pVec + { 0.04, -0.03,  0.11}],
  Text[lbl["Z", accentCol, 20], zVec + { 0.10, -0.03, -0.09}],
  Text[lbl["X", warmCol, 20],   xVec + { 0.08, -0.06,  0.09}],

  (* Side labels at arc midpoints *)
  Text[lbl[colatStr, navyCol,   12], midPZ],
  Text[lbl[codeStr,  warmCol,   12], midPX],
  Text[lbl[coaltStr, accentCol, 12], midZX],

  (* Angle labels *)
  Text[lbl["LHA = 40\[Degree]", navyCol,   12], angPpos],
  Text[lbl["Zn",                accentCol, 12], angZpos]
};

g3d = Graphics3D[prims,
  Background      -> White,
  Boxed           -> False,
  Lighting        -> {{"Ambient",     GrayLevel[0.82]},
                      {"Directional", White,           {3., -2., 4.}},
                      {"Directional", GrayLevel[0.30], {-1., 1., -2.}}},
  ViewPoint       -> {1.65, -2.55, 1.55},
  ViewVertical    -> {0, 0, 1},
  ImageSize       -> 1400,
  SphericalRegion -> True,
  PlotRangePadding -> Scaled[0.10]
];

(* ── Wrap with title and caption via Labeled ─────────────────────────────── *)
titleStr = "The Navigational (PZX) Triangle";
captStr  = "sin H = sin \[Phi] sin \[Delta] + cos \[Phi] cos \[Delta] cos(LHA)" <>
           "     [\[Phi]=30\[Degree], \[Delta]=15\[Degree], LHA=40\[Degree]" <>
           ", H\[TildeTilde]" <> ToString[NumberForm[Round[Hdeg, 0.1], {4, 1}]] <>
           "\[Degree]]";

fig = Labeled[g3d,
  {Style[titleStr,
         FontFamily -> "Helvetica", FontSize -> 17, Bold, FontColor -> textCol],
   Style[captStr,
         FontFamily -> "Helvetica", FontSize -> 12, Italic, FontColor -> textCol]},
  {Top, Bottom}
];

(* ── Export ──────────────────────────────────────────────────────────────── *)
img  = Rasterize[fig, Background -> White, ImageSize -> {1600, Automatic}];
img  = ImagePad[ImagePad[img, -3], 3, White];
Export[outFile, img, "PNG"];

dims   = ImageDimensions[img];
szB    = FileByteCount[outFile];
corner = PixelValue[img, {1, 1}];

Print["---"];
Print["EXPORTED: ", outFile];
Print["Dims: ", dims[[1]], "\[Times]", dims[[2]], " px"];
Print["Size: ", Round[szB/1024], " KB"];
Print["Corner pixel RGB: ", corner, "  white? ", corner == {1.,1.,1.}];
Print["H = ", NumberForm[Hdeg, {6, 3}], "\[Degree]  (phi=30\[Degree], delta=15\[Degree], LHA=40\[Degree])"];
