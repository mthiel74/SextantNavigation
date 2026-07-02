#!/usr/bin/env wolframscript
(* celestial_sphere.wls -- Item 6: Celestial sphere with equatorial & horizon systems *)
(* Run: /usr/local/bin/wolframscript -file wolfram/celestial_sphere.wls             *)

$CharacterEncoding = "UTF-8";

SetDirectory[DirectoryName[$InputFileName]];
If[!DirectoryQ["../docs/images"], CreateDirectory["../docs/images"]];
outDir = "../docs/images";
imgW   = 1600;

(* ── Colors ──────────────────────────────────────────────────────────────── *)
navyC   = RGBColor[0.13, 0.29, 0.53];
blueC   = RGBColor[0.25, 0.45, 0.95];
warmC   = RGBColor[0.85, 0.50, 0.20];
bodyC   = RGBColor[0.20, 0.65, 0.35];
gridC   = Directive[GrayLevel[0.83], Thin, Opacity[0.7]];
sphereC = RGBColor[0.87, 0.92, 0.98];

(* ── Observer / body parameters ──────────────────────────────────────────── *)
phiObs  = 40.0 Degree;   (* observer latitude                *)
shaBody = 70.0 Degree;   (* SHA of body (westward from Aries) *)
decBody = 30.0 Degree;   (* declination of body (north)       *)

(* ── Key unit vectors on the celestial sphere ────────────────────────────── *)
(* Frame: x = First Point of Aries, z = NCP, y completes right-hand system.  *)
(* Westward (GHA) rotation about z is negative in this frame.                 *)
NCP         = {0, 0, 1};
SCP         = {0, 0, -1};
ariesPt     = {1, 0, 0};
bodyVec     = Normalize[{Cos[decBody] Cos[shaBody],
                         -Cos[decBody] Sin[shaBody],
                          Sin[decBody]}];
(* Observer meridian in xz-plane; zenith at latitude phiObs *)
zenith      = {Cos[phiObs], 0, Sin[phiObs]};
nadir       = -zenith;

(* Horizon cardinal points *)
northH  = {-Sin[phiObs], 0,  Cos[phiObs]};  (* north on horizon *)
southH  = { Sin[phiObs], 0, -Cos[phiObs]};  (* south on horizon *)
eastH   = {0,  1, 0};
westH   = {0, -1, 0};

(* ── Arc helpers ─────────────────────────────────────────────────────────── *)
(* Great-circle arc from unit vector a to unit vector b, n steps *)
gArc[a_, b_, n_:80] := Module[{ah, bperp, ang},
  ah    = Normalize[a];
  bperp = Normalize[b - (ah . b) ah];
  ang   = ArcCos[Clip[ah . Normalize[b], {-1, 1}]];
  Table[Cos[t] ah + Sin[t] bperp, {t, 0, ang, ang/n}]
];

(* Full great circle perpendicular to nrm *)
gCircle[nrm_, n_:120] := Module[{u, v},
  u = Normalize[Cross[nrm,
        If[Abs[Normalize[nrm] . {1,0,0}] < 0.9, {1,0,0}, {0,1,0}]]];
  v = Normalize[Cross[nrm, u]];
  Table[Cos[t] u + Sin[t] v, {t, 0, 2 Pi, 2 Pi/n}]
];

(* Midpoint label position: pushes label outward from sphere *)
midPt[a_, b_, f_:1.22] := f Normalize[Normalize[a] + Normalize[b]];

(* ── Pre-compute geometry ─────────────────────────────────────────────────── *)
equatorBodyFoot = Normalize[{bodyVec[[1]], bodyVec[[2]], 0}];
bodyHorizFoot   = Normalize[bodyVec - (zenith . bodyVec) zenith];

equatorPts  = gCircle[{0,0,1}];                (* celestial equator        *)
horizonPts  = gCircle[zenith];                  (* observer's horizon       *)
hourCircPts = gCircle[Normalize[Cross[NCP, bodyVec]]]; (* body's hour circle *)
decArcPts   = gArc[equatorBodyFoot, bodyVec];   (* declination arc          *)
shaArcPts   = gArc[ariesPt, equatorBodyFoot];   (* SHA arc along equator    *)
altArcPts   = gArc[bodyHorizFoot, bodyVec];     (* altitude arc             *)
azArcPts    = gArc[northH, bodyHorizFoot];      (* azimuth arc along horizon *)
polElevPts  = gArc[northH, NCP];               (* pole elevation = latitude *)
meridPts    = gArc[zenith, NCP];               (* upper meridian arc        *)

(* ── Sparse background grid ─────────────────────────────────────────────── *)
gridPrims = Flatten[{
  (* 6 meridians every 30 degrees *)
  Table[{gridC, Line[gCircle[{Sin[k Pi/6], -Cos[k Pi/6], 0}]]}, {k, 0, 5}],
  (* 4 latitude parallels *)
  Table[With[{z = Sin[lt Degree], r = Cos[lt Degree]},
    {gridC, Line[Table[{r Cos[t], r Sin[t], z}, {t, 0, 2 Pi, Pi/40}]]}],
    {lt, {-60, -30, 30, 60}}]
}, 1];

(* ── Font helpers ────────────────────────────────────────────────────────── *)
fBig[c_] := Directive[FontFamily->"Helvetica", FontSize->20, Bold, FontColor->c];
fMed[c_] := Directive[FontFamily->"Helvetica", FontSize->17, FontColor->c];
fSml[c_] := Directive[FontFamily->"Helvetica", FontSize->14, FontColor->c];

lk = 1.18;   (* standard label push-out factor *)

(* ── 3D figure ───────────────────────────────────────────────────────────── *)
fig3D = Graphics3D[
  {
    (* Celestial sphere — very transparent fill *)
    {Opacity[0.10], FaceForm[sphereC], EdgeForm[None], Sphere[{0,0,0}, 1]},

    (* Sparse grid *)
    Sequence @@ gridPrims,

    (* ──── Equatorial coordinate system (blue / navy) ──────────────────── *)

    (* Celestial equator — solid blue *)
    {blueC, Thickness[0.005], Line[equatorPts]},
    (* Hour circle of body — dashed blue *)
    {Directive[blueC, Opacity[0.55], Thickness[0.003], Dashed], Line[hourCircPts]},
    (* North Celestial Pole *)
    {navyC, PointSize[0.032], Point[NCP]},
    (* South Celestial Pole — dimmer *)
    {Directive[navyC, Opacity[0.5]], PointSize[0.022], Point[SCP]},
    (* First Point of Aries *)
    {navyC, PointSize[0.034], Point[ariesPt]},
    (* Declination arc — thick blue *)
    {Directive[blueC, Thickness[0.010]], Line[decArcPts]},
    (* SHA arc — thick navy *)
    {Directive[navyC, Thickness[0.010]], Line[shaArcPts]},

    (* ──── Horizon coordinate system (warm orange) ───────────────────── *)

    (* Horizon circle *)
    {warmC, Thickness[0.005], Line[horizonPts]},
    (* Vertical circle through body — dashed orange, upper + lower halves *)
    {Directive[warmC, Opacity[0.65], Thickness[0.003], Dashed],
     Line[gArc[zenith, bodyVec, 60]], Line[gArc[bodyVec, nadir, 60]]},
    (* Zenith *)
    {warmC, PointSize[0.032], Point[zenith]},
    (* Cardinal points *)
    {warmC, PointSize[0.025], Point[northH]},
    {Directive[warmC, Opacity[0.7]], PointSize[0.018], Point[southH]},
    {Directive[warmC, Opacity[0.7]], PointSize[0.018], Point[eastH]},
    {Directive[warmC, Opacity[0.7]], PointSize[0.018], Point[westH]},
    (* Altitude arc — thick orange *)
    {Directive[warmC, Thickness[0.010]], Line[altArcPts]},
    (* Azimuth arc — thick orange *)
    {Directive[warmC, Thickness[0.010]], Line[azArcPts]},

    (* ──── Meridian and pole elevation ───────────────────────────────── *)
    (* Pole elevation arc (latitude) — dashed navy *)
    {Directive[navyC, Opacity[0.75], Thickness[0.005], Dashed], Line[polElevPts]},
    (* Upper meridian arc — light gray *)
    {Directive[GrayLevel[0.60], Thickness[0.003], Dashed], Line[meridPts]},

    (* ──── Body ───────────────────────────────────────────────────────── *)
    (* Halo glow *)
    {Directive[bodyC, Opacity[0.20]], PointSize[0.095], Point[bodyVec]},
    (* Body point *)
    {bodyC, PointSize[0.048], Point[bodyVec]},

    (* ──── TEXT LABELS (Text[] primitives only — no Inset) ───────────── *)

    (* NCP *)
    Text[Style["NCP", fBig[navyC]], lk NCP, {0, -1.3}],
    (* SCP *)
    Text[Style["SCP", fSml[Directive[navyC, Opacity[0.65]]]], lk SCP, {0, 1.2}],
    (* Celestial equator *)
    Text[Style["Celestial Equator", fMed[blueC]], {0.0, -1.10, 0}, {0, 1}],
    (* First Point of Aries *)
    Text[Style["First Point of Aries (\[Gamma])", fSml[navyC]], 1.20 ariesPt, {-1.0, 0}],
    (* Body *)
    Text[Style["Body (star)", fBig[bodyC]], lk bodyVec, {-1.1, 0}],
    (* Declination arc mid-label *)
    Text[Style["\[Delta]  (declination)", fMed[blueC]],
         midPt[equatorBodyFoot, bodyVec, 1.24], {1.2, 0}],
    (* SHA arc mid-label *)
    Text[Style["SHA", fMed[navyC]],
         midPt[ariesPt, equatorBodyFoot, 1.22], {0, -1.2}],
    (* Zenith *)
    Text[Style["Zenith", fBig[warmC]], lk zenith, {0, -1.3}],
    (* Horizon label *)
    Text[Style["Horizon", fMed[warmC]], 1.10 eastH + {0, 0, -0.14}, {-1.1, 0}],
    (* Cardinal labels *)
    Text[Style["N", fBig[warmC]],  1.15 northH, {1.2, 0}],
    Text[Style["S", fMed[warmC]],  1.15 southH, {-1.2, 0}],
    Text[Style["E", fMed[warmC]],  1.14 eastH,  {0, -1.2}],
    Text[Style["W", fMed[warmC]],  1.13 westH,  {0, 1.2}],
    (* Altitude arc label *)
    Text[Style["Altitude (Hc)", fMed[warmC]],
         midPt[bodyHorizFoot, bodyVec, 1.20], {1.1, 0}],
    (* Azimuth arc label *)
    Text[Style["Azimuth (Zn)", fMed[warmC]],
         midPt[northH, bodyHorizFoot, 1.18], {0, 1.2}],
    (* Pole elevation = latitude *)
    Text[Style["\[Phi] (lat) = pole elevation", fSml[navyC]],
         midPt[northH, NCP, 1.28], {1.1, 0}]
  },
  ViewPoint    -> {2.5, -1.8, 1.1},
  ViewVertical -> {0, 0, 1},
  Lighting     -> {{"Ambient", GrayLevel[0.88]},
                   {"Directional", White, {3, 1, 4}}},
  Background   -> White,
  Boxed        -> False,
  PlotRangePadding -> 0.22,
  ImageSize    -> imgW
];

(* ── Title and caption ───────────────────────────────────────────────────── *)
titleExpr = Style[
  "Celestial Sphere: Equatorial & Horizon Coordinate Systems",
  Directive[FontFamily->"Helvetica", FontSize->26, Bold, FontColor->navyC]];

captExpr = Style[
  "GHA(body) = GAST + SHA(body)   |   " <>
  "Altitude & azimuth (sextant) are linked to GHA & declination (almanac) " <>
  "via the observer's latitude \[Phi]: the celestial pole stands at altitude \[Phi] " <>
  "above the north horizon.",
  Directive[FontFamily->"Helvetica", FontSize->16, FontColor->navyC]];

finalFig = Column[
  {titleExpr, fig3D, captExpr},
  Alignment -> Center,
  Spacings  -> {0, {0.5, 0.8}}
];

(* ── Export ──────────────────────────────────────────────────────────────── *)
outPath = FileNameJoin[{outDir, "fig_celestial_sphere.png"}];

Module[{img, dims, sz, corners},
  img  = Rasterize[finalFig, Background -> White, ImageResolution -> 144];
  (* Crop potential pink-fringe artefact then re-pad with white *)
  img  = ImagePad[ImagePad[img, -3], 3, White];
  Export[outPath, img, "PNG"];
  dims = ImageDimensions[img];
  sz   = FileByteCount[outPath];
  Print["EXPORTED: fig_celestial_sphere.png  ",
        dims[[1]], "\[Times]", dims[[2]], " px  ", Round[sz/1024], " KB"];
  (* Probe four corners for white background *)
  corners = {
    PixelValue[img, {1, 1}],
    PixelValue[img, {dims[[1]], 1}],
    PixelValue[img, {1, dims[[2]]}],
    PixelValue[img, {dims[[1]], dims[[2]]}]
  };
  Print["Corner pixels (expect {1,1,1} white): ", corners]
];
