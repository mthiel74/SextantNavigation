#!/usr/bin/env wolframscript
(* crlb.wls — Fisher information / Cramer-Rao bound figure (Item 11)
   Output: docs/images/fig_crlb.png
   Shows: MC fix scatter (light blue) + empirical 95% ellipse (solid navy)
          + CRLB 95% ellipse (dashed orange) with CEP annotations. *)

$CharacterEncoding = "UTF-8";

repo = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repo, "wolfram", "CelestialNavigation.wl"}]];

(* ── Scenario ──────────────────────────────────────────────────────────────── *)
truePos = {20., -40.};       (* 20\[Degree]N, 40\[Degree]W *)
sigma   = 1.5;               (* sight \[Sigma] in arcminutes = nm along LOP *)
t1 = DateObject[{2024, 11, 15, 10, 0, 0}, TimeZone -> 0];
t2 = DateObject[{2024, 11, 15, 14, 0, 0}, TimeZone -> 0];
t3 = DateObject[{2024, 11, 15, 17, 0, 0}, TimeZone -> 0];
times = {t1, t2, t3};

(* ── CRLB ───────────────────────────────────────────────────────────────────── *)
azimuths  = cnComputedAltitude[truePos, cnSunGP[#]][[2]] & /@ times;
cov       = cnCRLBCovariance[azimuths, sigma];
crlbEll   = cnErrorEllipse[cov];

Print["Azimuths (deg): ", Round[azimuths, 0.1]];
Print["CRLB semi-major: ", Round[crlbEll["semiMajorNm"], 0.001], " nm"];
Print["CRLB semi-minor: ", Round[crlbEll["semiMinorNm"], 0.001], " nm"];
Print["CRLB CEP: ",        Round[crlbEll["cep"],         0.001], " nm"];

(* ── Monte-Carlo fix (n = 3000) ─────────────────────────────────────────────── *)
Print["Running MC..."];
mcResult = cnMonteCarloFix[truePos, times,
             <|"sigmaMin" -> sigma, "seed" -> 7|>, 3000];
mcEll    = cnErrorEllipse[mcResult["covNm"]];

Print["MC semi-major: ", Round[mcEll["semiMajorNm"], 0.001], " nm"];
Print["MC semi-minor: ", Round[mcEll["semiMinorNm"], 0.001], " nm"];
Print["MC CEP: ",        Round[mcResult["cep"],       0.001], " nm"];

(* ── Convert fixes to local E/N errors (nm) ─────────────────────────────────── *)
toEN[fix_] := {(fix[[2]] - truePos[[2]]) Cos[truePos[[1]] Degree] 60,
               (fix[[1]] - truePos[[1]]) 60};
enPts = toEN /@ mcResult["fixes"];

(* ── Build 95% ellipse point lists ──────────────────────────────────────────── *)
(* 95% bivariate normal: chi^2_2(0.95) = -2 ln(0.05) ~5.991; scale = sqrt of that *)
s95 = Sqrt[-2 Log[0.05]];

ellipsePoints[cmat_, scale_] := Module[
  {eig, vals, vecs, order, l1, l2, v1, v2},
  eig   = Eigensystem[N[cmat]];
  vals  = eig[[1]]; vecs = eig[[2]];
  order = Ordering[vals, All, Greater];
  l1 = vals[[order[[1]]]]; l2 = vals[[order[[2]]]];
  v1 = vecs[[order[[1]]]]; v2 = vecs[[order[[2]]]];
  (* Parametric ellipse: scale*Sqrt[l1]*Cos[t]*v1 + scale*Sqrt[l2]*Sin[t]*v2 *)
  N @ Table[scale Sqrt[l1] Cos[t] v1 + scale Sqrt[l2] Sin[t] v2,
    {t, 0, 2 Pi, 2 Pi / 360}]
];

crlbEllPts = ellipsePoints[cov,              s95];
mcEllPts   = ellipsePoints[mcResult["covNm"], s95];

(* ── Axis range ──────────────────────────────────────────────────────────────── *)
(* Generous margin: circle occupies the central ~64%, leaving clean empty strips
   at top (title + stats) and bottom (legend) so no text overlaps the ellipses. *)
maxR = 1.55 Max[
  Max[Abs @ Flatten[enPts]],
  Max[Abs @ Flatten[crlbEllPts]],
  Max[Abs @ Flatten[mcEllPts]]
];

(* ── Colour palette ─────────────────────────────────────────────────────────── *)
colScatter  = RGBColor[0.53, 0.81, 0.98, 0.35];   (* light blue, semi-transparent *)
colNavy     = RGBColor[0.13, 0.24, 0.56];          (* solid navy — empirical *)
colOrange   = RGBColor[1.0,  0.55, 0.0 ];          (* orange     — CRLB *)
colGray     = GrayLevel[0.55];

(* ── Annotation helpers ──────────────────────────────────────────────────────── *)
(* Convert relative plot position (rx, ry) in [0,1]^2 to plot coordinates *)
pCoord[rx_, ry_] := {maxR (2 rx - 1), maxR (2 ry - 1)};

(* ── Legend (bottom-left, inside the empty lower strip, white-backed) ─────────── *)
legX0 = 0.035; legDX = 0.05;  (* left edge and line-length of legend indicators *)
legY  = {0.15, 0.095, 0.04};   (* three rows, all below the circle (~rel 0.18) *)
legendItems = {
  (* CRLB ellipse: dashed orange line *)
  {colOrange, Dashing[{0.02, 0.01}], Thickness[0.004],
    Line[{pCoord[legX0, legY[[1]]], pCoord[legX0 + legDX, legY[[1]]]}]},
  {Black, Text[Style["CRLB 95% ellipse  (Cram\[EAcute]r\[Dash]Rao bound)",
      FontSize -> 26, FontFamily -> "Helvetica", Background -> White],
    pCoord[legX0 + legDX + 0.012, legY[[1]]], {-1, 0}]},
  (* Empirical ellipse: solid navy line *)
  {colNavy, Thickness[0.004],
    Line[{pCoord[legX0, legY[[2]]], pCoord[legX0 + legDX, legY[[2]]]}]},
  {Black, Text[Style["Empirical 95% ellipse  (Monte-Carlo)",
      FontSize -> 26, FontFamily -> "Helvetica", Background -> White],
    pCoord[legX0 + legDX + 0.012, legY[[2]]], {-1, 0}]},
  (* MC scatter: filled dot *)
  {RGBColor[0.35, 0.70, 0.92], PointSize[0.018],
    Point[pCoord[legX0 + legDX/2, legY[[3]]]]},
  {Black, Text[Style["MC fixes (n = 3000)",
      FontSize -> 26, FontFamily -> "Helvetica", Background -> White],
    pCoord[legX0 + legDX + 0.012, legY[[3]]], {-1, 0}]}
};

(* ── Stats box (top-left, in the empty upper strip above the circle) ──────────── *)
azStr    = StringRiffle[ToString[Round[#, 0.1]] & /@ azimuths, "\[Degree], "] <> "\[Degree]";
(* Use Round[x, 0.01] → ToString to get a readable decimal without invoking NumberForm in a string context *)
fmtNm[x_] := ToString[N[Round[x, 0.01]]];
statsStr = "\[Sigma] = 1.5\[CloseCurlyQuote]   |   Azimuths: " <> azStr <>
           "\nCRLB CEP = " <> fmtNm[crlbEll["cep"]] <> " nm,   MC CEP = " <>
             fmtNm[mcResult["cep"]] <> " nm    (95% \[Rule] \[Chi]^2(2) = 5.991)" <>
           "\nRandom-noise floor; assumes perfect ephemeris \[LongDash] real fixes dominated by systematics";
statsAnnot = {Black,
  Text[Style[statsStr, FontSize -> 26, FontFamily -> "Helvetica", Background -> White],
    pCoord[0.02, 0.915], {-1, 1}]};

(* ── Title ───────────────────────────────────────────────────────────────────── *)
titleAnnot = {Black,
  Text[Style[
    "Fisher Information and Cram\[EAcute]r\[Dash]Rao Bound  \[LongDash]  " <>
    "Three-Sight Sun Fix  \[LongDash]  20\[Degree]N, 40\[Degree]W",
    FontSize -> 34, FontWeight -> "Bold", FontFamily -> "Helvetica", Background -> White],
  pCoord[0.5, 0.975]]};

(* ── True-position marker label ──────────────────────────────────────────────── *)
tpLabel = {Black,
  Text[Style["True position", FontSize -> 26, FontFamily -> "Helvetica"],
    {0.12 maxR, -0.08 maxR}, {-1, 0}]};

(* ── Assemble Graphics ───────────────────────────────────────────────────────── *)
fig = Graphics[
  Flatten[{
    (* Grid *)
    {GrayLevel[0.88], Thin,
      Line /@ Table[{{x, -maxR}, {x, maxR}}, {x, -maxR, maxR, maxR/4}],
      Line /@ Table[{{-maxR, y}, {maxR, y}}, {y, -maxR, maxR, maxR/4}]},
    (* Axis crosshairs *)
    {colGray, Dashing[{0.010, 0.006}], Thickness[0.0015],
      Line[{{-maxR, 0}, {maxR, 0}}],
      Line[{{0, -maxR}, {0, maxR}}]},
    (* MC scatter *)
    {colScatter, PointSize[0.004], Point[enPts]},
    (* CRLB 95% ellipse — dashed orange *)
    {colOrange, Dashing[{0.020, 0.010}], Thickness[0.004],
      Line[Append[crlbEllPts, crlbEllPts[[1]]]]},
    (* Empirical 95% ellipse — solid navy *)
    {colNavy, Thickness[0.004],
      Line[Append[mcEllPts, mcEllPts[[1]]]]},
    (* True-position marker *)
    {Black, PointSize[0.015], Point[{0., 0.}]},
    tpLabel,
    (* Annotations — Text[] throughout (no Inset[]) *)
    legendItems,
    statsAnnot,
    titleAnnot
  }, Infinity],
  Background  -> White,
  PlotRange   -> {{-maxR, maxR}, {-maxR, maxR}},
  Frame       -> True,
  FrameLabel  -> {{"North error (nm)", None}, {"East error (nm)", None}},
  FrameStyle  -> Directive[Black, 28, FontFamily -> "Helvetica"],
  FrameTicksStyle -> Directive[Black, 24],
  AspectRatio -> 1,
  ImageSize   -> 1600,
  ImagePadding -> {{80, 40}, {80, 40}}
];

(* ── Export ──────────────────────────────────────────────────────────────────── *)
outPath = FileNameJoin[{repo, "docs", "images", "fig_crlb.png"}];
Export[outPath, fig, "PNG", ImageResolution -> 144];

(* ── Report dimensions and file size ────────────────────────────────────────── *)
{w, h} = ImageDimensions[Import[outPath]];
sz     = FileByteCount[outPath];
Print["Output:     ", outPath];
Print["Dimensions: ", w, " x ", h, " pixels"];
Print["File size:  ", Round[sz / 1024], " KB"];
