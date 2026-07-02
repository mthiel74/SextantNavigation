#!/usr/bin/env wolframscript
(* error_budget.wls  ─  Item 13: Total error-budget dashboard
   Position-fix error decomposition for celestial navigation.
   Output: docs/images/fig_error_budget.png
*)

$CharacterEncoding = "UTF-8";

repo    = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repo, "wolfram", "CelestialNavigation.wl"}]];
outPath = FileNameJoin[{repo, "docs", "images", "fig_error_budget.png"}];

(* ── Colour palette (light theme) ─────────────────────────────────────────── *)
cBlue  = RGBColor[0.25, 0.45, 0.95];
cNavy  = RGBColor[0.13, 0.29, 0.53];
cWarm  = RGBColor[0.85, 0.50, 0.20];
cRed   = RGBColor[0.82, 0.20, 0.20];
cGreen = RGBColor[0.20, 0.68, 0.35];
cGray  = GrayLevel[0.55];

(* ── Export helper ─────────────────────────────────────────────────────────── *)
exportFig[path_, fig_] := Module[{img, dims, sz, pix},
  img  = Rasterize[fig, Background -> White];
  img  = ImagePad[ImagePad[img, -3], 3, White];
  Export[path, img, "PNG"];
  dims = ImageDimensions[img];
  sz   = FileByteCount[path];
  pix  = ImageValue[img, {5, 5}];
  Print["EXPORTED: ", FileNameTake[path], "  ",
        dims[[1]], "\[Times]", dims[[2]], " px  ", Round[sz / 1024], " KB"];
  Print["White-bg probe (corner {5,5}): ", pix];
  {dims, sz}
];

(* ══════════════════════════════════════════════════════════════════════════════
   SECTION 1  ─  Numeric error budget
   ══════════════════════════════════════════════════════════════════════════════
   Convention: 1 arcminute of altitude error \[TildeEqual] 1 nautical mile of LOP error.
   GDOP amplifies sextant noise: \[Sigma]_pos = \[Sigma]_sight \[Times] GDOP (nm).
   Clock error propagates east-west: cnLongitudeErrorPerSecond[\[CapitalDelta]t].
   Refraction / dip uncertainties enter as single-LOP systematic shifts.
*)

lat      = 15.0;
nmPerSec = cnLongitudeErrorPerSecond[lat];

(* Refraction: half-range of P/T swing + Bennett-Saem\[UDoubleDot]ndsson model spread *)
ptHalfNm[h_] := (cnRefractionPT[h, 1040, -10] - cnRefractionPT[h, 980, 30]) / 2.0;
modelNm[h_]  := Abs[cnRefraction[h] - cnRefractionSaemundsson[h]];
refrNm[h_]   := Sqrt[ptHalfNm[h]^2 + modelNm[h]^2];

(* Dip: half-range change in cnDip for ±dh metres *)
dipNm[h_, dh_] := (cnDip[h + dh] - cnDip[Max[0.05, h - dh]]) / 2.0;

rss[vals_] := Sqrt[Total[vals^2]];

srcKeys = {"Sextant", "Chronometer", "Refraction", "Dip"};

calcSc[sigma_, gdop_, clockSec_, altDeg_, eyeH_, dEye_] :=
  {sigma * gdop, nmPerSec * clockSec, refrNm[altDeg], dipNm[eyeH, dEye]};

sc1 = calcSc[0.2, 1.2,  1., 30., 3., 0.5];
sc2 = calcSc[0.5, 1.5,  5., 25., 3., 1.0];
sc3 = calcSc[2.0, 3.0, 30., 10., 3., 2.0];

allSc    = {sc1, sc2, sc3};
scTotals = rss /@ allSc;
gpsNm    = 5.0 / 1852.0;

(* ── Print sanity check ──────────────────────────────────────────────────── *)
Print["=== ITEM 13: ERROR BUDGET ==="];
Print["Lon sensitivity lat=15\[Degree]: ", Round[nmPerSec, 0.0001], " nm/s"];
Print[""];
MapThread[
  Function[{nm, sc, name},
    Print["--- ", name, " ---"];
    MapThread[Print["  ", #1, ": ", Round[#2, 0.001], " nm"] &, {srcKeys, sc}];
    Print["  TOTAL (RSS): ", Round[nm, 0.001], " nm"];
    Print[""]],
  {scTotals, allSc, {"Best case", "Typical", "Worst case"}}];
Print["GPS reference: ", Round[gpsNm, 0.00001], " nm (~5 m)"];
Print["Typical/GPS ratio: ~", Round[scTotals[[2]] / gpsNm, 100], "\[Times]"];

(* ══════════════════════════════════════════════════════════════════════════════
   SECTION 2  ─  PANEL A: Horizontal tornado bar chart
   ══════════════════════════════════════════════════════════════════════════════ *)

baseVals  = sc2;
baseTotal = scTotals[[2]];

ord   = Ordering[baseVals];   (* ascending => largest ends up at index n = top *)
sVals = baseVals[[ord]];
sKeys = srcKeys[[ord]];

srcColor = AssociationThread[srcKeys -> {cBlue, cWarm, cGreen, cNavy}];

srcLabel = <|
  "Sextant"     -> "Sextant  (0.5\[Prime] \[Times] GDOP 1.5)",
  "Chronometer" -> "Chronometer  (5 s, lat 15\[Degree])",
  "Refraction"  -> "Refraction  (P/T \[PlusMinus]20\[Degree]C / 30 hPa, alt 25\[Degree])",
  "Dip"         -> "Dip  (\[PlusMinus]1 m eye-height, h = 3 m)"
|>;

n    = Length[sVals];   (* 4 *)
maxX = Max[sVals] * 1.95;   (* extra right room for the RSS-total label *)
barH = 0.50;

gridXs  = Select[Range[0.0, 2.5, 0.5], # <= maxX &];
labelXs = Select[Range[0.0, 3.0, 1.0], # <= maxX &];   (* label only integers: no squish *)
gridEls = {
  Directive[GrayLevel[0.86], Dashed, AbsoluteThickness[1]],
  Line /@ ({{#, 0.28}, {#, n + 0.56}} & /@ gridXs)
};

barEls = Table[{
  Opacity[0.92], srcColor[sKeys[[i]]],
  Rectangle[{0, i - barH}, {sVals[[i]], i + barH}, RoundingRadius -> 0.04]
}, {i, n}];

(* White-on-bar only for bars wide enough to hold the whole "x.xx nm" label
   without overflowing the bar's left edge (~0.9 nm at this font size); shorter
   bars get dark text placed to the RIGHT so the value stays fully legible.
   (0.75 nm Sextant bar was clipping to "5 nm" with the old 0.62 threshold.) *)
valEls = Table[
  With[{txt = ToString[NumberForm[sVals[[i]], {3, 2}]] <> " nm"},
    If[sVals[[i]] > 0.9,
      Text[Style[txt, Bold, 17, FontFamily -> "Helvetica", FontColor -> White],
        {sVals[[i]] - 0.02, i}, {1, 0}],
      Text[Style[txt, Bold, 17, FontFamily -> "Helvetica",
                 FontColor -> srcColor[sKeys[[i]]]],
        {sVals[[i]] + 0.03, i}, {-1, 0}]]],
  {i, n}];

rssLineEl  = {cRed, Dashed, AbsoluteThickness[2.5],
              Line[{{baseTotal, 0.26}, {baseTotal, n + 0.42}}]};
rssLabelEl = Text[
  Style[
    "RSS: " <> ToString[NumberForm[baseTotal, {4, 2}]] <> " nm",
    Bold, 16, FontFamily -> "Helvetica", FontColor -> cRed, Background -> White],
  {baseTotal + maxX * 0.02, n + 0.28}, {-1, 0}];

yTicks = Table[
  {i, Style[srcLabel[sKeys[[i]]], 15, FontFamily -> "Helvetica",
            FontColor -> GrayLevel[0.20]]},
  {i, n}];
xTicks = Table[
  {x, Style[ToString[NumberForm[x, {2, 1}]] <> " nm", 14, FontColor -> GrayLevel[0.45]]},
  {x, labelXs}];

panelA = Graphics[
  {gridEls, barEls, valEls, rssLineEl, rssLabelEl},
  Frame        -> True,
  FrameStyle   -> Directive[GrayLevel[0.65], AbsoluteThickness[1]],
  FrameTicks   -> {{yTicks, None}, {xTicks, None}},
  PlotRange    -> {{0, maxX}, {0.22, n + 0.78}},
  PlotLabel    -> Style[
    "(a)  Error contributions \[LongDash] typical 3-star fix (GDOP 1.5)\n" <>
    "Systematic terms dominate; MC floor (~1.1 nm) is the random-noise limit",
    17, Bold, FontFamily -> "Helvetica", FontColor -> cNavy],
  Background   -> White,
  ImagePadding -> {{300, 60}, {62, 108}},
  ImageSize    -> {1000, 470}
];

(* ══════════════════════════════════════════════════════════════════════════════
   SECTION 3  ─  PANEL B: Scenario comparison — colour-coded table
   5 rows (4 sources + RSS total) × 3 scenario columns, with mini proportional
   bars inside each cell and a total-strip below.  GPS reference in footer.
   ══════════════════════════════════════════════════════════════════════════════ *)

rowLabels = {
  "Sextant (\[Sigma] \[Times] GDOP)",
  "Chronometer (\[CapitalDelta]t)",
  "Refraction (P/T)",
  "Dip (\[PlusMinus]\[Delta]h)",
  "TOTAL  (RSS)"
};
scHdr = {
  "Best case\n\[Sigma]=0.2\[Prime], GDOP=1.2\n\[CapitalDelta]t=1 s",
  "Typical\n\[Sigma]=0.5\[Prime], GDOP=1.5\n\[CapitalDelta]t=5 s",
  "Worst case\n\[Sigma]=2\[Prime], GDOP=3\n\[CapitalDelta]t=30 s"
};

nR   = 5;    cw = 2.00;  ch = 0.62;
nC   = 3;    lw = 2.40;  hdrH = 1.05;

tblVals = Table[
  If[r <= 4, allSc[[c, r]], scTotals[[c]]],
  {r, nR}, {c, nC}];

colTint = {Lighter[cGreen, 0.82], Lighter[cBlue, 0.88], Lighter[cRed, 0.88]};
colFg   = {Darker[cGreen, 0.30], cNavy, cRed};
maxVal  = Max[tblVals];
warnNm  = 2.0;

(* ── Header: build as separate Tables to avoid Sequence-in-Table issue ──── *)
hdrLblBg  = {FaceForm[GrayLevel[0.93]], EdgeForm[GrayLevel[0.72]],
             Rectangle[{0, nR * ch}, {lw, nR * ch + hdrH}]};
hdrLblTxt = Text[Style["Error source", Bold, 16, FontFamily -> "Helvetica",
                        FontColor -> GrayLevel[0.25]],
                  {lw / 2, nR * ch + hdrH / 2}];

hdrColBgs = Table[
  {FaceForm[Lighter[colTint[[c]], 0.10]], EdgeForm[GrayLevel[0.72]],
   Rectangle[{lw + (c - 1) * cw, nR * ch},
             {lw + c * cw, nR * ch + hdrH}]},
  {c, nC}];

hdrColTxts = Table[
  Text[Style[scHdr[[c]], Bold, 14, FontFamily -> "Helvetica", FontColor -> colFg[[c]]],
       {lw + (c - 0.5) * cw, nR * ch + hdrH / 2}],
  {c, nC}];

hdrElems = Join[{hdrLblBg, hdrLblTxt}, hdrColBgs, hdrColTxts];

(* ── Data cells: build with Do/AppendTo for unambiguous iteration ──────── *)
dataElems = {};
Do[
  yB     = (nR - r) * ch;
  isTot  = (r == nR);
  rowBg  = If[OddQ[r], GrayLevel[0.97], White];

  (* Row-label cell *)
  AppendTo[dataElems,
    {FaceForm[If[isTot, GrayLevel[0.91], rowBg]], EdgeForm[GrayLevel[0.74]],
     Rectangle[{0, yB}, {lw, yB + ch}]}];
  AppendTo[dataElems,
    If[isTot,
       Text[Style[rowLabels[[r]], Bold, 16, FontFamily -> "Helvetica", FontColor -> cNavy],
            {lw / 2, yB + ch / 2}],
       Text[Style[rowLabels[[r]], 15, FontFamily -> "Helvetica",
                  FontColor -> GrayLevel[0.22]],
            {lw / 2, yB + ch / 2}]]];

  (* Data cells, one per scenario *)
  Do[
    v     = tblVals[[r, c]];
    xL    = lw + (c - 1) * cw;
    cellBg = If[isTot, Lighter[colTint[[c]], 0.25], rowBg];
    barW   = (v / maxVal) * (cw - 0.12);

    AppendTo[dataElems,
      {FaceForm[cellBg], EdgeForm[GrayLevel[0.74]],
       Rectangle[{xL, yB}, {xL + cw, yB + ch}]}];
    AppendTo[dataElems,
      {Opacity[0.22], colFg[[c]],
       Rectangle[{xL + 0.04, yB + 0.08},
                 {xL + 0.04 + barW, yB + ch - 0.08},
                 RoundingRadius -> 0.03]}];
    valColor = Which[isTot, colFg[[c]],
                     c == 3 && v >= warnNm, cRed,
                     True, GrayLevel[0.15]];
    AppendTo[dataElems,
      If[isTot,
         Text[Style[ToString[NumberForm[v, {3, 2}]] <> " nm", Bold, 16,
                    FontFamily -> "Helvetica", FontColor -> valColor],
              {xL + cw / 2, yB + ch / 2}],
         Text[Style[ToString[NumberForm[v, {3, 2}]] <> " nm", 15,
                    FontFamily -> "Helvetica", FontColor -> valColor],
              {xL + cw / 2, yB + ch / 2}]]],
    {c, nC}],
  {r, nR}];

(* ── Total-strip bar below table ─────────────────────────────────────────── *)
(* Horizontal bars proportional to scenario total RSS.  GPS reference lives in
   the footer text (it is ~0.003 nm — too small to show as a strip tick without
   colliding with the best-case bar), so no in-strip GPS mark. *)
stripY = -1.40;  stripH = 0.60;
totW   = lw + nC * cw;
maxTot = Max[scTotals];

stripBg  = {FaceForm[GrayLevel[0.95]], EdgeForm[GrayLevel[0.80]],
            Rectangle[{0, stripY - 0.05}, {totW, stripY + stripH + 0.05}]};
stripLbl = Text[Style["Total RSS:", Bold, 15, FontFamily -> "Helvetica",
                       FontColor -> cNavy],
                {lw / 2, stripY + stripH / 2}];

stripBars = Table[
  Module[{xL = lw + (c - 1) * cw, bw = (scTotals[[c]] / maxTot) * (cw - 0.18) * 0.95},
    {Opacity[0.90], colFg[[c]],
     Rectangle[{xL + 0.06, stripY + 0.11},
               {xL + 0.06 + bw, stripY + stripH - 0.11},
               RoundingRadius -> 0.04]}],
  {c, nC}];

(* Value labels: white-on-bar when the bar is wide enough, else dark text to the
   right of a short bar so the tiny best-case value stays legible. *)
stripTxts = Table[
  Module[{xL = lw + (c - 1) * cw, bw = (scTotals[[c]] / maxTot) * (cw - 0.18) * 0.95,
          mid = stripY + stripH / 2, txt},
    txt = ToString[NumberForm[scTotals[[c]], {4, 2}]] <> " nm";
    If[bw > 0.62,
      Text[Style[txt, Bold, 15, FontFamily -> "Helvetica", FontColor -> White],
           {xL + 0.06 + bw - 0.05, mid}, {1, 0}],
      Text[Style[txt, Bold, 15, FontFamily -> "Helvetica", FontColor -> colFg[[c]]],
           {xL + 0.06 + bw + 0.05, mid}, {-1, 0}]]],
  {c, nC}];

(* Footer note — B4 HONESTY: flag MC floor vs systematic budget *)
typRatio = Round[scTotals[[2]] / gpsNm, 100];
footerTxt = Text[
  Style[
    "GPS CEP \[TildeEqual] " <> ToString[NumberForm[gpsNm, {7, 5}]] <>
    " nm (5 m) \[LongDash] a skilled celestial fix is \[TildeEqual] " <>
    ToString[typRatio] <> "\[Times] larger.\n" <>
    "MC floor (~1.1 nm) understates real accuracy; " <>
    "the systematic terms above dominate.",
    Italic, 13, FontFamily -> "Helvetica", FontColor -> cGray],
  {totW / 2, stripY - 0.46}];

panelB = Graphics[
  Join[hdrElems, dataElems, {stripBg, stripLbl}, stripBars, stripTxts, {footerTxt}],
  PlotRange    -> {{-0.02, totW + 0.02}, {stripY - 0.92, nR * ch + hdrH + 0.06}},
  PlotLabel    -> Style[
    "(b)  Fix error by scenario (nm per source, bars proportional to value)",
    17, Bold, FontFamily -> "Helvetica", FontColor -> cNavy],
  Background   -> White,
  ImagePadding -> {{25, 25}, {50, 65}},
  ImageSize    -> {1000, 560}
];

(* ══════════════════════════════════════════════════════════════════════════════
   SECTION 4  ─  Dashboard assembly and export
   ══════════════════════════════════════════════════════════════════════════════ *)

dashboard = GraphicsGrid[
  {{panelA}, {panelB}},
  ImageSize  -> 1000,
  Spacings   -> {0, Scaled[0.01]},
  Background -> White
];

exportFig[outPath, dashboard];
