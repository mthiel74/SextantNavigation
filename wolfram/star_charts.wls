#!/usr/bin/env wolframscript
(* star_charts.wls -- Item 18: All-sky navigational star chart (twin-hemisphere, equatorial)
   Exports: docs/images/fig_star_chart.png
   Offline only. Dark-sky aesthetic. ImageSize -> 1800.
   Text[] primitives only (no Inset). Named WL characters (\[Degree] etc.) *)

$CharacterEncoding = "UTF-8";

Get[FileNameJoin[{DirectoryName[$InputFileName], "CelestialNavigation.wl"}]];

(* ── Load star data ──────────────────────────────────────────────────────── *)
stars     = cnLoadStars[];
starNames = Keys[stars];
nStars    = Length[starNames];
Print["Stars loaded: ", nStars];

(* Sort brightest first (lowest magnitude) *)
sortedByMag = SortBy[starNames, stars[#]["mag"] &];
Print["Brightest 5: ",
  StringRiffle[Take[sortedByMag, 5], ", "],
  "  magnitudes: ",
  StringRiffle[ToString /@ (stars[#]["mag"] & /@ Take[sortedByMag, 5]), ", "]];

(* Coverage check: how many unique stars across both hemispheres *)
nNorth  = Length[Select[starNames, stars[#]["decDeg"] >= -30. &]];
nSouth  = Length[Select[starNames, stars[#]["decDeg"] <=  30. &]];
nUnique = Length[Union[
  Select[starNames, stars[#]["decDeg"] >= -30. &],
  Select[starNames, stars[#]["decDeg"] <=  30. &]]];
Print["Stars in N chart: ", nNorth, " | in S chart: ", nSouth,
      " | unique across both: ", nUnique];

(* ── Colour palette (dark-sky theme) ───────────────────────────────────── *)
bgCol    = RGBColor[0.04, 0.05, 0.12];                  (* deep night sky *)
starCol  = RGBColor[1.00, 0.97, 0.88];                  (* warm white *)
glowDir  = Directive[RGBColor[0.92, 0.86, 0.68], Opacity[0.28]]; (* soft glow *)
equCol   = RGBColor[0.40, 0.65, 1.00];                  (* celestial equator blue *)
eclCol   = RGBColor[1.00, 0.82, 0.25];                  (* ecliptic gold *)
gridDir  = Directive[RGBColor[0.22, 0.28, 0.52], Opacity[0.70],
                     AbsoluteThickness[0.6]];            (* grid lines *)
labelCol = RGBColor[0.92, 0.90, 0.78];                  (* cream labels *)
ariesCol = RGBColor[1.00, 0.82, 0.25];                  (* First Point of Aries *)
poleCol  = RGBColor[0.55, 0.75, 1.00];                  (* pole annotation *)
titleCol = RGBColor[0.88, 0.90, 1.00];                  (* heading text *)

(* ── Projection: polar equidistant (pole at centre, equator at r = 1) ──── *)
(* North: NCP at centre. RA=0 at top, RA increases clockwise                 *)
(*        (as seen from outside the sphere looking at the north pole).        *)
(* South: SCP at centre. RA=0 at top, RA increases counterclockwise          *)
(*        (mirrored east-west vs north, as on a globe facing south).          *)

northXY[raDeg_?NumericQ, decDeg_?NumericQ] :=
  With[{r = (90. - decDeg) / 90., th = raDeg * Degree},
    {r * Sin[th], r * Cos[th]}];

southXY[raDeg_?NumericQ, decDeg_?NumericQ] :=
  With[{r = (90. + decDeg) / 90., th = raDeg * Degree},
    {-r * Sin[th], r * Cos[th]}];

(* ── Star dot radius: brighter (lower mag) -> larger ───────────────────── *)
(* Mag range ~ -1.5 to 2.9 across the 58 stars *)
dotR[mag_]  := Max[0.012, 0.046 - 0.012 * mag];
glowR[mag_] := 2.20 * dotR[mag];

(* ── Ecliptic in equatorial coordinates ─────────────────────────────────── *)
(* Standard transformation: RA = atan2(cos(eps)*sin(lam), cos(lam))          *)
(*                          Dec = arcsin(sin(eps)*sin(lam))                   *)
(* eps = obliquity = 23.44 deg.  WL ArcTan[x,y] = atan2(y,x).               *)
eps = 23.44 * Degree;
eclTable = Table[
  Module[{lam = l * Degree, raE, decE},
    raE  = Mod[ArcTan[Cos[lam], Cos[eps] * Sin[lam]], 2 Pi] / Degree;
    decE = ArcSin[Sin[eps] * Sin[lam]] / Degree;
    {raE, decE}],
  {l, 0, 362, 2}];   (* slight overshoot to close the curve *)

eclPtsN = northXY[#[[1]], #[[2]]] & /@ eclTable;
eclPtsS = southXY[#[[1]], #[[2]]] & /@ eclTable;

(* ── Grid: RA lines every 30 deg (= 2 h), Dec circles ──────────────────── *)
raGrid[projFn_] := Table[
  With[{th = a * Degree},
    {gridDir, Line[{{0., 0.}, projFn[a, 0.]}]}],
  {a, 0, 330, 30}];

decGridN = Table[
  {gridDir, Circle[{0., 0.}, (90. - d) / 90.]},
  {d, {0, 30, 60}}];

decGridS = Table[
  {gridDir, Circle[{0., 0.}, (90. + d) / 90.]},
  {d, {0, -30, -60}}];

(* ── RA hour labels at equator (r = 1.09) ──────────────────────────────── *)
raHourLabels[projFn_] := Table[
  With[{pos = 1.09 * projFn[h * 15., 0.]},
    {Directive[RGBColor[0.55, 0.60, 0.78], FontSize -> 9],
     Text[Style[ToString[h] <> "h",
       FontFamily -> "Helvetica", FontSize -> 9,
       FontColor -> RGBColor[0.55, 0.60, 0.78]],
       pos, {0, 0}]}],
  {h, 0, 23, 2}];

(* ── Dec tick labels (along RA=0 spoke, north) ─────────────────────────── *)
decLabelsN = Table[
  With[{r = (90. - d) / 90.},
    Text[Style[ToString[d] <> "\[Degree]",
      FontFamily -> "Helvetica", FontSize -> 8.5,
      FontColor -> RGBColor[0.50, 0.55, 0.75]],
      {r * Sin[5 Degree] + 0.02, r * Cos[5 Degree]}, {-1, 0}]],
  {d, {30, 60}}];

decLabelsS = Table[
  With[{r = (90. + d) / 90.},(* d is negative here *)
    Text[Style[ToString[d] <> "\[Degree]",
      FontFamily -> "Helvetica", FontSize -> 8.5,
      FontColor -> RGBColor[0.50, 0.55, 0.75]],
      {-r * Sin[5 Degree] - 0.02, r * Cos[5 Degree]}, {1, 0}]],
  {d, {-30, -60}}];

(* ── Star primitives (glow + dot) ──────────────────────────────────────── *)
buildStarPrims[projFn_, hemisFilter_] :=
  Flatten[
    Map[Function[s,
      Module[{ra, dec, mag, xy, r, gr},
        ra  = stars[s]["raDeg"];
        dec = stars[s]["decDeg"];
        mag = stars[s]["mag"];
        r   = dotR[mag];
        gr  = glowR[mag];
        xy  = projFn[ra, dec];
        If[hemisFilter[dec],
          {{glowDir, Disk[xy, gr]}, {starCol, Disk[xy, r]}},
          {}]
      ]],
      starNames],
    1];

northStarPrims = buildStarPrims[northXY, (# >= -30.) &];
southStarPrims = buildStarPrims[southXY, (#  <=  30.) &];

(* ── Star labels for bright stars (mag < 1.6) ──────────────────────────── *)
buildStarLabels[projFn_, hemisFilter_] :=
  Flatten[
    Map[Function[s,
      Module[{ra, dec, mag, xy, dir, lpos},
        ra  = stars[s]["raDeg"];
        dec = stars[s]["decDeg"];
        mag = stars[s]["mag"];
        xy  = projFn[ra, dec];
        dir = Normalize[xy + {0.001, 0.001}];
        lpos = xy + 0.075 * dir;
        If[hemisFilter[dec] && mag < 1.6,
          {Text[Style[s,
            FontFamily -> "Helvetica", FontSize -> 9.0, Bold,
            FontColor -> RGBColor[0.88, 0.86, 0.74]],
            lpos, {0, 0}]},
          {}]
      ]],
      starNames],
    1];

northStarLabels = buildStarLabels[northXY, (# >= -30.) &];
southStarLabels = buildStarLabels[southXY, (#  <=  30.) &];

(* ── First Point of Aries (RA=0, Dec=0: on equator of both charts) ─────── *)
(* North: northXY[0,0] = {0,1} = top.  South: southXY[0,0] = {0,1} = top. *)
ariesMarker[projFn_] := Module[{pos},
  pos = projFn[0., 0.];   (* = {0, 1} in both projections *)
  {
    {ariesCol, AbsoluteThickness[2.0],
     Line[{0.88 * pos, 1.14 * pos}]},
    {ariesCol, AbsolutePointSize[6],
     Point[pos]},
    Text[Style["\[Gamma]",
      FontFamily -> "Helvetica", FontSize -> 13, Bold,
      FontColor -> ariesCol],
      1.22 * pos, {0, -1}]
  }];

(* ── Pole markers ───────────────────────────────────────────────────────── *)
polePrim[label_] := {
  {poleCol, Disk[{0., 0.}, 0.015]},
  Text[Style[label,
    FontFamily -> "Helvetica", FontSize -> 10, Bold,
    FontColor -> poleCol],
    {0., 0.05}, {0, -1}]
};

(* ── Background disc (slightly larger than equator) ────────────────────── *)
bgDisk = {bgCol, Disk[{0., 0.}, 1.14]};

(* ── Per-hemisphere title and legend text ──────────────────────────────── *)
hemiTitle[lbl_] :=
  Text[Style[lbl,
    FontFamily -> "Helvetica", FontSize -> 13, Bold,
    FontColor -> titleCol],
    {0., 1.33}];

legendPrims[equLbl_, eclLbl_] := {
  Text[Style[equLbl,
    FontFamily -> "Helvetica", FontSize -> 8.5,
    FontColor -> equCol],
    {0., -1.07}, {0, 1}],
  Text[Style[eclLbl,
    FontFamily -> "Helvetica", FontSize -> 8.5,
    FontColor -> eclCol],
    {0., -1.15}, {0, 1}]
};

(* ── Assemble each hemisphere ───────────────────────────────────────────── *)
plotRange = {{-1.28, 1.28}, {-1.24, 1.48}};

northGfx = Graphics[
  {
    bgDisk,
    raGrid[northXY],
    decGridN,
    (* Celestial equator *)
    {equCol, AbsoluteThickness[1.6], Circle[{0., 0.}, 1.0]},
    (* Ecliptic (dashed gold) *)
    {eclCol, AbsoluteThickness[1.3], Dashing[{0.014, 0.009}],
     Line[eclPtsN]},
    (* Stars *)
    northStarPrims,
    (* Labels *)
    northStarLabels,
    (* Aries marker *)
    ariesMarker[northXY],
    (* Pole *)
    polePrim["NCP"],
    (* RA hour labels *)
    raHourLabels[northXY],
    (* Dec tick labels *)
    decLabelsN,
    (* Hemisphere title *)
    hemiTitle["Northern Sky  (Dec \[GreaterEqual] \[Minus]30\[Degree])"],
    legendPrims["Celestial Equator",
                "Ecliptic (\[Epsilon] = 23.44\[Degree])"]
  },
  PlotRange   -> plotRange,
  Background  -> bgCol,
  ImagePadding -> {{8, 8}, {8, 8}}
];

southGfx = Graphics[
  {
    bgDisk,
    raGrid[southXY],
    decGridS,
    {equCol, AbsoluteThickness[1.6], Circle[{0., 0.}, 1.0]},
    {eclCol, AbsoluteThickness[1.3], Dashing[{0.014, 0.009}],
     Line[eclPtsS]},
    southStarPrims,
    southStarLabels,
    ariesMarker[southXY],
    polePrim["SCP"],
    raHourLabels[southXY],
    decLabelsS,
    hemiTitle["Southern Sky  (Dec \[LessEqual] 30\[Degree])"],
    legendPrims["Celestial Equator",
                "Ecliptic (\[Epsilon] = 23.44\[Degree])"]
  },
  PlotRange   -> plotRange,
  Background  -> bgCol,
  ImagePadding -> {{8, 8}, {8, 8}}
];

combined = GraphicsRow[{northGfx, southGfx},
  Spacings  -> 6,
  Background -> bgCol,
  ImageSize  -> 1800];

(* ── Main title and caption ──────────────────────────────────────────────── *)
mainTitle = Style[
  "All-Sky Navigational Star Chart \[LongDash] 58 Stars \[LongDash] J2000.0 Catalogue",
  Directive[FontFamily -> "Helvetica", FontSize -> 24, Bold,
            FontColor -> titleCol]];

caption = Style[
  "Polar equidistant projection \[LongDash] pole at centre, equator at rim. " <>
  "Star sizes \[Proportional] brightness. " <>
  "Both hemispheres overlap between Dec = \[Minus]30\[Degree] and Dec = +30\[Degree]. " <>
  "Positions are J2000.0 equatorial coordinates; the navigation engine precesses to date.",
  Directive[FontFamily -> "Helvetica", FontSize -> 12,
            FontColor -> RGBColor[0.68, 0.70, 0.86]]];

finalFig = Panel[
  Column[{mainTitle, combined, caption},
    Alignment -> Center,
    Spacings  -> {0, {0.5, 0.4}}],
  Background -> bgCol,
  FrameMargins -> {{20, 20}, {16, 16}}];

(* ── Export ──────────────────────────────────────────────────────────────── *)
outDir  = FileNameJoin[{DirectoryName[$InputFileName], "..", "docs", "images"}];
If[!DirectoryQ[outDir], CreateDirectory[outDir]];
outPath = FileNameJoin[{outDir, "fig_star_chart.png"}];

Module[{img, dims, sz, bgPx, midPx},
  img  = Rasterize[finalFig, Background -> bgCol, ImageResolution -> 144];
  Export[outPath, img, "PNG"];
  dims = ImageDimensions[img];
  sz   = FileByteCount[outPath];
  Print["EXPORTED: ", outPath];
  Print["Dimensions: ", dims[[1]], " \[Times] ", dims[[2]], " px"];
  Print["File size:  ", Round[sz / 1024.], " KB  (",
        ToString[NumberForm[N[sz / 1024. / 1024., 3], {3, 2}]], " MB)"];
  (* Probe: corner should be dark; sample near a bright-star region *)
  bgPx  = PixelValue[img, {25, 25}];
  midPx = PixelValue[img, {Round[dims[[1]] * 0.25], Round[dims[[2]] * 0.50]}];
  Print["Corner pixel (expect dark): ", N[bgPx, 3]];
  Print["Interior sample:            ", N[midPx, 3]];
  If[Max[bgPx[[{1,2,3}]]] < 0.25,
    Print["Background check OK: corner is dark (max channel = ",
          N[Max[bgPx[[{1,2,3}]]], 3], ")"],
    Print["WARNING: corner is not dark -- check background rendering"]];
];

Print["Done."];
