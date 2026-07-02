#!/usr/bin/env wolframscript
(* star_selection.wls  --  Alt-Az sky chart for GDOP optimal star selection (Item 4)
   Exports: docs/images/fig_star_selection.png
   Offline only (no AstroGraphics, no network). White/light theme. ImageSize -> 1050.
   Text primitives only (no Inset); named WL characters (\[Degree] etc). *)

$CharacterEncoding = "UTF-8";

Get[FileNameJoin[{DirectoryName[$InputFileName], "CelestialNavigation.wl"}]];

(* ---- observation parameters ---- *)
pos  = {15, -40};
t    = DateObject[{2025, 6, 21, 22, 0, 0}, TimeZone -> 0];

Print["Computing star positions..."];
allStarData = cnLoadStars[];
visible     = cnVisibleStars[pos, t, 15];
result      = cnBestStarTriplet[pos, t, 15];
bestStars   = result["stars"];
bestGDOP    = result["gdop"];
bestAz      = result["azimuths"];

Print["Visible stars: ", Length[visible]];
Print["Best triplet:  ", bestStars];
Print["Best GDOP:     ", N[bestGDOP, 6]];
Print["Best azimuths: ", N[bestAz, 5]];

(* ---- clustered reference GDOP ---- *)
clustGDOP = cnFixGDOP[pos, t, {"Kochab", "Alkaid", "Alioth"}];
Print["Cluster GDOP:  ", N[clustGDOP, 5]];

(* ---- polar alt-az coordinate helper ----
   r = (90 - alt) / 90  so zenith = 0, horizon = 1
   {x, y} = r {Sin[Az], Cos[Az]}  so North is up, East is right *)
starXY[name_] := Module[{hcaz},
  hcaz = cnComputedAltitude[pos, cnBodyGPFor[{"Star", name}, t]];
  With[{r = (90. - hcaz[[1]]) / 90., az = hcaz[[2]]},
    {r Sin[az Degree], r Cos[az Degree]}]
];

(* ---- colours (light theme) ---- *)
colStar    = RGBColor[0.25, 0.45, 0.95];   (* blue accent for regular stars *)
colBest    = RGBColor[0.85, 0.50, 0.20];   (* warm orange for best triplet *)
colGrid    = GrayLevel[0.80];
colHorizon = GrayLevel[0.55];
colLabel   = GrayLevel[0.15];
colMinAlt  = RGBColor[0.25, 0.45, 0.95, 0.45];   (* translucent blue for 15 deg ring *)

(* ---- dot size: brighter star -> larger dot (mag scale ~(-0.7 to 2.8)) ---- *)
dotR[mag_] := Max[0.012, 0.038 - 0.010 mag];

(* ---- altitude grid ---- *)
altAngles = {15, 30, 45, 60, 75};
altCircles = Table[
  With[{r = (90. - a) / 90.},
    {colGrid, AbsoluteThickness[0.7], Circle[{0., 0.}, r]}],
  {a, altAngles}];

(* Dashed ring at minimum altitude (15 deg) *)
minAltRing = {colMinAlt, AbsoluteThickness[1.2], Dashing[{0.012, 0.008}],
  Circle[{0., 0.}, (90. - 15.) / 90.]};

(* ---- azimuth grid (every 30 deg) ---- *)
azLines = Table[
  With[{az = a Degree},
    {colGrid, AbsoluteThickness[0.7],
     Line[{{0., 0.}, {Sin[az], Cos[az]}}]}],
  {a, 0, 330, 30}];

(* ---- horizon circle ---- *)
horizonCircle = {colHorizon, AbsoluteThickness[1.5], Circle[{0., 0.}, 1.]};

(* ---- compass labels ---- *)
compassLabels = {
  {colLabel, Text[Style["N", FontFamily -> "Helvetica", FontSize -> 22, Bold],
    {0., 1.10}]},
  {colLabel, Text[Style["E", FontFamily -> "Helvetica", FontSize -> 22, Bold],
    {1.10, 0.}]},
  {colLabel, Text[Style["S", FontFamily -> "Helvetica", FontSize -> 22, Bold],
    {0., -1.10}]},
  {colLabel, Text[Style["W", FontFamily -> "Helvetica", FontSize -> 22, Bold],
    {-1.10, 0.}]}};

(* ---- altitude labels along the East spoke (Az=90 deg) ---- *)
altLabels = Table[
  With[{r = (90. - a) / 90.},
    {colLabel, Text[Style[ToString[a] <> "\[Degree]",
      FontFamily -> "Helvetica", FontSize -> 16],
      {r + 0.04, 0.04}, {-1, -1}]}],
  {a, {30, 45, 60, 75}}];

(* ---- best-triplet azimuth spokes (full spoke to horizon) ---- *)
spokePrims = Table[
  With[{az = az0 Degree},
    {colBest, AbsoluteThickness[1.3], Dashing[{0.015, 0.010}],
     Line[{{0., 0.}, {Sin[az], Cos[az]}}]}],
  {az0, bestAz}];

(* ---- angular gaps between best-triplet spokes ---- *)
sortedAz = Sort[bestAz];  (* sort ascending *)
gaps = Mod[Differences[Append[sortedAz, sortedAz[[1]] + 360.]], 360.];
gapMidAz = Table[
  Mod[sortedAz[[i]] + gaps[[i]] / 2., 360.],
  {i, 3}];
(* Label the two smaller gaps (the informative ones) *)
gapAnnotations = Table[
  With[{az = gapMidAz[[i]] Degree, g = gaps[[i]]},
    If[g < 200.,  (* skip the large "empty sky" gap *)
      {colBest, Text[Style[ToString[Round[g, 0.1]] <> "\[Degree]",
        FontFamily -> "Helvetica", FontSize -> 17, Bold],
        0.40 {Sin[az], Cos[az]}]},
      Nothing]],
  {i, 3}];

(* ---- star dot primitives ---- *)
starDots = Table[
  Module[{xy = starXY[s], mag = allStarData[s]["mag"], r, inBest},
    inBest = MemberQ[bestStars, s];
    r = dotR[mag];
    If[inBest,
      {colBest, Disk[xy, r * 1.3]},
      {colStar,  Disk[xy, r]}]],
  {s, visible}];

(* ---- star labels ---- *)
(* Always label best-triplet stars; also label mag < 1.5 bright stars *)
labelStars = Union[bestStars,
  Select[visible, allStarData[#]["mag"] < 1.5 &]];

starLabels = Table[
  Module[{xy = starXY[s], inBest = MemberQ[bestStars, s]},
    With[{col = If[inBest, colBest, colLabel],
          dir = Normalize[xy + {0.001, 0.}]},
      {col, Text[Style[s, FontFamily -> "Helvetica", FontSize -> 18,
        If[inBest, Bold, Plain]],
        xy + 0.07 dir, {-1, 0}]}]],
  {s, labelStars}];

(* ---- title and annotation (Text, not Inset) ---- *)
titleLine1 = Text[
  Style["Alt-Az Sky Chart \[LongDash] GDOP Optimal Star Selection",
    FontFamily -> "Helvetica", FontSize -> 24, Bold, GrayLevel[0.1]],
  {0., 1.26}];

titleLine2 = Text[
  Style["Pos: 15\[Degree]N 40\[Degree]W  \[LongDash]  2025-Jun-21 22:00 UTC  \[LongDash]  19 stars above 15\[Degree]",
    FontFamily -> "Helvetica", FontSize -> 17, GrayLevel[0.25]],
  {0., 1.185}];

bestAnnotation = Text[
  Style["Best triplet: " <> StringRiffle[bestStars, ", "] <>
    "   GDOP = " <> ToString[NumberForm[N[bestGDOP, 3], {3, 3}]] <>
    "  (vs cluster GDOP = " <> ToString[NumberForm[N[clustGDOP, 3], {3, 2}]] <> ")",
    FontFamily -> "Helvetica", FontSize -> 19, Bold, colBest],
  {0., -1.19}];

theoryAnnotation = Text[
  Style["Theoretical minimum (3 bodies, 120\[Degree] apart): GDOP = \[Sqrt](4/3) \[TildeEqual] 1.155",
    FontFamily -> "Helvetica", FontSize -> 17, Italic, GrayLevel[0.35]],
  {0., -1.265}];

(* ---- assemble Graphics ---- *)
gfx = Graphics[
  {
    (* background *)
    {White, Rectangle[{-1.4, -1.4}, {1.4, 1.4}]},
    (* grid *)
    azLines,
    altCircles,
    minAltRing,
    horizonCircle,
    (* best-triplet spokes *)
    spokePrims,
    (* star dots *)
    starDots,
    (* gap angle annotations *)
    gapAnnotations,
    (* star labels *)
    starLabels,
    (* compass and altitude labels *)
    compassLabels,
    altLabels,
    (* titles / annotations *)
    titleLine1, titleLine2,
    bestAnnotation, theoryAnnotation
  },
  PlotRange    -> {{-1.40, 1.40}, {-1.35, 1.35}},
  ImageSize    -> 1050,
  Background   -> White,
  ImagePadding -> {{30, 30}, {50, 50}}
];

(* ---- export ---- *)
outDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "docs", "images"}];
If[!DirectoryQ[outDir], CreateDirectory[outDir]];
outPath = FileNameJoin[{outDir, "fig_star_selection.png"}];

Export[outPath, gfx, "PNG", ImageResolution -> 150];
Print["Exported: ", outPath];

(* ---- verify ---- *)
img  = Import[outPath];
dims = ImageDimensions[img];
sz   = FileByteCount[outPath];
Print["Image dimensions: ", dims, " pixels"];
Print["File size: ", sz, " bytes  (", Round[sz / 1024.], " KB)"];

(* Probe corners and centre for white background *)
pv1 = PixelValue[img, {8, 8}];
pv2 = PixelValue[img, {dims[[1]] - 8, dims[[2]] - 8}];
pv3 = PixelValue[img, {Round[dims[[1]] / 2], Round[dims[[2]] / 2]}];
Print["PixelValue top-left corner:     ", N[pv1, 3]];
Print["PixelValue bottom-right corner: ", N[pv2, 3]];
Print["PixelValue image centre:        ", N[pv3, 3]];
If[Min[pv1[[{1,2,3}]]] > 0.90 && Min[pv2[[{1,2,3}]]] > 0.90,
  Print["Background check OK: corners are white"],
  Print["WARNING: corner pixels are not white -- check rendering"]];
Print["Done."];
