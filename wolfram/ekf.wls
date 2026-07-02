#!/usr/bin/env wolframscript
(* ekf.wls -- EKF running fix + systematic-vs-random error demonstration figure *)
(* Run: /usr/local/bin/wolframscript -file wolfram/ekf.wls                       *)
(* R2 feature.  Offline, LIGHT theme. -> docs/images/fig_ekf.png                 *)

$CharacterEncoding = "UTF-8";
SetDirectory[DirectoryName[$InputFileName]];
Get["CelestialNavigation.wl"];

If[!DirectoryQ["../docs/images"], CreateDirectory["../docs/images"]];
outPath = FileNameJoin[{"../docs/images", "fig_ekf.png"}];

(* -- Colour palette (light theme, matching figures.wls / cockedhat.wls) -- *)
blue   = RGBColor[0.25, 0.45, 0.95];
green  = RGBColor[0.20, 0.68, 0.35];
red    = RGBColor[0.82, 0.20, 0.20];
navy   = RGBColor[0.13, 0.29, 0.53];
amber  = RGBColor[0.90, 0.62, 0.10];
cyan   = RGBColor[0.10, 0.65, 0.78];
mid    = GrayLevel[0.45];
gridC  = Directive[GrayLevel[0.88], Dashed];
frameC = GrayLevel[0.85];
bg     = White;
textC  = RGBColor[0.13, 0.29, 0.53];
titleDir = Directive[FontFamily -> "Helvetica", FontSize -> 28, Bold, FontColor -> textC];
labelDir = Directive[FontFamily -> "Helvetica", FontSize -> 26, FontColor -> textC];
convDir  = Directive[FontFamily -> "Helvetica", FontSize -> 19, FontColor -> textC];

toImg[expr_, w_] := ImageResize[Rasterize[expr, Background -> bg], w];
(* Rasterise on a canvas as wide as the final target so long titles never clip. *)
txtImg[txt_, w_, h_, fsize_, bold_ : False] := ImageResize[
  Rasterize[
    Graphics[{Text[Style[txt, Directive[FontFamily -> "Helvetica", FontSize -> fsize,
                              If[bold, Bold, Plain], FontColor -> textC]],
                   Scaled[{0.5, 0.5}]]},
      Background -> bg, ImageSize -> {w, h}], Background -> bg], w];

(* ============================================================ *)
(*  Build voyage and run the EKF                                *)
(* ============================================================ *)
rows = Import["../data/voyage.csv", "CSV"];
hdr  = rows[[1]]; data = rows[[2 ;;]];
col[name_] := Position[hdr, name][[1, 1]];
mkDate[v_] := If[Head[v] === DateObject, v,
                 DateObject[StringReplace[v, "Z" -> ""], TimeZone -> 0]];
voyage = Table[
  <|"day" -> r[[col["day"]]],
    "t" -> mkDate[r[[col["datetimeUTC"]]]],
    "truePos" -> {r[[col["latTrue"]]], r[[col["lonTrue"]]]},
    "drPos"   -> {r[[col["latDR"]]],   r[[col["lonDR"]]]}|>,
  {r, data}];

SeedRandom[42];
recs = cnEKFVoyage[voyage, 3, <|"seed" -> 42, "sigmaMin" -> 1.0|>];

truthTrack = #["truePos"] & /@ recs;
drTrack    = #["drPos"]   & /@ recs;
ekfTrack   = #["estPos"]  & /@ recs;
ekfErrs    = #["errVsTruthNm"] & /@ recs;
drErrs     = #["drErrorNm"]    & /@ recs;
ekfRMS     = Sqrt[Mean[ekfErrs^2]];
drRMS      = Sqrt[Mean[drErrs^2]];

(* running-fix dots from the stored sights *)
sj      = Import["../data/sights.json", "RawJSON"];
rfTrack = #["runningFix"] & /@ sj["days"];
rfErrs  = #["runningFixErrorNm"] & /@ sj["days"];
rfRMS   = Sqrt[Mean[rfErrs^2]];

Print["EKF RMS = ", NumberForm[ekfRMS, {4, 2}], " nm | DR RMS = ",
      NumberForm[drRMS, {4, 1}], " nm | running-fix RMS = ",
      NumberForm[rfRMS, {4, 2}], " nm"];

(* plot in (lon, lat); ellipses scaled for visibility on the wide map *)
ptLL[{lat_, lon_}] := {lon, lat};                  (* x = lon, y = lat *)
ellScale = 6.0;                                     (* exaggeration factor *)
(* ellipse polygon for covariance P (nm^2) centred at {lat,lon} *)
covEllipse[{lat_, lon_}, P_] := Module[{e, a, b, th, pts},
  e = cnErrorEllipse[P];
  a = ellScale e["semiMajorNm"]; b = ellScale e["semiMinorNm"];
  th = e["orientDeg"];                              (* bearing from N of major axis *)
  pts = Table[
    Module[{ce = a Cos[u], se = b Sin[u], dE, dN},
      (* major axis along bearing th: {E,N} = ce*{Sin th,Cos th} + se*{Cos th,-Sin th} *)
      dE = ce Sin[th Degree] + se Cos[th Degree];
      dN = ce Cos[th Degree] - se Sin[th Degree];
      {lon + dE/(60 Cos[lat Degree]), lat + dN/60}],
    {u, 0, 2 Pi, 2 Pi/60}];
  pts];

(* PRIOR (initial) covariance ellipse — the large uncertainty before any sight *)
priorP    = 20.0^2 IdentityMatrix[2];               (* initialCovNm default *)
priorPoly = covEllipse[recs[[1]]["estPos"], priorP];
(* a few POSTERIOR ellipses along the track (steady, small — shown as the prior collapses) *)
ellipseDays  = {1, 6, 12, 18, 24};
ellipsePolys = Table[covEllipse[recs[[d]]["estPos"], recs[[d]]["covNm"]], {d, ellipseDays}];

(* covariance-trace convergence: predict (inflated) vs posterior (shrunk) *)
covTr     = Tr[#["covNm"]] & /@ recs;
covPredTr = Tr[#["covPredictNm"]] & /@ recs;
days      = #["day"] & /@ recs;
convPlot = ListLogPlot[
  {Transpose[{days, covPredTr}], Transpose[{days, covTr}]},
  Joined -> True, PlotStyle -> {Directive[red, AbsoluteThickness[2]], Directive[navy, AbsoluteThickness[2]]},
  PlotMarkers -> {{"\[FilledSmallCircle]", 7}, {"\[FilledSmallCircle]", 7}},
  Frame -> True, FrameStyle -> Directive[frameC, AbsoluteThickness[1]],
  FrameLabel -> {Style["day", convDir], Style["tr P  (nm\.b2)", convDir]},
  GridLines -> Automatic, GridLinesStyle -> gridC,
  PlotRange -> {{-0.5, 23.5}, {0.5, 1200}},
  Background -> Directive[White, Opacity[0.95]],
  PlotLabel -> Style["cov: prior \[Rule] posterior", convDir],
  ImageSize -> {300, 210},
  Epilog -> {
    Text[Style["predict (+Q)", Directive[FontFamily -> "Helvetica", FontSize -> 17, FontColor -> red]],
         Scaled[{0.97, 0.74}], {Right, Center}],
    Text[Style["posterior", Directive[FontFamily -> "Helvetica", FontSize -> 17, FontColor -> navy]],
         Scaled[{0.97, 0.18}], {Right, Center}]}
];

lonMin = Min[drTrack[[All, 2]]] - 1.2;
lonMax = Max[drTrack[[All, 2]]] + 1.2;
latMin = Min[drTrack[[All, 1]]] - 1.2;
latMax = Max[drTrack[[All, 1]]] + 1.2;

panelA = Graphics[{
    (* PRIOR covariance ellipse (large, before any sight) *)
    {FaceForm[Opacity[0.10, blue]], EdgeForm[Directive[blue, Opacity[0.4], AbsoluteThickness[1.5], Dashing[{4, 4}]]],
     Polygon[priorPoly]},
    (* posterior covariance ellipses along the track (drawn behind tracks) *)
    {FaceForm[Opacity[0.30, blue]], EdgeForm[Directive[blue, Opacity[0.7], AbsoluteThickness[1]]],
     Polygon /@ ellipsePolys},
    (* DR track *)
    {amber, AbsoluteThickness[2.5], Line[ptLL /@ drTrack]},
    {amber, AbsolutePointSize[5], Point[ptLL /@ drTrack]},
    (* truth track *)
    {green, AbsoluteThickness[2.5], Line[ptLL /@ truthTrack]},
    (* running-fix dots *)
    {cyan, AbsolutePointSize[7], Point[ptLL /@ rfTrack]},
    (* EKF estimate *)
    {navy, AbsoluteThickness[3], Line[ptLL /@ ekfTrack]},
    {navy, AbsolutePointSize[6], Point[ptLL /@ ekfTrack]},
    (* start / end markers *)
    {Black, AbsolutePointSize[9], Point[ptLL[truthTrack[[1]]]]},
    Text[Style["start", Directive[FontFamily -> "Helvetica", FontSize -> 20, FontColor -> mid]],
         ptLL[truthTrack[[1]]] + {0.4, 0.5}, {Left, Bottom}],
    Text[Style["end", Directive[FontFamily -> "Helvetica", FontSize -> 20, FontColor -> mid]],
         ptLL[truthTrack[[-1]]] + {-0.4, -0.6}, {Right, Top}]
  },
  Frame -> True,
  FrameLabel -> {Style["Longitude (\[Degree]E)", labelDir], Style["Latitude (\[Degree]N)", labelDir]},
  FrameStyle -> Directive[frameC, AbsoluteThickness[1]],
  GridLines -> Automatic, GridLinesStyle -> gridC,
  PlotRange -> {{lonMin, lonMax}, {latMin, latMax}},
  AspectRatio -> (latMax - latMin)/((lonMax - lonMin) Cos[Mean[{latMin, latMax}] Degree]),
  Background -> bg,
  ImageSize -> {780, 760},
  ImagePadding -> {{70, 20}, {55, 30}},
  PlotLabel -> Style["(a)  EKF running fix vs DR \[LongDash] the filter hugs truth", titleDir],
  Epilog -> {
    (* covariance-convergence inset (top-left, away from the track) *)
    Inset[convPlot, Scaled[{0.035, 0.965}], {Left, Top}, Scaled[0.45]],
    (* legend *)
    Inset[Framed[
      Grid[{
        {Graphics[{green, AbsoluteThickness[3], Line[{{0, 0}, {1, 0}}]}, ImageSize -> 26],
         Style["truth", labelDir]},
        {Graphics[{amber, AbsoluteThickness[3], Line[{{0, 0}, {1, 0}}]}, ImageSize -> 26],
         Style["DR (RMS " <> ToString[NumberForm[drRMS, {4, 1}]] <> " nm)", labelDir]},
        {Graphics[{cyan, AbsolutePointSize[8], Point[{0.5, 0}]}, ImageSize -> 26],
         Style["daily running fix (RMS " <> ToString[NumberForm[rfRMS, {3, 2}]] <> " nm)", labelDir]},
        {Graphics[{navy, AbsoluteThickness[3], Line[{{0, 0}, {1, 0}}]}, ImageSize -> 26],
         Style["EKF (RMS " <> ToString[NumberForm[ekfRMS, {3, 2}]] <> " nm)", labelDir]},
        {Graphics[{FaceForm[Opacity[0.3, blue]], EdgeForm[blue], Disk[{0.5, 0}, {0.5, 0.32}]}, ImageSize -> 26],
         Style["EKF cov ellipse (\[Times]" <> ToString[ellScale] <> "; dashed = prior)", labelDir]}
       }, Alignment -> {Left, Center}, Spacings -> {0.6, 0.5}],
      Background -> Directive[White, Opacity[0.92]], FrameStyle -> frameC, RoundingRadius -> 4],
      Scaled[{0.985, 0.022}], {Right, Bottom}]
  }
];

(* ============================================================ *)
(*  Panel (b): systematic vs random scatter                     *)
(* ============================================================ *)
truth  = {20.0, -40.0};
times  = {DateObject[{2024, 11, 22, 10, 0, 0}, TimeZone -> 0],
          DateObject[{2024, 11, 22, 13, 0, 0}, TimeZone -> 0],
          DateObject[{2024, 11, 22, 16, 0, 0}, TimeZone -> 0]};
bodies = {"Sun", "Sun", "Sun"};
nMC    = 600;
ieMin  = 3.0;

SeedRandom[2024]; sys0 = cnSystematicFixError[truth, times, bodies, 1.5, 0.0,   nMC];
SeedRandom[2024]; sys3 = cnSystematicFixError[truth, times, bodies, 1.5, ieMin, nMC];

toXY[{lat_, lon_}] := {(lon - truth[[2]]) Cos[truth[[1]] Degree] 60, (lat - truth[[1]]) 60};
cloud0 = toXY /@ sys0["fixes"];
cloud3 = toXY /@ sys3["fixes"];
bias3  = sys3["biasVecNm"];

Print["Systematic: IE=", ieMin, "' -> bias ", NumberForm[sys3["biasNm"], {3, 2}],
      " nm; scatter trace random=", NumberForm[sys0["scatterTraceNm2"], {3, 2}],
      " systematic=", NumberForm[sys3["scatterTraceNm2"], {3, 2}], " nm^2"];

(* representative cocked hat for the systematic case (seeded once, mean-ish trial) *)
SeedRandom[99];
repLops = MapThread[Function[{body, ti},
  cnReduceSightBody[
    cnGenerateSightBody[truth, ti, body, <|"sigmaMin" -> 1.5, "indexErrorMin" -> ieMin|>],
    ti, truth, body, <|"indexErrorMin" -> 0.0|>]], {bodies, times}];
repHat   = cnCockedHat[repLops];
hatVerts = toXY /@ repHat["vertices"];

rng = 8.5;
panelB = Graphics[{
    (* random-only cloud *)
    {Opacity[0.5, green], AbsolutePointSize[3.2], Point[cloud0]},
    (* systematic cloud *)
    {Opacity[0.5, red], AbsolutePointSize[3.2], Point[cloud3]},
    (* representative cocked hat (systematic) *)
    {Directive[red, AbsoluteThickness[2.2], Dashing[{6, 5}]],
     Line[{hatVerts[[1]], hatVerts[[2]], hatVerts[[3]], hatVerts[[1]]}]},
    {GrayLevel[0.55], AbsolutePointSize[5], Point /@ hatVerts},
    (* truth marker *)
    {green, AbsolutePointSize[13], Point[{0, 0}]},
    {White, AbsolutePointSize[5], Point[{0, 0}]},
    (* systematic mean marker + bias arrow *)
    {navy, AbsoluteThickness[2.5], Arrowheads[0.04], Arrow[{{0, 0}, bias3}]},
    {red, AbsolutePointSize[13], Point[bias3]},
    {White, AbsolutePointSize[5], Point[bias3]}
  },
  Frame -> True,
  FrameLabel -> {Style["East offset from truth (nm)", labelDir],
                 Style["North offset from truth (nm)", labelDir]},
  FrameStyle -> Directive[frameC, AbsoluteThickness[1]],
  GridLines -> {Range[-8, 8, 2], Range[-8, 8, 2]}, GridLinesStyle -> gridC,
  PlotRange -> {{-rng, rng}, {-rng, rng}},
  AspectRatio -> 1,
  Background -> bg,
  ImageSize -> {780, 760},
  ImagePadding -> {{60, 20}, {55, 30}},
  PlotLabel -> Style["(b)  Systematic vs random error \[LongDash] bias \[NotEqual] scatter", titleDir],
  Epilog -> {
    Text[Style["truth", Directive[FontFamily -> "Helvetica", FontSize -> 21, Bold, FontColor -> green]],
         {0.4, 0.5}, {Left, Bottom}],
    Text[Style["random only:\nunbiased,\nspread \[Sqrt]tr " <> ToString[NumberForm[Sqrt[sys0["scatterTraceNm2"]], {3, 2}]] <> " nm",
               Directive[FontFamily -> "Helvetica", FontSize -> 21, FontColor -> green]],
         {-rng + 0.4, rng - 0.4}, {Left, Top}],
    Text[Style["random + 3\[Prime] index error:\nshifted " <> ToString[NumberForm[sys3["biasNm"], {3, 2}]] <> " nm,\nsame spread " <> ToString[NumberForm[Sqrt[sys3["scatterTraceNm2"]], {3, 2}]] <> " nm",
               Directive[FontFamily -> "Helvetica", FontSize -> 21, FontColor -> red]],
         {rng - 0.4, -rng + 0.4}, {Right, Bottom}],
    Text[Style["cocked hat unchanged:\ndoes NOT reveal the bias",
               Directive[FontFamily -> "Helvetica", FontSize -> 18, Italic, FontColor -> mid]],
         {rng - 0.4, rng - 0.4}, {Right, Top}]
  }
];

(* ============================================================ *)
(*  Assemble                                                    *)
(* ============================================================ *)
tgtW = 1120; gapW = 4;
imgA = toImg[panelA, (tgtW - gapW)/2];
imgB = toImg[panelB, (tgtW - gapW)/2];
hA = ImageDimensions[imgA][[2]]; hB = ImageDimensions[imgB][[2]];
maxH = Max[hA, hB];
imgA = ImagePad[imgA, {{0, 0}, {0, maxH - hA}}, bg];
imgB = ImagePad[imgB, {{0, 0}, {0, maxH - hB}}, bg];
gap  = ConstantImage[bg, {gapW, maxH}];
bodyRow = ImageAssemble[{{imgA, gap, imgB}}, Background -> bg];

mainTitle = txtImg[
  "Recursive Bayesian running fix (EKF)  \[LongDash]  fusing dead reckoning with sights over a voyage",
  tgtW, 46, 21, True];
subTitle = txtImg[
  "(a) EKF (navy) fuses DR motion (PREDICT, Q) with each day's Sun sights (UPDATE, R=\[Sigma]\.b2) into one\n" <>
  "estimate with a shrinking covariance \[LongDash] tracking truth far better than uncorrected DR.\n" <>
  "(b) A constant index error biases the fix (shifts the centre) WITHOUT enlarging the scatter or\n" <>
  "the cocked hat: a tight cocked hat is NOT proof of accuracy.",
  tgtW, 124, 17];

bw = ImageDimensions[bodyRow][[1]];
bodyFinal = If[bw === tgtW, bodyRow, ImageResize[bodyRow, tgtW]];
fullImg = ImageAssemble[{{mainTitle}, {subTitle}, {bodyFinal}}, Background -> bg];
fullImg = ImagePad[ImagePad[fullImg, -3], 3, White];

Export[outPath, fullImg, "PNG"];
dims = ImageDimensions[fullImg]; sz = FileByteCount[outPath];
Print["EXPORTED: fig_ekf.png  ", dims[[1]], "\[Times]", dims[[2]], " px  ", Round[sz/1024], " KB"];
Print["Corner pixel (expect {1.,1.,1.}): ", ImageValue[fullImg, {5, 5}]];
