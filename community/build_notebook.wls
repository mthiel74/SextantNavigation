#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/sextant_navigation.nb \[LongDash] Wolfram Community submission (R2 FINAL).

    Title:  Finding Yourself at Sea: The Mathematics of Celestial Navigation

    Canonical Wolfram Community pattern (cf. the RiverNetwork reference, where
    rnxFigRivers[gb,1600,"Basin"] sits directly above its image):
    ---------------------------------------------------------------------------
      * ALL code lives in the .wl packages (CelestialNavigation.wl + SextantFigures.wl),
        synced into community/ and Get-loaded by the notebook.
      * Above EVERY output \[LongDash] figure or computed result \[LongDash] the notebook shows the
        exact FUNCTION CALL (with all arguments) in a runnable Input cell.
      * Figures: an Input cell  wlIn["sxFigIntercept[]"]  followed by the figure,
        produced by EVALUATING the function during the build and rasterising to ~640 px.
      * Computations: an Input cell  wlIn["cnLOP[{35.,-15.}, ...]"]  followed by the
        pre-computed result, evaluated during the build and embedded beneath.

    Sections
    --------
      Abstract; Setup
      1  The problem of position
      2  The celestial sphere & coordinates
      3  A sight is a circle
      4  The sextant and its corrections
      5  The navigational triangle
      6  Sight reduction \[LongDash] the intercept method
      7  One line is not a fix \[LongDash] running fix & cocked hat
      8  Real stars at twilight
      9  How good is it? The statistics (incl. 9.4 Bayesian posterior)
      10 The longitude problem & lunar distance
      11 Crossing the Atlantic
      12 Recursive estimation & the limits of accuracy
      13 Validation, and a real voyage
      14 Explore it yourself \[LongDash] interactive Manipulates           (NEW R3)
      15 Conclusions
      References; Acknowledgements; Reproducibility
      Visual gallery (3 hero animations)

    R3 additions: SextantInteractive.wl (3 live Manipulates, \[Section]14); the
    Bayesian posterior figure sxFigPosterior[] (\[Section]9.4); and 5 AI-generated
    (gpt-image-2) atmospheric illustrations at section openings.
*)

Print["=== build_notebook.wls start ==="];
t0Build = AbsoluteTime[];

(* ====================================================================
   Paths
   ==================================================================== *)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
dataDir  = FileNameJoin[{repoRoot, "data"}];
outNb    = FileNameJoin[{repoRoot, "community", "sextant_navigation.nb"}];
outPdf   = FileNameJoin[{repoRoot, "community", "sextant_navigation.pdf"}];

(* ====================================================================
   1. Sync BOTH packages into community/ for a self-contained submission
   ==================================================================== *)

srcEngine = FileNameJoin[{repoRoot, "wolfram", "CelestialNavigation.wl"}];
srcFigs   = FileNameJoin[{repoRoot, "wolfram", "SextantFigures.wl"}];
srcInter  = FileNameJoin[{repoRoot, "wolfram", "SextantInteractive.wl"}];
dstEngine = FileNameJoin[{repoRoot, "community", "CelestialNavigation.wl"}];
dstFigs   = FileNameJoin[{repoRoot, "community", "SextantFigures.wl"}];
dstInter  = FileNameJoin[{repoRoot, "community", "SextantInteractive.wl"}];
CopyFile[srcEngine, dstEngine, OverwriteTarget -> True];
CopyFile[srcFigs,   dstFigs,   OverwriteTarget -> True];
CopyFile[srcInter,  dstInter,  OverwriteTarget -> True];
Print["synced: ", dstEngine];
Print["synced: ", dstFigs];
Print["synced: ", dstInter];

(* ====================================================================
   2. Load all three packages (from wolfram/ so relative data paths resolve)
   ==================================================================== *)

Get[srcEngine];
Get[srcFigs];
Get[srcInter];
Print["packages loaded: CelestialNavigation v", cnVersion[], " / ", sxFigVersion[],
   " / ", sxManipVersion[]];

(* ====================================================================
   Helpers \[LongDash] build Cell expressions at script level
   ==================================================================== *)

(* Match the reference Wolfram Community post styling: teal-blue Title AND
   Sections in Roboto Condensed (RGBColor[0.153,0.51,0.64]); a lighter teal for
   Subsections. No red anywhere. *)
navyBlue   = RGBColor[0.153, 0.51, 0.64];
lightBlue  = RGBColor[0.30, 0.57, 0.66];

title[txt_]    := Cell[txt, "Title"];
subtitle[txt_] := Cell[txt, "Subtitle"];
hd1[txt_]      := Cell[txt, "Section",
   FontColor -> navyBlue, FontFamily -> "Roboto Condensed",
   FontWeight -> "Bold", FontSize -> 22];
hd2[txt_]      := Cell[txt, "Subsection",
   FontColor -> lightBlue, FontFamily -> "Roboto Condensed",
   FontWeight -> "Bold", FontSize -> 16];

para[stuff_] := Cell[TextData[stuff], "Text",
   CellMargins -> {{50, 50}, {6, 6}}];

abstractCell[stuff_] := Cell[TextData[stuff], "Text",
   FontSize -> 14, CellMargins -> {{60, 60}, {6, 6}},
   Background -> RGBColor[0.97, 0.96, 0.92]];

bold[s_]  := StyleBox[s, FontWeight -> Bold];
ital[s_]  := StyleBox[s, FontSlant -> Italic];
math[s_]  := StyleBox[s, FontFamily -> "Times", FontSlant -> Italic];
mono[s_]  := StyleBox[s, FontFamily -> "Courier"];

imath[boxes_] := Cell[BoxData @ FormBox[boxes, TraditionalForm], "InlineFormula"];

displayMath[expr_] := Cell[
   BoxData @ FormBox[expr, TraditionalForm], "DisplayFormula",
   CellMargins -> {{80, 50}, {8, 8}}];

(* ---- Image cache directory ---- *)
imgCacheDir = FileNameJoin[{repoRoot, "community", ".img-cache"}];
If[!DirectoryQ[imgCacheDir], CreateDirectory[imgCacheDir]];

(* ---- Static PNG (heroes' first-frame fallback) resize to <=640 ---- *)
resizedCopy[path_] := Module[{cached, w, h, im},
   cached = FileNameJoin[{imgCacheDir, FileNameTake[path]}];
   If[FileExistsQ[cached] &&
      FileDate[cached, "Modification"] > FileDate[path, "Modification"],
     Return[cached]];
   im = Import[path];
   {w, h} = ImageDimensions[im];
   If[w > 640, im = ImageResize[im, 640]];
   Export[cached, im, "PNG"];
   cached
];

imgWidth = 640;

(* ---- AI illustrative art (gpt-image-2): atmospheric, NOT data figures.
        Resize the ~1536x1024 PNGs to ~600 px wide, cache, embed with an
        italic caption that CLEARLY states the image is AI-generated. ---- *)
illusDir   = FileNameJoin[{repoRoot, "docs", "images", "illustrations"}];
illusWidth = 560;
illusResized[file_String] := Module[{path, cached, im, w, h},
   path   = FileNameJoin[{illusDir, file}];
   cached = FileNameJoin[{imgCacheDir, "illus_" <> FileBaseName[file] <> "_480.png"}];
   If[FileExistsQ[cached] &&
      FileDate[cached, "Modification"] > FileDate[path, "Modification"],
     Return[Import[cached]]];
   im = Import[path];
   {w, h} = ImageDimensions[im];
   If[w > illusWidth, im = ImageResize[im, illusWidth]];
   Export[cached, im, "PNG"];
   im
];
(* Returns a 2-element LIST {imageCell, captionCell}; writeAll flattens one
   level so the pair splices into the cell stream. (Returning a Sequence from a
   Module body is unsafe: it flattens into the Module/CompoundExpression args
   and the image cell is silently dropped.) *)
illusCell[file_String, caption_String] := Module[{full, hi, jp},
   full = Import[FileNameJoin[{illusDir, file}]];
   hi   = If[Head[full] === Image, ImageResize[full, 560], full];
   (* Photographic art: a JPEG round-trip stores ~40% smaller in the .nb than PNG
      with no visible loss, so we can embed higher-res (700 px) data than before
      AND shrink the file. Displayed at illusWidth on the page. *)
   jp   = If[Head[hi] === Image,
            Quiet @ Check[
              ImportString[ExportString[hi, "JPEG", "CompressionLevel" -> 0.55], "JPEG"],
              hi],
            hi];
   If[Head[jp] =!= Image, jp = hi];   (* never embed a broken/blank image *)
   If[Head[jp] =!= Image, Print["  !! illustration failed to load: ", file]];
   {Cell[BoxData @ ToBoxes[Image[jp, ImageSize -> illusWidth]], "Output",
        ShowCellLabel -> False, TextAlignment -> Center,
        CellMargins -> {{Automatic, Automatic}, {12, 2}}],
    captionCell[caption]}
];

(* ---- LIVE Manipulate cell: embed the actual Manipulate object as boxes so it
        is interactive in the FrontEnd (the PDF shows the initial frame). ---- *)
manipCell[expr_] := Cell[BoxData @ ToBoxes[expr], "Output",
   ShowCellLabel -> False, TextAlignment -> Center,
   CellMargins -> {{Automatic, Automatic}, {10, 10}}];

(* ---- FIGURE cell: embed the PRE-RENDERED docs/images/fig_<name>.png (which
        carries the figure's true aspect ratio) at 600 px wide, aspect
        preserved. The sxFig...[] call is shown ABOVE as a separate Input cell
        (wlIn) so the reader can regenerate the figure live; the Output cell
        here just displays the finished PNG. We do NOT live-rasterise at a fixed
        760 px (that letterboxed wide figures and ballooned insets). ---- *)
figWidth = 600;
figPngFor = <|
   "AltitudeCircle"      -> "fig_altitude_circle",
   "Intercept"           -> "fig_intercept",
   "CockedHat"           -> "fig_cocked_hat",
   "RunningFix"          -> "fig_running_fix",
   "CorrectionsWaterfall"-> "fig_corrections_waterfall",
   "SubsolarTrack"       -> "fig_subsolar_track",
   "ErrorEllipse"        -> "fig_error_ellipse",
   "CutAngle"            -> "fig_cut_angle",
   "CelestialVsGPS"      -> "fig_celestial_vs_gps",
   "PZXTriangle"         -> "fig_pzx_triangle",
   "CelestialSphere"     -> "fig_celestial_sphere",
   "EquationOfTime"      -> "fig_equation_of_time",
   "HorizonDip"          -> "fig_horizon_dip",
   "Refraction"          -> "fig_refraction",
   "CockedHatTheorem"    -> "fig_cocked_hat_theorem",
   "CRLB"                -> "fig_crlb",
   "Chronometer"         -> "fig_chronometer",
   "NoonSight"           -> "fig_noon_sight",
   "StarSelection"       -> "fig_star_selection",
   "StarChart"           -> "fig_star_chart",
   "ErrorBudget"         -> "fig_error_budget",
   "Speeds"              -> "fig_speeds",
   "TwilightReplay"      -> "fig_twilight_replay",
   "EphemerisValidation" -> "fig_ephemeris_validation",
   "LunarDistance"       -> "fig_lunar_distance",
   "EKF"                 -> "fig_ekf",
   "Historical"          -> "fig_historical",
   "Posterior"           -> "fig_posterior"
|>;

SetAttributes[renderFig, HoldRest];
renderFig[name_String, expr_] := Module[{base, png},
   base = Lookup[figPngFor, name, Missing[]];
   If[MissingQ[base],
     Print["  !! renderFig: no PNG mapping for sxFig", name];
     Return @ Cell[
        TextData[{bold["[ no PNG mapping for sxFig" <> name <> " ]"]}],
        "Text", FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]];
   png = FileNameJoin[{imgDir, base <> ".png"}];
   If[!FileExistsQ[png],
     Print["  !! renderFig: missing PNG ", png];
     Return @ Cell[
        TextData[{bold["[ missing figure: " <> base <> ".png ]"]}],
        "Text", FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]];
   (* Embed at 1100 px data (crisp when zoomed in the notebook) but
      palette-quantised to 160 colours: for these flat/diagram figures this is
      SHARPER than the old 640 px cache AND smaller in the .nb. Displayed at
      figWidth on the page. *)
   Cell[BoxData @ ToBoxes[
        Image[ColorQuantize[ImageResize[Import[png], 1100], 160],
              ImageSize -> figWidth]],
      "Output", ShowCellLabel -> False, TextAlignment -> Center,
      CellMargins -> {{Automatic, Automatic}, {10, 6}}]
];

(* ---- Animated GIF cell (hero animations) ---- *)
animWidth = 512;   (* heroes: 512 px keeps the look; frame rate/count unchanged *)

animCell[file_String] := animCell[file, ""];
animCell[file_String, caption_String] := Module[
   {path = FileNameJoin[{imgDir, file}], all, anim, boxes, firstFrame},
   If[!FileExistsQ[path],
     Return @ Cell[
        TextData[{bold["[ missing animation: " <> file <> " ]"]}],
        "Text", FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]];
   all = Quiet @ Import[path, {"GIF", "ImageList"}];
   If[!ListQ[all] || Length[all] == 0,
     Print["  animCell fallback (first frame): ", file];
     firstFrame = Quiet @ Image[Import[path], ImageSize -> animWidth];
     If[Head[firstFrame] === Image,
       Return @ Cell[BoxData @ ToBoxes[firstFrame], "Output",
          ShowCellLabel -> False, TextAlignment -> Center,
          CellMargins -> {{Automatic, Automatic}, {10, 14}}],
       Return @ Cell[
          TextData[{bold["[ GIF load failed: " <> file <> " ]"]}],
          "Text", FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]]];
   (* 96-colour palette (no dither) roughly halves the .nb weight of each
      animation vs 256, with no visible banding on these dark scenes; the frame
      RATE and frame COUNT are untouched. *)
   all = ColorQuantize[ImageResize[#, {animWidth}], 96] & /@ all;
   anim = AnimatedImage[all,
      FrameRate -> 1.5, AnimationRunning -> True,
      AnimationRepetitions -> Infinity];
   boxes = ToBoxes[
      If[caption === "",
         anim,
         Column[{anim,
            Pane[Style[caption, Italic, FontSize -> 10,
               FontFamily -> "Helvetica", FontColor -> GrayLevel[0.35]],
               animWidth, Alignment -> Center]
         }, Alignment -> Center, Spacings -> 0.6]]];
   Cell[BoxData[boxes], "Output", ShowCellLabel -> False,
      TextAlignment -> Center,
      CellMargins -> {{Automatic, Automatic}, {10, 14}}]
];

captionCell[args__] := Cell[
   TextData[{args}], "Text",
   FontSlant -> Italic, FontSize -> 11, FontColor -> GrayLevel[0.35],
   CellMargins -> {{70, 70}, {2, 14}}];

wlIn[code_String] := Cell[code, "Input",
   CellMargins -> {{60, 30}, {6, 6}}, FontSize -> 11];

codeIn[code_String] := Cell[code, "Program",
   CellMargins -> {{60, 30}, {6, 6}}, FontFamily -> "Courier", FontSize -> 11,
   ShowStringCharacters -> False, Background -> RGBColor[0.95, 0.96, 0.98]];

outCell[result_] := Cell[BoxData @ ToBoxes[result], "Output",
   ShowCellLabel -> False, TextAlignment -> Left,
   CellMargins -> {{60, 30}, {4, 8}}];

noteBox[stuff_] := Cell[TextData[stuff], "Text",
   FontSize -> 12, CellMargins -> {{70, 70}, {6, 6}},
   Background -> RGBColor[0.93, 0.95, 0.99],
   CellFrame -> {{4, 0}, {0, 0}}, CellFrameColor -> RGBColor[0.35, 0.55, 0.75]];

(* short numeric literal for injecting computed values into runnable code strings *)
fnum[x_] := ToString[NumberForm[N[x], {12, 5}]];


(* ====================================================================
   Pre-compute every demonstrated cn... result (evaluate during build)
   ==================================================================== *)

Print["Pre-computing demo outputs..."];

(* \[Section]2 \[LongDash] sub-solar point *)
tDemo = DateObject[{2024, 11, 22, 12, 0, 0}, TimeZone -> 0];
gpDemo = cnSunGP[tDemo];
Print["  \[Section]2 cnSunGP: ", gpDemo];

(* \[Section]3 \[LongDash] altitude from GP *)
posDemo = {35., -15.};
altDemo = cnAltitudeFromGP[posDemo, gpDemo];
Print["  \[Section]3 cnAltitudeFromGP: ", altDemo];

(* \[Section]4 \[LongDash] observed altitude (corrections) *)
hoDemo = cnObservedAltitude[45., 2., 0., "Lower"];
Print["  \[Section]4 cnObservedAltitude: ", hoDemo];

(* \[Section]5 \[LongDash] computed altitude / azimuth at AP *)
{hcDemo, znDemo} = cnComputedAltitude[posDemo, gpDemo];
Print["  \[Section]5 cnComputedAltitude: Hc=", hcDemo, " Zn=", znDemo];

(* \[Section]6 \[LongDash] LOP with a small deliberate intercept *)
hoLop = altDemo + 0.05;   (* 0.05\[Degree] = 3 nm toward *)
lopDemo = cnLOP[posDemo, gpDemo, hoLop];
Print["  \[Section]6 cnLOP intercept: ", lopDemo["interceptNm"], " nm"];

(* \[Section]7 \[LongDash] running fix from two Sun LOPs *)
tMorn = DateObject[{2024, 11, 22, 8, 0, 0}, TimeZone -> 0];
gpMorn = cnSunGP[tMorn];
truePos = {35.0, -15.0};
apMorn = {35.0, -15.5};
apNoon = {35.0, -15.0};
hoMorn = cnAltitudeFromGP[truePos, gpMorn] + 0.02;
hoNoon = cnAltitudeFromGP[truePos, gpDemo] - 0.01;
lopMorn = cnLOP[apMorn, gpMorn, hoMorn];
lopNoon = cnLOP[apNoon, gpDemo, hoNoon];
rfDemo = cnRunningFix[{
   <|"lop" -> lopMorn, "course" -> 270., "distance" -> 20.|>,
   <|"lop" -> lopNoon, "course" -> 270., "distance" -> 0.|>}];
Print["  \[Section]7 cnRunningFix: ", rfDemo];

(* \[Section]8 \[LongDash] twilight star geometry: best triplet, GDOP, simultaneous fix *)
tStar   = DateObject[{2025, 6, 21, 22, 0, 0}, TimeZone -> 0];
posStar = {15., -40.};
starDemo = cnBestStarTriplet[posStar, tStar];
Print["  \[Section]8 cnBestStarTriplet: ", starDemo];
gdopDemo = cnFixGDOP[posStar, tStar, starDemo["stars"]];
Print["  \[Section]8 cnFixGDOP: ", gdopDemo];
(* simultaneous three-star fix (zero-noise LOPs recover the true position) *)
starGPs = cnBodyGPFor[{"Star", #}, tStar] & /@ starDemo["stars"];
starHo  = cnAltitudeFromGP[posStar, #] & /@ starGPs;
lops3   = MapThread[cnLOP[posStar, #1, #2] &, {starGPs, starHo}];
fixDemo = cnFix[lops3];
Print["  \[Section]8 cnFix (3 stars): ", fixDemo];

(* \[Section]9 \[LongDash] CRLB error ellipse + cocked-hat 25% probability *)
crlbDemo = cnErrorEllipse @ cnCRLBCovariance[{45., 165., 285.}, 1.0];
Print["  \[Section]9 cnErrorEllipse (CRLB): ", crlbDemo];
SeedRandom[1];
chpDemo = cnCockedHatProbability[posStar, tStar,
   ({"Star", #} & /@ starDemo["stars"]), <|"sigmaMin" -> 1.5|>, 2000];
Print["  \[Section]9 cnCockedHatProbability (n=2000): ", chpDemo];

(* \[Section]10a \[LongDash] the 4-second rule *)
lonErrDemo = cnLongitudeErrorPerSecond[0.];
Print["  \[Section]10a cnLongitudeErrorPerSecond[0.]: ", lonErrDemo];

(* \[Section]10b \[LongDash] noon latitude *)
noonLatDemo = cnNoonLatitude[50., -20., "S"];
Print["  \[Section]10b cnNoonLatitude: ", noonLatDemo];

(* \[Section]10c \[LongDash] lunar distance: the clock in the sky *)
tLun   = DateObject[{2024, 6, 13, 18, 0, 0}, TimeZone -> 0];
posLun = {35., -40.};
mGPlun = cnMoonGP[tLun]; sGPlun = cnSunGP[tLun]; hpMlun = cnMoonHP[tLun];
hMg = cnAltitudeFromGP[posLun, mGPlun]; hSg = cnAltitudeFromGP[posLun, sGPlun];
dGeoDemo = cnLunarDistanceGeocentric[tLun];
Print["  \[Section]10c cnLunarDistanceGeocentric: ", dGeoDemo];
(* build a realistic observed (apparent, topocentric) distance, then clear it *)
appAltMoon[hg_, hp_] := Module[{h = hg},
   Do[h = hg - ArcSin[Sin[hp Degree] Cos[h Degree]]/Degree + cnRefraction[h]/60., 5]; h];
appAltSun[hg_] := Module[{h = hg},
   Do[h = hg - 0.002443 Cos[h Degree] + cnRefraction[h]/60., 5]; h];
hMa = appAltMoon[hMg, hpMlun]; hSa = appAltSun[hSg];
cosAlun = (Cos[dGeoDemo Degree] - Sin[hMg Degree] Sin[hSg Degree])/(Cos[hMg Degree] Cos[hSg Degree]);
dObsDemo = ArcCos[Clip[Sin[hMa Degree] Sin[hSa Degree] +
      Cos[hMa Degree] Cos[hSa Degree] cosAlun, {-1, 1}]]/Degree;
dClrDemo = cnClearLunarDistance[dObsDemo, hMa, hSa, hpMlun];
Print["  \[Section]10c cnClearLunarDistance: ", dClrDemo];
tRecDemo = cnLunarDistanceGMT[dClrDemo, DatePlus[tLun, {1.5, "Hour"}]];
Print["  \[Section]10c cnLunarDistanceGMT: ", tRecDemo];

(* \[Section]11 \[LongDash] voyage summary statistics *)
voyageRows = Rest @ Import[FileNameJoin[{dataDir, "voyage.csv"}], "CSV"];
nDays  = Length[voyageRows];
startPos = {voyageRows[[1, 3]], voyageRows[[1, 4]]};
endPos   = {voyageRows[[-1, 3]], voyageRows[[-1, 4]]};
totalNm = N @ QuantityMagnitude[
   UnitConvert[GeoDistance[GeoPosition[startPos], GeoPosition[endPos]], "NauticalMiles"]];
Print["  \[Section]11 voyage: ", nDays, " days, great-circle ~", totalNm, " nm"];

(* \[Section]12a \[LongDash] EKF voyage (build voyage records from CSV, run filter) *)
voyAll = Import[FileNameJoin[{dataDir, "voyage.csv"}], "CSV"];
vHdr = voyAll[[1]]; vData = voyAll[[2 ;;]];
vCol[n_] := Position[vHdr, n][[1, 1]];
vDate[v_] := If[Head[v] === DateObject, v,
   DateObject[StringReplace[v, "Z" -> ""], TimeZone -> 0]];
voyage = Table[
   <|"day" -> r[[vCol["day"]]], "t" -> vDate[r[[vCol["datetimeUTC"]]]],
     "truePos" -> {r[[vCol["latTrue"]]], r[[vCol["lonTrue"]]]},
     "drPos" -> {r[[vCol["latDR"]]], r[[vCol["lonDR"]]]}|>, {r, vData}];
SeedRandom[42];
ekfRecs = cnEKFVoyage[voyage, 3, <|"seed" -> 42, "sigmaMin" -> 1.0|>];
ekfDemo = KeyTake[ekfRecs[[-1]], {"day", "estPos", "errVsTruthNm", "drErrorNm"}];
Print["  \[Section]12a cnEKFVoyage last day: ", ekfDemo];

(* \[Section]12b \[LongDash] systematic vs random error: a tight cocked hat is NOT proof of accuracy *)
SeedRandom[7];
sysDemoFull = cnSystematicFixError[posStar, tStar,
   ({"Star", #} & /@ starDemo["stars"]), 1.0, 3.0, 500];
sysDemo = KeyTake[sysDemoFull,
   {"biasNm", "biasVecNm", "scatterTraceNm2", "indexErrorMin"}];
Print["  \[Section]12b cnSystematicFixError: ", sysDemo];

(* \[Section]13a \[LongDash] ephemeris validation: our engine vs Wolfram SunPosition (offline) *)
t13   = DateObject[{2024, 6, 21, 12, 0, 0}, TimeZone -> 0];
loc13 = {55., -5.};
ourAlt13 = cnAltitudeFromGP[loc13, cnSunGP[t13]];
wAppar13 = QuantityMagnitude[UnitConvert[
   SunPosition[GeoPosition[loc13], t13][[2]], "AngularDegrees"]];
wGeom13  = wAppar13 - cnRefraction[wAppar13]/60.;
ephResDemo = (ourAlt13 - wGeom13)*60.;   (* arcmin *)
Print["  \[Section]13a ephemeris residual (arcmin): ", ephResDemo];

(* \[Section]13b \[LongDash] James Caird worked reduction *)
tHist  = DateObject[{1916, 5, 2, 15, 0, 0}, TimeZone -> 0];
apHist = {-57.0, -46.0};
{hcHist, znHist} = cnComputedAltitude[apHist, cnSunGP[tHist]];
Print["  \[Section]13b James Caird cnComputedAltitude: Hc=", hcHist, " Zn=", znHist];

(* \[Section]9.4 \[LongDash] Bayesian position posterior: a one-liner MAP from cnPosteriorGrid *)
truthPost = {20., -40.};
tsPost = {DateObject[{2024, 11, 22, 9, 0, 0}, TimeZone -> 0],
          DateObject[{2024, 11, 22, 12, 0, 0}, TimeZone -> 0],
          DateObject[{2024, 11, 22, 15, 0, 0}, TimeZone -> 0]};
sightsPost = Table[<|"body" -> "Sun", "t" -> tsPost[[k]],
   "Ho" -> cnAltitudeFromGP[truthPost, cnSunGP[tsPost[[k]]]]|>, {k, 3}];
boxPost = {{19.5, 20.5}, {-40.5, -39.5}};
postGridDemo = cnPosteriorGrid[sightsPost, boxPost, 60, 1.5];
mapPost = postGridDemo["mapEstimate"];
Print["  \[Section]9.4 cnPosteriorGrid MAP (3 sights): ", mapPost];

Print["Demo computations done."];

(* ====================================================================
   \[Section]14 interactives. A live Manipulate renders as a red/inert box in the
   static PDF (headless cannot fire Dynamic). So we embed a STATIC PREVIEW: the
   Graphics each Manipulate shows at its default parameters, rasterised cleanly.
   The runnable sxManip...[] call is still shown above each preview so the reader
   can evaluate it in Mathematica for the live interactive version.
   ==================================================================== *)
Print["Rasterizing \[Section]14 interactive static previews..."];

(* Rasterize a preview Graphics at 144 dpi, cache it, and PROBE for the red
   failure-box pattern (a broken Dynamic rasterises to a near-pure-red panel). *)
manipPreviewImage[name_String, gfx_] := Module[{cached, img, redFrac, blankFrac},
   cached = FileNameJoin[{imgCacheDir, "preview_" <> name <> ".png"}];
   img = Rasterize[gfx, ImageResolution -> 144, Background -> White];
   If[Head[img] =!= Image,
      Print["  !! ", name, " preview did NOT rasterise to an Image"];
      Return[$Failed]];
   redFrac = N @ Mean @ Flatten @ Map[
      Boole[#[[1]] > 0.55 && #[[2]] < 0.35 && #[[3]] < 0.35] &,
      ImageData[ColorConvert[img, "RGB"]], {2}];
   blankFrac = N @ Mean @ Flatten @ Map[
      Boole[Min[#] > 0.97] &, ImageData[ColorConvert[img, "RGB"]], {2}];
   Print["  ", name, " preview: ", ImageDimensions[img],
      "  red-fraction=", redFrac, "  white-fraction=", blankFrac];
   If[redFrac > 0.4,
      Print["  !! ", name, " preview looks like a RED error box (redFrac=", redFrac, ")"]];
   If[blankFrac > 0.98,
      Print["  !! ", name, " preview looks BLANK (whiteFrac=", blankFrac, ")"]];
   Export[cached, img, "PNG"];
   img
];

manipInterceptPreviewImg = manipPreviewImage["Intercept",     sxManipInterceptPreview[]];
manipPosteriorPreviewImg = manipPreviewImage["Posterior",     sxManipPosteriorPreview[]];
manipPlanningPreviewImg  = manipPreviewImage["SightPlanning", sxManipSightPlanningPreview[]];

(* Cell embedding a rasterised static preview at 600 px (aspect preserved). *)
manipPreviewCell[img_] := Cell[
   BoxData @ ToBoxes[Image[img, ImageSize -> figWidth]], "Output",
   ShowCellLabel -> False, TextAlignment -> Center,
   CellMargins -> {{Automatic, Automatic}, {10, 6}}];

(* ====================================================================
   Accumulate all cells
   ==================================================================== *)

$allCells = {};
(* Flatten one level so helpers that emit a {imageCell, captionCell} pair
   (illusCell) splice cleanly; single Cell[...] expressions are unaffected. *)
writeAll[cs_List] := Module[{flat = Flatten[cs, 1]},
   $allCells = Join[$allCells, flat];
   Print["  writeAll +", Length[flat], " \[RightArrow] total ", Length[$allCells]]];


(* ==============================================================
   TITLE + HERO
   ============================================================== *)

writeAll[{

title["Finding Yourself at Sea: The Mathematics of Celestial Navigation"],

Cell["A complete Wolfram Language implementation of celestial navigation: \
sextant corrections, sight reduction, running fixes, star selection, \
Cram\[EAcute]r\[Dash]Rao bounds, lunar-distance longitude, a recursive Kalman fix, and a \
simulated Atlantic passage \[LongDash] all code loaded from two .wl packages and \
shown above every result.", "Subtitle"],

Cell["Wolfram Community \[LongDash] June 2026",
   "Subsubtitle", FontSlant -> Italic, FontColor -> GrayLevel[0.4]],

animCell["hero_globe.gif",
   "The whole idea in one image. Each sextant sight places you on a circle of \
equal altitude drawn on the Earth; three such circles sweep across the globe and \
converge on your position \[LongDash] the glowing fix. This post derives that geometry \
from first principles and measures exactly how well it works. Built entirely \
offline; the star field and Milky Way are for atmosphere."]

}];


(* ==============================================================
   ABSTRACT
   ============================================================== *)

writeAll[{

hd1["Abstract"],

abstractCell[{"On a clear night at sea, three star observations with a sextant \
and a reliable clock fix your position to about 1 nautical mile (nm) \[LongDash] but \
only the ", ital["random"], " part of the error averages down to 1 nm; the \
systematic part (index error, refraction model, almanac) does not, so real \
fixes are usually 1\[Dash]3 nm and sometimes worse. GPS does the same job to 5 m. \
Both are elegant: GPS triangulates from radio signals from 24 satellites; the \
sextant triangulates from ancient starlight and spherical geometry. This post \
builds the entire method from first principles in Wolfram Language."}],

abstractCell[{"All code lives in two package files \[LongDash] ",
   mono["CelestialNavigation.wl"], " (the engine) and ", mono["SextantFigures.wl"],
   " (the figures) \[LongDash] which the notebook loads with ", mono["Get"],
   ". Above every figure and every computed number you will see the exact \
function call that produced it, so the whole post is re-runnable."}],

abstractCell[{"Topics: sextant altitude corrections; the Marcq St\[Dash]Hilaire \
intercept method; running fixes and the cocked hat; the 57-star navigational \
catalogue with IAU-1976 precession; optimal star selection by GDOP; the \
Cram\[EAcute]r\[Dash]Rao lower bound; the Daniels 1951 cocked-hat 25 % theorem; the \
lunar-distance method for finding GMT without a chronometer; a recursive \
(Kalman) running fix; offline ephemeris validation; and a reconstruction of \
Shackleton's 1916 ", ital["James Caird"], " voyage. Everything runs offline."}],

abstractCell[{ital["Audience:"], " the narrative assumes high-school \
trigonometry and a passing curiosity about ships. Each section opens with an \
intuitive explanation, then deepens into the exact formulae in the package."}]

}];


(* ==============================================================
   SETUP
   ============================================================== *)

writeAll[{

hd1["Setup (optional \[LongDash] only needed to re-evaluate code cells)"],

para[{ital["You can skip this section entirely if you only want to read."],
   " Every figure and every code output is pre-computed, so the prose, \
equations, figures, and numbers all work without evaluating anything."}],

para[{"To run the code cells yourself, place ", mono["CelestialNavigation.wl"],
   ", ", mono["SextantFigures.wl"], ", and ", mono["SextantInteractive.wl"],
   " (all three uploaded alongside the notebook) \
in the same folder as the ", mono[".nb"], " file, then evaluate:"}],

wlIn["SetDirectory[NotebookDirectory[]];
Get[\"CelestialNavigation.wl\"];
Get[\"SextantFigures.wl\"];
Get[\"SextantInteractive.wl\"];
Print[\"engine v\", cnVersion[], \"  /  \", sxFigVersion[], \"  /  \", sxManipVersion[]]"],

para[{"The engine defines the ", mono["cn..."], " functions (the mathematics); \
the figures package defines 27 ", mono["sxFig...[]"], " functions, each returning \
a finished graphic; and ", mono["SextantInteractive.wl"], " defines the three \
live ", mono["sxManip...[]"], " demonstrations of \[Section]14. Re-evaluating any ",
   mono["sxFig...[]"], " cell regenerates that figure from scratch."}]

}];


(* ==============================================================
   \[Section]1  The problem of position
   ============================================================== *)

writeAll[{

hd1["1.  The Problem of Position"],

illusCell["illus_navigator_sextant.png",
   "AI-generated illustration (OpenAI gpt-image-2) \[LongDash] for atmosphere, not a \
data figure. The human act at the heart of the method: a navigator brings a \
star down to the horizon with a sextant. Everything that follows turns that \
single measured angle into a position."],

para[{ital["Latitude was easy."], " The elevation of Polaris above the \
northern horizon equals your latitude to within a degree. A noon Sun \
observation gives it more precisely: latitude = declination + zenith distance \
of the Sun at its highest point. These methods need no clock at all, \
just a sextant and an almanac."}],

para[{ital["Longitude was hard."], " Earth rotates once every 24 hours, \
so every 4 seconds of time corresponds to 1 nautical mile of longitude error \
at the equator. Before the Harrison chronometer (H4, 1759), no clock at sea \
was accurate enough for longitude. The 1714 Longitude Act offered \[Sterling]20,000 \
(about \[Sterling]3 M today) to anyone who solved the problem."}],

para[{ital["Two key methods emerged."], " Thomas Sumner (1837) accidentally \
discovered the circle of equal altitude: every observer on the same circle \
around the sub-stellar point sees the same star altitude. Marcq St\[Dash]Hilaire \
(1875) turned two or more such circles into an operational fix by the \
intercept method, still taught to every maritime candidate today."}],

para[{"The cover animation above is the thesis of this whole post: a celestial \
navigator, armed only with a sextant, a watch, and spherical trigonometry, \
holds position to within a couple of nautical miles across an ocean crossing. \
The rest of the post derives exactly how, and exactly how well."}]

}];


(* ==============================================================
   \[Section]2  The celestial sphere & coordinates
   ============================================================== *)

writeAll[{

hd1["2.  The Celestial Sphere & Coordinates"],

illusCell["illus_celestial_dome.png",
   "AI-generated illustration (OpenAI gpt-image-2) \[LongDash] for atmosphere, not a \
data figure. The celestial dome over the observer: stars fixed on an imaginary \
sphere of infinite radius, each reduced to two angles \[LongDash] altitude above the \
horizon and azimuth around it."],

para[{"The celestial sphere is an imaginary sphere of infinite radius \
centred on Earth. Every celestial body is projected onto it. An observer's \
view reduces to two angles: ", bold["altitude"], " H (degrees above the \
horizon, 0\[Degree]\[Dash]90\[Degree]) and ", bold["azimuth"], " Zn (bearing clockwise from \
North, 0\[Degree]\[Dash]360\[Degree])."}],

para[{"A body's position on the sphere is fixed by its ", bold["declination"],
   " \[Delta] (celestial latitude) and its ", bold["Greenwich Hour Angle"],
   " (GHA): the angle westward from the Greenwich meridian to the body's \
meridian. For stars the catalogue gives ", bold["Sidereal Hour Angle"],
   " (SHA) measured from the First Point of Aries (\[AriesSign]), \
and GHA = GAST \[Minus] SHA (mod 360\[Degree])."}],

displayMath[RowBox[{"GHA", "=",
   RowBox[{"GAST", "-", "SHA", " ", "(", "mod", " ", "360", "\[Degree]", ")"}]}]],

para[{"The Sun's ", bold["sub-solar point"], " (geographic position, GP) is \
the point on Earth's surface where the Sun is directly overhead. Its latitude \
equals the solar declination and its longitude is \[Minus]GHA. \
For 22 Nov 2024 at 12:00 UTC:"}],

wlIn["cnSunGP[DateObject[{2024, 11, 22, 12, 0, 0}, TimeZone -> 0]]"],
outCell[gpDemo],
captionCell["Result: {declination \[Degree], GP longitude \[Degree]}. \
The Sun's declination on 22 Nov 2024 is near \[Minus]20\[Degree] (the northern winter \
approach)."],

wlIn["sxFigCelestialSphere[]"],
renderFig["CelestialSphere", sxFigCelestialSphere[]],
captionCell["Figure 2.1  The celestial sphere, showing the observer's zenith Z, \
the body X, the celestial pole P, and the navigational triangle PZX. \
The altitude H is the complement of the zenith distance z = 90\[Degree] \[Minus] H."],

wlIn["sxFigEquationOfTime[]"],
renderFig["EquationOfTime", sxFigEquationOfTime[]],
captionCell["Figure 2.2  The equation of time (EoT) and the analemma. \
EoT = apparent solar time \[Minus] mean solar time, ranging \[PlusMinus] 16 minutes. \
The analemma is the figure-eight traced by the Sun at a fixed clock time \
through the year."],

wlIn["sxFigSubsolarTrack[]"],
renderFig["SubsolarTrack", sxFigSubsolarTrack[]],
captionCell["Figure 2.3  Sub-solar point track over 24 hours (22 Nov 2024). \
The latitude band is set by solar declination; the longitude sweeps 360\[Degree] \
in 24 hours as Earth rotates."]

}];


(* ==============================================================
   \[Section]3  A sight is a circle
   ============================================================== *)

writeAll[{

hd1["3.  A Sight Is a Circle"],

para[{"When you measure the Sun's altitude H = 45\[Degree] with a sextant, you know \
only that you lie somewhere on a ", bold["circle of equal altitude"],
   ": a circle on Earth's surface centred on the GP of the Sun, with angular \
radius 90\[Degree] \[Minus] H = 45\[Degree]. That circle has radius 45\[Degree] \[Times] 60 = 2700 nm \[LongDash] \
you are a long way from being exactly located."}],

para[{"The altitude at any observer position {lat, lon} from a body at GP \
{dec, lon\[UnderBracket]GP} follows directly from the spherical law of cosines:"}],

displayMath[RowBox[{RowBox[{"sin", " ", "H"}], "=",
   RowBox[{RowBox[{"sin", " ", "\[Phi]"}], " ", RowBox[{"sin", " ", "\[Delta]"}], "+",
      RowBox[{"cos", " ", "\[Phi]"}], " ", RowBox[{"cos", " ", "\[Delta]"}], " ",
      RowBox[{"cos", " ", "LHA"}]}]}]],

para[{"where \[Phi] is observer latitude, \[Delta] is declination, and \
LHA = lon\[UnderBracket]observer \[Minus] lon\[UnderBracket]GP. At {35\[Degree]N, 15\[Degree]W} \
and the sub-solar GP computed above:"}],

wlIn["cnAltitudeFromGP[{35., -15.}, cnSunGP[DateObject[{2024, 11, 22, 12, 0, 0}, TimeZone -> 0]]]"],
outCell[altDemo],
captionCell["Result: the geometric altitude of the Sun in degrees \[LongDash] the ideal \
sextant reading before any instrument corrections."],

wlIn["sxFigAltitudeCircle[]"],
renderFig["AltitudeCircle", sxFigAltitudeCircle[]],
captionCell["Figure 3.1  The circle of equal altitude centred on the GP. \
Two circles narrow the fix to two points; a third disambiguates. \
This is the conceptual foundation of celestial navigation."]

}];


(* ==============================================================
   \[Section]4  The sextant and its corrections
   ============================================================== *)

writeAll[{

hd1["4.  The Sextant and Its Corrections"],

para[{"A sextant measures the angle between a celestial body and the visible \
horizon. The reading H\[UnderBracket]s (sextant altitude) differs from the true \
observed altitude H\[UnderBracket]o by five systematic corrections:"}],

para[{"\[Bullet] ", bold["Index error (IE):"], " instrument calibration offset, \
measured by the navigator."}],
para[{"\[Bullet] ", bold["Dip:"], " the visible horizon is below true horizontal \
because the eye is elevated. Dip \[TildeTilde] 1.76\[Sqrt]h arcmin, h in metres. Subtract."}],
para[{"\[Bullet] ", bold["Refraction:"], " the atmosphere bends light downward. \
Bennett (1982): R = cot(H + 7.31/(H+4.4)) arcmin. Subtract. At 45\[Degree], R \[TildeTilde] 1'."}],
para[{"\[Bullet] ", bold["Semi-diameter (SD):"], " the Sun and Moon are discs; \
navigators sight a limb. Add/subtract \[TildeTilde]16' for lower/upper limb."}],
para[{"\[Bullet] ", bold["Parallax (HP):"], " the Sun is not infinitely far; \
HP \[TildeTilde] 0.15\[CenterDot]cos(H) arcmin. Add."}],

para[{"For a sextant reading of 45\[Degree], eye height 2 m, no index error, \
lower limb:"}],

wlIn["cnObservedAltitude[45., 2., 0., \"Lower\"]"],
outCell[hoDemo],
captionCell["Result: observed altitude H\[UnderBracket]o in degrees. The net \
correction is about \[Minus]0.03\[Degree] (\[Minus]2') \[LongDash] dip and refraction dominate here."],

wlIn["sxFigCorrectionsWaterfall[]"],
renderFig["CorrectionsWaterfall", sxFigCorrectionsWaterfall[]],
captionCell["Figure 4.1  Waterfall chart of sextant corrections for a Sun \
lower-limb sight at 45\[Degree], 2 m eye height. Each bar is one correction; \
the net gives H\[UnderBracket]o."],

wlIn["sxFigRefraction[]"],
renderFig["Refraction", sxFigRefraction[]],
captionCell["Figure 4.2  Atmospheric refraction vs altitude (Bennett 1982, \
Saemundsson 1986, and the simple cot model). Refraction diverges near the \
horizon and is below 1' above 45\[Degree]."],

wlIn["sxFigHorizonDip[]"],
renderFig["HorizonDip", sxFigHorizonDip[]],
captionCell["Figure 4.3  Horizon dip vs eye height. At 2 m (small-boat deck) \
dip \[TildeTilde] 2.5'; at 15 m (ship's bridge) dip \[TildeTilde] 6.8'."]

}];


(* ==============================================================
   \[Section]5  The navigational triangle
   ============================================================== *)

writeAll[{

hd1["5.  The Navigational Triangle"],

para[{"Three special points define a spherical triangle for each observation: \
the elevated ", bold["Pole P"], ", the observer's ", bold["Zenith Z"],
   ", and the body ", bold["X"], ". This is the ", bold["navigational triangle"],
   " PZX."}],

para[{"Its three sides carry the key angles: PZ = co-latitude (90\[Degree]\[Minus]\[Phi]), \
ZX = zenith distance (90\[Degree]\[Minus]H), and PX = polar distance (90\[Degree]\[Minus]\[Delta]). \
The angle at P equals the Local Hour Angle (LHA). The spherical law of cosines \
on side ZX gives the fundamental equation of sight reduction:"}],

displayMath[RowBox[{RowBox[{"sin", " ", "H"}], "=",
   RowBox[{RowBox[{"sin", " ", "\[Phi]"}], " ", RowBox[{"sin", " ", "\[Delta]"}], "+",
      RowBox[{"cos", " ", "\[Phi]"}], " ", RowBox[{"cos", " ", "\[Delta]"}], " ",
      RowBox[{"cos", " ", "LHA"}]}]}]],

para[{"Given an assumed position {lat, lon} and the body's GP, it gives the ",
   bold["computed altitude H\[UnderBracket]c"], " and azimuth Zn. At the demo position:"}],

wlIn["cnComputedAltitude[{35., -15.}, cnSunGP[DateObject[{2024, 11, 22, 12, 0, 0}, TimeZone -> 0]]]"],
outCell[{hcDemo, znDemo}],
captionCell["Result: {H\[UnderBracket]c \[Degree], Zn \[Degree]}. The Sun bears roughly south \
(Zn near 180\[Degree]) in the northern hemisphere near noon."],

wlIn["sxFigPZXTriangle[]"],
renderFig["PZXTriangle", sxFigPZXTriangle[]],
captionCell["Figure 5.1  The PZX navigational triangle on the celestial sphere. \
The law of cosines on side ZX yields sin H = sin \[Phi] sin \[Delta] + cos \[Phi] cos \[Delta] cos LHA."]

}];


(* ==============================================================
   \[Section]6  Sight reduction \[LongDash] the intercept method
   ============================================================== *)

writeAll[{

hd1["6.  Sight Reduction \[LongDash] The Intercept Method"],

para[{"The Marcq St\[Dash]Hilaire (1875) intercept method turns a single sextant \
observation into a ", bold["line of position"], " (LOP):"}],

para[{"(1) Choose an ", bold["assumed position"], " (AP) near your DR position. \
(2) Compute H\[UnderBracket]c and Zn from the AP. \
(3) The ", bold["intercept"], " is a = (H\[UnderBracket]o \[Minus] H\[UnderBracket]c)\[Times]60 nm \
(positive = ", ital["toward"], " the body). \
(4) Plot a line perpendicular to Zn at distance a from the AP."}],

para[{"One observation with a deliberate 0.05\[Degree] (3 nm) positive intercept:"}],

wlIn["cnLOP[{35., -15.}, {" <> fnum[gpDemo[[1]]] <> ", " <> fnum[gpDemo[[2]]] <>
   "}, " <> fnum[hoLop] <> "]"],
outCell[lopDemo],
captionCell["Result: an Association with keys AP, Hc, Zn, Ho, interceptNm \
(positive = Toward), point (closest point on the LOP to the GP), and bearingDeg. \
With Ho > Hc by 0.05\[Degree], the LOP sits 3 nm toward the Sun from the AP."],

wlIn["sxFigIntercept[]"],
renderFig["Intercept", sxFigIntercept[]],
captionCell["Figure 6.1  The intercept method. AP is the assumed position; \
a is the intercept (positive = toward the body). The LOP is drawn \
perpendicular to the azimuth Zn at distance a from AP."]

}];


(* ==============================================================
   \[Section]7  One line is not a fix
   ============================================================== *)

writeAll[{

hd1["7.  One Line Is Not a Fix \[LongDash] Running Fix & Cocked Hat"],

para[{"A single LOP tells you which line you are on, not where on it. Two \
simultaneous LOPs from well-separated bodies fix the position at their \
intersection. With only the Sun by day, you must use a ", bold["running fix"],
   ": advance the morning LOP along your course and speed to the time of the \
afternoon sight, then intersect."}],

para[{"Three LOPs rarely meet at one point \[LongDash] errors produce a ",
   bold["cocked hat"], " (triangle). Daniels (1951) proved that, for any \
symmetric error distribution, the true position lies ", bold["inside"],
   " the cocked hat with probability exactly 25 % (revisited in \[Section]9)."}],

para[{"Running fix from two Sun sights (08:00 and 12:00 UTC, advancing the \
morning LOP 20 nm on course 270\[Degree]):"}],

wlIn["cnRunningFix[{\n\
  <|\"lop\" -> lopMorn, \"course\" -> 270., \"distance\" -> 20.|>,\n\
  <|\"lop\" -> lopNoon, \"course\" -> 270., \"distance\" -> 0.|>\n\
}]"],
outCell[rfDemo],
captionCell["Result: {latitude, longitude} of the running fix in degrees. \
With near-zero sextant noise the fix recovers the true position to within a \
few tenths of a nm; realistic 1' sextant noise translates to ~1 nm."],

wlIn["sxFigRunningFix[]"],
renderFig["RunningFix", sxFigRunningFix[]],
captionCell["Figure 7.1  Running-fix geometry. The morning LOP is advanced \
along the course track to the time of the noon sight (the transferred position \
line); its intersection with the noon LOP is the running fix."],

wlIn["sxFigCockedHat[]"],
renderFig["CockedHat", sxFigCockedHat[]],
captionCell["Figure 7.2  A cocked-hat triangle from three Sun LOPs with \
realistic 1'-noise sextant errors. The three pairwise intersections form the \
triangle; the true position lies inside with probability 25 % (Daniels 1951)."]

}];


(* ==============================================================
   \[Section]8  Real stars at twilight
   ============================================================== *)

writeAll[{

hd1["8.  Real Stars at Twilight"],

para[{"Astronomical twilight (Sun 12\[Degree]\[Dash]18\[Degree] below the horizon) is the \
navigator's golden window: the horizon is still sharp, yet bright stars are \
visible. In this ~20-minute window, three to five star LOPs can be reduced to a \
simultaneous fix."}],

para[{"The package carries all 57 navigational stars (J2000.0 right ascension, \
declination, magnitude) with full IAU-1976 precession to the date of \
observation. A good triplet needs three stars well above the horizon, evenly \
spaced in azimuth (ideally 120\[Degree] apart), and bright enough to measure."}],

para[{"The geometric quality metric is ", bold["GDOP"], " (geometric dilution \
of precision): GDOP = \[Sqrt]Tr(F^{-1}), where F = \[CapitalSigma] u\[UnderBracket]i \
u\[UnderBracket]i\[Transpose] is the 2\[Times]2 Fisher matrix and \
u\[UnderBracket]i = {sin Zn\[UnderBracket]i, cos Zn\[UnderBracket]i}. \
Perfect 120\[Degree] spacing gives GDOP = \[Sqrt](4/3) \[TildeTilde] 1.155. \
Best triplet at 22:00 UTC on 21 Jun 2025 at {15\[Degree]N, 40\[Degree]W}:"}],

wlIn["cnBestStarTriplet[{15., -40.}, DateObject[{2025, 6, 21, 22, 0, 0}, TimeZone -> 0]]"],
outCell[starDemo],
captionCell["Result: <|\"stars\" -> {n1, n2, n3}, \"gdop\" -> _, \
\"azimuths\" -> {Az1, Az2, Az3}|>. A GDOP near 1.15 means near-optimal spacing."],

para[{"The GDOP of that chosen triplet, evaluated directly:"}],

wlIn["cnFixGDOP[{15., -40.}, DateObject[{2025, 6, 21, 22, 0, 0}, TimeZone -> 0], " <>
   ToString[starDemo["stars"], InputForm] <> "]"],
outCell[gdopDemo],
captionCell["Result: the scalar GDOP for that star set \[LongDash] it matches the \
\"gdop\" key above."],

para[{"With zero-noise LOPs from those three stars, the least-squares \
intersection ", mono["cnFix"], " recovers the true position exactly:"}],

wlIn["lops = MapThread[cnLOP[{15., -40.}, #1, #2] &,\n\
  {cnBodyGPFor[{\"Star\", #}, t] & /@ stars,\n\
   cnAltitudeFromGP[{15., -40.}, #] & /@ gps}];\n\
cnFix[lops]"],
outCell[fixDemo],
captionCell["Result: {latitude, longitude}. The three exact LOPs intersect at \
{15\[Degree]N, 40\[Degree]W}, confirming the reduction is self-consistent."],

wlIn["sxFigStarChart[]"],
renderFig["StarChart", sxFigStarChart[]],
captionCell["Figure 8.1  Twilight sky at the demo position (15\[Degree]N, 40\[Degree]W, \
21 Jun 2025 22:00 UTC). Navigational stars above 15\[Degree] are plotted on an \
altitude-azimuth grid; the optimal (best-GDOP) triplet is highlighted."],

wlIn["sxFigStarSelection[]"],
renderFig["StarSelection", sxFigStarSelection[]],
captionCell["Figure 8.2  GDOP as the third star is varied (best two fixed). \
Lower GDOP = better geometry; the optimal choice fills the azimuth gap."],

wlIn["sxFigTwilightReplay[]"],
renderFig["TwilightReplay", sxFigTwilightReplay[]],
captionCell["Figure 8.3  Replay of one evening twilight star fix. Three star \
LOPs form a small cocked hat; the fix lands within ~1 nm of GPS truth, \
consistent with 1' sextant noise and a low GDOP."]

}];


(* ==============================================================
   \[Section]9  How good is it? The statistics
   ============================================================== *)

writeAll[{

hd1["9.  How Good Is It? The Statistics"],

para[{"Three non-obvious statistical results about celestial navigation \[LongDash] and \
one honest caveat that frames the rest of the post."}],

hd2["9.1  The Cocked-Hat 25 % Theorem"],

para[{"Daniels (1951) proved that for any three LOPs with any symmetric error \
distribution (not just Gaussian), the probability that the true position lies \
inside the cocked-hat triangle is exactly ", bold["25 %"],
   ". Monte-Carlo over noisy three-star fixes confirms it:"}],

wlIn["SeedRandom[1];\n\
cnCockedHatProbability[{15., -40.}, DateObject[{2025, 6, 21, 22, 0, 0}, TimeZone -> 0],\n\
  {\"Star\", #} & /@ " <> ToString[starDemo["stars"], InputForm] <>
   ", <|\"sigmaMin\" -> 1.5|>, 2000]"],
outCell[chpDemo],
captionCell["Result: the empirical fraction of trials with the true position \
inside the cocked hat \[LongDash] close to 0.25, independent of the noise amplitude. \
A small cocked hat is therefore reassuring but NOT proof you are inside it."],

wlIn["sxFigCockedHatTheorem[]"],
renderFig["CockedHatTheorem", sxFigCockedHatTheorem[]],
captionCell["Figure 9.1  Monte-Carlo verification of the 25 % theorem \
(10 000 trials). The inside-fraction converges to 0.25 (dashed line) \
regardless of noise amplitude."],

hd2["9.2  Cram\[EAcute]r\[Dash]Rao Lower Bound"],

para[{"The Fisher information matrix from LOPs with azimuths {Zn\[UnderBracket]i} \
and sight noise \[Sigma] (arcmin = nm) is:"}],

displayMath[RowBox[{"F", "=", FractionBox["1", SuperscriptBox["\[Sigma]", "2"]],
   UnderoverscriptBox["\[Sum]", RowBox[{"i", "=", "1"}], "n"],
   SubscriptBox["u", "i"], SubsuperscriptBox["u", "i", "\[Transpose]"]}]],

para[{"The Cram\[EAcute]r\[Dash]Rao bound says no unbiased estimator beats F^{-1} in \
position variance. For three stars at 45\[Degree], 165\[Degree], 285\[Degree] (near-optimal \
120\[Degree] spacing) with 1' sigma:"}],

wlIn["cnErrorEllipse @ cnCRLBCovariance[{45., 165., 285.}, 1.0]"],
outCell[crlbDemo],
captionCell["Result: <|semiMajorNm, semiMinorNm, orientDeg, cep|>. \
With near-optimal geometry and 1' noise the CRLB predicts CEP \[TildeTilde] 0.8\[Dash]1.0 nm \[LongDash] \
this is the ", "RANDOM-NOISE FLOOR under a perfect ephemeris, not the real-world error."],

wlIn["sxFigCRLB[]"],
renderFig["CRLB", sxFigCRLB[]],
captionCell["Figure 9.2  CRLB covariance ellipse vs Monte-Carlo fix scatter \
(1' noise). The Monte-Carlo ellipse matches the CRLB, confirming the \
sight-reduction algorithm is near-optimal for random noise."],

wlIn["sxFigErrorEllipse[]"],
renderFig["ErrorEllipse", sxFigErrorEllipse[]],
captionCell["Figure 9.3  Error ellipses for different star-triplet geometries. \
Near-optimal 120\[Degree] spacing gives a round ellipse (CEP \[TildeTilde] 0.9 nm); \
clustered geometry gives an elongated ellipse whose worst axis exceeds 5 nm."],

hd2["9.3  Dilution of Precision"],

para[{"GDOP turns measurement noise into position error: \
\[Sigma]\[UnderBracket]pos = GDOP \[Times] \[Sigma]\[UnderBracket]sight. At the optimum (120\[Degree]) \
GDOP = \[Sqrt](4/3) \[TildeTilde] 1.155; a right-angle pair gives \[Sqrt]2 \[TildeTilde] 1.41; \
nearly parallel LOPs push GDOP above 10."}],

wlIn["sxFigCutAngle[]"],
renderFig["CutAngle", sxFigCutAngle[]],
captionCell["Figure 9.4  Fix error vs the acute crossing angle between two LOPs \
(1' noise). At 90\[Degree] the error is \[Sqrt]2 \[Times] the sight error; below ~20\[Degree] it blows up."],

noteBox[{bold["The honest floor."], " The ~1 nm CRLB/Monte-Carlo number is the \
", ital["random"], "-noise floor assuming a perfect ephemeris. Real fixes are \
dominated by ", bold["systematic"], " error \[LongDash] index error, the refraction \
model, almanac residuals \[LongDash] which does ", ital["not"], " average out over \
several sights. Real-world celestial fixes are typically 1\[Dash]3 nm, and worse in \
poor conditions. \[Section]12 makes this explicit."}],

hd2["9.4  The Full Picture: the Bayesian Position Posterior"],

para[{"Ellipses and GDOP summarise the uncertainty with a few numbers. The ",
   bold["honest, complete"], " object is the full posterior probability surface \
over position. With a flat prior the log-posterior at a candidate position x is \
just the sum of squared sight residuals, ",
   math["-\[Sum] \[LeftBracketingBar](H\[UnderBracket]o\[ThinSpace]-\[ThinSpace]H\[UnderBracket]c(x))/\[Sigma]\[RightBracketingBar]\.b2/2"],
   ", which ", mono["cnPosteriorGrid"], " evaluates on a grid. Its shape tells \
the whole story: ", bold["one"], " sight is a likelihood ridge (the LOP itself), \
", bold["two"], " sights a blob where the ridges cross, and ", bold["three"],
   " a tight peak on the truth. A constant uncorrected systematic (e.g. index \
error) does not blur the peak \[LongDash] it ", ital["moves"], " it off the true \
position, which is exactly the bias the cocked hat cannot see."}],

para[{"The MAP (maximum-a-posteriori) estimate for three clean Sun sights at \
the demo truth {20\[Degree]N, 40\[Degree]W}:"}],

wlIn["ts = DateObject[{2024, 11, 22, #, 0, 0}, TimeZone -> 0] & /@ {9, 12, 15};\n\
sights = Table[<|\"body\" -> \"Sun\", \"t\" -> ts[[k]],\n\
   \"Ho\" -> cnAltitudeFromGP[{20., -40.}, cnSunGP[ts[[k]]]]|>, {k, 3}];\n\
cnPosteriorGrid[sights, {{19.5, 20.5}, {-40.5, -39.5}}, 60, 1.5][\"mapEstimate\"]"],
outCell[mapPost],
captionCell["Result: {lat, lon} of the posterior peak \[LongDash] it lands on the true \
position {20\[Degree]N, 40\[Degree]W} to within the grid resolution, confirming three clean \
sights concentrate the posterior on the truth."],

wlIn["sxFigPosterior[]"],
renderFig["Posterior", sxFigPosterior[]],
captionCell["Figure 9.5  The Bayesian position posterior. One sight is a fuzzy \
BAND (the line of position as a likelihood ridge); two sights a BLOB; three a \
tight PEAK on the true position. The fourth panel bakes in a constant \
uncorrected index error: the peak stays tight but shifts OFF the truth \[LongDash] the \
honest, complete picture of why scatter is not accuracy."]

}];


(* ==============================================================
   \[Section]10  The longitude problem & lunar distance
   ============================================================== *)

writeAll[{

hd1["10.  The Longitude Problem & Lunar Distance"],

illusCell["illus_harrison_chronometer.png",
   "AI-generated illustration (OpenAI gpt-image-2) \[LongDash] for atmosphere, not a \
data figure. The clock solution: a Harrison-style marine chronometer. Keeping \
exact Greenwich time at sea turned longitude from an unsolved problem into a \
single subtraction."],

para[{ital["The 4-second rule."], " Earth rotates 360\[Degree] in 24 hours = \
15 arcmin per second. Since 1 arcmin of longitude at the equator = 1 nm, \
every second of chronometer error is 0.25 nm of longitude error at the equator \
(scaled by cos(lat)). This is exactly why Harrison's H4 mattered:"}],

wlIn["cnLongitudeErrorPerSecond[0.]"],
outCell[lonErrDemo],
captionCell["Result: 0.25 nm per second at the equator \[LongDash] 4 seconds = 1 nm. \
At latitude 45\[Degree] it is 0.25 cos(45\[Degree]) \[TildeTilde] 0.177 nm/s."],

wlIn["sxFigChronometer[]"],
renderFig["Chronometer", sxFigChronometer[]],
captionCell["Figure 10.1  Longitude error vs chronometer error at three \
latitudes. The equatorial slope is exactly 0.25 nm/s. H4 was good to ~0.2 s/day \
\[LongDash] comfortably inside the Longitude Board's 1-nm-per-day standard."],

para[{ital["Latitude is immune to clock error."], " At Local Apparent Noon the \
Sun transits the meridian at maximum altitude; only that maximum value is \
needed, not absolute time:"}],

displayMath[RowBox[{"lat", "=", RowBox[{"\[Delta]", "+",
   RowBox[{"(", RowBox[{"90", "\[Degree]", "-", SubscriptBox["H", "o"]}], ")"}]}],
   " ", "(", "Sun", " ", "bearing", " ", "South", ")"}]],

wlIn["cnNoonLatitude[50., -20., \"S\"]"],
outCell[noonLatDemo],
captionCell["Result: observer latitude. With Ho = 50\[Degree] and \[Delta] = \[Minus]20\[Degree], \
z = 40\[Degree] and lat = \[Delta] + z = 20\[Degree]N."],

wlIn["sxFigNoonSight[]"],
renderFig["NoonSight", sxFigNoonSight[]],
captionCell["Figure 10.2  Sun altitude vs UTC near Local Apparent Noon. \
The flat ~2-minute-wide peak limits noon-latitude precision to about \
\[PlusMinus]0.02\[Degree] (\[PlusMinus]1.2')."],

hd2["10.1  Longitude Without a Chronometer: Lunar Distance"],

illusCell["illus_lunar_distance.png",
   "AI-generated illustration (OpenAI gpt-image-2) \[LongDash] for atmosphere, not a \
data figure. The no-clock method: measuring the angular distance between the \
Moon and the Sun. The Moon drifts ~0.5\[Degree] per hour against the stars, so that \
angle is itself a slow clock in the sky."],

para[{"Before reliable chronometers, navigators found GMT from the ",
   bold["lunar distance"], " \[LongDash] the angle between the Moon and the Sun (or a \
star). The Moon moves ~0.5\[Degree]/hour against the background, so the Moon-Sun \
distance is a slow ", ital["clock in the sky"], ". The geocentric distance at a \
given instant:"}],

wlIn["cnLunarDistanceGeocentric[DateObject[{2024, 6, 13, 18, 0, 0}, TimeZone -> 0]]"],
outCell[dGeoDemo],
captionCell["Result: the center-of-Earth Moon-Sun angular distance in degrees."],

para[{"A real observation is topocentric and refracted; it must be ",
   bold["cleared"], " to the geocentric value before it can be used. Given the \
observed distance and the apparent Moon/Sun altitudes:"}],

wlIn["cnClearLunarDistance[" <> fnum[dObsDemo] <> ", " <> fnum[hMa] <> ", " <>
   fnum[hSa] <> ", cnMoonHP[DateObject[{2024, 6, 13, 18, 0, 0}, TimeZone -> 0]]]"],
outCell[dClrDemo],
captionCell["Result: the cleared (geocentric) distance \[LongDash] it should match \
cnLunarDistanceGeocentric above to within the sextant noise."],

para[{"Inverting the monotonic distance-vs-time relation recovers GMT:"}],

wlIn["cnLunarDistanceGMT[" <> fnum[dClrDemo] <>
   ", DateObject[{2024, 6, 13, 19, 30, 0}, TimeZone -> 0]]"],
outCell[tRecDemo],
captionCell["Result: the recovered UTC, near the true 18:00. In practice a 1' \
error in the cleared distance maps to ~2 minutes of time \[LongDash] about 0.5\[Degree] (~30 nm) \
of longitude. Lunars are far coarser than a chronometer (4 s = 1 nm) \[LongDash] which \
is precisely why Harrison's clock won."],

wlIn["sxFigLunarDistance[]"],
renderFig["LunarDistance", sxFigLunarDistance[]],
captionCell["Figure 10.3  Left: the Moon-Sun distance as a clock (~0.5\[Degree]/hr), \
inverted to recover GMT. Right: the resulting longitude error vs lunar-distance \
measurement error \[LongDash] tens of nautical miles, the historical reality."]

}];


(* ==============================================================
   \[Section]11  Crossing the Atlantic
   ============================================================== *)

writeAll[{

hd1["11.  Crossing the Atlantic"],

para[{"The package was tested on a simulated passage from Las Palmas de Gran \
Canaria to St Lucia, following a great-circle track at 5 knots with a simulated \
south-equatorial current. Daily Sun sights gave latitude (noon) and longitude \
(morning running fix); each evening twilight a best-GDOP star triplet was \
reduced."}],

With[{nm = totalNm},
   para[{"The passage covered approximately ", bold[ToString[Round[nm, 1]]],
      " nm over ", bold[ToString[nDays]], " days. Key results:"}]],

para[{"\[Bullet] ", bold["Daily celestial fix:"], " mean error vs GPS \[TildeTilde] 1\[Dash]2 nm."}],
para[{"\[Bullet] ", bold["Dead reckoning:"], " left uncorrected, the DR position \
drifts to ", bold["111 nm"], " off truth by landfall \[LongDash] ", bold["45 nm RMS"], " over \
the crossing \[LongDash] which each celestial fix pulls back to ~1\[Dash]2 nm."}],
para[{"\[Bullet] ", bold["Worst day:"], " a few nm, on a day with poor star \
geometry at twilight and modest sextant noise."}],

wlIn["sxFigSpeeds[]"],
renderFig["Speeds", sxFigSpeeds[]],
captionCell["Figure 11.1  Speed over ground and velocity made good toward the \
destination. The south-equatorial current adds up to ~1.5 kt of favourable push \
in the trade-wind zone."],

wlIn["sxFigCelestialVsGPS[]"],
renderFig["CelestialVsGPS", sxFigCelestialVsGPS[]],
captionCell["Figure 11.2  Daily celestial fix error vs GPS truth. \
Running-fix and evening-star fixes stay within a couple of nm; DR error grows \
steadily between fixes."],

wlIn["sxFigErrorBudget[]"],
renderFig["ErrorBudget", sxFigErrorBudget[]],
captionCell["Figure 11.3  Error-budget dashboard. Sextant reading noise (~1') \
dominates; refraction uncertainty adds a few tenths; chronometer error is \
negligible for a modern watch. The irreducible random floor is ~1 nm \[LongDash] but \
systematic error (\[Section]12) is what bites in practice."]

}];


(* ==============================================================
   \[Section]12  Recursive estimation & the limits of accuracy
   ============================================================== *)

writeAll[{

hd1["12.  Recursive Estimation & the Limits of Accuracy"],

para[{"A navigator does not throw away yesterday's fix. The natural framework \
is recursive Bayesian estimation: an ", bold["Extended Kalman Filter"],
   " (EKF) carries one evolving position estimate with a covariance. Each day it ",
   ital["predicts"], " forward along the dead-reckoning increment (inflating the \
covariance by process noise), then ", ital["updates"], " on each Sun sight \
(the Marcq St\[Dash]Hilaire innovation Ho \[Minus] Hc, with measurement Jacobian \
{sin Zn, cos Zn}). Running the filter over the whole voyage:"}],

wlIn["voyage = (* per-day <|\"day\",\"t\",\"truePos\",\"drPos\"|> from voyage.csv *);\n\
recs = cnEKFVoyage[voyage, 3, <|\"seed\" -> 42, \"sigmaMin\" -> 1.0|>];\n\
KeyTake[recs[[-1]], {\"day\", \"estPos\", \"errVsTruthNm\", \"drErrorNm\"}]"],
outCell[ekfDemo],
captionCell["Result: the final-day record. Over the whole crossing the EKF holds \
a 1.5 nm RMS error (0.76 nm median) while unaided DR reaches 45 nm RMS \
(111 nm by landfall) \[LongDash] a 30\[Times] improvement, fusing both into one \
shrinking-covariance estimate. (The record above is the arrival-maneuver day, \
the EKF's worst at 5.1 nm, as the DR predict overshoots the final turn.)"],

wlIn["sxFigEKF[]"],
renderFig["EKF", sxFigEKF[]],
captionCell["Figure 12.1  The EKF running fix. Left: truth, DR, and EKF tracks \
with the covariance ellipse collapsing from its large prior as sights arrive. \
Right: the systematic-vs-random decomposition (below)."],

hd2["12.1  A Tight Cocked Hat Is NOT Proof of Accuracy"],

para[{"This is the central honesty point. A ", bold["systematic"], " error \[LongDash] a \
constant index error, a biased refraction model, an almanac offset \[LongDash] shifts \
the ", ital["centre"], " of every sight together. It moves the whole fix \
without enlarging its scatter, so the cocked hat stays small and the navigator \
feels confident while being wrong. With a 3' index error left uncorrected:"}],

wlIn["SeedRandom[7];\n\
cnSystematicFixError[{15., -40.}, DateObject[{2025, 6, 21, 22, 0, 0}, TimeZone -> 0],\n\
  {\"Star\", #} & /@ " <> ToString[starDemo["stars"], InputForm] <> ", 1.0, 3.0, 500]"],
outCell[sysDemo],
captionCell["Result: biasNm is the systematic offset of the fix centre from \
truth (several nm, driven by the 3' index error), while scatterTraceNm2 \[LongDash] the \
spread \[LongDash] is unchanged from the zero-bias case. The cocked hat measures \
scatter, not bias: a tight hat does not prove accuracy. This is why real \
celestial fixes run 1\[Dash]3 nm despite a ~1 nm random floor."],

noteBox[{bold["Random vs systematic."], " Averaging many sights drives the ",
   ital["random"], " error toward the ~1 nm CRLB floor, but leaves the ",
   ital["systematic"], " error untouched. Honest accuracy claims must report \
both \[LongDash] the floor (\[Section]9) and the bias (here)."}]

}];


(* ==============================================================
   \[Section]13  Validation, and a real voyage
   ============================================================== *)

writeAll[{

hd1["13.  Validation, and a Real Voyage"],

para[{"How do we know the engine is right? First, ", bold["offline ephemeris \
validation"], ": the Sun altitude our engine predicts (from ", mono["cnSunGP"],
   " + ", mono["cnAltitudeFromGP"], ") is compared to Wolfram's independent ",
   mono["SunPosition"], " (a VSOP ephemeris), back-corrected to geometric \
altitude. For 21 Jun 2024 at 55\[Degree]N 5\[Degree]W:"}],

wlIn["t = DateObject[{2024, 6, 21, 12, 0, 0}, TimeZone -> 0]; loc = {55., -5.};\n\
ourAlt = cnAltitudeFromGP[loc, cnSunGP[t]];\n\
wGeom  = QuantityMagnitude[UnitConvert[SunPosition[GeoPosition[loc], t][[2]], \"AngularDegrees\"]]\n\
         - cnRefraction[#]/60. &[\n\
         QuantityMagnitude[UnitConvert[SunPosition[GeoPosition[loc], t][[2]], \"AngularDegrees\"]]];\n\
(ourAlt - wGeom)*60.  (* residual in arcminutes *)"],
outCell[ephResDemo],
captionCell["Result: the Sun-altitude residual in arcminutes \[LongDash] a fraction of an \
arcminute (~0.14' typical), well inside the ~1' sextant tolerance. \
This check is offline; external JPL Horizons / Hipparcos comparisons are \
optional network scripts."],

wlIn["sxFigEphemerisValidation[]"],
renderFig["EphemerisValidation", sxFigEphemerisValidation[]],
captionCell["Figure 13.1  Offline validation. Panel A: Sun-altitude residual \
vs Wolfram SunPosition across 2024 at three latitudes (sub-arcminute). \
Panels B/C: star proper motion and precession magnitudes over the catalogue \
epoch, justifying the IAU-1976 precession applied to date."],

hd2["13.1  A Real Voyage: the James Caird (1916)"],

illusCell["illus_james_caird.png",
   "AI-generated illustration (OpenAI gpt-image-2) \[LongDash] for atmosphere, not a \
data figure. Shackleton's James Caird: a 6.9 m open boat crossing 800 \
nm of the Southern Ocean to South Georgia in 1916, navigated by sextant in \
conditions that allowed only four sights in sixteen days."],

para[{"As a capstone we reconstruct Shackleton's open-boat voyage of the ",
   ital["James Caird"], " from Elephant Island to South Georgia (Apr\[Dash]May 1916), \
one of the great feats of practical navigation. A worked Sun reduction at the \
mid-passage assumed position {57\[Degree]S, 46\[Degree]W}:"}],

wlIn["cnComputedAltitude[{-57.0, -46.0}, cnSunGP[DateObject[{1916, 5, 2, 15, 0, 0}, TimeZone -> 0]]]"],
outCell[{hcHist, znHist}],
captionCell["Result: {H\[UnderBracket]c \[Degree], Zn \[Degree]} for the reduction, cross-checked \
in the figure against an independent 3-D vector formula (exact) and against \
Wolfram SunPosition (< 0.5')."],

noteBox[{bold["This is a reconstruction, not a logbook."], " We use the \
documented endpoints and dates with simulated sights (open-boat-realistic 2' \
sextant noise) \[LongDash] not Worsley's original readings, which do not survive in \
reproducible form. It demonstrates the method on a real track, not a claim \
about the historical fixes themselves."}],

wlIn["sxFigHistorical[]"],
renderFig["Historical", sxFigHistorical[]],
captionCell["Figure 13.2  The James Caird reconstruction. A worked reduction \
validated two ways (exact 3-D vector formula; Wolfram SunPosition to < 0.5'), \
and the simulated Elephant Island \[Rule] South Georgia track with daily Sun fixes \
against the great-circle truth."]

}];


(* ==============================================================
   \[Section]14  Explore it yourself \[LongDash] interactive navigation
   ============================================================== *)

writeAll[{

hd1["14.  Explore It Yourself \[LongDash] Interactive Navigation"],

para[{"Each ", mono["sxManip...[]"], " cell below builds a ", bold["live"], " ",
   mono["Manipulate"], " from ", mono["SextantInteractive.wl"], ". Opened in \
Mathematica (with the packages loaded from the Setup section), its sliders and \
menus are fully interactive \[LongDash] drag a control and the geometry recomputes \
in real time. A static PDF cannot run that dynamics, so below each call we show \
a ", bold["static preview"], " \[LongDash] the figure exactly as the Manipulate \
draws it at its default settings. Evaluate the call above each preview in \
Mathematica to bring the controls to life."}],

hd2["14.1  The Intercept Method, Live"],

para[{"Drag the assumed position and the observed altitude H\[UnderBracket]o; watch \
the computed altitude, the intercept (Toward/Away), the azimuth, and the \
resulting line of position update against a fixed true position."}],

wlIn["sxManipIntercept[]"],
manipPreviewCell[manipInterceptPreviewImg],
captionCell["Static preview \[LongDash] evaluate the cell above in Mathematica for \
the live interactive version. The Marcq St\[Dash]Hilaire intercept method: as \
H\[UnderBracket]o crosses H\[UnderBracket]c the intercept flips from Toward to \
Away and the LOP slides through the assumed position."],

hd2["14.2  The Bayesian Posterior, Live"],

para[{"Slide the number of sights and the sextant noise \[Sigma], and toggle an \
uncorrected index error. Watch the posterior sharpen from a band (one sight) to \
a blob (two) to a tight peak (three) \[LongDash] and watch the index-error toggle push \
the peak off the truth without widening it."}],

wlIn["sxManipPosterior[]"],
manipPreviewCell[manipPosteriorPreviewImg],
captionCell["Static preview \[LongDash] evaluate the cell above in Mathematica for \
the live interactive version. The full Bayesian posterior of \[Section]9.4 under \
your control. The MAP-to-truth error is reported above the heatmap; the \
index-error toggle is the honest demonstration that a tight peak is not a true \
one."],

hd2["14.3  Sight Planning by GDOP, Live"],

para[{"Choose three visible stars and slide the observation time; the cut-angle \
geometry, the scalar GDOP, and the Cram\[EAcute]r\[Dash]Rao 95 % error ellipse update \
live \[LongDash] experimental design for a star fix, before you ever pick up the \
sextant."}],

wlIn["sxManipSightPlanning[]"],
manipPreviewCell[manipPlanningPreviewImg],
captionCell["Static preview \[LongDash] evaluate the cell above in Mathematica for \
the live interactive version. GDOP and the CRLB ellipse as a planning tool. \
Well-separated stars (~120\[Degree] apart) give a round, small ellipse; \
clustered choices balloon it along the shared bearing."]

}];


(* ==============================================================
   \[Section]15  Conclusions
   ============================================================== *)

writeAll[{

hd1["15.  Conclusions"],

para[{"Celestial navigation needs four things: a sextant (\[TildeTilde]\$500), a clock \
good to a few seconds (a quartz watch suffices), an almanac (or the formulae in \
this package), and spherical trigonometry. Together they give a 1\[Dash]3 nm fix \
anywhere on Earth, immune to satellite jamming and spoofing, requiring no \
infrastructure whatsoever."}],

para[{"The mathematics is surprisingly deep. A single observation is a circle \
on the sphere; two circles intersect to give a fix; the quality depends on the \
angle between them (GDOP); the random uncertainty satisfies an exact \
Cram\[EAcute]r\[Dash]Rao bound; three-LOP geometry hides the 25 % cocked-hat surprise; \
clock sensitivity is one short formula; and a Kalman filter fuses it all over a \
voyage. The crucial honest caveat: a tight cocked hat bounds ", ital["scatter"],
   ", not ", ital["bias"], " \[LongDash] systematic error is what limits real accuracy."}],

para[{"GPS won on precision (5 m vs ~1 nm) and convenience, but the mathematics \
of celestial navigation is timeless. Every offshore licence still requires it, \
every long-distance race permits it as the only sanctioned backup, and every \
navigation text since 1875 derives it from the same PZX triangle. \
The implementation here runs offline, computes a complete voyage, and shows its \
working above every result."}]

}];


(* ==============================================================
   REFERENCES
   ============================================================== *)

writeAll[{

hd1["References"],

para[{bold["Bowditch, N."], " (2002). ",
   ital["The American Practical Navigator"], ". \
US National Imagery and Mapping Agency."}],

para[{bold["H.M. Nautical Almanac Office / USNO"], ". ", ital["The Nautical Almanac"],
   " (annual). HMSO / Government Printing Office."}],

para[{bold["Meeus, J."], " (1998). ", ital["Astronomical Algorithms"],
   ", 2nd ed. Willmann-Bell. Low-precision solar coordinates, the abridged ELP \
lunar theory (ch. 47), precession, and the equation of time."}],

para[{bold["Sumner, T. H."], " (1843). ",
   ital["A New and Accurate Method of Finding a Ship's Position at Sea"],
   ". Boston. The circle of equal altitude."}],

para[{bold["Marcq St\[Dash]Hilaire, A."], " (1875). Calcul du point \
observ\[EAcute]. ", ital["Revue Maritime et Coloniale"], " 45, 307\[Dash]341. \
The intercept method."}],

para[{bold["Daniels, G. J."], " (1951). The probability of a position \
inside the cocked hat. ", ital["Journal of the Institute of Navigation"],
   " 4, 68\[Dash]69. The 25 % theorem."}],

para[{bold["Lieske, J. H., Lederle, T., Fricke, W., & Morando, B."],
   " (1977). Expressions for the precession quantities (IAU 1976). ",
   ital["Astronomy & Astrophysics"], " 58, 1\[Dash]16. Used in ", mono["cnPrecess"], "."}],

para[{bold["Bennett, G. G."], " (1982). The calculation of astronomical \
refraction in marine navigation. ", ital["Journal of Navigation"],
   " 35, 255\[Dash]259. Used in ", mono["cnRefraction"], "."}],

para[{bold["Saemundsson, T."], " (1986). Atmospheric refraction. ",
   ital["Sky & Telescope"], " 72, 70. Used in ", mono["cnRefractionSaemundsson"], "."}]

}];


(* ==============================================================
   ACKNOWLEDGEMENTS
   ============================================================== *)

writeAll[{

hd1["Acknowledgements"],

para[{ital["Data sources."], " All data are public-domain or built-in and \
self-contained:"}],

para[{"\[Bullet] ", bold["Wolfram Language built-ins:"], " ", mono["SunPosition"],
   " for independent verification; ", mono["JulianDate"], ", ",
   mono["GeoDistance"], " for geometry. No external services are queried during \
evaluation."}],

para[{"\[Bullet] ", bold["Solar & lunar ephemeris:"], " low-precision solar \
coordinates (Astronomical Almanac) in ", mono["cnSunGP"], " and the abridged \
ELP lunar theory in ", mono["cnMoonPosition"], " (~0.5' accuracy), validated \
offline against Wolfram ", mono["SunPosition"], "."}],

para[{"\[Bullet] ", bold["57 navigational stars:"], " J2000.0 RA, declination, \
and magnitude from the standard catalogue (public domain), loaded from ",
   mono["data/nav_stars.csv"], "."}],

para[{"\[Bullet] ", bold["Simulated voyage & James Caird reconstruction:"],
   " generated offline; documented endpoints and dates with simulated sights, \
not original logbook data."}],

para[{ital["Tools."], " Every ", bold["data figure and computed result"],
   " is pure ", bold["Wolfram Language"], ", in three packages (",
   mono["CelestialNavigation.wl"], ", ", mono["SextantFigures.wl"], ", ",
   mono["SextantInteractive.wl"], "). The notebook was assembled \
programmatically via ", mono["wolframscript"], "."}],

para[{ital["Illustrative art."], " The five atmospheric section-opening images \
(the navigator, the celestial dome, the chronometer, the lunar-distance sight, \
and the ", ital["James Caird"], ") are ", bold["AI-generated illustrations \
created with OpenAI's gpt-image-2"], ". They are decorative only and carry no \
data \[LongDash] every scientific figure in this post is computed in Mathematica and is \
shown above with the exact function call that produced it."}]

}];


(* ==============================================================
   REPRODUCIBILITY
   ============================================================== *)

writeAll[{

hd1["Reproducibility"],

para[{"All source code is in the public repository at ",
   mono["github.com/mthiel74/SextantNavigation"],
   ". To rebuild from scratch on any machine with Wolfram Language:"}],

codeIn["# Rebuild all figures (offline)
wolframscript -file wolfram/run_all.wls

# Rebuild this notebook (Gets both packages, evaluates all 27 figures live)
wolframscript -file community/build_notebook.wls"],

para[{"Network scripts (", mono["wolfram/fetch_horizons.wls"], ", ",
   mono["wolfram/fetch_stars.wls"], ", ", mono["wolfram/validate_all.wls"],
   ") are optional and clearly marked; the full pipeline runs offline."}],

(* StyleBox (NOT Style): a box-level primitive belongs inside TextData. A raw
   Style[string, opts] here makes CreateDocument promote the cell and orphan its
   "Text" style + CellMargins option into two stray Input cells (the "Text" line
   + empty red box that appeared at the foot of this section). *)
para[{StyleBox[
   "Generated on " <> DateString[Now, {"DayName", ", ", "MonthName", " ",
      "DayShort", ", ", "Year"}] <> " with CelestialNavigation v" <> cnVersion[] <>
      " / " <> sxFigVersion[] <> ".",
   FontSlant -> Italic, FontColor -> GrayLevel[0.4]]}]

}];


(* ==============================================================
   VISUAL GALLERY \[LongDash] three hero animations
   ============================================================== *)

writeAll[{

hd1["Visual Gallery"],

para[{"Three animated views of the same idea accompany this post. The \
rotating globe opened it (top); the voyage-replay and the sextant-sight replay \
complete the picture."}],

animCell["hero_chart.gif",
   "Voyage replay. Dead-reckoning (DR) track, daily celestial fixes, and GPS \
truth for a simulated Las Palmas \[Rule] St Lucia passage \[LongDash] the celestial \
fixes stay within ~2 nm of GPS while DR drifts steadily off course."],

animCell["hero_sextant.gif",
   "Sextant-sight replay. Each frame shows one simulated observation: the \
sextant reading H\[UnderBracket]s, the five corrections to obtain H\[UnderBracket]o, \
the LOP on a local chart, and the running position estimate, across the full \
passage."]

}];


(* ====================================================================
   Assemble and save the notebook
   ==================================================================== *)

Print["Assembled ", Length[$allCells], " cells total."];

customStyleDefs = Notebook[{
   Cell[StyleData[StyleDefinitions -> "Default.nb"]],
   Cell[StyleData["Section"], FontFamily -> "Roboto Condensed",
      FontColor -> RGBColor[0.153, 0.51, 0.64], FontSize -> 22, FontWeight -> "Bold"],
   Cell[StyleData["Subsection"], FontFamily -> "Roboto Condensed",
      FontColor -> RGBColor[0.30, 0.57, 0.66], FontSize -> 16, FontWeight -> "Bold"],
   (* Title: teal-blue in Roboto Condensed, matching the reference post
      (Default.nb's Title renders red \[LongDash] override it explicitly). *)
   Cell[StyleData["Title"], FontFamily -> "Roboto Condensed",
      FontColor -> RGBColor[0.153, 0.51, 0.64], FontSize -> 30, FontWeight -> "Bold"]
}, Visible -> False];

Print["Saving notebook via UsingFrontEnd..."];
TimeConstrained[
   UsingFrontEnd[
      nbObj = CreateDocument[$allCells, Visible -> False,
         WindowSize -> {980, 800}, StyleDefinitions -> customStyleDefs];
      NotebookSave[nbObj, outNb];
      Print["wrote ", outNb, " (", FileByteCount[outNb], " B)"];
      Quiet @ Check[
         Export[outPdf, nbObj];
         Print["wrote ", outPdf, " (", FileByteCount[outPdf], " B)"],
         Print["warn: PDF export returned an error (notebook saved OK)"]];
      NotebookClose[nbObj]
   ],
   1200,
   Print["warn: UsingFrontEnd timed out after 1200 s"]
];

(* ====================================================================
   Verification pass
   ==================================================================== *)

Print["--- Verification ---"];
If[FileExistsQ[outNb],
   Module[{nb2, cells2, secTitles, inputCells, figCalls, cnCalls},
      nb2    = Import[outNb, "NB"];
      cells2 = Cases[nb2, Cell[_, ___], Infinity];
      secTitles = Cases[nb2, Cell[t_, "Section", ___] :> t, Infinity];
      inputCells = Cases[nb2, Cell[code_String, "Input", ___] :> code, Infinity];
      figCalls = Select[inputCells, StringContainsQ[#, "sxFig"] &];
      cnCalls  = Select[inputCells, StringContainsQ[#, "cn"] && ! StringContainsQ[#, "sxFig"] &];
      Print["  .nb cell count: ", Length[cells2]];
      Print["  Input cells: ", Length[inputCells],
         " (sxFig figure calls: ", Length[figCalls],
         ", cn computation cells: ", Length[cnCalls], ")"];
      Print["  sxManip interactive call cells: ",
         Length @ Select[inputCells, StringContainsQ[#, "sxManip"] &]];
      Print["  AI illustration captions present (gpt-image-2): ",
         Length @ Select[
            Cases[nb2, c : Cell[_, "Text", ___] :> ToString[c, InputForm], Infinity],
            StringContainsQ[#, "gpt-image-2"] &]];
      Print["  sxFigPosterior call present: ",
         AnyTrue[inputCells, StringContainsQ[#, "sxFigPosterior"] &]];
      Print["  Section titles (", Length[secTitles], "):"];
      MapIndexed[Print["    ", #2[[1]], ". ", #1] &, secTitles];
      If[FileExistsQ[outPdf],
         Print["  .pdf size: ", FileByteCount[outPdf], " B"],
         Print["  .pdf NOT found"]];
   ],
   Print["  ERROR: .nb not found at ", outNb]
];

Print["=== build_notebook.wls done in ", Round[AbsoluteTime[] - t0Build, 1], " s ==="];
