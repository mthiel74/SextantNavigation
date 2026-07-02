(* ::Package:: *)
(* SextantFigures.wl -- Companion package wrapping every SextantNavigation     *)
(* figure/visualization generator as a callable function returning a           *)
(* Graphics/Graphics3D/GeoGraphics/Image/Column/... expression (no PNG export).*)
(*                                                                             *)
(* Canonical Wolfram Community pattern: a one-liner CALL above each figure.    *)
(* All figure code lives here; the standalone wolfram/*.wls scripts remain for *)
(* batch PNG regeneration. This package is the source of truth for the figure  *)
(* logic.                                                                      *)
(*                                                                             *)
(* Each sxFig<Name>[] is deterministic (SeedRandom where the original used     *)
(* randomness) and RETURNS the figure expression; the notebook builder sizes   *)
(* them. Data-file paths resolve via the package directory so Get works from   *)
(* anywhere (including the notebook FrontEnd).                                  *)

$CharacterEncoding = "UTF-8";

(* Ensure the engine is available before BeginPackage's Needs runs, so the    *)
(* package Gets from anywhere (incl. a notebook FrontEnd) without the caller   *)
(* having to pre-load CelestialNavigation.wl. Loads the sibling engine file by *)
(* absolute path (captured from $InputFileName) when its context is absent.    *)
If[!MemberQ[$Packages, "CelestialNavigation`"],
  Get[FileNameJoin[{DirectoryName[$InputFileName], "CelestialNavigation.wl"}]]
];

BeginPackage["SextantFigures`", {"CelestialNavigation`"}];

sxFigVersion::usage = "sxFigVersion[] gives the SextantFigures package version string.";

(* --- figures.wls (9) --- *)
sxFigAltitudeCircle::usage     = "sxFigAltitudeCircle[] returns the circle-of-equal-altitude GeoGraphics (fig_altitude_circle).";
sxFigIntercept::usage          = "sxFigIntercept[] returns the Marcq St-Hilaire intercept-method schematic Graphics (fig_intercept).";
sxFigCockedHat::usage          = "sxFigCockedHat[] returns the three-star cocked-hat fix Graphics from the real twilight catalogue (fig_cocked_hat).";
sxFigRunningFix::usage         = "sxFigRunningFix[] returns the running-fix (transferred position line) Graphics (fig_running_fix).";
sxFigCorrectionsWaterfall::usage = "sxFigCorrectionsWaterfall[] returns the Hs->Ho altitude-correction cascade Graphics (fig_corrections_waterfall).";
sxFigSubsolarTrack::usage      = "sxFigSubsolarTrack[] returns the sub-solar track + analemma GraphicsRow (fig_subsolar_track).";
sxFigErrorEllipse::usage       = "sxFigErrorEllipse[] returns the Monte-Carlo fix distribution + 95% error ellipse Graphics (fig_error_ellipse).";
sxFigCutAngle::usage           = "sxFigCutAngle[] returns the fix-accuracy vs LOP cut-angle plot (fig_cut_angle).";
sxFigCelestialVsGPS::usage     = "sxFigCelestialVsGPS[] returns the celestial-vs-GPS accuracy DateListPlot (fig_celestial_vs_gps).";

(* --- single-figure scripts (18) --- *)
sxFigPZXTriangle::usage        = "sxFigPZXTriangle[] returns the navigational (PZX) triangle Graphics3D (fig_pzx_triangle).";
sxFigCelestialSphere::usage    = "sxFigCelestialSphere[] returns the celestial-sphere coordinate-systems figure (fig_celestial_sphere).";
sxFigEquationOfTime::usage     = "sxFigEquationOfTime[] returns the equation-of-time figure (fig_equation_of_time).";
sxFigHorizonDip::usage         = "sxFigHorizonDip[] returns the horizon-dip figure (fig_horizon_dip).";
sxFigRefraction::usage         = "sxFigRefraction[] returns the atmospheric-refraction figure (fig_refraction).";
sxFigCockedHatTheorem::usage   = "sxFigCockedHatTheorem[] returns the cocked-hat 25% theorem figure (fig_cocked_hat_theorem).";
sxFigCRLB::usage               = "sxFigCRLB[] returns the Cramer-Rao lower bound figure (fig_crlb).";
sxFigChronometer::usage        = "sxFigChronometer[] returns the chronometer / longitude-error figure (fig_chronometer).";
sxFigNoonSight::usage          = "sxFigNoonSight[] returns the noon-sight (meridian altitude) figure (fig_noon_sight).";
sxFigStarSelection::usage      = "sxFigStarSelection[] returns the star-selection / GDOP figure (fig_star_selection).";
sxFigStarChart::usage          = "sxFigStarChart[] returns the star-chart figure (fig_star_chart).";
sxFigErrorBudget::usage        = "sxFigErrorBudget[] returns the error-budget figure (fig_error_budget).";
sxFigSpeeds::usage             = "sxFigSpeeds[] returns the speeds / signal-propagation figure (fig_speeds).";
sxFigTwilightReplay::usage     = "sxFigTwilightReplay[] returns the twilight-replay figure (fig_twilight_replay).";
sxFigEphemerisValidation::usage = "sxFigEphemerisValidation[] returns the offline ephemeris-validation figure (fig_ephemeris_validation).";
sxFigLunarDistance::usage      = "sxFigLunarDistance[] returns the lunar-distance method figure (fig_lunar_distance).";
sxFigEKF::usage                = "sxFigEKF[] returns the EKF recursive-Bayesian running-fix figure (fig_ekf).";
sxFigHistorical::usage         = "sxFigHistorical[] returns the historical-sight (James Caird) reconstruction figure (fig_historical).";

(* --- Bayesian posterior (R3) --- *)
sxFigPosterior::usage          = "sxFigPosterior[] returns the Bayesian position-posterior figure (fig_posterior): a four-panel light-theme heatmap row showing the normalised posterior probability surface after 1 sight (a fuzzy BAND = the line of position as a likelihood ridge), 2 sights (a BLOB), and 3 sights (a tight PEAK on the true position), plus a fourth panel where a constant uncorrected index error shifts the peak OFF the true position (honest systematic bias). True position and MAP estimate are marked on each panel.";

Begin["`Private`"];

(* Capture the package directory ONCE at load time (mirrors cnPackageDir). *)
sxPkgDir = DirectoryName[$InputFileName];

sxFigVersion[] := "SextantFigures 1.0";

(* Data-file path resolver: data/ is a sibling of wolfram/. *)
sxDataFile[name_String] := FileNameJoin[{sxPkgDir, "..", "data", name}];

(* JSON -> Association normalizer (identical to figures.wls fixJSON). *)
fixJSON[x_List] := If[
  Length[x] > 0 && AllTrue[x, MatchQ[#, _Rule] &],
  Association[fixJSON /@ x], fixJSON /@ x];
fixJSON[x_Rule] := Rule[x[[1]], fixJSON[x[[2]]]];
fixJSON[x_]     := x;

(* Voyage data -- loaded once (memoized). *)
sightsData := sightsData = fixJSON[Import[sxDataFile["sights.json"], "JSON"]];
daysData   := daysData   = sightsData["days"];
twData     := twData     = fixJSON[Import[sxDataFile["twilight_fixes.json"], "JSON"]];

(* Render width shared across the figures.wls-derived generators.
   Reduced 1600 -> 860 so the 11-15 pt fonts stay legible when the PNG is
   embedded at ~620 px in the notebook (font-to-width ratio ~2x larger). *)
imgW = 860;

(* Theme colour palettes (verbatim from figures.wls). *)
themeColors = <|
  "light" -> <|
    "bg"     -> White,
    "text"   -> RGBColor[0.13, 0.29, 0.53],
    "accent" -> RGBColor[0.25, 0.45, 0.95],
    "navy"   -> RGBColor[0.13, 0.29, 0.53],
    "warm"   -> RGBColor[0.85, 0.50, 0.20],
    "mid"    -> GrayLevel[0.45],
    "grid"   -> Directive[GrayLevel[0.88], Dashed],
    "frame"  -> GrayLevel[0.85],
    "green"  -> RGBColor[0.20, 0.68, 0.35]
  |>,
  "dark" -> <|
    "bg"     -> RGBColor[0.07, 0.09, 0.13],
    "text"   -> GrayLevel[0.90],
    "accent" -> RGBColor[0.40, 0.65, 1.00],
    "navy"   -> GrayLevel[0.85],
    "warm"   -> RGBColor[1.00, 0.65, 0.30],
    "mid"    -> GrayLevel[0.60],
    "grid"   -> Directive[GrayLevel[0.22], Dashed],
    "frame"  -> GrayLevel[0.28],
    "green"  -> RGBColor[0.30, 0.85, 0.45]
  |>
|>;

$tcL = themeColors["light"];
$tcD = themeColors["dark"];

(* Style helpers (verbatim from figures.wls). *)
titleDir[tc_] := Directive[FontFamily -> "Helvetica", FontSize -> 17,
                            Bold, FontColor -> tc["text"]];
labelDir[tc_] := Directive[FontFamily -> "Helvetica", FontSize -> 15,
                            FontColor -> tc["text"]];

(* ==========================================================================
   FIGURE GENERATORS -- return Graphics/GeoGraphics expressions
   ========================================================================== *)

(* ── Fig 1: Altitude Circle ─────────────────────────────────────────────── *)
(* globeStyle: "relief" (offline default), "tiled" (network OK), "dark"     *)
makeFig1[tc_, globeStyle_:"relief"] :=
  Module[{t, gp, lat, lon, H, radiusNm, geoCirc, gpPos, obsPos, geoBg, bgColor},
    t        = DateObject[{2024, 11, 22, 12, 0, 0}, TimeZone -> 0];
    gp       = cnSunGP[t];
    lat = gp[[1]];  lon = gp[[2]];
    H        = cnAltitudeFromGP[{35.0, -15.0}, gp];
    radiusNm = (90.0 - H) * 60;
    geoCirc  = GeoCircle[GeoPosition[{lat, lon}],
                 Quantity[radiusNm, "NauticalMiles"]];
    gpPos  = GeoPosition[{lat, lon}];
    obsPos = GeoPosition[{35.0, -15.0}];

    (* Globe background:
       "relief" (default): CountryBorders on blue ocean -- offline after 1st-run entity cache
       "dark":  CountryBorders on dark navy background -- same cache
       "tiled": StreetMapNoLabels tiles (needs network; renders best-effort if blocked)
       GeoStyling["LandMask"] was tried but makes a failing GeoServer call in this env,
       so we use "CountryBorders" which caches its entity metadata locally after first use. *)
    {geoBg, bgColor} = Switch[globeStyle,
      "dark",  {"CountryBorders", RGBColor[0.04, 0.09, 0.19]},
      "tiled", {GeoStyling["StreetMapNoLabels"], White},
      _,       {"CountryBorders", RGBColor[0.67, 0.82, 0.92]}  (* blue ocean background *)
    ];

    GeoGraphics[
      {
        {EdgeForm[{tc["warm"], Thickness[0.003]}],
         FaceForm[Opacity[0.14, tc["warm"]]], geoCirc},
        {tc["warm"],   Thickness[0.005], geoCirc},
        {tc["warm"],   PointSize[0.032], Point[gpPos]},
        {tc["accent"], PointSize[0.022], Point[obsPos]}
      },
      GeoProjection -> {"Orthographic", "Centering" -> gpPos},
      GeoBackground -> geoBg,
      GeoRange      -> "World",
      ImageSize     -> {imgW, Round[imgW * 0.65]},
      Background    -> bgColor,
      PlotLabel     -> Style[
        "Circle of Equal Altitude \[LongDash] You Are Somewhere on This Circle",
        titleDir[tc]],
      (* Legend drawn as Text[] primitives (bullet glyphs + labels) at Scaled
         positions.  Inset[Framed[Column[...]]] of typeset content triggers a
         macOS WL rasteriser bug that paints a stray pink/red rectangle, so the
         boxed legend is rebuilt from Text with a translucent background halo. *)
      Epilog -> {
        Text[Style[Row[{Style["\[FilledCircle]  ", tc["warm"]],
                        "Sub-solar point (GP) \[LongDash] Sun overhead here"}],
                   FontFamily -> "Helvetica", FontSize -> 15,
                   FontColor -> tc["text"],
                   Background -> Directive[Opacity[0.82], bgColor]],
             Scaled[{0.015, 0.115}], {Left, Center}],
        Text[Style[Row[{Style["\[FilledCircle]  ", tc["accent"]],
                        "Observer reference (35\[Degree]N 15\[Degree]W)"}],
                   FontFamily -> "Helvetica", FontSize -> 15,
                   FontColor -> tc["text"],
                   Background -> Directive[Opacity[0.82], bgColor]],
             Scaled[{0.015, 0.065}], {Left, Center}],
        Text[Style[Row[{Style["\[EmptyCircle]  ", tc["warm"]],
                        "Circle of equal altitude \[LongDash] " <>
                        ToString[Round[radiusNm]] <> " nm radius"}],
                   FontFamily -> "Helvetica", FontSize -> 15,
                   FontColor -> tc["text"],
                   Background -> Directive[Opacity[0.82], bgColor]],
             Scaled[{0.015, 0.015}], {Left, Center}]
      }
    ]
  ];

(* ── Fig 2: Intercept Method Schematic ─────────────────────────────────── *)
(* A2 FIX: paleBlue Rectangle removed -- white Background is the backdrop   *)
makeFig2[tc_] :=
  Module[{znDeg, znRad, gpDir, lopDir, apPt, interPt, lopLine},
    znDeg   = 55;
    znRad   = (90 - znDeg) Degree;
    gpDir   = {Cos[znRad], Sin[znRad]};
    lopDir  = {-Sin[znRad], Cos[znRad]};
    apPt    = {0.0, 0.0};
    interPt = apPt + 2.2 gpDir;
    lopLine = {interPt - 4.5 lopDir, interPt + 4.5 lopDir};

    Graphics[
      {
        (* LOP *)
        {tc["accent"], Thickness[0.006], Line[lopLine]},
        Text[Style["Line of Position (LOP)",
                   Directive[FontFamily->"Helvetica", FontSize->13, Bold,
                             FontColor->tc["accent"]]],
             interPt + 4.2 lopDir + {0, -0.4}, {Right, Center}],

        (* Azimuth arrow *)
        {tc["navy"], Thickness[0.004], Arrow[{apPt, apPt + 7.8 gpDir}]},
        Text[Style["Azimuth Zn = " <> ToString[znDeg] <> "\[Degree] (toward GP)",
                   Directive[FontFamily->"Helvetica", FontSize->13,
                             FontColor->tc["navy"]]],
             apPt + 8.3 gpDir, {Center, -1}],

        (* AP *)
        {tc["navy"], PointSize[0.032], Point[apPt]},
        Text[Style["AP\n(Assumed\nPosition)",
                   Directive[FontFamily->"Helvetica", FontSize->13,
                             FontColor->tc["navy"]]],
             apPt + {-0.3, 0}, {Right, Center}],

        (* Intercept arrow *)
        {tc["warm"], Thickness[0.006],
         Arrow[{apPt + 0.28 gpDir, interPt - 0.12 gpDir}]},
        Text[Style["p = 2.2 nm\nIntercept (Toward)\nHo > Hc",
                   Directive[FontFamily->"Helvetica", FontSize->12, Bold,
                             FontColor->tc["warm"]]],
             (apPt + interPt)/2 + lopDir 0.7, {Center, Bottom}],

        (* LOP point *)
        {tc["warm"], PointSize[0.025], Point[interPt]},

        (* Right-angle mark *)
        {tc["navy"], Thickness[0.003],
         Line[{interPt + 0.38 lopDir,
               interPt + 0.38 lopDir + 0.38 gpDir,
               interPt + 0.38 gpDir}]},

        (* Hc label *)
        Text[Style["Hc (computed\naltitude angle)",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["mid"], Italic]],
             apPt + 5.5 gpDir + lopDir 0.7, {Center, Bottom}],

        (* You are here *)
        {tc["green"], PointSize[0.022], Point[interPt + 2.2 lopDir]},
        Text[Style["You are\nsomewhere\non this line",
                   Directive[FontFamily->"Helvetica", FontSize->12, Bold,
                             FontColor->tc["green"]]],
             interPt + 2.2 lopDir + {0.2, 0.6}, {Left, Bottom}],

        (* Explanation box -- drawn from primitives. Inset[Framed[...]] (and any
           Inset of typeset content) triggers a macOS WL rasteriser bug that
           tints the whole Graphics body pale pink; Text primitives are clean. *)
        {FaceForm[tc["bg"]], EdgeForm[Directive[tc["frame"], Thickness[0.0018]]],
         Rectangle[{-5.9, 5.55}, {1.5, 9.15}]},
        Text[Style["Intercept method:",
                   Directive[FontFamily->"Helvetica", FontSize->12, Bold,
                             FontColor->tc["navy"]]], {-5.75, 8.95}, {Left, Top}],
        Text[Style["1. Compute Hc and Zn from assumed position AP",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["navy"]]], {-5.75, 8.25}, {Left, Top}],
        Text[Style["2. Compare Ho (observed) to Hc",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["navy"]]], {-5.75, 7.55}, {Left, Top}],
        Text[Style["3. Intercept p = (Ho \[Minus] Hc) \[Times] 60 nm",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["warm"]]], {-5.75, 6.85}, {Left, Top}],
        Text[Style["4. LOP is perpendicular to Zn through intercept point",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["accent"]]], {-5.75, 6.15}, {Left, Top}]
      },
      Frame      -> False, Axes -> False,
      ImageSize  -> {imgW, Round[imgW * 0.65]},
      Background -> tc["bg"],
      PlotLabel  -> Style["The Marcq St-Hilaire Intercept Method", titleDir[tc]],
      PlotRange  -> {{-6.0, 10.0}, {-1.3, 9.3}},
      ImagePadding -> {{40, 40}, {40, 60}}
    ]
  ];

(* ── Fig 3: Cocked Hat — Real Twilight Star Fix Day 12 ─────────────────────── *)
(* B2 FIX: now uses REAL twilight star fix from twilight_fixes.json day 12.     *)
(* Stars Ankaa / Enif / Rasalhague (cnBestStarTriplet); same seeds as           *)
(* twilight_replay.wls (dayIdx*100 + si). Synthetic starFix removed.            *)
makeFig3[tc_] :=
  Module[{twDay12, starNames, twTStr, twT, truePt, drPt,
          lops, fixPt, cht, verts,
          p0, toXY, extendLine, lopLineSegs, lopColors,
          hullPts, truePtXY, fixPtXY, fixErr},

    (* ── Day 12 twilight metadata from real catalogue ── *)
    twDay12   = SelectFirst[twData["days"], #["day"] == 12 &];
    starNames = twDay12["chosenStars"];    (* {"Ankaa","Enif","Rasalhague"} *)
    twTStr    = twDay12["twilightUTC"];    (* "2024-11-27T20:39:24Z" *)
    twT       = DateObject[DateList[twTStr], TimeZone -> 0];

    (* True & DR positions from day 12 in sights data (1-indexed: [[13]] = day 12) *)
    truePt = daysData[[13]]["truePos"];  (* {22.1364, -41.1026} *)
    drPt   = daysData[[13]]["drPos"];

    (* Re-derive 3 star LOPs using the engine — same seeds as twilight_replay.wls *)
    lops = Table[
      Module[{body = {"Star", starNames[[si]]}, hs},
        hs = cnGenerateSightBody[truePt, twT, body,
               <|"sigmaMin" -> 1.0, "seed" -> 12 * 100 + si|>];
        cnReduceSightBody[hs, twT, drPt, body]
      ], {si, 3}];

    fixPt  = cnFix[lops];
    cht    = cnCockedHat[lops];
    verts  = cht["vertices"];
    fixErr = QuantityMagnitude[
      GeoDistance[GeoPosition[truePt], GeoPosition[fixPt]] / Quantity[1, "NauticalMiles"]];

    p0 = fixPt;   (* centre the plot on the celestial fix *)
    toXY[{lat_, lon_}] := {(lon - p0[[2]]) Cos[p0[[1]] Degree] 60,
                            (lat - p0[[1]]) 60};
    extendLine[lop_] := Module[{pt, brg, brgRad, dir},
      pt     = toXY[lop["point"]];
      brg    = lop["bearingDeg"];
      brgRad = (90 - brg) Degree;
      dir    = {Cos[brgRad], Sin[brgRad]};
      {pt - 50 dir, pt + 50 dir}
    ];

    lopLineSegs = extendLine /@ lops;
    lopColors   = {tc["accent"], tc["warm"], tc["green"]};
    hullPts     = toXY /@ verts;
    truePtXY    = toXY[truePt];
    fixPtXY     = toXY[fixPt];   (* = {0,0} by construction *)

    Graphics[
      Flatten[{
        (* Triangle fill *)
        {FaceForm[Opacity[0.18, tc["accent"]]], EdgeForm[None],
         Polygon[hullPts]},

        (* Three LOPs *)
        MapThread[{#2, Thickness[0.005], Line[#1]} &,
                  {lopLineSegs, lopColors}],

        (* LOP labels — star name + azimuth from real catalogue computation *)
        MapThread[
          Text[Style[#3, Directive[FontFamily->"Helvetica", FontSize->11,
                                   Bold, FontColor->#2]],
               #1[[2]] + {1.5, 0.5}, {Left, Bottom}] &,
          {lopLineSegs, lopColors,
           MapThread[#1 <> " \[LongDash] Zn~" <> ToString[Round[#2]] <> "\[Degree]" &,
                     {starNames, twDay12["azimuths"]}]}],

        (* Triangle vertex dots *)
        {GrayLevel[0.55], PointSize[0.016], Point /@ hullPts},

        (* Fix point *)
        {tc["navy"], PointSize[0.030], Point[fixPtXY]},
        {White,      PointSize[0.013], Point[fixPtXY]},
        Text[Style["Celestial Fix\n" <>
                   ToString[NumberForm[N[fixPt[[1]]], {5,2}]] <> "\[Degree]N  " <>
                   ToString[NumberForm[Abs[N[fixPt[[2]]]], {5,2}]] <> "\[Degree]W",
                   Directive[FontFamily->"Helvetica", FontSize->12, Bold,
                             FontColor->tc["navy"]]],
             fixPtXY + {2.0, -1.0}, {Left, Top}],

        (* True position *)
        {tc["warm"], PointSize[0.030], Point[truePtXY]},
        {White,      PointSize[0.012], Point[truePtXY]},
        Text[Style["True Position\n" <>
                   ToString[NumberForm[N[truePt[[1]]], {5,2}]] <> "\[Degree]N  " <>
                   ToString[NumberForm[Abs[N[truePt[[2]]]], {5,2}]] <> "\[Degree]W",
                   Directive[FontFamily->"Helvetica", FontSize->12, Bold,
                             FontColor->tc["warm"]]],
             truePtXY + {-2.0, 1.5}, {Right, Bottom}],

        (* Error vector *)
        {tc["warm"], Dashed, Thickness[0.003], Line[{truePtXY, fixPtXY}]},
        Text[Style["Error = " <> ToString[NumberForm[N[Round[fixErr, 0.01]], {4,2}]] <> " nm",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["mid"]]],
             (truePtXY + fixPtXY)/2 + {1.5, 0.3}, {Left, Center}],

        (* Cocked-hat area label *)
        Text[Style["Cocked-hat area\n= " <>
                   ToString[NumberForm[N[Round[cht["areaNm2"], 0.001]], {4,3}]] <>
                   " nm^2",
                   Directive[FontFamily->"Helvetica", FontSize->12,
                             FontColor->tc["navy"]]],
             Mean[hullPts], {Center, Center}]
      }],
      Frame      -> True,
      FrameLabel -> {Style["East\[LongDash]West (nm)", labelDir[tc]],
                     Style["North\[LongDash]South (nm)", labelDir[tc]]},
      FrameStyle -> Directive[tc["frame"], Thickness[0.001]],
      GridLines  -> Automatic,
      GridLinesStyle -> tc["grid"],
      ImageSize  -> {imgW, Round[imgW * 0.55]},
      Background -> tc["bg"],
      PlotLabel  -> Style[
        "Cocked Hat \[LongDash] Three-Star Fix (Real Navigational Star Catalogue)\n" <>
        "(22.1\[Degree]N, 41.1\[Degree]W, mid-Atlantic, 27 Nov 2024 20:39 UTC)  " <>
        StringRiffle[starNames, " / "],
        titleDir[tc]],
      PlotRange  -> All,
      ImagePadding -> {{70, 60}, {65, 80}}
    ]
  ];

(* ── Fig 4: Running Fix ─────────────────────────────────────────────────── *)
makeFig4[tc_] :=
  Module[{day5, lop1, lop3, courseDeg, distNm, advLop, rfix,
          p0, toXY, extLine, l1segs, l3segs, advSegs,
          pt1XY, pt3XY, advPtXY, rfixXY},
    day5      = daysData[[6]];
    lop1      = day5["lops"][[1]];
    lop3      = day5["lops"][[3]];
    courseDeg = 260.0;
    distNm    = 42.0;
    advLop    = cnAdvanceLOP[lop1, courseDeg, distNm];
    rfix      = cnFix[{advLop, lop3}];

    p0 = lop3["point"];
    toXY[{lat_, lon_}] := {(lon - p0[[2]]) Cos[p0[[1]] Degree] 60,
                            (lat - p0[[1]]) 60};
    extLine[lop_] := Module[{pt, brg, brgRad, dir},
      pt     = toXY[lop["point"]];
      brg    = lop["bearingDeg"];
      brgRad = (90 - brg) Degree;
      dir    = {Cos[brgRad], Sin[brgRad]};
      {pt - 28 dir, pt + 28 dir}
    ];

    l1segs  = extLine[lop1];
    l3segs  = extLine[lop3];
    advSegs = extLine[advLop];
    pt1XY   = toXY[lop1["point"]];
    pt3XY   = toXY[lop3["point"]];
    advPtXY = toXY[advLop["point"]];
    rfixXY  = toXY[rfix];

    Graphics[
      {
        (* Original LOP1 faint *)
        {Opacity[0.30, tc["accent"]], Dashed, Thickness[0.004], Line[l1segs]},
        Text[Style["LOP1 (morning sight, not yet advanced)",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             Italic, FontColor->Opacity[0.50, tc["accent"]]]],
             l1segs[[1]] + {0.2, 0.5}, {Left, Bottom}],

        (* Transferred LOP1 *)
        {tc["accent"], Thickness[0.006], Line[advSegs]},
        Text[Style["LOP1 transferred\n(" <> ToString[distNm] <>
                   " nm at " <> ToString[courseDeg] <> "\[Degree])",
                   Directive[FontFamily->"Helvetica", FontSize->12,
                             Bold, FontColor->tc["accent"]]],
             advSegs[[2]] + {0.3, 0.5}, {Left, Bottom}],

        (* LOP2 (afternoon) *)
        {tc["warm"], Thickness[0.006], Line[l3segs]},
        Text[Style["LOP2 (afternoon sight)",
                   Directive[FontFamily->"Helvetica", FontSize->12,
                             Bold, FontColor->tc["warm"]]],
             l3segs[[1]] + {-0.3, 0.6}, {Right, Bottom}],

        (* Run vector *)
        {tc["navy"], Thickness[0.006], Arrow[{pt1XY, advPtXY}]},
        Text[Style["Run: " <> ToString[distNm] <> " nm\nCourse: " <>
                   ToString[courseDeg] <> "\[Degree]",
                   Directive[FontFamily->"Helvetica", FontSize->12,
                             FontColor->tc["navy"]]],
             (pt1XY + advPtXY)/2 + {-1.5, 1.2}, {Center, Bottom}],

        (* Markers *)
        {tc["accent"],                PointSize[0.018], Point[pt1XY]},
        {Opacity[0.5, tc["accent"]], PointSize[0.018], Point[advPtXY]},
        {tc["warm"],                  PointSize[0.018], Point[pt3XY]},

        (* Running fix *)
        {tc["green"], PointSize[0.032], Point[rfixXY]},
        {White,       PointSize[0.014], Point[rfixXY]},
        Text[Style["Running Fix",
                   Directive[FontFamily->"Helvetica", FontSize->13,
                             Bold, FontColor->tc["green"]]],
             rfixXY + {0.6, -0.8}, {Left, Top}]
      },
      Frame      -> True,
      FrameLabel -> {Style["East\[LongDash]West (nm)", labelDir[tc]],
                     Style["North\[LongDash]South (nm)", labelDir[tc]]},
      FrameStyle -> Directive[tc["frame"], Thickness[0.001]],
      GridLines  -> Automatic,
      GridLinesStyle -> tc["grid"],
      ImageSize  -> {imgW, Round[imgW * 0.55]},
      Background -> tc["bg"],
      PlotLabel  -> Style["Running Fix \[LongDash] Transferring a Position Line",
                          titleDir[tc]],
      PlotRange  -> {{-32, 32}, {-12, 12}},
      ImagePadding -> {{70, 60}, {55, 70}}
    ]
  ];

(* ── Fig 5: Corrections Waterfall ──────────────────────────────────────── *)
(* A5 FIX: explicit ImageSize {imgW, Round[imgW*0.60]} removes letterboxing *)
(* A1 FIX: arcminute tick uses \[Prime] instead of raw UTF-8 '             *)
makeFig5[tc_] :=
  Module[{hs, height, dipMin, refMin, sdMin, parMin, ho,
          running, barPairs, barColors, labels, nBars, redCol, greenCol},
    hs     = 43.2;
    height = 2.0;
    dipMin = cnDip[height];
    refMin = cnRefraction[hs];
    sdMin  = cnSunSemidiameter[];
    parMin = cnSunParallax[hs];
    ho     = hs - dipMin/60 - refMin/60 + sdMin/60 + parMin/60;

    labels = {"Hs\n(sextant)",
               "\[Minus]Dip\n(" <> ToString[NumberForm[-dipMin, {4,1}]] <> "\[Prime])",
               "\[Minus]Refraction\n(" <> ToString[NumberForm[-refMin, {4,2}]] <> "\[Prime])",
               "+Semi-diam\n(+16.0\[Prime])",
               "+Parallax\n(+" <> ToString[NumberForm[parMin, {4,2}]] <> "\[Prime])",
               "Ho\n(corrected)"};

    running = {hs,
               hs - dipMin/60,
               hs - dipMin/60 - refMin/60,
               hs - dipMin/60 - refMin/60 + sdMin/60,
               hs - dipMin/60 - refMin/60 + sdMin/60 + parMin/60,
               ho};

    nBars    = Length[running];
    redCol   = If[tc["bg"] === White,
                 RGBColor[0.75, 0.25, 0.20], RGBColor[0.90, 0.35, 0.30]];
    greenCol = If[tc["bg"] === White,
                 RGBColor[0.20, 0.60, 0.35], RGBColor[0.30, 0.80, 0.45]];

    barPairs = Table[
      Which[
        i == 1,     {43.0, running[[1]]},
        i == nBars, {43.0, running[[nBars]]},
        True,       {Min[running[[i]], running[[i-1]]],
                     Max[running[[i]], running[[i-1]]]}
      ],
      {i, 1, nBars}];

    barColors = {tc["navy"], redCol, redCol, greenCol, greenCol, tc["accent"]};

    Graphics[
      Flatten[{
        (* Bars *)
        MapThread[Function[{pair, col, xi},
          {FaceForm[col], EdgeForm[{tc["bg"], Thickness[0.003]}],
           Rectangle[{xi - 0.40, pair[[1]]}, {xi + 0.40, pair[[2]]}]}],
          {barPairs, barColors, Range[nBars]}],

        (* Value labels inside bars *)
        MapThread[Function[{pair, xi},
          If[Abs[pair[[2]] - pair[[1]]] > 0.003,
             Text[Style[ToString[NumberForm[pair[[2]], {5,3}]] <> "\[Degree]",
                        Directive[FontFamily->"Helvetica", FontSize->15,
                                  Bold, FontColor->White]],
                  {xi, (pair[[1]] + pair[[2]])/2}],
             Nothing]],
          {barPairs, Range[nBars]}],

        (* Delta labels *)
        MapThread[Function[{pair, xi, running1, running2},
          If[xi > 1 && xi < nBars,
             Module[{delta = (running2 - running1) * 60},
               Text[Style[If[delta >= 0, "+", ""] <>
                          ToString[NumberForm[delta, {4,2}]] <> "\[Prime]",
                          Directive[FontFamily->"Helvetica", FontSize->16, Bold,
                                    FontColor->If[delta < 0, redCol, greenCol]]],
                    {xi, If[delta < 0, pair[[1]] - 0.004, pair[[2]] + 0.004]},
                    {Center, If[delta < 0, Top, Bottom]}]
             ],
             Nothing]],
          {barPairs, Range[nBars], Prepend[Most[running], 0], running}],

        (* Connectors *)
        Table[
          {tc["mid"], Dashed, Thickness[0.002],
           Line[{{i + 0.40, running[[i+1]]},
                 {i + 1 - 0.40, running[[i+1]]}}]},
          {i, 1, nBars - 1}],

        (* X-axis labels *)
        MapThread[
          Text[Style[#2, Directive[FontFamily->"Helvetica", FontSize->14,
                                   FontColor->tc["text"]]],
               {#1, 42.998}, {Center, Top}] &,
          {Range[nBars], labels}]
      }],
      Frame       -> True, Axes -> False,
      FrameLabel  -> {None, Style["Altitude (\[Degree])", labelDir[tc]]},
      FrameStyle  -> Directive[tc["frame"]],
      GridLines   -> {None, Table[{v, Directive[tc["frame"], Dashed]},
                                  {v, 43.00, 43.45, 0.05}]},
      (* Explicit AspectRatio: the tiny altitude y-range vs 6-bar x-range made
         Automatic aspect collapse the bars into an unreadable strip.  Forcing
         AspectRatio fills the panel so every label reads at ~620 px display. *)
      AspectRatio -> 0.62,
      ImageSize   -> imgW,
      Background  -> tc["bg"],
      PlotLabel   -> Style[
        "Altitude Correction Cascade: Hs \[Rule] Ho\n" <>
        "(Hs = 43.2\[Degree], eye height 2 m, lower limb)",
        titleDir[tc]],
      PlotRange   -> {{0.4, nBars + 0.6}, {42.985, 43.47}},
      ImagePadding -> {{80, 30}, {70, 68}}
    ]
  ];

(* ── Fig 6: Sub-solar Track + Analemma ─────────────────────────────────── *)
makeFig6[tc_] :=
  Module[{tDay, hourVals, dayPts, dayLons, tYear0, yearDays,
          yearTimes, yearPts, fig1, fig2},
    tDay     = DateObject[{2024, 11, 22, 0, 0, 0}, TimeZone -> 0];
    hourVals = Range[0, 24, 0.5];
    dayPts   = cnSunGP[DatePlus[tDay, Quantity[#, "Hours"]]] & /@ hourVals;
    dayLons  = dayPts[[All, 2]];

    tYear0    = DateObject[{2024, 1, 1, 12, 0, 0}, TimeZone -> 0];
    yearDays  = Range[0, 364, 3];
    yearTimes = DatePlus[tYear0, Quantity[#, "Days"]] & /@ yearDays;
    yearPts   = cnSunGP /@ yearTimes;

    fig1 = ListLinePlot[
      Transpose[{hourVals, dayLons}],
      PlotStyle    -> {tc["warm"], Thickness[0.004]},
      Frame        -> True,
      FrameLabel   -> {Style["UTC Hour", labelDir[tc]],
                       Style["GP Longitude (\[Degree]E)", labelDir[tc]]},
      FrameStyle   -> Directive[tc["frame"]],
      GridLines    -> {Range[0, 24, 3], Automatic},
      GridLinesStyle -> tc["grid"],
      ImageSize    -> {imgW, 430},
      Background   -> tc["bg"],
      PlotLabel    -> Style["Sub-solar Point Longitude\n22 November 2024",
                            titleDir[tc]],
      Epilog -> {
        Text[Style["GP moves westward\n~15\[Degree]/hour (360\[Degree]/24h)",
                   Directive[FontFamily->"Helvetica", FontSize->12,
                             FontColor->tc["navy"]]],
             Scaled[{0.5, 0.12}], {Center, Bottom}]
      },
      ImagePadding -> {{70, 20}, {55, 60}}
    ];

    fig2 = ListLinePlot[
      yearPts[[All, {2, 1}]],
      PlotStyle            -> {tc["navy"], Thickness[0.003]},
      ColorFunction        -> (Blend[{tc["accent"], tc["green"],
                                      tc["warm"], tc["navy"]}, #1] &),
      ColorFunctionScaling -> True,
      Frame        -> True,
      FrameLabel   -> {Style["Equation of Time (GP lon at solar noon, \[Degree])",
                             labelDir[tc]],
                       Style["Declination (\[Degree])", labelDir[tc]]},
      FrameStyle   -> Directive[tc["frame"]],
      GridLines    -> {Range[-20, 20, 5], Range[-25, 25, 5]},
      GridLinesStyle -> tc["grid"],
      ImageSize    -> {imgW, 430},
      Background   -> tc["bg"],
      PlotLabel    -> Style["Analemma \[LongDash] Sub-solar Point over 2024",
                            titleDir[tc]],
      PlotRange    -> {{-20, 18}, {-25, 25}},
      Epilog -> {
        {tc["warm"], PointSize[0.025],
         Point[{yearPts[[58, 2]], yearPts[[58, 1]]}]},
        Text[Style["Jun solstice",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["warm"]]],
             {yearPts[[58, 2]], 24.5}, {Center, Bottom}],
        {tc["accent"], PointSize[0.025],
         Point[{yearPts[[118, 2]], yearPts[[118, 1]]}]},
        Text[Style["Dec solstice",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["accent"]]],
             {yearPts[[118, 2]], -24.5}, {Center, Top}],
        Text[Style["Mar equinox",
                   Directive[FontFamily->"Helvetica", FontSize->10,
                             FontColor->tc["mid"]]],
             {yearPts[[26, 2]], 1.5}, {Center, Bottom}],
        Text[Style["Sep equinox",
                   Directive[FontFamily->"Helvetica", FontSize->10,
                             FontColor->tc["mid"]]],
             {yearPts[[88, 2]], -1.5}, {Center, Top}]
      },
      ImagePadding -> {{75, 20}, {55, 60}}
    ];

    (* Stacked vertically (was a 3.2:1 side-by-side row that shrank to an
       unreadable strip at ~620 px); each panel is now full width. *)
    GraphicsColumn[{fig1, fig2}, ImageSize -> imgW, Background -> tc["bg"],
                Spacings -> 20]
  ];

(* ── Fig 7: Error Ellipse (Monte-Carlo) ────────────────────────────────── *)
makeFig7[tc_] :=
  Module[{truePos, t0, times, mc, fixes, cep, cov,
          p0, toXY, fixPts, eigenVals, eigenVecs,
          axA, axB, rotAngle, theta, ellXY},
    truePos = {28.13, -15.43};
    t0      = DateObject[{2024, 11, 22, 10, 0, 0}, TimeZone -> 0];
    times   = {t0,
               DatePlus[t0, Quantity[3, "Hours"]],
               DatePlus[t0, Quantity[6, "Hours"]]};
    mc      = cnMonteCarloFix[truePos, times,
                <|"sigmaMin" -> 1.0, "seed" -> 42|>, 300];
    fixes   = mc["fixes"];
    cep     = mc["cep"];
    cov     = mc["covNm"];

    p0    = truePos;
    toXY[{lat_, lon_}] := {(lon - p0[[2]]) Cos[p0[[1]] Degree] 60,
                            (lat - p0[[1]]) 60};
    fixPts = toXY /@ fixes;

    {eigenVals, eigenVecs} = Eigensystem[cov];
    axA      = Sqrt[Max[eigenVals] * 5.991];
    axB      = Sqrt[Min[eigenVals] * 5.991];
    rotAngle = ArcTan @@ eigenVecs[[1]];

    theta = Range[0, 2 Pi, Pi/60];
    ellXY = Table[
      {axA Cos[t] Cos[rotAngle] - axB Sin[t] Sin[rotAngle],
       axA Cos[t] Sin[rotAngle] + axB Sin[t] Cos[rotAngle]},
      {t, theta}];

    Graphics[
      {
        (* Scatter *)
        {Opacity[0.35, tc["accent"]], PointSize[0.008], Point /@ fixPts},

        (* 95% ellipse *)
        {tc["warm"], Thickness[0.005], Line[Append[ellXY, ellXY[[1]]]]},

        (* CEP circle 50% *)
        {tc["navy"], Dashed, Thickness[0.004], Circle[{0, 0}, cep]},

        (* True position *)
        {tc["warm"], PointSize[0.028], Point[{0, 0}]},
        {White,      PointSize[0.013], Point[{0, 0}]},
        Text[Style["True\nPosition",
                   Directive[FontFamily->"Helvetica", FontSize->12,
                             Bold, FontColor->tc["warm"]]],
             {0.25, -0.35}, {Left, Top}],

        (* Labels *)
        Text[Style["CEP = " <> ToString[NumberForm[cep, {4, 2}]] <>
                   " nm  (50% containment circle)",
                   Directive[FontFamily->"Helvetica", FontSize->12,
                             Bold, FontColor->tc["navy"]]],
             {cep + 0.15, 0.1}, {Left, Center}],
        Text[Style["95% confidence ellipse  (\[Chi]^2(2)=5.991, containment\[TildeEqual]95%)",
                   Directive[FontFamily->"Helvetica", FontSize->12,
                             Bold, FontColor->tc["warm"]]],
             ellXY[[16]] + {0.15, 0.15}, {Left, Bottom}],

        Text[Style["n = 300 Monte-Carlo trials | \[Sigma] = 1.0 arcmin | 3 sights",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["mid"]]],
             Scaled[{0.97, 0.02}], {Right, Bottom}],

        (* B4/A7 HONESTY NOTE: MC is random-noise floor under perfect ephemeris *)
        Text[Style["NOTE: errors shown are random-noise floor (same ephemeris generates & reduces sights).\n" <>
                   "Real-world fixes: ~1-3 nm, dominated by systematic errors (refraction, dip, index error).",
                   Directive[FontFamily->"Helvetica", FontSize->11, Italic,
                             FontColor->tc["mid"]]],
             Scaled[{0.02, 0.98}], {Left, Top}]
      },
      Frame      -> True, Axes -> False,
      FrameLabel -> {Style["East error (nm)", labelDir[tc]],
                     Style["North error (nm)", labelDir[tc]]},
      FrameStyle -> Directive[tc["frame"]],
      GridLines  -> Automatic, GridLinesStyle -> tc["grid"],
      ImageSize  -> {imgW, Round[imgW * 0.55]},
      Background -> tc["bg"],
      PlotLabel  -> Style[
        "Monte-Carlo Fix Distribution + 95% Confidence Ellipse  " <>
        "(\[Chi]^2(2)=5.991, containment\[TildeEqual]95%)\n" <>
        "\[Sigma] = 1.0 arcmin per sight, 3 sights  |  Random-noise floor: perfect-ephemeris simulation",
        titleDir[tc]],
      PlotRange  -> All,
      ImagePadding -> {{70, 60}, {55, 70}}
    ]
  ];

(* ── Fig 8: Cut Angle vs CEP ────────────────────────────────────────────── *)
makeFig8[tc_] :=
  Module[{angles, cepsNm},
    angles = Range[5, 175, 2.5];
    cepsNm = Table[0.675 / Abs[Sin[a Degree]], {a, angles}];

    Show[
      ListLinePlot[
        Transpose[{angles, cepsNm}],
        PlotStyle    -> {tc["navy"], Thickness[0.005]},
        Filling      -> Bottom,
        FillingStyle -> Directive[Opacity[0.10, tc["accent"]]],
        Frame        -> True,
        FrameLabel   -> {Style["Cut Angle Between Two LOPs (\[Degree])", labelDir[tc]],
                         Style["CEP (nautical miles)", labelDir[tc]]},
        FrameStyle   -> Directive[tc["frame"]],
        GridLines    -> {Range[0, 180, 30], Automatic},
        GridLinesStyle -> tc["grid"],
        PlotRange    -> {{0, 180}, {0, 8}},
        ImageSize    -> {imgW, Round[imgW * 0.55]},
        Background   -> tc["bg"],
        PlotLabel    -> Style[
          "Fix Accuracy vs LOP Cut Angle\n" <>
          "(\[Sigma] = 1.0 arcmin per LOP, two-LOP fix)",
          titleDir[tc]],
        ImagePadding -> {{75, 60}, {55, 70}},
        Epilog -> {
          {tc["warm"], Dashed, Thickness[0.004], Line[{{90, 0}, {90, 8}}]},
          Text[Style["Optimal cut: 90\[Degree]\nCEP = 0.68 nm",
                     Directive[FontFamily->"Helvetica", FontSize->12,
                               Bold, FontColor->tc["warm"]]],
               {90, 5.8}, {Center, Bottom}],
          {Opacity[0.10, RGBColor[0.8,0.15,0.15]], Rectangle[{0, 0}, {30, 8}]},
          {Opacity[0.10, RGBColor[0.8,0.15,0.15]], Rectangle[{150, 0}, {180, 8}]},
          Text[Style["Poor\ncut",
                     Directive[FontFamily->"Helvetica", FontSize->12,
                               FontColor->RGBColor[0.65,0.1,0.1]]],
               {15, 7}, {Center, Top}],
          Text[Style["Poor\ncut",
                     Directive[FontFamily->"Helvetica", FontSize->12,
                               FontColor->RGBColor[0.65,0.1,0.1]]],
               {165, 7}, {Center, Top}],
          Text[Style["\[Sigma] = 1.0 arcmin ~ 1 nm per LOP",
                     Directive[FontFamily->"Helvetica", FontSize->11,
                               FontColor->tc["mid"]]],
               Scaled[{0.97, 0.03}], {Right, Bottom}]
        }
      ]
    ]
  ];

(* ── Fig 9: Celestial vs GPS ────────────────────────────────────────────── *)
(* A4 FIX: legend Placed[Below] to prevent right-edge clipping              *)
makeFig9[tc_] :=
  Module[{dates, errors, gpsErr, datePairs, gpsLine},
    dates     = DateObject[#["datetimeNoonUTC"], TimeZone -> 0] & /@ daysData;
    errors    = N[#["runningFixErrorNm"]] & /@ daysData;
    gpsErr    = 0.005;
    datePairs = Transpose[{dates, errors}];
    gpsLine   = {{First[dates], gpsErr}, {Last[dates], gpsErr}};

    DateListPlot[
      {datePairs, gpsLine},
      PlotStyle -> {
        Directive[tc["accent"], Thickness[0.004]],
        Directive[tc["warm"],   Dashed, Thickness[0.003]]},
      PlotMarkers  -> {{"\[FilledCircle]", 10}, None},
      Frame        -> True,
      FrameLabel   -> {Style["Date (UTC)", labelDir[tc]],
                       Style["Fix Error (nautical miles)", labelDir[tc]]},
      FrameStyle   -> Directive[tc["frame"]],
      GridLines    -> Automatic, GridLinesStyle -> tc["grid"],
      ImageSize    -> {imgW, Round[imgW * 0.55]},
      Background   -> tc["bg"],
      PlotLabel    -> Style[
        "Celestial Navigation Accuracy vs GPS\n" <>
        "Nov\[LongDash]Dec 2024 Atlantic Crossing  (random-noise floor, perfect-ephemeris simulation)",
        titleDir[tc]],
      PlotLegends  -> Placed[
        LineLegend[
          {Directive[tc["accent"], Thickness[0.004]],
           Directive[tc["warm"],   Dashed, Thickness[0.003]]},
          {"Celestial running fix error (nm)",
           "GPS accuracy (~0.005 nm / ~10 m)"},
          LegendMarkerSize -> {{35, 3}},
          LabelStyle -> Directive[FontFamily->"Helvetica", FontSize->12,
                                  FontColor->tc["text"]]],
        Below],   (* was Placed[..., {Scaled[0.97], ...}] -- fixed clipping *)
      Epilog -> {
        {tc["navy"], Dashed, Thickness[0.003],
         Line[{{First[dates], 1.092}, {Last[dates], 1.092}}]},
        Text[Style["Mean = 1.09 nm",
                   Directive[FontFamily->"Helvetica", FontSize->12,
                             Bold, FontColor->tc["navy"]]],
             {dates[[12]], 1.15}, {Center, Bottom}],
        {GrayLevel[0.6], Dotted, Thickness[0.002],
         Line[{{First[dates], 1.0}, {Last[dates], 1.0}}]},
        Text[Style["1 nm reference",
                   Directive[FontFamily->"Helvetica", FontSize->11,
                             FontColor->tc["mid"]]],
             {dates[[20]], 1.05}, {Right, Bottom}],

        (* B4 HONESTY NOTE: these errors are random-noise floor only *)
        Text[Style["NOTE: errors are random-noise floor (same ephemeris generates & reduces sights).\n" <>
                   "Real-world celestial: ~1-3 nm, dominated by systematic errors.",
                   Directive[FontFamily->"Helvetica", FontSize->11, Italic,
                             FontColor->tc["mid"]]],
             {dates[[2]], 3.1}, {Left, Top}]
      },
      ImagePadding -> {{75, 20}, {55, 100}}  (* extra bottom for legend *)
    ]
  ];

(* ==========================================================================
   PUBLIC WRAPPERS for the figures.wls-derived generators (light theme).
   ========================================================================== *)
sxFigAltitudeCircle[]       := makeFig1[$tcL, "relief"];
sxFigIntercept[]            := makeFig2[$tcL];
sxFigCockedHat[]            := makeFig3[$tcL];
sxFigRunningFix[]           := makeFig4[$tcL];
sxFigCorrectionsWaterfall[] := makeFig5[$tcL];
sxFigSubsolarTrack[]        := makeFig6[$tcL];
sxFigErrorEllipse[]         := makeFig7[$tcL];
sxFigCutAngle[]             := makeFig8[$tcL];
sxFigCelestialVsGPS[]       := makeFig9[$tcL];

(* fragA.wl — figure functions: EoT, horizon dip, refraction *)

sxFigEquationOfTime[] := Module[
  {accBlue, warmOg, navy, midGray, green, year, t0, doys, rawData,
   doyVals, eotVals, decVals, iMax, iMin, eotMax, doyMax, eotMin, doyMin,
   doy2label, pairs, crossPairs, zeroDoys, monthStarts, monthLabels,
   monthMidDoys, xTicks, yTicksEoT, maxLbl, minLbl, zeroCrossDots, epiLeft,
   leftPanel, seasonDoys, seasonNames, seasonColors, seasonPosOff, seasonAnch,
   seasonIdxs, seasonEoTs, seasonDecs, seasonPts, seasonEpi, monthIdxs,
   monthPts, monthEpi, epiRight, yTicksDec, xTicksEoT, rightPanel, caption,
   row, fig},

  accBlue = RGBColor[0.25, 0.45, 0.95];
  warmOg  = RGBColor[0.85, 0.50, 0.20];
  navy    = RGBColor[0.13, 0.29, 0.53];
  midGray = GrayLevel[0.48];
  green   = RGBColor[0.15, 0.60, 0.25];

  year = 2024;
  t0   = DateObject[{year, 1, 1, 12, 0, 0}, TimeZone -> 0];
  doys = Range[1, 366, 2];

  rawData = Table[
    Module[{t, gp},
      t  = DatePlus[t0, Quantity[d - 1, "Days"]];
      gp = cnSunGP[t];
      {d, -gp[[2]] * 4.0, gp[[1]]}
    ],
    {d, doys}
  ];

  doyVals = rawData[[All, 1]];
  eotVals = rawData[[All, 2]];
  decVals = rawData[[All, 3]];

  iMax = First[Ordering[eotVals, -1]];
  iMin = First[Ordering[eotVals,  1]];

  eotMax = eotVals[[iMax]]; doyMax = doyVals[[iMax]];
  eotMin = eotVals[[iMin]]; doyMin = doyVals[[iMin]];

  doy2label[dd_] :=
    Module[{mlen, cum, m, dy},
      mlen = {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
      cum  = FoldList[Plus, 0, mlen];
      m    = First[Select[Range[12], cum[[#]] < dd <= cum[[# + 1]] &, 1]];
      dy   = dd - cum[[m]];
      {"Jan","Feb","Mar","Apr","May","Jun",
       "Jul","Aug","Sep","Oct","Nov","Dec"}[[m]] <> " " <> ToString[dy]
    ];

  pairs  = Partition[Transpose[{doyVals, eotVals}], 2, 1];
  crossPairs = Select[pairs, Sign[#[[1,2]]] * Sign[#[[2,2]]] < 0 &];
  zeroDoys = Map[
    Function[pr,
      Module[{d1 = pr[[1,1]], e1 = pr[[1,2]], d2 = pr[[2,1]], e2 = pr[[2,2]]},
        d1 + (d2 - d1) * Abs[e1] / (Abs[e1] + Abs[e2])
      ]
    ],
    crossPairs
  ];

  monthStarts  = {1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336};
  monthLabels  = {"Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec"};
  monthMidDoys = {16, 47, 76, 107, 137, 168, 198, 229, 260, 290, 321, 351};

  xTicks = Table[
    {monthStarts[[i]],
     Style[monthLabels[[i]], 11, FontFamily -> "Helvetica", FontColor -> navy]},
    {i, 12}
  ];
  yTicksEoT = Table[
    {v, Style[ToString[v], 11, FontFamily -> "Helvetica", FontColor -> navy]},
    {v, Range[-16, 18, 2]}
  ];

  maxLbl = "+" <> ToString[Round[eotMax, 0.1]] <> " min (~" <> doy2label[doyMax] <> ")";
  minLbl = ToString[Round[eotMin, 0.1]] <> " min (~" <> doy2label[doyMin] <> ")";

  zeroCrossDots = Map[
    {midGray, PointSize[0.009], Point[{#, 0}]}&,
    zeroDoys
  ];

  epiLeft = Join[
    {
      accBlue, PointSize[0.014], Point[{doyMax, eotMax}],
      Text[Style[maxLbl, 9, FontFamily -> "Helvetica", FontColor -> navy],
           {doyMax, eotMax + 1.5}, {0, -1}],
      warmOg, PointSize[0.014], Point[{doyMin, eotMin}],
      Text[Style[minLbl, 9, FontFamily -> "Helvetica", FontColor -> warmOg],
           {doyMin, eotMin - 1.8}, {0, 1}]
    },
    zeroCrossDots
  ];

  leftPanel = ListLinePlot[
    Transpose[{doyVals, eotVals}],
    PlotStyle     -> {Thick, accBlue},
    Frame         -> True,
    FrameStyle    -> Directive[GrayLevel[0.75], Thin],
    FrameLabel    -> {
      None,
      Style["Equation of Time (minutes)", 12, FontFamily -> "Helvetica",
            FontColor -> navy]
    },
    PlotLabel     -> Style["Equation of Time — 2024", 13, Bold,
                           FontFamily -> "Helvetica", FontColor -> navy],
    FrameTicks    -> {{yTicksEoT, None}, {xTicks, None}},
    Background    -> White,
    GridLines     -> {monthStarts, {0}},
    GridLinesStyle -> Directive[GrayLevel[0.88]],
    PlotRange     -> {{1, 366}, {-17, 20}},
    ImagePadding  -> {{65, 25}, {55, 55}},
    Epilog        -> epiLeft
  ];

  seasonDoys   = {80, 172, 266, 356};
  seasonNames  = {"Spring Equinox", "Summer Solstice", "Autumn Equinox", "Winter Solstice"};
  seasonColors = {green, RGBColor[0.82, 0.62, 0.05], warmOg, accBlue};
  seasonPosOff = {{-1.5, -2.5}, {-4.0, +1.5}, {+1.5, +2.5}, {+1.5, -2.5}};
  seasonAnch   = {{1, 1}, {1, -1}, {-1, 1}, {-1, 1}};

  seasonIdxs = Map[First[Ordering[Abs[doyVals - #]]]&, seasonDoys];
  seasonEoTs = eotVals[[seasonIdxs]];
  seasonDecs = decVals[[seasonIdxs]];
  seasonPts  = Transpose[{seasonEoTs, seasonDecs}];

  seasonEpi = Flatten[Table[
    {
      seasonColors[[i]], PointSize[0.016], Point[seasonPts[[i]]],
      Text[Style[seasonNames[[i]], 9, Bold, FontFamily -> "Helvetica",
                 FontColor -> seasonColors[[i]]],
           seasonPts[[i]] + seasonPosOff[[i]], seasonAnch[[i]]]
    },
    {i, 4}
  ], 1];

  monthIdxs = Map[First[Ordering[Abs[doyVals - #]]]&, monthMidDoys];
  monthPts  = Transpose[{eotVals[[monthIdxs]], decVals[[monthIdxs]]}];

  monthEpi = Flatten[Table[
    {midGray, PointSize[0.007], Point[monthPts[[i]]],
     Text[Style[monthLabels[[i]], 8, FontFamily -> "Helvetica",
                FontColor -> midGray],
          monthPts[[i]] + {0.4, 0.5}, {-1, -1}]},
    {i, 12}
  ], 1];

  epiRight = Join[seasonEpi, monthEpi];

  yTicksDec = Table[
    {v, Style[ToString[v] <> "\[Degree]", 11, FontFamily -> "Helvetica",
              FontColor -> navy]},
    {v, Range[-24, 24, 6]}
  ];
  xTicksEoT = Table[
    {v, Style[ToString[v], 11, FontFamily -> "Helvetica", FontColor -> navy]},
    {v, Range[-14, 16, 4]}
  ];

  rightPanel = ListLinePlot[
    Transpose[{eotVals, decVals}],
    PlotStyle     -> {Thick, navy},
    Frame         -> True,
    FrameStyle    -> Directive[GrayLevel[0.75], Thin],
    FrameLabel    -> {
      Style["EoT (min): apparent \[Minus] mean solar time", 12,
            FontFamily -> "Helvetica", FontColor -> navy],
      Style["Solar Declination (\[Degree])", 12,
            FontFamily -> "Helvetica", FontColor -> navy]
    },
    PlotLabel     -> Style["Analemma: Sun's Position at Clock Noon", 13, Bold,
                           FontFamily -> "Helvetica", FontColor -> navy],
    FrameTicks    -> {{yTicksDec, None}, {xTicksEoT, None}},
    Background    -> White,
    GridLines     -> {{0}, {0}},
    GridLinesStyle -> Directive[GrayLevel[0.72], Dashed],
    PlotRange     -> {{-17, 20}, {-25.5, 25.5}},
    AspectRatio   -> 1.25,
    ImagePadding  -> {{70, 20}, {55, 55}},
    Epilog        -> epiRight
  ];

  caption = Style[
    "Equation of Time: apparent minus mean solar time (" <>
    "\[Minus]14 to +16 min over the year, from orbital eccentricity + axial tilt). " <>
    "The apparent Sun can cross your meridian up to ~16 min off clock noon — " <>
    "navigators apply this correction to determine Local Apparent Noon (LAN). " <>
    "The analemma (right) is the figure-8 traced by the Sun's noon position " <>
    "over a year, uniting EoT and declination.",
    11, Italic, FontFamily -> "Helvetica", FontColor -> GrayLevel[0.35]
  ];

  row = GraphicsRow[
    {leftPanel, rightPanel},
    ImageSize  -> 1600,
    Background -> White,
    Spacings   -> 30
  ];

  fig = Column[
    {row, Spacer[4], caption},
    Alignment -> Center
  ];

  fig
];


sxFigHorizonDip[] := Module[
  {bgColor, axColor, colBlue, colNavy, colWarm, colSky, colSea, colEarth,
   Rd, hd, d, alpha, Hx, Hy, horizPt, eyePt, nadirPt, dirSight, dirHoriz,
   dipAngleDiag, arcHalf, arcPts, yBtm, earthFillPoly, sightEnd, horizLeft,
   horizRight, hMarkX, tickLen, rAngle, rPtOnSurf, dipArcR, sightAngle,
   dipArcPts, dipLabelAngle, dipLabelOff, dipLabelPos, rDirH, tDirH, sqSz,
   boxPts, sightMidPos, hLabelPos, rLabelPos, earthLabelPos, diag, heights,
   dipVals, distVals, cases, sens2m, sens2mStr, dipCasePrims, panel2a,
   distCasePrims, panel2b, makeTitle, titleLeft, titleRight, leftCol,
   rightCol, combined},

  bgColor = White;
  axColor = GrayLevel[0.18];
  colBlue = RGBColor[0.25, 0.45, 0.95];
  colNavy = RGBColor[0.13, 0.29, 0.53];
  colWarm = RGBColor[0.85, 0.50, 0.20];
  colSky  = RGBColor[0.87, 0.93, 0.98];
  colSea  = RGBColor[0.22, 0.50, 0.72];
  colEarth = RGBColor[0.18, 0.40, 0.58];

  Rd = 1.0;
  hd = 0.22;
  d  = Rd + hd;

  alpha = ArcCos[Rd / d];

  Hx = -Rd * Sin[alpha];
  Hy =  Rd * Cos[alpha];
  horizPt  = {Hx, Hy};
  eyePt    = {0.0, d};
  nadirPt  = {0.0, Rd};

  dirSight = Normalize[horizPt - eyePt];

  dirHoriz = {-1.0, 0.0};

  dipAngleDiag = ArcCos[Dot[dirHoriz, dirSight]];

  arcHalf = 78 * Degree;
  arcPts = Table[
    {Rd * Cos[t], Rd * Sin[t]},
    {t, Pi/2 - arcHalf, Pi/2 + arcHalf, 0.8 * Degree}
  ];
  yBtm = -0.16;
  earthFillPoly = Polygon[Join[arcPts,
    {{Last[arcPts][[1]], yBtm}, {First[arcPts][[1]], yBtm}}]];

  sightEnd   = eyePt + 1.07 * (horizPt - eyePt);
  horizLeft  = eyePt + {-0.90, 0.0};
  horizRight = eyePt + {0.30, 0.0};

  hMarkX = 0.08;
  tickLen = 0.032;

  rAngle = -50 * Degree;
  rPtOnSurf = {Rd * Sin[-rAngle], Rd * Cos[-rAngle]};

  dipArcR = 0.175;
  sightAngle = ArcTan[dirSight[[1]], dirSight[[2]]];
  dipArcPts = Table[
    eyePt + dipArcR * {Cos[t], Sin[t]},
    {t, sightAngle, -Pi, -0.5 * Degree}
  ];
  dipLabelAngle = (sightAngle + (-Pi)) / 2;
  dipLabelOff   = dipArcR + 0.10;
  dipLabelPos   = eyePt + dipLabelOff * {Cos[dipLabelAngle], Sin[dipLabelAngle]};

  rDirH = Normalize[horizPt];
  tDirH = {-rDirH[[2]], rDirH[[1]]};
  sqSz  = 0.042;
  boxPts = {horizPt + sqSz*rDirH,
            horizPt + sqSz*rDirH + sqSz*tDirH,
            horizPt + sqSz*tDirH};

  sightMidPos   = (eyePt + horizPt) / 2 + {-0.02, 0.09};
  hLabelPos     = {hMarkX + 0.065, (Rd + d) / 2};
  rLabelPos     = 0.55 * rPtOnSurf + {0.10, 0.02};
  earthLabelPos = {0.0, 0.58};

  diag = Graphics[
    {
      {colSky, Rectangle[{-1.38, Rd - 0.002}, {0.55, 1.60}]},
      {colEarth, earthFillPoly},
      {colSea, Thickness[0.007], Line[arcPts]},

      {colBlue, Dashing[{0.015, 0.010}], Thickness[0.0045],
       Line[{horizLeft, horizRight}]},
      {colWarm, Thickness[0.0055],
       Line[{eyePt, sightEnd}]},

      {GrayLevel[0.35], Thickness[0.003],
       Line[{{hMarkX, Rd}, {hMarkX, d}}]},
      {GrayLevel[0.35], Thickness[0.003],
       Line[{{hMarkX - tickLen, Rd}, {hMarkX + tickLen, Rd}}]},
      {GrayLevel[0.35], Thickness[0.003],
       Line[{{hMarkX - tickLen, d}, {hMarkX + tickLen, d}}]},

      {GrayLevel[0.55], Dashing[{0.01, 0.008}], Thickness[0.003],
       Line[{{0.0, 0.0}, rPtOnSurf}]},

      {colWarm, Thickness[0.006], Line[dipArcPts]},

      {GrayLevel[0.35], Thickness[0.003], Line[boxPts]},

      {colBlue, PointSize[0.024], Point[eyePt]},
      {colNavy, PointSize[0.019], Point[horizPt]},
      {GrayLevel[0.6], PointSize[0.012], Point[{0.0, 0.0}]},

      Text[Style["True horizontal", FontSize -> 11, FontColor -> colBlue,
                 FontFamily -> "Helvetica", FontSlant -> Italic],
           {-0.82, d + 0.045}, {-1, 0}],

      Text[Style["Sea horizon", FontSize -> 11, FontColor -> colSea,
                 FontFamily -> "Helvetica"],
           horizPt + {-0.06, -0.09}, {1, 0}],

      Text[Style["dip  d", Bold, FontSize -> 13, FontColor -> colWarm,
                 FontFamily -> "Helvetica"],
           dipLabelPos, {0, 0}],

      Text[Style["h", FontSize -> 13, FontColor -> GrayLevel[0.25],
                 FontFamily -> "Helvetica", FontSlant -> Italic],
           hLabelPos, {-1, 0}],

      Text[Style["R", FontSize -> 13, FontColor -> GrayLevel[0.50],
                 FontFamily -> "Helvetica", FontSlant -> Italic],
           rLabelPos, {0, 0}],

      Text[Style["D \[TildeEqual] 2.07 \[Times] \[Sqrt]h  nm",
                 FontSize -> 11, FontColor -> colWarm,
                 FontFamily -> "Helvetica"],
           sightMidPos, {0, 0}],

      Text[Style["Eye", FontSize -> 11, FontColor -> colBlue,
                 FontFamily -> "Helvetica"],
           eyePt + {0.07, 0.035}, {-1, 0}],

      Text[Style["C", FontSize -> 11, FontColor -> GrayLevel[0.60],
                 FontFamily -> "Helvetica", FontSlant -> Italic],
           {0.0, 0.0} + {0.06, -0.06}, {-1, 0}],

      Text[Style["Ocean", FontSize -> 11, FontColor -> GrayLevel[0.88],
                 FontFamily -> "Helvetica"],
           earthLabelPos, {0, 0}],

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

  heights  = Range[0.1, 25.0, 0.1];
  dipVals  = cnDip /@ heights;
  distVals = 2.07 * Sqrt[heights];

  cases = {
    {"Dinghy (~1 m)",        1.0,  0.28,  0.18},
    {"Yacht deck (~3 m)",    3.0,  0.28,  0.20},
    {"Ship bridge (~20 m)", 20.0, -0.70, -0.55}
  };

  sens2m = 0.88 / Sqrt[2.0];
  sens2mStr = ToString[Round[sens2m, 0.01]];

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
        Text[Style["dip(\[Prime]) = 1.76 \[Times] \[Sqrt]h  (h in m)",
                   FontSize -> 11, FontColor -> colBlue,
                   FontFamily -> "Helvetica", FontSlant -> Italic],
             {0.6, 8.8}, {-1, 1}],
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

  combined
];


sxFigRefraction[] := Module[
  {bgColor, axColor, colBennett, colSaemundsson, colSimple, colPTenv,
   altMin, altMax, alts, rBennett, rSaemundsson, rSimple, ptsBennett,
   ptsSaemundsson, ptsSimple, annotLine, annotText, panel1, altsPosErr,
   modelGapNm, ptSwingNm, ptsModelGap, ptsPTSwing, threshLine, threshText,
   panel2, legend1, legend2, title1, title2, col1, col2, combined},

  bgColor   = White;
  axColor   = GrayLevel[0.2];
  colBennett    = RGBColor[0.122, 0.467, 0.706];
  colSaemundsson = RGBColor[0.839, 0.153, 0.157];
  colSimple     = RGBColor[0.172, 0.627, 0.172];
  colPTenv      = RGBColor[0.580, 0.404, 0.741];

  altMin = 0.5; altMax = 60.0;
  alts = Range[altMin, altMax, 0.25];

  rBennett     = Clip[cnRefraction[#], {0, 50}] & /@ alts;
  rSaemundsson = Clip[cnRefractionSaemundsson[#], {0, 50}] & /@ alts;
  rSimple      = Clip[cnRefractionSimple[#], {0, 50}] & /@ alts;

  ptsBennett     = Transpose[{alts, rBennett}];
  ptsSaemundsson = Transpose[{alts, rSaemundsson}];
  ptsSimple      = Transpose[{alts, rSimple}];

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

  altsPosErr = Range[1.0, 60.0, 0.5];

  modelGapNm = (Abs[cnRefractionSaemundsson[#] - cnRefractionSimple[#]]) & /@ altsPosErr;
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

  combined
];
(* fragB.wl -- Figure functions B: cocked-hat theorem, CRLB, chronometer.
   Each is a zero-arg function returning a figure expression (no Export). *)

(* ============================================================================
   sxFigCockedHatTheorem -- Cocked-hat 25% theorem Monte-Carlo demonstration.
   Deterministic via the script's own SeedRandom[2024] (seeds the whole
   Module body, including the later RandomSample for the example panel).
   Returns the assembled composite Image (the script's fullImg).
   ============================================================================ *)
sxFigCockedHatTheorem[] := Module[
  {blue, green, red, navy, mid, gridC, frameC, bg, textC, titleDir, labelDir,
   toImg, txtImg, truth, times, bodies, sightOpts, nFig, trials, insideFrac,
   toXY, insideT, outsideT, nIn, nOut, display12, rng, makeMini, miniPanels,
   leftGrid, leftW, leftTitleImg, leftSubImg, leftGridImg, leftCaptImg, leftCol,
   insideVec, ns, runFrac, sePlus, seMinus, bandPts, rightW, rightPlot, tgtW,
   gapW, leftFull, rightFull, lh, rh, maxH, leftPad, rightPad, gapStrip, bodyRow,
   bodyW, mainTitle, subTitle, bodyFinal, fullImg},

  (* Colour palette (light theme) *)
  blue  = RGBColor[0.25, 0.45, 0.95];
  green = RGBColor[0.20, 0.68, 0.35];
  red   = RGBColor[0.82, 0.20, 0.20];
  navy  = RGBColor[0.13, 0.29, 0.53];
  mid   = GrayLevel[0.45];
  gridC = Directive[GrayLevel[0.88], Dashed];
  frameC = GrayLevel[0.85];
  bg    = White;
  textC = RGBColor[0.13, 0.29, 0.53];
  titleDir = Directive[FontFamily->"Helvetica", FontSize->18, Bold, FontColor->textC];
  labelDir = Directive[FontFamily->"Helvetica", FontSize->16, FontColor->textC];

  (* Helpers: pixel-exact image ops *)
  toImg[expr_, w_] := ImageResize[Rasterize[expr, Background -> bg], w];

  (* Render on a canvas the FULL target width w (not a fixed 500 px) so long
     titles/captions are not clipped before the final resize. *)
  txtImg[txt_, w_, h_, fsize_, bold_:False] := ImageResize[
    Rasterize[
      Graphics[
        {Text[Style[txt, Directive[FontFamily->"Helvetica", FontSize->fsize,
                                    If[bold, Bold, Plain], FontColor->textC]],
               Scaled[{0.5, 0.5}]]},
        Background -> bg, ImageSize -> {w, h}, PlotRangeClipping -> False],
      Background -> bg],
    w];

  (* Simulation parameters *)
  truth    = {20.0, -40.0};
  times    = {DateObject[{2024,11,22,10, 0,0}, TimeZone->0],
              DateObject[{2024,11,22,13, 0,0}, TimeZone->0],
              DateObject[{2024,11,22,16, 0,0}, TimeZone->0]};
  bodies   = {"Sun", "Sun", "Sun"};
  sightOpts = <|"sigmaMin" -> 1.5|>;

  (* Run Monte-Carlo (seed once; do NOT re-seed per sight) *)
  nFig = 500;
  SeedRandom[2024];

  trials = Table[
    Module[{lops, hat, verts, inside},
      lops = MapThread[Function[{body, ti},
        cnReduceSightBody[
          cnGenerateSightBody[truth, ti, body, sightOpts],
          ti, truth, body, sightOpts]],
        {bodies, times}];
      hat    = cnCockedHat[lops];
      verts  = hat["vertices"];
      inside = cnCockedHatContains[truth, lops];
      <|"verts" -> verts, "inside" -> inside|>
    ],
    {nFig}
  ];

  insideFrac = N[Total[Boole[#["inside"]] & /@ trials] / nFig];

  (* Tangent-plane projection centred on truth *)
  toXY[{lat_, lon_}] := {(lon - truth[[2]]) Cos[truth[[1]] Degree] 60,
                          (lat - truth[[1]]) 60};

  (* LEFT PANEL: 3 x 4 grid of 12 example triangles *)
  insideT  = Select[trials, #["inside"] &];
  outsideT = Select[trials, ! #["inside"] &];
  nIn      = Min[3, Length[insideT]];
  nOut     = Min[9, Length[outsideT]];
  display12 = RandomSample[Join[Take[insideT, nIn], Take[outsideT, nOut]], nIn + nOut];

  rng = 6.5;   (* half-range in nm *)

  makeMini[t_] := Module[{verts, inside, pts2d, col},
    verts  = t["verts"];
    inside = t["inside"];
    pts2d  = toXY /@ verts;
    col    = If[inside, green, red];
    Graphics[{
      {FaceForm[Opacity[0.20, col]], EdgeForm[None], Polygon[pts2d]},
      {col, AbsoluteThickness[1.8],
       Line[{pts2d[[1]], pts2d[[2]], pts2d[[3]], pts2d[[1]]}]},
      {GrayLevel[0.65], AbsolutePointSize[5], Point /@ pts2d},
      {col,   AbsolutePointSize[11], Point[{0., 0.}]},
      {White, AbsolutePointSize[5],  Point[{0., 0.}]}
    },
    PlotRange   -> {{-rng, rng}, {-rng, rng}},
    Background  -> bg,
    Frame       -> True,
    FrameStyle  -> Directive[col, AbsoluteThickness[2.5]],
    FrameTicks  -> None,
    ImageSize   -> {185, 185},
    ImagePadding -> {{3,3},{3,3}}
    ]
  ];

  miniPanels = makeMini /@ display12;

  leftGrid = GraphicsGrid[
    Partition[miniPanels, 4],
    Spacings   -> {3, 3},
    Background -> bg,
    ImageSize  -> {790, 600}
  ];

  (* Left panel column: title + grid + caption, all at leftW pixels wide *)
  leftW = 790;

  leftTitleImg  = txtImg["~12 example trials from the simulation", leftW, 30, 13, True];
  leftSubImg    = txtImg[
    "GREEN = truth inside  |  RED = truth outside  |  dot marks true position",
    leftW, 24, 11];
  leftGridImg   = toImg[leftGrid, leftW];
  leftCaptImg   = txtImg[
    "Expect ~25% green triangles  |  Daniels 1951: exactly 1/4 for any 3 " <>
    "independent, symmetric, zero-mean bearings",
    leftW, 26, 11];

  leftCol = ImageAssemble[
    {{leftTitleImg}, {leftGridImg}, {leftSubImg}, {leftCaptImg}},
    Background -> bg];

  (* RIGHT PANEL: running P(inside) converging to 0.25 *)
  insideVec = Boole[#["inside"]] & /@ trials;
  ns        = N @ Range[1, nFig];
  runFrac   = N @ Table[Total[Take[insideVec, i]] / i, {i, 1, nFig}];

  (* +-2 SE band polygon: upper edge left-to-right, lower edge right-to-left *)
  sePlus  = 0.25 + 2 Sqrt[0.25 * 0.75 / ns];
  seMinus = 0.25 - 2 Sqrt[0.25 * 0.75 / ns];
  bandPts = Join[
    Transpose[{ns, sePlus}],
    Reverse[Transpose[{ns, seMinus}]]];

  rightW = 1600 - leftW - 4;   (* 4 px gap *)

  rightPlot = ListLinePlot[
    Transpose[{ns, runFrac}],
    PlotStyle   -> Directive[blue, AbsoluteThickness[2.5]],
    Frame       -> True,
    FrameLabel  -> {
      Style["Number of trials  n", labelDir],
      Style["Running P(truth inside cocked hat)", labelDir]
    },
    FrameStyle  -> Directive[frameC, AbsoluteThickness[1]],
    GridLines   -> {Automatic, {0.15, 0.20, 0.25, 0.30, 0.35}},
    GridLinesStyle -> gridC,
    PlotRange   -> {{0, nFig}, {0.05, 0.65}},
    Background  -> bg,
    PlotLabel   -> Style[
      "Convergence of running P(inside)  |  \[Sigma] = 1.5\[CloseCurlyQuote] per sight  |  n = " <> ToString[nFig],
      titleDir],
    ImageSize   -> {rightW, 700},
    ImagePadding -> {{82, 25}, {65, 65}},
    Epilog -> {
      (* Shaded +-2 SE band *)
      {Opacity[0.18, red], Polygon[bandPts]},
      (* 0.25 dashed reference line *)
      {Directive[navy, Dashed, AbsoluteThickness[2]],
       Line[{{1., 0.25}, {N[nFig], 0.25}}]},
      (* P = 1/4 label *)
      Text[Style["P = 1/4 = 0.25  (Daniels 1951)",
                 Directive[FontFamily->"Helvetica", FontSize->12, Bold, FontColor->navy]],
           {nFig * 0.44, 0.263}, {Left, Bottom}],
      (* +-2 SE band label *)
      Text[Style["\[PlusMinus]2 SE",
                 Directive[FontFamily->"Helvetica", FontSize->11, FontColor->red]],
           {nFig * 0.08, 0.25 + 2 Sqrt[0.25*0.75/(nFig*0.08)] + 0.02}, {Left, Bottom}],
      (* Final value *)
      Text[Style["n=" <> ToString[nFig] <> ":  P = " <>
                 ToString[NumberForm[insideFrac, {4,3}]],
                 Directive[FontFamily->"Helvetica", FontSize->12, Bold, FontColor->blue]],
           {nFig * 0.82, insideFrac + 0.04}, {Center, Bottom}],
      (* Counter-intuitive warning *)
      Text[Style["A tight cocked hat does NOT mean a reliable fix!",
                 Directive[FontFamily->"Helvetica", FontSize->11, Italic, FontColor->mid]],
           {nFig * 0.50, 0.60}, {Center, Bottom}]
    }
  ];

  (* ASSEMBLE two panels side by side then add title rows. *)
  tgtW = 1600;
  gapW = 4;

  leftFull  = leftCol;                         (* already exact leftW wide    *)
  rightFull = toImg[rightPlot, rightW];        (* forced to exact rightW wide *)

  (* Match heights by padding the shorter panel at the bottom *)
  lh = ImageDimensions[leftFull][[2]];
  rh = ImageDimensions[rightFull][[2]];
  maxH = Max[lh, rh];
  leftPad  = ImagePad[leftFull,  {{0,0},{0, Max[0, maxH-lh]}}, bg];
  rightPad = ImagePad[rightFull, {{0,0},{0, Max[0, maxH-rh]}}, bg];
  gapStrip = ConstantImage[bg, {gapW, maxH}];

  bodyRow = ImageAssemble[{{leftPad, gapStrip, rightPad}}, Background -> bg];
  bodyW = ImageDimensions[bodyRow][[1]];

  (* Title strips at tgtW = 1600 px *)
  mainTitle = txtImg[
    "The Cocked-Hat 25% Theorem  \[LongDash]  " <>
    "P(truth inside triangle) = 1/4  regardless of bearings or triangle size",
    tgtW, 48, 15, True];
  subTitle = txtImg[
    "Three Sun sights at daytime azimuths SE / S / WSW  |  Daniels 1951  |  " <>
    "A tight cocked hat does NOT guarantee a reliable fix\n" <>
    "Theorem assumes INDEPENDENT symmetric errors, well-separated bearings, NO common systematic bias " <>
    "(a constant index error or refraction offset shifts all three LOPs together and breaks the 1/4 rule)",
    tgtW, 50, 11];

  (* Resize bodyRow to tgtW if body width drifted slightly from rounding *)
  bodyFinal = If[bodyW === tgtW, bodyRow,
                 ImageResize[bodyRow, tgtW]];

  fullImg = ImageAssemble[
    {{mainTitle}, {subTitle}, {bodyFinal}},
    Background -> bg];

  (* Crop the 2-px macOS pink perimeter ring, re-pad white *)
  fullImg = ImagePad[ImagePad[fullImg, -3], 3, White];

  fullImg
];


(* ============================================================================
   sxFigCRLB -- Fisher information / Cramer-Rao bound figure.
   Deterministic via cnMonteCarloFix "seed"->7. Returns the Graphics fig.
   ============================================================================ *)
sxFigCRLB[] := Module[
  {truePos, sigma, t1, t2, t3, times, azimuths, cov, crlbEll, mcResult, mcEll,
   toEN, enPts, s95, ellipsePoints, crlbEllPts, mcEllPts, maxR, colScatter,
   colNavy, colOrange, colGray, pCoord, legX0, legDX, legendItems, azStr,
   fmtNm, statsStr, statsAnnot, titleAnnot, tpLabel, fig},

  (* Scenario *)
  truePos = {20., -40.};       (* 20 N, 40 W *)
  sigma   = 1.5;               (* sight sigma in arcminutes = nm along LOP *)
  t1 = DateObject[{2024, 11, 15, 10, 0, 0}, TimeZone -> 0];
  t2 = DateObject[{2024, 11, 15, 14, 0, 0}, TimeZone -> 0];
  t3 = DateObject[{2024, 11, 15, 17, 0, 0}, TimeZone -> 0];
  times = {t1, t2, t3};

  (* CRLB *)
  azimuths  = cnComputedAltitude[truePos, cnSunGP[#]][[2]] & /@ times;
  cov       = cnCRLBCovariance[azimuths, sigma];
  crlbEll   = cnErrorEllipse[cov];

  (* Monte-Carlo fix (n = 3000) *)
  mcResult = cnMonteCarloFix[truePos, times,
               <|"sigmaMin" -> sigma, "seed" -> 7|>, 3000];
  mcEll    = cnErrorEllipse[mcResult["covNm"]];

  (* Convert fixes to local E/N errors (nm) *)
  toEN[fix_] := {(fix[[2]] - truePos[[2]]) Cos[truePos[[1]] Degree] 60,
                 (fix[[1]] - truePos[[1]]) 60};
  enPts = toEN /@ mcResult["fixes"];

  (* Build 95% ellipse point lists *)
  s95 = Sqrt[-2 Log[0.05]];

  ellipsePoints[cmat_, scale_] := Module[
    {eig, vals, vecs, order, l1, l2, v1, v2},
    eig   = Eigensystem[N[cmat]];
    vals  = eig[[1]]; vecs = eig[[2]];
    order = Ordering[vals, All, Greater];
    l1 = vals[[order[[1]]]]; l2 = vals[[order[[2]]]];
    v1 = vecs[[order[[1]]]]; v2 = vecs[[order[[2]]]];
    N @ Table[scale Sqrt[l1] Cos[t] v1 + scale Sqrt[l2] Sin[t] v2,
      {t, 0, 2 Pi, 2 Pi / 360}]
  ];

  crlbEllPts = ellipsePoints[cov,              s95];
  mcEllPts   = ellipsePoints[mcResult["covNm"], s95];

  (* Axis range — generous margin: circle occupies the central ~64%, leaving clean
     empty strips at top (title + stats) and bottom (legend) so no text overlaps. *)
  maxR = 1.55 Max[
    Max[Abs @ Flatten[enPts]],
    Max[Abs @ Flatten[crlbEllPts]],
    Max[Abs @ Flatten[mcEllPts]]
  ];

  (* Colour palette *)
  colScatter  = RGBColor[0.53, 0.81, 0.98, 0.35];   (* light blue, semi-transparent *)
  colNavy     = RGBColor[0.13, 0.24, 0.56];          (* solid navy - empirical *)
  colOrange   = RGBColor[1.0,  0.55, 0.0 ];          (* orange - CRLB *)
  colGray     = GrayLevel[0.55];

  (* Annotation helpers *)
  pCoord[rx_, ry_] := {maxR (2 rx - 1), maxR (2 ry - 1)};

  (* Legend (bottom-left, inside the empty lower strip, white-backed) *)
  legX0 = 0.035; legDX = 0.05;
  legY  = {0.15, 0.095, 0.04};
  legendItems = {
    {colOrange, Dashing[{0.02, 0.01}], Thickness[0.004],
      Line[{pCoord[legX0, legY[[1]]], pCoord[legX0 + legDX, legY[[1]]]}]},
    {Black, Text[Style["CRLB 95% ellipse  (Cram\[EAcute]r\[Dash]Rao bound)",
        FontSize -> 26, FontFamily -> "Helvetica", Background -> White],
      pCoord[legX0 + legDX + 0.012, legY[[1]]], {-1, 0}]},
    {colNavy, Thickness[0.004],
      Line[{pCoord[legX0, legY[[2]]], pCoord[legX0 + legDX, legY[[2]]]}]},
    {Black, Text[Style["Empirical 95% ellipse  (Monte-Carlo)",
        FontSize -> 26, FontFamily -> "Helvetica", Background -> White],
      pCoord[legX0 + legDX + 0.012, legY[[2]]], {-1, 0}]},
    {RGBColor[0.35, 0.70, 0.92], PointSize[0.018],
      Point[pCoord[legX0 + legDX/2, legY[[3]]]]},
    {Black, Text[Style["MC fixes (n = 3000)",
        FontSize -> 26, FontFamily -> "Helvetica", Background -> White],
      pCoord[legX0 + legDX + 0.012, legY[[3]]], {-1, 0}]}
  };

  (* Stats box (top-left, in the empty upper strip above the circle) *)
  azStr    = StringRiffle[ToString[Round[#, 0.1]] & /@ azimuths, "\[Degree], "] <> "\[Degree]";
  fmtNm[x_] := ToString[N[Round[x, 0.01]]];
  statsStr = "\[Sigma] = 1.5\[CloseCurlyQuote]   |   Azimuths: " <> azStr <>
             "\nCRLB CEP = " <> fmtNm[crlbEll["cep"]] <> " nm,   MC CEP = " <>
               fmtNm[mcResult["cep"]] <> " nm    (95% \[Rule] \[Chi]^2(2) = 5.991)" <>
             "\nRandom-noise floor; assumes perfect ephemeris \[LongDash] real fixes dominated by systematics";
  statsAnnot = {Black,
    Text[Style[statsStr, FontSize -> 26, FontFamily -> "Helvetica", Background -> White],
      pCoord[0.02, 0.915], {-1, 1}]};

  (* Title *)
  titleAnnot = {Black,
    Text[Style[
      "Fisher Information and Cram\[EAcute]r\[Dash]Rao Bound  \[LongDash]  " <>
      "Three-Sight Sun Fix  \[LongDash]  20\[Degree]N, 40\[Degree]W",
      FontSize -> 34, FontWeight -> "Bold", FontFamily -> "Helvetica", Background -> White],
    pCoord[0.5, 0.975]]};

  (* True-position marker label *)
  tpLabel = {Black,
    Text[Style["True position", FontSize -> 26, FontFamily -> "Helvetica"],
      {0.12 maxR, -0.08 maxR}, {-1, 0}]};

  (* Assemble Graphics *)
  fig = Graphics[
    Flatten[{
      {GrayLevel[0.88], Thin,
        Line /@ Table[{{x, -maxR}, {x, maxR}}, {x, -maxR, maxR, maxR/4}],
        Line /@ Table[{{-maxR, y}, {maxR, y}}, {y, -maxR, maxR, maxR/4}]},
      {colGray, Dashing[{0.010, 0.006}], Thickness[0.0015],
        Line[{{-maxR, 0}, {maxR, 0}}],
        Line[{{0, -maxR}, {0, maxR}}]},
      {colScatter, PointSize[0.004], Point[enPts]},
      {colOrange, Dashing[{0.020, 0.010}], Thickness[0.004],
        Line[Append[crlbEllPts, crlbEllPts[[1]]]]},
      {colNavy, Thickness[0.004],
        Line[Append[mcEllPts, mcEllPts[[1]]]]},
      {Black, PointSize[0.015], Point[{0., 0.}]},
      tpLabel,
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

  fig
];


(* ============================================================================
   sxFigChronometer -- chronometer-error longitude sensitivity figure.
   Deterministic (no randomness). Returns the Column fig.
   ============================================================================ *)
sxFigChronometer[] := Module[
  {blue, green, orange, red, gray, lw, lats, cols, names, labelPos, labelAnchor,
   leftPlot, truePos, sunTimes, bodies, biasVals, results, eastPts, northPts,
   rightPlot, caption, fig},

  (* Palette *)
  blue   = RGBColor[0.122, 0.467, 0.706];
  green  = RGBColor[0.173, 0.627, 0.173];
  orange = RGBColor[0.890, 0.467, 0.008];
  red    = RGBColor[0.839, 0.153, 0.157];
  gray   = GrayLevel[0.55];
  lw     = Directive[AbsoluteThickness[2.2]];

  (* LEFT PANEL -- sensitivity formula: 0.25 cos(lat) nm/s for 4 latitudes *)
  lats  = {0, 30, 45, 60};
  cols  = {blue, green, orange, red};
  names = {"0\[Degree] (equator)", "30\[Degree]", "45\[Degree]", "60\[Degree]"};

  labelPos = Table[{61, 0.25 Cos[lat Degree] * 60}, {lat, lats}];
  labelAnchor = {-0.1, 0};

  leftPlot = Show[
    Plot[
      Evaluate[Table[0.25 Cos[lat Degree] t, {lat, lats}]],
      {t, 0, 60},
      PlotRange   -> {{0, 60}, {0, 17}},
      PlotStyle   -> (Directive[lw, #] & /@ cols),
      Frame       -> True,
      FrameLabel  -> {
        {"Longitude error  (nm)", None},
        {"Chronometer error  (s)",
         "Chronometer \[LongDash] Longitude Sensitivity"}},
      FrameStyle  -> Directive[Black, AbsoluteThickness[1]],
      GridLines   -> Automatic,
      GridLinesStyle -> Directive[GrayLevel[0.85]],
      Background  -> White,
      ImageSize   -> {760, 560}
    ],
    Graphics[
      MapThread[
        Function[{pos, col, name},
          {col, Text[Style[name, FontSize -> 13, FontFamily -> "Arial"],
                     pos, {-1.05, 0}]}],
        {labelPos, cols, names}]
    ],
    Graphics[{
      {Directive[Black, AbsolutePointSize[7]], Point[{4, 1.0}]},
      Text[Style["4 s = 1 nm", FontSize -> 12, FontFamily -> "Arial",
                 FontWeight -> Bold],
           {4, 1.0}, {-0.15, 1.5}]
    }]
  ];

  (* RIGHT PANEL -- fix error components vs clock bias (3 Sun sights, lat 15 N) *)
  truePos = {15.0, -40.0};
  sunTimes = {DateObject[{2024, 11, 15, 10, 0, 0}, TimeZone -> 0],
              DateObject[{2024, 11, 15, 15, 0, 0}, TimeZone -> 0],
              DateObject[{2024, 11, 15, 19, 0, 0}, TimeZone -> 0]};
  bodies   = {"Sun", "Sun", "Sun"};

  biasVals = Range[-60, 60, 5];
  results  = cnChronometerFixError[truePos, sunTimes, bodies, #] & /@ biasVals;
  eastPts  = Transpose[{biasVals, #["eastNm"] & /@ results}];
  northPts = Transpose[{biasVals, #["northNm"] & /@ results}];

  rightPlot = Show[
    ListLinePlot[
      {eastPts, northPts},
      PlotStyle   -> {Directive[lw, blue], Directive[lw, red, Dashing[{0.015, 0.01}]]},
      PlotRange   -> {{-60, 60}, {-17, 17}},
      Frame       -> True,
      FrameLabel  -> {
        {"Fix error  (nm)", None},
        {"Clock bias  (s)",
         "Fix Error vs Clock Bias  (lat 15\[Degree]N, 3-Sun fix)"}},
      FrameStyle  -> Directive[Black, AbsoluteThickness[1]],
      GridLines   -> {Range[-60, 60, 20], Range[-15, 15, 5]},
      GridLinesStyle -> Directive[GrayLevel[0.85]],
      Background  -> White,
      ImageSize   -> {760, 560}
    ],
    Graphics[{Directive[gray, AbsoluteThickness[1], Dashing[{0.02, 0.01}]],
              Line[{{-60, 0}, {60, 0}}]}],
    Graphics[{
      {blue, Text[Style["East (longitude)", FontSize -> 13, FontFamily -> "Arial"],
                  {-28, -10.5}, {0, 0}]},
      {red,  Text[Style["North (latitude)", FontSize -> 13, FontFamily -> "Arial"],
                  {-28, -2.2}, {0, 0}]}
    }]
  ];

  (* COMPOSE *)
  caption = "Harrison\[CloseCurlyQuote]s H4 chronometer (1759) solved the Longitude Problem by keeping " <>
            "time to \[PlusMinus]5 s/month \[LongDash] limiting longitude error to \[PlusMinus]1.2 nm (equator).";

  (* Stacked vertically (was a 3.4:1 side-by-side row unreadable at ~620 px). *)
  fig = Column[
    {
      GraphicsColumn[{leftPlot, rightPlot}, Spacings -> 12, Background -> White],
      Style[caption, FontSize -> 16, FontFamily -> "Arial",
            FontColor -> GrayLevel[0.25], TextAlignment -> Center]
    },
    Alignment -> Center,
    Background -> White,
    ImageSize  -> 820
  ];

  fig
];
(* fragC.wl — figure functions: noon sight, star selection, star chart *)

sxFigNoonSight[] := Module[
  {csvPath, rawRows, dataRows, dayNums, latTrue, lonTrue, nRows,
   sigmaMin, noonLatitudes, errNm, xVals, latPlot, errPlot,
   titleRow, subtitleRow, fig},

  SeedRandom[12345];

  (* ── Load voyage data ── *)
  csvPath = sxDataFile["voyage.csv"];
  rawRows = Import[csvPath, "CSV"];
  dataRows = rawRows[[2 ;;]];
  (* Columns: day, datetimeUTC, latTrue, lonTrue, latDR, lonDR, courseDeg, speedKts *)
  dayNums = dataRows[[All, 1]];
  latTrue = dataRows[[All, 3]];
  lonTrue = dataRows[[All, 4]];
  nRows = Length[dataRows];

  (* ── Simulate noon sights with ~1' noise ── *)
  sigmaMin = 1.0;   (* arcmin noise *)

  noonLatitudes = Table[
    Module[{lat, lon, date, tLAN, decT, hoMer, brg, noise, hoObs},
      lat  = latTrue[[i]];
      lon  = lonTrue[[i]];
      date = DateObject[{2024, 11, 15 + dayNums[[i]]}, TimeZone -> 0];
      tLAN = cnLANTimeUTC[lon, date];
      decT = cnSunGP[tLAN][[1]];
      {hoMer, brg} = cnMeridianAltitude[lat, decT];
      noise = RandomVariate[NormalDistribution[0, sigmaMin / 60.0]];
      hoObs = hoMer + noise;
      cnNoonLatitude[hoObs, decT, brg]
    ],
    {i, nRows}
  ];

  (* ── Latitude errors in nautical miles ── *)
  errNm = (noonLatitudes - latTrue) * 60.0;   (* 1 deg lat = 60 nm *)

  (* ── Build the figure ── *)
  xVals = dayNums;

  (* Panel 1: true latitude vs noon-sight latitude *)
  latPlot = ListLinePlot[
    {Transpose[{xVals, latTrue}], Transpose[{xVals, noonLatitudes}]},
    PlotStyle -> {{Thick, Darker[Blue]}, {Thick, Dashed, Darker[Orange]}},
    PlotLegends -> Placed[
      LineLegend[
        {Directive[Thick, Darker[Blue]], Directive[Thick, Dashed, Darker[Orange]]},
        {"True latitude", "Noon-sight latitude"},
        LegendMarkerSize -> 24,
        LabelStyle -> Directive[14, FontFamily -> "Helvetica"],
        LegendFunction -> (Framed[#, Background -> White,
                                    FrameStyle -> GrayLevel[0.7]] &)
      ],
      {0.80, 0.20}
    ],
    PlotRange -> All,
    Frame -> True,
    FrameLabel -> {None, Style["Latitude (\[Degree])", 16, Black]},
    FrameTicksStyle -> 14,
    GridLines -> Automatic,
    GridLinesStyle -> Directive[GrayLevel[0.85], Dashed],
    Background -> White,
    ImageSize -> {860, 330},
    ImagePadding -> {{75, 25}, {12, 20}}
  ];

  (* Panel 2: latitude error in nautical miles *)
  errPlot = ListLinePlot[
    Transpose[{xVals, errNm}],
    PlotStyle -> {Thick, Darker[Red]},
    PlotRange -> All,
    Frame -> True,
    FrameLabel -> {
      Style["Day of voyage", 16, Black],
      Style["Lat error (nm)", 16, Black]
    },
    FrameTicksStyle -> 14,
    GridLines -> {{}, {0}},
    GridLinesStyle -> Directive[GrayLevel[0.7], Dashed],
    Background -> White,
    Epilog -> {
      Text[
        Style[
          "Latitude (noon sight): time-insensitive",
          13, Black, FontFamily -> "Helvetica"
        ],
        Scaled[{0.97, 0.90}], {1, 1}
      ],
      Text[
        Style[
          "Longitude: 0.25\[Prime]/s \[RightArrow] needs a chronometer",
          13, GrayLevel[0.35], FontFamily -> "Helvetica"
        ],
        Scaled[{0.97, 0.76}], {1, 1}
      ]
    },
    ImageSize -> {860, 300},
    ImagePadding -> {{75, 25}, {55, 12}}
  ];

  (* Combine panels vertically *)
  titleRow = Style[
    "Noon-Sight Latitude Along the Voyage",
    19, Bold, Black, FontFamily -> "Helvetica"
  ];
  subtitleRow = Style[
    "Meridian altitude (noon sight) recovers latitude with ~1\[Prime] sextant noise; " <>
    "immune to chronometer error (only max altitude needed, not time).",
    14, GrayLevel[0.3], FontFamily -> "Helvetica"
  ];

  (* Explicit ImageSize added on both plots + Column: they had none, so they
     rendered as tiny cramped panels with a clipped legend and dead whitespace. *)
  fig = Column[
    {titleRow, subtitleRow, Spacer[6], latPlot, errPlot},
    Alignment -> Left,
    Background -> White,
    ImageSize -> 860,
    Spacings -> {0, 0.4}
  ];

  fig
];


sxFigStarSelection[] := Module[
  {pos, t, allStarData, visible, result, bestStars, bestGDOP, bestAz,
   clustGDOP, starXY, colStar, colBest, colGrid, colHorizon, colLabel,
   colMinAlt, dotR, altAngles, altCircles, minAltRing, azLines,
   horizonCircle, compassLabels, altLabels, spokePrims, sortedAz, gaps,
   gapMidAz, gapAnnotations, starDots, labelStars, starLabels,
   titleLine1, titleLine2, bestAnnotation, theoryAnnotation, gfx},

  (* ---- observation parameters ---- *)
  pos  = {15, -40};
  t    = DateObject[{2025, 6, 21, 22, 0, 0}, TimeZone -> 0];

  allStarData = cnLoadStars[];
  visible     = cnVisibleStars[pos, t, 15];
  result      = cnBestStarTriplet[pos, t, 15];
  bestStars   = result["stars"];
  bestGDOP    = result["gdop"];
  bestAz      = result["azimuths"];

  (* ---- clustered reference GDOP ---- *)
  clustGDOP = cnFixGDOP[pos, t, {"Kochab", "Alkaid", "Alioth"}];

  (* ---- polar alt-az coordinate helper ---- *)
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

  (* ---- dot size: brighter star -> larger dot ---- *)
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
    {colLabel, Text[Style["N", FontFamily -> "Helvetica", FontSize -> 14, Bold],
      {0., 1.10}]},
    {colLabel, Text[Style["E", FontFamily -> "Helvetica", FontSize -> 14, Bold],
      {1.10, 0.}]},
    {colLabel, Text[Style["S", FontFamily -> "Helvetica", FontSize -> 14, Bold],
      {0., -1.10}]},
    {colLabel, Text[Style["W", FontFamily -> "Helvetica", FontSize -> 14, Bold],
      {-1.10, 0.}]}};

  (* ---- altitude labels along the East spoke (Az=90 deg) ---- *)
  altLabels = Table[
    With[{r = (90. - a) / 90.},
      {colLabel, Text[Style[ToString[a] <> "\[Degree]",
        FontFamily -> "Helvetica", FontSize -> 13],
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
          FontFamily -> "Helvetica", FontSize -> 13, Bold],
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
        {col, Text[Style[s, FontFamily -> "Helvetica", FontSize -> 13,
          If[inBest, Bold, Plain]],
          xy + 0.07 dir, {-1, 0}]}]],
    {s, labelStars}];

  (* ---- title and annotation (Text, not Inset) ---- *)
  titleLine1 = Text[
    Style["Alt-Az Sky Chart \[LongDash] GDOP Optimal Star Selection",
      FontFamily -> "Helvetica", FontSize -> 20, Bold, GrayLevel[0.1]],
    {0., 1.26}];

  titleLine2 = Text[
    Style["Pos: 15\[Degree]N 40\[Degree]W  \[LongDash]  2025-Jun-21 22:00 UTC  \[LongDash]  19 stars above 15\[Degree]",
      FontFamily -> "Helvetica", FontSize -> 15, GrayLevel[0.25]],
    {0., 1.185}];

  bestAnnotation = Text[
    Style["Best triplet: " <> StringRiffle[bestStars, ", "] <>
      "   GDOP = " <> ToString[NumberForm[N[bestGDOP, 3], {3, 3}]] <>
      "  (vs cluster GDOP = " <> ToString[NumberForm[N[clustGDOP, 3], {3, 2}]] <> ")",
      FontFamily -> "Helvetica", FontSize -> 15, Bold, colBest],
    {0., -1.19}];

  theoryAnnotation = Text[
    Style["Theoretical minimum (3 bodies, 120\[Degree] apart): GDOP = \[Sqrt](4/3) \[TildeEqual] 1.155",
      FontFamily -> "Helvetica", FontSize -> 14, Italic, GrayLevel[0.35]],
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
    ImageSize    -> 1080,
    Background   -> White,
    ImagePadding -> {{30, 30}, {50, 50}}
  ];

  gfx
];


sxFigStarChart[] := Module[
  {stars, starNames, nStars, sortedByMag, nNorth, nSouth, nUnique,
   bgCol, starCol, glowDir, equCol, eclCol, gridDir, labelCol, ariesCol,
   poleCol, titleCol, northXY, southXY, dotR, glowR, eps, eclTable,
   eclPtsN, eclPtsS, raGrid, decGridN, decGridS, raHourLabels,
   decLabelsN, decLabelsS, buildStarPrims, northStarPrims, southStarPrims,
   buildStarLabels, northStarLabels, southStarLabels, ariesMarker,
   polePrim, bgDisk, hemiTitle, legendPrims, plotRange, northGfx,
   southGfx, combined, mainTitle, caption, finalFig},

  (* ── Load star data ── *)
  stars     = cnLoadStars[];
  starNames = Keys[stars];
  nStars    = Length[starNames];

  (* Sort brightest first (lowest magnitude) *)
  sortedByMag = SortBy[starNames, stars[#]["mag"] &];

  (* Coverage check *)
  nNorth  = Length[Select[starNames, stars[#]["decDeg"] >= -30. &]];
  nSouth  = Length[Select[starNames, stars[#]["decDeg"] <=  30. &]];
  nUnique = Length[Union[
    Select[starNames, stars[#]["decDeg"] >= -30. &],
    Select[starNames, stars[#]["decDeg"] <=  30. &]]];

  (* ── Colour palette (dark-sky theme) ── *)
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

  (* ── Projection: polar equidistant ── *)
  northXY[raDeg_?NumericQ, decDeg_?NumericQ] :=
    With[{r = (90. - decDeg) / 90., th = raDeg * Degree},
      {r * Sin[th], r * Cos[th]}];

  southXY[raDeg_?NumericQ, decDeg_?NumericQ] :=
    With[{r = (90. + decDeg) / 90., th = raDeg * Degree},
      {-r * Sin[th], r * Cos[th]}];

  (* ── Star dot radius ── *)
  dotR[mag_]  := Max[0.012, 0.046 - 0.012 * mag];
  glowR[mag_] := 2.20 * dotR[mag];

  (* ── Ecliptic in equatorial coordinates ── *)
  eps = 23.44 * Degree;
  eclTable = Table[
    Module[{lam = l * Degree, raE, decE},
      raE  = Mod[ArcTan[Cos[lam], Cos[eps] * Sin[lam]], 2 Pi] / Degree;
      decE = ArcSin[Sin[eps] * Sin[lam]] / Degree;
      {raE, decE}],
    {l, 0, 362, 2}];   (* slight overshoot to close the curve *)

  eclPtsN = northXY[#[[1]], #[[2]]] & /@ eclTable;
  eclPtsS = southXY[#[[1]], #[[2]]] & /@ eclTable;

  (* ── Grid ── *)
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

  (* ── RA hour labels at equator ── *)
  raHourLabels[projFn_] := Table[
    With[{pos = 1.09 * projFn[h * 15., 0.]},
      {Directive[RGBColor[0.55, 0.60, 0.78], FontSize -> 9],
       Text[Style[ToString[h] <> "h",
         FontFamily -> "Helvetica", FontSize -> 9,
         FontColor -> RGBColor[0.55, 0.60, 0.78]],
         pos, {0, 0}]}],
    {h, 0, 23, 2}];

  (* ── Dec tick labels ── *)
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

  (* ── Star primitives (glow + dot) ── *)
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

  (* ── Star labels for bright stars (mag < 1.6) ── *)
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

  (* ── First Point of Aries ── *)
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

  (* ── Pole markers ── *)
  polePrim[label_] := {
    {poleCol, Disk[{0., 0.}, 0.015]},
    Text[Style[label,
      FontFamily -> "Helvetica", FontSize -> 10, Bold,
      FontColor -> poleCol],
      {0., 0.05}, {0, -1}]
  };

  (* ── Background disc ── *)
  bgDisk = {bgCol, Disk[{0., 0.}, 1.14]};

  (* ── Per-hemisphere title and legend text ── *)
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

  (* ── Assemble each hemisphere ── *)
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

  (* ── Main title and caption ── *)
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

  finalFig
];
(* ============================================================
   sxFigErrorBudget  ─  from wolfram/error_budget.wls
   ============================================================ *)
sxFigErrorBudget[] := Module[
  {cBlue, cNavy, cWarm, cRed, cGreen, cGray,
   lat, nmPerSec, ptHalfNm, modelNm, refrNm, dipNm, rss, srcKeys, calcSc,
   sc1, sc2, sc3, allSc, scTotals, gpsNm,
   baseVals, baseTotal, ord, sVals, sKeys, srcColor, srcLabel, n, maxX, barH,
   gridXs, gridEls, barEls, valEls, rssLineEl, rssLabelEl, yTicks, xTicks, panelA,
   rowLabels, scHdr, nR, cw, ch, nC, lw, hdrH, tblVals, colTint, colFg, maxVal, warnNm,
   hdrLblBg, hdrLblTxt, hdrColBgs, hdrColTxts, hdrElems, dataElems,
   yB, isTot, rowBg, v, xL, cellBg, barW, valColor,
   stripY, stripH, totW, maxTot, stripBg, stripLbl, stripBars, stripTxts, gpsTick,
   typRatio, footerTxt, panelB, dashboard},

  cBlue  = RGBColor[0.25, 0.45, 0.95];
  cNavy  = RGBColor[0.13, 0.29, 0.53];
  cWarm  = RGBColor[0.85, 0.50, 0.20];
  cRed   = RGBColor[0.82, 0.20, 0.20];
  cGreen = RGBColor[0.20, 0.68, 0.35];
  cGray  = GrayLevel[0.55];

  lat      = 15.0;
  nmPerSec = cnLongitudeErrorPerSecond[lat];

  ptHalfNm[h_] := (cnRefractionPT[h, 1040, -10] - cnRefractionPT[h, 980, 30]) / 2.0;
  modelNm[h_]  := Abs[cnRefraction[h] - cnRefractionSaemundsson[h]];
  refrNm[h_]   := Sqrt[ptHalfNm[h]^2 + modelNm[h]^2];

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

  (* PANEL A: Horizontal tornado bar chart *)
  baseVals  = sc2;
  baseTotal = scTotals[[2]];

  ord   = Ordering[baseVals];
  sVals = baseVals[[ord]];
  sKeys = srcKeys[[ord]];

  srcColor = AssociationThread[srcKeys -> {cBlue, cWarm, cGreen, cNavy}];

  srcLabel = <|
    "Sextant"     -> "Sextant  (0.5\[Prime] \[Times] GDOP 1.5)",
    "Chronometer" -> "Chronometer  (5 s, lat 15\[Degree])",
    "Refraction"  -> "Refraction  (P/T \[PlusMinus]20\[Degree]C / 30 hPa, alt 25\[Degree])",
    "Dip"         -> "Dip  (\[PlusMinus]1 m eye-height, h = 3 m)"
  |>;

  n    = Length[sVals];
  maxX = Max[sVals] * 1.95;   (* extra right room for the RSS-total label *)
  barH = 0.50;

  gridXs  = Select[Range[0.0, 2.5, 0.5], # <= maxX &];   (* fewer ticks: no squish *)
  gridEls = {
    Directive[GrayLevel[0.86], Dashed, AbsoluteThickness[1]],
    Line /@ ({{#, 0.28}, {#, n + 0.56}} & /@ gridXs)
  };

  barEls = Table[{
    Opacity[0.92], srcColor[sKeys[[i]]],
    Rectangle[{0, i - barH}, {sVals[[i]], i + barH}, RoundingRadius -> 0.04]
  }, {i, n}];

  (* White-on-bar for wide bars; dark text right of short bars for legibility. *)
  valEls = Table[
    With[{txt = ToString[NumberForm[sVals[[i]], {3, 2}]] <> " nm"},
      If[sVals[[i]] > 0.62,
        Text[Style[txt, Bold, 13, FontFamily -> "Helvetica", FontColor -> White],
          {sVals[[i]] - 0.02, i}, {1, 0}],
        Text[Style[txt, Bold, 13, FontFamily -> "Helvetica",
                   FontColor -> srcColor[sKeys[[i]]]],
          {sVals[[i]] + 0.03, i}, {-1, 0}]]],
    {i, n}];

  rssLineEl  = {cRed, Dashed, AbsoluteThickness[2.5],
                Line[{{baseTotal, 0.26}, {baseTotal, n + 0.42}}]};
  rssLabelEl = Text[
    Style[
      "RSS total: " <> ToString[NumberForm[baseTotal, {4, 2}]] <> " nm",
      Bold, 12, FontFamily -> "Helvetica", FontColor -> cRed, Background -> White],
    {baseTotal + maxX * 0.02, n + 0.28}, {-1, 0}];

  yTicks = Table[
    {i, Style[srcLabel[sKeys[[i]]], 12, FontFamily -> "Helvetica",
              FontColor -> GrayLevel[0.20]]},
    {i, n}];
  xTicks = Table[
    {x, Style[ToString[x] <> " nm", 11, FontColor -> GrayLevel[0.45]]},
    {x, gridXs}];

  panelA = Graphics[
    {gridEls, barEls, valEls, rssLineEl, rssLabelEl},
    Frame        -> True,
    FrameStyle   -> Directive[GrayLevel[0.65], AbsoluteThickness[1]],
    FrameTicks   -> {{yTicks, None}, {xTicks, None}},
    PlotRange    -> {{0, maxX}, {0.22, n + 0.78}},
    PlotLabel    -> Style[
      "(a)  Error contributions \[LongDash] typical 3-star fix (GDOP 1.5)\n" <>
      "Systematic terms dominate; MC floor (~1.1 nm) is the random-noise limit",
      15, Bold, FontFamily -> "Helvetica", FontColor -> cNavy],
    Background   -> White,
    ImagePadding -> {{290, 45}, {58, 92}},
    ImageSize    -> {780, 500}
  ];

  (* PANEL B: Scenario comparison — colour-coded table *)
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

  hdrLblBg  = {FaceForm[GrayLevel[0.93]], EdgeForm[GrayLevel[0.72]],
               Rectangle[{0, nR * ch}, {lw, nR * ch + hdrH}]};
  hdrLblTxt = Text[Style["Error source", Bold, 13, FontFamily -> "Helvetica",
                          FontColor -> GrayLevel[0.25]],
                    {lw / 2, nR * ch + hdrH / 2}];

  hdrColBgs = Table[
    {FaceForm[Lighter[colTint[[c]], 0.10]], EdgeForm[GrayLevel[0.72]],
     Rectangle[{lw + (c - 1) * cw, nR * ch},
               {lw + c * cw, nR * ch + hdrH}]},
    {c, nC}];

  hdrColTxts = Table[
    Text[Style[scHdr[[c]], Bold, 12, FontFamily -> "Helvetica", FontColor -> colFg[[c]]],
         {lw + (c - 0.5) * cw, nR * ch + hdrH / 2}],
    {c, nC}];

  hdrElems = Join[{hdrLblBg, hdrLblTxt}, hdrColBgs, hdrColTxts];

  dataElems = {};
  Do[
    yB     = (nR - r) * ch;
    isTot  = (r == nR);
    rowBg  = If[OddQ[r], GrayLevel[0.97], White];

    AppendTo[dataElems,
      {FaceForm[If[isTot, GrayLevel[0.91], rowBg]], EdgeForm[GrayLevel[0.74]],
       Rectangle[{0, yB}, {lw, yB + ch}]}];
    AppendTo[dataElems,
      If[isTot,
         Text[Style[rowLabels[[r]], Bold, 13, FontFamily -> "Helvetica", FontColor -> cNavy],
              {lw / 2, yB + ch / 2}],
         Text[Style[rowLabels[[r]], 12, FontFamily -> "Helvetica",
                    FontColor -> GrayLevel[0.22]],
              {lw / 2, yB + ch / 2}]]];

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
           Text[Style[ToString[NumberForm[v, {3, 2}]] <> " nm", Bold, 13,
                      FontFamily -> "Helvetica", FontColor -> valColor],
                {xL + cw / 2, yB + ch / 2}],
           Text[Style[ToString[NumberForm[v, {3, 2}]] <> " nm", 12,
                      FontFamily -> "Helvetica", FontColor -> valColor],
                {xL + cw / 2, yB + ch / 2}]]],
      {c, nC}],
    {r, nR}];

  (* GPS reference lives in the footer text (~0.003 nm — too small to show as a
     strip tick without colliding with the best-case bar), so no in-strip mark. *)
  stripY = -1.40;  stripH = 0.60;
  totW   = lw + nC * cw;
  maxTot = Max[scTotals];

  stripBg  = {FaceForm[GrayLevel[0.95]], EdgeForm[GrayLevel[0.80]],
              Rectangle[{0, stripY - 0.05}, {totW, stripY + stripH + 0.05}]};
  stripLbl = Text[Style["Total RSS:", Bold, 12, FontFamily -> "Helvetica",
                         FontColor -> cNavy],
                  {lw / 2, stripY + stripH / 2}];

  stripBars = Table[
    Module[{xL = lw + (c - 1) * cw, bw = (scTotals[[c]] / maxTot) * (cw - 0.18) * 0.95},
      {Opacity[0.90], colFg[[c]],
       Rectangle[{xL + 0.06, stripY + 0.11},
                 {xL + 0.06 + bw, stripY + stripH - 0.11},
                 RoundingRadius -> 0.04]}],
    {c, nC}];

  stripTxts = Table[
    Module[{xL = lw + (c - 1) * cw, bw = (scTotals[[c]] / maxTot) * (cw - 0.18) * 0.95,
            mid = stripY + stripH / 2, txt},
      txt = ToString[NumberForm[scTotals[[c]], {4, 2}]] <> " nm";
      If[bw > 0.62,
        Text[Style[txt, Bold, 12, FontFamily -> "Helvetica", FontColor -> White],
             {xL + 0.06 + bw - 0.05, mid}, {1, 0}],
        Text[Style[txt, Bold, 12, FontFamily -> "Helvetica", FontColor -> colFg[[c]]],
             {xL + 0.06 + bw + 0.05, mid}, {-1, 0}]]],
    {c, nC}];

  gpsTick = {};

  typRatio = Round[scTotals[[2]] / gpsNm, 100];
  footerTxt = Text[
    Style[
      "GPS CEP \[TildeEqual] " <> ToString[NumberForm[gpsNm, {7, 5}]] <>
      " nm (5 m) \[LongDash] a skilled celestial fix is \[TildeEqual] " <>
      ToString[typRatio] <> "\[Times] larger.\n" <>
      "MC floor (~1.1 nm) understates real accuracy; " <>
      "the systematic terms above dominate.",
      Italic, 10, FontFamily -> "Helvetica", FontColor -> cGray],
    {totW / 2, stripY - 0.46}];

  panelB = Graphics[
    Join[hdrElems, dataElems, {stripBg, stripLbl}, stripBars, stripTxts, {footerTxt}],
    PlotRange    -> {{-0.02, totW + 0.02}, {stripY - 0.92, nR * ch + hdrH + 0.06}},
    PlotLabel    -> Style[
      "(b)  Fix error by scenario (nm per source, bars proportional to value)",
      15, Bold, FontFamily -> "Helvetica", FontColor -> cNavy],
    Background   -> White,
    ImagePadding -> {{20, 20}, {45, 60}},
    ImageSize    -> {760, 500}
  ];

  (* Stacked vertically (was a 3.1:1 side-by-side grid unreadable at ~620 px);
     each panel is now full width. *)
  dashboard = GraphicsGrid[
    {{panelA}, {panelB}},
    ImageSize  -> 900,
    Spacings   -> {0, Scaled[0.02]},
    Background -> White
  ];

  dashboard
];

(* ============================================================
   sxFigSpeeds  ─  from wolfram/speeds.wls
   ============================================================ *)
sxFigSpeeds[] := Module[
  {cBlue, cNavy, cWarm, cGreen, cGray, cGridL, cFrame, dirT, dirL, dirA,
   rawData, dataRows, days, latTrue, lonTrue, latDR, lonDR, plannedCourse,
   n, nIntervals, dest, geoDist, geoDir, distNm, sogKts, cogDeg,
   drBearingToDest, vmgKts, distToDestNm,
   nTrue, nDRv, mLat, eTrue, eDRv,
   currNorthKts, currEastKts, currDrift, currSet,
   meanSOG, meanVMG, meanDrift, meanSet, fmt,
   intDays, sogData, vmgData, distData, driftData, panelA, panelB,
   driftMax, driftMin, setMin, setMax, setToDrift, setScaledData, rightTicks,
   driftP, setP, panelCBase, panelC, fig},

  cBlue  = RGBColor[0.25, 0.45, 0.95];
  cNavy  = RGBColor[0.13, 0.29, 0.53];
  cWarm  = RGBColor[0.85, 0.50, 0.20];
  cGreen = RGBColor[0.20, 0.68, 0.35];
  cGray  = GrayLevel[0.45];
  cGridL = Directive[GrayLevel[0.88], Dashed];
  cFrame = GrayLevel[0.82];

  dirT = Directive[FontFamily -> "Helvetica", Bold, FontSize -> 21, FontColor -> cNavy];
  dirL = Directive[FontFamily -> "Helvetica", FontSize -> 19, FontColor -> cNavy];
  dirA = Directive[FontFamily -> "Helvetica", Italic, FontSize -> 16, FontColor -> cGray];

  rawData  = Import[sxDataFile["voyage.csv"], "CSV"];
  dataRows = rawData[[2;;]];

  days          = N[dataRows[[All, 1]]];
  latTrue       = N[dataRows[[All, 3]]];
  lonTrue       = N[dataRows[[All, 4]]];
  latDR         = N[dataRows[[All, 5]]];
  lonDR         = N[dataRows[[All, 6]]];
  plannedCourse = N[dataRows[[All, 7]]];

  n          = Length[dataRows];
  nIntervals = n - 1;

  dest = {14.07, -60.95};

  geoDist[p1_, p2_] := QuantityMagnitude[UnitConvert[
    GeoDistance[GeoPosition[p1], GeoPosition[p2]], "NauticalMiles"]];

  geoDir[p1_, p2_] := QuantityMagnitude[
    GeoDirection[GeoPosition[p1], GeoPosition[p2]]];

  distNm = Table[
    geoDist[{latTrue[[i]], lonTrue[[i]]}, {latTrue[[i+1]], lonTrue[[i+1]]}],
    {i, 1, nIntervals}];
  sogKts = distNm / 24.0;

  cogDeg = Table[
    geoDir[{latTrue[[i]], lonTrue[[i]]}, {latTrue[[i+1]], lonTrue[[i+1]]}],
    {i, 1, nIntervals}];

  drBearingToDest = Table[
    geoDir[{latDR[[i]], lonDR[[i]]}, dest],
    {i, 1, nIntervals}];

  vmgKts = Table[
    sogKts[[i]] * Cos[(plannedCourse[[i]] - drBearingToDest[[i]]) * Degree],
    {i, 1, nIntervals}];

  distToDestNm = Table[
    geoDist[{latTrue[[i]], lonTrue[[i]]}, dest],
    {i, 1, n}];

  currNorthKts = Table[
    nTrue = (latTrue[[i+1]] - latTrue[[i]]) * 60.0;
    nDRv  = (latDR[[i+1]]   - latDR[[i]])   * 60.0;
    (nTrue - nDRv) / 24.0,
    {i, 1, nIntervals}];

  currEastKts = Table[
    mLat  = (latTrue[[i]] + latTrue[[i+1]]) / 2.0;
    eTrue = (lonTrue[[i+1]] - lonTrue[[i]]) * Cos[mLat * Degree] * 60.0;
    eDRv  = (lonDR[[i+1]]   - lonDR[[i]])   * Cos[mLat * Degree] * 60.0;
    (eTrue - eDRv) / 24.0,
    {i, 1, nIntervals}];

  currDrift = Table[
    Sqrt[currEastKts[[i]]^2 + currNorthKts[[i]]^2],
    {i, 1, nIntervals}];

  currSet = Table[
    Mod[90.0 - ArcTan[currEastKts[[i]], currNorthKts[[i]]] / Degree, 360.0],
    {i, 1, nIntervals}];

  meanSOG   = Mean[sogKts]   // N;
  meanVMG   = Mean[vmgKts]   // N;
  meanDrift = Mean[currDrift] // N;
  meanSet   = Mean[currSet]   // N;

  fmt[x_, d_:2] := ToString[NumberForm[N[x], {5, d}]];

  intDays   = days[[;; -2]];
  sogData   = Transpose[{intDays, sogKts}];
  vmgData   = Transpose[{intDays, vmgKts}];
  distData  = Transpose[{days, distToDestNm}];
  driftData = Transpose[{intDays, currDrift}];

  panelA = ListLinePlot[
    {sogData, vmgData},
    PlotStyle -> {
      Directive[cBlue, Thickness[0.003]],
      Directive[cWarm, Thickness[0.003], Dashed]
    },
    PlotMarkers -> {{"\[FilledCircle]", 9}, {"\[FilledUpTriangle]", 9}},
    Frame -> True,
    FrameStyle -> cFrame,
    PlotLabel -> Style[
      "(a)  Speed over ground and VMG to Rodney Bay", dirT],
    FrameLabel -> {
      {Style["Speed (kt)", dirL], None},
      {Style["Day of passage", dirL], None}
    },
    PlotRange -> {{-0.5, 23.5}, {0, 6.5}},
    GridLines -> {Table[i, {i, 0, 23}], Table[i, {i, 0, 6}]},
    GridLinesStyle -> cGridL,
    PlotLegends -> Placed[
      LineLegend[
        {Directive[cBlue, Thickness[0.004]],
         Directive[cWarm, Thickness[0.004], Dashed]},
        {"SOG (true track, kt)",
         "VMG nav: 5 kt \[Times] cos(\[CapitalDelta]bearing) (kt)"},
        LabelStyle -> dirA,
        LegendLabel -> None
      ],
      {0.72, 0.30}
    ],
    Epilog -> {
      {Opacity[0.30], cBlue, Dashed,
       Line[{{-0.5, meanSOG}, {23.5, meanSOG}}]},
      Text[Style["mean SOG = " <> fmt[meanSOG] <> " kt",
                 dirA, FontColor -> cBlue],
           {4, meanSOG + 0.25}],
      {Opacity[0.30], cWarm, Dashed,
       Line[{{-0.5, meanVMG}, {23.5, meanVMG}}]},
      Text[Style["mean VMG = " <> fmt[meanVMG] <> " kt",
                 dirA, FontColor -> cWarm],
           {4, meanVMG - 0.30}],
      Text[Style["VMG \[LessEqual] SOG; gap widens as DR course diverges from bearing to dest",
                 dirA, FontColor -> cGray],
           {10.5, 1.0}]
    },
    Background -> White,
    AspectRatio -> Full,            (* fill the wide ImageSize — no centred margins *)
    ImageSize -> {1120, 480}
  ];

  panelB = ListLinePlot[
    distData,
    PlotStyle -> Directive[cNavy, Thickness[0.003]],
    PlotMarkers -> {{"\[FilledCircle]", 8}},
    Frame -> True,
    FrameStyle -> cFrame,
    PlotLabel -> Style[
      "(b)  Closing the gap -- distance remaining to Rodney Bay", dirT],
    FrameLabel -> {
      {Style["Distance (nm)", dirL], None},
      {Style["Day of passage", dirL], None}
    },
    PlotRange -> {{-0.5, 23.5}, {0, 3200}},
    GridLines -> {Table[i, {i, 0, 23}], Automatic},
    GridLinesStyle -> cGridL,
    Filling -> Bottom,
    FillingStyle -> Directive[Opacity[0.10], cNavy],
    Background -> White,
    AspectRatio -> Full,
    ImageSize -> {1120, 440}
  ];

  driftMax = Ceiling[Max[currDrift] * 1.25 * 10] / 10;
  driftMin = 0.0;
  setMin   = 0.0;
  setMax   = 360.0;

  setToDrift = Function[s, (s - setMin) / (setMax - setMin) * (driftMax - driftMin) + driftMin];

  setScaledData = Transpose[{intDays, setToDrift /@ currSet}];

  rightTicks = Table[
    {setToDrift[s], ToString[s] <> "\[Degree]"},
    {s, 0, 360, 45}];

  driftP = ListLinePlot[
    driftData,
    PlotStyle -> Directive[cWarm, Thickness[0.004]],
    PlotMarkers -> {{"\[FilledCircle]", 8}},
    Frame -> True,
    FrameStyle -> cFrame,
    PlotLabel -> Style[
      "(c)  Estimated current -- set and drift from DR discrepancy", dirT],
    FrameLabel -> {
      {Style["Drift (kt)", dirL, FontColor -> cWarm],
       Style["Set (\[Degree])", dirL, FontColor -> cNavy]},
      {Style["Day of passage", dirL], None}
    },
    PlotRange -> {{-0.5, 23.5}, {driftMin, driftMax}},
    GridLines -> {Table[i, {i, 0, 23}], Automatic},
    GridLinesStyle -> cGridL,
    FrameTicks -> {
      {Automatic, rightTicks},
      {Range[0, 22, 2], None}
    },
    Epilog -> {
      {Opacity[0.30], cWarm, Dashed,
       Line[{{-0.5, meanDrift}, {23.5, meanDrift}}]},
      Text[Style["mean drift = " <> fmt[meanDrift] <> " kt",
                 dirA, FontColor -> cWarm],
           {4.0, meanDrift + driftMax * 0.07}],
      {Opacity[0.25], cNavy, Dashed,
       Line[{{-0.5, setToDrift[meanSet]}, {23.5, setToDrift[meanSet]}}]},
      Text[Style["mean set = " <> ToString[Round[meanSet, 1]] <> "\[Degree] (SSE)",
                 dirA, FontColor -> cNavy],
           {18.0, setToDrift[meanSet] + driftMax * 0.07}]
    },
    Background -> White,
    AspectRatio -> Full,
    ImageSize -> {1120, 440}
  ];

  setP = ListLinePlot[
    setScaledData,
    PlotStyle -> Directive[cNavy, Thickness[0.003], Dashed],
    PlotMarkers -> {{"\[FilledUpTriangle]", 8}},
    Frame -> False,
    Axes -> False,
    PlotRange -> {{-0.5, 23.5}, {driftMin, driftMax}},
    AspectRatio -> Full,
    ImageSize -> {1120, 440},
    Background -> None
  ];

  panelCBase = Show[driftP, setP, Background -> White];

  panelC = Legended[
    panelCBase,
    Placed[
      LineLegend[
        {Directive[cWarm, Thickness[0.004]],
         Directive[cNavy, Thickness[0.004], Dashed]},
        {"Drift (kt)", "Set (\[Degree])"},
        LabelStyle -> dirA
      ],
      {0.88, 0.30}
    ]
  ];

  fig = Grid[
    {{panelA}, {panelB}, {panelC}},
    Spacings  -> {0, Scaled[0.006]},
    Background -> White
  ];

  fig
];

(* ============================================================
   sxFigTwilightReplay  ─  from wolfram/twilight_replay.wls
   In-memory deterministic computation preserved (seeded
   cnGenerateSightBody calls); JSON Export dropped entirely.
   fixJSON supplied by package contract (not redefined here).
   ============================================================ *)
sxFigTwilightReplay[] := Module[
  {rawCSV, header, rows, voyage, sightsData, sunErrMap,
   baseDate, dtToStr, rd, geoDist,
   colBlue, colNavy, colWarm, colGreen, colGrid, colLabel,
   twilightRecs, skipped,
   v, dayIdx, truePos, drPos, noonHrs, dayMidnight, scanDts, scanTs, sunAlts,
   bestIdx, twT, twAlt, visible, trip, stars, gdop, azs, lops, fix, errNm, cht,
   meanErr, meanGDOP,
   validDays, twErrors, sunErrors, meanSunErr, panelA,
   medGDOP, repRec, repDay, repStars, repVoyRow, repPosTrue, noonHrsRep,
   dayMidRep, scanTsRep, sunAltsRep, twTrep, allStarData, repDrRow, repDrPos,
   visRep, repAzs, starXYrep, altCircPrims, azLinePrims, horizCirc, minAltRing,
   compassLbls, altLbls, spokePrims, dotR, starDotPrims, labelSet, starLblPrims,
   sortedAz, gaps, gapMidAzs, gapAnnotPrims, titleB1, titleB2, titleB3,
   panelB, combined},

  rawCSV = Import[sxDataFile["voyage.csv"], "CSV"];
  header = First[rawCSV];
  rows   = Rest[rawCSV];
  voyage = Association[Thread[header -> #]] & /@ rows;

  sightsData = fixJSON[Import[sxDataFile["sights.json"], "JSON"]];
  sunErrMap  = Association[#["day"] -> #["runningFixErrorNm"] & /@ sightsData["days"]];

  baseDate = DateObject[{2024, 11, 15, 0, 0, 0}, TimeZone -> 0];
  dtToStr[t_] := DateString[t,
    {"Year","-","Month","-","Day","T","Hour24",":","Minute",":","Second"},
    TimeZone -> 0] <> "Z";
  rd[x_?NumericQ, n_:4] := N[Round[N[x], 10^-n]];
  geoDist[p1_, p2_] := QuantityMagnitude[
    GeoDistance[GeoPosition[p1], GeoPosition[p2]] / Quantity[1, "NauticalMiles"]];

  colBlue  = RGBColor[0.25, 0.45, 0.95];
  colNavy  = RGBColor[0.13, 0.29, 0.53];
  colWarm  = RGBColor[0.85, 0.50, 0.20];
  colGreen = RGBColor[0.20, 0.68, 0.35];
  colGrid  = GrayLevel[0.85];
  colLabel = GrayLevel[0.12];

  twilightRecs = {};
  skipped      = {};

  Do[
    v       = voyage[[i]];
    dayIdx  = v["day"];
    truePos = {N[v["latTrue"]], N[v["lonTrue"]]};
    drPos   = {N[v["latDR"]],   N[v["lonDR"]]};

    noonHrs     = 12.0 - truePos[[2]] / 15.0;
    dayMidnight = DatePlus[baseDate, {dayIdx, "Days"}];

    scanDts = Range[2.0, 9.0, 1/12.0];
    scanTs  = DatePlus[dayMidnight, {noonHrs + #, "Hours"}] & /@ scanDts;
    sunAlts = cnAltitudeFromGP[truePos, cnSunGP[#]] & /@ scanTs;
    bestIdx = First[Ordering[Abs[sunAlts + 9.0], 1]];
    twT     = scanTs[[bestIdx]];
    twAlt   = sunAlts[[bestIdx]];

    If[Abs[twAlt + 9.0] > 4.5,
      AppendTo[skipped, <|"day" -> dayIdx,
        "reason" -> "Sun alt at closest scan = " <> ToString[N[twAlt, 3]] <> "\[Degree]"|>];
      Continue[]
    ];

    visible = cnVisibleStars[drPos, twT, 15];
    If[Length[visible] < 3,
      AppendTo[skipped, <|"day" -> dayIdx,
        "reason" -> ToString[Length[visible]] <> " stars visible (need >=3)"|>];
      Continue[]
    ];

    trip = cnBestStarTriplet[drPos, twT, 15];
    If[trip["gdop"] > 9000,
      AppendTo[skipped, <|"day" -> dayIdx, "reason" -> "Degenerate GDOP"|>];
      Continue[]
    ];

    stars = trip["stars"];
    gdop  = trip["gdop"];
    azs   = trip["azimuths"];

    lops = Table[
      Module[{body = {"Star", stars[[si]]}, hs},
        hs = cnGenerateSightBody[truePos, twT, body,
               <|"sigmaMin" -> 1.0, "seed" -> dayIdx * 100 + si|>];
        cnReduceSightBody[hs, twT, drPos, body]
      ], {si, 3}];

    fix   = cnFix[lops];
    errNm = geoDist[truePos, fix];
    cht   = cnCockedHat[lops];

    AppendTo[twilightRecs, <|
      "day"              -> dayIdx,
      "twilightUTC"      -> dtToStr[twT],
      "sunAltDeg"        -> rd[twAlt, 3],
      "nVisible"         -> Length[visible],
      "chosenStars"      -> stars,
      "gdop"             -> rd[gdop, 4],
      "azimuths"         -> Map[rd[#, 3] &, azs],
      "fix"              -> Map[rd[#, 5] &, fix],
      "errorNm"          -> rd[errNm, 3],
      "cockedHatAreaNm2" -> rd[cht["areaNm2"], 4]
    |>],
    {i, 1, Length[voyage]}
  ];

  meanErr  = N[Mean[#["errorNm"] & /@ twilightRecs]];
  meanGDOP = N[Mean[#["gdop"]    & /@ twilightRecs]];

  (* ---- Panel A data ---- *)
  validDays  = #["day"]     & /@ twilightRecs;
  twErrors   = #["errorNm"] & /@ twilightRecs;
  sunErrors  = sunErrMap /@ validDays;
  meanSunErr = N[Mean[sunErrors]];

  panelA = ListLinePlot[
    {Transpose[{validDays, twErrors}],
     Transpose[{validDays, sunErrors}]},
    PlotStyle -> {
      Directive[colBlue, AbsoluteThickness[2.5]],
      Directive[colWarm,  AbsoluteThickness[2.5], Dashing[{0.022, 0.012}]]
    },
    PlotMarkers -> {
      {Style["\[FilledCircle]", colBlue, 10], Scaled[0.015]},
      {Style["\[FilledSquare]", colWarm, 10], Scaled[0.015]}
    },
    Frame      -> True,
    FrameStyle -> Directive[GrayLevel[0.40], AbsoluteThickness[0.9]],
    FrameLabel -> {
      {Style["Fix error (nm)", FontFamily -> "Helvetica", FontSize -> 13, colNavy], None},
      {Style["Voyage day", FontFamily -> "Helvetica", FontSize -> 13, colNavy], None}
    },
    FrameTicks -> {
      {Automatic, None},
      {Table[{d, Style[ToString[d], FontFamily -> "Helvetica", FontSize -> 11, colLabel]},
         {d, 0, 23, 2}], None}
    },
    PlotRange   -> {{-0.5, 23.5}, {0, Automatic}},
    GridLines   -> {Range[0, 23, 2], Automatic},
    GridLinesStyle -> Directive[GrayLevel[0.88], AbsoluteThickness[0.6]],
    PlotLabel   -> Style[
      "Twilight Star Fix vs Sun Running Fix \[LongDash] Atlantic Crossing Nov\[Dash]Dec 2024",
      FontFamily -> "Helvetica", FontSize -> 14, Bold, colNavy],
    PlotLegends -> Placed[
      LineLegend[
        {Directive[colBlue, AbsoluteThickness[2.5]],
         Directive[colWarm,  AbsoluteThickness[2.5], Dashing[{0.022, 0.012}]]},
        {"Twilight star fix (3-star, real catalogue, optimal GDOP)",
         "Sun running fix (3 sights/day, 6 h spread)"},
        LegendFunction -> (Framed[#,
          FrameStyle -> GrayLevel[0.88], Background -> White, RoundingRadius -> 4] &),
        LabelStyle -> Directive[FontFamily -> "Helvetica", FontSize -> 11, colNavy]
      ], {0.68, 0.88}],
    Epilog -> {
      {colBlue, Opacity[0.7], AbsoluteThickness[1.1], Dashing[{0.018, 0.008}],
       Line[{{-0.5, meanErr}, {23.5, meanErr}}]},
      {colBlue, Text[
        Style["Mean \[Star] fix: " <> ToString[NumberForm[N[meanErr, 3], {3, 2}]] <> " nm",
          FontFamily -> "Helvetica", FontSize -> 10, colBlue],
        {0.5, meanErr + 0.13}, {-1, -1}]},
      {colWarm, Opacity[0.7], AbsoluteThickness[1.1], Dashing[{0.018, 0.008}],
       Line[{{-0.5, meanSunErr}, {23.5, meanSunErr}}]},
      {colWarm, Text[
        Style["Mean \[Sun] fix: " <> ToString[NumberForm[N[meanSunErr, 3], {3, 2}]] <> " nm",
          FontFamily -> "Helvetica", FontSize -> 10, colWarm],
        {0.5, meanSunErr + 0.13}, {-1, 1}]},
      {GrayLevel[0.35], Text[
        Style["Mean GDOP = " <> ToString[NumberForm[N[meanGDOP, 3], {3, 2}]],
          FontFamily -> "Helvetica", FontSize -> 10, Italic, GrayLevel[0.35]],
        {22.5, 0.18}, {1, -1}]}
    },
    Background -> White,
    ImageSize  -> {940, 500}
  ];

  (* ---- Panel B: polar sky chart for the day with median GDOP ---- *)
  medGDOP = N[Median[#["gdop"] & /@ twilightRecs]];
  repRec  = twilightRecs[[First[Ordering[Abs[#["gdop"] - medGDOP] & /@ twilightRecs, 1]]]];
  repDay  = repRec["day"];
  repStars = repRec["chosenStars"];

  repVoyRow   = SelectFirst[voyage, #["day"] == repDay &];
  repPosTrue  = {N[repVoyRow["latTrue"]], N[repVoyRow["lonTrue"]]};
  noonHrsRep  = 12.0 - repPosTrue[[2]] / 15.0;
  dayMidRep   = DatePlus[baseDate, {repDay, "Days"}];
  scanTsRep   = DatePlus[dayMidRep, {noonHrsRep + #, "Hours"}] & /@ Range[2.0, 9.0, 1/12.0];
  sunAltsRep  = cnAltitudeFromGP[repPosTrue, cnSunGP[#]] & /@ scanTsRep;
  twTrep      = scanTsRep[[First[Ordering[Abs[sunAltsRep + 9.0], 1]]]];

  allStarData = cnLoadStars[];
  repDrRow    = SelectFirst[voyage, #["day"] == repDay &];
  repDrPos    = {N[repDrRow["latDR"]], N[repDrRow["lonDR"]]};
  visRep      = cnVisibleStars[repDrPos, twTrep, 15];
  repAzs      = repRec["azimuths"];

  starXYrep[name_] := Module[{hcaz},
    hcaz = cnComputedAltitude[repDrPos, cnBodyGPFor[{"Star", name}, twTrep]];
    With[{r = (90. - hcaz[[1]]) / 90., az = hcaz[[2]]},
      {r Sin[az Degree], r Cos[az Degree]}]
  ];

  altCircPrims = Table[
    With[{r = (90. - a) / 90.},
      {colGrid, AbsoluteThickness[0.6], Circle[{0., 0.}, r]}],
    {a, {15, 30, 45, 60, 75}}];

  azLinePrims = Table[
    With[{az = a Degree},
      {colGrid, AbsoluteThickness[0.5], Line[{{0., 0.}, {Sin[az], Cos[az]}}]}],
    {a, 0, 330, 30}];

  horizCirc = {GrayLevel[0.5], AbsoluteThickness[1.4], Circle[{0., 0.}, 1.]};

  minAltRing = {Directive[colBlue, Opacity[0.35], AbsoluteThickness[1.1], Dashing[{0.013, 0.008}]],
    Circle[{0., 0.}, (90. - 15.) / 90.]};

  compassLbls = {
    {colLabel, Text[Style["N", FontFamily -> "Helvetica", FontSize -> 11, Bold], {0.,  1.13}]},
    {colLabel, Text[Style["E", FontFamily -> "Helvetica", FontSize -> 11, Bold], { 1.13, 0.}]},
    {colLabel, Text[Style["S", FontFamily -> "Helvetica", FontSize -> 11, Bold], {0., -1.13}]},
    {colLabel, Text[Style["W", FontFamily -> "Helvetica", FontSize -> 11, Bold], {-1.13, 0.}]}};

  altLbls = Table[
    With[{r = (90. - a) / 90.},
      {colLabel, Text[Style[ToString[a] <> "\[Degree]",
        FontFamily -> "Helvetica", FontSize -> 8], {r + 0.04, 0.03}, {-1, -1}]}],
    {a, {30, 45, 60}}];

  spokePrims = Table[
    With[{az = az0 Degree},
      {colWarm, Opacity[0.65], AbsoluteThickness[1.2], Dashing[{0.015, 0.010}],
       Line[{{0., 0.}, {Sin[az], Cos[az]}}]}],
    {az0, repAzs}];

  dotR[mag_] := Max[0.011, 0.035 - 0.008 mag];
  starDotPrims = Table[
    Module[{xy = starXYrep[s], mag = allStarData[s]["mag"], r, inBest},
      inBest = MemberQ[repStars, s];
      r = dotR[mag];
      {If[inBest, colWarm, colBlue],
       Disk[xy, If[inBest, r * 1.45, r]]}],
    {s, visRep}];

  labelSet = Union[repStars, Select[visRep, allStarData[#]["mag"] < 1.5 &]];
  starLblPrims = Table[
    Module[{xy = starXYrep[s], inBest = MemberQ[repStars, s]},
      With[{col = If[inBest, colWarm, colLabel],
            dir = Normalize[xy + {0.001, 0.}]},
        {col, Text[Style[s, FontFamily -> "Helvetica", FontSize -> 8,
          If[inBest, Bold, Plain]],
          xy + 0.07 dir, {-1, 0}]}]],
    {s, labelSet}];

  sortedAz  = Sort[N[repAzs]];
  gaps      = Mod[Differences[Append[sortedAz, sortedAz[[1]] + 360.]], 360.];
  gapMidAzs = Table[Mod[sortedAz[[k]] + gaps[[k]] / 2., 360.], {k, 3}];
  gapAnnotPrims = Table[
    With[{az = gapMidAzs[[k]] Degree, g = gaps[[k]]},
      If[g < 200.,
        {colWarm, Text[Style[ToString[Round[g, 0.1]] <> "\[Degree]",
          FontFamily -> "Helvetica", FontSize -> 9, Bold],
          0.38 {Sin[az], Cos[az]}]},
        Nothing]],
    {k, 3}];

  titleB1 = Text[Style[
    "Day " <> ToString[repDay] <> " \[LongDash] Evening Twilight Sky",
    FontFamily -> "Helvetica", FontSize -> 13, Bold, colNavy], {0., 1.31}];

  titleB2 = Text[Style[
    StringRiffle[repStars, ", "],
    FontFamily -> "Helvetica", FontSize -> 10, Bold, colWarm], {0., 1.215}];

  titleB3 = Text[Style[
    "GDOP = " <> ToString[NumberForm[N[repRec["gdop"], 3], {3, 2}]] <>
    "   err = " <> ToString[NumberForm[N[repRec["errorNm"], 3], {3, 2}]] <> " nm",
    FontFamily -> "Helvetica", FontSize -> 10, colNavy], {0., -1.27}];

  panelB = Graphics[
    {White, Rectangle[{-1.45, -1.45}, {1.45, 1.45}],
     azLinePrims, altCircPrims, minAltRing, horizCirc,
     spokePrims, starDotPrims, gapAnnotPrims, starLblPrims,
     compassLbls, altLbls,
     titleB1, titleB2, titleB3},
    PlotRange    -> {{-1.45, 1.45}, {-1.36, 1.36}},
    Background   -> White,
    ImageSize    -> {620, 500},
    ImagePadding -> {{25, 25}, {50, 50}}
  ];

  (* Stacked vertically (was a 3.15:1 side-by-side grid unreadable at ~620 px);
     each panel is now full width. *)
  combined = GraphicsGrid[
    {{panelA}, {panelB}},
    Spacings   -> {0, Scaled[0.02]},
    Background -> White,
    ImageSize  -> 940
  ];

  combined
];
(* fragE.wl - figure functions transformed from standalone scripts *)

sxFigEphemerisValidation[] := Module[
  {cBg, cText, cBlue, cNavy, cWarm, cMid, cGrid, cFrame,
   fTitle, fLabel, fAnnot, fSmall, fTiny, fmt,
   startDate, dates2024, testLocs, locLabels, locColors, wSunAppar,
   residualsByLoc, allResVals, medianRes, maxAbsRes, annotA,
   validSeries, resData, resCols, resLbls, panelA,
   stars, dtYr, pmData, pmNames, pmVals, nStars, top3,
   pmBarColors, pmBarLabels, top3str, panelB,
   epoch2024, precData, precVals, medianPrec, maxPrec,
   precBarLabels, precBarColors, panelC, captionStr, captionPanel, fig},

  Off[DateListPlot::ldata];

  cBg    = White;
  cText  = RGBColor[0.13, 0.29, 0.53];
  cBlue  = RGBColor[0.25, 0.45, 0.95];
  cNavy  = RGBColor[0.13, 0.29, 0.53];
  cWarm  = RGBColor[0.85, 0.50, 0.20];
  cMid   = GrayLevel[0.55];
  cGrid  = Directive[GrayLevel[0.88], Dashed];
  cFrame = GrayLevel[0.82];

  (* Fonts enlarged for legibility when embedded at ~620 px.  fTitle kept
     moderate: the long panel titles overflow the panel width if too large. *)
  fTitle = Directive[FontFamily -> "Helvetica", FontSize -> 16, Bold, FontColor -> cText];
  fLabel = Directive[FontFamily -> "Helvetica", FontSize -> 18, FontColor -> cText];
  fAnnot = Directive[FontFamily -> "Helvetica", FontSize -> 17, FontColor -> cMid];
  fSmall = Directive[FontFamily -> "Helvetica", FontSize -> 14, FontColor -> cText];
  fTiny  = Directive[FontFamily -> "Helvetica", FontSize -> 12, FontColor -> cText];

  fmt[x_, d_:2] := ToString[Round[N[x], 10.0^(-d)]];

  (* PANEL A *)
  startDate = DateObject[{2024, 1, 1, 12, 0, 0}, TimeZone -> 0];
  dates2024 = Table[DatePlus[startDate, {5(k-1), "Day"}], {k, 1, 73}];

  testLocs  = {{55.0, -5.0}, {10.0, -5.0}, {-40.0, -5.0}};
  locLabels = {"55\[Degree]N 5\[Degree]W (N Atlantic)", "10\[Degree]N 5\[Degree]W (Gulf of Guinea)", "40\[Degree]S 5\[Degree]W (S Atlantic)"};
  locColors = {cBlue, cNavy, cWarm};

  wSunAppar[loc_, t_] :=
    QuantityMagnitude[UnitConvert[SunPosition[GeoPosition[loc], t][[2]], "AngularDegrees"]];

  residualsByLoc = Table[
    Module[{loc = testLocs[[li]], pairs = {}, ourAlt, wAppar, wGeom, t},
      Do[
        t       = dates2024[[ki]];
        ourAlt  = cnAltitudeFromGP[loc, cnSunGP[t]];
        wAppar  = wSunAppar[loc, t];
        wGeom   = wAppar - cnRefraction[wAppar] / 60.0;
        If[ourAlt > 5.0 && wGeom > 5.0,
          AppendTo[pairs, {t, (ourAlt - wGeom) * 60.0}]
        ],
        {ki, Length[dates2024]}
      ];
      pairs
    ],
    {li, Length[testLocs]}
  ];

  allResVals = Flatten[Select[residualsByLoc, Length[#] > 0 &][[All, All, 2]]];
  medianRes  = Median[allResVals];
  maxAbsRes  = Max[Abs[allResVals]];

  annotA = "median " <> fmt[medianRes, 2] <> "\[Prime]   |max| " <>
           fmt[maxAbsRes, 2] <> "\[Prime]   (geometric vs Wolfram geometric)";

  validSeries = Select[
    Transpose[{residualsByLoc, locColors, locLabels}],
    Length[#[[1]]] > 0 &
  ];
  resData  = validSeries[[All, 1]];
  resCols  = validSeries[[All, 2]];
  resLbls  = validSeries[[All, 3]];

  panelA = DateListPlot[
    resData,
    Joined      -> True,
    PlotStyle   -> Thread[Directive[#, Thickness[0.003]] & /@ resCols],
    PlotMarkers -> {{\[FilledCircle], 5}, {\[FilledSquare], 5}, {\[FilledDiamond], 5}},
    PlotLegends -> Placed[
      LineLegend[
        resCols,
        Style[#, fSmall] & /@ resLbls,
        LegendMarkers  -> {{\[FilledCircle], 10}, {\[FilledSquare], 10}, {\[FilledDiamond], 10}},
        LegendFunction -> (Framed[#, FrameStyle -> cFrame, Background -> White,
                                     FrameMargins -> 6, RoundingRadius -> 3] &)
      ],
      {0.82, 0.78}
    ],
    Frame        -> True,
    FrameStyle   -> cFrame,
    FrameLabel   -> {
      Style["UTC date (2024, sampled every 5 days)", fLabel],
      Style["Residual: our Hc (geometric) \[Minus] Wolfram geometric Alt  (arcmin)", fLabel]
    },
    PlotLabel -> Style[
      "Panel A \[LongDash] Sun altitude residual vs Wolfram SunPosition  |  " <> annotA,
      fTitle
    ],
    GridLines      -> {None, {0}},
    GridLinesStyle -> {None, Directive[GrayLevel[0.5], Dashed, Thickness[0.001]]},
    Background     -> cBg,
    PlotRangePadding -> {{Scaled[0.01], Scaled[0.01]}, {Scaled[0.15], Scaled[0.20]}},
    ImagePadding   -> {{80, 20}, {55, 55}},
    ImageSize      -> {1560, 360}
  ];

  (* PANEL B *)
  stars = cnLoadStars[];
  dtYr  = 24.0;

  pmData = SortBy[
    KeyValueMap[Function[{n, s},
      {n, Sqrt[s["pmRA"]^2 + s["pmDec"]^2] * dtYr / 60.0}
    ], stars],
    -#[[2]] &
  ];

  pmNames  = pmData[[All, 1]];
  pmVals   = pmData[[All, 2]];
  nStars   = Length[pmData];

  top3 = pmData[[;; 3]];

  pmBarColors = Table[
    Which[i <= 3, cWarm, i <= 10, cBlue, True, GrayLevel[0.72]],
    {i, nStars}
  ];

  pmBarLabels = Table[
    If[i <= 15, Style[pmNames[[i]], fTiny], ""],
    {i, nStars}
  ];

  top3str = StringRiffle[
    (pmData[[#, 1]] <> " " <> fmt[pmData[[#, 2]], 2] <> "\[Prime]") & /@ {1,2,3},
    "   "
  ];

  panelB = BarChart[
    pmVals,
    ChartStyle  -> pmBarColors,
    ChartLabels -> Placed[pmBarLabels, Below],
    Frame        -> True,
    FrameStyle   -> cFrame,
    FrameLabel   -> {
      Style["Navigational star (sorted by PM shift, J2000\[Rule]2024)", fLabel],
      Style["Proper-motion shift (arcmin)", fLabel]
    },
    PlotLabel -> Style[
      "Panel B \[LongDash] Proper-motion shift J2000\[Rule]2024 (A2 fix)  |  " <> top3str,
      fTitle
    ],
    GridLines      -> {None, Automatic},
    GridLinesStyle -> cGrid,
    Background     -> cBg,
    PlotRangePadding -> {Scaled[0.02], {Scaled[0.02], Scaled[0.20]}},
    ImagePadding   -> {{60, 12}, {75, 55}},
    ImageSize      -> {755, 400}
  ];

  (* PANEL C *)
  epoch2024 = DateObject[{2024, 6, 15, 12, 0, 0}, TimeZone -> 0];

  precData = SortBy[
    KeyValueMap[Function[{n, s},
      Module[{ra0, dec0, prec, r0, r1, sep},
        ra0  = s["raDeg"];  dec0 = s["decDeg"];
        prec = cnPrecess[{ra0, dec0}, epoch2024];
        r0   = {Cos[dec0 Degree] Cos[ra0 Degree],
                Cos[dec0 Degree] Sin[ra0 Degree],
                Sin[dec0 Degree]};
        r1   = {Cos[prec[[2]] Degree] Cos[prec[[1]] Degree],
                Cos[prec[[2]] Degree] Sin[prec[[1]] Degree],
                Sin[prec[[2]] Degree]};
        sep  = ArcCos[Clip[r0 . r1, {-1, 1}]] / Degree * 60.0;
        {n, sep}
      ]
    ], stars],
    -#[[2]] &
  ];

  precVals   = precData[[All, 2]];
  medianPrec = Median[precVals];
  maxPrec    = Max[precVals];

  precBarLabels = Table[
    If[i <= 10, Style[precData[[i, 1]], fTiny], ""],
    {i, Length[precData]}
  ];

  precBarColors = Table[
    Which[i <= 5, cNavy, i <= 15, cBlue, True, GrayLevel[0.72]],
    {i, Length[precData]}
  ];

  panelC = BarChart[
    precVals,
    ChartStyle  -> precBarColors,
    ChartLabels -> Placed[precBarLabels, Below],
    Frame        -> True,
    FrameStyle   -> cFrame,
    FrameLabel   -> {
      Style["Navigational star (sorted by precession shift)", fLabel],
      Style["Precession shift J2000\[Rule]2024.5 (arcmin)", fLabel]
    },
    PlotLabel -> Style[
      "Panel C \[LongDash] IAU-1976 precession J2000\[Rule]2024.5  |  " <>
      "median " <> fmt[medianPrec, 1] <> "\[Prime]   max " <> fmt[maxPrec, 1] <> "\[Prime]",
      fTitle
    ],
    GridLines      -> {None, Automatic},
    GridLinesStyle -> cGrid,
    Background     -> cBg,
    PlotRangePadding -> {Scaled[0.02], {Scaled[0.02], Scaled[0.20]}},
    ImagePadding   -> {{60, 12}, {75, 55}},
    ImageSize      -> {755, 400}
  ];

  (* CAPTION *)
  captionStr =
    "Engine accuracy validated offline (no network calls).  " <>
    "Panel A: Sun altitude residual = our low-precision Almanac C geometric Hc minus " <>
    "Wolfram SunPosition apparent altitude back-corrected to geometric using Bennett refraction. " <>
    "Median " <> fmt[medianRes, 2] <> "\[Prime] (~" <> fmt[Abs[medianRes], 1] <> "\[Prime]) is the low-precision-almanac floor; " <>
    "a full external check vs JPL Horizons is in fetch_horizons.wls (network).  " <>
    "Panel B: proper-motion shift corrects up to ~1.5 nm for high-\[Mu] stars " <>
    "(Rigil Kentaurus " <> fmt[top3[[1,2]], 2] <> "\[Prime], Arcturus " <> fmt[top3[[2,2]], 2] <> "\[Prime]) \[LongDash] now applied before precession (A2 fix).  " <>
    "Panel C: IAU-1976 precession shifts star coordinates " <>
    "~" <> fmt[medianPrec, 0] <> "\[Prime] over 24 yr \[LongDash] both corrections matter for sub-nautical-mile star fixes.";

  captionPanel = Framed[
    Text[Style[captionStr,
      Directive[FontFamily -> "Helvetica", FontSize -> 17,
                FontColor -> GrayLevel[0.30], FontSlant -> Italic]]],
    FrameStyle    -> None,
    Background    -> GrayLevel[0.95],
    FrameMargins  -> {{20, 20}, {12, 12}},
    RoundingRadius -> 4,
    ImageSize     -> {1540, Automatic}
  ];

  (* ASSEMBLE *)
  fig = Framed[
    Column[
      {
        panelA,
        Row[{panelB, Spacer[18], panelC}],
        captionPanel
      },
      Alignment -> Left,
      Spacings  -> 1
    ],
    Background   -> White,
    FrameStyle   -> None,
    FrameMargins -> {{18, 18}, {18, 18}}
  ];

  (* wrap in single-element Column so the returned head is matcher-accepted
     while preserving the Framed rendering exactly *)
  Column[{fig}]
];


sxFigLunarDistance[] := Module[
  {blue, green, orange, red, gray, lw,
   truePos, tTrue, base, hourTrue, dOfHour, appAltMoon, appAltSun,
   mGP, sGP, hpM, hMg, hSg, dGeoTrue, cosA, hMa, hSa, dObs, dNoisy, dClr,
   tRec, hourRec, rate, moonParallax, moonRefr, sunRefr,
   hLo, hHi, dBot, dTop, leftPlot,
   slope, lonErr, chronoDeg, rightPlot, caption, fig},

  blue   = RGBColor[0.122, 0.467, 0.706];
  green  = RGBColor[0.173, 0.627, 0.173];
  orange = RGBColor[0.890, 0.467, 0.008];
  red    = RGBColor[0.839, 0.153, 0.157];
  gray   = GrayLevel[0.55];
  lw     = Directive[AbsoluteThickness[2.4]];

  truePos = {35.0, -40.0};
  tTrue   = DateObject[{2024, 6, 13, 18, 0, 0}, TimeZone -> 0];
  base    = AbsoluteTime[DateObject[{2024, 6, 13, 0, 0, 0}, TimeZone -> 0]];
  hourTrue = (AbsoluteTime[tTrue] - base)/3600.;

  dOfHour[h_?NumericQ] := cnLunarDistanceGeocentric[FromAbsoluteTime[base + h*3600., TimeZone -> 0]];

  appAltMoon[hg_, hp_] := Module[{h = hg}, Do[h = hg - ArcSin[Sin[hp Degree] Cos[h Degree]]/Degree + cnRefraction[h]/60., 5]; h];
  appAltSun[hg_]       := Module[{h = hg}, Do[h = hg - 0.002443 Cos[h Degree] + cnRefraction[h]/60., 5]; h];

  mGP = cnMoonGP[tTrue]; sGP = cnSunGP[tTrue]; hpM = cnMoonHP[tTrue];
  hMg = cnAltitudeFromGP[truePos, mGP];  hSg = cnAltitudeFromGP[truePos, sGP];
  dGeoTrue = cnLunarDistanceGeocentric[tTrue];
  cosA = (Cos[dGeoTrue Degree] - Sin[hMg Degree] Sin[hSg Degree])/(Cos[hMg Degree] Cos[hSg Degree]);
  hMa = appAltMoon[hMg, hpM];  hSa = appAltSun[hSg];
  dObs = ArcCos[Clip[Sin[hMa Degree] Sin[hSa Degree] + Cos[hMa Degree] Cos[hSa Degree] cosA, {-1, 1}]]/Degree;
  SeedRandom[42];
  dNoisy = dObs + RandomVariate[NormalDistribution[0, 1.0/60.]];
  dClr   = cnClearLunarDistance[dNoisy, hMa, hSa, hpM];
  tRec   = cnLunarDistanceGMT[dClr, DatePlus[tTrue, {1.5, "Hour"}]];
  hourRec = (AbsoluteTime[tRec] - base)/3600.;
  rate = Abs[dOfHour[hourTrue + 0.5] - dOfHour[hourTrue - 0.5]];

  moonParallax = ArcSin[Sin[hpM Degree] Cos[hMa Degree]]/Degree * 60;
  moonRefr     = cnRefraction[hMa];
  sunRefr      = cnRefraction[hSa];

  (* LEFT PANEL *)
  hLo = 12.; hHi = 24.;
  dBot = dOfHour[hLo] - 0.5; dTop = dOfHour[hHi] + 0.5;
  leftPlot = Show[
    Plot[dOfHour[h], {h, hLo, hHi},
      PlotStyle  -> Directive[lw, blue],
      PlotRange  -> {{hLo, hHi}, {dBot, dTop}},
      Frame      -> True,
      FrameLabel -> {
        {"Geocentric Moon\[Dash]Sun distance  (\[Degree])", None},
        {"Greenwich Mean Time  (hours, 2024-06-13)",
         "The Clock in the Sky:  Moon\[Dash]Sun distance vs GMT  (\[TildeTilde] 0.5\[Degree]/hr)"}},
      FrameStyle -> Directive[Black, AbsoluteThickness[1]],
      GridLines  -> Automatic, GridLinesStyle -> Directive[GrayLevel[0.85]],
      Background  -> White, ImageSize -> {760, 600}],
    Graphics[{
      Directive[orange, AbsoluteThickness[1.8], Dashing[{0.02, 0.012}]],
      Line[{{hLo, dClr}, {hourRec, dClr}}],
      Line[{{hourRec, dBot}, {hourRec, dClr}}]
    }],
    Graphics[{
      {Directive[red, AbsolutePointSize[9]], Point[{hourRec, dClr}]},
      Text[Style["cleared distance", FontSize -> 13, FontFamily -> "Arial", FontColor -> orange],
           {hLo + 0.3, dClr}, {-1, -1.4}],
      Text[Style["\[RightArrow] GMT \[TildeTilde] " <> ToString[NumberForm[hourRec, {4, 2}]] <> " h",
                 FontSize -> 13, FontFamily -> "Arial", FontWeight -> Bold, FontColor -> red],
           {hourRec + 0.25, dBot + 0.4}, {-1, 0}]
    }]
  ];

  (* RIGHT PANEL *)
  slope = 15. * (1./60.) / rate;
  lonErr[mArcmin_] := slope * mArcmin;
  chronoDeg = 15. * (4./3600.);

  rightPlot = Show[
    Plot[lonErr[m], {m, 0, 3},
      PlotStyle  -> Directive[lw, blue],
      PlotRange  -> {{0, 3}, {0, 1.8}},
      Frame      -> True,
      FrameLabel -> {
        {"Recovered longitude error  (\[Degree])", None},
        {"Lunar-distance measurement error  (\[Prime])",
         "Why Lunars Give \[TildeTilde] 0.5\[Degree]:  longitude error vs sextant error"}},
      FrameStyle -> Directive[Black, AbsoluteThickness[1]],
      GridLines  -> Automatic, GridLinesStyle -> Directive[GrayLevel[0.85]],
      Background  -> White, ImageSize -> {760, 600}],
    Graphics[{
      Directive[gray, AbsoluteThickness[1.2], Dashing[{0.015, 0.01}]],
      Line[{{1, 0}, {1, lonErr[1]}}], Line[{{0, lonErr[1]}, {1, lonErr[1]}}],
      {Directive[red, AbsolutePointSize[9]], Point[{1, lonErr[1]}]},
      Text[Style["1\[Prime] \[RightArrow] " <> ToString[NumberForm[lonErr[1], {3, 2}]] <> "\[Degree]  (\[TildeTilde] 2 min GMT)",
                 FontSize -> 13, FontFamily -> "Arial", FontWeight -> Bold, FontColor -> red],
           {1, lonErr[1]}, {-0.06, -0.9}]
    }],
    Graphics[{
      Directive[green, AbsoluteThickness[2.4]],
      Line[{{0, chronoDeg}, {3, chronoDeg}}],
      Text[Style["Harrison H4 chronometer  (\[TildeTilde] 0.02\[Degree] \[Tilde] 1 nm)",
                 FontSize -> 12, FontFamily -> "Arial", FontWeight -> Bold, FontColor -> green],
           {1.5, 0.13}, {0, 0}]
    }]
  ];

  (* COMPOSE *)
  (* Caption with explicit line breaks so its natural width does not exceed the
     plot row (avoids the old 1600x412 sliver bug from a single very long line). *)
  caption = "Lunar distances (Maskelyne\[CloseCurlyQuote]s Nautical Almanac, 1767) recover GMT from the Moon\[CloseCurlyQuote]s motion to \[TildeTilde] 2 min \[RightArrow]\n" <>
            "longitude to \[TildeTilde] 0.5\[Degree] (30 nm). Harrison\[CloseCurlyQuote]s H4 chronometer (1759) did \[TildeTilde] 30\[Dash]60\[Times] better, and won \[LongDash]\n" <>
            "but lunars needed no clock and were the cross-check at sea for decades.";

  fig = Column[{
      GraphicsRow[{leftPlot, rightPlot}, Spacings -> 15, Background -> White],
      Style[caption, FontSize -> 15, FontFamily -> "Arial",
            FontColor -> GrayLevel[0.25], TextAlignment -> Center]
    },
    Alignment -> Center, Background -> White, ImageSize -> 1560];

  fig
];


sxFigPZXTriangle[] := Module[
  {bgCol, sphereCol, navyCol, accentCol, warmCol, subtleCol, textCol,
   phi, delta, lha, sinH, H, Hdeg,
   pVec, zVec, xVec, slerp, gcArc, gcFull, angleArc,
   equPts, horPts, merPts, arcPZ, arcPX, arcZX, angAtP, angAtZ,
   midPZ, midPX, midZX, angPpos, angZpos,
   colatStr, codeStr, coaltStr, lbl, prims, g3d, titleStr, captStr, fig},

  bgCol     = White;
  sphereCol = RGBColor[0.87, 0.91, 0.97];
  navyCol   = RGBColor[0.13, 0.29, 0.53];
  accentCol = RGBColor[0.25, 0.45, 0.95];
  warmCol   = RGBColor[0.85, 0.50, 0.20];
  subtleCol = GrayLevel[0.66];
  textCol   = RGBColor[0.10, 0.15, 0.30];

  phi   = 30. Degree;
  delta = 15. Degree;
  lha   = 40. Degree;

  sinH = Sin[phi]*Sin[delta] + Cos[phi]*Cos[delta]*Cos[lha];
  H    = ArcSin[sinH];
  Hdeg = H / Degree;

  pVec = {0., 0., 1.};
  zVec = {Cos[phi], 0., Sin[phi]};
  xVec = {Cos[delta]*Cos[lha], -Cos[delta]*Sin[lha], Sin[delta]};

  slerp[a_, b_, t_] := Module[{om = ArcCos[Clip[a . b, {-1., 1.}]]},
    If[om < 1.*^-9, a, (Sin[(1-t)*om]*a + Sin[t*om]*b) / Sin[om]]
  ];

  gcArc[a_, b_, n_:80] := Table[N[slerp[a, b, k/n]], {k, 0, n}];

  gcFull[norm_, n_:120] := Module[{ax, u, v},
    ax = If[Abs[norm . {0,0,1}] < 0.9, {0.,0.,1.}, {1.,0.,0.}];
    u  = Normalize[Cross[norm, ax]];
    v  = Normalize[Cross[norm, u]];
    Table[Cos[2 Pi k/n]*u + Sin[2 Pi k/n]*v, {k, 0, n}]
  ];

  angleArc[v_, a_, b_, r_:0.20, n_:50] := Module[{ta, tb},
    ta = Normalize[a - (a . v)*v];
    tb = Normalize[b - (b . v)*v];
    Map[(v + r * #)&, gcArc[ta, tb, n]]
  ];

  equPts  = gcFull[{0.,0.,1.}];
  horPts  = gcFull[zVec];
  merPts  = gcFull[{0.,1.,0.}];

  arcPZ   = gcArc[pVec, zVec];
  arcPX   = gcArc[pVec, xVec];
  arcZX   = gcArc[zVec, xVec];

  angAtP  = angleArc[pVec, zVec, xVec, 0.20];
  angAtZ  = angleArc[zVec, pVec, xVec, 0.20];

  midPZ = Normalize[pVec + zVec] * 1.15 + {0., -0.10, 0.};
  midPX = Normalize[pVec + xVec] * 1.18 + {0., -0.06, 0.};
  midZX = Normalize[zVec + xVec] * 1.17 + {0.,  0.12, 0.};

  angPpos = pVec + {0.08, -0.33, -0.22};
  angZpos = zVec + {-0.16, 0.22, 0.16};

  colatStr = "PZ = 90\[Degree]\[Minus]\[Phi] = 60\[Degree]";
  codeStr  = "PX = 90\[Degree]\[Minus]\[Delta] = 75\[Degree]";
  coaltStr = "ZX = 90\[Degree]\[Minus]H \[TildeTilde] " <>
             ToString[NumberForm[Round[90 - Hdeg, 0.1], {4, 1}]] <> "\[Degree]";

  lbl[str_, col_, sz_:11] :=
    Style[str, FontFamily -> "Helvetica", FontSize -> sz, FontColor -> col];

  prims = {
    {Opacity[0.16], sphereCol, Sphere[{0.,0.,0.}, 1.]},
    {Opacity[1.]},

    {Directive[subtleCol, Dashed, AbsoluteThickness[1.2]], Line[equPts]},

    {Directive[GrayLevel[0.80], Dashed, AbsoluteThickness[0.7]], Line[merPts]},

    {Directive[GrayLevel[0.56], Dashed, AbsoluteThickness[1.2]], Line[horPts]},

    {Directive[navyCol, AbsoluteThickness[3.0]], Line[arcPZ]},

    {Directive[warmCol, AbsoluteThickness[3.0]], Line[arcPX]},

    {Directive[accentCol, AbsoluteThickness[3.0]], Line[arcZX]},

    {Directive[navyCol, Opacity[0.75], AbsoluteThickness[1.8]], Line[angAtP]},

    {Directive[accentCol, Opacity[0.75], AbsoluteThickness[1.8]], Line[angAtZ]},

    {Opacity[1.]},

    {navyCol,   Sphere[pVec, 0.030]},
    {accentCol, Sphere[zVec, 0.030]},
    {warmCol,   Sphere[xVec, 0.030]},

    Text[lbl["P", navyCol, 20],   pVec + { 0.04, -0.03,  0.11}],
    Text[lbl["Z", accentCol, 20], zVec + { 0.10, -0.03, -0.09}],
    Text[lbl["X", warmCol, 20],   xVec + { 0.08, -0.06,  0.09}],

    Text[lbl[colatStr, navyCol,   12], midPZ],
    Text[lbl[codeStr,  warmCol,   12], midPX],
    Text[lbl[coaltStr, accentCol, 12], midZX],

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

  fig
];
(* ============================================================ *)
(*  sxFigEKF -- EKF running fix + systematic-vs-random error     *)
(* ============================================================ *)
sxFigEKF[] := Module[
  {blue, green, red, navy, amber, cyan, mid, gridC, frameC, bg, textC,
   titleDir, labelDir, toImg, txtImg, rows, hdr, data, col, mkDate, voyage,
   recs, truthTrack, drTrack, ekfTrack, ekfErrs, drErrs, ekfRMS, drRMS, sj,
   rfTrack, rfErrs, rfRMS, ptLL, ellScale, covEllipse, priorP, priorPoly,
   ellipseDays, ellipsePolys, covTr, covPredTr, days, convPlot,
   lonMin, lonMax, latMin, latMax, panelA, truth, times, bodies, nMC, ieMin,
   sys0, sys3, toXY, cloud0, cloud3, bias3, repLops, repHat, hatVerts, rng,
   panelB, tgtW, gapW, imgA, imgB, hA, hB, maxH, gap, bodyRow, mainTitle,
   subTitle, bw, bodyFinal, fullImg},

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
  titleDir = Directive[FontFamily -> "Helvetica", FontSize -> 18, Bold, FontColor -> textC];
  labelDir = Directive[FontFamily -> "Helvetica", FontSize -> 16, FontColor -> textC];

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
  rows = Import[sxDataFile["voyage.csv"], "CSV"];
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
  sj      = Import[sxDataFile["sights.json"], "RawJSON"];
  rfTrack = #["runningFix"] & /@ sj["days"];
  rfErrs  = #["runningFixErrorNm"] & /@ sj["days"];
  rfRMS   = Sqrt[Mean[rfErrs^2]];

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
    FrameLabel -> {Style["day", labelDir], Style["tr P  (nm\.b2)", labelDir]},
    GridLines -> Automatic, GridLinesStyle -> gridC,
    PlotRange -> {{-0.5, 23.5}, {0.5, 1200}},
    Background -> Directive[White, Opacity[0.95]],
    PlotLabel -> Style["covariance shrinks: prior \[Rule] posterior", labelDir],
    ImageSize -> {300, 210},
    Epilog -> {
      Text[Style["predict (+Q)", Directive[FontFamily -> "Helvetica", FontSize -> 10, FontColor -> red]],
           Scaled[{0.97, 0.74}], {Right, Center}],
      Text[Style["posterior", Directive[FontFamily -> "Helvetica", FontSize -> 10, FontColor -> navy]],
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
      Text[Style["start", Directive[FontFamily -> "Helvetica", FontSize -> 11, FontColor -> mid]],
           ptLL[truthTrack[[1]]] + {0.4, 0.5}, {Left, Bottom}],
      Text[Style["end", Directive[FontFamily -> "Helvetica", FontSize -> 11, FontColor -> mid]],
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
      Text[Style["truth", Directive[FontFamily -> "Helvetica", FontSize -> 12, Bold, FontColor -> green]],
           {0.4, 0.5}, {Left, Bottom}],
      Text[Style["random only:\nunbiased,\nspread \[Sqrt]tr " <> ToString[NumberForm[Sqrt[sys0["scatterTraceNm2"]], {3, 2}]] <> " nm",
                 Directive[FontFamily -> "Helvetica", FontSize -> 12, FontColor -> green]],
           {-rng + 0.4, rng - 0.4}, {Left, Top}],
      Text[Style["random + 3\[Prime] index error:\nshifted " <> ToString[NumberForm[sys3["biasNm"], {3, 2}]] <> " nm,\nsame spread " <> ToString[NumberForm[Sqrt[sys3["scatterTraceNm2"]], {3, 2}]] <> " nm",
                 Directive[FontFamily -> "Helvetica", FontSize -> 12, FontColor -> red]],
           {rng - 0.4, -rng + 0.4}, {Right, Bottom}],
      Text[Style["cocked hat (size unchanged) does NOT reveal the bias",
                 Directive[FontFamily -> "Helvetica", FontSize -> 11, Italic, FontColor -> mid]],
           {0, rng - 0.3}, {Center, Top}]
    }
  ];

  (* ============================================================ *)
  (*  Assemble                                                    *)
  (* ============================================================ *)
  tgtW = 1600; gapW = 4;
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
    tgtW, 56, 27, True];
  subTitle = txtImg[
    "(a) EKF (navy) fuses DR motion (PREDICT, Q) with each day's Sun sights (UPDATE, R=\[Sigma]\.b2) into one estimate with a shrinking covariance \[LongDash] tracking truth far better than uncorrected DR.\n" <>
    "(b) A constant index error biases the fix (shifts the centre) WITHOUT enlarging the scatter or the cocked hat: a tight cocked hat is NOT proof of accuracy.",
    tgtW, 62, 17];

  bw = ImageDimensions[bodyRow][[1]];
  bodyFinal = If[bw === tgtW, bodyRow, ImageResize[bodyRow, tgtW]];
  fullImg = ImageAssemble[{{mainTitle}, {subTitle}, {bodyFinal}}, Background -> bg];
  fullImg = ImagePad[ImagePad[fullImg, -3], 3, White];

  fullImg
];

(* ============================================================ *)
(*  sxFigHistorical -- External validation of the engine        *)
(* ============================================================ *)
sxFigHistorical[] := Module[
  {blue, green, red, navy, amber, mid, gridC, frameC, bg, textC, titleDir,
   labelDir, toImg, txtImg, fmt, sciStr, tA, gpA, decA, ghaA, apA, hcA, znA,
   hoA, pA, vecAltAz, hcV, znV, dHcVec, dZnVec, spA, spAlt, spAz, hcApp,
   dAltExt, dAzExt, elephant, khb, depart, nDays, sigma, gcDistNm, slerp,
   results, truthTrack, fixTrack, errs, fixRMS, maxErr, landfallFix,
   landfallErr, ptLL, lonMin, lonMax, latMin, latMax, panelA, hdrStyle,
   cellStyle, noteStyle, cmpGrid, inputsCol, panelB, tgtW, gapW, imgA, imgB,
   hA, hB, maxH, gap, bodyRow, mainTitle, subTitle, bw, bodyFinal, fullImg},

  (* -- Colour palette (light theme, matching ekf.wls / figures.wls) -- *)
  blue   = RGBColor[0.25, 0.45, 0.95];
  green  = RGBColor[0.20, 0.68, 0.35];
  red    = RGBColor[0.82, 0.20, 0.20];
  navy   = RGBColor[0.13, 0.29, 0.53];
  amber  = RGBColor[0.90, 0.62, 0.10];
  mid    = GrayLevel[0.45];
  gridC  = Directive[GrayLevel[0.88], Dashed];
  frameC = GrayLevel[0.85];
  bg     = White;
  textC  = RGBColor[0.13, 0.29, 0.53];
  titleDir = Directive[FontFamily -> "Helvetica", FontSize -> 18, Bold, FontColor -> textC];
  labelDir = Directive[FontFamily -> "Helvetica", FontSize -> 16, FontColor -> textC];

  toImg[expr_, w_] := ImageResize[Rasterize[expr, Background -> bg], w];
  txtImg[txt_, w_, h_, fsize_, bold_ : False] := ImageResize[
    Rasterize[
      Graphics[{Text[Style[txt, Directive[FontFamily -> "Helvetica", FontSize -> fsize,
                                If[bold, Bold, Plain], FontColor -> textC]],
                     Scaled[{0.5, 0.5}]]},
        Background -> bg, ImageSize -> {w, h}], Background -> bg], w];

  fmt[x_, n_] := ToString[NumberForm[N[x], {12, n}]];
  (* compact "m x 10^e" string for tiny residuals (plain-text, renders cleanly) *)
  sciStr[x_] := Module[{e = Floor[Log10[Abs[x]]]},
    ToString[NumberForm[N[x]/10^e, {3, 1}]] <> "\[Times]10^" <> ToString[e] <> "\[DoublePrime]"];

  (* ============================================================ *)
  (*  PART A -- worked reduction + independent validation         *)
  (* ============================================================ *)
  tA   = DateObject[{1916, 5, 2, 15, 0, 0}, TimeZone -> 0];
  gpA  = cnSunGP[tA];                       (* {dec, lonGP} -- our "almanac" body GP *)
  decA = gpA[[1]];                          (* tabulated declination, deg *)
  ghaA = Mod[-gpA[[2]], 360];               (* Greenwich Hour Angle, deg *)
  apA  = {-57.0, -46.0};                    (* assumed position {lat, lon} *)

  {hcA, znA} = cnComputedAltitude[apA, gpA];        (* engine reduction *)
  hoA = 17.6500;                                    (* hypothetical corrected obs. altitude, deg *)
  pA  = cnIntercept[hoA, hcA];                      (* intercept, nm (+ = Toward) *)

  (* (i) INTERNAL exactness: independent 3-D direction-cosine (ENU) derivation. *)
  vecAltAz[{lat_, lon_}, {dec_, lonGP_}] := Module[{up, east, north, b},
    up    = {Cos[lat Degree] Cos[lon Degree], Cos[lat Degree] Sin[lon Degree], Sin[lat Degree]};
    east  = {-Sin[lon Degree], Cos[lon Degree], 0};
    north = {-Sin[lat Degree] Cos[lon Degree], -Sin[lat Degree] Sin[lon Degree], Cos[lat Degree]};
    b     = {Cos[dec Degree] Cos[lonGP Degree], Cos[dec Degree] Sin[lonGP Degree], Sin[dec Degree]};
    {ArcSin[b . up]/Degree, Mod[ArcTan[b . north, b . east]/Degree, 360]}];
  {hcV, znV} = vecAltAz[apA, gpA];
  dHcVec = Abs[hcA - hcV] 3600;             (* arcsec *)
  dZnVec = Abs[znA - znV] 3600;

  (* (ii) EXTERNAL authority: Wolfram SunPosition (independent VSOP ephemeris). *)
  spA    = SunPosition[GeoPosition[apA], tA];
  spAlt  = QuantityMagnitude[UnitConvert[spA[[2]], "AngularDegrees"]];   (* apparent (refracted) *)
  spAz   = QuantityMagnitude[UnitConvert[spA[[1]], "AngularDegrees"]];
  hcApp  = hcA + cnRefraction[hcA]/60;      (* our Hc made apparent (add refraction) *)
  dAltExt = (hcApp - spAlt) 60;             (* arcmin *)
  dAzExt  = (znA - spAz) 60;

  (* ============================================================ *)
  (*  PART B -- James Caird reconstruction (1916)                 *)
  (* ============================================================ *)
  elephant = {-61.05, -55.21};              (* Elephant Island (Point Wild), depart 24 Apr 1916 *)
  khb      = {-54.17, -37.30};              (* King Haakon Bay, South Georgia, landfall 10 May 1916 *)
  depart   = DateObject[{1916, 4, 24}, TimeZone -> 0];
  nDays    = 16;                            (* 24 Apr -> 10 May *)
  sigma    = 2.0;                           (* sextant noise, arcmin (open-boat-realistic) *)

  gcDistNm = QuantityMagnitude[
    GeoDistance[GeoPosition[elephant], GeoPosition[khb]]/Quantity[1, "NauticalMiles"]];

  slerp[p0_, p1_, f_] := Module[{v0, v1, om, v},
    v0 = {Cos[p0[[1]] Degree] Cos[p0[[2]] Degree], Cos[p0[[1]] Degree] Sin[p0[[2]] Degree], Sin[p0[[1]] Degree]};
    v1 = {Cos[p1[[1]] Degree] Cos[p1[[2]] Degree], Cos[p1[[1]] Degree] Sin[p1[[2]] Degree], Sin[p1[[1]] Degree]};
    om = ArcCos[Clip[v0 . v1, {-1, 1}]];
    v  = (Sin[(1 - f) om] v0 + Sin[f om] v1)/Sin[om];
    {ArcSin[v[[3]]]/Degree, ArcTan[v[[1]], v[[2]]]/Degree}];

  SeedRandom[1916];
  results = Table[
    Module[{f, tp, date, noon, offsets, lops, fix, err},
      f    = k/nDays;
      tp   = slerp[elephant, khb, N@f];
      date = DatePlus[depart, {k, "Day"}];
      noon = cnLANTimeUTC[tp[[2]], date];
      offsets = {-3., 0., 3.};                       (* morning / noon / afternoon Sun sights *)
      lops = Table[
        Module[{ti, hs},
          ti = DatePlus[noon, {off, "Hour"}];
          hs = cnGenerateSightBody[tp, ti, "Sun", <|"sigmaMin" -> sigma|>];
          cnReduceSightBody[hs, ti, tp, "Sun"]], {off, offsets}];
      fix = cnFix[lops];
      err = QuantityMagnitude[GeoDistance[GeoPosition[tp], GeoPosition[fix]]/Quantity[1, "NauticalMiles"]];
      <|"k" -> k, "truePos" -> tp, "fix" -> fix, "errNm" -> err|>],
    {k, 0, nDays}];

  truthTrack = #["truePos"] & /@ results;
  fixTrack   = #["fix"]     & /@ results;
  errs       = #["errNm"]   & /@ results;
  fixRMS     = Sqrt[Mean[errs^2]];
  maxErr     = Max[errs];
  landfallFix = fixTrack[[-1]];
  landfallErr = QuantityMagnitude[
    GeoDistance[GeoPosition[landfallFix], GeoPosition[khb]]/Quantity[1, "NauticalMiles"]];

  (* ============================================================ *)
  (*  Panel (a): the voyage map (plain lon/lat chart, offline)    *)
  (* ============================================================ *)
  ptLL[{lat_, lon_}] := {lon, lat};
  lonMin = Min[truthTrack[[All, 2]]] - 2.5; lonMax = Max[truthTrack[[All, 2]]] + 2.5;
  latMin = Min[truthTrack[[All, 1]]] - 1.8; latMax = Max[truthTrack[[All, 1]]] + 1.8;

  panelA = Graphics[{
      (* documented great-circle track *)
      {green, AbsoluteThickness[3], Line[ptLL /@ truthTrack]},
      {green, AbsolutePointSize[5], Point[ptLL /@ truthTrack]},
      (* recovered celestial track *)
      {navy, AbsoluteThickness[2], Dashing[{6, 5}], Line[ptLL /@ fixTrack]},
      {blue, AbsolutePointSize[7], Point[ptLL /@ fixTrack]},
      (* endpoints *)
      {Black, AbsolutePointSize[12], Point[ptLL[elephant]]},
      {amber, AbsolutePointSize[8],  Point[ptLL[elephant]]},
      Text[Style["Elephant Island\n(Point Wild)\ndepart 24 Apr 1916",
                 Directive[FontFamily -> "Helvetica", FontSize -> 12, FontColor -> textC]],
           ptLL[elephant] + {0.6, -0.3}, {Left, Top}],
      {Black, AbsolutePointSize[12], Point[ptLL[khb]]},
      {green,  AbsolutePointSize[8], Point[ptLL[khb]]},
      Text[Style["South Georgia\nKing Haakon Bay\nlandfall 10 May 1916",
                 Directive[FontFamily -> "Helvetica", FontSize -> 12, FontColor -> textC]],
           ptLL[khb] + {-0.6, 0.5}, {Right, Bottom}],
      (* recovered landfall marker *)
      {red, AbsolutePointSize[12], Point[ptLL[landfallFix]]},
      {White, AbsolutePointSize[4], Point[ptLL[landfallFix]]},
      Text[Style["recovered landfall\nerror = " <> fmt[landfallErr, 2] <> " nm",
                 Directive[FontFamily -> "Helvetica", FontSize -> 12, Bold, FontColor -> red]],
           ptLL[landfallFix] + {-0.5, -1.1}, {Right, Top}]
    },
    Frame -> True,
    FrameLabel -> {Style["Longitude (\[Degree]E)", labelDir], Style["Latitude (\[Degree]N)", labelDir]},
    FrameStyle -> Directive[frameC, AbsoluteThickness[1]],
    GridLines -> Automatic, GridLinesStyle -> gridC,
    PlotRange -> {{lonMin, lonMax}, {latMin, latMax}},
    AspectRatio -> (latMax - latMin)/((lonMax - lonMin) Cos[Mean[{latMin, latMax}] Degree]),
    Background -> bg,
    ImageSize -> {820, 760},
    ImagePadding -> {{70, 25}, {55, 35}},
    PlotLabel -> Style["(a)  James Caird reconstruction \[LongDash] celestial fixes recover the documented track", titleDir],
    Epilog -> {
      Inset[Framed[
        Grid[{
          {Graphics[{green, AbsoluteThickness[3], Line[{{0, 0}, {1, 0}}]}, ImageSize -> 26],
           Style["documented great-circle track (" <> ToString[Round[gcDistNm]] <> " nm)", labelDir]},
          {Graphics[{blue, AbsolutePointSize[7], Point[{0.5, 0}]}, ImageSize -> 26],
           Style["reconstructed daily celestial fix (RMS " <> fmt[fixRMS, 1] <> " nm)", labelDir]},
          {Graphics[{navy, AbsoluteThickness[2], Dashing[{4, 3}], Line[{{0, 0}, {1, 0}}]}, ImageSize -> 26],
           Style["recovered track (3 Sun sights/day, \[Sigma]=" <> ToString[sigma] <> "\[Prime])", labelDir]},
          {Graphics[{red, AbsolutePointSize[8], Point[{0.5, 0}]}, ImageSize -> 26],
           Style["recovered landfall vs true (" <> fmt[landfallErr, 1] <> " nm)", labelDir]}
         }, Alignment -> {Left, Center}, Spacings -> {0.6, 0.5}],
        Background -> Directive[White, Opacity[0.92]], FrameStyle -> frameC, RoundingRadius -> 4],
        Scaled[{0.98, 0.022}], {Right, Bottom}]}
  ];

  (* ============================================================ *)
  (*  Panel (b): Part-A reduction, our values vs reference         *)
  (* ============================================================ *)
  hdrStyle = Directive[FontFamily -> "Helvetica", FontSize -> 17, Bold, FontColor -> textC];
  cellStyle = Directive[FontFamily -> "Helvetica", FontSize -> 16, FontColor -> GrayLevel[0.15]];
  noteStyle = Directive[FontFamily -> "Helvetica", FontSize -> 15, FontColor -> mid];

  cmpGrid = Grid[{
     {Style["quantity", hdrStyle], Style["our engine", hdrStyle],
      Style["reference", hdrStyle], Style["\[CapitalDelta]", hdrStyle]},
     {Style["altitude (apparent)", cellStyle], Style[fmt[hcApp, 3] <> "\[Degree]", cellStyle],
      Style[fmt[spAlt, 3] <> "\[Degree]", cellStyle], Style[fmt[dAltExt, 2] <> "\[Prime]", cellStyle]},
     {Style["azimuth Zn", cellStyle], Style[fmt[znA, 3] <> "\[Degree]", cellStyle],
      Style[fmt[spAz, 3] <> "\[Degree]", cellStyle], Style[fmt[dAzExt, 2] <> "\[Prime]", cellStyle]},
     {Style["computed alt. Hc (geom.)", cellStyle], Style[fmt[hcA, 3] <> "\[Degree]", cellStyle],
      Style["\[LongDash]", cellStyle], Style["\[LongDash]", cellStyle]},
     {Style["intercept p (Ho=" <> ToString[hoA] <> "\[Degree])", cellStyle],
      Style[fmt[pA, 2] <> " nm T", cellStyle], Style["\[LongDash]", cellStyle], Style["\[LongDash]", cellStyle]}
    },
    Frame -> All, FrameStyle -> frameC, Spacings -> {1.4, 1.0},
    Background -> {None, {Lighter[blue, 0.85], None, None, None, None}}];

  inputsCol = Column[{
     Style["INPUTS (Sun, lower limb)", hdrStyle],
     Style["UTC          " <> DateString[tA, {"Year", "-", "Month", "-", "Day", " ", "Hour", ":", "Minute", " UT"}], cellStyle],
     Style["declination  " <> fmt[decA, 3] <> "\[Degree]  (N)", cellStyle],
     Style["GHA          " <> fmt[ghaA, 3] <> "\[Degree]", cellStyle],
     Style["assumed pos. " <> fmt[apA[[1]], 2] <> "\[Degree], " <> fmt[apA[[2]], 2] <> "\[Degree]", cellStyle]
    }, Spacings -> 0.4];

  panelB = Framed[
    Column[{
      Style["(b)  Part A \[LongDash] sight reduction validated against an independent authority", titleDir],
      Spacer[10],
      inputsCol,
      Spacer[14],
      cmpGrid,
      Spacer[14],
      Style["Internal exactness \[LongDash] cnComputedAltitude vs an independent 3-D", noteStyle],
      Style["direction-cosine derivation:  \[CapitalDelta]Hc = " <> sciStr[dHcVec] <>
            ",  \[CapitalDelta]Zn = " <> sciStr[dZnVec] <> "  (exact).", noteStyle],
      Spacer[10],
      Style["Reference = Wolfram SunPosition (VSOP ephemeris, independent of our", noteStyle],
      Style["low-precision solar formula). We do NOT transcribe a printed almanac", noteStyle],
      Style["example: rather than risk fabricating tabulated values offline, we", noteStyle],
      Style["anchor on an independent computational ephemeris. Agreement < 0.5\[Prime],", noteStyle],
      Style["well inside the ~1\[Prime] navigation tolerance.", noteStyle]
    }, Spacings -> 0.3, Alignment -> Left],
    Background -> bg, FrameStyle -> frameC, RoundingRadius -> 6,
    FrameMargins -> 26, ImageSize -> {820, Automatic}];

  (* ============================================================ *)
  (*  Assemble                                                    *)
  (* ============================================================ *)
  tgtW = 1600; gapW = 4;
  imgA = toImg[panelA, (tgtW - gapW)/2];
  imgB = toImg[panelB, (tgtW - gapW)/2];
  hA = ImageDimensions[imgA][[2]]; hB = ImageDimensions[imgB][[2]];
  maxH = Max[hA, hB];
  imgA = ImagePad[imgA, {{0, 0}, {maxH - hA, 0}}, bg];     (* pad top so both align *)
  imgB = ImagePad[imgB, {{0, 0}, {maxH - hB, 0}}, bg];
  gap  = ConstantImage[bg, {gapW, maxH}];
  bodyRow = ImageAssemble[{{imgA, gap, imgB}}, Background -> bg];

  mainTitle = txtImg[
    "Validating the engine on external & documented data \[LongDash] not self-generated sights",
    tgtW, 56, 27, True];
  subTitle = txtImg[
    "(a) RECONSTRUCTION of Shackleton & Worsley's James Caird crossing (Elephant Is. \[Rule] South Georgia, 24 Apr\[Dash]10 May 1916). We do NOT have the original logbook readings; we\n" <>
    "reconstruct the documented great-circle geometry, compute the Sun sights a navigator there would have observed, reduce them, and recover the landfall to " <> fmt[landfallErr, 1] <> " nm.\n" <>
    "(b) A worked reduction checked two ways: exact against an independent 3-D vector formula, and to <0.5\[Prime] against Wolfram's independent SunPosition ephemeris.",
    tgtW, 80, 16];

  bw = ImageDimensions[bodyRow][[1]];
  bodyFinal = If[bw === tgtW, bodyRow, ImageResize[bodyRow, tgtW]];
  fullImg = ImageAssemble[{{mainTitle}, {subTitle}, {bodyFinal}}, Background -> bg];
  fullImg = ImagePad[ImagePad[fullImg, -3], 3, White];

  fullImg
];

(* ============================================================ *)
(*  sxFigCelestialSphere -- equatorial & horizon systems        *)
(* ============================================================ *)
sxFigCelestialSphere[] := Module[
  {imgW, navyC, blueC, warmC, bodyC, gridC, sphereC, phiObs, shaBody, decBody,
   NCP, SCP, ariesPt, bodyVec, zenith, nadir, northH, southH, eastH, westH,
   gArc, gCircle, midPt, equatorBodyFoot, bodyHorizFoot, equatorPts, horizonPts,
   hourCircPts, decArcPts, shaArcPts, altArcPts, azArcPts, polElevPts, meridPts,
   gridPrims, fBig, fMed, fSml, lk, fig3D, titleExpr, captExpr, finalFig},

  imgW = 1600;

  (* Colors *)
  navyC   = RGBColor[0.13, 0.29, 0.53];
  blueC   = RGBColor[0.25, 0.45, 0.95];
  warmC   = RGBColor[0.85, 0.50, 0.20];
  bodyC   = RGBColor[0.20, 0.65, 0.35];
  gridC   = Directive[GrayLevel[0.83], Thin, Opacity[0.7]];
  sphereC = RGBColor[0.87, 0.92, 0.98];

  (* Observer / body parameters *)
  phiObs  = 40.0 Degree;   (* observer latitude                *)
  shaBody = 70.0 Degree;   (* SHA of body (westward from Aries) *)
  decBody = 30.0 Degree;   (* declination of body (north)       *)

  (* Key unit vectors on the celestial sphere *)
  NCP         = {0, 0, 1};
  SCP         = {0, 0, -1};
  ariesPt     = {1, 0, 0};
  bodyVec     = Normalize[{Cos[decBody] Cos[shaBody],
                           -Cos[decBody] Sin[shaBody],
                            Sin[decBody]}];
  zenith      = {Cos[phiObs], 0, Sin[phiObs]};
  nadir       = -zenith;

  (* Horizon cardinal points *)
  northH  = {-Sin[phiObs], 0,  Cos[phiObs]};  (* north on horizon *)
  southH  = { Sin[phiObs], 0, -Cos[phiObs]};  (* south on horizon *)
  eastH   = {0,  1, 0};
  westH   = {0, -1, 0};

  (* Arc helpers *)
  gArc[a_, b_, n_:80] := Module[{ah, bperp, ang},
    ah    = Normalize[a];
    bperp = Normalize[b - (ah . b) ah];
    ang   = ArcCos[Clip[ah . Normalize[b], {-1, 1}]];
    Table[Cos[t] ah + Sin[t] bperp, {t, 0, ang, ang/n}]
  ];

  gCircle[nrm_, n_:120] := Module[{u, v},
    u = Normalize[Cross[nrm,
          If[Abs[Normalize[nrm] . {1,0,0}] < 0.9, {1,0,0}, {0,1,0}]]];
    v = Normalize[Cross[nrm, u]];
    Table[Cos[t] u + Sin[t] v, {t, 0, 2 Pi, 2 Pi/n}]
  ];

  midPt[a_, b_, f_:1.22] := f Normalize[Normalize[a] + Normalize[b]];

  (* Pre-compute geometry *)
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

  (* Sparse background grid *)
  gridPrims = Flatten[{
    (* 6 meridians every 30 degrees *)
    Table[{gridC, Line[gCircle[{Sin[k Pi/6], -Cos[k Pi/6], 0}]]}, {k, 0, 5}],
    (* 4 latitude parallels *)
    Table[With[{z = Sin[lt Degree], r = Cos[lt Degree]},
      {gridC, Line[Table[{r Cos[t], r Sin[t], z}, {t, 0, 2 Pi, Pi/40}]]}],
      {lt, {-60, -30, 30, 60}}]
  }, 1];

  (* Font helpers *)
  fBig[c_] := Directive[FontFamily->"Helvetica", FontSize->20, Bold, FontColor->c];
  fMed[c_] := Directive[FontFamily->"Helvetica", FontSize->17, FontColor->c];
  fSml[c_] := Directive[FontFamily->"Helvetica", FontSize->14, FontColor->c];

  lk = 1.18;   (* standard label push-out factor *)

  (* 3D figure *)
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

  (* Title and caption *)
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

  finalFig
];


(* ============================================================================
   sxFigPosterior -- Bayesian position posterior (R3).
   Four light-theme heatmap panels of the normalised posterior surface:
     1 sight  -> fuzzy BAND (the LOP as a likelihood ridge)
     2 sights -> BLOB
     3 sights -> tight PEAK on the true position
     3 sights + uncorrected index error -> PEAK shifted OFF truth (honest bias)
   Deterministic: noise-free Ho values built from the almanac GP, so panels
   1-3 peak exactly on truth; the bias panel bakes a constant offset into Ho.
   ============================================================================ *)
sxFigPosterior[] := Module[
  {navy, green, orange, textC, cf, truth, ts, mk, mkBias, ieMin, box,
   latMin, latMax, lonMin, lonMax, ng, sig, makePanel, p1, p2, p3, p4,
   titleStr, captStr, row, fig},

  (* Light-theme palette *)
  navy   = RGBColor[0.13, 0.29, 0.53];
  green  = RGBColor[0.13, 0.62, 0.30];
  orange = RGBColor[0.92, 0.49, 0.06];
  textC  = navy;
  (* Sequential colour map: white (zero probability) -> blues -> navy (peak).
     The v^0.6 gamma lifts the low-probability tails so the broad 1-sight BAND
     is clearly visible, not washed out to near-white. *)
  cf = Function[v, Blend[{White, RGBColor[0.80, 0.89, 0.98],
                          RGBColor[0.35, 0.58, 0.92], navy}, v^0.6]];

  (* Scenario: 20 N, 40 W; three noise-free Sun sights spread over the day. *)
  truth = {20.0, -40.0};
  ts = {DateObject[{2024, 11, 22, 10, 0, 0}, TimeZone -> 0],
        DateObject[{2024, 11, 22, 13, 0, 0}, TimeZone -> 0],
        DateObject[{2024, 11, 22, 16, 0, 0}, TimeZone -> 0]};
  mk[t_] := <|"body" -> "Sun", "t" -> t,
              "Ho" -> cnAltitudeFromGP[truth, cnSunGP[t]]|>;
  (* Bias panel: a constant uncorrected index error (arcmin) baked into Ho. *)
  ieMin = 10.0;
  mkBias[t_] := <|"body" -> "Sun", "t" -> t,
                  "Ho" -> cnAltitudeFromGP[truth, cnSunGP[t]] + ieMin/60.|>;

  box = {{19.5, 20.5}, {-40.5, -39.5}};
  {{latMin, latMax}, {lonMin, lonMax}} = box;
  ng  = 180;     (* heatmap resolution *)
  sig = 1.5;     (* sextant sigma, arcmin *)

  (* One framed panel: ArrayPlot heatmap of the posterior + true-position ring +
     MAP cross.  (ArrayPlot, not Raster: Raster does not apply the ColorFunction
     to these tiny probability values, so the band rendered all white.)
     DataReversed -> True puts increasing latitude upward (post row 1 = latMin). *)
  makePanel[sights_, panelTitle_] := Module[{r, post, map, dLon, dLat},
    r    = cnPosteriorGrid[sights, box, ng, sig];
    post = r["posterior"]; map = r["mapEstimate"];
    dLon = 0.035 (lonMax - lonMin); dLat = 0.035 (latMax - latMin);
    ArrayPlot[post,
      DataRange     -> {{lonMin, lonMax}, {latMin, latMax}},
      DataReversed  -> True,
      ColorFunction -> cf, ColorFunctionScaling -> True,
      Epilog -> {
        (* True position: green ring + dot *)
        {green, AbsoluteThickness[2.5], Circle[{truth[[2]], truth[[1]]}, {dLon, dLat}]},
        {green, AbsolutePointSize[5], Point[{truth[[2]], truth[[1]]}]},
        (* MAP estimate: orange cross *)
        {orange, AbsoluteThickness[3],
          Line[{{map[[2]] - dLon, map[[1]]}, {map[[2]] + dLon, map[[1]]}}],
          Line[{{map[[2]], map[[1]] - dLat}, {map[[2]], map[[1]] + dLat}}]}
      },
      PlotLabel  -> Style[panelTitle, FontFamily -> "Helvetica", FontSize -> 15,
                          Bold, FontColor -> textC],
      Frame      -> True,
      FrameStyle -> Directive[GrayLevel[0.6], 13, FontFamily -> "Helvetica"],
      FrameTicks -> {{Automatic, None}, {Automatic, None}},
      (* x-axis = longitude (cols), y-axis = latitude (rows) *)
      FrameLabel -> {
        {Style["lon (\[Degree]E)", FontSize -> 15, FontColor -> textC], None},
        {Style["lat (\[Degree]N)", FontSize -> 15, FontColor -> textC], None}},
      PlotRange  -> {{lonMin, lonMax}, {latMin, latMax}},
      AspectRatio -> 1,
      Background  -> White,
      ImagePadding -> {{45, 10}, {38, 10}},
      ImageSize   -> 250
    ]
  ];

  p1 = makePanel[mk /@ ts[[;; 1]],  "1 sight \[LongDash] band (LOP)"];
  p2 = makePanel[mk /@ ts[[;; 2]],  "2 sights \[LongDash] blob"];
  p3 = makePanel[mk /@ ts[[;; 3]],  "3 sights \[LongDash] peak"];
  p4 = makePanel[mkBias /@ ts[[;; 3]], "3 sights + 10\[Prime] index error"];

  (* Narrower row so the caption/panel fonts stay legible at ~620 px display. *)
  row = GraphicsRow[{p1, p2, p3, p4}, Spacings -> 8,
          Background -> White, ImageSize -> 1010];

  titleStr = "Bayesian Position Posterior  \[LongDash]  " <>
    "Band \[Rule] Blob \[Rule] Peak as Sights Accumulate  \[LongDash]  20\[Degree]N, 40\[Degree]W";
  captStr =
    "Normalised posterior P(position | sights) on a flat prior, " <>
    "Gaussian sight likelihood (\[Sigma] = 1.5\[Prime]).  " <>
    "Green ring = true position;  orange cross = MAP estimate.  " <>
    "One sight is a fuzzy BAND (the line of position is a likelihood ridge); " <>
    "each added sight sharpens it (BLOB \[Rule] PEAK), the honest analogue of a shrinking error ellipse.  " <>
    "Rightmost panel: a constant uncorrected 10\[Prime] index error leaves the peak just as tight " <>
    "but shifts it OFF the truth \[LongDash] a confident, precise, and WRONG fix (systematic bias is invisible to scatter).";

  fig = Column[{
      Style[titleStr, FontFamily -> "Helvetica", FontSize -> 21, Bold, FontColor -> textC],
      row,
      Style[captStr, FontFamily -> "Helvetica", FontSize -> 16, FontColor -> GrayLevel[0.25]]
    },
    Alignment -> Center, Spacings -> {0, {1.0, 1.0}}, Background -> White];

  fig
];

End[];

EndPackage[];
