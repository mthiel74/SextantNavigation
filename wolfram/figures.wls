#!/usr/bin/env wolframscript
(* figures.wls -- Static educational figures for SextantNavigation *)
(* Run: /usr/local/bin/wolframscript -file wolfram/figures.wls     *)
(*                                                                  *)
(* Task 11b changes:                                                *)
(*   A1. UTF-8 guard + named-char escapes eliminate mojibake        *)
(*   A2. Stray paleBlue Rectangle removed from fig_intercept        *)
(*   A3. fig_altitude_circle: offline GeoStyling[LandMask]         *)
(*   A4. Legend Placed[Below] in fig_celestial_vs_gps              *)
(*   A5. Explicit ImageSize for cocked_hat + waterfall (no lb)     *)
(*   A6. CompressionLevel 0 removed; PNG uses default compression   *)
(*   B.  Theme (light/dark) + globe-style (relief/tiled/dark) param*)
(*   B.  Two comparison sheets                                      *)

$CharacterEncoding = "UTF-8";

(* ── Bootstrap ─────────────────────────────────────────────────────────── *)
SetDirectory[DirectoryName[$InputFileName]];
Get["CelestialNavigation.wl"];

If[!DirectoryQ["../docs/images"], CreateDirectory["../docs/images"]];
outDir = "../docs/images";
(* Reduced 1600 -> 860 so 11-15 pt fonts stay legible at the ~620 px the       *)
(* notebook embeds these PNGs at (font-to-width ratio ~2x larger).             *)
imgW   = 860;

(* ── Theme colour palettes ──────────────────────────────────────────────── *)
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

(* ── Utilities ──────────────────────────────────────────────────────────── *)
fixJSON[x_List] := If[
  Length[x] > 0 && AllTrue[x, MatchQ[#, _Rule] &],
  Association[fixJSON /@ x], fixJSON /@ x];
fixJSON[x_Rule] := Rule[x[[1]], fixJSON[x[[2]]]];
fixJSON[x_]     := x;

exportFig[path_, fig_] := Module[{img, sz, dims},
  img = Rasterize[fig, Background -> White];
  (* WL on this macOS config renders a 2-px pink (#FF5454) outer perimeter on
     frame-less Graphics and GeoGraphics. Crop 3 px then re-pad with white.
     Content is safe: minimum ImagePadding is 40 logical = 80 physical pixels. *)
  img = ImagePad[ImagePad[img, -3], 3, White];
  Export[path, img, "PNG"];
  dims = ImageDimensions[img];
  sz   = FileByteCount[path];
  Print["EXPORTED: ", FileNameTake[path],
        "  ", dims[[1]], "\[Times]", dims[[2]], " px  ", Round[sz/1024], " KB"];
];

(* ── Load voyage data ───────────────────────────────────────────────────── *)
sightsData = fixJSON[Import["../data/sights.json", "JSON"]];
daysData   = sightsData["days"];
(* Real star fixes from twilight_replay.wls — used in Fig 3 cocked hat      *)
(* (synthetic starFix removed from sights.json by B2 honesty fix)            *)
twData     = fixJSON[Import["../data/twilight_fixes.json", "JSON"]];

(* ── Style helpers ──────────────────────────────────────────────────────── *)
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
   PART C -- DEFAULT RENDER: all 9 figures, light theme + relief globe
   ========================================================================== *)
Print[""];
Print["=== PART C: Default render (light theme, relief globe) ==="];
tcL = themeColors["light"];

Print["--- Fig 1: altitude circle ---"];
exportFig[FileNameJoin[{outDir, "fig_altitude_circle.png"}],       makeFig1[tcL, "relief"]];
Print["--- Fig 2: intercept method ---"];
exportFig[FileNameJoin[{outDir, "fig_intercept.png"}],             makeFig2[tcL]];
Print["--- Fig 3: cocked hat ---"];
exportFig[FileNameJoin[{outDir, "fig_cocked_hat.png"}],            makeFig3[tcL]];
Print["--- Fig 4: running fix ---"];
exportFig[FileNameJoin[{outDir, "fig_running_fix.png"}],           makeFig4[tcL]];
Print["--- Fig 5: corrections waterfall ---"];
exportFig[FileNameJoin[{outDir, "fig_corrections_waterfall.png"}], makeFig5[tcL]];
Print["--- Fig 6: subsolar track ---"];
exportFig[FileNameJoin[{outDir, "fig_subsolar_track.png"}],        makeFig6[tcL]];
Print["--- Fig 7: error ellipse ---"];
exportFig[FileNameJoin[{outDir, "fig_error_ellipse.png"}],         makeFig7[tcL]];
Print["--- Fig 8: cut angle ---"];
exportFig[FileNameJoin[{outDir, "fig_cut_angle.png"}],             makeFig8[tcL]];
Print["--- Fig 9: celestial vs GPS ---"];
exportFig[FileNameJoin[{outDir, "fig_celestial_vs_gps.png"}],      makeFig9[tcL]];
Print["=== ALL 9 FIGURES COMPLETE ==="];

(* ==========================================================================
   PART B -- COMPARISON SHEET 1: globe styles
   fig_altitude_circle rendered in relief | tiled | dark, side by side
   ========================================================================== *)
Print[""];
Print["=== PART B: Sheet 1 -- globe styles ==="];
tcD = themeColors["dark"];

(* Target: ~2000 px wide sheet, 3 panels. Each panel ~667 px physical.
   Rasterize then ImageResize gives exact pixel control. *)
panelPhys = 667;

Print["  relief panel (CountryBorders, offline after entity cache)..."];
reliefImg = ImageResize[Rasterize[makeFig1[tcL, "relief"]], panelPhys];

Print["  tiled panel (StreetMapNoLabels -- best-effort; tiles may be blank if offline)..."];
tiledImg = Quiet[ImageResize[Rasterize[makeFig1[tcL, "tiled"]], panelPhys]];

Print["  dark panel (CountryBorders, offline after entity cache)..."];
darkImg = ImageResize[Rasterize[makeFig1[tcD, "dark"]], panelPhys];

(* Equalise heights by padding to tallest panel *)
maxH = Max @@ (ImageDimensions[#][[2]] & /@ {reliefImg, tiledImg, darkImg});
padH[img_, rgb_:{255,255,255}] := Module[{need = maxH - ImageDimensions[img][[2]]},
  If[need <= 0, img,
     ImagePad[img, {{0,0},{0,need}}, RGBColor @@ (rgb/255)]]
];
reliefPad = padH[reliefImg, {255,255,255}];
tiledPad  = padH[tiledImg,  {255,255,255}];
darkPad   = padH[darkImg,   {18, 23, 33}];

(* Header labels *)
mkHdrPng[lbl_, w_] := ImageResize[
  Rasterize[Graphics[{Text[Style[lbl, Directive[FontFamily->"Helvetica",
                                                FontSize->11, Bold, Black]],
                           Scaled[{0.5, 0.5}]]},
                     Background -> GrayLevel[0.92], ImageSize -> {200, 22}]], w];

hdr1 = mkHdrPng["relief  (offline)",  panelPhys];
hdr2 = mkHdrPng["tiled  (network)",   panelPhys];
hdr3 = mkHdrPng["dark  (offline)",    panelPhys];

globeSheet = ImageAssemble[
  {{hdr1,       hdr2,      hdr3},
   {reliefPad,  tiledPad,  darkPad}},
  Background -> White];

globeSheetPath = FileNameJoin[{outDir, "sheet_globe_styles.png"}];
Export[globeSheetPath, globeSheet, "PNG"];
Module[{sz, dims},
  dims = ImageDimensions[globeSheet];
  sz   = FileByteCount[globeSheetPath];
  Print["EXPORTED: sheet_globe_styles.png  ",
        dims[[1]], "\[Times]", dims[[2]], " px  ", Round[sz/1024], " KB"]];

(* ==========================================================================
   PART B -- COMPARISON SHEET 2: light vs dark theme
   Three figures x two themes = 3 x 2 grid (figures in rows, themes in cols)
   ========================================================================== *)
Print[""];
Print["=== PART B: Sheet 2 -- light vs dark themes ==="];
(* Target: ~2000 px wide sheet, 2 columns. Each column ~1000 px physical.
   ImageResize after Rasterize gives exact pixel control independent of Retina. *)
colPhys = 1000;

(* Render at full imgW then resize to colPhys *)
Print["  rendering light panels..."];
f2L = ImageResize[Rasterize[makeFig2[tcL]], colPhys];
f9L = ImageResize[Rasterize[makeFig9[tcL]], colPhys];
f7L = ImageResize[Rasterize[makeFig7[tcL]], colPhys];

Print["  rendering dark panels..."];
f2D = ImageResize[Rasterize[makeFig2[tcD]], colPhys];
f9D = ImageResize[Rasterize[makeFig9[tcD]], colPhys];
f7D = ImageResize[Rasterize[makeFig7[tcD]], colPhys];

(* Theme headers at physical colPhys width *)
thHdrL = ImageResize[
  Rasterize[Graphics[{Text[Style["LIGHT THEME",
                                  Directive[FontFamily->"Helvetica", FontSize->13,
                                            Bold, RGBColor[0.13,0.29,0.53]]],
                           Scaled[{0.5, 0.5}]]},
                     Background -> GrayLevel[0.94], ImageSize -> {500, 30}]], colPhys];
thHdrD = ImageResize[
  Rasterize[Graphics[{Text[Style["DARK THEME",
                                  Directive[FontFamily->"Helvetica", FontSize->13,
                                            Bold, GrayLevel[0.9]]],
                           Scaled[{0.5, 0.5}]]},
                     Background -> RGBColor[0.10, 0.12, 0.18], ImageSize -> {500, 30}]], colPhys];

(* Fig name labels spanning full width = 2 * colPhys *)
fullPhys = 2 * colPhys;
figLbl[name_] := ImageResize[
  Rasterize[Graphics[{Text[Style[name,
                                  Directive[FontFamily->"Helvetica", FontSize->11,
                                            Bold, GrayLevel[0.3]]],
                           Scaled[{0.5, 0.5}]]},
                     Background -> GrayLevel[0.97], ImageSize -> {800, 22}]], fullPhys];

(* Equalise heights within each figure row using ImagePad *)
eqH[imgA_, imgB_] := Module[{hA, hB},
  hA = ImageDimensions[imgA][[2]];
  hB = ImageDimensions[imgB][[2]];
  {ImagePad[imgA, {{0,0},{0,Max[0, hB-hA]}}, White],
   ImagePad[imgB, {{0,0},{0,Max[0, hA-hB]}}, RGBColor[0.07, 0.09, 0.13]]}
];

{f2LE, f2DE} = eqH[f2L, f2D];
{f9LE, f9DE} = eqH[f9L, f9D];
{f7LE, f7DE} = eqH[f7L, f7D];

(* Assemble: all rows are fullPhys = 2*colPhys wide *)
hdrRow   = ImageAssemble[{{thHdrL, thHdrD}}];
pairRow2 = ImageAssemble[{{f2LE,   f2DE}}];
pairRow9 = ImageAssemble[{{f9LE,   f9DE}}];
pairRow7 = ImageAssemble[{{f7LE,   f7DE}}];

themeSheet = ImageAssemble[
  {{hdrRow},
   {figLbl["Intercept Method"]},
   {pairRow2},
   {figLbl["Celestial vs GPS"]},
   {pairRow9},
   {figLbl["Error Ellipse"]},
   {pairRow7}},
  Background -> White];

themeSheetPath = FileNameJoin[{outDir, "sheet_theme_light_vs_dark.png"}];
Export[themeSheetPath, themeSheet, "PNG"];
Module[{sz, dims},
  dims = ImageDimensions[themeSheet];
  sz   = FileByteCount[themeSheetPath];
  Print["EXPORTED: sheet_theme_light_vs_dark.png  ",
        dims[[1]], "\[Times]", dims[[2]], " px  ", Round[sz/1024], " KB"]];

Print[""];
Print["=== ALL EXPORTS COMPLETE ==="];
