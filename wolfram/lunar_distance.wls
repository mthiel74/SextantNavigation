#!/usr/bin/env wolframscript
(* lunar_distance.wls — Figure: the lunar-distance method (GMT, hence longitude,
   without a chronometer).  Maskelyne's lunar tables (1767) vs Harrison's H4 (1759).
   Left  : geocentric Moon-Sun distance vs GMT (the "clock in the sky", ~0.5 deg/hr) with
           the cleared observed distance marked -> the GMT solution.
   Right : recovered longitude error vs lunar-distance measurement error (the line of slope
           ~0.56 deg/arcmin that pins the historical ~0.5 deg accuracy), with Harrison's
           chronometer regime for comparison (~30-60x better).
   Output: docs/images/fig_lunar_distance.png
   Offline; LIGHT theme.
*)

$CharacterEncoding = "UTF-8";

repo = DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repo, "wolfram", "CelestialNavigation.wl"}]];
outPath = FileNameJoin[{repo, "docs", "images", "fig_lunar_distance.png"}];

(* ── Palette ─────────────────────────────────────────────────────────────── *)
blue   = RGBColor[0.122, 0.467, 0.706];
green  = RGBColor[0.173, 0.627, 0.173];
orange = RGBColor[0.890, 0.467, 0.008];
red    = RGBColor[0.839, 0.153, 0.157];
gray   = GrayLevel[0.55];
lw     = Directive[AbsoluteThickness[2.4]];

(* ── Scenario: observer {35N,40W}, 2024-06-13 18:00 UTC; both bodies ~45 deg up ── *)
truePos = {35.0, -40.0};
tTrue   = DateObject[{2024, 6, 13, 18, 0, 0}, TimeZone -> 0];
base    = AbsoluteTime[DateObject[{2024, 6, 13, 0, 0, 0}, TimeZone -> 0]];
hourTrue = (AbsoluteTime[tTrue] - base)/3600.;

dOfHour[h_?NumericQ] := cnLunarDistanceGeocentric[FromAbsoluteTime[base + h*3600., TimeZone -> 0]];

(* fixed-point apparent altitudes consistent with cnClearLunarDistance *)
appAltMoon[hg_, hp_] := Module[{h = hg}, Do[h = hg - ArcSin[Sin[hp Degree] Cos[h Degree]]/Degree + cnRefraction[h]/60., 5]; h];
appAltSun[hg_]       := Module[{h = hg}, Do[h = hg - 0.002443 Cos[h Degree] + cnRefraction[h]/60., 5]; h];

mGP = cnMoonGP[tTrue]; sGP = cnSunGP[tTrue]; hpM = cnMoonHP[tTrue];
hMg = cnAltitudeFromGP[truePos, mGP];  hSg = cnAltitudeFromGP[truePos, sGP];
dGeoTrue = cnLunarDistanceGeocentric[tTrue];
cosA = (Cos[dGeoTrue Degree] - Sin[hMg Degree] Sin[hSg Degree])/(Cos[hMg Degree] Cos[hSg Degree]);
hMa = appAltMoon[hMg, hpM];  hSa = appAltSun[hSg];
dObs = ArcCos[Clip[Sin[hMa Degree] Sin[hSa Degree] + Cos[hMa Degree] Cos[hSa Degree] cosA, {-1, 1}]]/Degree;
SeedRandom[42];
dNoisy = dObs + RandomVariate[NormalDistribution[0, 1.0/60.]];   (* 1' measurement noise *)
dClr   = cnClearLunarDistance[dNoisy, hMa, hSa, hpM];
tRec   = cnLunarDistanceGMT[dClr, DatePlus[tTrue, {1.5, "Hour"}]];
hourRec = (AbsoluteTime[tRec] - base)/3600.;
rate = Abs[dOfHour[hourTrue + 0.5] - dOfHour[hourTrue - 0.5]];   (* deg/hr *)

Print["D_geo(true) = ", dGeoTrue, " deg   D_obs(topo) = ", dObs, " deg"];
Print["clearing correction D_obs - D_geo = ", (dObs - dGeoTrue)*60, " arcmin (parallax + refraction)"];
Print["cleared (with 1' noise) = ", dClr, " deg   rate = ", rate, " deg/hr"];
Print["recovered GMT hour = ", hourRec, "  (true ", hourTrue, ")  err = ",
      (hourRec - hourTrue)*60, " min"];

(* breakdown of the clearing correction (for the caption / annotation) *)
moonParallax = ArcSin[Sin[hpM Degree] Cos[hMa Degree]]/Degree * 60;   (* arcmin *)
moonRefr     = cnRefraction[hMa];                                     (* arcmin *)
sunRefr      = cnRefraction[hSa];                                     (* arcmin *)
Print["Moon parallax-in-alt = ", moonParallax, "'  Moon refr = ", moonRefr,
      "'  Sun refr = ", sunRefr, "'"];

(* ═══════════════ LEFT PANEL — the clock in the sky ═══════════════ *)
hLo = 12.; hHi = 24.;
dBot = dOfHour[hLo] - 0.5; dTop = dOfHour[hHi] + 0.5;   (* distance increases with time *)
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
  (* cleared distance -> GMT solution: horizontal dashed at dClr, vertical at tRec *)
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

(* ═══════════════ RIGHT PANEL — accuracy: lunar vs chronometer ═══════════════ *)
(* longitude error (deg) = 15 deg/hr * GMT error; GMT error = distanceErr / rate.
   slope = 15 * (1/60) / rate  deg per arcmin of distance error. *)
slope = 15. * (1./60.) / rate;          (* deg longitude per arcmin distance error *)
lonErr[mArcmin_] := slope * mArcmin;
chronoDeg = 15. * (4./3600.);           (* H4 ~ a few s/day -> use 4 s: 0.0167 deg, the "1 nm" floor *)

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
  (* the canonical 1' -> ~0.5 deg point *)
  Graphics[{
    Directive[gray, AbsoluteThickness[1.2], Dashing[{0.015, 0.01}]],
    Line[{{1, 0}, {1, lonErr[1]}}], Line[{{0, lonErr[1]}, {1, lonErr[1]}}],
    {Directive[red, AbsolutePointSize[9]], Point[{1, lonErr[1]}]},
    Text[Style["1\[Prime] \[RightArrow] " <> ToString[NumberForm[lonErr[1], {3, 2}]] <> "\[Degree]  (\[TildeTilde] 2 min GMT)",
               FontSize -> 13, FontFamily -> "Arial", FontWeight -> Bold, FontColor -> red],
         {1, lonErr[1]}, {-0.06, -0.9}]
  }],
  (* Harrison chronometer regime — essentially on the axis *)
  Graphics[{
    Directive[green, AbsoluteThickness[2.4]],
    Line[{{0, chronoDeg}, {3, chronoDeg}}],
    Text[Style["Harrison H4 chronometer  (\[TildeTilde] 0.02\[Degree] \[Tilde] 1 nm)",
               FontSize -> 12, FontFamily -> "Arial", FontWeight -> Bold, FontColor -> green],
         {1.5, 0.13}, {0, 0}]
  }]
];

(* ═══════════════ COMPOSE ═══════════════ *)
(* Caption with explicit line breaks so its natural width does not exceed the
   plot row — otherwise ImageSize -> 1600 scales the whole column down and the
   panels become a tiny sliver (the old 1600x412 bug). *)
caption = "Lunar distances (Maskelyne\[CloseCurlyQuote]s Nautical Almanac, 1767) recover GMT from the Moon\[CloseCurlyQuote]s motion to \[TildeTilde] 2 min \[RightArrow]\n" <>
          "longitude to \[TildeTilde] 0.5\[Degree] (30 nm). Harrison\[CloseCurlyQuote]s H4 chronometer (1759) did \[TildeTilde] 30\[Dash]60\[Times] better, and won \[LongDash]\n" <>
          "but lunars needed no clock and were the cross-check at sea for decades.";

fig = Column[{
    GraphicsRow[{leftPlot, rightPlot}, Spacings -> 15, Background -> White],
    Style[caption, FontSize -> 15, FontFamily -> "Arial",
          FontColor -> GrayLevel[0.25], TextAlignment -> Center]
  },
  Alignment -> Center, Background -> White, ImageSize -> 1560];

Print["Exporting to ", outPath, " ..."];
Export[outPath, fig, "PNG", Background -> White, ImageSize -> 1560];

(* ── Verify ──────────────────────────────────────────────────────────────── *)
img  = Import[outPath];
dims = ImageDimensions[img];
sz   = FileByteCount[outPath];
Print["Dimensions : ", dims[[1]], " x ", dims[[2]], " px"];
Print["File size  : ", Round[sz/1024], " KB"];
Print["Top-left px : ", PixelValue[img, {1, 1}, "Byte"], "  (255,255,255 = white)"];
Print["Center px   : ", PixelValue[img, Round[dims/2], "Byte"]];
If[sz > 2*^6, Print["WARNING: file >2 MB (", Round[sz/1024], " KB)"]];
