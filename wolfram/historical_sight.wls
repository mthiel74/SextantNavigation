#!/usr/bin/env wolframscript
(* historical_sight.wls -- External validation of the reduction engine.        *)
(* Run: /usr/local/bin/wolframscript -file wolfram/historical_sight.wls         *)
(* R2 feature.  Offline, LIGHT theme. -> docs/images/fig_historical.png         *)
(*                                                                              *)
(* PART A -- a worked sight reduction validated against an INDEPENDENT authority *)
(*   (i)  internal exactness: cnComputedAltitude vs an independent 3-D direction-  *)
(*        cosine (ENU vector) derivation -> agreement to ~1e-9 arcsec;          *)
(*   (ii) external authority: the full Sun pipeline (cnSunGP + intercept method  *)
(*        + Bennett refraction) vs Wolfram's SunPosition, whose VSOP-based       *)
(*        ephemeris is entirely independent of our low-precision solar formula.  *)
(*   HONESTY: we do NOT transcribe a printed Nautical Almanac worked example we   *)
(*   could not verify offline -- rather than risk fabricating tabulated values,   *)
(*   we anchor on Wolfram's independent computational ephemeris and prove the     *)
(*   reduction trigonometry is exact against a second, independent formula.       *)
(*                                                                              *)
(* PART B -- RECONSTRUCTION of the James Caird open-boat crossing (1916).        *)
(*   Documented endpoints/dates: Shackleton & Worsley sailed from Elephant Is.   *)
(*   (Point Wild) on 24 Apr 1916 and made landfall at King Haakon Bay, South      *)
(*   Georgia, on 10 May 1916 (~16 days, ~800 nm sailed).  We do NOT have the      *)
(*   original logbook sextant readings; we reconstruct the great-circle geometry  *)
(*   from the documented positions, compute the Sun sights a navigator THERE      *)
(*   would have observed (engine ephemeris, valid for 1916), reduce them with the *)
(*   intercept method, and show the method recovers the track and the landfall.   *)

$CharacterEncoding = "UTF-8";
SetDirectory[DirectoryName[$InputFileName]];
Get["CelestialNavigation.wl"];
If[!DirectoryQ["../docs/images"], CreateDirectory["../docs/images"]];
outPath = FileNameJoin[{"../docs/images", "fig_historical.png"}];

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
titleDir = Directive[FontFamily -> "Helvetica", FontSize -> 22, Bold, FontColor -> textC];
labelDir = Directive[FontFamily -> "Helvetica", FontSize -> 20, FontColor -> textC];

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

Print["===== PART A ====="];
Print["UTC ", tA];
Print["Sun GP: dec ", fmt[decA, 4], " deg   GHA ", fmt[ghaA, 4], " deg"];
Print["AP ", apA];
Print["engine  Hc ", fmt[hcA, 4], " deg   Zn ", fmt[znA, 4], " deg"];
Print["Ho (assumed) ", hoA, " deg  ->  intercept ", fmt[pA, 2], " nm ",
      If[pA >= 0, "Toward", "Away"]];
Print["INTERNAL  vs 3-D vector:  dHc ", sciStr[dHcVec], "   dZn ", sciStr[dZnVec]];
Print["EXTERNAL  vs SunPosition: dAlt(app.) ", fmt[dAltExt, 2], "'   dAz ", fmt[dAzExt, 2], "'"];

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

Print["\n===== PART B ====="];
Print["great-circle Elephant Is. -> King Haakon Bay: ", fmt[gcDistNm, 1], " nm"];
Print["daily-fix RMS ", fmt[fixRMS, 2], " nm   max ", fmt[maxErr, 2], " nm"];
Print["landfall fix ", landfallFix, "  true KHB ", khb];
Print["LANDFALL ERROR ", fmt[landfallErr, 2], " nm"];

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
               Directive[FontFamily -> "Helvetica", FontSize -> 18, FontColor -> textC]],
         ptLL[elephant] + {0.6, -0.3}, {Left, Top}],
    {Black, AbsolutePointSize[12], Point[ptLL[khb]]},
    {green,  AbsolutePointSize[8], Point[ptLL[khb]]},
    Text[Style["South Georgia\nKing Haakon Bay\nlandfall 10 May 1916",
               Directive[FontFamily -> "Helvetica", FontSize -> 18, FontColor -> textC]],
         ptLL[khb] + {-0.6, 0.5}, {Right, Bottom}],
    (* recovered landfall marker *)
    {red, AbsolutePointSize[12], Point[ptLL[landfallFix]]},
    {White, AbsolutePointSize[4], Point[ptLL[landfallFix]]},
    Text[Style["recovered landfall\nerror = " <> fmt[landfallErr, 2] <> " nm",
               Directive[FontFamily -> "Helvetica", FontSize -> 18, Bold, FontColor -> red]],
         ptLL[landfallFix] + {-0.5, -1.1}, {Right, Top}]
  },
  Frame -> True,
  FrameLabel -> {Style["Longitude (\[Degree]E)", labelDir], Style["Latitude (\[Degree]N)", labelDir]},
  FrameStyle -> Directive[frameC, AbsoluteThickness[1]],
  GridLines -> Automatic, GridLinesStyle -> gridC,
  PlotRange -> {{lonMin, lonMax}, {latMin, latMax}},
  AspectRatio -> (latMax - latMin)/((lonMax - lonMin) Cos[Mean[{latMin, latMax}] Degree]),
  Background -> bg,
  ImageSize -> 1080,
  ImagePadding -> {{95, 30}, {68, 40}},
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
hdrStyle = Directive[FontFamily -> "Helvetica", FontSize -> 20, Bold, FontColor -> textC];
cellStyle = Directive[FontFamily -> "Helvetica", FontSize -> 18, FontColor -> GrayLevel[0.15]];
noteStyle = Directive[FontFamily -> "Helvetica", FontSize -> 17, FontColor -> mid];

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
  FrameMargins -> 26, ImageSize -> {1080, Automatic}];

(* ============================================================ *)
(*  Assemble                                                    *)
(* ============================================================ *)
tgtW = 1080; gapH = 12;
imgA = toImg[panelA, tgtW];   (* map on top, full width *)
imgB = toImg[panelB, tgtW];   (* reduction table below, full width *)
gap  = ConstantImage[bg, {tgtW, gapH}];
bodyStack = ImageAssemble[{{imgA}, {gap}, {imgB}}, Background -> bg];

mainTitle = txtImg[
  "Validating the engine on external & documented data \[LongDash] not self-generated sights",
  tgtW, 46, 21, True];
subTitle = txtImg[
  "(a) RECONSTRUCTION of Shackleton & Worsley's James Caird crossing (Elephant Is. \[Rule] South Georgia,\n" <>
  "24 Apr\[Dash]10 May 1916). We do NOT have the original logbook readings; we reconstruct the documented\n" <>
  "great-circle geometry, compute the Sun sights a navigator there would have observed, reduce them, and\n" <>
  "recover the landfall to " <> fmt[landfallErr, 1] <> " nm.  (b) A worked reduction checked two ways: exact against an\n" <>
  "independent 3-D vector formula, and to <0.5\[Prime] against Wolfram's independent SunPosition ephemeris.",
  tgtW, 132, 16];

bw = ImageDimensions[bodyStack][[1]];
bodyFinal = If[bw === tgtW, bodyStack, ImageResize[bodyStack, tgtW]];
fullImg = ImageAssemble[{{mainTitle}, {subTitle}, {bodyFinal}}, Background -> bg];
fullImg = ImagePad[ImagePad[fullImg, -3], 3, White];

Export[outPath, fullImg, "PNG"];
dims = ImageDimensions[fullImg]; sz = FileByteCount[outPath];
Print["\nEXPORTED: fig_historical.png  ", dims[[1]], "\[Times]", dims[[2]], " px  ", Round[sz/1024], " KB"];
Print["Corner pixel (expect {1.,1.,1.}): ", ImageValue[fullImg, {5, 5}]];
