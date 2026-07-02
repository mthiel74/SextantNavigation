#!/usr/bin/env wolframscript
(* hero_globe.wls  --  Item 17a: Hero animation – CINEMATIC celestial-navigation opener
   A dark glowing Earth turning in a seeded star field with a faint Milky Way band,
   the Atlantic great-circle track drawing in, three luminous circles of equal altitude
   sweeping on and converging on a pulsing golden FIX.  Offline (CountryData vector
   coastlines, no map tiles).

   Outputs:  docs/images/hero_globe.gif   (target < 6 MB)
             docs/images/hero_globe.mp4   (target < 1 MB)

   Hard-won macOS-rasteriser fixes preserved:
     * NO Glow on Line/Tube primitives (salmon-rectangle bug) — brightness/glow on the
       coloured Lines is created by a POST-PROCESS bloom pass on the raster instead.
     * fixWash inverse-blend: the rasteriser alpha-blends a uniform ~8.4% #FF5454 wash
       over every rasterised frame; invert exactly then crop the 3 px edge fringe.
       Applied to BOTH the Graphics3D frames and the 2-D star layers (each Rasterize
       call adds one wash, so correct each before compositing).
     * Offline CountryData globe.
   Run: /usr/local/bin/wolframscript -file wolfram/hero_globe.wls                        *)

$CharacterEncoding = "UTF-8";
Print["hero_globe.wls starting ", DateString[]];

repoBase = ParentDirectory[DirectoryName[$InputFileName]];
pkgFile  = FileNameJoin[{repoBase, "wolfram",        "CelestialNavigation.wl"}];
gifOut   = FileNameJoin[{repoBase, "docs", "images", "hero_globe.gif"}];
mp4Out   = FileNameJoin[{repoBase, "docs", "images", "hero_globe.mp4"}];

If[!DirectoryQ[FileNameJoin[{repoBase,"docs","images"}]],
  CreateDirectory[FileNameJoin[{repoBase,"docs","images"}]]];

Get[pkgFile];
Print["Package loaded: v", cnVersion[]];

(* ─────────────────────────────── Colours ──────────────────────────────────── *)
colOcean  = RGBColor[0.02,  0.05, 0.11];      (* deep space-blue ocean       *)
colAtmos  = RGBColor[0.10,  0.35, 0.85];      (* atmospheric rim glow        *)
colLand   = RGBColor[0.13,  0.16, 0.20];      (* dark slate land             *)
colLandE  = RGBColor[0.30,  0.36, 0.44];      (* land edge highlight         *)
colTrack  = RGBColor[0.35,  0.82, 1.00];      (* cyan Atlantic track         *)
colBody1  = RGBColor[1.00,  0.62, 0.18];      (* circle 1 – warm amber       *)
colBody2  = RGBColor[0.40,  0.92, 1.00];      (* circle 2 – ice cyan         *)
colBody3  = RGBColor[0.82,  0.48, 1.00];      (* circle 3 – violet           *)
colFix    = RGBColor[1.00,  0.88, 0.38];      (* fix pulse – bright gold     *)
circColors = {colBody1, colBody2, colBody3};
NATIVE    = 760;

(* The macOS rasteriser can alpha-blend a uniform #FF5454 wash over a rasterised
   frame.  In this build it is strong on complex 2-D content (the star layers) and
   absent from the 3-D globe raster.  Rather than hard-code 8.4%, SELF-CALIBRATE the
   wash fraction a from a near-black reference in each raster and invert exactly:
   true = (px - a*W)/(1 - a),  W = {1, 0.33, 0.33}.  This is a no-op when a~0, so it
   never corrupts an unwashed raster while still fully correcting a washed one. *)
washInv[img_, a_] := ImageApply[Clip[(# - a {1., 0.33, 0.33})/(1. - a), {0, 1}] &, img];
(* a from the darkest floor (10th-percentile red) — content-filled star rasters *)
washSub[img_] := washInv[img, Clip[Quantile[Flatten[ImageData[img][[All, All, 1]]], 0.10], {0., 0.5}]];

(* ─────────────────────────────── Helpers ──────────────────────────────────── *)
ll3D[lat_, lon_] := N[{
  Cos[lat Degree] Cos[lon Degree],
  Cos[lat Degree] Sin[lon Degree],
  Sin[lat Degree]}];

gcSlerp[p1_, p2_, t_] := Module[{v1, v2, om},
  v1 = ll3D @@ p1;  v2 = ll3D @@ p2;
  om = ArcCos[Clip[v1 . v2, {-1, 1}]];
  If[om < 1*^-8, v1,
    (Sin[(1-t)*om] v1 + Sin[t*om] v2) / Sin[om]]];

eqAltCircle[gp_, alt_, n_:200] := Module[{gpv, ar, arb, u, v},
  gpv = ll3D[gp[[1]], gp[[2]]];
  ar  = (90. - alt) Degree;
  arb = If[Abs[gpv . {0,0,1}] < 0.85, {0,0,1}, {1,0,0}];
  u   = Normalize[Cross[gpv, arb]];
  v   = Normalize[Cross[gpv, u]];
  Table[1.004 (Cos[ar] gpv + Sin[ar] (Cos[t] u + Sin[t] v)),
    {t, 0., 2 Pi, 2 Pi/n}]];

decimate[pts_, maxPts_] :=
  If[Length[pts] <= maxPts, pts,
    pts[[;;  ;; Ceiling[Length[pts]/maxPts]]]];

cl01[x_]   := Clip[N[x], {0., 1.}];
easeIO[x_] := With[{t = cl01[x]}, If[t < 0.5, 2 t^2, 1 - 2 (1-t)^2]];

(* ─────────────────────────────── Star field (seeded, 2-D) ─────────────────── *)
(* Built once as a fixed background image the globe occludes.  Deterministic via
   SeedRandom so the hero is reproducible.  Each Rasterize is wash-corrected. *)
Print["Building star field..."];
SeedRandom[1729];
mkStars[sz_] := Module[{bg, fp, mag, ssz, scol, heroP, heroS, heroC,
    mwT, mwPerp, mwPos, mwN, dir, nrm, bandLine, prims, img0, gS, gB, img, band},
  bg = RGBColor[0.004, 0.007, 0.020];                 (* deep-space blue-black  *)
  dir = Normalize[{1., 0.62}]; nrm = {-dir[[2]], dir[[1]]};
  (* soft luminous Milky-Way band: a thick blurred diagonal stroke *)
  bandLine = Table[{0.5,0.5} + s dir, {s, -1.3, 1.3, 0.02}];
  band = washSub@Rasterize[Graphics[{AbsoluteThickness[40], CapForm["Round"],
      RGBColor[0.17,0.20,0.34], Line[bandLine]}, Background -> Black,
      PlotRange -> {{0,1},{0,1}}, PlotRangePadding -> None, ImagePadding -> None,
      ImageMargins -> 0, ImageSize -> sz], Background -> Black,
      RasterSize -> sz, ImageSize -> {sz, sz}];
  band = ImageResize[band, {sz, sz}];
  band = ImageMultiply[GaussianFilter[band, sz/22], 0.55];
  (* dense faint dust stars clustered along the band *)
  mwN = 2800; mwT = RandomReal[{-0.25, 1.25}, mwN];
  mwPerp = RandomVariate[NormalDistribution[0., 0.05], mwN];
  mwPos = Table[{0.5,0.5} + (mwT[[i]]-0.5) dir + mwPerp[[i]] nrm, {i, mwN}];
  mwPos = Select[mwPos, 0 <= #[[1]] <= 1 && 0 <= #[[2]] <= 1 &];
  (* scattered field stars of varied brightness (few bright, many faint) *)
  fp  = RandomReal[{0,1}, {1200, 2}];
  mag = RandomReal[{0,1}, 1200]^2.0; ssz = 1.3 + 3.2 mag;
  scol = Table[If[RandomReal[] < 0.72,
       RGBColor[0.86,0.91,1.0] (0.66 + 0.44 mag[[i]]),    (* blue-white *)
       RGBColor[1.0,0.83,0.60] (0.66 + 0.44 mag[[i]])], {i,1200}]; (* warm *)
  (* a handful of bright hero stars *)
  heroP = RandomReal[{0.05,0.95}, {18, 2}];
  heroS = RandomReal[{4.0,6.5}, 18];
  heroC = Table[If[RandomReal[] < 0.7, RGBColor[0.90,0.94,1.0],
                   RGBColor[1.0,0.85,0.62]], {18}];
  prims = {
    Sequence @@ Table[{RGBColor[0.62,0.68,0.88] RandomReal[{0.30,0.78}],
       Disk[mwPos[[i]], RandomReal[{0.0009,0.0022}]]}, {i, Length[mwPos]}],
    Sequence @@ Table[{scol[[i]],  Disk[fp[[i]],    ssz[[i]]/sz]},   {i, 1200}],
    Sequence @@ Table[{heroC[[i]], Disk[heroP[[i]], heroS[[i]]/sz]}, {i, 18}]};
  img0 = washSub@Rasterize[Graphics[prims, Background -> Black,
      PlotRange -> {{0,1},{0,1}}, PlotRangePadding -> None, ImagePadding -> None,
      ImageMargins -> 0, ImageSize -> sz], Background -> Black,
      RasterSize -> sz, ImageSize -> {sz, sz}];
  img0 = ImageResize[img0, {sz, sz}];
  gS = GaussianFilter[img0, sz/380]; gB = GaussianFilter[img0, sz/130];
  img = ImageAdd[img0, ImageMultiply[gS, 1.1], ImageMultiply[gB, 0.55]];  (* sparkle *)
  img = ImageAdd[img, band];
  img = ImageAdd[img, ConstantImage[bg, {sz, sz}]];
  ImagePad[ImagePad[img, -3], 3, bg]];                (* crop rasteriser edge fringe *)
starStatic = ImageResize[mkStars[820], {NATIVE, NATIVE}];
Print["  star field ready."];

(* ─────────────────────────────── Country polygons ─────────────────────────── *)
Print["Loading country polygons..."];
processCountry[c_] := Module[{raw, rings, ring, pts},
  raw = Quiet[CountryData[c, "Polygon"]];
  If[!MatchQ[raw, _Polygon], Return[{}]];
  rings = Quiet[raw[[1, 1]]];
  If[!ListQ[rings] || Length[rings] < 1, Return[{}]];
  Flatten[Table[
    ring = rings[[k]];
    If[!ListQ[ring] || Length[ring] < 3, Nothing,
      pts = decimate[N[ring], 250];
      {Polygon[1.0015 (ll3D[#[[1]], #[[2]]] & /@ pts)]}
    ], {k, Length[rings]}], 1]];
allC = CountryData[];
Print["  ", Length[allC], " country entities found."];
landPolys = Flatten[Table[processCountry[c], {c, allC}], 1];
landPolys = DeleteCases[landPolys, {} | Nothing | Null];
Print["  ", Length[landPolys], " land polygons loaded."];

(* ─────────────────────────────── Fix geometry ─────────────────────────────── *)
lasP   = {28.13,  -15.43};   (* Las Palmas departure *)
rodB   = {14.07,  -60.95};   (* Rodney Bay arrival   *)
fixPos = {21.5,   -38.0};    (* mid-Atlantic DR fix  *)

baseNight = DateObject[{2024, 11, 27, 19, 0, 0}, TimeZone -> 0];
fixTime   = Null;
{starNames, gps, alts} = {{},{},{}};
Do[
  tryT   = DatePlus[baseNight, {k/2., "Hours"}];
  tryVis = cnVisibleStars[fixPos, tryT, 15.];
  If[Length[tryVis] >= 3,
    tryTrip = cnBestStarTriplet[fixPos, tryT, 15.];
    If[tryTrip["gdop"] < 3.0,
      fixTime   = tryT;
      starNames = tryTrip["stars"];
      gps       = cnBodyGPFor[{"Star", #}, fixTime] & /@ starNames;
      alts      = cnAltitudeFromGP[fixPos, #] & /@ gps;
      Print["  Found fix time: ", DateString[fixTime]];
      Print["  Stars: ", StringRiffle[starNames, ", "]];
      Print["  Alts:  ", N[alts, 3], "\[Degree]"];
      Print["  GDOP:  ", N[tryTrip["gdop"], 3]];
      Break[]]], {k, 0, 10}];
If[fixTime === Null,
  Print["WARNING: no good triplet found; using default stars at 22:30 UTC"];
  fixTime   = DateObject[{2024, 11, 27, 22, 30, 0}, TimeZone -> 0];
  visNow    = cnVisibleStars[fixPos, fixTime, 10.];
  starNames = Take[visNow, Min[3, Length[visNow]]];
  gps       = cnBodyGPFor[{"Star", #}, fixTime] & /@ starNames;
  alts      = cnAltitudeFromGP[fixPos, #] & /@ gps];

gpVecs  = ll3D[#[[1]], #[[2]]] & /@ gps;
circles = MapThread[eqAltCircle, {gps, alts}];
nTrk    = 100;
trackAll = Table[1.007 gcSlerp[lasP, rodB, t/nTrk], {t, 0, nTrk}];
fixVec  = ll3D @@ fixPos;

(* ─────────────────────────────── Camera ───────────────────────────────────── *)
(* Cinematic narrative sweep: begin over the western Atlantic (lon -80) and drift
   east+up, settling centred on the mid-Atlantic FIX (lon -38) for the final beat. *)
nFrames = 110;
vpLat = 16.; vpLon0 = -80.; totalRot = 42.; viewAngle = 0.98;

(* ─────────────────────────────── Frame builder (Graphics3D) ────────────────── *)
globeFrame[i_] := Module[{f, rot, vp, elev, trackN, c1f, c2f, c3f, fixPhase, fixR,
    trkPrims, cirPrims, gpPrims, fixPrims, pulsePrims},
  f = i / (nFrames - 1.);
  rot  = easeIO[f] * totalRot Degree;
  elev = 0.22 Sin[Pi f];                             (* gentle camera lift        *)
  vp   = RotationTransform[rot, {0,0,1}][3.6 ll3D[vpLat, vpLon0]] + {0,0,elev};

  trackN = Max[2, Floor[cl01[(i-3)/46.] * (nTrk+1)]];
  c1f = easeIO[(i-26)/30.]; c2f = easeIO[(i-46)/30.]; c3f = easeIO[(i-64)/30.];
  fixPhase = If[i >= 84, (i-84)*0.42, 0.];
  fixR     = 0.034 + 0.016 Sin[fixPhase];

  (* Atlantic track (bright cyan Line — bloom adds the glow) + endpoint markers *)
  trkPrims = {
    Directive[colTrack, Opacity[0.95], Thickness[0.0040]],
    If[trackN >= 2, Line[trackAll[[;; trackN]]], {}],
    Directive[colTrack, Opacity[cl01[(i-3)/15.]], Glow[colTrack]],
    Sphere[1.014 ll3D @@ lasP, 0.019], Sphere[1.014 ll3D @@ rodB, 0.019]};

  (* Circles of equal altitude — bright Lines, NO Glow (bloom supplies the glow) *)
  cirPrims = Flatten[MapThread[
    Function[{col, cpts, cf},
      If[cf < 0.02, {},
        With[{n = Max[3, Floor[cf * Length[cpts]]]},
          {Directive[col, Opacity[0.97], Thickness[0.0050]], Line[cpts[[;; n]]]}]]],
    {circColors, circles, {c1f, c2f, c3f}}], 1];

  (* Geographical-position markers (Glow on Sphere is safe) *)
  gpPrims = Flatten[MapThread[
    Function[{col, gpv, cf},
      If[cf < 0.08, {},
        {Directive[col, Opacity[Min[1., cf*2.]], Glow[col]], Sphere[1.018 gpv, 0.016]}]],
    {circColors, gpVecs, {c1f, c2f, c3f}}], 1];

  (* Expanding-ring pulse when each circle finishes drawing *)
  pulsePrims = Flatten[MapThread[
    Function[{col, gpv, cf},
      If[0.55 < cf < 1.0,
        With[{g = (cf - 0.55)/0.45},
          {Directive[col, Opacity[0.32 (1-g)], Glow[col]], Sphere[1.02 gpv, 0.02 + 0.11 g]}], {}]],
    {circColors, gpVecs, {c1f, c2f, c3f}}], 1];

  (* Pulsing golden FIX — layered haloes + an expanding ring *)
  fixPrims = If[i >= 80,
    With[{op = cl01[(i-80)/8.]},
      {Directive[colFix, Opacity[op],       Glow[colFix]], Sphere[1.026 fixVec, fixR],
       Directive[colFix, Opacity[op 0.45],  Glow[colFix]], Sphere[1.030 fixVec, fixR 1.9],
       Directive[colFix, Opacity[op 0.20],  Glow[colFix]], Sphere[1.035 fixVec, fixR 3.2],
       With[{g = FractionalPart[fixPhase/(2 Pi) + 0.001]},
         {Directive[colFix, Opacity[op 0.32 (1-g)], Glow[colFix]],
          Sphere[1.02 fixVec, 0.035 + 0.16 g]}]}], {}];

  Graphics3D[{
      (* lush layered atmospheric halo *)
      {Directive[colAtmos, Opacity[0.17]],                       Sphere[{0,0,0}, 1.020]},
      {Directive[RGBColor[0.08,0.28,0.75], Opacity[0.11]],       Sphere[{0,0,0}, 1.045]},
      {Directive[RGBColor[0.05,0.20,0.60], Opacity[0.065]],      Sphere[{0,0,0}, 1.080]},
      {Directive[RGBColor[0.04,0.14,0.45], Opacity[0.04]],       Sphere[{0,0,0}, 1.125]},
      {Directive[colOcean, Specularity[RGBColor[0.20,0.38,0.75], 14]], Sphere[{0,0,0}, 1.000]},
      {Directive[colLand,  Specularity[RGBColor[0.25,0.28,0.32], 3],
        EdgeForm[Directive[colLandE, Opacity[0.35], Thin]]], landPolys},
      trkPrims, cirPrims, gpPrims, pulsePrims, fixPrims},
    Background      -> Black,
    Lighting        -> {
      {"Directional", RGBColor[0.10, 0.20, 0.55], {3, -2, 3}},
      {"Directional", RGBColor[0.06, 0.05, 0.04], {-2, 1, 0}},
      {"Ambient",     RGBColor[0.03, 0.045, 0.09]}},
    ViewPoint       -> vp,
    ViewVertical    -> {0, 0, 1},
    ViewAngle       -> viewAngle,
    Boxed           -> False,
    SphericalRegion -> False,
    ImageSize       -> {NATIVE, NATIVE}]];

(* fixWash: self-calibrate the wash fraction a from the raster's true-black corners
   (a~0 for the globe raster here → no-op), invert, then crop the 3 px edge fringe. *)
fixWash[raw_] := Module[{a},
  a = Clip[Median[ImageValue[raw,
        {{4,4},{NATIVE-4,4},{4,NATIVE-4},{NATIVE-4,NATIVE-4}}][[All, 1]]], {0., 0.5}];
  ImagePad[ImagePad[washInv[raw, a], -3], 3, Black]];

(* Post-process bloom: glow the bright features (sidesteps the Glow-on-Line bug) *)
bloomPass[img_] := Module[{lum, mask, bloom},
  lum   = ColorConvert[img, "Grayscale"];
  mask  = ImageApply[Clip[(# - 0.30)/0.55, {0,1}] &, lum];
  bloom = ImageMultiply[GaussianFilter[ImageMultiply[img, mask], 7], 0.85];
  ImageAdd[img, bloom]];

(* Compose one finished frame: globe (bloomed) over the seeded star field.
   Rasterize returns 2x DPI; ImageResize to NATIVE so the globe layer matches
   the NATIVE star layer (mismatched sizes were the source of a red cast). *)
composeFrame[i_] := Module[{raw, wash, glowed, blue, alpha, invA},
  raw    = ImageResize[Rasterize[globeFrame[i], Background -> Black,
             ImageSize -> {NATIVE, NATIVE}], {NATIVE, NATIVE}];
  wash   = fixWash[raw];
  glowed = bloomPass[wash];
  blue   = ColorSeparate[wash][[3]];                 (* globe silhouette from blue *)
  alpha  = Blur[ImageApply[Clip[(# - 0.045)/0.05, {0,1}] &, blue], 3];
  invA   = ImageApply[1 - # &, alpha];
  ImageAdd[ImageMultiply[starStatic, invA], glowed]];

(* ─────────────────────────────── Timing test ──────────────────────────────── *)
Print["Timing test frame..."];
t0 = AbsoluteTime[]; f0 = composeFrame[0];  t1 = AbsoluteTime[];
f1 = composeFrame[95]; t2 = AbsoluteTime[];
Print["  Frame 0:  ", N[t1-t0, 3], " s   Frame 95: ", N[t2-t1, 3], " s"];
Print["  Estimated total: ", N[Round[(t1-t0+t2-t1)/2. nFrames/60., 0.1], 3], " min"];

(* ─────────────────────────────── Render all frames ────────────────────────── *)
Print["Rendering ", nFrames, " frames..."];
t0Total = AbsoluteTime[];
frames = Table[
  If[Mod[i, 10] == 0, Print["  ", i, "/", nFrames-1, "  ",
    N[AbsoluteTime[] - t0Total, 3], " s elapsed"]];
  composeFrame[i],
  {i, 0, nFrames - 1}];
Print["All frames done in ", N[AbsoluteTime[] - t0Total, 4], " s"];

(* corner probe: confirm clean near-black background, no maroon/rectangles *)
cornerCheck = Median[Flatten[
  ImageValue[#, {{5,5},{NATIVE-5,5},{5,NATIVE-5},{NATIVE-5,NATIVE-5}}] & /@ frames[[;; ;; 20]], 1]];
Print["Median corner pixel (should be ~deep blue-black, R<=G<=B): ", N[cornerCheck, 3]];

(* ─────────────────────────────── GIF export ───────────────────────────────── *)
Print["Exporting GIF (720 px)..."];
frames720 = ImageResize[#, {720, 720}] & /@ frames;
Export[gifOut, frames720, "GIF",
  "DisplayDurations" -> ConstantArray[0.065, nFrames],
  "AnimationRepetitions" -> Infinity,
  "ColorReductionDither" -> False];
gifSz = FileByteCount[gifOut];
Print["GIF size: ", N[gifSz/1024^2, 3], " MB"];

If[gifSz > 6*1024^2,
  Print["GIF > 6 MB; reducing to 640 px..."];
  frames640 = ImageResize[#, {640, 640}] & /@ frames;
  Export[gifOut, frames640, "GIF", "DisplayDurations" -> ConstantArray[0.07, nFrames],
    "AnimationRepetitions" -> Infinity, "ColorReductionDither" -> False];
  gifSz = FileByteCount[gifOut];
  Print["GIF (640 px) size: ", N[gifSz/1024^2, 3], " MB"]];
If[gifSz > 6*1024^2,
  Print["GIF still > 6 MB; reducing to 560 px..."];
  frames560 = ImageResize[#, {560, 560}] & /@ frames;
  Export[gifOut, frames560, "GIF", "DisplayDurations" -> ConstantArray[0.07, nFrames],
    "AnimationRepetitions" -> Infinity, "ColorReductionDither" -> False];
  gifSz = FileByteCount[gifOut];
  Print["GIF (560 px) size: ", N[gifSz/1024^2, 3], " MB"]];

(* ─────────────────────────────── MP4 export ───────────────────────────────── *)
Print["Exporting MP4 (1080 px)..."];
frames1080 = ImageResize[#, {1080, 1080}] & /@ frames;
mp4Ok = False;
Quiet[Export[mp4Out, frames1080, "MP4", "FrameRate" -> 24, "VideoEncoding" -> "H264"];
  mp4Ok = FileExistsQ[mp4Out] && FileByteCount[mp4Out] > 1000];
If[!mp4Ok,
  Print["MP4 direct failed; trying VideoEncode path..."];
  Quiet[Export[mp4Out, frames1080, {"Video", "H264"}, "FrameRate" -> 24];
    mp4Ok = FileExistsQ[mp4Out] && FileByteCount[mp4Out] > 1000]];
If[mp4Ok,
  mp4Sz = FileByteCount[mp4Out];
  Print["MP4 size: ", N[mp4Sz/1024^2, 3], " MB"],
  Print["MP4 export not available; skipping."]; mp4Sz = 0];

(* ─────────────────────────────── Summary ──────────────────────────────────── *)
Print[""];
Print["══════════════════════════════════════════════"];
Print["hero_globe.wls DONE  ", DateString[]];
Print["  GIF:      ", gifOut, "  (", N[gifSz/1024^2, 3], " MB)"];
If[mp4Ok, Print["  MP4:      ", mp4Out, "  (", N[mp4Sz/1024^2, 3], " MB)"]];
Print["  Frames:   ", nFrames];
Print["  Stars:    ", StringRiffle[starNames, ", "]];
Print["══════════════════════════════════════════════"];
