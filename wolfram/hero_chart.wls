#!/usr/bin/env wolframscript
(* hero_chart.wls -- Hero Animation C: Atlantic crossing, three-track replay
   Outputs: docs/images/hero_chart.gif  (760-860 px wide, <9 MB)
            docs/images/hero_chart.mp4  (~1080 px, <20 MB)
   72 frames (3 per voyage day), dark cinematic Mercator chart, offline.
   Named chars only. No Inset[] in GeoGraphics. No package load. *)

$CharacterEncoding = "UTF-8";

(* ── Paths ──────────────────────────────────────────────────────────── *)
repoDir  = "/Users/thiel/GitHub/SextantNavigation";
gifOut   = FileNameJoin[{repoDir, "docs", "images", "hero_chart.gif"}];
mp4Out   = FileNameJoin[{repoDir, "docs", "images", "hero_chart.mp4"}];

(* ── Helpers ──────────────────────────────────────────────────────── *)
fixJSON[x_List] := If[Length[x] > 0 && AllTrue[x, MatchQ[#, _Rule]&],
  Association[fixJSON /@ x], fixJSON /@ x];
fixJSON[x_Rule] := Rule[x[[1]], fixJSON[x[[2]]]];
fixJSON[x_]     := x;

geoDist[p1_, p2_] := QuantityMagnitude[
  UnitConvert[GeoDistance[GeoPosition[p1], GeoPosition[p2]], "NauticalMiles"]];

(* ── Load CSV ──────────────────────────────────────────────────────── *)
Print["Loading data..."];
csv  = Import[FileNameJoin[{repoDir, "data", "voyage.csv"}], "CSV"];
rows = Rest[csv];   (* skip header: day,datetimeUTC,latTrue,lonTrue,latDR,lonDR,... *)
nDays   = Length[rows];

dayNums   = rows[[All, 1]];                   (* 0..23 *)
truePos   = N[rows[[All, {3, 4}]]];           (* {lat,lon} *)
drPos     = N[rows[[All, {5, 6}]]];
dateStrs  = rows[[All, 2]];                   (* ISO strings *)

(* ── Load sights.json ──────────────────────────────────────────────── *)
sd         = fixJSON[Import[FileNameJoin[{repoDir, "data", "sights.json"}],    "JSON"]];
td         = fixJSON[Import[FileNameJoin[{repoDir, "data", "twilight_fixes.json"}], "JSON"]];
sDays      = sd["days"];
tDays      = td["days"];
runFix     = N[#["runningFix"] & /@ sDays];    (* {lat,lon} per day, running (noon) fix *)
runErr     = N[#["runningFixErrorNm"] & /@ sDays];
twiFix     = N[#["fix"] & /@ tDays];           (* {lat,lon} per day, twilight star fix *)
twiErr     = N[#["errorNm"] & /@ tDays];

(* Precompute DR error vs truth per day (nm) *)
drErrPerDay = Table[geoDist[truePos[[i]], drPos[[i]]], {i, 1, nDays}];

Print["Days: ", nDays, "  DR final error: ", Round[Last[drErrPerDay], 0.1], " nm"];
Print["Mean celestial running-fix error: ", Round[Mean[runErr], 0.1], " nm"];

(* ── Colours ─────────────────────────────────────────────────────── *)
colSea      = RGBColor[0.04,  0.06,  0.11];   (* dark navy *)
colLand     = RGBColor[0.10,  0.14,  0.10];   (* dark olive-grey land *)
colBorder   = RGBColor[0.22,  0.28,  0.22];   (* subtle border *)
colTrue     = RGBColor[0.20,  0.85,  0.45];   (* bright green: GPS truth *)
colDR       = RGBColor[0.95,  0.60,  0.20];   (* amber: dead reckoning *)
colCelest   = RGBColor[0.30,  0.80,  1.00];   (* cyan: sun running fix *)
colTwi      = RGBColor[0.60,  0.85,  1.00];   (* pale cyan: twilight star fix *)
colText     = RGBColor[0.88,  0.92,  0.96];
colSub      = RGBColor[0.58,  0.65,  0.75];
colGold     = RGBColor[1.00,  0.90,  0.30];

(* ── Geo region ──────────────────────────────────────────────────── *)
(* Atlantic, Canaries → Caribbean *)
latMin = 9.5; latMax = 32.0;
lonMin = -65.5; lonMax = -9.0;
geoRange = {{latMin, latMax}, {lonMin, lonMax}};

(* ── Land polygons (offline, CountryData built-in) ──────────────── *)
Print["Loading land polygons (offline CountryData)..."];
landNames = {
  "Morocco", "WesternSahara", "Mauritania", "Senegal",
  "GuineaBissau", "Guinea", "SierraLeone", "Liberia",
  "Portugal", "Spain",
  "Venezuela", "Colombia", "Trinidad", "Barbados",
  "CapeVerde"
};
landPrimitives = Flatten[DeleteCases[
  Table[
    Module[{p = CountryData[c, "Polygon"]},
      If[MissingQ[p] || p === $Failed || Head[p] === Symbol || p === Missing["NotAvailable"],
        Nothing,
        {FaceForm[colLand], EdgeForm[Directive[colBorder, AbsoluteThickness[0.5]]], p}]],
    {c, landNames}],
  Nothing], 1];
Print["Land polygon groups loaded: ", Length[landPrimitives] / 3];

(* ── Mercator coordinate projection ─────────────────────────────── *)
(* WL GeoGraphics[..., GeoProjection->"Mercator"] clips to the GeoRange.
   We compute matching normalised coords for the Epilog Text primitives. *)
mercY[lat_] := Log[Tan[Pi/4 + lat * Degree / 2]];
mercYmin = mercY[latMin]; mercYmax = mercY[latMax];

(* Geographic → Scaled[{x,y}] (0=left/bottom, 1=right/top) *)
geoScaled[lat_, lon_] := Scaled[{
  (lon - lonMin) / (lonMax - lonMin),
  (mercY[lat] - mercYmin) / (mercYmax - mercYmin)}];

(* ── Animation parameters ──────────────────────────────────────── *)
framesPerDay  = 3;          (* 24 days × 3 = 72 frames *)
totalFrames   = nDays * framesPerDay;
gifW          = 820;        (* px wide for GIF *)
gifH          = Round[gifW * (mercYmax - mercYmin) / (lonMax - lonMin) * 55/22];
mp4W          = 1080;
mp4H          = Round[mp4W * gifH / gifW];
(* Clamp to sensible 16:9-ish ratio *)
gifH          = Round[gifW * 0.55];
mp4H          = Round[mp4W * 0.55];

Print["GIF size: ", gifW, " x ", gifH, "  MP4: ", mp4W, " x ", mp4H];
Print["Total frames: ", totalFrames];

(* ── Interpolation helper ──────────────────────────────────────── *)
(* Frame f in 1..totalFrames → fractional day frac = 0.0 .. 23.67 *)
(* dayInt = whole day index (0-based), alpha = 0..1 within day *)
frameDay[f_]   := (f - 1.0) / framesPerDay;
frameDayInt[f_] := Min[Floor[frameDay[f]], nDays - 1];
frameAlpha[f_]  := frameDay[f] - frameDayInt[f];

interpPos[arr_, d_, a_] :=
  If[d + 2 > Length[arr],
    arr[[Length[arr]]],
    arr[[d + 1]] * (1 - a) + arr[[d + 2]] * a];

(* ── Build one GeoGraphics frame ─────────────────────────────────── *)
makeFrame[f_, imgSize_] :=
  Module[{d, a, isFinal,
          nVis, truePts, drPts, fixGP, twiGP, fErr, tErr,
          trueNow, drNow,
          trueTrack, drTrack,
          fixCircles, fixDots, twiCircles, twiDots,
          trueGlow, drGlow, startDot, endDot,
          dayStr, drErrNow, cErrNow,
          epilog, gfx},

    d       = frameDayInt[f];          (* 0..23 *)
    a       = frameAlpha[f];           (* 0..1 *)
    isFinal = (f == totalFrames);

    (* Current nose positions *)
    trueNow = interpPos[truePos, d, a];
    drNow   = interpPos[drPos,   d, a];

    (* Track point lists up to current nose *)
    truePts = GeoPosition /@ Append[truePos[[1 ;; d + 1]], trueNow];
    drPts   = GeoPosition /@ Append[drPos[[1 ;; d + 1]], drNow];
    (* Avoid duplicate if alpha≈0 *)
    If[a < 0.01, truePts = Most[truePts]; drPts = Most[drPts]];

    (* Visible fixes: days 0..d *)
    nVis = d + 1;
    fixGP = GeoPosition /@ runFix[[1 ;; nVis]];
    twiGP = GeoPosition /@ twiFix[[1 ;; nVis]];
    fErr  = runErr[[1 ;; nVis]];
    tErr  = twiErr[[1 ;; nVis]];

    (* Track lines *)
    trueTrack = If[Length[truePts] > 1,
      {colTrue, AbsoluteThickness[2.8], Opacity[0.95],
       GeoPath[truePts, "Rhumb"]},
      Nothing];
    drTrack = If[Length[drPts] > 1,
      {colDR, AbsoluteThickness[2.2], Opacity[0.85],
       GeoPath[drPts, "Rhumb"]},
      Nothing];

    (* Fix error circles (running sun fix) *)
    fixCircles = Table[
      {colCelest, Opacity[0.12], EdgeForm[{colCelest, Opacity[0.3], AbsoluteThickness[0.8]}],
       GeoCircle[fixGP[[i]], Quantity[fErr[[i]], "NauticalMiles"]]},
      {i, 1, nVis}];

    (* Twilight star fix circles *)
    twiCircles = Table[
      {colTwi, Opacity[0.08], EdgeForm[{colTwi, Opacity[0.2], AbsoluteThickness[0.6]}],
       GeoCircle[twiGP[[i]], Quantity[tErr[[i]], "NauticalMiles"]]},
      {i, 1, nVis}];

    fixDots = {colCelest, PointSize[0.012], Point[fixGP]};
    twiDots = {colTwi,    PointSize[0.009], Point[twiGP]};

    (* Glow dots at current nose *)
    trueGlow = {colTrue, Opacity[0.85], PointSize[0.016],
      Point[GeoPosition[trueNow]]};
    drGlow   = {colDR,   Opacity[0.75], PointSize[0.013],
      Point[GeoPosition[drNow]]};

    (* Start / end markers *)
    startDot = {White,   PointSize[0.017], Point[GeoPosition[truePos[[1]]]]};
    endDot   = {colGold, PointSize[0.017], Point[GeoPosition[truePos[[nDays]]]]};

    (* ── Text / HUD via Epilog (no Inset[]) ── *)
    dayStr   = DateString[dateStrs[[d + 1]], "ISODate"];   (* "2024-11-15" *)
    drErrNow = drErrPerDay[[d + 1]];
    cErrNow  = runErr[[d + 1]];

    epilog = {
      (* HUD top-left *)
      {RGBColor[0,0,0,0.50],
       Rectangle[Scaled[{0.01, 0.865}], Scaled[{0.42, 0.995}]]},
      Text[Style["Day " <> ToString[dayNums[[d + 1]]] <>
                 "  \[Bullet]  " <> dayStr,
            FontFamily -> "Helvetica", Bold, FontSize -> 13, FontColor -> colText],
        Scaled[{0.025, 0.975}], {-1, 1}],
      Text[Style["DR error: " <> ToString[NumberForm[drErrNow, {4,1}]] <> " nm" <>
                 "   \[VerticalSeparator]   " <>
                 "Celest: " <> ToString[NumberForm[cErrNow, {4,1}]] <> " nm",
            FontFamily -> "Helvetica", FontSize -> 11, FontColor -> colSub],
        Scaled[{0.025, 0.935}], {-1, 1}],
      (* Legend bottom-left *)
      {RGBColor[0,0,0,0.50],
       Rectangle[Scaled[{0.01, 0.01}], Scaled[{0.33, 0.175}]]},
      Text[Style["\[LongDash] True / GPS track",
            FontFamily -> "Helvetica", FontSize -> 10, FontColor -> colTrue],
        Scaled[{0.025, 0.155}], {-1, 0}],
      Text[Style["\[LongDash] Dead reckoning (cumulative drift)",
            FontFamily -> "Helvetica", FontSize -> 10, FontColor -> colDR],
        Scaled[{0.025, 0.118}], {-1, 0}],
      Text[Style["\[FilledCircle]  Sun running fix (\[PlusMinus] error circle)",
            FontFamily -> "Helvetica", FontSize -> 10, FontColor -> colCelest],
        Scaled[{0.025, 0.080}], {-1, 0}],
      Text[Style["\[FilledCircle]  Twilight star fix",
            FontFamily -> "Helvetica", FontSize -> 10, FontColor -> colTwi],
        Scaled[{0.025, 0.043}], {-1, 0}],
      (* Title top-right *)
      Text[Style["Atlantic Crossing  \[LongDash]  Celestial vs DR Navigation",
            FontFamily -> "Helvetica", Bold, FontSize -> 12,
            FontColor -> RGBColor[0.75, 0.82, 0.90]],
        Scaled[{0.98, 0.975}], {1, 1}]
    };

    (* Final-frame summary *)
    If[isFinal,
      AppendTo[epilog,
        Text[Style[
          "Final DR error: " <> ToString[Round[Last[drErrPerDay]]] <> " nm" <>
          "    Mean celestial: " <> ToString[Round[Mean[runErr], 0.1]] <> " nm" <>
          "    \[RightArrow] " <>
          ToString[Round[Last[drErrPerDay] / Mean[runErr]]] <> "\[Times] more accurate",
          FontFamily -> "Helvetica", Bold, FontSize -> 12, FontColor -> colGold],
         Scaled[{0.50, 0.045}], {0, 0}]]];

    GeoGraphics[
      Flatten[{
        landPrimitives,
        trueTrack,
        drTrack,
        twiCircles,
        twiDots,
        fixCircles,
        fixDots,
        trueGlow,
        drGlow,
        startDot,
        endDot
      }, 1],
      GeoBackground -> None,
      Background    -> colSea,
      GeoRange      -> geoRange,
      GeoProjection -> "Mercator",
      ImageSize     -> imgSize,
      Epilog        -> epilog
    ]
  ];

(* ── Render all frames ─────────────────────────────────────────── *)
Print["Rendering ", totalFrames, " GeoGraphics frames at ", gifW, " x ", gifH, " ..."];
t0 = AbsoluteTime[];

cropBorder[img_] := ImagePad[ImagePad[img, -3], 3, colSea];

gifRasters = Table[
  If[Mod[f, 12] == 1, Print["  frame ", f, "/", totalFrames,
    "  t=", Round[AbsoluteTime[] - t0, 1], "s"]];
  cropBorder @ Rasterize[makeFrame[f, {gifW, gifH}], Background -> colSea,
    ImageResolution -> 72],
  {f, 1, totalFrames}];

tElapsed = AbsoluteTime[] - t0;
Print["All frames rendered in ", Round[tElapsed, 1], " s  (",
  Round[tElapsed / totalFrames, 1], " s/frame)"];

(* ── Check / reduce GIF size ──────────────────────────────────── *)
(* Hold final frame for 2s *)
holdFrames = 24;    (* extra copies of last frame *)
gifAllFrames = Join[gifRasters, Table[Last[gifRasters], {holdFrames}]];
gifDurations = Join[
  Table[0.07, totalFrames],
  Table[0.08, holdFrames]];

Print["Exporting GIF (", Length[gifAllFrames], " frames)..."];
Export[gifOut, gifAllFrames,
  "GIF",
  "DisplayDurations" -> gifDurations,
  "AnimationRepetitions" -> Infinity,
  "ColorReductionDither" -> False];

gifBytes = FileByteCount[gifOut];
Print["GIF written: ", gifOut];
Print["GIF size: ", Round[gifBytes / 1024 / 1024, 2], " MB  (",
  gifBytes, " bytes)"];

(* If GIF > 9 MB, reduce colour depth *)
If[gifBytes > 9 * 1024^2,
  Print["GIF too large (", Round[gifBytes/1024^2, 1], " MB), reducing colours..."];
  (* Quantise each frame to 128 colours *)
  gifReduced = ColorQuantize[#, 128] & /@ gifAllFrames;
  Export[gifOut, gifReduced,
    "GIF",
    "DisplayDurations" -> gifDurations,
    "AnimationRepetitions" -> Infinity];
  gifBytes = FileByteCount[gifOut];
  Print["Reduced GIF: ", Round[gifBytes/1024^2, 2], " MB"]];

(* ── MP4 export ───────────────────────────────────────────────── *)
Print["Rasterizing ", totalFrames, " frames at ", mp4W, " x ", mp4H, " for MP4..."];
t1 = AbsoluteTime[];
mp4Rasters = Table[
  If[Mod[f, 12] == 1, Print["  MP4 frame ", f, "/", totalFrames]];
  cropBorder @ Rasterize[makeFrame[f, {mp4W, mp4H}], Background -> colSea,
    ImageResolution -> 96],
  {f, 1, totalFrames}];
Print["MP4 rasters done in ", Round[AbsoluteTime[] - t1, 1], " s"];

(* Hold last frame *)
mp4All = Join[mp4Rasters, Table[Last[mp4Rasters], {36}]];

(* Save frames as PNG then encode with ffmpeg for reliable H264 *)
Print["Saving MP4 PNG frames to temp dir..."];
tmpDir = FileNameJoin[{$TemporaryDirectory,
  "heroframes_" <> ToString[Floor[AbsoluteTime[]]]}];
CreateDirectory[tmpDir];
Table[
  Export[FileNameJoin[{tmpDir, "frame_" <> IntegerString[f, 10, 4] <> ".png"}],
    mp4All[[f]], "PNG"],
  {f, 1, Length[mp4All]}];
Print["PNG frames saved to ", tmpDir];

Print["Running ffmpeg to encode MP4..."];
ffCmd = "/opt/homebrew/bin/ffmpeg -y -framerate 12 -pattern_type glob -i '" <>
  FileNameJoin[{tmpDir, "frame_*.png"}] <>
  "' -c:v libx264 -pix_fmt yuv420p -crf 20 -movflags +faststart '" <> mp4Out <> "'";
ffResult = RunProcess[{"bash", "-c", ffCmd}];
If[ffResult["ExitCode"] != 0,
  Print["ffmpeg stderr: ", ffResult["StandardError"]]];
mp4Bytes = If[FileExistsQ[mp4Out], FileByteCount[mp4Out], 0];
Print["MP4 written: ", mp4Out];
Print["MP4 size: ", Round[mp4Bytes / 1024 / 1024, 2], " MB  (", mp4Bytes, " bytes)"];
(* Cleanup temp dir *)
DeleteDirectory[tmpDir, DeleteContents -> True];

(* ── Summary ──────────────────────────────────────────────────── *)
Print[""];
Print["=== HERO ANIMATION C COMPLETE ==="];
Print["GIF: ", gifOut, "  ", gifW, " x ", gifH, " px  ",
  Length[gifAllFrames], " frames  ", Round[gifBytes/1024^2, 2], " MB"];
Print["MP4: ", mp4Out, "  ", mp4W, " x ", mp4H, " px  ",
  Length[mp4All], " frames  ", Round[mp4Bytes/1024, 0], " KB"];
Print["Total render time: ", Round[AbsoluteTime[] - t0, 1], " s"];
