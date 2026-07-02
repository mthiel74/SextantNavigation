#!/usr/bin/env wolframscript
(* hero_sextant.wls  --  Item 17b: Hero animation B, "Taking a Sight"
   Exports: docs/images/hero_sextant.gif  (700 px, <8 MB)
            docs/images/hero_sextant.mp4  (1080 px, <20 MB)
   Dark cinematic. Sextant schematic + altitude readout + LOP reveal.
   NO Inset[] of typeset content. Text[] primitives only.
   Named chars (\[Degree]). Offline. *)

$CharacterEncoding = "UTF-8";

repoBase = ParentDirectory[DirectoryName[$InputFileName]];
outGif   = FileNameJoin[{repoBase, "docs", "images", "hero_sextant.gif"}];
outMp4   = FileNameJoin[{repoBase, "docs", "images", "hero_sextant.mp4"}];

Print["hero_sextant.wls starting..."];

(* ================================================================
   PALETTE
   ================================================================ *)
skyDeep   = RGBColor[0.03, 0.05, 0.12];
skyHoriz  = RGBColor[0.10, 0.18, 0.32];
seaDeep   = RGBColor[0.02, 0.06, 0.14];
seaLight  = RGBColor[0.06, 0.14, 0.28];
horizCol  = RGBColor[0.55, 0.75, 0.95];
sunCore   = RGBColor[1.00, 0.95, 0.70];
sunGlow1  = RGBColor[1.00, 0.75, 0.30, 0.55];
sunGlow2  = RGBColor[1.00, 0.55, 0.10, 0.22];
brassCol  = RGBColor[0.82, 0.65, 0.25];
brassLight= RGBColor[0.96, 0.88, 0.55];
brassDark = RGBColor[0.50, 0.36, 0.08];
readoutCol= RGBColor[0.90, 0.85, 0.50];
flashCol  = RGBColor[1.00, 0.97, 0.80, 0.85];
lopCol    = RGBColor[0.35, 0.90, 0.65];
lopGlow   = RGBColor[0.35, 0.90, 0.65, 0.30];
textCol   = RGBColor[0.92, 0.90, 0.80];
dimText   = RGBColor[0.60, 0.58, 0.48];
chartBg   = RGBColor[0.04, 0.08, 0.18];
chartGrid = RGBColor[0.12, 0.20, 0.35];

(* ================================================================
   SCENE PARAMETERS
   ================================================================ *)
(* The overall canvas: two panels side by side
   Left panel: sky/sea/sextant scene  (0,0)-(1,1) coords
   Right panel: chart inset           -- but we use ONE Graphics
   with coordinate system {x in [0,2], y in [0,1]} to avoid Inset *)

(* Horizon y-position in scene coords *)
horizY = 0.42;

(* Sextant geometry (in scene [0,1]x[0,1] coords) *)
sxCx  = 0.50;   (* sextant arc centre x *)
sxCy  = 0.18;   (* sextant arc centre y *)
sxR   = 0.30;   (* arc radius *)
sxA0  = 25 Degree;   (* arc start angle (from +x axis, CCW) *)
sxA1  = 155 Degree;  (* arc end angle *)

(* Index arm: pivots at (sxCx, sxCy).
   At frame 0 the arm points high (body high in sky).
   At frame end the arm brings the body to the horizon. *)
armAngleStart = 118 Degree;   (* arm angle at start, CCW from +x *)
armAngleEnd   = 62 Degree;    (* arm angle at end -- body at horizon *)

(* Sun true position in scene: starts high, stays fixed (real sun).
   The reflected image in the index mirror descends. *)
sunTrueX = 0.65;
sunTrueY = 0.72;

(* ================================================================
   ANIMATION STRUCTURE
   Three phases:
     Phase 1 (frames 0..59):  arm rotates, reflected body descends,
                               altitude readout ticks
     Phase 2 (frames 60..74): flash at coincidence
     Phase 3 (frames 75..109): LOP / chart inset fades in, arm holds
   Total: 110 frames
   ================================================================ *)
nFrames     = 110;
phase1End   = 59;
phase2Start = 60;
phase2End   = 74;
phase3Start = 75;

(* Altitude: starts at ~65 deg, ends at ~32 deg *)
altStart = 65.4;
altEnd   = 32.1;

(* ================================================================
   HELPERS
   ================================================================ *)
smooth[t_] := t^2 (3 - 2 t);   (* smoothstep *)
clamp[x_, a_, b_] := Max[a, Min[b, x]];

(* Glow disc: several discs with decreasing opacity *)
glowDisc[x_, y_, r_, col_, n_:4] :=
  Table[{Opacity[(1 - k/(n+1))^2 * 0.55 + 0.05],
         col, Disk[{x, y}, r * (1 + 1.8 k/n)]},
        {k, 0, n}];

(* Graduated arc tick marks *)
arcTicks[cx_, cy_, r_, a0_, a1_, nTick_] :=
  Table[
    With[{a = a0 + (a1 - a0) k/(nTick-1),
          inner = r - 0.012, outer = r + 0.008},
      Line[{{cx + inner Cos[a], cy + inner Sin[a]},
            {cx + outer Cos[a], cy + outer Sin[a]}}]],
    {k, 0, nTick-1}];

arcMinorTicks[cx_, cy_, r_, a0_, a1_, nTick_] :=
  Table[
    With[{a = a0 + (a1 - a0) k/(nTick-1),
          inner = r - 0.007, outer = r + 0.004},
      Line[{{cx + inner Cos[a], cy + inner Sin[a]},
            {cx + outer Cos[a], cy + outer Sin[a]}}]],
    {k, 0, nTick-1}];

(* ================================================================
   BACKGROUND (sky + sea gradient, horizon line)
   Left panel x in [0,1], right panel x in [1,2]
   ================================================================ *)
makeBackground[] := {
  (* Sky gradient left panel -- bands *)
  Table[
    With[{y0 = horizY + (1-horizY)*k/40,
          y1 = horizY + (1-horizY)*(k+1)/40,
          frac = k/40},
      {Blend[{skyHoriz, skyDeep}, frac^0.6],
       Rectangle[{0, y0}, {1, y1}]}],
    {k, 0, 39}],
  (* Sea gradient left panel *)
  Table[
    With[{y0 = k/40 * horizY,
          y1 = (k+1)/40 * horizY,
          frac = k/40},
      {Blend[{seaDeep, seaLight}, frac^0.5],
       Rectangle[{0, y0}, {1, y1}]}],
    {k, 0, 39}],
  (* Right panel (chart area) background *)
  {chartBg, Rectangle[{1, 0}, {2, 1}]},
  (* Horizon line *)
  {Opacity[0.85], horizCol, AbsoluteThickness[1.2],
   Line[{{0, horizY}, {1, horizY}}]},
  (* Subtle sea shimmer lines *)
  Table[
    With[{y = horizY - 0.01 - k*0.035,
          opacity = 0.08 + 0.05*Mod[k,2]},
      {Opacity[opacity], horizCol,
       AbsoluteThickness[0.5],
       Line[{{0.05 + k*0.02, y}, {0.95 - k*0.01, y}}]}],
    {k, 1, 6}],
  (* Panel divider *)
  {Opacity[0.3], GrayLevel[0.5], AbsoluteThickness[1],
   Line[{{1, 0}, {1, 1}}]}
};

(* ================================================================
   SUN (true, always visible in sky on left panel)
   ================================================================ *)
makeSun[] := {
  (* Outer atmospheric glow *)
  {Opacity[0.12], sunGlow2, Disk[{sunTrueX, sunTrueY}, 0.10]},
  {Opacity[0.25], sunGlow2, Disk[{sunTrueX, sunTrueY}, 0.07]},
  {Opacity[0.50], sunGlow1, Disk[{sunTrueX, sunTrueY}, 0.045]},
  {Opacity[0.80], sunGlow1, Disk[{sunTrueX, sunTrueY}, 0.028]},
  (* Core *)
  {Opacity[1.0],  sunCore,  Disk[{sunTrueX, sunTrueY}, 0.016]}
};

(* ================================================================
   SEXTANT SCHEMATIC
   The sextant arc sits lower-left of the scene.
   We draw: arc limb, index arm, mirrors, frame struts.
   ================================================================ *)
makeSextant[armAngle_] := Module[
  {p0, p1, piv, armTip, mPos, mPerp, mArm, hmPos,
   tStart, tEnd, tPerp, prims},

  p0   = {sxCx + sxR*Cos[sxA0], sxCy + sxR*Sin[sxA0]};
  p1   = {sxCx + sxR*Cos[sxA1], sxCy + sxR*Sin[sxA1]};
  piv  = {sxCx, sxCy};

  armTip = {sxCx + sxR*Cos[armAngle], sxCy + sxR*Sin[armAngle]};
  mPos   = {sxCx + 0.06*Cos[armAngle], sxCy + 0.06*Sin[armAngle]};
  mPerp  = {-Sin[armAngle], Cos[armAngle]};
  mArm   = {Cos[armAngle], Sin[armAngle]};

  hmPos  = {sxCx + 0.06, sxCy + 0.02};

  tStart = {sxCx + 0.05*Cos[sxA0], sxCy + 0.05*Sin[sxA0]};
  tEnd   = {sxCx + 0.22*Cos[sxA0], sxCy + 0.22*Sin[sxA0]};
  tPerp  = {-Sin[sxA0], Cos[sxA0]};

  {
    (* Arc limb *)
    {Opacity[0.7], brassDark, AbsoluteThickness[8],
     Circle[{sxCx, sxCy}, sxR, {sxA0, sxA1}]},
    {Opacity[0.9], brassCol, AbsoluteThickness[4],
     Circle[{sxCx, sxCy}, sxR, {sxA0, sxA1}]},
    {Opacity[0.5], brassLight, AbsoluteThickness[1.5],
     Circle[{sxCx, sxCy}, sxR + 0.006, {sxA0 + 5*Degree, sxA1 - 5*Degree}]},

    (* Tick marks *)
    {Opacity[0.85], brassLight, AbsoluteThickness[1.2],
     arcTicks[sxCx, sxCy, sxR, sxA0, sxA1, 20]},
    {Opacity[0.55], brassCol, AbsoluteThickness[0.7],
     arcMinorTicks[sxCx, sxCy, sxR, sxA0, sxA1, 80]},

    (* Side struts *)
    {Opacity[0.6], brassDark, AbsoluteThickness[5], Line[{piv, p0}]},
    {Opacity[0.9], brassCol,  AbsoluteThickness[2.5], Line[{piv, p0}]},
    {Opacity[0.6], brassDark, AbsoluteThickness[5], Line[{piv, p1}]},
    {Opacity[0.9], brassCol,  AbsoluteThickness[2.5], Line[{piv, p1}]},

    (* Index arm shadow then arm *)
    {Opacity[0.7], brassDark, AbsoluteThickness[5], Line[{piv, armTip}]},
    {Opacity[1.0], brassLight, AbsoluteThickness[2], Line[{piv, armTip}]},
    {Opacity[0.9], brassLight, Disk[armTip, 0.008]},

    (* Index mirror *)
    {Opacity[0.80], brassCol,
     Polygon[{mPos + 0.015*mPerp - 0.004*mArm,
              mPos - 0.015*mPerp - 0.004*mArm,
              mPos - 0.015*mPerp + 0.013*mArm,
              mPos + 0.015*mPerp + 0.013*mArm}]},
    {Opacity[0.95], brassLight, AbsoluteThickness[1],
     Line[{mPos + 0.015*mPerp - 0.004*mArm,
           mPos - 0.015*mPerp - 0.004*mArm,
           mPos - 0.015*mPerp + 0.013*mArm,
           mPos + 0.015*mPerp + 0.013*mArm,
           mPos + 0.015*mPerp - 0.004*mArm}]},

    (* Horizon mirror (fixed) *)
    {Opacity[0.75], brassCol,
     Polygon[{hmPos + {0, 0.015}, hmPos + {0, -0.015},
              hmPos + {0.014, -0.015}, hmPos + {0.014, 0.015}}]},
    {Opacity[0.90], brassLight, AbsoluteThickness[1],
     Line[{hmPos + {0, 0.015}, hmPos + {0, -0.015},
           hmPos + {0.014, -0.015}, hmPos + {0.014, 0.015},
           hmPos + {0, 0.015}}]},

    (* Telescope barrel *)
    {Opacity[0.65], brassDark, AbsoluteThickness[7], Line[{tStart, tEnd}]},
    {Opacity[0.85], brassCol,  AbsoluteThickness[4], Line[{tStart, tEnd}]},
    {Opacity[0.60], brassLight, AbsoluteThickness[1.2],
     Line[{tStart + 0.012*tPerp, tEnd + 0.012*tPerp}]},
    {Opacity[0.90], brassLight, AbsoluteThickness[2], Circle[tEnd, 0.014]},

    (* Pivot hub *)
    {Opacity[0.95], brassDark, Disk[piv, 0.018]},
    {Opacity[1.00], brassLight, Disk[piv, 0.010]},
    {Opacity[0.80], brassDark,  Disk[piv, 0.004]}
  }
];

(* ================================================================
   REFLECTED SUN IMAGE (descends toward horizon as arm rotates)
   The reflected image x-tracks near the telescope line of sight;
   y interpolates from high in sky to the horizon.
   ================================================================ *)
makeReflectedSun[t_, flash_] :=
  With[{reflY = Lerp[0.78, horizY + 0.003, smooth[t]],
        reflX = 0.55 + 0.04 * (1 - t),
        opacity = (1 - 0.7 flash)},
  {
    {Opacity[opacity * 0.30], sunGlow2, Disk[{reflX, reflY}, 0.042]},
    {Opacity[opacity * 0.60], sunGlow1, Disk[{reflX, reflY}, 0.024]},
    {Opacity[opacity * 0.85], sunGlow1, Disk[{reflX, reflY}, 0.014]},
    {Opacity[opacity * 1.00], sunCore,  Disk[{reflX, reflY}, 0.009]},
    (* Dashed guide line from reflected sun to horizon *)
    With[{dashOp = 0.25 * (1 - flash)},
     {Opacity[dashOp], horizCol, AbsoluteThickness[0.5],
      Dashing[{0.012, 0.008}],
      Line[{{reflX, reflY}, {reflX, horizY}}]}]
  }];

Lerp[a_, b_, t_] := a + (b - a) t;

(* ================================================================
   ALTITUDE READOUT
   ================================================================ *)
makeReadout[alt_, phase_] :=
  (* phase: 0=ticking, 1=flash, 2=LOP *)
  With[{altStr = ToString[IntegerPart[alt]] <> "\[Degree] " <>
                 ToString[Round[Mod[alt, 1]*60, 0.1]] <> "'",
        boxX = 0.68, boxY = 0.85,
        boxW = 0.28, boxH = 0.10,
        opBox = If[phase == 1, 0.95, 0.80]},
  {
    (* Box background *)
    {Opacity[opBox * 0.7], RGBColor[0.02, 0.05, 0.15],
     Rectangle[{boxX - boxW/2, boxY - boxH/2}, {boxX + boxW/2, boxY + boxH/2}]},
    {Opacity[opBox * 0.9], brassCol, AbsoluteThickness[1.2],
     Rectangle[{boxX - boxW/2, boxY - boxH/2}, {boxX + boxW/2, boxY + boxH/2}]},
    (* Label *)
    {Opacity[opBox * 0.70], dimText,
     Text[Style["Hs", FontFamily -> "Courier", FontSize -> 8],
          {boxX - 0.07, boxY + 0.025}]},
    (* Altitude value *)
    {Opacity[opBox], readoutCol,
     Text[Style[altStr, FontFamily -> "Courier", Bold, FontSize -> 12],
          {boxX, boxY - 0.010}]}
  }];

(* ================================================================
   FLASH OVERLAY
   ================================================================ *)
makeFlash[flashT_] :=
  If[flashT <= 0, {},
  {{Opacity[flashT * 0.90], flashCol,
    Rectangle[{0, 0}, {1, 1}]}}];

(* ================================================================
   CHART INSET (right panel, x in [1,2], y in [0,1])
   Shows circle of equal altitude (LOP) with "You are on this line"
   ================================================================ *)
makeChart[lopT_] :=
  If[lopT <= 0, {},
  With[{
    (* Chart centre in panel coords *)
    cx = 1.50, cy = 0.50,
    (* Assumed position dot *)
    apX = 1.50, apY = 0.50,
    (* LOP circle radius in chart coords *)
    lopR = 0.25,
    (* Grid spacing *)
    gs  = 0.08
  },
  {
    (* Grid lines *)
    {Opacity[lopT * 0.40], chartGrid, AbsoluteThickness[0.5],
     Table[Line[{{1 + k*gs, 0.05}, {1 + k*gs, 0.95}}], {k, 0, 12}]},
    {Opacity[lopT * 0.40], chartGrid, AbsoluteThickness[0.5],
     Table[Line[{{1.02, k*gs}, {1.98, k*gs}}], {k, 0, 14}]},

    (* LOP glow circle *)
    {Opacity[lopT * 0.35], lopGlow,
     Circle[{cx, cy}, lopR + 0.018]},
    {Opacity[lopT * 0.20], lopGlow,
     Circle[{cx, cy}, lopR + 0.032]},

    (* LOP circle main *)
    {Opacity[lopT], lopCol, AbsoluteThickness[2.0],
     Circle[{cx, cy}, lopR]},

    (* Geographic position of body (GP) dot *)
    {Opacity[lopT * 0.80], sunCore,
     Disk[{cx - 0.03, cy - 0.05}, 0.010]},
    {Opacity[lopT * 0.60], sunGlow1,
     Disk[{cx - 0.03, cy - 0.05}, 0.018]},

    (* Assumed position cross *)
    {Opacity[lopT * 0.80], brassLight, AbsoluteThickness[1.5],
     Line[{{apX - 0.018, apY}, {apX + 0.018, apY}}]},
    {Opacity[lopT * 0.80], brassLight, AbsoluteThickness[1.5],
     Line[{{apX, apY - 0.018}, {apX, apY + 0.018}}]},

    (* Intercept line from AP to LOP *)
    {Opacity[lopT * 0.60], brassCol, AbsoluteThickness[1.0],
     Dashing[{0.015, 0.010}],
     Line[{{apX, apY}, {cx + lopR * 0.71, cy + lopR * 0.71}}]},

    (* Labels *)
    {Opacity[lopT * 0.80], sunCore,
     Text[Style["GP", FontFamily -> "Courier", FontSize -> 7],
          {cx - 0.03, cy - 0.07}]},

    {Opacity[lopT * 0.75], brassLight,
     Text[Style["AP", FontFamily -> "Courier", FontSize -> 7],
          {apX + 0.03, apY + 0.025}]},

    {Opacity[lopT], lopCol,
     Text[Style["Circle of equal altitude", FontFamily -> "Courier", FontSize -> 7],
          {cx, cy + lopR + 0.065}]},

    (* "You are somewhere on this line" *)
    {Opacity[lopT], textCol,
     Text[Style["You are somewhere", FontFamily -> "Courier", Bold, FontSize -> 9],
          {cx, cy - lopR - 0.085}]},
    {Opacity[lopT], textCol,
     Text[Style["on this line.", FontFamily -> "Courier", Bold, FontSize -> 9],
          {cx, cy - lopR - 0.115}]},

    (* Hs label in chart *)
    {Opacity[lopT * 0.70], dimText,
     Text[Style["Hs = 32\[Degree] 6'   Az = 214\[Degree]", FontFamily -> "Courier", FontSize -> 7],
          {cx, 0.10}]},

    (* Chart title *)
    {Opacity[lopT], brassLight,
     Text[Style["Line of Position", FontFamily -> "Courier", Bold, FontSize -> 10],
          {cx, 0.92}]}
  }]];

(* ================================================================
   TITLE / SUBTITLE
   ================================================================ *)
makeTitle[phase_] :=
  With[{op = If[phase < 2, 0.65, 0.85]},
  {
    {Opacity[op], brassLight,
     Text[Style["TAKING A SIGHT", FontFamily -> "Courier", Bold, FontSize -> 11],
          {0.50, 0.965}]},
    {Opacity[op * 0.75], dimText,
     Text[Style["Celestial Navigation  \[Bullet]  Sextant Altitude", FontFamily -> "Courier", FontSize -> 7],
          {0.50, 0.940}]}
  }];

(* ================================================================
   BUILD ONE FRAME
   ================================================================ *)
makeFrame[fi_] :=
  Module[{t, armAng, alt, flashT, lopT, phase,
          bg, sun, sext, reflSun, readout, flash, chart, title, prims},

    (* Phase and parameter values *)
    Which[
      fi <= phase1End,
        phase  = 0;
        t      = fi / phase1End;
        armAng = armAngleStart + (armAngleEnd - armAngleStart) smooth[t];
        alt    = altStart + (altEnd - altStart) smooth[t];
        flashT = 0;
        lopT   = 0,

      fi <= phase2End,
        phase  = 1;
        t      = 1.0;
        armAng = armAngleEnd;
        alt    = altEnd;
        With[{ft = (fi - phase2Start)/(phase2End - phase2Start)},
          flashT = If[ft < 0.4, ft/0.4, (1-ft)/0.6]];
        lopT   = 0,

      True,
        phase  = 2;
        t      = 1.0;
        armAng = armAngleEnd;
        alt    = altEnd;
        flashT = 0;
        lopT   = smooth[clamp[(fi - phase3Start)/30.0, 0, 1]]
    ];

    bg      = makeBackground[];
    sun     = makeSun[];
    sext    = makeSextant[armAng];
    reflSun = makeReflectedSun[t, flashT];
    readout = makeReadout[alt, phase];
    flash   = makeFlash[flashT];
    chart   = makeChart[lopT];
    title   = makeTitle[phase];

    prims = {bg, sun, sext, reflSun, readout, flash, chart, title};

    Graphics[prims,
      Background   -> Black,
      PlotRange    -> {{0, 2}, {0, 1}},
      AspectRatio  -> 0.5,
      ImageSize    -> {1400, 700},
      ImagePadding -> None]
  ];

(* ================================================================
   RENDER ALL FRAMES
   ================================================================ *)
Print["Rendering ", nFrames, " frames..."];
t0 = AbsoluteTime[];
frames = Table[
  If[Mod[i, 10] == 0, Print["  frame ", i, "/", nFrames]];
  makeFrame[i],
  {i, 0, nFrames - 1}];
Print["Render done in ", Round[AbsoluteTime[] - t0, 0.1], " s"];

(* ================================================================
   EXPORT GIF (~700 px wide, downsized from 1400)
   ================================================================ *)
Print["Exporting GIF..."];
gifFrames = ImageResize[Rasterize[#, RasterSize -> {1400, 700}], {760, 380}] & /@ frames;
Export[outGif, gifFrames,
  "GIF",
  "AnimationRepetitions" -> Infinity,
  "DisplayDurations" -> 0.06];
gifSize = FileByteCount[outGif];
Print["GIF: ", outGif];
Print["GIF size: ", gifSize, " bytes (", Round[gifSize/1024^2, 0.01], " MB)"];

(* ================================================================
   EXPORT MP4 (~1080 px wide, from 1400 rasterise then resize)
   ================================================================ *)
Print["Exporting MP4..."];
mp4Frames = ImageResize[Rasterize[#, RasterSize -> {1400, 700}], {1080, 540}] & /@ frames;
Export[outMp4, mp4Frames,
  "MP4",
  "FrameRate" -> 16];
mp4Size = FileByteCount[outMp4];
Print["MP4: ", outMp4];
Print["MP4 size: ", mp4Size, " bytes (", Round[mp4Size/1024^2, 0.01], " MB)"];

Print["Frame count: ", nFrames];
Print["Done."];
