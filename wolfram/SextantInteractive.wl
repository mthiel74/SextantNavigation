(* ::Package:: *)
(* SextantInteractive.wl -- Interactive Manipulate demonstrations for the       *)
(* SextantNavigation post (R3).  Each sxManip<Name>[] returns a Manipulate[...]  *)
(* object that lives in the notebook; the controls are inert in headless         *)
(* wolframscript but the object constructs and its initial frame rasterizes to   *)
(* a valid image.  Scenarios/bodies/grids are precomputed where possible so the  *)
(* sliders stay responsive.                                                      *)
(*                                                                               *)
(*   sxManipIntercept[]     -- drag the assumed position + observed altitude;    *)
(*                             watch Hc, intercept, azimuth and the LOP update    *)
(*                             (the Marcq St-Hilaire method, live).               *)
(*   sxManipPosterior[]     -- slide the number of sights and the sextant sigma   *)
(*                             (+ index-error toggle); watch the Bayesian         *)
(*                             posterior sharpen band -> blob -> peak.            *)
(*   sxManipSightPlanning[] -- choose 3 visible stars (+ a time slider); watch    *)
(*                             the cut-angle geometry, GDOP and CRLB error        *)
(*                             ellipse update -- experimental design, live.       *)

$CharacterEncoding = "UTF-8";

(* Engine-load guard (mirrors SextantFigures.wl): Get the sibling engine by      *)
(* absolute path if its context is absent, so this package Gets from anywhere.   *)
If[!MemberQ[$Packages, "CelestialNavigation`"],
  Get[FileNameJoin[{DirectoryName[$InputFileName], "CelestialNavigation.wl"}]]
];
If[!MemberQ[$Packages, "SextantFigures`"],
  Get[FileNameJoin[{DirectoryName[$InputFileName], "SextantFigures.wl"}]]
];

BeginPackage["SextantInteractive`", {"CelestialNavigation`", "SextantFigures`"}];

sxManipVersion::usage     = "sxManipVersion[] gives the SextantInteractive package version string.";
sxManipIntercept::usage   = "sxManipIntercept[] returns a Manipulate teaching the Marcq St-Hilaire intercept method: drag the assumed-position lat/lon and the observed altitude Ho; the computed altitude Hc, the intercept (Toward/Away, nm), the azimuth Zn and the resulting line of position update live, with the true position fixed.";
sxManipPosterior::usage   = "sxManipPosterior[] returns a Manipulate over the Bayesian position posterior: a slider for the NUMBER of sights (1->4) and the sextant sigma, plus an index-error toggle; the posterior heatmap sharpens band -> blob -> peak, with the MAP estimate and true position marked.  Uses cnPosteriorGrid on a precomputed fixed scenario (60x60 grid) for responsiveness.";
sxManipSightPlanning::usage = "sxManipSightPlanning[] returns a Manipulate turning GDOP/Fisher into an interactive experimental-design tool: choose 3 stars from a precomputed visible set and slide the observation time; the cut-angle geometry, the GDOP scalar and the Cramer-Rao error ellipse update live.";

sxManipInterceptPreview::usage    = "sxManipInterceptPreview[] returns the STATIC Graphics shown by sxManipIntercept[] at its default control values (a clean, non-interactive frame for print/PDF where Dynamic cannot run).";
sxManipPosteriorPreview::usage    = "sxManipPosteriorPreview[] returns the STATIC Graphics shown by sxManipPosterior[] at its default control values (a clean, non-interactive frame for print/PDF where Dynamic cannot run).";
sxManipSightPlanningPreview::usage = "sxManipSightPlanningPreview[] returns the STATIC Graphics shown by sxManipSightPlanning[] at its default control values (a clean, non-interactive frame for print/PDF where Dynamic cannot run).";

Begin["`Private`"];

sxManipVersion[] := "SextantInteractive v1.0.0";

(* Shared light-theme palette *)
$navy   = RGBColor[0.13, 0.29, 0.53];
$green  = RGBColor[0.13, 0.62, 0.30];
$orange = RGBColor[0.92, 0.49, 0.06];
$blue   = RGBColor[0.25, 0.45, 0.95];
$gray   = GrayLevel[0.55];

(* ==========================================================================
   sxManipIntercept -- Marcq St-Hilaire intercept method, live.
   ========================================================================== *)
(* Display body, factored out so it can be rendered statically (preview) and    *)
(* live (inside the Manipulate). gp/truePos are precomputed and passed in.       *)
sxManipInterceptFrame[apLat_, apLon_, Ho_, gp_, truePos_] :=
  Module[{ap, hc, zn, interceptNm, u, perp, apEN, ipEN, L, toEN, dir, statusStr},
    ap = {apLat, apLon};
    {hc, zn} = cnComputedAltitude[ap, gp];
    interceptNm = (Ho - hc) 60.;                         (* +ve = Toward *)
    u    = {Sin[zn Degree], Cos[zn Degree]};             (* E/N unit toward GP *)
    perp = {Cos[zn Degree], -Sin[zn Degree]};            (* LOP direction *)
    (* Local E/N tangent plane (nm) centred on the true position *)
    toEN[{la_, lo_}] := {(lo - truePos[[2]]) Cos[truePos[[1]] Degree] 60,
                          (la - truePos[[1]]) 60};
    apEN = toEN[ap];
    ipEN = apEN + interceptNm u;                         (* LOP foot point *)
    L = 28.0;                                            (* LOP half-length, nm *)
    dir = If[interceptNm >= 0, "Toward", "Away"];
    statusStr = StringJoin[
      "Hc = ", ToString[NumberForm[hc, {5, 2}]], "\[Degree]    ",
      "Ho = ", ToString[NumberForm[Ho, {5, 2}]], "\[Degree]    ",
      "Zn = ", ToString[NumberForm[Mod[zn, 360], {4, 1}]], "\[Degree]    ",
      "intercept = ", ToString[NumberForm[Abs[interceptNm], {4, 1}]], " nm ", dir];
    Column[{
      Style[statusStr, FontFamily -> "Helvetica", FontSize -> 15, Bold, FontColor -> $navy],
      Graphics[{
        (* grid *)
        {GrayLevel[0.9], Thin,
          Line /@ Table[{{x, -35}, {x, 35}}, {x, -30, 30, 10}],
          Line /@ Table[{{-35, y}, {35, y}}, {y, -30, 30, 10}]},
        (* line of position *)
        {$blue, AbsoluteThickness[3.5], Line[{ipEN - L perp, ipEN + L perp}]},
        (* intercept segment from AP to LOP foot, along azimuth *)
        {$orange, AbsoluteThickness[2.5], Arrowheads[0.04],
          Arrow[{apEN, ipEN}]},
        (* assumed position *)
        {$gray, AbsolutePointSize[10], Point[apEN]},
        {Black, Text[Style["AP", 13, FontFamily -> "Helvetica"], apEN, {1.4, -1.0}]},
        (* true position *)
        {$green, AbsolutePointSize[12], Point[{0., 0.}]},
        {$green, AbsoluteThickness[2], Circle[{0., 0.}, 2.2]},
        {Black, Text[Style["True position", 13, FontFamily -> "Helvetica"], {0., 0.}, {-1.2, 1.4}]}
        },
        PlotRange -> {{-35, 35}, {-35, 35}}, AspectRatio -> 1,
        Frame -> True, FrameLabel -> {"East (nm)", "North (nm)"},
        FrameStyle -> Directive[Black, 13, FontFamily -> "Helvetica"],
        Background -> White, ImageSize -> 560, ImagePadding -> {{55, 15}, {45, 15}}]
      }, Alignment -> Center]
  ];

(* Static preview at the Manipulate's DEFAULT control values (for the print PDF). *)
sxManipInterceptPreview[] := Module[{truePos, t, gp, trueHo},
  truePos = {20.0, -40.0};
  t  = DateObject[{2024, 11, 22, 14, 0, 0}, TimeZone -> 0];
  gp = cnSunGP[t];
  trueHo = cnAltitudeFromGP[truePos, gp];
  sxManipInterceptFrame[20.25, -39.75, trueHo, gp, truePos]
];

sxManipIntercept[] := Module[{truePos, t, gp, trueHo},
  truePos = {20.0, -40.0};
  t  = DateObject[{2024, 11, 22, 14, 0, 0}, TimeZone -> 0];
  gp = cnSunGP[t];
  trueHo = cnAltitudeFromGP[truePos, gp];

  Manipulate[
    sxManipInterceptFrame[apLat, apLon, Ho, gp, truePos],
    {{apLat, 20.25, "assumed lat (\[Degree]N)"}, 19.5, 20.5, 0.01, Appearance -> "Labeled"},
    {{apLon, -39.75, "assumed lon (\[Degree]E)"}, -40.5, -39.5, 0.01, Appearance -> "Labeled"},
    {{Ho, trueHo, "observed altitude Ho (\[Degree])"}, trueHo - 0.4, trueHo + 0.4, 0.01, Appearance -> "Labeled"},
    ControlPlacement -> Bottom,
    Initialization :> {},
    TrackedSymbols :> {apLat, apLon, Ho}
  ]
];

(* ==========================================================================
   sxManipPosterior -- Bayesian posterior sharpening, live.
   ========================================================================== *)
(* Display body, factored out. The precomputed scenario (truth/ts/box/ng/cf) is  *)
(* passed in; the three controls (nSights, sigma, indexErr) are the arguments.    *)
sxManipPosteriorFrame[nSights_, sigma_, indexErr_, truth_, ts_, box_, ng_, cf_] :=
  Module[{latMin, latMax, lonMin, lonMax, offset, sights, r, post, maxp, map,
          dLon, dLat, errNm, statusStr},
    {{latMin, latMax}, {lonMin, lonMax}} = box;
    offset = If[indexErr, 10.0/60., 0.0];       (* uncorrected index error, deg *)
    sights = Table[
      <|"body" -> "Sun", "t" -> ts[[k]],
        "Ho" -> cnAltitudeFromGP[truth, cnSunGP[ts[[k]]]] + offset|>,
      {k, nSights}];
    r    = cnPosteriorGrid[sights, box, ng, sigma];
    post = r["posterior"]; maxp = Max[post]; map = r["mapEstimate"];
    dLon = 0.04 (lonMax - lonMin); dLat = 0.04 (latMax - latMin);
    errNm = QuantityMagnitude[
      GeoDistance[GeoPosition[truth], GeoPosition[map]] / Quantity[1, "NauticalMiles"]];
    statusStr = StringJoin[
      ToString[nSights], If[nSights == 1, " sight", " sights"],
      "    \[Sigma] = ", ToString[NumberForm[sigma, {3, 1}]], "\[Prime]",
      "    MAP error = ", ToString[NumberForm[errNm, {4, 1}]], " nm",
      If[indexErr, "    (10\[Prime] index error \[Rule] biased peak)", ""]];
    Column[{
      Style[statusStr, FontFamily -> "Helvetica", FontSize -> 15, Bold, FontColor -> $navy],
      Graphics[{
        Raster[post, {{lonMin, latMin}, {lonMax, latMax}}, {0, maxp}, ColorFunction -> cf],
        {$green, AbsoluteThickness[2.5], Circle[{truth[[2]], truth[[1]]}, {dLon, dLat}]},
        {$green, AbsolutePointSize[5], Point[{truth[[2]], truth[[1]]}]},
        {$orange, AbsoluteThickness[3],
          Line[{{map[[2]] - dLon, map[[1]]}, {map[[2]] + dLon, map[[1]]}}],
          Line[{{map[[2]], map[[1]] - dLat}, {map[[2]], map[[1]] + dLat}}]}
        },
        PlotRange -> {{lonMin, lonMax}, {latMin, latMax}}, AspectRatio -> 1,
        Frame -> True, FrameLabel -> {"lon (\[Degree]E)", "lat (\[Degree]N)"},
        FrameStyle -> Directive[Black, 13, FontFamily -> "Helvetica"],
        Background -> White, ImageSize -> 560, ImagePadding -> {{60, 15}, {45, 15}}]
      }, Alignment -> Center]
  ];

sxManipPosteriorScenario[] := <|
  "truth" -> {20.0, -40.0},
  "ts" -> {DateObject[{2024, 11, 22, 9,  0, 0}, TimeZone -> 0],
           DateObject[{2024, 11, 22, 12, 0, 0}, TimeZone -> 0],
           DateObject[{2024, 11, 22, 15, 0, 0}, TimeZone -> 0],
           DateObject[{2024, 11, 22, 17, 0, 0}, TimeZone -> 0]},
  "box" -> {{19.5, 20.5}, {-40.5, -39.5}},
  "ng" -> 60,
  "cf" -> Function[v, Blend[{White, RGBColor[0.78, 0.88, 0.98],
                             RGBColor[0.32, 0.55, 0.92], $navy}, v]]|>;

(* Static preview. The live Manipulate opens at nSights=1, but a one-sight
   posterior is a faint band that washes out to near-white in print; we preview
   the representative 3-sight frame (the sharpened peak the demo is about). The
   other controls keep their defaults (sigma=1.5, no index error). *)
sxManipPosteriorPreview[] := Module[{s = sxManipPosteriorScenario[]},
  sxManipPosteriorFrame[3, 1.5, False,
    s["truth"], s["ts"], s["box"], s["ng"], s["cf"]]
];

sxManipPosterior[] := Module[{s = sxManipPosteriorScenario[], truth, ts, box, ng, cf},
  {truth, ts, box, ng, cf} = s /@ {"truth", "ts", "box", "ng", "cf"};

  Manipulate[
    sxManipPosteriorFrame[nSights, sigma, indexErr, truth, ts, box, ng, cf],
    {{nSights, 1, "number of sights"}, 1, 4, 1, Appearance -> "Labeled"},
    {{sigma, 1.5, "sextant \[Sigma] (arcmin)"}, 0.5, 5.0, 0.5, Appearance -> "Labeled"},
    {{indexErr, False, "uncorrected 10\[Prime] index error"}, {False, True}},
    ControlPlacement -> Bottom,
    ContinuousAction -> False,
    TrackedSymbols :> {nSights, sigma, indexErr}
  ]
];

(* ==========================================================================
   sxManipSightPlanning -- GDOP / CRLB as an interactive design tool, live.
   ========================================================================== *)
(* Display body, factored out. pos/baseTime are precomputed and passed in; the   *)
(* three star choices and the time offset are the arguments.                      *)
sxManipSightPlanningFrame[s1_, s2_, s3_, dtHours_, pos_, baseTime_] :=
  Module[{t, stars, azimuths, sigma, gdop, cov, ell, s95, ellPts, eig, vals,
          vecs, order, l1, l2, v1, v2, rays, maxR, statusStr, azStr},
    t = DatePlus[baseTime, {dtHours, "Hour"}];
    stars = {s1, s2, s3};
    sigma = 1.5;
    azimuths = cnComputedAltitude[pos, cnBodyGPFor[{"Star", #}, t]][[2]] & /@ stars;
    gdop = cnGDOPFromAzimuths[azimuths];
    cov  = cnCRLBCovariance[azimuths, sigma];
    ell  = cnErrorEllipse[cov];
    (* 95% ellipse point list (chi^2(2)=5.991 -> scale Sqrt[5.991]) *)
    s95 = Sqrt[5.991];
    eig = Eigensystem[N[cov]]; vals = eig[[1]]; vecs = eig[[2]];
    order = Ordering[vals, All, Greater];
    l1 = vals[[order[[1]]]]; l2 = vals[[order[[2]]]];
    v1 = vecs[[order[[1]]]]; v2 = vecs[[order[[2]]]];
    ellPts = If[gdop >= 9999. || ! AllTrue[{l1, l2}, Positive],
      {},  (* singular geometry: skip ellipse *)
      N @ Table[s95 (Sqrt[l1] Cos[th] v1 + Sqrt[l2] Sin[th] v2), {th, 0, 2 Pi, 2 Pi/180}]];
    (* azimuth rays (unit direction toward each GP), scaled for display *)
    maxR = If[ellPts === {}, 8., 1.35 Max[Abs @ Flatten[ellPts]]];
    maxR = Max[maxR, 6.];
    rays = {0.7 maxR {Sin[# Degree], Cos[# Degree]}} & /@ azimuths;
    azStr = StringRiffle[ToString[Round[#, 0.1]] <> "\[Degree]" & /@ azimuths, ", "];
    statusStr = StringJoin[
      "GDOP = ", If[gdop >= 9999., "\[Infinity] (clustered)", ToString[NumberForm[gdop, {4, 2}]]],
      "      CEP = ", ToString[NumberForm[ell["cep"], {4, 1}]], " nm",
      "      azimuths: ", azStr];
    Column[{
      Style[statusStr, FontFamily -> "Helvetica", FontSize -> 15, Bold, FontColor -> $navy],
      Graphics[{
        {GrayLevel[0.9], Thin,
          Circle[{0, 0}, maxR/2], Circle[{0, 0}, maxR],
          Line[{{-maxR, 0}, {maxR, 0}}], Line[{{0, -maxR}, {0, maxR}}]},
        (* azimuth rays *)
        {$orange, AbsoluteThickness[2.5], Arrowheads[0.045],
          Arrow[{{0, 0}, #[[1]]}] & /@ rays},
        MapThread[
          {Black, Text[Style[#2, 12, FontFamily -> "Helvetica"], 1.08 #1[[1]]]} &,
          {rays, stars}],
        (* CRLB 95% error ellipse *)
        If[ellPts === {}, {},
          {$blue, AbsoluteThickness[3.5], Line[Append[ellPts, First[ellPts]]]}],
        {Black, AbsolutePointSize[9], Point[{0., 0.}]}
        },
        PlotRange -> {{-maxR, maxR}, {-maxR, maxR}}, AspectRatio -> 1,
        Frame -> True, FrameLabel -> {"East (nm)", "North (nm)"},
        FrameStyle -> Directive[Black, 13, FontFamily -> "Helvetica"],
        PlotLabel -> Style["Cut-angle geometry + CRLB 95% error ellipse",
                           14, Bold, FontFamily -> "Helvetica", $navy],
        Background -> White, ImageSize -> 560, ImagePadding -> {{55, 15}, {45, 35}}]
      }, Alignment -> Center]
  ];

(* Precompute pos/baseTime and the selectable star menu ONCE. *)
sxManipSightPlanningSetup[] := Module[{pos, baseTime, visible, candidates, defaults},
  pos      = {20.0, -40.0};
  baseTime = DateObject[{2024, 11, 22, 22, 0, 0}, TimeZone -> 0];  (* evening twilight *)
  (* Visible navigational stars (>15 deg); take up to 8 brightest as the menu. *)
  visible = cnVisibleStars[pos, baseTime, 15];
  candidates = If[Length[visible] > 8,
    Take[SortBy[visible, cnLoadStars[][#]["mag"] &], 8], visible];
  defaults = PadRight[candidates, 3, candidates][[;; 3]];
  <|"pos" -> pos, "baseTime" -> baseTime,
    "candidates" -> candidates, "defaults" -> defaults|>
];

(* Static preview at the default star triple and zero time offset. *)
sxManipSightPlanningPreview[] := Module[{s = sxManipSightPlanningSetup[], d},
  d = s["defaults"];
  sxManipSightPlanningFrame[d[[1]], d[[2]], d[[3]], 0.0, s["pos"], s["baseTime"]]
];

sxManipSightPlanning[] := Module[
  {s = sxManipSightPlanningSetup[], pos, baseTime, candidates, defaults},
  {pos, baseTime, candidates, defaults} =
    s /@ {"pos", "baseTime", "candidates", "defaults"};

  Manipulate[
    sxManipSightPlanningFrame[s1, s2, s3, dtHours, pos, baseTime],
    {{s1, defaults[[1]], "star 1"}, candidates},
    {{s2, defaults[[2]], "star 2"}, candidates},
    {{s3, defaults[[3]], "star 3"}, candidates},
    {{dtHours, 0.0, "time offset (h)"}, -3.0, 3.0, 0.25, Appearance -> "Labeled"},
    ControlPlacement -> Bottom,
    ContinuousAction -> False,
    TrackedSymbols :> {s1, s2, s3, dtHours}
  ]
];

End[];

EndPackage[];
