#!/usr/bin/env wolframscript
(* ============================================================================
   generate_tables.wls
   Emits LaTeX table fragments for "The Sextant Navigator's Almanac 2026"
   from THIS project's validated ephemeris engine (wolfram/CelestialNavigation.wl).
   Run from the repo root:  wolframscript -file almanac/generate_tables.wls
   All data are computed offline; nothing is invented here.
   ============================================================================ *)

SetDirectory[DirectoryName[$InputFileName]];              (* almanac/ *)
repoRoot = ParentDirectory[];
Get[FileNameJoin[{repoRoot, "wolfram", "CelestialNavigation.wl"}]];
AppendTo[$ContextPath, "CelestialNavigation`Private`"];   (* expose cnGMST, cnSunGHA, ... *)

outDir = FileNameJoin[{Directory[], "tables"}];
If[!DirectoryQ[outDir], CreateDirectory[outDir]];
wf[name_, str_] := Export[FileNameJoin[{outDir, name}], str, "Text"];

(* ---------------------------------------------------------------------------
   Angle formatting helpers.  Work in integer tenths-of-arcminute to avoid any
   60.0' carry glitch.  \dg and \pr are macros defined in the .tex preamble.
   --------------------------------------------------------------------------- *)
pad2[n_] := If[n < 10, "0" <> ToString[n], ToString[n]];

(* minutes field for a time, zero-padded to two digits with one decimal, e.g. 3.5 -> "03.5" *)
padMin1[m_] := Module[{r, mi, te}, r = Round[N[m]*10]; mi = Quotient[r, 10]; te = Mod[r, 10];
  pad2[mi] <> "." <> ToString[te]];

(* GHA-style 0..360 *)
fmtGHA[g_] := Module[{a = Mod[N[g], 360], tt, deg, rem, mi, te},
  tt = Round[a*600]; If[tt >= 216000, tt -= 216000];
  deg = Quotient[tt, 600]; rem = Mod[tt, 600];
  mi = Quotient[rem, 10]; te = Mod[rem, 10];
  ToString[deg] <> "\\dg " <> pad2[mi] <> "." <> ToString[te] <> "\\pr"];

(* plain angle magnitude D MM.M (no mod), for increments etc *)
fmtAng[g_] := Module[{a = N[g], tt, deg, rem, mi, te},
  tt = Round[a*600]; deg = Quotient[tt, 600]; rem = Mod[tt, 600];
  mi = Quotient[rem, 10]; te = Mod[rem, 10];
  ToString[deg] <> "\\dg " <> pad2[mi] <> "." <> ToString[te] <> "\\pr"];

(* Declination with N/S hemisphere *)
fmtDec[d_] := Module[{hemi, a, tt, deg, rem, mi, te},
  hemi = If[d >= 0, "N", "S"]; a = Abs[N[d]];
  tt = Round[a*600]; deg = Quotient[tt, 600]; rem = Mod[tt, 600];
  mi = Quotient[rem, 10]; te = Mod[rem, 10];
  hemi <> ToString[deg] <> "\\dg " <> pad2[mi] <> "." <> ToString[te] <> "\\pr"];

(* arcminutes to 0.1' *)
fmtMin[m_] := ToString[NumberForm[Round[N[m], 0.1], {4, 1}]];

monthNames = {"January","February","March","April","May","June",
              "July","August","September","October","November","December"};
daysInMonth = {31,28,31,30,31,30,31,31,30,31,30,31};   (* 2026 not a leap year *)
tAt[y_,mo_,d_,h_] := DateObject[{y,mo,d,h,0,0}, TimeZone -> 0];

(* ---------------------------------------------------------------------------
   Generic N-up longtable emitter.
   entries: list of {c1,c2,...,ck} cell-string lists (each length blockCols)
   --------------------------------------------------------------------------- *)
emitLongtable[entries_, blocksPerRow_, blockCols_, blockHdr_, caption_, label_] :=
 Module[{colspec, ntot, hdr, groups, bodyRows, pad, s},
  colspec = StringRiffle[Table[StringJoin[Table["r", blockCols]], blocksPerRow], "|"];
  ntot = blocksPerRow*blockCols;
  hdr = StringRiffle[Flatten[Table[blockHdr, blocksPerRow]], " & "] <> " \\\\";
  pad = Table["", blockCols];
  groups = Partition[entries, blocksPerRow, blocksPerRow, {1, 1}, pad];
  bodyRows = (StringRiffle[Flatten[#], " & "] <> " \\\\") & /@ groups;
  s = "{\\footnotesize\n" <>
      "\\begin{longtable}{" <> colspec <> "}\n" <>
      "\\caption{" <> caption <> "}\\label{" <> label <> "}\\\\\n" <>
      "\\toprule\n" <> hdr <> "\n\\midrule\n\\endfirsthead\n" <>
      "\\multicolumn{" <> ToString[ntot] <> "}{c}{\\textit{" <> caption <>
      " (continued)}}\\\\\n\\toprule\n" <> hdr <> "\n\\midrule\n\\endhead\n" <>
      "\\midrule\\multicolumn{" <> ToString[ntot] <>
      "}{r}{\\textit{continued on next page}}\\\\\n\\endfoot\n" <>
      "\\bottomrule\n\\endlastfoot\n" <>
      StringRiffle[bodyRows, "\n"] <> "\n\\end{longtable}}\n";
  s];

Print["=== Generating Sun hourly tables (Part II) ==="];
sunHdr = {"\\textbf{d}", "\\textbf{h}", "\\textbf{GHA}", "\\textbf{Dec}"};
Do[
  entries = Flatten[Table[
      With[{t = tAt[2026, mo, d, h]},
        {ToString[d], ToString[h], fmtGHA[cnSunGHA[t]], fmtDec[cnSunGP[t][[1]]]}],
      {d, 1, daysInMonth[[mo]]}, {h, 0, 23}], 1];
  tbl = emitLongtable[entries, 3, 4, sunHdr,
     "Sun --- GHA and Declination, " <> monthNames[[mo]] <> " 2026 (UT)",
     "sun" <> pad2[mo]];
  wf["sun_" <> pad2[mo] <> ".tex", tbl];
  Print["  ", monthNames[[mo]], ": ", Length[entries], " rows"],
  {mo, 1, 12}];

Print["=== Generating GHA Aries hourly tables (Part III) ==="];
ariesHdr = {"\\textbf{d}", "\\textbf{h}", "\\textbf{GHA} $\\gamma$"};
Do[
  entries = Flatten[Table[
      With[{t = tAt[2026, mo, d, h]},
        {ToString[d], ToString[h], fmtGHA[Mod[cnGMST[t], 360]]}],
      {d, 1, daysInMonth[[mo]]}, {h, 0, 23}], 1];
  tbl = emitLongtable[entries, 4, 3, ariesHdr,
     "Aries --- GHA of the First Point of Aries, " <> monthNames[[mo]] <> " 2026 (UT)",
     "aries" <> pad2[mo]];
  wf["aries_" <> pad2[mo] <> ".tex", tbl];
  Print["  ", monthNames[[mo]], ": ", Length[entries], " rows"],
  {mo, 1, 12}];

(* ---------------------------------------------------------------------------
   Daily Sun: meridian passage at Greenwich, Equation of Time, semidiameter.
   Mer.pass. = UT when the Sun's GHA passes 0 (Sun over Greenwich meridian).
   EoT = 12h00m - Mer.pass.  (apparent - mean; +ve = sundial ahead of clock)
   --------------------------------------------------------------------------- *)
Print["=== Generating daily Sun table (mer.pass, EoT, SD) ==="];
tf[y_,mo_,d_,h_] := DateObject[{y,mo,d,0,0, h*3600.}, TimeZone->0];
sunSignedGHA[y_,mo_,d_,h_?NumericQ] := Mod[cnSunGHA[tf[y,mo,d,h]] + 180, 360] - 180;
merEoTSD[mo_, d_] := Module[{root, merH, eotMin, sd, hh, mm},
  root = h /. FindRoot[sunSignedGHA[2026, mo, d, h] == 0, {h, 12}];
  merH = root;                                     (* UT hours of transit *)
  eotMin = (12 - merH)*60;                         (* minutes, signed *)
  sd = cnSunSemidiameterAt[tAt[2026, mo, d, 12]];
  hh = Floor[merH]; mm = (merH - hh)*60;
  {hh, mm, eotMin, sd}];
fmtEoT[e_] := Module[{sgn, a, mm, ss},
  sgn = If[e >= 0, "+", "$-$"]; a = Abs[e];
  mm = Floor[a]; ss = Round[(a - mm)*60];
  If[ss == 60, ss = 0; mm += 1];
  sgn <> ToString[mm] <> ":" <> pad2[ss]];
sunDailyStr ="{\\footnotesize\n\\begin{longtable}{r r r r | r r r r}\n" <>
  "\\caption{Sun --- daily Meridian Passage at Greenwich, Equation of Time, and Semidiameter, 2026}\\label{sundaily}\\\\\n" <>
  "\\toprule\n\\multicolumn{4}{c|}{} & \\multicolumn{4}{c}{} \\\\\n" <>
  "\\textbf{Date} & \\textbf{Mer.Pass} & \\textbf{EoT} & \\textbf{SD} & \\textbf{Date} & \\textbf{Mer.Pass} & \\textbf{EoT} & \\textbf{SD} \\\\\n" <>
  " & (UT) & (m:s) & ($\\pr$) & & (UT) & (m:s) & ($\\pr$) \\\\\n\\midrule\n\\endfirsthead\n" <>
  "\\multicolumn{8}{c}{\\textit{Daily Sun data, 2026 (continued)}}\\\\\n\\toprule\n" <>
  "\\textbf{Date} & \\textbf{Mer.Pass} & \\textbf{EoT} & \\textbf{SD} & \\textbf{Date} & \\textbf{Mer.Pass} & \\textbf{EoT} & \\textbf{SD} \\\\\n\\midrule\n\\endhead\n" <>
  "\\midrule\\multicolumn{8}{r}{\\textit{continued}}\\\\\n\\endfoot\n\\bottomrule\n\\endlastfoot\n";
Do[
  With[{din = daysInMonth[[mo]]},
   sunDailyStr = sunDailyStr <> "\\multicolumn{8}{l}{\\textbf{" <> monthNames[[mo]] <> " 2026}}\\\\\n";
   Module[{rows, mid, i},
    rows = Table[
      With[{v = merEoTSD[mo, d]},
        StringTake[monthNames[[mo]], 3] <> " " <> ToString[d] <> " & " <>
        pad2[v[[1]]] <> ":" <> padMin1[v[[2]]] <> " & " <>
        fmtEoT[v[[3]]] <> " & " <> fmtMin[v[[4]]]],
      {d, 1, din}] // Quiet;
    mid = Ceiling[din/2];
    For[i = 1, i <= mid, i++,
      sunDailyStr = sunDailyStr <> rows[[i]] <> " & " <>
        If[i + mid <= din, rows[[i + mid]], " & & & "] <> " \\\\\n"];
   ]],
  {mo, 1, 12}];
sunDailyStr = sunDailyStr <> "\\end{longtable}}\n";
wf["sun_daily.tex", sunDailyStr];
Print["  daily Sun table written"];

(* ---------------------------------------------------------------------------
   Navigational stars: SHA, Dec at epoch 2026.5, magnitude.
   --------------------------------------------------------------------------- *)
Print["=== Generating navigational-star table (Part III) ==="];
tStar = DateObject[{2026, 7, 2, 12, 0, 0}, TimeZone -> 0];   (* ~2026.5 *)
starCat = cnLoadStars[];
starRows = Table[
   With[{gp = cnBodyGPFor[{"Star", nm}, tStar]},
     Module[{sha, dec},
      sha = Mod[-gp[[2]] - Mod[cnGMST[tStar], 360], 360];
      dec = gp[[1]];
      {nm, sha, dec, starCat[nm]["mag"]}]],
   {nm, Sort[Keys[starCat]]}];
starTblStr = "{\\footnotesize\n\\begin{longtable}{l r r r || l r r r}\n" <>
  "\\caption{The 58 navigational stars --- SHA, Declination (epoch 2026.5), and magnitude}\\label{stars}\\\\\n" <>
  "\\toprule\n\\textbf{Star} & \\textbf{SHA} & \\textbf{Dec} & \\textbf{Mag} & \\textbf{Star} & \\textbf{SHA} & \\textbf{Dec} & \\textbf{Mag}\\\\\n\\midrule\n\\endfirsthead\n" <>
  "\\toprule\n\\textbf{Star} & \\textbf{SHA} & \\textbf{Dec} & \\textbf{Mag} & \\textbf{Star} & \\textbf{SHA} & \\textbf{Dec} & \\textbf{Mag}\\\\\n\\midrule\n\\endhead\n\\bottomrule\n\\endlastfoot\n";
Module[{n, mid, i, fmtrow},
 fmtrow[r_] := r[[1]] <> " & " <> fmtGHA[r[[2]]] <> " & " <> fmtDec[r[[3]]] <> " & " <> ToString[NumberForm[N[r[[4]]], {3,1}]];
 n = Length[starRows]; mid = Ceiling[n/2];
 For[i = 1, i <= mid, i++,
   starTblStr = starTblStr <> fmtrow[starRows[[i]]] <> " & " <>
     If[i + mid <= n, fmtrow[starRows[[i + mid]]], " & & & "] <> " \\\\\n"]];
starTblStr = starTblStr <> "\\end{longtable}}\n";
wf["stars.tex", starTblStr];
Print["  ", Length[starRows], " stars written"];

(* ---------------------------------------------------------------------------
   Part IV correction tables.
   --------------------------------------------------------------------------- *)
Print["=== Generating correction tables (Part IV) ==="];
(* Dip vs height of eye (m) *)
dipHeights = Join[Range[1.0, 6.0, 0.5], Range[7.0, 12.0, 1.0], {15.0, 20.0, 25.0, 30.0}];
dipStr = "\\begin{tabular}{r r || r r}\n\\toprule\n" <>
  "\\textbf{Height (m)} & \\textbf{Dip ($\\pr$)} & \\textbf{Height (m)} & \\textbf{Dip ($\\pr$)}\\\\\n\\midrule\n";
Module[{n, mid, i, row},
 row[h_] := ToString[NumberForm[N[h], {4,1}]] <> " & $-$" <> fmtMin[cnDip[h]];
 n = Length[dipHeights]; mid = Ceiling[n/2];
 For[i = 1, i <= mid, i++,
  dipStr = dipStr <> row[dipHeights[[i]]] <> " & " <>
    If[i + mid <= n, row[dipHeights[[i + mid]]], " & "] <> " \\\\\n"]];
dipStr = dipStr <> "\\bottomrule\n\\end{tabular}\n";
wf["corr_dip.tex", dipStr];

(* Refraction vs apparent altitude (Bennett) *)
refAlts = Join[Range[5, 20, 1], {22, 25, 30, 35, 40, 50, 60, 70, 80, 90}];
refStr = "\\begin{tabular}{r r || r r}\n\\toprule\n" <>
  "\\textbf{App.Alt ($\\dg$)} & \\textbf{Refr ($\\pr$)} & \\textbf{App.Alt ($\\dg$)} & \\textbf{Refr ($\\pr$)}\\\\\n\\midrule\n";
Module[{n, mid, i, row},
 row[a_] := Module[{r = Max[N[cnRefraction[N[a]]], 0.]},
   ToString[a] <> " & " <> If[Round[r, 0.1] == 0, "", "$-$"] <> fmtMin[r]];
 n = Length[refAlts]; mid = Ceiling[n/2];
 For[i = 1, i <= mid, i++,
  refStr = refStr <> row[refAlts[[i]]] <> " & " <>
    If[i + mid <= n, row[refAlts[[i + mid]]], " & "] <> " \\\\\n"]];
refStr = refStr <> "\\bottomrule\n\\end{tabular}\n";
wf["corr_refr.tex", refStr];

(* Sun semidiameter by month (day 1 and day 15) *)
sdStr = "\\begin{tabular}{l r || l r}\n\\toprule\n" <>
  "\\textbf{Date} & \\textbf{SD ($\\pr$)} & \\textbf{Date} & \\textbf{SD ($\\pr$)}\\\\\n\\midrule\n";
Module[{dates, n, mid, i, row},
 dates = Flatten[Table[{mo, 1}, {mo, 1, 12}], 1] ~Join~ Flatten[Table[{mo, 15}, {mo, 1, 12}], 1];
 dates = Flatten[Table[{{mo, 1}, {mo, 15}}, {mo, 1, 12}], 1];
 row[md_] := StringTake[monthNames[[md[[1]]]], 3] <> " " <> ToString[md[[2]]] <> " & " <>
   fmtMin[cnSunSemidiameterAt[tAt[2026, md[[1]], md[[2]], 12]]];
 n = Length[dates]; mid = Ceiling[n/2];
 For[i = 1, i <= mid, i++,
  sdStr = sdStr <> row[dates[[i]]] <> " & " <>
    If[i + mid <= n, row[dates[[i + mid]]], " & "] <> " \\\\\n"]];
sdStr = sdStr <> "\\bottomrule\n\\end{tabular}\n";
wf["corr_sd.tex", sdStr];

(* GHA increments: minutes 0..59 for Sun (15 deg/hr) and Aries (15.04107 deg/hr) *)
sunRate = 15.0;            (* deg per hour *)
ariesRate = 15.0410686;    (* deg per hour, mean sidereal *)
incStr = "{\\footnotesize\n\\begin{tabular}{r r r || r r r}\n\\toprule\n" <>
  "\\textbf{m} & \\textbf{Sun} & \\textbf{Aries} & \\textbf{m} & \\textbf{Sun} & \\textbf{Aries}\\\\\n\\midrule\n";
Module[{i, row},
 row[m_] := ToString[m] <> " & " <> fmtAng[sunRate*m/60] <> " & " <> fmtAng[ariesRate*m/60];
 For[i = 0, i <= 29, i++,
  incStr = incStr <> row[i] <> " & " <> row[i + 30] <> " \\\\\n"]];
incStr = incStr <> "\\bottomrule\n\\end{tabular}}\n";
wf["corr_incr.tex", incStr];
Print["  correction tables written"];

(* ---------------------------------------------------------------------------
   WORKED EXAMPLES.  All numbers computed here so the prose macros are exact.
   --------------------------------------------------------------------------- *)
Print["=== Building worked examples ==="];

(* ---- Example 1: Sun intercept (Marcq St-Hilaire) ---- *)
t1 = tAt[2026, 6, 29, 15];
gha1 = cnSunGHA[t1]; dec1 = cnSunGP[t1][[1]]; lonGP1 = cnSunGP[t1][[2]];
aLat1 = 40;                                     (* assumed lat, N, whole degree *)
ghaDeg1 = Floor[gha1]; ghaMin1 = (gha1 - ghaDeg1)*60;
aLonW1 = 30 + ghaMin1/60;                        (* assumed lon West, minutes = GHA minutes *)
LHA1 = Mod[ghaDeg1 - 30, 360];                   (* whole degrees *)
{hc1, zn1} = cnComputedAltitude[{aLat1, -aLonW1}, {dec1, lonGP1}];
(* choose observed Ho ~ 5.0' toward: build a plausible sextant reading *)
sd1 = cnSunSemidiameterAt[t1]; dip1 = cnDip[3.0]; ie1 = 2.0; (* index error on the arc *)
targetHo = hc1 + 5.0/60;
haApprox = targetHo + cnRefraction[targetHo]/60 - sd1/60;
hsRaw = haApprox + ie1/60 + dip1/60;
hs1 = Round[hsRaw*600]/600;                      (* round Hs to 0.1' *)
ha1 = hs1 - ie1/60 - dip1/60;                    (* apparent altitude *)
ref1 = cnRefraction[ha1];
ho1 = ha1 - ref1/60 + sd1/60;                    (* observed altitude, lower limb *)
intercept1 = (ho1 - hc1)*60;                     (* nm; +ve = toward *)
dir1 = If[intercept1 >= 0, "Toward", "Away"];
(* cross-check hand formula for Hc *)
hcHand1 = ArcSin[Sin[aLat1 Degree] Sin[dec1 Degree] +
    Cos[aLat1 Degree] Cos[dec1 Degree] Cos[LHA1 Degree]]/Degree;

ex1Work = "\\begin{tabular}{l r}\n\\toprule\n\\multicolumn{2}{l}{\\textbf{Sight and corrections}}\\\\\n\\midrule\n" <>
  "Body & Sun (lower limb)\\\\\n" <>
  "UT & 2026 June 29, 15:00:00\\\\\n" <>
  "DR position & 40\\dg N, 30\\dg W (approx.)\\\\\n" <>
  "Sextant altitude $H_s$ & " <> fmtGHA[hs1] <> "\\\\\n" <>
  "Index error (on the arc) & $-$" <> fmtMin[ie1] <> "\\pr\\\\\n" <>
  "Dip (height of eye 3.0 m) & $-$" <> fmtMin[dip1] <> "\\pr\\\\\n" <>
  "Apparent altitude $H_a$ & " <> fmtGHA[ha1] <> "\\\\\n" <>
  "Refraction & $-$" <> fmtMin[ref1] <> "\\pr\\\\\n" <>
  "Semidiameter (lower limb) & $+$" <> fmtMin[sd1] <> "\\pr\\\\\n" <>
  "\\textbf{Observed altitude} $H_o$ & \\textbf{" <> fmtGHA[ho1] <> "}\\\\\n" <>
  "\\midrule\n\\multicolumn{2}{l}{\\textbf{From the almanac (15:00 UT, 29 June)}}\\\\\n\\midrule\n" <>
  "GHA Sun & " <> fmtGHA[gha1] <> "\\\\\n" <>
  "Declination & " <> fmtDec[dec1] <> "\\\\\n" <>
  "Assumed longitude (W) & " <> fmtGHA[aLonW1] <> "\\\\\n" <>
  "Local hour angle LHA & " <> fmtGHA[LHA1] <> "\\\\\n" <>
  "Assumed latitude & " <> fmtDec[aLat1] <> "\\\\\n" <>
  "\\midrule\n\\multicolumn{2}{l}{\\textbf{Sight reduction}}\\\\\n\\midrule\n" <>
  "Computed altitude $H_c$ & " <> fmtGHA[hc1] <> "\\\\\n" <>
  "Azimuth $Z_n$ & " <> fmtGHA[zn1] <> "\\\\\n" <>
  "Intercept $a=60(H_o-H_c)$ & " <> fmtMin[Abs[intercept1]] <> " nm " <> dir1 <> "\\\\\n" <>
  "\\bottomrule\n\\end{tabular}\n";
wf["example1_work.tex", ex1Work];

(* ---- Example 2: Latitude by the noon Sun ---- *)
(* Local apparent noon at 30 W: Sun's GHA = 30 (LHA=0). Find UT. *)
lanRoot = h /. FindRoot[Mod[cnSunGHA[tf[2026,6,29,h]] - 30 + 180, 360] - 180 == 0, {h, 14}];
t2 = tf[2026, 6, 29, lanRoot];
dec2 = cnSunGP[t2][[1]];
trueLat2 = 40.1;                                  (* observer true latitude for the demo *)
hoTrue2 = cnAltitudeFromGP[{trueLat2, -30.0}, cnSunGP[t2]];  (* true meridian altitude *)
ho2 = Round[hoTrue2*600]/600;                     (* to 0.1' *)
zd2 = 90 - ho2;                                    (* zenith distance *)
(* Sun bears South (dec<lat): Lat = Dec + ZD *)
latCalc2 = dec2 + zd2;
lanH = Floor[lanRoot]; lanM = (lanRoot - lanH)*60;
ex2Work = "\\begin{tabular}{l r}\n\\toprule\n" <>
  "Observer (near) & 40\\dg N, 30\\dg W\\\\\n" <>
  "Date & 2026 June 29\\\\\n" <>
  "Local apparent noon (UT) & " <> pad2[lanH] <> ":" <> padMin1[lanM] <> "\\\\\n" <>
  "\\midrule\n" <>
  "Observed mer. altitude $H_o$ & " <> fmtGHA[ho2] <> "\\\\\n" <>
  "Zenith distance $z=90\\dg-H_o$ & " <> fmtGHA[zd2] <> "\\\\\n" <>
  "Sun's declination (from almanac) & " <> fmtDec[dec2] <> "\\\\\n" <>
  "Sun bears & South\\\\\n" <>
  "\\textbf{Latitude} $=$ Dec $+ z$ & \\textbf{" <> fmtDec[latCalc2] <> "}\\\\\n" <>
  "\\bottomrule\n\\end{tabular}\n";
wf["example2_work.tex", ex2Work];

(* ---- Example 3: a star sight using GHA Aries + SHA (Sirius) ----
   The reader looks Sirius up in the epoch-2026.5 star table, so the example
   USES exactly the tabulated SHA and Dec (computed at tStar, as in stars.tex),
   not the instantaneous values at the sight time.  Everything downstream
   (GHA, LHA, Hc, Zn) is then re-derived from those tabulated figures. *)
t3 = tAt[2026, 1, 15, 22];
ghaAries3 = Mod[cnGMST[t3], 360];
gpSirTab = cnBodyGPFor[{"Star", "Sirius"}, tStar];   (* same epoch as the star table *)
shaSir = Mod[-gpSirTab[[2]] - Mod[cnGMST[tStar], 360], 360];  (* = table SHA 258 25.3' *)
decSir = gpSirTab[[1]];                               (* = table Dec S16 45.3' *)
ghaSir = Mod[ghaAries3 + shaSir, 360];               (* GHA of the star at the sight time *)
aLat3 = 30;                                          (* assumed lat N *)
ghaSirDeg = Floor[ghaSir]; ghaSirMin = (ghaSir - ghaSirDeg)*60;
aLonW3 = 40 + ghaSirMin/60;                          (* choose so LHA whole *)
LHA3 = Mod[ghaSirDeg - 40, 360];
lonGPex3 = Mod[-ghaSir + 180, 360] - 180;            (* GP longitude for this GHA *)
{hc3, zn3} = cnComputedAltitude[{aLat3, -aLonW3}, {decSir, lonGPex3}];
(* honesty check: instantaneous SHA at the sight time vs the tabulated value used *)
gpSirNow = cnBodyGPFor[{"Star", "Sirius"}, t3];
shaSirNow = Mod[-gpSirNow[[2]] - ghaAries3, 360];
ex3Work = "\\begin{tabular}{l r}\n\\toprule\n" <>
  "Star & Sirius\\\\\n" <>
  "UT & 2026 January 15, 22:00:00\\\\\n" <>
  "Assumed position & 30\\dg N, " <> fmtGHA[aLonW3] <> " W\\\\\n\\midrule\n" <>
  "GHA Aries (Part III) & " <> fmtGHA[ghaAries3] <> "\\\\\n" <>
  "SHA Sirius (star table) & " <> fmtGHA[shaSir] <> "\\\\\n" <>
  "GHA Sirius $=$ GHA$\\gamma+$SHA & " <> fmtGHA[ghaSir] <> "\\\\\n" <>
  "Declination Sirius & " <> fmtDec[decSir] <> "\\\\\n" <>
  "LHA $=$ GHA $-$ Long(W) & " <> fmtGHA[LHA3] <> "\\\\\n\\midrule\n" <>
  "Computed altitude $H_c$ & " <> fmtGHA[hc3] <> "\\\\\n" <>
  "Azimuth $Z_n$ & " <> fmtGHA[zn3] <> "\\\\\n" <>
  "\\bottomrule\n\\end{tabular}\n";
wf["example3_work.tex", ex3Work];

(* ---- Macros for inline prose ---- *)
macros = StringJoin[
  "\\newcommand{\\exOneInt}{" <> fmtMin[Abs[intercept1]] <> "}\n",
  "\\newcommand{\\exOneDir}{" <> dir1 <> "}\n",
  "\\newcommand{\\exOneZn}{" <> fmtGHA[zn1] <> "}\n",
  "\\newcommand{\\exOneHo}{" <> fmtGHA[ho1] <> "}\n",
  "\\newcommand{\\exOneHc}{" <> fmtGHA[hc1] <> "}\n",
  "\\newcommand{\\exOneLHA}{" <> fmtGHA[LHA1] <> "}\n",
  "\\newcommand{\\exTwoLat}{" <> fmtDec[latCalc2] <> "}\n",
  "\\newcommand{\\exThreeHc}{" <> fmtGHA[hc3] <> "}\n",
  "\\newcommand{\\exThreeZn}{" <> fmtGHA[zn3] <> "}\n",
  "\\newcommand{\\genDate}{" <> DateString[Now, {"Year","-","Month","-","Day"}] <> "}\n"];
wf["macros.tex", macros];

(* ---------------------------------------------------------------------------
   VERIFICATION (printed to stdout, independent sanity checks)
   --------------------------------------------------------------------------- *)
Print["\n================ VERIFICATION ================"];
decJun = cnSunGP[tAt[2026,6,21,12]][[1]];
decDec = cnSunGP[tAt[2026,12,21,12]][[1]];
Print["Sun Dec near June solstice (Jun 21 12h): ", decJun, "  (expect ~ +23.44)"];
Print["Sun Dec near Dec  solstice (Dec 21 12h): ", decDec, "  (expect ~ -23.44)"];
(* find actual max/min dec over year *)
maxdec = Max[Table[cnSunGP[tAt[2026,6,21,h]][[1]], {h,0,23}]];
mindec = Min[Table[cnSunGP[tAt[2026,12,21,h]][[1]], {h,0,23}]];
Print["Max Dec on Jun 21 (hourly): ", maxdec, "   Min Dec on Dec 21: ", mindec];
g0 = cnSunGHA[tAt[2026,3,20,0]]; g1 = cnSunGHA[tAt[2026,3,20,1]];
Print["GHA rate (Sun, 1h): ", Mod[g1 - g0, 360], " deg/hr  (expect ~15)"];
ga0 = Mod[cnGMST[tAt[2026,3,20,0]],360]; ga1 = Mod[cnGMST[tAt[2026,3,20,1]],360];
Print["GHA rate (Aries, 1h): ", Mod[ga1 - ga0, 360], " deg/hr  (expect ~15.041)"];
(* star SHA stability across the year *)
shaJan = Module[{t = tAt[2026,1,1,0]}, Mod[-cnBodyGPFor[{"Star","Vega"},t][[2]] - Mod[cnGMST[t],360],360]];
shaDec = Module[{t = tAt[2026,12,31,0]}, Mod[-cnBodyGPFor[{"Star","Vega"},t][[2]] - Mod[cnGMST[t],360],360]];
Print["Vega SHA Jan 1: ", shaJan, "  Dec 31: ", shaDec, "  drift(arcmin): ", (shaDec-shaJan)*60];
Print["Vega Dec 2026.5: ", cnBodyGPFor[{"Star","Vega"},tStar][[1]], " (Hipparcos ~ +38.78)"];
(* spot Sun GHA/Dec vs a known reference: 2026-01-01 12:00 UT *)
Print["Sun GHA/Dec 2026-01-01 12:00 UT: ", {cnSunGHA[tAt[2026,1,1,12]], cnSunGP[tAt[2026,1,1,12]][[1]]}];
Print["--- Example 1 checks ---"];
Print["Hc engine=", hc1, "  Hc hand=", hcHand1, "  diff(arcmin)=", (hc1-hcHand1)*60];
Print["Ho=", ho1, " Hc=", hc1, " intercept=", intercept1, " nm ", dir1, "  Zn=", zn1];
Print["--- Example 2 checks ---"];
Print["LAN UT=", lanRoot, "  Dec=", dec2, "  Ho=", ho2, "  Lat calc=", latCalc2, " (true 40.1)"];
Print["--- Example 3 checks ---"];
Print["SHA Sirius (table 2026.5)=", shaSir, "  (instant Jan15)=", shaSirNow, "  diff(arcmin)=", (shaSir-shaSirNow)*60];
Print["GHA Aries+SHA=", ghaSir, "  LHA=", LHA3, "  aLonW=", aLonW3];
Print["Hc=", hc3, "  Zn=", zn3, "  Dec=", decSir];
Print["================ DONE ================"];
