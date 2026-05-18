(* ::Package:: *)

(*  enso_emergence_helpers.wl
    ===================================================================
    Companion package for the Wolfram Community notebook
    "ENSO emergence and the spring predictability barrier".

    Upload this file as an attachment alongside the .nb.  The notebook's
    Setup cell does:

        SetDirectory[NotebookDirectory[]];
        Get["enso_emergence_helpers.wl"];

    after which every wlIn cell in the post is runnable.

    Contents:
      - Recharge–discharge oscillator (Jin 1997)
            simulate, forecastEnsemble, drawClim*
      - Delayed-action oscillator (Suarez–Schopf 1988)
            simulateDDE, forecastEnsembleDDE, drawClimDDE
      - Hero-animation renderer
            renderSatFrame, loadERSSTFrame
*)

BeginPackage["ENSOHelpers`"];

(* ============== Public usage messages ============================== *)

simulate::usage = "simulate[T0, h0, m0, tEnd, dt, seed] integrates the \
recharge-discharge oscillator forward by tEnd months starting from \
(T0, h0) in calendar month m0, using Euler-Maruyama with step dt and \
the given random seed. Returns an Association with keys t, T, h.";

simulateDDE::usage = "simulateDDE[m0, tEnd, dt, hist0, seed] integrates \
the Suarez-Schopf delayed-action oscillator. Time is in 2-month units \
so delayTau = 3 corresponds to a 6-month wave round-trip.";

forecastEnsemble::usage = "forecastEnsemble[m0, nMembers, leadMonths, dt] \
runs an nMembers-member ensemble of the RDO from calendar month m0, \
returning percentile bands for each lead.";

forecastEnsembleDDE::usage = "forecastEnsembleDDE[m0, nMembers, leadMonths, dt] \
runs an nMembers-member DDE ensemble from calendar month m0.";

renderSatFrame::usage = "renderSatFrame[frame, latSub, lonSub] returns \
a GeoGraphics frame with SST anomaly overlaid on satellite imagery.";

loadERSSTFrame::usage = "loadERSSTFrame[file, latAxis, lonAxis, latIdx, \
lonIdx] reads one monthly ERSSTv5 NetCDF and returns the Pacific window.";

Rseason::usage = "Rseason[t, m0] -- seasonal Bjerknes growth rate.";
sigWWB::usage  = "sigWWB[t, m0] -- seasonal westerly-wind-burst std-dev.";
divergingCmap::usage = "divergingCmap[v] -- red/blue diverging colour function.";
lonTo180::usage = "lonTo180[L] -- convert 0..360 longitude to -180..180.";
monthName::usage = "monthName[m] -- 3-letter month name for m in 1..12.";

{R0, R1, phiR, gammaC, rDamp, alpha, sigmaT, sigmaH, sigW0, aW, phiW,
 alphaDel, delayTau, sigBase, sigSpring, unit2Mon};


Begin["`Private`"];

(* ============== Recharge–discharge oscillator ====================== *)

(* Parameters (months^-1 units, T in degC, h scaled). *)
R0      = -0.03;     R1     = 0.18;    phiR  = 9.0;
gammaC  = 0.06;      rDamp  = 0.04;    alpha = 0.15;
sigmaT  = 0.06;      sigmaH = 0.04;
sigW0   = 0.22;      aW     = 0.80;    phiW  = 3.0;

Rseason[t_, m0_] := R0 + R1 Cos[2 Pi (t + m0 - phiR)/12];
sigWWB[t_, m0_]  := sigW0 (1 + aW Cos[2 Pi (t + m0 - phiW)/12]);

simulate[T0_, h0_, m0_, tEnd_, dt_, seed_] := Module[
   {steps, T, h, t, sqrtDt, ts, Ts, hs, eT, eH, eW, i},
   SeedRandom[seed];
   steps  = Round[tEnd/dt];
   sqrtDt = Sqrt[dt];
   T = T0; h = h0; t = 0.;
   ts = ConstantArray[0., steps + 1];
   Ts = ConstantArray[0., steps + 1];
   hs = ConstantArray[0., steps + 1];
   ts[[1]] = 0.; Ts[[1]] = T; hs[[1]] = h;
   Do[
     eT = RandomVariate[NormalDistribution[]];
     eH = RandomVariate[NormalDistribution[]];
     eW = RandomVariate[NormalDistribution[]];
     T += dt (Rseason[t, m0] T + gammaC h) +
           sqrtDt (sigmaT eT + sigWWB[t, m0] eW);
     h += dt (-rDamp h - alpha T) + sqrtDt sigmaH eH;
     t += dt;
     ts[[i + 1]] = t; Ts[[i + 1]] = T; hs[[i + 1]] = h,
     {i, steps}];
   <|"t" -> ts, "T" -> Ts, "h" -> hs|>
];

(* Build a per-month climatology of (T, h) by running one 600-yr trajectory
   and bucketing samples by calendar month. *)
$climByMonth = None;

buildClimatology[] := Module[{run, monthFracs, monthIdx},
   SeedRandom[1];
   run = simulate[0., 0., 1, 12*600, 0.5, 1];
   monthFracs = Mod[run["t"], 12.];
   monthIdx   = Floor[monthFracs] + 1;
   $climByMonth = AssociationThread[Range[12] -> Table[{}, {12}]];
   Do[
      With[{m = monthIdx[[k]]},
         AppendTo[$climByMonth[m],
            {run["T"][[k]], run["h"][[k]]}]],
      {k, Length @ run["t"]}];
   (* drop the burn-in *)
   $climByMonth = (Drop[#, Min[200, Floor[Length[#]/4]]] &) /@ $climByMonth;
   $climByMonth
];

drawClim[m0_Integer, k_Integer] := Module[{pool},
   If[$climByMonth === None, buildClimatology[]];
   pool = $climByMonth[m0];
   pool[[1 + Mod[k, Length[pool]]]]
];

forecastEnsemble[m0_Integer, nMembers_:500, leadMonths_:18, dt_:0.05] :=
   Module[{trajs, Tmat},
      trajs = Table[
         With[{ic = drawClim[m0, k]},
            simulate[ic[[1]], ic[[2]], m0, leadMonths, dt, m0*100000 + k]],
         {k, nMembers}];
      Tmat = Transpose[#["T"] & /@ trajs];
      <|
        "t"      -> trajs[[1, "t"]],
        "median" -> Median /@ Tmat,
        "p05"    -> Map[Quantile[#, 0.05] &, Tmat],
        "p25"    -> Map[Quantile[#, 0.25] &, Tmat],
        "p75"    -> Map[Quantile[#, 0.75] &, Tmat],
        "p95"    -> Map[Quantile[#, 0.95] &, Tmat],
        "std"    -> StandardDeviation /@ Tmat
      |>
   ];


(* ============== Delayed-action oscillator (Suarez & Schopf) ======== *)

alphaDel  = 0.75;   delayTau = 3.0;
sigBase   = 0.03;   sigSpring = 0.10;
unit2Mon  = 2.0;

sigStoch[t_, m0_] := sigBase + sigSpring (
   1 + Cos[2 Pi (unit2Mon t + m0 - 3)/12]);

simulateDDE[m0_, tEnd_, dt_, hist0_, seed_] := Module[
   {nHist, hist, idx, T, t, sqrtDt, ts, Ts, eta, Tdel, drift, diff,
    nSteps, i},
   SeedRandom[seed];
   nHist  = Ceiling[delayTau/dt] + 2;
   hist   = ConstantArray[hist0, nHist];
   idx    = 1;
   T      = hist0;
   t      = 0.;
   nSteps = Round[tEnd/dt];
   sqrtDt = Sqrt[dt];
   ts = ConstantArray[0., nSteps + 1];
   Ts = ConstantArray[0., nSteps + 1];
   ts[[1]] = 0.; Ts[[1]] = T;
   Do[
     With[{kBack = Round[delayTau/dt]},
        Tdel = hist[[1 + Mod[idx - 1 - kBack + nHist, nHist]]]];
     drift = T - alphaDel Tdel - T^3;
     diff  = sigStoch[t, m0];
     eta   = RandomVariate[NormalDistribution[]];
     T += dt drift + sqrtDt diff eta;
     t += dt;
     idx = 1 + Mod[idx, nHist];
     hist[[idx]] = T;
     ts[[i + 1]] = t; Ts[[i + 1]] = T,
     {i, nSteps}];
   <|"t" -> ts, "T" -> Ts|>
];

(* DDE climatology pool, same idea as drawClim. *)
$climByMonthDDE = None;

buildClimatologyDDE[] := Module[{run, monthIdx},
   SeedRandom[1];
   run = simulateDDE[1, 12*400/unit2Mon, 0.2, 0., 1];
   monthIdx = Floor[Mod[run["t"]*unit2Mon, 12.]] + 1;
   $climByMonthDDE = AssociationThread[Range[12] -> Table[{}, {12}]];
   Do[
      AppendTo[$climByMonthDDE[monthIdx[[k]]], run["T"][[k]]],
      {k, Length @ run["t"]}];
   $climByMonthDDE = (Drop[#, Min[100, Floor[Length[#]/4]]] &) /@ $climByMonthDDE;
   $climByMonthDDE
];

drawClimDDE[m0_Integer, k_Integer] := Module[{pool},
   If[$climByMonthDDE === None, buildClimatologyDDE[]];
   pool = $climByMonthDDE[m0];
   pool[[1 + Mod[k, Length[pool]]]]
];

forecastEnsembleDDE[m0_Integer, nMembers_:400, leadMonths_:18,
   dt_:0.05] := Module[{trajs, Tmat},
   trajs = Table[
      simulateDDE[m0, leadMonths/unit2Mon, dt, drawClimDDE[m0, k],
         m0*97 + k],
      {k, nMembers}];
   Tmat = Transpose[#["T"] & /@ trajs];
   <|
     "t"      -> trajs[[1, "t"]] * unit2Mon,
     "median" -> Median /@ Tmat,
     "p05"    -> Map[Quantile[#, 0.05] &, Tmat],
     "p25"    -> Map[Quantile[#, 0.25] &, Tmat],
     "p75"    -> Map[Quantile[#, 0.75] &, Tmat],
     "p95"    -> Map[Quantile[#, 0.95] &, Tmat],
     "std"    -> StandardDeviation /@ Tmat
   |>
];


(* ============== Hero satellite-overlay renderer ==================== *)

divergingCmap = Blend[{
   {-3.0, RGBColor[0.05, 0.10, 0.45]},
   {-1.5, RGBColor[0.15, 0.40, 0.75]},
   {-0.5, RGBColor[0.55, 0.75, 0.92]},
   { 0.0, RGBColor[0.97, 0.97, 0.95]},
   { 0.5, RGBColor[0.95, 0.65, 0.45]},
   { 1.5, RGBColor[0.85, 0.20, 0.15]},
   { 3.0, RGBColor[0.45, 0.05, 0.10]}}, Clip[#, {-3., 3.}]] &;

lonTo180[L_] := If[L > 180., L - 360., L];
monthName[m_Integer] := {"Jan","Feb","Mar","Apr","May","Jun",
   "Jul","Aug","Sep","Oct","Nov","Dec"}[[m]];

loadERSSTFrame[file_String, latAxis_, lonAxis_, latIdx_, lonIdx_] :=
   Module[{ssta, sub, mean, n34LatIdx, n34LonIdx, base, ym},
      ssta = Normal @ Import[file, {"NetCDF", "Data", "ssta"}];
      sub = ssta[[1, 1, latIdx, lonIdx]];
      n34LatIdx = Flatten @ Position[latAxis[[latIdx]], _?(-5 <= # <= 5 &)];
      n34LonIdx = Flatten @ Position[lonAxis[[lonIdx]], _?(190 <= # <= 240 &)];
      mean = Mean @ Select[Flatten[sub[[n34LatIdx, n34LonIdx]]],
                           # > -10 &];
      base = FileBaseName[file];
      ym = StringTake[base, -6];
      <|"data"  -> sub,
        "month" -> {ToExpression@StringTake[ym, 4],
                    ToExpression@StringTake[ym, -2]},
        "n34"   -> mean|>
   ];

renderSatFrame[f_, latSub_, lonSub_] := Module[
   {dat, ym, n34, label, n34Label, polygons, fig,
    nLat = Length[latSub], nLon = Length[lonSub],
    fw = 1100, fh = 580},
   dat = Normal @ f["data"];
   ym = f["month"];
   n34 = f["n34"];
   label    = monthName[ym[[2]]] <> " " <> ToString[ym[[1]]];
   n34Label = ToString[NumberForm[n34, {3, 2}]] <> " \[Degree]C";
   polygons = Flatten @ Table[
      With[{v = dat[[i, j]]},
         If[v < -10., Nothing,
            {Directive[divergingCmap[v], Opacity[0.65], EdgeForm[None]],
             GeoPolygon[{
                {latSub[[i]] - 1, lonTo180[lonSub[[j]] - 1]},
                {latSub[[i]] - 1, lonTo180[lonSub[[j]] + 1]},
                {latSub[[i]] + 1, lonTo180[lonSub[[j]] + 1]},
                {latSub[[i]] + 1, lonTo180[lonSub[[j]] - 1]}}]}]],
      {i, nLat}, {j, nLon}];
   fig = GeoGraphics[
      {polygons,
       Directive[Yellow, AbsoluteThickness[2.5]],
       GeoPath[{{5, -170}, {5, -120}, {-5, -120}, {-5, -170}, {5, -170}}],
       Inset[Style["Ni\[NTilde]o 3.4", RGBColor[1, 1, 0.6], Bold, 13],
          GeoPosition[{8.5, -145}]]},
      GeoRange      -> {{-40, 40}, {110, -50}},
      GeoProjection -> "Equirectangular",
      GeoBackground -> "Satellite",
      GeoGridLines  -> {Range[-30, 30, 15], Range[-180, 180, 30]},
      GeoGridLinesStyle -> Directive[GrayLevel[0.7, 0.4],
         Thickness[0.0008]],
      ImageSize -> {fw, fh},
      Frame -> True, FrameStyle -> Black,
      PlotLabel -> Style[
         "Equatorial Pacific SST anomaly (ERSSTv5) over satellite imagery",
         Bold, FontFamily -> "Helvetica", FontSize -> 14]];
   Show[fig, Epilog -> {
      Inset[
         Framed[
            Column[{
               Style[label, FontSize -> 22, FontWeight -> Bold,
                  FontColor -> White],
               Style["Ni\[NTilde]o 3.4 anomaly", FontSize -> 10,
                  FontColor -> GrayLevel[0.85]],
               Style[n34Label, FontSize -> 20, FontWeight -> Bold,
                  FontColor -> If[n34 > 0.5, RGBColor[1, 0.5, 0.35],
                     If[n34 < -0.5, RGBColor[0.5, 0.7, 1], White]]]
            }, Alignment -> Center, Spacings -> 0.4],
            Background    -> RGBColor[0, 0, 0, 0.7],
            FrameStyle    -> Directive[GrayLevel[0.85], Thickness[0.001]],
            FrameMargins  -> 8, RoundingRadius -> 4],
         Scaled[{0.045, 0.94}], {Left, Top}]
      }]
];


End[];   (* `Private` *)
EndPackage[];

Print["ENSOHelpers package loaded: simulate, simulateDDE, ",
   "forecastEnsemble, forecastEnsembleDDE, renderSatFrame, ",
   "loadERSSTFrame and parameter symbols."];
