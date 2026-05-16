#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Recharge-discharge oscillator (Jin 1997, F.-F. Jin, J.Atmos.Sci 54)
    with seasonal modulation of the Bjerknes feedback and of the
    stochastic WWB amplitude (Levine & Jin 2010, Stein et al. 2010
    style). Integrated with Euler-Maruyama.

    State (departures from climatology):
      T(t)  eastern Pacific SST anomaly (\[Degree]C)
      h(t)  western Pacific thermocline depth anomaly (rescaled)

    System:

        dT/dt = R(t) T  +  \[Gamma]  h  +  \[Sigma]_T \[Xi]_T(t)  +  \[Sigma]_w(t) \[Xi]_w(t)
        dh/dt =  -r h  -  \[Alpha]  T   +  \[Sigma]_h \[Xi]_h(t)

    with seasonal cycles:
        R(t)   = R_0 + R_1 cos(2\[Pi](t - 9)/12)        (max in September)
        \[Sigma]_w(t) = \[Sigma]_w0 (1 + a_w cos(2\[Pi](t - 3)/12)) (max in March/April)

    Outputs to docs/images/:
      rdo_timeseries.png       1 sample T(t), 20 yr
      rdo_phase.png            (T, h) phase orbit
      rdo_seasonality.png      R(t) and \[Sigma]_w(t) annual cycles
      rdo_barrier_cone.png     4 ensemble cone panels: init Jan/Apr/Jul/Oct
      rdo_spread_heatmap.png   ensemble StDev as f(init month, lead)
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];

(* ----- parameters (month^-1 units, T in degC, h in scaled m) ----- *)

R0     = -0.03;          (* mean Bjerknes growth rate (sub-critical) *)
R1     =  0.18;          (* seasonal amplitude — max in Sep         *)
phiR   =  9.0;
gamma  =  0.06;
rDamp  =  0.04;
alpha  =  0.15;
sigmaT =  0.06;          (* slow red-noise stoch. forcing on T      *)
sigmaH =  0.04;
sigW0  =  0.22;          (* mean WWB stoch. forcing amplitude       *)
aW     =  0.80;          (* seasonal amplitude of WWB (max ~ March) *)
phiW   =  3.0;

Rseason[t_, m0_]  := R0 + R1 Cos[2 Pi (t + m0 - phiR)/12];
sigWWB[t_, m0_]   := sigW0 (1 + aW Cos[2 Pi (t + m0 - phiW)/12]);

(* ----- Euler-Maruyama integrator -------------------------------- *)

simulate[T0_, h0_, m0_, tEnd_, dt_, seed_] := Module[
   {steps, T, h, t, sqrtDt, ts, Ts, hs, eT, eH, eW},
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
     T += dt (Rseason[t, m0] T + gamma h) +
           sqrtDt (sigmaT eT + sigWWB[t, m0] eW);
     h += dt (-rDamp h - alpha T) + sqrtDt sigmaH eH;
     t += dt;
     ts[[i + 1]] = t; Ts[[i + 1]] = T; hs[[i + 1]] = h;
     , {i, steps}
   ];
   <|"t" -> ts, "T" -> Ts, "h" -> hs|>
];

(* ----- 1. Sanity plots: seasonality, sample trajectory, phase --- *)

seasonPlot = Plot[
   {Rseason[t - 1, 1], sigWWB[t - 1, 1]},
   {t, 1, 13}, PlotRange -> All,
   Frame -> True, Axes -> False,
   FrameLabel -> {{"value (month\!\(\*SuperscriptBox[\(\), \(-1\)]\) or \[Degree]C)",
       None}, {"calendar month", None}},
   PlotLegends -> {"R(t)  Bjerknes feedback", "\[Sigma]\_w(t)  WWB std-dev"},
   PlotStyle -> {RGBColor[0.05, 0.20, 0.55], RGBColor[0.85, 0.32, 0.10]},
   FrameTicks -> {{Automatic, None},
      {Table[{m, {"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}[[m]]},
         {m, 1, 12}], None}},
   PlotLabel -> "Seasonal modulation in the recharge-discharge model",
   GridLines -> {None, {0}},
   ImageSize -> 720, AspectRatio -> 0.55
];
Export[FileNameJoin[{imgDir, "rdo_seasonality.png"}], seasonPlot,
   ImageResolution -> 144];
Print["wrote rdo_seasonality.png"];

samp = simulate[0.3, 0.6, 1, 12 20, 0.05, 17];

Export[FileNameJoin[{imgDir, "rdo_timeseries.png"}],
   ListLinePlot[
     Transpose[{samp["t"]/12., samp["T"]}],
     Frame -> True,
     FrameLabel -> {{"T anomaly (\[Degree]C)", None}, {"years", None}},
     PlotLabel -> "Recharge-discharge oscillator: one realisation",
     PlotStyle -> Directive[Thickness[0.003], RGBColor[0.83, 0.32, 0.10]],
     PlotRange -> {Automatic, {-3.5, 3.5}},
     GridLines -> {Automatic, {-0.5, 0, 0.5}},
     ImageSize -> 950, AspectRatio -> 1/3.2
   ], ImageResolution -> 144];
Print["wrote rdo_timeseries.png"];

Export[FileNameJoin[{imgDir, "rdo_phase.png"}],
   ListLinePlot[
     Transpose[{samp["T"], samp["h"]}],
     Frame -> True,
     FrameLabel -> {{"thermocline anomaly h", None},
                    {"SST anomaly T (\[Degree]C)", None}},
     PlotLabel -> "Phase-space portrait",
     PlotStyle -> Directive[Thickness[0.0012],
                            Opacity[0.55], RGBColor[0.20, 0.40, 0.65]],
     GridLines -> {{0}, {0}},
     ImageSize -> 480, AspectRatio -> 1
   ], ImageResolution -> 144];
Print["wrote rdo_phase.png"];

(* ----- 2. Estimate the model climatology ------------------------ *)

(* Generate one long run, sample (T, h) by calendar month so we can
   draw a realistic initial condition for the forecast ensemble. *)
SeedRandom[1];
clim = simulate[0., 0., 1, 12*600, 0.5, 1];   (* 600-year run, dt=0.5 *)
climMonthFracs = Mod[clim["t"], 12.];
climMonthIdx   = Floor[climMonthFracs] + 1;     (* 1..12 *)
climByMonth = AssociationThread[
   Range[12] -> Table[{}, {12}]
];
Do[
   With[{m = climMonthIdx[[k]]},
     AppendTo[climByMonth[m], {clim["T"][[k]], clim["h"][[k]]}]
   ],
   {k, Length @ clim["t"]}
];
(* Trim the burn-in *)
climByMonth = (Drop[#, Min[200, Floor[Length[#]/4]]] &) /@ climByMonth;

drawClim[m0_, k_] := Module[{pool = climByMonth[m0]},
   pool[[1 + Mod[k, Length[pool]]]]
];

(* ----- 3. Ensemble forecast cones for 4 initial months ---------- *)

nMembers = 500;
leadMonths = 18;
dtFcst = 0.05;

forecastEnsemble[m0_] := Module[{trajs, T0h0},
   trajs = Table[
     T0h0 = drawClim[m0, k];
     simulate[T0h0[[1]], T0h0[[2]], m0, leadMonths, dtFcst,
        m0*100000 + k],
     {k, nMembers}
   ];
   With[{Tmat = Transpose[#["T"] & /@ trajs]},
     <|
       "t" -> trajs[[1, "t"]],
       "T" -> Tmat,
       "median" -> Median /@ Tmat,
       "p05"  -> Map[Quantile[#, 0.05] &, Tmat],
       "p25"  -> Map[Quantile[#, 0.25] &, Tmat],
       "p75"  -> Map[Quantile[#, 0.75] &, Tmat],
       "p95"  -> Map[Quantile[#, 0.95] &, Tmat],
       "std"  -> StandardDeviation /@ Tmat
     |>
   ]
];

monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

SeedRandom[42];
panelMonths = {1, 4, 7, 10};
fcs = AssociationThread[panelMonths -> (forecastEnsemble /@ panelMonths)];

band[t_, lo_, hi_, color_, opacity_] := ListLinePlot[
   {Transpose[{t, lo}], Transpose[{t, hi}]},
   PlotStyle -> Directive[Thickness[0.0005], color, Opacity[0.5]],
   Filling -> {1 -> {{2}, Directive[Opacity[opacity], color]}},
   PlotRange -> All
];

conePanel[m0_] := With[
   {f = fcs[m0]},
   With[
     {t = f["t"], p05 = f["p05"], p25 = f["p25"], p50 = f["median"],
      p75 = f["p75"], p95 = f["p95"]},
     Show[
       band[t, p05, p95, RGBColor[0.85, 0.32, 0.10], 0.18],
       band[t, p25, p75, RGBColor[0.85, 0.32, 0.10], 0.40],
       ListLinePlot[Transpose[{t, p50}],
         PlotStyle -> Directive[Thickness[0.005], Black]],
       Frame -> True,
       FrameLabel -> {{"T anomaly (\[Degree]C)", None},
                      {"verification month", None}},
       FrameTicks -> {
          {Automatic, None},
          {Table[{tau,
             monthNames[[1 + Mod[m0 - 1 + tau, 12]]]}, {tau, 0, leadMonths, 3}],
            None}
       },
       PlotRange -> {{0, leadMonths}, {-3, 3}},
       PlotLabel -> Style[
         "init " <> monthNames[[m0]] <> "  (m\:2080 = " <> ToString[m0] <> ")",
         Bold],
       GridLines -> {None, {-0.5, 0, 0.5}},
       GridLinesStyle -> Directive[GrayLevel[0.7], Dashed],
       AspectRatio -> 0.7, ImageSize -> 520
     ]
   ]
];

conePanelGrid = Grid[
   Partition[conePanel /@ panelMonths, 2],
   Spacings -> {1.5, 1.5}
];

Export[FileNameJoin[{imgDir, "rdo_barrier_cone.png"}], conePanelGrid,
   ImageResolution -> 144];
Print["wrote rdo_barrier_cone.png"];

(* ----- 4. StDev as function of (initial month, lead) ------------ *)

SeedRandom[2026];
nMemH = 250;
leadMaxH = 15;
dtH = 0.1;

allFcs = Table[
   Module[{trajs, Tmat},
     trajs = Table[
       With[{ic = drawClim[m0, k]},
          simulate[ic[[1]], ic[[2]], m0, leadMaxH, dtH, m0*99991 + k]
       ], {k, nMemH}];
     Tmat = Transpose[#["T"] & /@ trajs];
     <|"t" -> trajs[[1, "t"]], "std" -> StandardDeviation /@ Tmat|>
   ], {m0, 12}
];

times = allFcs[[1, "t"]];
leadIdx = Flatten @ Table[
   FirstPosition[times, _?(# >= tau - 0.05 &)],
   {tau, 0, leadMaxH}
];
(* {nLead+1, 12}: rows = lead in months, cols = init month *)
spreadMatrix = Table[
   allFcs[[m0, "std"]][[leadIdx[[k]]]],
   {k, Length @ leadIdx}, {m0, 12}
];

xTicks = Table[{m, monthNames[[m]]}, {m, 12}];
yTicks = Table[{tau + 1, ToString[tau]}, {tau, 0, leadMaxH}];

spreadFig = ArrayPlot[
   spreadMatrix,
   ColorFunction -> "TemperatureMap",
   ColorFunctionScaling -> True,
   PlotLegends -> Placed[
     BarLegend[Automatic,
        LegendLabel -> "ensemble std (\[Degree]C)",
        LegendMarkerSize -> {16, 240}],
     Right],
   Frame -> True,
   FrameLabel -> {{"initial calendar month m\:2080", None},
                  {"lead (months)", None}},
   FrameTicks -> {{yTicks, yTicks}, {xTicks, xTicks}},
   PlotLabel -> Style[
     "RDO ensemble spread (\[Degree]C) \[LongDash] toy-model spring barrier",
     Bold, 13],
   Mesh -> All,
   MeshStyle -> Directive[GrayLevel[0.92], Thickness[0.0015]],
   ImageSize -> 820, AspectRatio -> 1.05
];

Export[FileNameJoin[{imgDir, "rdo_spread_heatmap.png"}], spreadFig,
   ImageResolution -> 144];
Print["wrote rdo_spread_heatmap.png"];

(* Sanity check: print spread at lead 6 for each initial month *)
Print["StDev at lead 6 by init month: ",
   N @ Round[spreadMatrix[[7]], 0.01]];
