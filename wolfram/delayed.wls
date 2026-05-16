#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Delayed-action oscillator of Suarez & Schopf (1988, JAS 45).

    Scalar delay differential equation in the eastern Pacific SST
    anomaly T(t):

        dT/dt = T  -  \[Alpha] T(t - \[Delta])  -  T^3  +  \[Sigma](t) \[Xi](t)

    where
        \[Delta]   wave-reflection delay (~ 6 months for the cross-basin
              Kelvin/Rossby round trip in the equatorial Pacific)
        \[Alpha]   ocean-wave coupling strength (0 < \[Alpha] < 1)
        T^3   cubic damping from nonlinear ocean dynamics
        \[Sigma](t) westerly wind-burst noise, max in boreal spring

    Time units chosen so that 1 unit = 2 months  =>  \[Delta] = 3 dimensionless
    units corresponds to the canonical 6-month wave delay.

    Integration: forward Euler-Maruyama with a circular history buffer
    so the delayed value is available without recomputation.

    Outputs to docs/images/:
      delayed_timeseries.png    sample T(t) over 20 years
      delayed_bifurcation.png   sweep over alpha at fixed delta
      delayed_cone.png          ensemble cones init Jan / Apr / Jul / Oct
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];

(* --- model parameters --------------------------------------------- *)

alphaDel = 0.75;        (* wave-reflection coupling                 *)
delayTau = 3.0;         (* time-unit delay (= 6 calendar months)    *)
sigBase  = 0.03;        (* background stochastic forcing            *)
sigSpring = 0.10;       (* extra WWB std-dev in boreal spring       *)
unit2Mon = 2.0;         (* 1 model unit  =  unit2Mon calendar months *)

(* WWB envelope peaks in March (calendar month = 3) *)
sigStoch[t_, m0_] := sigBase + sigSpring (
   1 + Cos[2 Pi (unit2Mon t + m0 - 3)/12]
);

(* --- Euler-Maruyama with circular history buffer ---------------- *)

simulateDDE[
   m0_, tEnd_, dt_, hist0_, seed_
] := Module[
   {nHist, hist, idx, T, t, ts, Ts, sqrtDt, eta, Tdel,
    nSteps, drift, diff},
   SeedRandom[seed];
   nHist  = Ceiling[delayTau/dt] + 2;
   hist   = ConstantArray[hist0, nHist];   (* circular buffer       *)
   idx    = 1;
   T      = hist0;
   t      = 0.;
   nSteps = Round[tEnd/dt];
   sqrtDt = Sqrt[dt];
   ts     = ConstantArray[0., nSteps + 1];
   Ts     = ConstantArray[0., nSteps + 1];
   ts[[1]] = 0.; Ts[[1]] = T;
   Do[
     (* look up T(t - delayTau) from history buffer *)
     With[{kBack = Round[delayTau/dt]},
       Tdel = hist[[1 + Mod[idx - 1 - kBack + nHist, nHist]]]
     ];
     drift = T - alphaDel Tdel - T^3;
     diff  = sigStoch[t, m0];
     eta   = RandomVariate[NormalDistribution[]];
     T += dt drift + sqrtDt diff eta;
     t += dt;
     idx = 1 + Mod[idx, nHist];
     hist[[idx]] = T;
     ts[[i + 1]] = t; Ts[[i + 1]] = T;
     , {i, nSteps}
   ];
   <|"t" -> ts, "T" -> Ts|>
];

(* --- 1. Single trajectory --------------------------------------- *)

samp = simulateDDE[1, 20*12/unit2Mon, 0.05, 0.3, 7];
Export[FileNameJoin[{imgDir, "delayed_timeseries.png"}],
   ListLinePlot[
     Transpose[{samp["t"] unit2Mon / 12., samp["T"]}],
     Frame -> True,
     FrameLabel -> {{"T anomaly", None}, {"years", None}},
     PlotLabel -> "Delayed-action oscillator: one realisation",
     PlotStyle -> Directive[Thickness[0.003], RGBColor[0.20, 0.45, 0.75]],
     PlotRange -> {Automatic, {-1.6, 1.6}},
     GridLines -> {Automatic, {-0.5, 0, 0.5}},
     ImageSize -> 950, AspectRatio -> 1/3.2
   ], ImageResolution -> 144];
Print["wrote delayed_timeseries.png"];

(* --- 2. Bifurcation sweep over alpha (with fixed delta) --------- *)

alphaList = Range[0.3, 1.15, 0.05];
bifurc = Table[
   With[{aSave = alphaDel},
     alphaDel = aL;
     With[{r = simulateDDE[1, 600., 0.1, 0.3, 13]},
       alphaDel = aSave;
       (* Take T values from t > 200 to skip transient; then sample
          local extrema by detecting sign changes in dT *)
       With[{TT = Drop[r["T"], Round[200./0.1]],
             tt = Drop[r["t"], Round[200./0.1]]},
         {aL, MinMax @ TT}
       ]
     ]
   ],
   {aL, alphaList}
];

bifurcPlot = Show[
   ListPlot[
     {{#1, #2[[1]]}} & @@@ bifurc,
     PlotStyle -> Directive[PointSize[0.012], RGBColor[0.20, 0.45, 0.75]]],
   ListPlot[
     {{#1, #2[[2]]}} & @@@ bifurc,
     PlotStyle -> Directive[PointSize[0.012], RGBColor[0.85, 0.32, 0.10]]],
   Frame -> True,
   FrameLabel -> {{"min / max of T (steady state)", None},
                  {"wave coupling \[Alpha]", None}},
   PlotLabel -> "Bifurcation: amplitude of stochastic oscillation vs \[Alpha]",
   GridLines -> {{0.5}, {0}},
   ImageSize -> 720, AspectRatio -> 0.55
];
Export[FileNameJoin[{imgDir, "delayed_bifurcation.png"}], bifurcPlot,
   ImageResolution -> 144];
Print["wrote delayed_bifurcation.png"];

(* --- 3. Ensemble cones for four initial months ------------------ *)

nMembers = 400;
leadMonths = 18;
leadUnits = leadMonths / unit2Mon;
dtF = 0.05;
monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

(* Climatology of T at each calendar month: same long-run trick. *)
SeedRandom[1];
climRun = simulateDDE[1, 12*400/unit2Mon, 0.2, 0., 1];
climMonths = Floor[Mod[climRun["t"] unit2Mon, 12.]] + 1;
climByMonth = AssociationThread[Range[12] -> Table[{}, {12}]];
Do[
   AppendTo[climByMonth[climMonths[[k]]], climRun["T"][[k]]]
   , {k, Length @ climRun["t"]}
];
climByMonth = (Drop[#, Min[100, Floor[Length[#]/4]]] &) /@ climByMonth;
drawClim[m0_, k_] := With[{pool = climByMonth[m0]},
   pool[[1 + Mod[k, Length[pool]]]]];

forecastEnsemble[m0_] := Module[{trajs, Tmat},
   trajs = Table[
     simulateDDE[m0, leadUnits, dtF, drawClim[m0, k], m0*97 + k],
     {k, nMembers}];
   Tmat = Transpose[#["T"] & /@ trajs];
   <|
     "t"      -> trajs[[1, "t"]] * unit2Mon,   (* in calendar months *)
     "median" -> Median /@ Tmat,
     "p05"    -> Map[Quantile[#, 0.05] &, Tmat],
     "p25"    -> Map[Quantile[#, 0.25] &, Tmat],
     "p75"    -> Map[Quantile[#, 0.75] &, Tmat],
     "p95"    -> Map[Quantile[#, 0.95] &, Tmat],
     "std"    -> StandardDeviation /@ Tmat
   |>
];

band[t_, lo_, hi_, color_, opacity_] := ListLinePlot[
   {Transpose[{t, lo}], Transpose[{t, hi}]},
   PlotStyle -> Directive[Thickness[0.0005], color, Opacity[0.5]],
   Filling -> {1 -> {{2}, Directive[Opacity[opacity], color]}},
   PlotRange -> All
];

panelMonths = {1, 4, 7, 10};
SeedRandom[42];
fcs = AssociationThread[panelMonths -> (forecastEnsemble /@ panelMonths)];

conePanel[m0_] := With[{f = fcs[m0]},
   With[{t = f["t"], p05 = f["p05"], p25 = f["p25"], p50 = f["median"],
         p75 = f["p75"], p95 = f["p95"]},
     Show[
       band[t, p05, p95, RGBColor[0.20, 0.45, 0.75], 0.20],
       band[t, p25, p75, RGBColor[0.20, 0.45, 0.75], 0.45],
       ListLinePlot[Transpose[{t, p50}],
          PlotStyle -> Directive[Thickness[0.005], Black]],
       Frame -> True,
       FrameLabel -> {{"T anomaly", None},
                      {"verification month", None}},
       FrameTicks -> {
         {Automatic, None},
         {Table[{tau, monthNames[[1 + Mod[m0 - 1 + tau, 12]]]},
            {tau, 0, leadMonths, 3}], None}
       },
       PlotRange -> {{0, leadMonths}, {-1.6, 1.6}},
       PlotLabel -> Style[
         "init " <> monthNames[[m0]] <> "  (m\:2080 = " <> ToString[m0] <> ")",
         Bold],
       GridLines -> {None, {-0.5, 0, 0.5}},
       GridLinesStyle -> Directive[GrayLevel[0.7], Dashed],
       AspectRatio -> 0.7, ImageSize -> 520
     ]
   ]
];

Export[FileNameJoin[{imgDir, "delayed_cone.png"}],
   Grid[Partition[conePanel /@ panelMonths, 2], Spacings -> {1.5, 1.5}],
   ImageResolution -> 144];
Print["wrote delayed_cone.png"];

(* --- 4. Spread heatmap ----------------------------------------- *)

leadMaxH = 15;
nMemH = 200;
dtH = 0.1;
SeedRandom[91];

allFcs = Table[
   Module[{trajs, Tmat},
     trajs = Table[
        simulateDDE[m0, leadMaxH/unit2Mon, dtH,
           drawClim[m0, k], m0*7919 + k]
        , {k, nMemH}];
     Tmat = Transpose[#["T"] & /@ trajs];
     <|"t" -> trajs[[1, "t"]] * unit2Mon,
       "std" -> StandardDeviation /@ Tmat|>
   ], {m0, 12}];

timesD = allFcs[[1, "t"]];
leadIdxD = Flatten @ Table[
   FirstPosition[timesD, _?(# >= tau - 0.05 &)],
   {tau, 0, leadMaxH}];
spreadMatD = Table[
   allFcs[[m0, "std"]][[leadIdxD[[k]]]],
   {k, Length @ leadIdxD}, {m0, 12}];

xTicksD = Table[{m, monthNames[[m]]}, {m, 12}];
yTicksD = Table[{tau + 1, ToString[tau]}, {tau, 0, leadMaxH}];

Export[FileNameJoin[{imgDir, "delayed_spread_heatmap.png"}],
   ArrayPlot[
     spreadMatD, ColorFunction -> "TemperatureMap",
     PlotLegends -> Placed[
       BarLegend[Automatic,
          LegendLabel -> "ensemble std",
          LegendMarkerSize -> {16, 240}],
       Right],
     Frame -> True,
     FrameLabel -> {{"initial calendar month m\:2080", None},
                    {"lead (months)", None}},
     FrameTicks -> {{yTicksD, yTicksD}, {xTicksD, xTicksD}},
     PlotLabel -> Style[
        "Delayed-oscillator ensemble spread", Bold, 13],
     Mesh -> All,
     MeshStyle -> Directive[GrayLevel[0.92], Thickness[0.0015]],
     ImageSize -> 820, AspectRatio -> 1.05
   ], ImageResolution -> 144];
Print["wrote delayed_spread_heatmap.png"];
Print["DDE StDev at lead 6 by init month: ",
   N @ Round[spreadMatD[[7]], 0.01]];
