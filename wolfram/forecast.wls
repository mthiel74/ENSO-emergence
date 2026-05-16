#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Live-data overlay: initialise the recharge-discharge oscillator
    from the most recent observed Ni\[NTilde]o 3.4 anomaly, run a 500-
    member ensemble out to ~18 months, and stack three panels:

       1. The IRI/CPC ENSO forecast plume PNG (issue we last grabbed).
       2. The toy-model cone, on the same Ni\[NTilde]o 3.4 / lead axis.
       3. The NOAA CPC May-2026 ENSO Watch probability bars.

    Output:  docs/images/forecast_overlay.png

    The toy-model thermocline state h is inferred from the recent
    monthly tendency dT/dt via the linearised SST equation
        dT/dt \[TildeTilde] R(t) T + \[Gamma] h  =>  h \[TildeTilde] (dT/dt - R(t) T) / \[Gamma].
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "load_data.wl"}]];

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];

(* ---- parameters mirror wolfram/rdo.wls ---------------------------- *)
R0     = -0.03;  R1   = 0.18;  phiR = 9.0;
gamma  = 0.06;   rDamp = 0.04; alpha = 0.15;
sigmaT = 0.06;   sigmaH = 0.04;
sigW0  = 0.22;   aW = 0.80;    phiW = 3.0;
Rseason[t_, m0_]  := R0 + R1 Cos[2 Pi (t + m0 - phiR)/12];
sigWWB[t_, m0_]   := sigW0 (1 + aW Cos[2 Pi (t + m0 - phiW)/12]);

simulate[T0_, h0_, m0_, tEnd_, dt_, seed_] := Module[
   {steps, T, h, t, sqrtDt, ts, Ts, hs, eT, eH, eW},
   SeedRandom[seed];
   steps = Round[tEnd/dt];
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
     , {i, steps}];
   <|"t" -> ts, "T" -> Ts, "h" -> hs|>
];

(* ---- read the latest observed state ------------------------------- *)
n34 = ENSOData`LoadNino34[];
lastIdx = Length @ n34["anom"];

Tlast  = n34["anom"][[lastIdx]];
Tprev2 = n34["anom"][[lastIdx - 2]];
m0last = n34["month"][[lastIdx]];
yrlast = n34["year"][[lastIdx]];

(* monthly tendency in degC/month, smoothed over 2 months *)
dTdt = (Tlast - Tprev2) / 2.;
(* infer thermocline state from linearised SST equation *)
hEst = (dTdt - Rseason[0., m0last] Tlast) / gamma;

Print["Last observation: ", yrlast, "-", m0last,
   "  T = ", Tlast, "  dT/dt \[TildeTilde] ", dTdt,
   "  inferred h \[TildeTilde] ", hEst];

(* ---- live forecast ensemble --------------------------------------- *)
nMembers   = 500;
leadMonths = 18;
dtF        = 0.05;

SeedRandom[2026];
trajs = Table[
   simulate[Tlast, hEst, m0last, leadMonths, dtF, k],
   {k, nMembers}];

Tmat = Transpose[#["T"] & /@ trajs];
times = trajs[[1, "t"]];   (* months from t=0 *)
median = Median /@ Tmat;
p05    = Map[Quantile[#, 0.05] &, Tmat];
p25    = Map[Quantile[#, 0.25] &, Tmat];
p75    = Map[Quantile[#, 0.75] &, Tmat];
p95    = Map[Quantile[#, 0.95] &, Tmat];

(* ---- panel 2: toy-model cone in NOAA-ENSO axes -------------------- *)

monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

band[t_, lo_, hi_, color_, opacity_] := ListLinePlot[
   {Transpose[{t, lo}], Transpose[{t, hi}]},
   PlotStyle -> Directive[Thickness[0.0005], color, Opacity[0.5]],
   Filling -> {1 -> {{2}, Directive[Opacity[opacity], color]}},
   PlotRange -> All];

(* include the last 12 months of observations to anchor the cone *)
hist12 = Table[
   {-12. + k, n34["anom"][[lastIdx - 12 + k]]},
   {k, 0, 11}];

conePanel = Show[
   band[times, p05, p95, RGBColor[0.83, 0.32, 0.10], 0.18],
   band[times, p25, p75, RGBColor[0.83, 0.32, 0.10], 0.45],
   ListLinePlot[Transpose[{times, median}],
      PlotStyle -> Directive[Thickness[0.005], Black]],
   ListLinePlot[hist12,
      PlotStyle -> Directive[Thickness[0.004], Black, Dashed],
      PlotMarkers -> {Automatic, 6}],
   Graphics[{
      Directive[Thick, Gray, Dashed], Line[{{0, -3}, {0, 3}}],
      Directive[FontFamily -> "Helvetica", FontSize -> 11, Black],
      Text["latest obs\n(Apr 2026)", {0., 2.5}, {-1.1, 0}]
   }],
   Frame -> True,
   FrameLabel -> {{"Ni\[NTilde]o 3.4 SST anomaly (\[Degree]C)", None},
                  {"verification month", None}},
   FrameTicks -> {
     {Automatic, None},
     {Join[
        Table[{tau, monthNames[[1 + Mod[m0last - 1 + tau, 12]]]},
            {tau, -12, leadMonths, 3}]
      ], None}
   },
   PlotRange -> {{-12, leadMonths}, {-3, 3}},
   PlotLabel -> Style[
     "Recharge-discharge ensemble forecast (init " <>
       monthNames[[m0last]] <> " " <> ToString[yrlast] <> ")",
     Bold],
   GridLines -> {Range[-12, leadMonths, 3], {-0.5, 0, 0.5}},
   GridLinesStyle -> Directive[GrayLevel[0.8], Dashed],
   AspectRatio -> 0.55, ImageSize -> 900
];

(* ---- panel 3: NOAA probability bars ------------------------------- *)
probData = ENSOData`LoadProbSnapshot[];
seasons = #["season"] & /@ probData["probabilities"];
laNina  = #["la_nina"] & /@ probData["probabilities"];
neutral = #["neutral"] & /@ probData["probabilities"];
elNino  = #["el_nino"] & /@ probData["probabilities"];
nSea    = Length @ seasons;

bars = Table[
   {
     {Style[seasons[[k]], FontFamily -> "Helvetica", FontSize -> 9],
       laNina[[k]], neutral[[k]], elNino[[k]]}
   }, {k, nSea}];

probBars = BarChart[
   Transpose[{laNina, neutral, elNino}],
   ChartLayout -> "Stacked",
   ChartStyle -> {RGBColor[0.20, 0.45, 0.75],   (* La Nina blue *)
                  RGBColor[0.6, 0.6, 0.6],      (* Neutral grey *)
                  RGBColor[0.83, 0.32, 0.10]},  (* El Nino red  *)
   ChartLabels -> {Style[#, FontSize -> 9] & /@ seasons, None},
   ChartLegends -> Placed[
     {"La Ni\[NTilde]a", "Neutral", "El Ni\[NTilde]o"},
     Right],
   Frame -> True,
   FrameLabel -> {{"probability (%)", None},
                  {"rolling 3-month season", None}},
   PlotLabel -> Style[
     "CPC / IRI official ENSO probabilities (May 2026 issuance)",
     Bold],
   PlotRange -> {Automatic, {0, 100}},
   AspectRatio -> 0.45, ImageSize -> 900
];

(* ---- panel 1: IRI plume PNG --------------------------------------- *)
iriPath = FileNameJoin[{repoRoot, "data", "raw", "iri_plume_current.png"}];
iriManifest = Import[
   FileNameJoin[{repoRoot, "data", "iri_plume_manifest.json"}], "RawJSON"];
iriIssue = Lookup[iriManifest, "issue_month", "unknown"];
iriImg = Image[Import[iriPath, "PNG"]];
iriPanel = Labeled[
   iriImg,
   Style[
     "IRI / CPC plume (issue " <> ToString[iriIssue] <> ")\n" <>
       "Live ensemble of ~25 dynamical + statistical models",
     Bold, FontFamily -> "Helvetica", FontSize -> 11],
   Top
];

(* ---- assemble & export -------------------------------------------- *)
overlay = Grid[
   {{iriPanel}, {conePanel}, {probBars}},
   Spacings -> {2, 2},
   ItemSize -> {Full, Automatic}
];
Export[FileNameJoin[{imgDir, "forecast_overlay.png"}], overlay,
   ImageResolution -> 144];
Print["wrote ", FileNameJoin[{imgDir, "forecast_overlay.png"}]];

(* also save a single-panel version of the toy-model forecast cone *)
Export[FileNameJoin[{imgDir, "rdo_live_forecast.png"}], conePanel,
   ImageResolution -> 144];
Print["wrote rdo_live_forecast.png"];

(* and the standalone probability bars *)
Export[FileNameJoin[{imgDir, "noaa_probabilities.png"}], probBars,
   ImageResolution -> 144];
Print["wrote noaa_probabilities.png"];
