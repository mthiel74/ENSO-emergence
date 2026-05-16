#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Spring predictability barrier from the observed Ni\[NTilde]o 3.4 record.

   For each initial calendar month m1 in 1..12 and lead tau in 0..15,
   compute the Pearson correlation

       \[Rho](m1, tau) = corr( T(year, m1), T(year', m1 + tau) )

   across all years available, with year' shifted into the next year
   when m1 + tau > 12.

   The hallmark of the spring barrier is a band of low \[Rho] whenever
   the *verification* month is May-June, independent of the initial
   month, because westerly-wind-burst noise in boreal spring
   decorrelates the system across April.

   Outputs:
     docs/images/nino34_record.png       Niño 3.4 record (time series)
     docs/images/spring_barrier_obs.png  \[Rho](m, lead) heatmap
     data/spring_barrier_obs.csv         raw rho matrix
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "load_data.wl"}]];

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];
If[! DirectoryQ[imgDir],
   CreateDirectory[imgDir, CreateIntermediateDirectories -> True]];

n34 = ENSOData`LoadNino34[];

(* ----- 1. Time series of the anomaly ------------------------------ *)

tsPairs = Transpose[{
   AbsoluteTime /@ n34["date"],
   n34["anom"]
}];

tsPlot = DateListPlot[
   Transpose[{n34["date"], n34["anom"]}],
   Joined -> True,
   PlotStyle -> Directive[Thickness[0.0035], RGBColor[0.85, 0.32, 0.10]],
   Filling -> 0,
   FillingStyle -> Directive[Opacity[0.25], RGBColor[0.85, 0.32, 0.10]],
   FrameLabel -> {"year",
      "Ni\[NTilde]o 3.4 anomaly (\[Degree]C)"},
   PlotLabel -> "Monthly Ni\[NTilde]o 3.4 SST anomaly (ERSSTv5, NOAA PSL)",
   GridLines -> {None, {-0.5, 0, 0.5}},
   GridLinesStyle -> Directive[Gray, Dashed],
   ImageSize -> 1100,
   AspectRatio -> 1/3.4
];

Export[FileNameJoin[{imgDir, "nino34_record.png"}], tsPlot,
   ImageResolution -> 144];
Print["wrote ", FileNameJoin[{imgDir, "nino34_record.png"}]];

(* ----- 2. Spring predictability barrier --------------------------- *)

(* Build a "long" 1-D vector of monthly anomalies indexed by
       i = (year - y0)*12 + month
   so that lead lookups become trivial array offsets. *)
years = Sort @ DeleteDuplicates @ n34["year"];
y0 = First[years];
y1 = Last[years];
nMonths = (y1 - y0 + 1)*12;

flat = ConstantArray[Missing["NotAvailable"], nMonths];
MapThread[
   (flat[[(#1 - y0)*12 + #2]] = #3) &,
   {n34["year"], n34["month"], n34["anom"]}
];

(* For initial month m1 and lead tau, list all { T_i, T_{i+tau} } pairs
   such that i corresponds to month m1 and both entries are numeric. *)
pairAt[m1_, tau_] := Module[{idx0, idx1, pairs},
   idx0 = Range[m1, nMonths, 12];
   idx1 = idx0 + tau;
   idx0 = Select[idx0, # + tau <= nMonths &];
   idx1 = idx0 + tau;
   pairs = Transpose[{flat[[idx0]], flat[[idx1]]}];
   Select[pairs, And @@ (NumberQ /@ #) &]
];

corrSafe[pairs_] := If[
   Length[pairs] < 4 ||
   StandardDeviation[pairs[[All, 1]]] == 0 ||
   StandardDeviation[pairs[[All, 2]]] == 0,
   Missing["TooFew"],
   Correlation[pairs[[All, 1]], pairs[[All, 2]]]
];

leadMax = 15;
(* rows = lead 0..leadMax (with lead 0 = perfect autocorr = 1) *)
(* cols = initial month 1..12                                     *)
rhoMatrix = Table[
   corrSafe @ pairAt[m1, tau],
   {tau, 0, leadMax}, {m1, 1, 12}
];

(* Sanity print: lead-1 correlations should all be ~0.9 except the spring drop. *)
Print["lead=1 corr by initial month: ",
   N @ Round[rhoMatrix[[2]], 0.01]];
Print["lead=6 corr by initial month: ",
   N @ Round[rhoMatrix[[7]], 0.01]];

(* Save raw rho matrix. *)
csvOut = FileNameJoin[{repoRoot, "data", "spring_barrier_obs.csv"}];
Export[csvOut,
   Prepend[
     MapIndexed[
       Prepend[#1 /. _Missing -> "NaN", First[#2] - 1] &,
       rhoMatrix
     ],
     Prepend[{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"},
       "lead\\month"]
   ],
   "CSV"
];
Print["wrote ", csvOut];

(* Heatmap. ArrayPlot draws row 1 at the top, so as-is lead 0 (= 1.0)
   sits on the top edge, lead leadMax at the bottom. *)
monthLabels = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

(* ArrayPlot displays row 1 at the TOP and uses tick y-coordinate that
   matches the *row index from top* (i.e. row 1 has y-tick position 1).
   Our matrix has lead 0 in row 1, so lead tau is at tick position
   tau + 1. *)
xTicks = Table[{m, monthLabels[[m]]}, {m, 12}];
yTicks = Table[{tau + 1, ToString[tau]}, {tau, 0, leadMax}];

barrierFig = ArrayPlot[
   rhoMatrix /. _Missing -> 0.,
   ColorFunction -> (Blend[
     {{-0.2, RGBColor[0.65, 0.10, 0.10]},
      {0.0,  RGBColor[0.90, 0.50, 0.40]},
      {0.3,  RGBColor[0.97, 0.97, 0.95]},
      {0.6,  RGBColor[0.35, 0.55, 0.85]},
      {1.0,  RGBColor[0.05, 0.20, 0.55]}}, #] &),
   ColorFunctionScaling -> False,
   PlotLegends -> Placed[
     BarLegend[{
        (Blend[
          {{-0.2, RGBColor[0.65, 0.10, 0.10]},
           {0.0,  RGBColor[0.90, 0.50, 0.40]},
           {0.3,  RGBColor[0.97, 0.97, 0.95]},
           {0.6,  RGBColor[0.35, 0.55, 0.85]},
           {1.0,  RGBColor[0.05, 0.20, 0.55]}}, #] &),
        {-0.2, 1.0}},
        LegendLabel -> "\[Rho]",
        LegendMarkerSize -> {16, 240}
     ],
     Right
   ],
   Frame -> True,
   FrameLabel -> {{"initial calendar month m\:2080", None},
                  {"lead (months)", None}},
   FrameTicks -> {{yTicks, yTicks}, {xTicks, xTicks}},
   PlotLabel -> Style[
     "Spring predictability barrier \[LongDash] "
     <> "Ni\[NTilde]o 3.4 autocorrelation \[Rho](m\:2080, lead)",
     Bold, 13
   ],
   Mesh -> All,
   MeshStyle -> Directive[GrayLevel[0.85], Thickness[0.0015]],
   ImageSize -> 820,
   AspectRatio -> 1.05
];

Export[FileNameJoin[{imgDir, "spring_barrier_obs.png"}], barrierFig,
   ImageResolution -> 144];
Print["wrote ", FileNameJoin[{imgDir, "spring_barrier_obs.png"}]];

(* ----- 3. Companion: "persistence forecast skill drop" curve ------ *)

(* For each starting month m1, plot rho vs lead. The lines that start
   in Jan-Mar drop quickly as the forecast crosses April; lines that
   start in Jul-Aug stay high through the calendar year. *)
persistenceCurves = Table[
   With[{r = rhoMatrix[[All, m1]]},
     Tooltip[
       Transpose[{Range[0, leadMax], r /. _Missing -> 0.}],
       monthLabels[[m1]]
     ]
   ], {m1, 12}
];

persistencePlot = ListLinePlot[
   persistenceCurves,
   PlotRange -> {{0, leadMax}, {-0.2, 1.05}},
   PlotLegends -> Placed[monthLabels, Right],
   FrameLabel -> {{"autocorrelation \[Rho]", None},
                  {"lead (months from initial month m\:2080)", None}},
   PlotLabel -> Style[
     "Persistence-skill drop, observed Ni\[NTilde]o 3.4",
     Bold, 13
   ],
   GridLines -> {Automatic, {0, 0.5}},
   GridLinesStyle -> Directive[Gray, Dashed],
   ImageSize -> 900,
   Frame -> True,
   AspectRatio -> 0.55
];

Export[FileNameJoin[{imgDir, "persistence_drop.png"}], persistencePlot,
   ImageResolution -> 144];
Print["wrote ", FileNameJoin[{imgDir, "persistence_drop.png"}]];
