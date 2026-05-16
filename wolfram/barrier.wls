#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Spring predictability barrier from the observed Niño 3.4 record.

   For each initial calendar month m in 1..12 and lead time tau in
   0..15 months, compute the Pearson correlation

       rho(m, tau) = corr( T(year, m), T(year', m + tau) )

   across all years available, with year' shifted into the next year
   when m + tau > 12. The hallmark of the spring barrier is a band of
   low rho whenever the *verification* month is May-June, independent
   of the initial month, because westerly-wind-burst noise in
   boreal spring decorrelates the system across April.

   Outputs:
     docs/images/spring_barrier_obs.png   ArrayPlot heatmap
     docs/images/nino34_record.png        Time series of the anomaly
     data/spring_barrier_obs.csv          Raw rho matrix for reuse
*)

ClearAll[$Loaded];
Get[FileNameJoin[{DirectoryName[$InputFileName], "load_data.wl"}]];

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];
If[! DirectoryQ[imgDir], CreateDirectory[imgDir, CreateIntermediateDirectories -> True]];

n34 = ENSOData`LoadNino34[];

(* --- 1. Time series figure --------------------------------------- *)

tsPlot = DateListPlot[
   Transpose[{n34["date"], n34["anom"]}],
   PlotStyle -> {Thickness[0.0035], RGBColor[0.85, 0.32, 0.10]},
   Filling -> Axis,
   FillingStyle -> Directive[Opacity[0.25], RGBColor[0.85, 0.32, 0.10]],
   FrameLabel -> {"year", "Niño 3.4 anomaly (°C)"},
   PlotLabel -> "Monthly Niño 3.4 SST anomaly (ERSSTv5, NOAA PSL)",
   GridLines -> {None, {-0.5, 0, 0.5}},
   GridLinesStyle -> Directive[Gray, Dashed],
   ImageSize -> 900, AspectRatio -> 1/3.2
];
Export[FileNameJoin[{imgDir, "nino34_record.png"}], tsPlot, ImageResolution -> 144];
Print["wrote ", FileNameJoin[{imgDir, "nino34_record.png"}]];

(* --- 2. Spring predictability barrier ---------------------------- *)

(* Reshape the monthly anomalies into a {nYears, 12} matrix. Pad the
   trailing partial year with Missing[] so that years align. *)
years = Sort @ DeleteDuplicates @ n34["year"];
ymGrouped = GroupBy[
   Transpose[{n34["year"], n34["month"], n34["anom"]}],
   First -> (#[[{2, 3}]] &)
];
toRow[yr_] := Module[{rec = ymGrouped[yr], byMonth},
   byMonth = Association @@ Rule @@@ rec;
   Table[Lookup[byMonth, m, Missing["NotAvailable"]], {m, 1, 12}]
];
monthlyMatrix = toRow /@ years;        (* {nYears, 12}, possibly Missing *)

(* Build a "long" matrix that lets us index (year, month + tau) safely
   by flattening to a single column vector indexed by year*12 + (m-1). *)
flat = Flatten[monthlyMatrix];

(* For initial month m and lead tau, build the paired list
   { ( T_{flat[i]}, T_{flat[i+tau]} ) : i corresponds to month m in
   some year, and both entries are numeric }. *)
pairAt[m_, tau_] := Module[{idx0, idx1, t0, t1, ok},
   idx0 = Range[m, Length[flat], 12];           (* positions of month m *)
   idx1 = idx0 + tau;
   ok = idx1 <= Length[flat];
   idx0 = Pick[idx0, ok]; idx1 = Pick[idx1, ok];
   t0 = flat[[idx0]]; t1 = flat[[idx1]];
   ok = NumberQ /@ t0 /. False -> 0 /. True -> 1;
   ok = ok * (NumberQ /@ t1 /. False -> 0 /. True -> 1);
   {Pick[t0, ok, 1], Pick[t1, ok, 1]}
];

corrSafe[a_, b_] := If[Length[a] < 4 || StandardDeviation[a] == 0 ||
    StandardDeviation[b] == 0, Missing["TooFew"], Correlation[a, b]];

leadMax = 15;
rhoMatrix = Table[
   With[{p = pairAt[m, tau]}, corrSafe @@ p],
   {tau, 0, leadMax}, {m, 1, 12}
];

(* Export the matrix (rows = lead in months, cols = initial month) *)
Export[
   FileNameJoin[{repoRoot, "data", "spring_barrier_obs.csv"}],
   Prepend[
     Prepend[rhoMatrix /. Missing[___] -> "NaN", Range[1, 12]],
     Prepend[Range[0, leadMax], "lead\\month"]
   ],
   "CSV"
];

(* Heatmap. We orient it so that the y-axis is lead in months,
   x-axis is initial calendar month, low correlation is red. *)
monthLabels = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

barrierFig = ArrayPlot[
   rhoMatrix /. _Missing -> 0,
   ColorFunction -> (Blend[
     {{-0.2, RGBColor[0.78, 0.10, 0.10]},
      {0.2,  RGBColor[0.95, 0.95, 0.95]},
      {0.6,  RGBColor[0.30, 0.55, 0.85]},
      {1.0,  RGBColor[0.05, 0.20, 0.55]}}, #] &),
   ColorFunctionScaling -> False,
   PlotLegends -> Placed[BarLegend[Automatic, LegendLabel -> "ρ"], Right],
   DataReversed -> True,
   FrameLabel -> {"initial calendar month", "lead (months)"},
   FrameTicks -> {
     {Table[{Length[rhoMatrix] - i, i}, {i, 0, leadMax}], None},
     {Table[{m, monthLabels[[m]]}, {m, 12}], None}
   },
   PlotLabel -> Style[
     "Spring predictability barrier — Niño 3.4 autocorrelation ρ(m, lead)",
     Bold, 13
   ],
   ImageSize -> 760,
   AspectRatio -> (leadMax + 1)/12.
];

Export[FileNameJoin[{imgDir, "spring_barrier_obs.png"}], barrierFig,
   ImageResolution -> 144];
Print["wrote ", FileNameJoin[{imgDir, "spring_barrier_obs.png"}]];
Print["wrote ", FileNameJoin[{repoRoot, "data", "spring_barrier_obs.csv"}]];
