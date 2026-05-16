(* ::Package:: *)

(* Shared loaders for the tidy CSVs produced by src/fetch_*.py.
   Usage:  Get[FileNameJoin[{Directory[], "wolfram", "load_data.wl"}]]
*)

BeginPackage["ENSOData`"];

DataDirectory::usage = "DataDirectory[] returns the absolute path of data/.";
LoadNino34::usage   = "LoadNino34[] -> association with :months, :anom, :dates.";
LoadONI::usage      = "LoadONI[] -> association with :season, :year, :midMonth, :sst, :anom.";
LoadProbSnapshot::usage = "LoadProbSnapshot[] -> the parsed cpc_probabilities_snapshot.json.";

Begin["`Private`"];

DataDirectory[] := FileNameJoin[{
   ParentDirectory@DirectoryName[$InputFileName],
   "data"
}];

LoadNino34[] := Module[{path, csv, hdr, rows, ys, ms},
   path = FileNameJoin[{DataDirectory[], "nino34_monthly.csv"}];
   csv = Import[path, "CSV"];
   hdr = csv[[1]];
   rows = csv[[2 ;;]];
   ys = rows[[All, 1]];
   ms = rows[[All, 2]];
   <|
     "year"   -> ys,
     "month"  -> ms,
     "date"   -> MapThread[DateObject[{#1, #2, 15}, "Day"] &, {ys, ms}],
     "anom"   -> N @ rows[[All, 4]],
     "header" -> hdr
   |>
];

LoadONI[] := Module[{path, csv, rows},
   path = FileNameJoin[{DataDirectory[], "oni.csv"}];
   csv = Import[path, "CSV"];
   rows = csv[[2 ;;]];
   (* columns: season, year, mid_month, sst_c, sst_anom *)
   <|
     "season"   -> rows[[All, 1]],
     "year"     -> rows[[All, 2]],
     "midMonth" -> rows[[All, 3]],
     "sst"      -> rows[[All, 4]] // N,
     "anom"     -> rows[[All, 5]] // N
   |>
];

LoadProbSnapshot[] := Import[
   FileNameJoin[{DataDirectory[], "cpc_probabilities_snapshot.json"}],
   "RawJSON"
];

End[];
EndPackage[];
