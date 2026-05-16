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

iso2Date[s_String] := DateObject[StringSplit[s, "-"] // ToExpression];

LoadNino34[] := Module[{path, csv, hdr, rows},
   path = FileNameJoin[{DataDirectory[], "nino34_monthly.csv"}];
   csv = Import[path, "CSV"];
   hdr = csv[[1]];
   rows = csv[[2 ;;]];
   (* columns: year, month, date(ISO), sst_anom *)
   <|
     "year"   -> rows[[All, 1]],
     "month"  -> rows[[All, 2]],
     "date"   -> iso2Date /@ rows[[All, 3]],
     "anom"   -> rows[[All, 4]] // N,
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
