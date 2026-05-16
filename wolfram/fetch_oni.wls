#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Oceanic Niño Index — CPC.
   Source:  https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "fetcher_common.wl"}]];

url     = "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt";
rawFile = FileNameJoin[{ENSOFetch`RawDir[], "oni.ascii.txt"}];
outCsv  = FileNameJoin[{ENSOFetch`DataDir[], "oni.csv"}];

ENSOFetch`Banner["Oceanic Niño Index — CPC"];
res = ENSOFetch`FetchText[url, rawFile, 6];
Print["  raw: ", res["savedTo"], "  (", res["bytes"], " B, cached=", res["cached"], ")"];

seasonMid = <|
   "DJF" -> 1, "JFM" -> 2, "FMA" -> 3, "MAM" -> 4, "AMJ" -> 5, "MJJ" -> 6,
   "JJA" -> 7, "JAS" -> 8, "ASO" -> 9, "SON" -> 10, "OND" -> 11, "NDJ" -> 12
|>;

parseONI[text_String] := Module[{lines, parts, rows = {}, seas, yr, total, anom},
   lines = Select[StringTrim /@ StringSplit[text, "\n"], # =!= "" &];
   Do[
     parts = StringSplit @ ln;
     If[Length[parts] == 4,
       seas = parts[[1]];
       If[KeyExistsQ[seasonMid, seas],
         yr = Quiet @ ToExpression @ parts[[2]];
         total = Quiet @ ToExpression @ parts[[3]];
         anom = Quiet @ ToExpression @ parts[[4]];
         If[IntegerQ[yr] && NumericQ[total] && NumericQ[anom],
           AppendTo[rows, {seas, yr, seasonMid[seas], total, anom}]
         ]
       ]
     ],
     {ln, lines}
   ];
   rows
];

rows = parseONI @ Import[rawFile, "Text"];
csvRows = Prepend[rows, {"season", "year", "mid_month", "sst_c", "sst_anom"}];
Export[outCsv, csvRows, "CSV"];

Print["  parsed: ", Length[rows], " rows  span=", {First@rows, Last@rows}];
Print["  tidy: ", outCsv];
