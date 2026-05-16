#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Niño 3.4 monthly SST anomalies (NOAA PSL, ERSSTv5).
   Source:  https://psl.noaa.gov/data/correlation/nina34.anom.data
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "fetcher_common.wl"}]];

url     = "https://psl.noaa.gov/data/correlation/nina34.anom.data";
rawFile = FileNameJoin[{ENSOFetch`RawDir[], "nina34.anom.data"}];
outCsv  = FileNameJoin[{ENSOFetch`DataDir[], "nino34_monthly.csv"}];

ENSOFetch`Banner["Niño 3.4 monthly SST anomalies (NOAA PSL, ERSSTv5)"];
res = ENSOFetch`FetchText[url, rawFile, 6];
Print["  raw: ", res["savedTo"], "  (", res["bytes"], " B, cached=", res["cached"], ")"];

parsePSL[text_String] := Module[
   {lines, header, y0, y1, missing = {}, rows = {}, parts, yr, vals},
   lines = Select[StringTrim /@ StringSplit[text, "\n"], # =!= "" &];
   header = StringSplit @ First @ lines;
   {y0, y1} = ToExpression /@ header;
   Do[
     parts = StringSplit @ ln;
     Which[
       Length[parts] == 1,
         Quiet@Check[AppendTo[missing, ToExpression@First@parts], Null],
       Length[parts] >= 13,
         yr = ToExpression @ parts[[1]];
         If[IntegerQ[yr] && y0 <= yr <= y1,
           vals = ToExpression /@ parts[[2 ;; 13]];
           MapIndexed[
             AppendTo[rows, {yr, First@#2, #1}] &, vals]
         ]
     ],
     {ln, Rest@lines}
   ];
   If[missing =!= {},
     rows = Select[rows, ! MemberQ[missing, #[[3]]] &]
   ];
   rows
];

rows = parsePSL @ Import[rawFile, "Text"];
date[y_, m_] := DateString[DateObject[{y, m, 15}], {"Year", "-", "Month", "-", "Day"}];
csvRows = Prepend[
   {#[[1]], #[[2]], date[#[[1]], #[[2]]], NumberForm[#[[3]], {6, 4}] // ToString} & /@ rows,
   {"year", "month", "date", "sst_anom"}
];
Export[outCsv, csvRows, "CSV"];

Print["  parsed: ", Length[rows], " rows  span=",
   {First@rows, Last@rows}];
Print["  tidy: ", outCsv];
