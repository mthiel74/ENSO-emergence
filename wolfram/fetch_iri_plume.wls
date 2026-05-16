#!/usr/bin/env wolframscript
(* ::Package:: *)

(* IRI / CPC ENSO forecast plume PNG.

   IRI stopped publishing the underlying per-model data table in 2024;
   only the rendered PNG remains publicly addressable, at

     https://ensoforecast.iri.columbia.edu/cgi-bin/sst_table_img?month=<m>&year=<y>

   The plume is issued in the second half of each month. We walk back
   from the current month until we find an available issue, then write
   data/iri_plume_manifest.json describing what we grabbed.
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "fetcher_common.wl"}]];

urlTemplate = "https://ensoforecast.iri.columbia.edu/cgi-bin/sst_table_img?month=`m`&year=`y`";
outPng      = FileNameJoin[{ENSOFetch`RawDir[], "iri_plume_current.png"}];
manifest    = FileNameJoin[{ENSOFetch`DataDir[], "iri_plume_manifest.json"}];

ENSOFetch`Banner["IRI / CPC ENSO forecast plume"];

today = Take[DateList[], 3];   (* {y, m, d} *)

walkBack[startY_, startM_, maxSteps_:6] := Module[
   {y = startY, m = startM, attempt, url, i = 0, foundIssue = None},
   While[i < maxSteps,
     url = StringTemplate[urlTemplate][<|"m" -> m, "y" -> y|>];
     attempt = ENSOFetch`FetchBytes[url, outPng, 6];
     If[attempt["ok"],
       foundIssue = {y, m};
       Return[{attempt, foundIssue}]
     ];
     If[m == 1, m = 12; y = y - 1, m = m - 1];
     i++;
   ];
   {attempt, None}
];

{attempt, issue} = walkBack[today[[1]], today[[2]]];

manifestData = <|
   "fetched_at"  -> DateString[Now, "ISODateTime"],
   "issue_month" -> If[issue === None, Null,
       DateString[DateObject[issue ~ Join ~ {1}], {"Year", "-", "Month", "-", "Day"}]],
   "attempt"     -> (attempt /. None -> Null)
|>;
Export[manifest, manifestData, "JSON"];

If[attempt["ok"],
   Print["  png: ", attempt["savedTo"], " (issue ", issue,
         ", ", attempt["bytes"], " B, cached=", attempt["cached"], ")"],
   Print["  png: FAILED  last_error=", attempt["error"]]
];
Print["  manifest: ", manifest];
