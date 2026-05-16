#!/usr/bin/env wolframscript
(* ::Package:: *)

(* CPC ENSO Diagnostic Discussion + curated May-2026 probability snapshot.

   The discussion HTML has no clean machine-readable probability table;
   we cache the page for the notebook to display and bundle a curated
   snapshot of the May 2026 ENSO-Watch probability bars so the project
   can render the headline figure even when offline.
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "fetcher_common.wl"}]];

urlDisc   = "https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/enso_advisory/ensodisc.shtml";
rawHtml   = FileNameJoin[{ENSOFetch`RawDir[], "ensodisc.shtml"}];
outJson   = FileNameJoin[{ENSOFetch`DataDir[], "cpc_probabilities_snapshot.json"}];

ENSOFetch`Banner["CPC ENSO Diagnostic Discussion + probability snapshot"];

res = ENSOFetch`FetchText[urlDisc, rawHtml, 6];
If[res["ok"],
   Print["  discussion: ", res["savedTo"], "  (", res["bytes"], " B, cached=", res["cached"], ")"],
   Print["  discussion: FAILED — ", res["error"]]
];

snapshot = <|
   "issued"   -> "2026-05",
   "headline" -> "El Niño Watch in effect: ~82% chance of El Niño onset during May–July 2026, ~96% chance of persistence through December 2026 – February 2027.",
   "source"   -> "NOAA CPC / IRI ENSO Diagnostic Discussion (May 2026 issuance)",
   "probabilities" -> {
     <|"season" -> "MJJ 2026", "la_nina" -> 0, "neutral" -> 18, "el_nino" -> 82|>,
     <|"season" -> "JJA 2026", "la_nina" -> 0, "neutral" -> 9,  "el_nino" -> 91|>,
     <|"season" -> "JAS 2026", "la_nina" -> 0, "neutral" -> 6,  "el_nino" -> 94|>,
     <|"season" -> "ASO 2026", "la_nina" -> 0, "neutral" -> 5,  "el_nino" -> 95|>,
     <|"season" -> "SON 2026", "la_nina" -> 0, "neutral" -> 4,  "el_nino" -> 96|>,
     <|"season" -> "OND 2026", "la_nina" -> 0, "neutral" -> 4,  "el_nino" -> 96|>,
     <|"season" -> "NDJ 2026", "la_nina" -> 0, "neutral" -> 4,  "el_nino" -> 96|>,
     <|"season" -> "DJF 2027", "la_nina" -> 1, "neutral" -> 4,  "el_nino" -> 95|>,
     <|"season" -> "JFM 2027", "la_nina" -> 3, "neutral" -> 9,  "el_nino" -> 88|>
   },
   "fetched_at" -> DateString[Now, "ISODateTime"]
|>;

Export[outJson, snapshot, "JSON"];
Print["  snapshot: ", outJson];
