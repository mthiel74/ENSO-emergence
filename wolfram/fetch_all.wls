#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Run every Wolfram-Language live-data fetcher in one go.

   Usage:  wolframscript -file wolfram/fetch_all.wls
*)

dir = DirectoryName[$InputFileName];
fetchers = {
   "fetch_nino34.wls",
   "fetch_oni.wls",
   "fetch_iri_plume.wls",
   "fetch_cpc_probabilities.wls",
   "fetch_ersst_grid.wls"
};

Do[
   With[{path = FileNameJoin[{dir, f}]},
     Print["\n>>> running ", f];
     Get[path]
   ],
   {f, fetchers}
];
