#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Fetch the most recent N months of ERSSTv5 monthly gridded NetCDF
    files from NOAA NCEI. Each file is ~170 KB and contains BOTH the
    raw SST field (`sst`) AND the anomaly (`ssta`, vs the 1971-2000
    climatology embedded by NCEI).

    Source:
      https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/ersst.v5.YYYYMM.nc

    Variables per file (CF-1.6 compliant):
      lat[89]   -88..88   2-deg spacing
      lon[180]    0..358  2-deg spacing
      sst[1,1,89,180]    surface temperature (degC)
      ssta[1,1,89,180]   anomaly vs 1971-2000 climatology (degC)

    Output:
      data/raw/ersst.v5.YYYYMM.nc   for each month fetched
      data/ersst_grid_manifest.json with which months are cached
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "fetcher_common.wl"}]];

monthsBack = 15;  (* how many trailing months to grab *)

today = Take[DateList[], 3];
yr = today[[1]]; mo = today[[2]];

monthSeq = NestList[
   With[{y = #[[1]], m = #[[2]] - 1},
      If[m == 0, {y - 1, 12}, {y, m}]] &,
   {yr, mo}, monthsBack];

urlOf[{y_, m_}] := StringTemplate[
   "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/ersst.v5.`y``mm`.nc"
][<|"y" -> y, "mm" -> StringPadLeft[ToString[m], 2, "0"]|>];

pathOf[{y_, m_}] := FileNameJoin[{ENSOFetch`RawDir[],
   "ersst.v5." <> ToString[y] <> StringPadLeft[ToString[m], 2, "0"] <> ".nc"}];

ENSOFetch`Banner["ERSSTv5 monthly gridded NetCDF (" <> ToString[monthsBack] <>
   " months)"];

cached = {};
fetched = {};
failed = {};
Do[
   With[{ym = monthSeq[[i]], url = urlOf[monthSeq[[i]]],
         dest = pathOf[monthSeq[[i]]]},
     res = ENSOFetch`FetchBytes[url, dest, 24*30];   (* 30-day cache *)
     Which[
        ! res["ok"],
           AppendTo[failed, ym];
           Print["  ", ym, " : FAIL ", res["error"]],
        res["cached"],
           AppendTo[cached, ym],
        True,
           AppendTo[fetched, ym];
           Print["  ", ym, " : fetched (", res["bytes"], " B)"]
     ]
   ], {i, Length[monthSeq]}
];

Print["  summary: ", Length[fetched], " fetched, ",
   Length[cached], " cached, ", Length[failed], " failed"];

manifestPath = FileNameJoin[{ENSOFetch`DataDir[], "ersst_grid_manifest.json"}];
Export[manifestPath, <|
   "fetched_at" -> DateString[Now, "ISODateTime"],
   "months"     -> (StringJoin[ToString[#[[1]]], "-",
                      StringPadLeft[ToString[#[[2]]], 2, "0"]] & /@ monthSeq),
   "fetched"    -> Length[fetched],
   "cached"     -> Length[cached],
   "failed"     -> Length[failed]
|>, "JSON"];
Print["  manifest: ", manifestPath];
