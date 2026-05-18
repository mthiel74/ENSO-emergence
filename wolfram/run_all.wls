#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  One-shot driver for the analysis pipeline. Runs (in order):

       1. observational spring-barrier figures
       2. recharge-discharge oscillator ensembles
       3. Suarez-Schopf delayed oscillator ensembles
       4. live forecast overlay using the most recent observed state

    Each step is a separate wolframscript invocation so a failure in
    one stage does not corrupt the kernel state of the next.

    Usage:
      wolframscript -file wolfram/fetch_all.wls   (* first, refresh data *)
      wolframscript -file wolfram/run_all.wls     (* then this *)
*)

dir = DirectoryName[$InputFileName];

scripts = {
   "barrier.wls",
   "rdo.wls",
   "delayed.wls",
   "forecast.wls",
   "hero.wls"
};

Do[
   Module[{path = FileNameJoin[{dir, s}], proc},
      Print["\n>>> running ", s];
      proc = RunProcess[{"wolframscript", "-file", path},
         ProcessEnvironment -> Inherited];
      If[proc["ExitCode"] =!= 0,
         Print["FAIL (exit ", proc["ExitCode"], "): ", s];
         Print[proc["StandardError"]];
         Exit[proc["ExitCode"]]
      ];
      Print[StringTrim @ proc["StandardOutput"]]
   ],
   {s, scripts}
];

Print["\n=== all analysis scripts completed ==="];
