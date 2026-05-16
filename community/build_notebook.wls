#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Build community/enso_emergence.nb from prose, equations, and the
   PNGs rendered by the wolfram/*.wls pipeline.

   Run AFTER:
     wolframscript -file wolfram/fetch_all.wls
     wolframscript -file wolfram/barrier.wls
     wolframscript -file wolfram/rdo.wls
     wolframscript -file wolfram/delayed.wls
     wolframscript -file wolfram/forecast.wls

   The notebook layout / style mirrors streetview360's community post.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
outNb    = FileNameJoin[{repoRoot, "community", "enso_emergence.nb"}];
outPdf   = FileNameJoin[{repoRoot, "community", "enso_emergence.pdf"}];

(* ---- small helpers ----------------------------------------------- *)

img[name_, caption_:""] := With[
   {path = FileNameJoin[{imgDir, name}]},
   If[FileExistsQ[path],
     Cell[BoxData @ ToBoxes @ Show[Import[path], ImageSize -> 700],
        "Output", CellMargins -> {{50, 10}, {6, 6}}],
     Cell["[ missing figure: " <> name <> " ]", "Output",
        FontColor -> RGBColor[0.7, 0.2, 0.2], FontWeight -> Bold]
   ]
];

caption[txt_] := Cell[
   TextData[{Style[txt, "Caption", FontFamily -> "Helvetica",
       FontSlant -> Italic, FontSize -> 11]}],
   "Caption", CellMargins -> {{60, 60}, {2, 14}}
];

para[txt_] := Cell[TextData[txt], "Text"];

hd1[txt_] := Cell[txt, "Section"];
hd2[txt_] := Cell[txt, "Subsection"];

eqn[boxes_] := Cell[BoxData @ boxes, "DisplayFormula",
   CellMargins -> {{80, 50}, {8, 8}}];

wlIn[code_String] := Cell[BoxData @ code, "Input",
   CellMargins -> {{60, 30}, {6, 6}},
   CellLabel -> "In[]:="];

(* ---- assemble the notebook -------------------------------------- *)

nbCells = {

Cell[CellGroupData[{
Cell["ENSO emergence and the spring predictability barrier",
   "Title"],
Cell["A live-data project on the May 2026 El Ni\[NTilde]o Watch and the \
classical toy models that explain why springtime ENSO forecasts collapse.",
   "Subtitle"],
Cell["Wolfram Community submission \[LongDash] May 2026",
   "Subtitle", FontSlant -> Italic],

(* ===================== section 1 =================================== *)

hd1["1.  The El Ni\[NTilde]o Watch (May 2026)"],

para[{"NOAA's Climate Prediction Center currently has an ",
   StyleBox["El Ni\[NTilde]o Watch", FontWeight -> Bold],
   " in effect. The May 2026 ENSO Diagnostic Discussion gives an ",
   StyleBox["~82% probability", FontWeight -> Bold],
   " of El Ni\[NTilde]o onset during May\[Dash]July 2026 and a ",
   StyleBox["~96% probability", FontWeight -> Bold],
   " that the event persists through December 2026\[Dash]February 2027. \
The tropical Pacific has just emerged from a weak La Ni\[NTilde]a and \
the equatorial cold tongue is warming sharply."
}],

img["nino34_record.png"],
caption["Monthly Ni\[NTilde]o 3.4 SST anomaly from NOAA PSL (ERSSTv5, \
base period 1991\[Dash]2020). The April 2026 value is +0.23 \[Degree]C, \
up from -0.67 \[Degree]C in December 2025 \[LongDash] one of the \
sharpest spring transitions in the record."],

para[{"The figure below is the latest IRI / CPC forecast plume, the \
canonical visualisation of the operational ENSO outlook. Each line is \
one dynamical or statistical model; the spread of the cloud is what \
forecasters call the \[OpenCurlyDoubleQuote]plume\[CloseCurlyDoubleQuote]."
}],

img["forecast_overlay.png"],
caption["Top: IRI / CPC plume (latest issue). Middle: toy-model \
ensemble cone from the recharge\[Dash]discharge oscillator implemented in \
this notebook, initialised from the same observed state. Bottom: \
NOAA CPC official 3-class probability bars for the next nine rolling \
3-month seasons."],

(* ===================== section 2 =================================== *)

hd1["2.  The spring predictability barrier"],

para[{"ENSO forecasts initialised in boreal winter and spring are \
notoriously unreliable. The phenomenon is called the ",
   StyleBox["spring predictability barrier", FontWeight -> Bold],
   ". It is not a feature of any single forecast model; it shows up \
directly in the observational record."
}],

para[{"For each calendar month ",
   StyleBox["m\:2080", FontSlant -> Italic],
   " and each lead \[Tau], we compute the lagged Pearson correlation of \
the Ni\[NTilde]o 3.4 anomaly:"
}],

eqn[RowBox[{"\[Rho]", "(", "m\:2080", ",", "\[Tau]", ")", "=", "corr",
   "(", RowBox[{"T", "(", "y", ",", "m\:2080", ")"}], ",",
   RowBox[{"T", "(", "y", "\[Prime]", ",",
      RowBox[{"m\:2080", "+", "\[Tau]"}], ")"}], ")"}]],

para[{"with the verification time shifted to the next year whenever ",
  StyleBox["m\:2080 + \[Tau]", FontSlant -> Italic],
  " > 12. The result is a striking V-shaped valley pointing into the \
April\[Dash]June verification window:"
}],

img["spring_barrier_obs.png"],
caption["Observed Ni\[NTilde]o 3.4 autocorrelation \[Rho](m\:2080, lead) \
since 1950. Whatever the start month, forecasts whose verification time \
crosses April\[Dash]June collapse to near-zero skill. The right-hand \
column (Aug\[Dash]Oct starts) keeps high \[Rho] all the way into next \
year's DJF event peak."],

img["persistence_drop.png"],
caption["The same data, sliced as persistence-skill curves vs lead. \
Jan/Feb starts (purple/blue, top of legend) drop fastest; Jul/Aug \
starts (red/orange) keep \[Rho] > 0.7 out to 9-month leads."],

(* ===================== section 3 =================================== *)

hd1["3.  Two toy models that produce the barrier"],

hd2["3.1  The recharge\[Dash]discharge oscillator (Jin 1997)"],

para[{"Jin's recharge\[Dash]discharge oscillator is the simplest \
2-dimensional model that captures the ENSO cycle. It tracks the \
eastern equatorial SST anomaly ",
   StyleBox["T", FontSlant -> Italic],
   " and the western equatorial thermocline depth anomaly ",
   StyleBox["h", FontSlant -> Italic], ":"
}],

eqn[RowBox[{
  RowBox[{FractionBox["dT","dt"], "=",
    RowBox[{"R", "(", "t", ")", "T"}], "+",
    RowBox[{"\[Gamma]", " ", "h"}], "+",
    SubscriptBox["\[Sigma]", "T"],
    SubscriptBox["\[Xi]", "T"], "(", "t", ")", "+",
    RowBox[{SubscriptBox["\[Sigma]", "w"], "(", "t", ")",
      SubscriptBox["\[Xi]", "w"], "(", "t", ")"}]
  }]
}]],

eqn[RowBox[{
  RowBox[{FractionBox["dh","dt"], "=",
    RowBox[{"-", "r", " ", "h"}], "-",
    RowBox[{"\[Alpha]", " ", "T"}], "+",
    SubscriptBox["\[Sigma]", "h"], SubscriptBox["\[Xi]", "h"], "(", "t", ")"
  }]
}]],

para[{"The Bjerknes feedback ", StyleBox["R(t)", FontSlant -> Italic],
   " is positive (growth) in boreal autumn and negative (damping) in \
boreal spring, when the climatological trades are weakest. The \
stochastic westerly-wind-burst (WWB) variance ",
   StyleBox["\[Sigma]\_w(t)", FontSlant -> Italic],
   " peaks in March\[Dash]April. This is the Stein \[Dash] Levine \[Dash] \
Jin mechanism for the spring barrier."
}],

img["rdo_seasonality.png"],
caption["The two seasonally varying coefficients used in the toy \
model. R(t) is negative through spring and positive through autumn; \
\[Sigma]\_w(t) is largest in early spring when WWBs are observed to \
peak."],

para[{"Integrating with Euler\[Dash]Maruyama (dt = 0.05 months) gives a \
realistic ENSO time series \[LongDash] interannual oscillations of \
\[PlusMinus]2 \[Degree]C, irregular in amplitude, period ~3\[Dash]6 \
years:"
}],

img["rdo_timeseries.png"],
caption["A 20-year realisation of the recharge\[Dash]discharge \
oscillator with stochastic forcing."],

img["rdo_phase.png"],
caption["Phase-space portrait (T vs h). The cycle is counter-clockwise \
\[LongDash] anomalous warming charges the thermocline; the thermocline \
then discharges back to neutral over ~4 years."],

para[{"Now we initialise 500-member ensembles in four different \
calendar months. The same model, the same parameters, the same \
random-number machinery \[LongDash] only the initial calendar phase \
differs. The cone of uncertainty breathes with the seasonal cycle:"
}],

img["rdo_barrier_cone.png"],
caption["Ensemble forecast cones from the recharge\[Dash]discharge \
oscillator, 500 members each, initialised in Jan / Apr / Jul / Oct \
respectively. The cone widens fastest when the verification window \
crosses the spring barrier."],

img["rdo_spread_heatmap.png"],
caption["Ensemble standard deviation as a function of initial month \
m\:2080 and lead. The brightest bands align with verification months \
around the model's growth season."],

hd2["3.2  The delayed-action oscillator (Suarez & Schopf 1988)"],

para[{"The delay model captures the same physics differently. It \
encodes the cross-basin ocean-wave round trip as an explicit delay ",
   StyleBox["\[Delta]", FontSlant -> Italic],
   " (~6 months) on a single scalar SST anomaly:"
}],

eqn[RowBox[{
   FractionBox["dT", "dt"], "=",
   "T", "-",
   RowBox[{"\[Alpha]", " ",
     RowBox[{"T", "(", RowBox[{"t", "-", "\[Delta]"}], ")"}]}],
   "-", SuperscriptBox["T", "3"], "+",
   RowBox[{"\[Sigma]", "(", "t", ")", " ", "\[Xi]", "(", "t", ")"}]
}]],

para[{"We integrate with Euler\[Dash]Maruyama on a circular history \
buffer so the delayed value is available without recomputation:"
}],

img["delayed_timeseries.png"],
caption["A 20-year realisation of the Suarez\[Dash]Schopf delayed \
oscillator (\[Alpha] = 0.75, \[Delta] = 6 months). Sustained \
nonlinear limit cycle with stochastic perturbations."],

img["delayed_bifurcation.png"],
caption["Amplitude (min/max of T) as a function of the wave-coupling \
strength \[Alpha]. The Hopf bifurcation around \[Alpha] \[TildeTilde] \
0.5 marks the transition from damped fixed-point behaviour to \
sustained oscillation."],

img["delayed_cone.png"],
caption["Ensemble forecast cones from the delayed oscillator. The \
spread \[OpenCurlyDoubleQuote]breathes\[CloseCurlyDoubleQuote] with the \
underlying limit cycle period rather than collapsing through a single \
barrier window."],

(* ===================== section 4 =================================== *)

hd1["4.  Live overlay: where the toy meets reality"],

para[{"Putting it all together: we take the most recent observed \
Ni\[NTilde]o 3.4 anomaly (April 2026, +0.23 \[Degree]C), infer the \
toy-model thermocline state ",
   StyleBox["h", FontSlant -> Italic],
   " from the recent dT/dt, and run a 500-member ensemble from there. \
The cone is overlaid on the IRI / CPC plume and the NOAA probability \
bars from the May 2026 ENSO Watch."
}],

img["rdo_live_forecast.png"],
caption["The toy-model ensemble forecast, initialised from observed \
April 2026 state. The forecast median peaks around DJF 2026\[Dash]27, \
consistent with the official NOAA 96% probability of an El Ni\[NTilde]o \
peaking in that season. The width of the cone is the toy-model \
analogue of the IRI plume spread."],

img["noaa_probabilities.png"],
caption["The May 2026 official NOAA / IRI probability bars: red = \
El Ni\[NTilde]o, grey = neutral, blue = La Ni\[NTilde]a, summed to \
100 % in each rolling 3-month season."],

(* ===================== section 5 =================================== *)

hd1["5.  Reproducibility \[LongDash] code and live data"],

para[{"All code is plain Wolfram Language. The pipeline:"}],

wlIn["wolframscript -file wolfram/fetch_all.wls   (* live data *)"],
wlIn["wolframscript -file wolfram/barrier.wls     (* observed barrier *)"],
wlIn["wolframscript -file wolfram/rdo.wls         (* RDO ensembles *)"],
wlIn["wolframscript -file wolfram/delayed.wls     (* DDE ensembles *)"],
wlIn["wolframscript -file wolfram/forecast.wls    (* live overlay *)"],
wlIn["wolframscript -file community/build_notebook.wls  (* this nb *)"],

para[{"Live data sources used:"}],

para[{"\[Bullet] ", StyleBox["Ni\[NTilde]o 3.4 monthly SST anomalies",
   FontWeight -> Bold], " \[LongDash] NOAA PSL ERSSTv5 (base period \
1991\[Dash]2020): ", Hyperlink["psl.noaa.gov/data/correlation/nina34.anom.data",
   "https://psl.noaa.gov/data/correlation/nina34.anom.data"]}],

para[{"\[Bullet] ", StyleBox["Oceanic Ni\[NTilde]o Index (3-mo running mean)",
   FontWeight -> Bold], " \[LongDash] NOAA CPC: ",
   Hyperlink["cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",
     "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"]}],

para[{"\[Bullet] ", StyleBox["IRI / CPC ENSO forecast plume",
   FontWeight -> Bold], " \[LongDash] ",
   Hyperlink["ensoforecast.iri.columbia.edu",
     "https://ensoforecast.iri.columbia.edu/"]}],

para[{"\[Bullet] ", StyleBox["CPC ENSO Diagnostic Discussion",
   FontWeight -> Bold], " \[LongDash] ",
   Hyperlink["cpc.ncep.noaa.gov ENSO Advisory",
     "https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/enso_advisory/ensodisc.shtml"]}],

para[{"Project repository: ", Hyperlink[
   "github.com/mthiel74/ENSO-emergence",
   "https://github.com/mthiel74/ENSO-emergence"]}],

para[{StyleBox["Companion projects:", FontWeight -> Bold]}],
para[{"\[Bullet] ", Hyperlink["Contiguous-Cartograms",
   "https://github.com/mthiel74/Contiguous-Cartograms"], " \[Dash] \
Gastner\[Dash]Newman density-equalising cartograms in Python."}],
para[{"\[Bullet] ", Hyperlink["streetview360",
   "https://github.com/mthiel74/streetview360"], " \[Dash] panoramic \
viewer paclet for Wolfram Language."}],
para[{"\[Bullet] ", Hyperlink["SofaProblem",
   "https://github.com/mthiel74/SofaProblem"], " \[Dash] numerical \
attack on the moving-sofa problem."}],

(* ===================== references =================================== *)

hd1["References"],

para[{"\[Bullet] Jin, F.-F. (1997). An equatorial ocean recharge \
paradigm for ENSO. Part I: Conceptual model. ",
   StyleBox["J. Atmos. Sci.", FontSlant -> Italic], " 54, 811\[Dash]829."}],
para[{"\[Bullet] Suarez, M. J., & Schopf, P. S. (1988). A delayed \
action oscillator for ENSO. ",
   StyleBox["J. Atmos. Sci.", FontSlant -> Italic], " 45, 3283\[Dash]3287."}],
para[{"\[Bullet] Burgers, G., Jin, F.-F., & van Oldenborgh, G. J. \
(2005). The simplest ENSO recharge oscillator. ",
   StyleBox["Geophys. Res. Lett.", FontSlant -> Italic], " 32, L13706."}],
para[{"\[Bullet] Levine, A. F. Z., & Jin, F.-F. (2010). Noise-induced \
instability in the ENSO recharge oscillator. ",
   StyleBox["J. Atmos. Sci.", FontSlant -> Italic], " 67, 529\[Dash]542."}],
para[{"\[Bullet] Stein, K., Schneider, N., Timmermann, A., & Jin, \
F.-F. (2010). Seasonal synchronization of ENSO events in a linear \
stochastic model. ",
   StyleBox["J. Climate", FontSlant -> Italic], " 23, 5629\[Dash]5643."}],
para[{"\[Bullet] Webster, P. J., & Yang, S. (1992). Monsoon and ENSO: \
Selectively interactive systems. ",
   StyleBox["Q. J. R. Meteorol. Soc.", FontSlant -> Italic], " 118, \
877\[Dash]926."}],
para[{"\[Bullet] Torrence, C., & Webster, P. J. (1998). The annual \
cycle of persistence in the El Ni\[NTilde]o/Southern Oscillation. ",
   StyleBox["Q. J. R. Meteorol. Soc.", FontSlant -> Italic], " 124, \
1985\[Dash]2004."}]
}, OpenAllGroups]]};

(* ---- save the notebook (needs the Wolfram FrontEnd) ------------- *)

nb = Notebook[
   nbCells,
   StyleDefinitions -> "Default.nb",
   WindowSize -> {980, Automatic},
   WindowMargins -> {{Automatic, 0}, {Automatic, 0}}
];

UsingFrontEnd[
   nbObj = CreateDocument[nb, Visible -> False];
   NotebookSave[nbObj, outNb];
   Print["wrote ", outNb];
   Quiet @ Check[
      Export[outPdf, nbObj];
      Print["wrote ", outPdf],
      Print["warn: PDF export failed"]
   ];
   NotebookClose[nbObj];
];
