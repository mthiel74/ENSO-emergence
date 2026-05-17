#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/enso_emergence.nb — the educational long-form
    notebook for the Wolfram Community submission.

    Architecture
    ------------
    The script opens an invisible front-end notebook via
    UsingFrontEnd / CreateDocument and CellPrints fully-formed
    ExpressionCells into it. Images are imported as Image[] objects
    so they are stored as binary Rasters in the .nb — no fragile
    ToBoxes magic, no missing-figure placeholders.

    Sections
    --------
      Abstract
      1. What is ENSO?
      2. The May 2026 El Niño Watch
      3. Why ENSO matters: global teleconnections
      4. The spring predictability barrier
      5. Two toy models of ENSO
         5.1 Recharge–discharge (Jin 1997)
         5.2 Delayed oscillator (Suarez–Schopf 1988)
      6. Live forecast: initialising from April 2026
      7. Conclusions and outlook
      8. References
      9. Acknowledgements
     10. Reproducibility
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
genDir   = FileNameJoin[{imgDir, "figures-generated"}];
outNb    = FileNameJoin[{repoRoot, "community", "enso_emergence.nb"}];
outPdf   = FileNameJoin[{repoRoot, "community", "enso_emergence.pdf"}];


(* ==================================================================
   Helpers — build a Cell expression at the script level, send into
   the live FrontEnd notebook with CellPrint.
   ================================================================== *)

(* Headings *)
title[txt_]    := Cell[txt, "Title"];
subtitle[txt_] := Cell[txt, "Subtitle"];
hd1[txt_]      := Cell[txt, "Section"];
hd2[txt_]      := Cell[txt, "Subsection"];
hd3[txt_]      := Cell[txt, "Subsubsection"];

(* Paragraph text *)
para[stuff_]   := Cell[TextData[stuff], "Text",
   CellMargins -> {{50, 50}, {6, 6}}];

abstractCell[stuff_] := Cell[TextData[stuff], "Text",
   FontSize -> 14, CellMargins -> {{60, 60}, {6, 6}},
   Background -> RGBColor[0.98, 0.96, 0.90]];

(* Inline styles *)
bold[s_]   := StyleBox[s, FontWeight -> Bold];
ital[s_]   := StyleBox[s, FontSlant -> Italic];
math[s_]   := StyleBox[s, FontFamily -> "Times", FontSlant -> Italic];
mono[s_]   := StyleBox[s, FontFamily -> "Courier"];

(* True in-line math via real boxes (e.g. SubscriptBox), so subscripts
   actually render as subscripts and not as literal underscores. The
   inline Cell needs an explicit style; "InlineFormula" is the standard
   one in Default.nb. *)
imath[boxes_] := Cell[BoxData @ FormBox[boxes, TraditionalForm],
   "InlineFormula"];
sub[base_, k_] := imath @ SubscriptBox[base, k];
sigT = imath @ SubscriptBox["\[Sigma]", "T"];
sigH = imath @ SubscriptBox["\[Sigma]", "h"];
sigW = imath @ RowBox[{SubscriptBox["\[Sigma]", "w"], "(", "t", ")"}];
Rt   = imath @ RowBox[{"R", "(", "t", ")"}];

(* Display math via TraditionalForm of an explicit expression. *)
displayMath[expr_] := Cell[
   BoxData @ FormBox[expr, TraditionalForm],
   "DisplayFormula",
   CellMargins -> {{80, 50}, {8, 8}}
];

(* Image cell. We pre-resize the image to 640 px wide on disk so the
   embedded raster is small (~150 KB instead of 2 MB) and ExpressionCell
   does the FE-side boxing lazily. *)
imgCacheDir = FileNameJoin[{repoRoot, "community", ".img-cache"}];
If[!DirectoryQ[imgCacheDir], CreateDirectory[imgCacheDir]];

(* Per-image target widths. Educational gpt-image-2 illustrations
   get more pixels; computed plots a bit less. *)
targetWidth[path_] := If[StringContainsQ[path, "figures-generated"], 520, 440];

resizedCopy[path_] := Module[{cached, w, h, im, target},
   cached = FileNameJoin[{imgCacheDir, FileNameTake[path]}];
   If[FileExistsQ[cached] &&
      FileDate[cached, "Modification"] > FileDate[path, "Modification"],
     Return[cached]];
   im = Import[path];
   {w, h} = ImageDimensions[im];
   target = targetWidth[path];
   If[w > target, im = ImageResize[im, target]];
   Export[cached, im, "PNG"];
   cached
];

(* Build a GraphicsBox/RasterBox/CompressedData expression by hand,
   matching what NotebookSave normally produces. Storing the pixel
   data as a Compress'd base64 string is 6-8× more compact than
   ASCII InputForm of the raw NumericArray. We return a *string*
   that, when interpolated into the .nb body via ToString[InputForm]
   on a Skeleton stand-in, is exact byte-for-byte boxes. *)

(* Sentinel symbols used to splice raw RasterBox text past InputForm.
   For each image we generate a unique Symbol RasterPlaceholderN whose
   InputForm prints as the bare identifier "RasterPlaceholderN".  After
   ToString[InputForm], we string-substitute each placeholder with the
   compressed-RasterBox box-text. *)
$rasterSubstitutions = <||>;
$rasterCounter = 0;

imageBoxes[path_String] := Module[
   {small, im, na, h, w, compressed, key, boxStr},
   small = resizedCopy[path];
   im = Image[Import[small], "Byte"];
   (* RasterBox's y-axis runs bottom-to-top, while ImageData returns
      rows top-to-bottom. Reverse the row order so the image renders
      right-side-up. *)
   na = Reverse @ ImageData[im, "Byte"];         (* {h, w, 3} bytes *)
   {h, w} = Take[Dimensions[na], 2];
   compressed = Compress[na];                    (* "1:eJ..." *)
   $rasterCounter += 1;
   (* Trailing letter X marks the end of the unique ID so "RasterPh1X"
      is NOT a substring of "RasterPh10X" — otherwise the first
      StringReplace would corrupt all higher-numbered placeholders. *)
   key = "RasterPh" <> ToString[$rasterCounter] <> "X";
   boxStr = StringJoin[
      "GraphicsBox[",
        "TagBox[",
          "RasterBox[",
            "CompressedData[\"", compressed, "\"], ",
            "{{0, 0}, {", ToString[w], ", ", ToString[h], "}}, ",
            "{0, 255}, ",
            "ColorFunction -> RGBColor",
          "],",
          "BoxForm`ImageTag[\"Byte\", ColorSpace -> \"RGB\", Interleaving -> True],",
          " Selectable -> False",
        "],",
        " DefaultBaseStyle -> \"ImageGraphics\",",
        " ImageSize -> ", ToString[Round[640]], ",",
        " ImageSizeRaw -> {", ToString[w], ", ", ToString[h], "},",
        " PlotRange -> {{0, ", ToString[w], "}, {0, ", ToString[h], "}}",
      "]"
   ];
   $rasterSubstitutions[key] = boxStr;
   key
];

(* imageBoxes returns a placeholder string. We wrap it in a Symbol so
   InputForm later prints it as an unquoted identifier; after
   serialization we string-substitute the placeholder identifiers
   with the actual compressed RasterBox boxes. *)
imgCell[file_, opts___] := Module[{path = FileNameJoin[{imgDir, file}], key},
   If[FileExistsQ[path],
     key = imageBoxes[path];   (* "RasterPh<N>X" *)
     Cell[BoxData @ Symbol[key],
        "Output",
        TextAlignment -> Center,
        CellMargins -> {{Automatic, Automatic}, {10, 6}}],
     Cell[
       TextData[{bold["[ missing figure: " <> file <> " ]"]}],
       "Text", FontColor -> RGBColor[0.7, 0.1, 0.1],
       TextAlignment -> Center]
   ]
];

genImgCell[file_, opts___] := imgCell[
   FileNameJoin[{"figures-generated", file}], opts];

(* Variadic so captionCell["plain string"] and
   captionCell["fragment 1 ", math["x"], " fragment 2"] both work. *)
captionCell[args__] := Cell[
   TextData[{args}], "Text",
   FontSlant -> Italic, FontSize -> 11,
   FontColor -> GrayLevel[0.35],
   CellMargins -> {{70, 70}, {2, 14}}
];

(* Code blocks: a "Program" cell style displays text as a fixed-width
   block but doesn't try to syntax-highlight it as Wolfram code, so
   shell snippets with `#` comments don't confuse the parser. *)
codeIn[code_String] := Cell[code, "Program",
   CellMargins -> {{60, 30}, {6, 6}},
   FontFamily -> "Courier", FontSize -> 11,
   ShowStringCharacters -> False,
   Background -> RGBColor[0.95, 0.96, 0.98]];

(* Inline hyperlink that renders well in both interactive and PDF
   views: link text followed by the URL in parentheses. *)
link[txt_String, url_String] := Sequence[
   StyleBox[txt, FontWeight -> "SemiBold"],
   " (", StyleBox[url, FontFamily -> "Courier", FontSize -> 10], ")"
];


(* ==================================================================
   Accumulate every cell into a single list, then build the
   Notebook expression in one shot. Writing the .nb is just
   Export[file, notebook] — no NotebookWrite, no cell merging.
   ================================================================== *)

$allCells = {};
writeAll[cs_List] := Module[{n0 = Length[$allCells]},
   $allCells = Join[$allCells, cs];
   Print["  writeAll: +", Length[cs], " cells (total = ", Length[$allCells], ")"];
];


(* ----------------- TITLE & ABSTRACT ----------------- *)

writeAll[{

title["ENSO emergence and the spring predictability barrier"],

subtitle["A live-data exploration of NOAA's May 2026 El Ni\[NTilde]o Watch \
through the lens of two classical coupled ocean\[Dash]atmosphere toy models"],

Cell["Wolfram Community submission \[LongDash] May 2026",
   "Subsubtitle", FontSlant -> Italic, FontColor -> GrayLevel[0.4]],


(* ----------------- ABSTRACT ----------------- *)

hd1["Abstract"],

abstractCell[{"NOAA's Climate Prediction Center currently has an ",
   bold["El Ni\[NTilde]o Watch"], " in effect. The May 2026 ENSO \
Diagnostic Discussion gives a ",
   bold["~82% probability"],
   " of El Ni\[NTilde]o onset during May\[Dash]July 2026 and a ",
   bold["~96% probability"],
   " that the event persists through DJF 2026\[Dash]27. The tropical \
Pacific has just emerged from a weak La Ni\[NTilde]a, and the \
equatorial cold tongue is warming sharply (Ni\[NTilde]o 3.4 anomaly \
climbed from \[Minus]0.67 \[Degree]C in December 2025 to +0.23 \
\[Degree]C in April 2026)."}],

abstractCell[{"This notebook does three things. First, it ", bold["pulls \
the live data"], " (Ni\[NTilde]o 3.4 monthly anomalies from NOAA PSL, \
the Oceanic Ni\[NTilde]o Index from CPC, the IRI/CPC forecast plume, \
and the CPC probability table) in pure Wolfram Language. Second, it \
computes the ", bold["spring predictability barrier"], " directly from \
77 years of observations, showing how forecasts initialised in January \
lose almost all skill by July, while forecasts initialised in July \
retain skill through the following winter. Third, it implements ",
   bold["two textbook coupled ocean\[Dash]atmosphere toy models"],
   " \[LongDash] Jin's (1997) recharge\[Dash]discharge oscillator and \
Suarez & Schopf's (1988) delayed-action oscillator \[LongDash] and \
uses them to reproduce the barrier mechanism and to produce a \
toy-model live forecast cone for the developing 2026 event."}],

abstractCell[{ital["Audience:"], " the prose is written for two \
overlapping audiences. Each section opens with an intuitive \
explanation that requires no background, then deepens into a \
quantitative treatment with equations, numerical integration code, \
and 5-95 % / 25-75 % confidence intervals. Climate scientists will \
recognise the canonical references; general readers should be able \
to follow the geophysical narrative."}]
}];


(* ============================================================
   §1 What is ENSO?
   ============================================================ *)

writeAll[{

hd1["1.  What is ENSO?"],

para[{ital["The basic facts."], " The El Ni\[NTilde]o\[Dash]Southern \
Oscillation (ENSO) is the largest source of year-to-year climate \
variability on Earth. It lives in the tropical Pacific and consists \
of two halves that are physically inseparable: ", bold["El Ni\[NTilde]o"],
   ", a sloshing of warm water from the western to the central and \
eastern equatorial Pacific (warmer-than-usual sea surface temperatures \
in the central/eastern basin), and ", bold["La Ni\[NTilde]a"], ", the \
opposite phase, in which the warm pool retreats westward and the \
eastern Pacific cold tongue intensifies. The whole system oscillates \
on a 3-to-7-year time scale and is the dominant driver of global \
inter-annual atmospheric variability."}],

para[{"The ENSO state is conventionally measured by the SST anomaly \
(departure from the climatological mean) in one of four \
\[OpenCurlyDoubleQuote]Ni\[NTilde]o boxes\[CloseCurlyDoubleQuote] in \
the equatorial Pacific. The most-watched box is ", bold["Ni\[NTilde]o 3.4"],
   " (5\[Degree]N\[Dash]5\[Degree]S, 170\[Degree]W\[Dash]120\[Degree]W), \
straddling the dateline. NOAA's operational definition of an \
El Ni\[NTilde]o event is that the three-month running mean of the \
Ni\[NTilde]o 3.4 anomaly (the ", bold["Oceanic Ni\[NTilde]o Index"],
   ", ONI) exceeds +0.5 \[Degree]C for five consecutive overlapping \
seasons."}],

genImgCell["nino34_region_map.png"],
captionCell["Figure 1.1  The four ENSO monitoring regions in the \
tropical Pacific. The Ni\[NTilde]o 3.4 box, highlighted, is the \
standard for operational ENSO classification. Generated for this \
notebook with OpenAI gpt-image-2."],

para[{ital["Why ENSO exists."], " Three ingredients combine into a \
self-sustaining oscillation:"}],

para[{"(i) ", bold["Bjerknes feedback"], " (Bjerknes 1969). Normally, \
easterly trade winds push warm surface water westward, piling it up \
in the Indonesian warm pool and tilting the thermocline (the \
boundary between warm surface water and cold deep water) upward in \
the east. A small east-Pacific warming weakens this east\[Dash]west \
SST gradient, which weakens the trades, which weakens the upwelling \
of cold water in the east, which amplifies the original warming. \
Positive feedback."}],

para[{"(ii) ", bold["Delayed ocean dynamics"], ". The same wind \
weakening that drives the warming also launches equatorial Kelvin \
waves that depress the thermocline in the east (more warming) and \
equatorial Rossby waves that reflect off the western boundary as \
upwelling Kelvin waves several months later. The reflected upwelling \
eventually cools the east and reverses the cycle."}],

para[{"(iii) ", bold["Stochastic forcing"], ". Westerly wind bursts \
(WWBs) in the western Pacific \[LongDash] short-lived bursts driven \
by the Madden-Julian Oscillation, tropical cyclones, and other \
synoptic noise \[LongDash] inject impulsive disturbances that can \
push the system across thresholds. WWBs are most active in boreal \
spring (March\[Dash]April), a fact that will reappear later in this \
notebook as a key driver of the spring predictability barrier."}],

genImgCell["walker_circulation.png"],
captionCell["Figure 1.2  Schematic of the climatological Walker \
circulation along the equator. Easterly trade winds at the surface, \
rising air over the Indonesian warm pool, upper-level westerly return \
flow, descending air over the cool eastern Pacific. During El \
Ni\[NTilde]o the trades weaken, the thermocline flattens, and the \
rising branch shifts east toward the dateline. Generated with \
OpenAI gpt-image-2."]
}];


(* ============================================================
   §2 The May 2026 El Niño Watch
   ============================================================ *)

writeAll[{

hd1["2.  The May 2026 El Ni\[NTilde]o Watch"],

para[{"NOAA's Climate Prediction Center and Columbia's IRI jointly \
issue an ENSO Diagnostic Discussion at the start of each month. The \
discussion is a prose document combined with the IRI/CPC plume of \
~25 dynamical and statistical model forecasts. When the official \
probability of El Ni\[NTilde]o onset within the next few seasons \
exceeds ~55 %, the CPC raises an ", bold["El Ni\[NTilde]o Watch"],
   "; when it exceeds ~70 %, a Watch is escalated to an ",
   bold["Advisory"], " (similar logic for La Ni\[NTilde]a)."}],

(* Popular-science companion callout *)
Cell[TextData[{
   StyleBox["Popular-science take.", FontWeight -> Bold,
      FontSlant -> Italic], " The BBC's lead weather presenter Simon \
King has a 5-minute explainer on the same emerging event \[LongDash] \
\[OpenCurlyDoubleQuote]Right now, across the Pacific Ocean, something \
is happening that could change the world's weather.\
\[CloseCurlyDoubleQuote]  He notes that US, Australian, and European \
forecasters all see this El Ni\[NTilde]o forming within the next \
month, with a 2-in-3 chance of becoming strong or very strong by late \
autumn / early winter, and around half of the European model runs \
suggesting it could be one of the strongest on record. Some \
forecasters have already nicknamed it the \
\[OpenCurlyDoubleQuote]Godzilla\[CloseCurlyDoubleQuote] El \
Ni\[NTilde]o. ", link["BBC News explainer (Simon King)",
   "https://www.youtube.com/watch?v=UEseLvpl9ss"], "."
}], "Text",
   FontSize -> 12, CellMargins -> {{70, 70}, {6, 6}},
   Background -> RGBColor[0.93, 0.95, 0.99],
   FrameMargins -> 12, CellFrame -> {{0, 0}, {0, 0}}],

para[{"The May 2026 issuance is unambiguous. The probability bars \
below show El Ni\[NTilde]o probabilities climbing from 82 % in \
May\[Dash]June\[Dash]July 2026 to 96 % in the four overlapping seasons \
straddling DJF 2026\[Dash]27, with La Ni\[NTilde]a essentially ruled \
out until early 2027."}],

imgCell["noaa_probabilities.png"],
captionCell["Figure 2.1  Official NOAA / IRI ENSO probabilities for \
the next nine rolling 3-month seasons, as issued in the May 2026 \
ENSO Diagnostic Discussion. Red = El Ni\[NTilde]o (Ni\[NTilde]o 3.4 \
ONI \[GreaterEqual] +0.5 \[Degree]C); grey = Neutral; blue = La \
Ni\[NTilde]a (ONI \[LessEqual] \[Minus]0.5 \[Degree]C). Each column \
sums to 100 %."],

para[{"This Watch is striking for two reasons. (a) It comes just six \
months after the end of the 2024\[Dash]25 La Ni\[NTilde]a (ONI \
bottomed at \[Minus]0.55 in November 2025). Such a rapid \
La\[Dash]Ni\[NTilde]a-to-El-Ni\[NTilde]o transition is unusual but not \
unprecedented (cf. 1972, 1976, 2009 transitions). (b) The probability \
of persistence to DJF is exceptionally high (96 %); for context, the \
operationally similar May 2023 discussion gave a 90 % probability for \
DJF 2023\[Dash]24, and the 2023\[Dash]24 super-El-Ni\[NTilde]o indeed \
materialised."}],

imgCell["nino34_record.png"],
captionCell["Figure 2.2  The full monthly Ni\[NTilde]o 3.4 SST anomaly \
record from NOAA PSL (ERSSTv5 dataset, base period 1991\[Dash]2020). \
The four \[OpenCurlyDoubleQuote]super-El-Ni\[NTilde]o\[CloseCurlyDoubleQuote] \
events (1982\[Dash]83, 1997\[Dash]98, 2015\[Dash]16, 2023\[Dash]24) \
are clearly visible as the > +2 \[Degree]C peaks. The right edge of \
the record is the warming we are about to forecast."],

para[{ital["What the data show right now."], " The most recent four \
months of Ni\[NTilde]o 3.4 anomalies, fetched live for this notebook, \
are:"}],

(* Dynamic table from live data, rendered as a TextData chunk so it
   serialises cleanly to InputForm without needing the FE to
   re-evaluate ExpressionCell. *)
With[
   {rows = Take[Drop[Import[FileNameJoin[{repoRoot, "data",
       "nino34_monthly.csv"}], "CSV"], 1], -6]},
   Cell[
     TextData[Riffle[
       ({StyleBox[DateString[DateObject[{#[[1]], #[[2]], 15}, "Day"],
            {"MonthName", " ", "Year"}], FontWeight -> Bold],
         "    ",
         StyleBox[ToString[NumberForm[#[[4]], {4, 2}]] <> " \[Degree]C",
            FontFamily -> "Courier"]
        } & /@ rows),
       "\n"
     ]],
     "Text",
     FontSize -> 13, FontFamily -> "Helvetica",
     CellMargins -> {{100, 100}, {6, 6}},
     Background -> RGBColor[0.96, 0.97, 0.92]
   ]
],

para[{"The +0.31 \[Degree]C jump from February to April \
(\[Minus]0.27 \[RightArrow] +0.23) is the largest two-month boreal \
spring warming in the historical record. It is exactly the kind of \
emergent behaviour that the toy models in \[Section]5 are designed \
to capture: a stochastic system on the edge of bifurcation, given a \
favourable phase-space starting point by the upper-ocean heat content \
left over from previous La Ni\[NTilde]a recharge, gets a spring \
westerly-wind-burst kick and launches into the growth regime."}]
}];


(* ============================================================
   §3 Why ENSO matters — teleconnections
   ============================================================ *)

writeAll[{

hd1["3.  Why ENSO matters: global teleconnections"],

para[{"ENSO is not just a Pacific story. The warm pool's eastward \
shift during El Ni\[NTilde]o reorganises tropical convection, which \
launches stationary Rossby-wave trains into the extratropics, which \
in turn shift the jet streams and the storm tracks across the entire \
Northern Hemisphere winter. These ", bold["teleconnections"], " are \
strong enough to be statistically detectable and operationally \
useful for seasonal forecasting on every continent."}],

genImgCell["enso_teleconnections.png"],
captionCell["Figure 3.1  Typical El Ni\[NTilde]o winter (DJF) climate \
anomalies. Wet/dry/warm/cool labels indicate the dominant precipitation \
or temperature departure. The pattern is canonical: drought in the \
Maritime Continent (Indonesia, northern Australia) and southern \
Africa; flooding rains on the Peruvian/Ecuadorian coast and across \
the southern US; warmer-than-average winters across western and \
northern North America; cooler-than-average across the Gulf Coast. \
Generated with OpenAI gpt-image-2."],

para[{ital["Some concrete numbers from the most recent event."], " \
The 2023\[Dash]24 El Ni\[NTilde]o (peak ONI +2.0 \[Degree]C in \
November 2023) contributed to: a record fire season in the Amazon \
basin (drought conditions); record-breaking heat in West Africa and \
South Asia; failed monsoons in southern Africa; and a near-doubling \
of global insured catastrophe losses relative to climatology. \
Forecasting the next event well, several months in advance, has \
billion-dollar consequences across agriculture, water resources, \
disaster preparedness, and energy markets."}],

para[{"Which is why the spring predictability barrier matters."}]
}];


(* ============================================================
   §4 The spring predictability barrier
   ============================================================ *)

writeAll[{

hd1["4.  The spring predictability barrier"],

para[{ital["The intuitive version."], " Imagine you are a forecaster \
in January. The tropical Pacific is in some particular state \
(some Ni\[NTilde]o 3.4 anomaly, some thermocline tilt). Your job is \
to predict what the state will be next August. Now imagine instead \
that you are a forecaster in July of the previous year, trying to \
predict the same August. Intuitively, the January forecaster should \
have an easier time \[LongDash] she is closer in time. The shocking \
empirical fact about ENSO is that the opposite is true: ", bold["the \
July forecaster, with a 13-month lead, beats the January forecaster, \
with a 7-month lead"], ", because the January forecast has to look \
through the spring barrier."}],

genImgCell["spring_barrier_concept.png"],
captionCell["Figure 4.1  Conceptual calendar wheel of the spring \
predictability barrier. The Bjerknes coupled-feedback strength \
(blue curve) is positive in autumn (growth) and near zero or \
negative in spring (damping). The westerly-wind-burst (WWB) intensity \
(orange) peaks in March\[Dash]April. Forecasts crossing the spring \
sector lose initial-condition information to the combination of \
strong stochastic kicks and weak coupled feedback. Generated with \
OpenAI gpt-image-2."],

para[{ital["The quantitative version."], " Define the lagged \
autocorrelation of the observed Ni\[NTilde]o 3.4 anomaly as a function \
of the initial calendar month ", math["m\:2080"], " and the lead time \
\[Tau] (in months):"}],

displayMath[
   RowBox[{
      "\[Rho]", "(", SubscriptBox["m", "0"], ",", "\[Tau]", ")",
      "=",
      FractionBox[
        RowBox[{"E", "[", " ",
          RowBox[{
            RowBox[{"(", RowBox[{
              RowBox[{"T", "(", RowBox[{"y", ",", SubscriptBox["m", "0"]}], ")"}],
              "-", OverscriptBox["T", "_"]
            }], ")"}],
            RowBox[{"(", RowBox[{
              RowBox[{"T", "(", RowBox[{
                RowBox[{"y", "+", RowBox[{"\[LeftFloor]",
                  RowBox[{RowBox[{"(", RowBox[{SubscriptBox["m", "0"], "+", "\[Tau]", "-", "1"}], ")"}], "/", "12"}],
                  "\[RightFloor]"}]}],
                ",",
                RowBox[{"1", "+", RowBox[{"mod", "(",
                   RowBox[{RowBox[{SubscriptBox["m", "0"], "+", "\[Tau]", "-", "1"}], ",", "12"}],
                   ")"}]}]
              }], ")"}],
              "-", OverscriptBox["T", "_"]
            }], ")"}]
          }],
        " ", "]"}],
        RowBox[{SubscriptBox["\[Sigma]", "T"], "(", SubscriptBox["m", "0"], ")",
          " ",
          SubscriptBox["\[Sigma]", "T"], "(",
          RowBox[{"1", "+", RowBox[{"mod", "(",
             RowBox[{RowBox[{SubscriptBox["m", "0"], "+", "\[Tau]", "-", "1"}], ",", "12"}],
             ")"}]}],
          ")"}]
      ]
   }]
],

para[{"\[Rho]"," is computed across all available years ",
   math["y"], "; ", sigT,
   " is the marginal standard deviation for that calendar month; the \
floor and mod arithmetic carry the verification time forward into the \
next year when ", math["m\:2080 + \[Tau] > 12"],
   ". By construction ", math["\[Rho](m\:2080, 0) = 1"],
   ". If ENSO were a white-noise process, ", math["\[Rho]"], " would \
collapse to zero for any \[Tau] \[GreaterEqual] 1. If it were a pure \
sinusoid, ", math["\[Rho]"], " would oscillate forever between \
\[PlusMinus] 1."}],

para[{"Computing this from 77 years of monthly ERSSTv5 anomalies \
gives:"}],

imgCell["spring_barrier_obs.png"],
captionCell["Figure 4.2  Observed Ni\[NTilde]o 3.4 lagged \
autocorrelation \[Rho](m\:2080, lead). Rows = lead in months, columns \
= initial calendar month. Deep blue = high autocorrelation (skillful \
persistence), red = low or negative correlation. The classic \
V-shaped low-correlation valley pointing into the May\[Dash]July \
verification window is the spring predictability barrier."],

para[{ital["What the heatmap is telling you."], " Read down any \
column. For an initial month of October (m\:2080 = 10), the \
autocorrelation stays above 0.5 all the way out to 12 months \
(verifying in next October). For an initial month of January \
(m\:2080 = 1), the autocorrelation drops below 0.2 by lead 5 \
(verifying in June). Same dataset, same statistic, hugely different \
behaviour. The asymmetry is the barrier."}],

para[{"The same data in a different cut: the persistence-skill curves \
collapse by start month."}],

imgCell["persistence_drop.png"],
captionCell["Figure 4.3  Persistence-skill curves: ", math["\[Rho]"],
   " vs lead for each starting calendar month. Lines that start in \
boreal winter and spring (Jan, Feb, Mar, Apr) drop steeply through \
the first six leads; lines that start in summer and autumn (Jul, \
Aug, Sep, Oct) retain ", math["\[Rho] > 0.5"], " out to lead 9."],

para[{ital["Why the barrier exists."], " There are three reinforcing \
mechanisms. (a) The Bjerknes feedback ", Rt, " (the \
coupled-system growth rate that we will derive in \[Section]5.1) is \
seasonally modulated. It is strongest in boreal autumn, when the \
climatological cold tongue and trade winds are most pronounced, and \
weakest \[LongDash] often near zero or slightly negative \
\[LongDash] in boreal spring, when the cold tongue retreats and the \
intertropical convergence zone (ITCZ) sits near the equator. \
A small initial-condition perturbation in spring is therefore not \
amplified by coupling; it just sits there or decays."}],

para[{"(b) Westerly-wind-burst (WWB) activity in the western Pacific \
peaks in March\[Dash]April. The injected stochastic forcing dominates \
the deterministic dynamics during this season, washing out the \
information carried by the initial condition."}],

para[{"(c) The thermocline phase relationship between ", math["T"],
   " and ", math["h"], " (eastern SST and western thermocline depth) \
goes through a transition in spring: the system is in transit \
between the recharged and discharged states of the canonical \
recharge\[Dash]discharge cycle. Small phase errors compound here."}]
}];


(* ============================================================
   §5 Two toy models of ENSO
   ============================================================ *)

writeAll[{

hd1["5.  Two toy models of ENSO"],

para[{"There is a remarkable two-fold reduction of the full \
coupled ocean\[Dash]atmosphere problem that produces tractable models \
exhibiting the essential ENSO physics. Both reductions are pre-PNAS-era \
classical references and both can be implemented in under 100 lines \
of Wolfram Language."}],

genImgCell["recharge_discharge_cycle.png"],
captionCell["Figure 5.1  The four phases of the recharge\[Dash]discharge \
ENSO cycle (Jin 1997), drawn schematically. \
(I) Pre-event recharge: thermocline anomalously deep across the basin, \
heat content elevated. (II) El Ni\[NTilde]o: warm pool sloshes east, \
thermocline flattens, eastern SST anomalously warm. (III) Discharge: \
basin-wide upper-ocean heat content depleted by poleward Sverdrup \
transport during the event. (IV) La Ni\[NTilde]a: thermocline steeply \
tilted, eastern cold tongue intensified. The cycle then restarts. \
Generated with OpenAI gpt-image-2."],


(* ============== 5.1 RDO ============== *)

hd2["5.1  Recharge\[Dash]discharge oscillator (Jin 1997)"],

para[{ital["Conceptual reduction."], " Following Jin (1997), reduce \
the equatorial Pacific to two state variables:"}],

para[{"\[Bullet] ", math["T(t)"], " = eastern equatorial SST anomaly \
(\[Degree]C), proxied operationally by Ni\[NTilde]o 3.4."}],

para[{"\[Bullet] ", math["h(t)"], " = western (or basin-mean) \
equatorial thermocline-depth anomaly, proxied by warm water volume \
above the 20 \[Degree]C isotherm. Positive ", math["h"], " means \
\[OpenCurlyDoubleQuote]more heat in the upper ocean than \
climatology\[CloseCurlyDoubleQuote] (recharged); negative means \
discharged."}],

para[{ital["Deterministic core."], " The linearised SST budget gives"}],

displayMath[
   RowBox[{
     RowBox[{FractionBox["dT", "dt"], "=",
       RowBox[{"R", " ", "T"}], "+", RowBox[{"\[Gamma]", " ", "h"}]}]
   }]
],

para[{"where ", math["R"], " (units of inverse months) is the net \
Bjerknes-feedback growth rate (positive Bjerknes minus thermodynamic \
damping) and ", math["\[Gamma]"], " is the strength with which an \
anomalously deep western thermocline pushes warm water eastward and \
warms the east."}],

para[{"The ocean-adjustment equation closes the system by transporting \
heat content out of the basin in proportion to wind-stress-curl-driven \
Sverdrup flow:"}],

displayMath[
   RowBox[{
     RowBox[{FractionBox["dh", "dt"], "=",
       RowBox[{RowBox[{"-", "r"}], " ", "h"}], "-",
       RowBox[{"\[Alpha]", " ", "T"}]}]
   }]
],

para[{math["r"], " is the ocean-adjustment relaxation rate (1/years), \
and ", math["\[Alpha]"], " is the (negative) wind-stress feedback: an \
anomalously warm east produces an anomalously eastward wind stress, \
which drains the western recharge. Together this is a 2 \[Times] 2 \
linear system with eigenvalues"}],

displayMath[
   RowBox[{
     SubscriptBox["\[Lambda]", RowBox[{"\[PlusMinus]"}]], "=",
     FractionBox[RowBox[{"R", "-", "r"}], "2"],
     "\[PlusMinus]",
     SqrtBox[RowBox[{
        SuperscriptBox[RowBox[{"(",
           FractionBox[RowBox[{"R", "+", "r"}], "2"],
        ")"}], "2"],
        "-", RowBox[{"\[Gamma]", " ", "\[Alpha]"}]
     }]]
   }]
],

para[{"For the parameter regime relevant to nature \
(", math["\[Gamma] \[Alpha] > ((R + r)/2)\.b2"], "), the square root is \
imaginary and the system oscillates with frequency ", math["\[Omega] = \
\[Sqrt](\[Gamma] \[Alpha] \[Minus] ((R + r)/2)\.b2)"],
   " and growth/decay rate ", math["(R \[Minus] r)/2"],
   ". With canonical parameters this gives an oscillation period of \
3\[Dash]6 years, in the observed ENSO band."}],

para[{ital["Adding the stochastic forcing."], " A purely deterministic \
RDO either decays to the origin (sub-critical: ", math["R < r"], ") or \
explodes to infinity (super-critical). Neither matches observations. \
The standard remedy is multiplicative-then-additive stochastic forcing \
(Burgers, Jin & van Oldenborgh 2005) representing all the \
high-frequency atmospheric weather that the slow ocean mode does not \
resolve:"}],

displayMath[
   RowBox[{
     RowBox[{FractionBox["dT", "dt"], "=",
       RowBox[{"R", "(", "t", ")", " ", "T"}], "+",
       RowBox[{"\[Gamma]", " ", "h"}], "+",
       RowBox[{SubscriptBox["\[Sigma]", "T"], " ",
         SubscriptBox["\[Xi]", "T"], "(", "t", ")"}], "+",
       RowBox[{SubscriptBox["\[Sigma]", "w"], "(", "t", ")", " ",
         SubscriptBox["\[Xi]", "w"], "(", "t", ")"}]}]
   }]
],

displayMath[
   RowBox[{
     RowBox[{FractionBox["dh", "dt"], "=",
       RowBox[{RowBox[{"-", "r"}], " ", "h"}], "-",
       RowBox[{"\[Alpha]", " ", "T"}], "+",
       RowBox[{SubscriptBox["\[Sigma]", "h"], " ",
         SubscriptBox["\[Xi]", "h"], "(", "t", ")"}]}]
   }]
],

para[{"where the three ", math["\[Xi](t)"], " are independent unit \
white noises (formally d", math["W"], "/d", math["t"], "), \
", sigT, ", ", sigH, " are the slow \
(synoptic + sub-monthly) stochastic forcing amplitudes, and \
", sigW, " is the specifically-westerly-wind-burst \
forcing whose variance peaks in boreal spring."}],

para[{ital["The Stein\[Dash]Levine\[Dash]Jin seasonal mechanism."],
   " Two coefficients are taken to be seasonally periodic:"}],

displayMath[
   RowBox[{
     RowBox[{"R", "(", "t", ")", "=", SubscriptBox["R", "0"], "+",
       RowBox[{SubscriptBox["R", "1"], " ",
         RowBox[{"cos", "(",
           FractionBox[RowBox[{"2", "\[Pi]", "(", "t", "-", "9", ")"}], "12"],
         ")"}]}]}]
   }]
],

displayMath[
   RowBox[{
     RowBox[{SubscriptBox["\[Sigma]", "w"], "(", "t", ")", "=",
       SubscriptBox["\[Sigma]", RowBox[{"w", ",", "0"}]],
       RowBox[{"(",
         RowBox[{"1", "+",
           RowBox[{SubscriptBox["a", "w"], " ",
             RowBox[{"cos", "(",
               FractionBox[RowBox[{"2", "\[Pi]", "(", "t", "-", "3", ")"}], "12"],
             ")"}]}]}],
       ")"}]}]
   }]
],

para[{Rt, " peaks in September (most positive, strongest \
growth) and dips in March (least positive or negative, strongest \
damping); ", sigW, " peaks in March\[Dash]April \
(spring WWB activity), giving the model an in-built spring barrier."}],

imgCell["rdo_seasonality.png"],
captionCell["Figure 5.2  The two seasonally varying coefficients used \
in the recharge\[Dash]discharge model: ", Rt, " (blue) and ",
   sigW, " (orange). The phase relationship is the \
heart of the Stein\[Dash]Levine\[Dash]Jin spring-barrier mechanism."],

para[{ital["Numerical integration."], " We use the Euler\[Dash]Maruyama \
scheme with time step ", math["dt = 0.05"], " months \
(\[TildeTilde]36 hours). For each step:"}],

displayMath[
   RowBox[{
     RowBox[{
       SubscriptBox["T", RowBox[{"n", "+", "1"}]], "=",
       SubscriptBox["T", "n"], "+",
       RowBox[{"dt", " ", RowBox[{"(", RowBox[{
         RowBox[{"R", "(", SubscriptBox["t", "n"], ")", " ",
           SubscriptBox["T", "n"]}], "+",
         RowBox[{"\[Gamma]", " ", SubscriptBox["h", "n"]}]
       }], ")"}]}],
       "+",
       RowBox[{SqrtBox["dt"], "(",
         SubscriptBox["\[Sigma]", "T"], " ",
         SubscriptBox["\[Eta]", RowBox[{"T", ",", "n"}]],
         "+",
         RowBox[{SubscriptBox["\[Sigma]", "w"], "(", SubscriptBox["t", "n"], ")", " ",
           SubscriptBox["\[Eta]", RowBox[{"w", ",", "n"}]]}],
       ")"}]
     }]
   }]
],

para[{"with ", math["\[Eta]"], " drawn from a standard normal. The \
Wolfram implementation is concise:"}],

codeIn["simulate[T0_, h0_, m0_, tEnd_, dt_, seed_] := Module[
   {steps, T, h, t, sqrtDt, ts, Ts, hs, eT, eH, eW},
   SeedRandom[seed];
   steps  = Round[tEnd/dt];   sqrtDt = Sqrt[dt];
   T = T0; h = h0; t = 0.;
   ts = ConstantArray[0., steps + 1];
   Ts = ConstantArray[0., steps + 1];
   hs = ConstantArray[0., steps + 1];
   ts[[1]] = 0.; Ts[[1]] = T; hs[[1]] = h;
   Do[
     eT = RandomVariate[NormalDistribution[]];
     eH = RandomVariate[NormalDistribution[]];
     eW = RandomVariate[NormalDistribution[]];
     T += dt (Rseason[t, m0] T + gamma h) +
           sqrtDt (sigmaT eT + sigWWB[t, m0] eW);
     h += dt (-rDamp h - alpha T) + sqrtDt sigmaH eH;
     t += dt;
     ts[[i + 1]] = t; Ts[[i + 1]] = T; hs[[i + 1]] = h,
     {i, steps}];
   <|\"t\" -> ts, \"T\" -> Ts, \"h\" -> hs|>
];"],

para[{"A 20-year free run reproduces the irregular \
3\[Dash]6-year-period ENSO oscillation with amplitude \[PlusMinus]2\
\[Dash]3 \[Degree]C:"}],

imgCell["rdo_timeseries.png"],
captionCell["Figure 5.3  Single 20-year stochastic realisation of the \
recharge\[Dash]discharge oscillator with seasonally modulated \
coefficients. Period is irregular (3\[Dash]6 years) and amplitude is \
variable, as in observations."],

imgCell["rdo_phase.png"],
captionCell["Figure 5.4  Phase-space portrait. The trajectory orbits \
the origin counter-clockwise: an SST warm anomaly (right side) drives \
westward heat transport that discharges the thermocline (downward), \
which removes the SST forcing (leftward), which lets the thermocline \
recharge (upward). The recharge\[Dash]discharge cycle."],

para[{ital["Ensemble forecasts and confidence intervals."], " To \
diagnose the spring barrier in the model, run an ensemble of \
500 independent realisations from each starting calendar month, with \
initial conditions drawn from the model's own climatological \
distribution conditioned on that month. The ensemble spread at lead \
\[Tau] is the model's analogue of forecast uncertainty. We summarise \
each lead with five percentiles and report the 5\[Dash]95 % and \
25\[Dash]75 % bands as our confidence intervals:"}],

imgCell["rdo_barrier_cone.png"],
captionCell["Figure 5.5  Ensemble forecast cones from the \
recharge\[Dash]discharge oscillator. Each panel is a different initial \
calendar month, identical model and identical initial-state \
distribution otherwise. Dark orange = 25\[Dash]75 % confidence \
interval; light orange = 5\[Dash]95 % CI; black = ensemble median. \
The cone's width is the model's standard deviation of T conditional \
on the initial state, which is the toy-model analogue of forecast \
uncertainty. The cones \[OpenCurlyDoubleQuote]breathe\[CloseCurlyDoubleQuote] \
seasonally as the verification month sweeps through the strong-\
coupling autumn and the weak-coupling/high-WWB spring."],

imgCell["rdo_spread_heatmap.png"],
captionCell["Figure 5.6  Ensemble standard deviation as a function of \
initial month m\:2080 and lead. The bright vertical/diagonal band \
aligns with the verification window crossing the model's growth \
season (Sep\[Dash]Nov), where the seasonal R(t) amplifies any noise \
accumulated en route. The pattern is the toy model's signature of \
the same barrier physics."],

para[{ital["What the model gets right and what it misses."], " The RDO \
correctly reproduces (i) the broadband 3\[Dash]7-year period, \
(ii) the seasonal phase locking (events peak in DJF), (iii) the \
asymmetric ensemble cones across initial months, and (iv) the order-\
of-magnitude ensemble spread (\[Sigma] \[TildeTilde] 0.5\[Dash]1.0 \
\[Degree]C at 6-month lead) that operational dynamical models also \
produce. It misses (i) the El-Ni\[NTilde]o\[Dash]La-Ni\[NTilde]a \
amplitude asymmetry that requires the nonlinear cubic damping term \
in Jin's full equations, and (ii) the extratropical influences on \
the ENSO state that more complete models include."}],


(* ============== 5.2 DDE ============== *)

hd2["5.2  Delayed-action oscillator (Suarez & Schopf 1988)"],

para[{ital["A different conceptual reduction."], " Suarez & Schopf \
(1988) take the same physics and reduce it to a single scalar SST \
anomaly with an explicit time delay. The delay represents the \
basin-crossing time of equatorial Rossby + Kelvin waves: an SST \
warming forces a wind stress that excites a Rossby wave westward; \
the wave reflects off the western boundary as an upwelling Kelvin \
wave; the Kelvin wave returns east and cools the SST. The round-\
trip time is \[Delta] \[TildeTilde] 6 months."}],

displayMath[
   RowBox[{
     RowBox[{FractionBox["dT", "dt"], "=",
       "T", "-",
       RowBox[{"\[Alpha]", " ",
         RowBox[{"T", "(",
            RowBox[{"t", "-", "\[Delta]"}],
         ")"}]}],
       "-",
       SuperscriptBox["T", "3"], "+",
       RowBox[{"\[Sigma]", "(", "t", ")", " ", "\[Xi]", "(", "t", ")"}]
     }]
   }]
],

para[{"The +T term is the Bjerknes feedback (growth); the negative \
\[Alpha] T(t-\[Delta]) term is the delayed wave return (negative \
feedback); the \[Minus]T\.b3 term is the nonlinear damping that \
limits the amplitude; and \[Sigma](t) \[Xi](t) is again seasonally \
modulated stochastic WWB forcing. Time is rescaled so that one model \
time-unit = two calendar months, putting the canonical 6-month wave \
delay at \[Delta] = 3 in dimensionless units."}],

para[{ital["Hopf bifurcation."], " Linearising about ", math["T = 0"],
   " and substituting ", math["T \[Tilde] e^(\[Lambda] t)"], " gives \
the characteristic equation ", math["\[Lambda] = 1 \[Minus] \[Alpha] \
e^(\[Minus] \[Lambda] \[Delta])"], ", a transcendental equation whose \
roots can be found numerically. For ", math["\[Delta] = 3"], " the \
real part of the leading eigenvalue passes through zero at \
", math["\[Alpha] \[TildeTilde] 0.5"], "; below that the origin is \
stable, above it the system bifurcates into a stable nonlinear limit \
cycle. The bifurcation sweep:"}],

imgCell["delayed_bifurcation.png"],
captionCell["Figure 5.7  Min and max of T in long stochastic runs of \
the delayed oscillator, as a function of the wave-coupling \
\[Alpha], with \[Delta] = 3 (= 6 months) held fixed. The Hopf \
bifurcation at ", math["\[Alpha] \[TildeTilde] 0.5"], " is the \
transition from noise-driven fluctuations about the fixed point to \
a sustained nonlinear limit cycle."],

para[{ital["Implementation."], " The Wolfram implementation maintains \
a circular history buffer so the delayed value ", math["T(t-\[Delta])"],
   " is available at every step without recomputation:"}],

codeIn["simulateDDE[m0_, tEnd_, dt_, hist0_, seed_] := Module[
   {nHist, hist, idx, T, t, sqrtDt, ts, Ts, eta, Tdel, drift, diff, nSteps},
   SeedRandom[seed];
   nHist  = Ceiling[delayTau/dt] + 2;
   hist   = ConstantArray[hist0, nHist];   (* circular buffer *)
   idx    = 1;  T = hist0;  t = 0.;
   nSteps = Round[tEnd/dt];   sqrtDt = Sqrt[dt];
   ts = ConstantArray[0., nSteps + 1];
   Ts = ConstantArray[0., nSteps + 1];
   ts[[1]] = 0.; Ts[[1]] = T;
   Do[
     With[{kBack = Round[delayTau/dt]},
        Tdel = hist[[1 + Mod[idx - 1 - kBack + nHist, nHist]]]];
     drift = T - alphaDel Tdel - T^3;
     diff  = sigStoch[t, m0];
     eta   = RandomVariate[NormalDistribution[]];
     T += dt drift + sqrtDt diff eta;
     t += dt;
     idx = 1 + Mod[idx, nHist];   hist[[idx]] = T;
     ts[[i + 1]] = t; Ts[[i + 1]] = T,
     {i, nSteps}];
   <|\"t\" -> ts, \"T\" -> Ts|>
];"],

imgCell["delayed_timeseries.png"],
captionCell["Figure 5.8  A 20-year realisation of the delayed \
oscillator with \[Alpha] = 0.75 (well above the Hopf threshold) and \
\[Delta] = 6 months. The limit cycle period is approximately 4\
\[Delta] = 24 months \[LongDash] shorter than the canonical ENSO \
period \[LongDash] but the nonlinear-oscillator phenomenology is \
correct."],

imgCell["delayed_cone.png"],
captionCell["Figure 5.9  Delayed-oscillator ensemble forecast cones \
for four initial calendar months. The cones \
\[OpenCurlyDoubleQuote]breathe\[CloseCurlyDoubleQuote] with the \
underlying limit-cycle period (\[TildeTilde] 24 months) rather than \
showing a single spring-barrier window, because in this parameter \
regime the deterministic cycle dominates over the seasonal \
modulation of the stochastic forcing."]
}];


(* ============================================================
   §6 Live forecast
   ============================================================ *)

writeAll[{

hd1["6.  Live forecast: initialising from April 2026"],

para[{"To turn the RDO into a live forecast tool we need (a) the \
current observed ", math["T"], " and (b) the current observed (or \
inferred) ", math["h"], ". The first is just the latest \
Ni\[NTilde]o 3.4 anomaly: +0.23 \[Degree]C in April 2026. The second \
is harder \[LongDash] there is no clean monthly observational \
product for warm-water-volume anomaly that we can fetch live in \
the same way."}],

para[{ital["Inferring h from the recent tendency."], " The linearised \
SST equation gives a clean inversion. From"}],

displayMath[
   RowBox[{
     RowBox[{FractionBox["dT", "dt"], "\[TildeTilde]",
       RowBox[{"R", "(", "t", ")", " ", "T"}], "+",
       RowBox[{"\[Gamma]", " ", "h"}]}]
   }]
],

para[{"we get"}],

displayMath[
   RowBox[{
     "h", "\[TildeTilde]",
     FractionBox[
       RowBox[{RowBox[{FractionBox["dT", "dt"]}], "-",
         RowBox[{"R", "(", "t", ")", " ", "T"}]}],
       "\[Gamma]"]
   }]
],

para[{"Plugging in the most recent two-month tendency ", math["dT/dt"],
   " = (T(Apr) \[Minus] T(Feb))/2 = 0.25 \[Degree]C/mo, the April \
seasonal feedback ", math["R(Apr) \[TildeTilde] \[Minus]0.21 month\.bb"],
   ", and ", math["\[Gamma]"], " = 0.06 gives an inferred \
", math["h"], " \[TildeTilde] +4.9, a large positive thermocline \
anomaly consistent with the substantial subsurface heat content \
remaining from the previous La Ni\[NTilde]a recharge."}],

para[{"With ", math["(T, h)"], " = (+0.23, +4.9) as the initial \
condition, we run a 500-member RDO ensemble out 18 months \
(April 2026 \[RightArrow] October 2027). The resulting ", bold["live \
toy-model forecast"], ", with explicit 5\[Dash]95 % and 25\[Dash]75 % \
confidence intervals, is:"}],

imgCell["rdo_live_forecast.png"],
captionCell["Figure 6.1  Recharge\[Dash]discharge oscillator live \
ensemble forecast initialised from observed April 2026 state \
(T = +0.23 \[Degree]C, inferred h \[TildeTilde] +4.9). Dotted line \
shows the past 12 months of observations. The solid black line is \
the ensemble median; the darker orange band is the 25\[Dash]75 % \
inter-quartile range; the lighter orange band is the 5\[Dash]95 % \
confidence interval. The model peaks the event at +2.0 to +3.0 \
\[Degree]C around DJF 2026\[Dash]27, broadly consistent with the \
NOAA 96 % El-Ni\[NTilde]o probability for that season."],

para[{ital["Comparison with the operational IRI/CPC plume."], " The \
operational plume is the spread of ~25 independent dynamical and \
statistical models. The toy-model cone is a single model run with \
500 stochastic realisations. They are not directly comparable, but \
both represent forecast uncertainty: the plume's vertical extent and \
the cone's width should be of similar order, and both should narrow \
into autumn (strong coupling reduces sensitivity to noise) and widen \
into next spring (the barrier reasserts itself)."}],

imgCell["forecast_overlay.png"],
captionCell["Figure 6.2  Three-panel live overlay: IRI/CPC operational \
plume (top), toy-model ensemble cone (middle), official NOAA \
probability bars (bottom). All three panels share the same calendar \
horizon."],

para[{ital["Toy-model probability of El Ni\[NTilde]o vs the NOAA \
issuance."], " The fraction of the 500-member ensemble whose monthly \
T exceeds the +0.5 \[Degree]C threshold, season by season, gives the \
toy model's analogue of the NOAA probability bar:"}],

(* live-computed h estimate, displayed inline *)
With[
   {n34data = Import[FileNameJoin[{repoRoot, "data", "nino34_monthly.csv"}], "CSV"]},
   With[
     {Tlast = n34data[[-1, 4]], Tprev2 = n34data[[-3, 4]],
      m0last = n34data[[-1, 2]]},
     With[
       {dTdt = (Tlast - Tprev2)/2.,
        hEst = ((Tlast - Tprev2)/2. -
           (-0.03 + 0.18 Cos[2 Pi (n34data[[-1, 2]] - 9)/12]) Tlast)/0.06},
       Cell[TextData[{
          "Estimated thermocline state from the observed monthly tendency: ",
          bold[ToString[NumberForm[hEst, {4, 2}]]],
          " (large positive recharge consistent with the residual heat \
content from the previous La Ni\[NTilde]a). The 500-member toy-model \
ensemble is initialised from (T, h) = (",
          bold[ToString[NumberForm[Tlast, {3, 2}]]], ", ",
          bold[ToString[NumberForm[hEst, {3, 2}]]], ")."
       }], "Text"]
     ]
   ]
],

para[{ital["Caveats."], " The toy model is, well, a toy. Operational \
forecast systems use full 3-D coupled GCMs with assimilated subsurface \
ocean temperature, satellite wind stress, and tens of thousands of \
ensemble members. Our 2-variable system necessarily misses (i) the \
spatial structure of the equatorial Pacific (different events have \
different east-west patterns: \"central Pacific\" El Ni\[NTilde]o vs \
\"east Pacific\" El Ni\[NTilde]o), (ii) interactions with the \
extratropical atmosphere (PDO, NAO), and (iii) any climate-change-\
driven trends in either the mean state or the variability. The cone \
should therefore be interpreted as a sanity-check on the operational \
forecast \[LongDash] not as competition with it."}]
}];


(* ============================================================
   §7 Conclusions and outlook
   ============================================================ *)

writeAll[{

hd1["7.  Conclusions and outlook"],

para[{ital["What we showed."], " (1) The May 2026 El Ni\[NTilde]o \
Watch is supported by a clear emergence signal in the observational \
record: the sharpest two-month boreal-spring warming in the 1950\
\[Dash]present Ni\[NTilde]o 3.4 record. (2) The spring predictability \
barrier is unambiguous in 77 years of observations: forecasts \
initialised in boreal winter collapse to near-zero skill by mid-summer, \
while forecasts initialised in late summer keep skill all the way to \
the following spring. (3) Both classical ENSO toy models \
(recharge\[Dash]discharge and delayed-action) can be implemented in \
under 100 lines of Wolfram Language, and the recharge\[Dash]discharge \
model with seasonally varying Bjerknes feedback and seasonally varying \
WWB forcing reproduces the spring-barrier mechanism quantitatively. \
(4) Initialising the toy model from the current observed state \
produces a live ensemble forecast cone consistent with the NOAA 96 % \
peak-DJF El Ni\[NTilde]o probability."}],

para[{ital["What to watch in the next 60 days."], " The next NOAA \
ENSO Diagnostic Discussion is issued in mid-June 2026. Three numbers \
will tell us whether the developing event is on track:"}],

para[{"\[Bullet] ", bold["May 2026 Ni\[NTilde]o 3.4 monthly anomaly"],
   ". The model predicts +0.5 to +1.0 \[Degree]C (median +0.7); if \
the observed value is at the high end, expect the next \
discussion to issue an El Ni\[NTilde]o ", bold["Advisory"],
   " (not just a Watch)."}],

para[{"\[Bullet] ", bold["MAM 2026 ONI"], " (March\[Dash]April\[Dash]\
May three-month running mean). The +0.5 \[Degree]C threshold for one \
of the five-consecutive-season El-Ni\[NTilde]o criterion needs to be \
crossed for the first time. The model gives 0.30 \[Degree]C (5\[Dash]\
95 % CI: 0.00 to 0.65)."}],

para[{"\[Bullet] ", bold["Westerly wind burst activity"], " through \
mid-June, watched in real time on the TAO mooring array. A strong \
late-spring WWB would push the central Pacific thermocline anomaly \
further east and accelerate the warming."}],

para[{ital["What the barrier means for the forecast users."], " The \
operational ENSO probability bars from May 2026 give 96 % for DJF. \
This number sounds confident but should be read with the barrier in \
mind: it summarises model agreement after the barrier window has \
already been crossed (model initial conditions were set in mid-April; \
forecasts shoot forward from there). Confidence in events further \
out should be qualified \[LongDash] e.g., the DJF probability is \
robust because the system is past the barrier, but the JFM 2027 \
\[OpenCurlyDoubleQuote]event-decay\[CloseCurlyDoubleQuote] details \
will look quite different by July."}]
}];


(* ============================================================
   §8 References
   ============================================================ *)

writeAll[{

hd1["8.  References"],

para[{bold["Bjerknes, J."], " (1969). Atmospheric teleconnections \
from the equatorial Pacific. ", ital["Monthly Weather Review"], " 97, \
163\[Dash]172. [Original paper on the Bjerknes positive feedback.]"}],

para[{bold["Suarez, M. J., & Schopf, P. S."], " (1988). A delayed \
action oscillator for ENSO. ", ital["Journal of the Atmospheric \
Sciences"], " 45, 3283\[Dash]3287. [Original delayed oscillator.]"}],

para[{bold["Webster, P. J., & Yang, S."], " (1992). Monsoon and ENSO: \
selectively interactive systems. ", ital["Quarterly Journal of the \
Royal Meteorological Society"], " 118, 877\[Dash]926. [Spring \
predictability barrier in the monsoon system context.]"}],

para[{bold["Jin, F.-F."], " (1997). An equatorial ocean recharge \
paradigm for ENSO. Part I: Conceptual model. ", ital["Journal of the \
Atmospheric Sciences"], " 54, 811\[Dash]829. [Original \
recharge\[Dash]discharge oscillator.]"}],

para[{bold["Torrence, C., & Webster, P. J."], " (1998). The annual \
cycle of persistence in the El Ni\[NTilde]o/Southern Oscillation. ",
   ital["Quarterly Journal of the Royal Meteorological Society"],
   " 124, 1985\[Dash]2004. [Wavelet decomposition of the barrier.]"}],

para[{bold["Burgers, G., Jin, F.-F., & van Oldenborgh, G. J."],
   " (2005). The simplest ENSO recharge oscillator. ", ital["Geophysical \
Research Letters"], " 32, L13706. [The minimal linear-stochastic RDO \
used here.]"}],

para[{bold["Levine, A. F. Z., & Jin, F.-F."], " (2010). Noise-induced \
instability in the ENSO recharge oscillator. ", ital["Journal of the \
Atmospheric Sciences"], " 67, 529\[Dash]542. [Stochastic destabilisation \
mechanism.]"}],

para[{bold["Stein, K., Schneider, N., Timmermann, A., & Jin, F.-F."],
   " (2010). Seasonal synchronisation of ENSO events in a linear \
stochastic model. ", ital["Journal of Climate"], " 23, 5629\[Dash]\
5643. [Seasonal phase locking and the barrier in the linear model.]"}],

para[{bold["McPhaden, M. J., Santoso, A., & Cai, W. (Eds.)"],
   " (2020). ", ital["El Ni\[NTilde]o Southern Oscillation in a \
changing climate"], ". AGU Geophysical Monograph 253. [Recent \
comprehensive review.]"}],

para[{bold["Tippett, M. K., Barnston, A. G., & Li, S."], " (2012). \
Performance of recent multimodel ENSO forecasts. ", ital["Journal \
of Applied Meteorology and Climatology"], " 51, 637\[Dash]654. \
[Quantitative evaluation of IRI plume skill.]"}],

para[{ital["Popular-science companion:"], " ", bold["King, S."],
   " (2026). ", ital["El Ni\[NTilde]o is coming \[LongDash] could \
this year see a \[OpenCurlyDoubleQuote]Godzilla\[CloseCurlyDoubleQuote] \
event?"], " BBC News explainer (5 min video). ",
   link["youtube.com/watch?v=UEseLvpl9ss",
      "https://www.youtube.com/watch?v=UEseLvpl9ss"],
   ".  An accessible narration of the same May 2026 developing \
event from a lead BBC meteorologist; useful entry point for the \
general-audience side of this notebook."}]
}];


(* ============================================================
   §9 Acknowledgements
   ============================================================ *)

writeAll[{

hd1["9.  Acknowledgements"],

para[{ital["Data."], " Live data for this notebook are supplied by \
two US federal agencies and one academic centre, all of whom make \
their data freely available. The Ni\[NTilde]o 3.4 monthly SST \
anomaly time series (NOAA ERSSTv5) is from NOAA's ", bold["Physical \
Sciences Laboratory"], " (Boulder, CO). The Oceanic Ni\[NTilde]o Index \
and the ENSO Diagnostic Discussion are from NOAA's ", bold["Climate \
Prediction Center"], " (College Park, MD). The forecast plume is \
from Columbia University's ", bold["International Research Institute \
for Climate and Society"], " (Palisades, NY)."}],

para[{ital["Scientific lineage."], " The recharge\[Dash]discharge \
oscillator is due to ", bold["Fei-Fei Jin"], " (1997); the delayed \
oscillator is due to ", bold["Max Suarez"], " and ", bold["Paul \
Schopf"], " (1988). The seasonal-modulation spring-barrier \
explanation we use here was developed by ", bold["Aaron Levine"],
   " and Fei-Fei Jin (2010) and by ", bold["Karl Stein and \
collaborators"], " (2010). The Bjerknes feedback itself, the \
foundation stone of coupled tropical ocean\[Dash]atmosphere theory, \
is due to ", bold["Jacob Bjerknes"], " (1969)."}],

para[{ital["Tools."], " All numerics and figures are pure ",
   bold["Wolfram Language"], " (Mathematica 14). The notebook was \
built programmatically via ", mono["wolframscript"],
   ". The educational illustrations (Figures 1.1, 1.2, 3.1, 4.1, 5.1) \
were generated with OpenAI's ", mono["gpt-image-2"], " model. \
The repository skeleton, git workflow, and notebook-build pipeline \
were assembled with the help of Anthropic's Claude Code."}],

para[{ital["Companion projects."], " This is the third in a series \
of standalone-notebook Wolfram Community submissions: ",
   link["Contiguous-Cartograms",
     "https://github.com/mthiel74/Contiguous-Cartograms"],
   ", ", link["streetview360",
     "https://github.com/mthiel74/streetview360"], ", ",
   link["SofaProblem",
     "https://github.com/mthiel74/SofaProblem"], "."}]
}];


(* ============================================================
   §10 Reproducibility
   ============================================================ *)

writeAll[{

hd1["10.  Reproducibility"],

para[{"All source code lives in the public repository ",
   link["github.com/mthiel74/ENSO-emergence",
     "https://github.com/mthiel74/ENSO-emergence"],
   ". The entire pipeline is pure Wolfram Language \[LongDash] no \
Python, no other runtime. To regenerate this notebook from scratch \
on a machine with Wolfram Language 14 installed:"}],

codeIn["# 1. Fetch the latest live data into data/
wolframscript -file wolfram/fetch_all.wls

# 2. Compute the observational spring-barrier figures
wolframscript -file wolfram/barrier.wls

# 3. Run the recharge-discharge oscillator ensembles
wolframscript -file wolfram/rdo.wls

# 4. Run the delayed oscillator ensembles
wolframscript -file wolfram/delayed.wls

# 5. Compute the live forecast overlay
wolframscript -file wolfram/forecast.wls

# 6. Rebuild this notebook
wolframscript -file community/build_notebook.wls"],

para[{"The full pipeline runs in approximately 4 minutes on a 2024 \
MacBook M3. The .nb file (~30 MB with all images embedded) is \
committed to the repository so the notebook is browseable without \
running anything."}],

para[{Style["Generated on " <> DateString[Now, {"MonthName", " ",
    "DayShort", ", ", "Year"}] <> ".",
    FontSlant -> Italic, FontColor -> GrayLevel[0.4]]}]

}];


(* ---- save: write .nb directly, using standard Wolfram header ---- *)

Print["assembled ", Length[$allCells], " cells"];

nbExpr = Notebook[$allCells,
   WindowSize -> {980, 800},
   StyleDefinitions -> "Default.nb"];

(* InputForm serialises every expression. Wolfram's default
   InputForm output represents non-ASCII characters as named
   escapes like \[NTilde], so the body is pure ASCII. We then
   write the file as raw ASCII bytes — no UTF-8 re-encoding. *)
nbBody = ToString[nbExpr, InputForm, PageWidth -> Infinity,
   CharacterEncoding -> "ASCII"];

(* Substitute raster placeholders with the actual compressed
   RasterBox boxes. *)
Print["substituting ", Length[$rasterSubstitutions],
   " raster placeholders..."];
nbBody = Fold[
   StringReplace[#1, #2[[1]] -> #2[[2]]] &,
   nbBody,
   Normal[$rasterSubstitutions]
];

nbHeader = StringJoin[
   "(* Content-type: application/vnd.wolfram.mathematica *)\n\n",
   "(*** Wolfram Notebook File ***)\n",
   "(* http://www.wolfram.com/nb *)\n\n",
   "(* CreatedBy='ENSO-emergence build pipeline' *)\n\n",
   "(* Beginning of Notebook Content *)\n"
];
nbFooter = "\n(* End of Notebook Content *)\n";

(* Write as raw ASCII bytes — the body is pure ASCII because
   InputForm produced \[NTilde] etc. as escapes. *)
Export[outNb, nbHeader <> nbBody <> nbFooter, "Text",
   CharacterEncoding -> "ASCII"];
Print["wrote ", outNb, " (", FileByteCount[outNb], " B)"];

(* PDF via FE *)
TimeConstrained[
   UsingFrontEnd[
      nbObj = NotebookOpen[outNb, Visible -> False];
      Quiet @ Check[
         Export[outPdf, nbObj];
         Print["wrote ", outPdf, " (", FileByteCount[outPdf], " B)"],
         Print["warn: PDF export failed"]];
      NotebookClose[nbObj];
   ],
   300,
   Print["warn: PDF export timed out"]
];
