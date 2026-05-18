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

(* Static image cell: load via Import + Image[..., ImageSize -> ...]
   and let Wolfram's native NB serialization compress the raster.
   Canonical pattern from reference_wolfram_community_pattern (used in
   GaltonBoard, ActualSizeofCountries, HistoricalWeather builders). *)
imgWidth = 640;

imgCell[file_, opts___] := Module[
   {path = FileNameJoin[{imgDir, file}], small, im},
   If[FileExistsQ[path],
     small = resizedCopy[path];
     im = Image[Import[small], ImageSize -> imgWidth];
     Cell[BoxData @ ToBoxes[im], "Output",
        ShowCellLabel -> False,
        TextAlignment -> Center,
        CellMargins -> {{Automatic, Automatic}, {10, 6}}],
     Cell[
       TextData[{bold["[ missing figure: " <> file <> " ]"]}],
       "Text", FontColor -> RGBColor[0.7, 0.1, 0.1],
       TextAlignment -> Center]
   ]
];

(* animCell — embed a GIF as an inline AnimatedImage cell.
   Wolfram Community's web renderer drops Video[] cells but preserves
   AnimatedImage as a looping <img> tag, so this is the canonical
   pattern for hook videos that actually play on the post page (see
   reference_wolfram_community_pattern memory note + GaltonBoard,
   ActualSizeofCountries, Wolfram3DObjects builders).

   ColorQuantize per frame keeps the embedded notebook small —
   without it the same 15-frame 700-px-wide sequence pushed the .nb
   over 20 MB. *)
animWidth = 640;

animCell[file_] := animCell[file, ""];
animCell[file_String, caption_] := Module[
   {path = FileNameJoin[{imgDir, file}], all, anim, boxes},
   If[!FileExistsQ[path],
     Return @ Cell[
       TextData[{bold["[ missing animation: " <> file <> " ]"]}],
       "Text", FontColor -> RGBColor[0.7, 0.1, 0.1],
       TextAlignment -> Center]];
   all = Import[path, {"GIF", "ImageList"}];
   (* Resize + 256-colour quantise each frame *)
   all = ColorQuantize[ImageResize[#, {animWidth}], 256] & /@ all;
   anim = AnimatedImage[all,
      FrameRate -> 1.25,
      AnimationRunning -> True,
      AnimationRepetitions -> Infinity];
   boxes = ToBoxes[
      If[caption === "",
         anim,
         Column[{
            anim,
            Pane[Style[caption, Italic, FontSize -> 10,
               FontFamily -> "Helvetica", FontColor -> GrayLevel[0.35]],
               animWidth, Alignment -> Center]
         }, Alignment -> Center, Spacings -> 0.6]]];
   Cell[BoxData[boxes], "Output",
      ShowCellLabel -> False,
      TextAlignment -> Center,
      CellMargins -> {{Automatic, Automatic}, {10, 14}}]
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

(* "Program" cell — fixed-width text, no Wolfram syntax-highlighting,
   safe for shell snippets, URLs, etc. *)
codeIn[code_String] := Cell[code, "Program",
   CellMargins -> {{60, 30}, {6, 6}},
   FontFamily -> "Courier", FontSize -> 11,
   ShowStringCharacters -> False,
   Background -> RGBColor[0.95, 0.96, 0.98]];

(* Wolfram-Language Input cell — runnable, syntax-highlighted. Use
   wlIn for the snippets that actually generate each figure, so a
   reader following the post on Wolfram Community can shift-Enter
   each one and see the same plot. *)
wlIn[code_String] := Cell[code, "Input",
   CellMargins -> {{60, 30}, {6, 6}},
   FontSize -> 11];

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

(* Hero animation: 15-month time-lapse, immediately under the title.
   Uses AnimatedImage so the embedded animation plays inline both in
   the Wolfram FrontEnd and on the Wolfram Community post page. *)
animCell["hero_satellite.gif",
   "Cover animation.  15-month time-lapse of the NOAA ERSSTv5 monthly \
SST anomaly field over the tropical Pacific (Feb 2025 \[Rule] Apr \
2026), overlaid at 65 % opacity on Wolfram's built-in satellite \
imagery. The Ni\[NTilde]o 3.4 monitoring box is outlined in yellow \
at the equator; the current basin-mean Ni\[NTilde]o 3.4 anomaly \
appears in the upper-left badge. Watch the 2024\[Dash]25 La \
Ni\[NTilde]a cold tongue dissolve through autumn 2025, then the \
central Pacific warm through winter and propagate eastward through \
spring 2026. The full code that produced this animation is given in \
\[Section]3.5."],


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
out until early 2027. The bar chart is built directly from the \
snapshot JSON written by ", mono["wolfram/fetch_cpc_probabilities.wls"],
   ":"}],

wlIn["(* Load the shared data-access helpers (LoadNino34, LoadONI, \
LoadProbSnapshot, ...) defined in wolfram/load_data.wl *)
Get[FileNameJoin[{NotebookDirectory[], \"..\", \"wolfram\", \"load_data.wl\"}]];

(* Figure 2.1: NOAA / IRI ENSO probability bars *)
probData = ENSOData`LoadProbSnapshot[];
{seasons, laNina, neutral, elNino} = Transpose[
   {#[\"season\"], #[\"la_nina\"], #[\"neutral\"], #[\"el_nino\"]} & /@
   probData[\"probabilities\"]];
BarChart[
   Transpose[{laNina, neutral, elNino}],
   ChartLayout -> \"Stacked\",
   ChartStyle -> {RGBColor[0.20, 0.45, 0.75],   (* La Niña blue *)
                  RGBColor[0.6, 0.6, 0.6],      (* Neutral grey *)
                  RGBColor[0.83, 0.32, 0.10]},  (* El Niño red  *)
   ChartLabels -> {Style[#, FontSize -> 9] & /@ seasons, None},
   ChartLegends -> Placed[
     {\"La Ni\\[NTilde]a\", \"Neutral\", \"El Ni\\[NTilde]o\"}, Right],
   Frame -> True,
   FrameLabel -> {{\"probability (%)\", None},
                  {\"rolling 3-month season\", None}},
   PlotRange -> {Automatic, {0, 100}},
   AspectRatio -> 0.45, ImageSize -> 900]"],

imgCell["noaa_probabilities.png"],
captionCell["Figure 2.1  Official NOAA / IRI ENSO probabilities for \
the next nine rolling 3-month seasons, as issued in the May 2026 \
ENSO Diagnostic Discussion. Red = El Ni\[NTilde]o (Ni\[NTilde]o 3.4 \
ONI \[GreaterEqual] +0.5 \[Degree]C); grey = Neutral; blue = La \
Ni\[NTilde]a (ONI \[LessEqual] \[Minus]0.5 \[Degree]C). Each column \
sums to 100 %.  ", ital["Note:"], " unlike the Ni\[NTilde]o 3.4 \
anomaly series and the ONI, these probability bars are not available \
in any machine-readable feed and were transcribed by hand from the \
May 2026 CPC PNG bar chart; they do not auto-refresh in subsequent \
runs of the pipeline."],

para[{"This Watch is striking for two reasons. (a) It comes just six \
months after the end of the 2024\[Dash]25 La Ni\[NTilde]a (ONI \
bottomed at \[Minus]0.55 in November 2025). Such a rapid \
La\[Dash]Ni\[NTilde]a-to-El-Ni\[NTilde]o transition is unusual but not \
unprecedented (cf. 1972, 1976, 2009 transitions). (b) The probability \
of persistence to DJF is exceptionally high (96 %); for context, the \
operationally similar May 2023 discussion gave a 90 % probability for \
DJF 2023\[Dash]24, and the 2023\[Dash]24 super-El-Ni\[NTilde]o indeed \
materialised."}],

wlIn["(* Figure 2.2: full Niño 3.4 monthly anomaly record from NOAA PSL.
   ENSOData`LoadNino34 parses the ASCII anomaly file fetched by
   wolfram/fetch_nino34.wls into a {date, anom} pair list. *)
n34 = ENSOData`LoadNino34[];
DateListPlot[
   Transpose[{n34[\"date\"], n34[\"anom\"]}],
   Joined -> True,
   PlotStyle -> Directive[Thickness[0.0035], RGBColor[0.85, 0.32, 0.10]],
   Filling -> 0,
   FillingStyle -> Directive[Opacity[0.25], RGBColor[0.85, 0.32, 0.10]],
   Frame -> True,
   FrameLabel -> {{\"Ni\\[NTilde]o 3.4 anomaly (\\[Degree]C)\", None},
                  {\"year\", None}},
   PlotLabel -> \"Monthly Ni\\[NTilde]o 3.4 SST anomaly (ERSSTv5, NOAA PSL)\",
   GridLines -> {None, {-0.5, 0, 0.5}},
   GridLinesStyle -> Directive[Gray, Dashed],
   ImageSize -> 1100, AspectRatio -> 1/3.4]"],

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
(\[Minus]0.27 \[RightArrow] +0.23) sits comfortably in the top decile \
of all Feb\[Dash]Apr changes since 1950 (ranked 9th of 77, tied with \
1963 \[LongDash] the top three are 1951 at +0.81, 2014 at +0.70, and \
2000 at +0.68). It is exactly the kind of emergent behaviour that the \
toy models in \[Section]5 are designed \
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
   §3.5 Spatial dynamics — the 12-month time-lapse
   ============================================================ *)

writeAll[{

hd1["3.5  Spatial dynamics: 15 months of the basin"],

para[{"The Ni\[NTilde]o 3.4 box-mean used everywhere else in this \
notebook collapses the entire equatorial Pacific into a single \
scalar per month. That is the right summary statistic for indices \
and forecasts, but it hides everything ", ital["spatial"],
   " about what is going on \[LongDash] in particular, ",
   bold["where"], " in the basin the warm anomaly is accumulating and \
how the cold tongue is dissolving. To make the spatial story \
visible, we render the most recent 15 months of the NOAA ERSSTv5 \
gridded monthly anomaly field, overlaid on Wolfram's built-in \
satellite imagery for the same region. Latitudes 40\[Degree]S to \
40\[Degree]N and longitudes 110\[Degree]E to 50\[Degree]W so that \
the western Maritime Continent, Australia, and both American \
coastlines are all visible for context:"}],

para[{ital["How this was made."], " The grid we are plotting is the \
", mono["ssta"], " variable from the NCEI ERSSTv5 monthly NetCDFs \
(2\[Degree]\[Times]2\[Degree], anomaly vs the 1971\[Dash]2000 \
climatology embedded by NCEI). One file per month, fetched from"}],

codeIn["https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/ersst.v5.YYYYMM.nc"],

para[{"by the helper script ", mono["wolfram/fetch_ersst_grid.wls"],
   " into ", mono["data/raw/"],
   ". The frame renderer (full implementation: ",
   mono["wolfram/hero_satellite.wls"],
   ", ~120 lines) loads the NetCDF, builds one ",
   mono["GeoPolygon"],
   " per 2\[Degree]\[Times]2\[Degree] cell coloured by the anomaly, \
and renders with the satellite background:"}],

codeIn["(* Load one month of gridded SST anomaly from the NetCDF *)
ssta = Normal @ Import[file, {\"NetCDF\", \"Data\", \"ssta\"}];

(* Build one geographic polygon per 2-degree-by-2-degree cell,
   coloured by the cell's anomaly value *)
polygons = Flatten @ Table[
   With[{v = ssta[[1, 1, latIdx[[i]], lonIdx[[j]]]]},
      If[v < -10., Nothing,    (* skip land sentinel *)
         {divergingCmap[v], Opacity[0.65], EdgeForm[None],
          GeoPolygon[{
             {latSub[[i]] - 1, lonTo180[lonSub[[j]] - 1]},
             {latSub[[i]] - 1, lonTo180[lonSub[[j]] + 1]},
             {latSub[[i]] + 1, lonTo180[lonSub[[j]] + 1]},
             {latSub[[i]] + 1, lonTo180[lonSub[[j]] - 1]}}]}]],
   {i, nLat}, {j, nLon}];

(* Render with satellite background via GeoGraphics *)
frame = GeoGraphics[
   {polygons,
    Directive[Yellow, Thick], GeoPath[{
       {5, -170}, {5, -120}, {-5, -120}, {-5, -170}, {5, -170}}]},
   GeoRange      -> {{-40, 40}, {110, -50}},
   GeoProjection -> \"Equirectangular\",
   GeoBackground -> \"Satellite\",
   ImageSize     -> {1100, 580}];

(* Stitch 15 monthly frames into MP4 + GIF *)
Export[\"hero_satellite.gif\",
   AnimatedImage[Rasterize /@ frames, FrameRate -> 1.25], \"GIF\"];
Export[\"hero_satellite.mp4\",
   AnimatedImage[Rasterize /@ frames, FrameRate -> 1.25], \"MP4\"];"],

para[{"Running the above produces the following animation \[LongDash] \
the same one used as the cover figure of this post, reproduced here \
with a different caption:"}],

animCell["hero_satellite.gif",
   "Figure 3.5.1  15-month NOAA ERSSTv5 SST-anomaly time-lapse, \
February 2025 \[Rule] April 2026, overlaid at 65 % opacity on \
Wolfram's satellite basemap. The Ni\[NTilde]o 3.4 monitoring box \
(5\[Degree]N\[Dash]5\[Degree]S, 170\[Degree]W\[Dash]120\[Degree]W) \
is outlined in yellow; the basin-mean Ni\[NTilde]o 3.4 anomaly is in \
the upper-left badge. Colour scale: bright red \[Tilde] +3 \
\[Degree]C, deep blue \[Tilde] \[Minus]3 \[Degree]C, white = 0."],

para[{ital["What the animation shows."], " The early frames (spring \
/summer 2025) sit in the tail end of the 2024\[Dash]25 La \
Ni\[NTilde]a: a cool band along the equator between the dateline \
and the South American coast, with the warm pool concentrated west \
of 160\[Degree]E. Through autumn 2025 the cold tongue weakens but \
stays present; the basin remains sub-thermal. The shift begins in \
December 2025\[Dash]January 2026: the central Pacific (around \
170\[Degree]W) warms first, then the warm signature propagates \
eastward along the equator through Feb\[Dash]Mar\[Dash]Apr 2026. By \
the final frame the eastern Pacific cold tongue has retreated to a \
thin sliver against the South American coast, and the basin-mean is \
+0.37 \[Degree]C. This is the spatial manifestation of the \
recharge\[Dash]discharge cycle described in \[Section]5.1 and \
animated in the toy model in Figures 5.3\[Dash]5.4."}],

para[{ital["Note on resolution."], " ERSSTv5 is a 2\[Degree]\
\[Times]2\[Degree] gridded product designed for long-term climate \
analysis (data back to 1854). The visible blockiness of the overlay \
is the underlying grid; finer-resolution products like NOAA OISST \
(0.25\[Degree], available daily) exist but at the cost of a much \
shorter homogeneous historical record. For the present analysis the \
coarser ERSSTv5 grid suffices, and the geographic context provided \
by the satellite basemap (the visible coastlines of Asia, Australia, \
and the Americas) makes the location of the equatorial cold tongue \
unambiguous to a non-specialist reader."}]
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

wlIn["(* Figure 4.2: observed lagged autocorrelation rho(m0, lead).
   Flatten the monthly anomalies into a single vector indexed by
   (year - y0)*12 + month so lead lookups are O(1) offsets, then
   correlate for each (initial-month, lead) pair. *)
n34 = ENSOData`LoadNino34[];
years = Sort @ DeleteDuplicates @ n34[\"year\"];
{y0, y1} = MinMax[years];
nMonths = (y1 - y0 + 1)*12;
flat = ConstantArray[Missing[\"NotAvailable\"], nMonths];
MapThread[
   (flat[[(#1 - y0)*12 + #2]] = #3) &,
   {n34[\"year\"], n34[\"month\"], n34[\"anom\"]}];

pairAt[m1_, tau_] := Module[{idx0, pairs},
   idx0 = Range[m1, nMonths - tau, 12];
   pairs = Transpose[{flat[[idx0]], flat[[idx0 + tau]]}];
   Select[pairs, And @@ (NumberQ /@ #) &]];

rhoMatrix = Table[
   With[{p = pairAt[m1, tau]},
      If[Length[p] < 4, 0., Correlation[p[[All, 1]], p[[All, 2]]]]],
   {tau, 0, 15}, {m1, 1, 12}];

(* Heatmap: rows = lead 0..15 from top, columns = Jan..Dec *)
months = {\"Jan\",\"Feb\",\"Mar\",\"Apr\",\"May\",\"Jun\",
          \"Jul\",\"Aug\",\"Sep\",\"Oct\",\"Nov\",\"Dec\"};
cmap = Blend[{
   {-0.2, RGBColor[0.65, 0.10, 0.10]},
   { 0.0, RGBColor[0.90, 0.50, 0.40]},
   { 0.3, RGBColor[0.97, 0.97, 0.95]},
   { 0.6, RGBColor[0.35, 0.55, 0.85]},
   { 1.0, RGBColor[0.05, 0.20, 0.55]}}, #] &;
ArrayPlot[rhoMatrix,
   ColorFunction -> cmap, ColorFunctionScaling -> False,
   Frame -> True,
   FrameLabel -> {{\"initial calendar month m\\:2080\", None},
                  {\"lead (months)\", None}},
   FrameTicks -> {
     {Table[{tau + 1, ToString[tau]}, {tau, 0, 15}],
      Table[{tau + 1, ToString[tau]}, {tau, 0, 15}]},
     {Table[{m, months[[m]]}, {m, 12}],
      Table[{m, months[[m]]}, {m, 12}]}},
   Mesh -> All, MeshStyle -> Directive[GrayLevel[0.85]],
   ImageSize -> 820, AspectRatio -> 1.05]"],

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

wlIn["(* Figure 4.3: persistence-skill curves, one per starting month *)
ListLinePlot[
   Table[
      Tooltip[
         Transpose[{Range[0, 15], rhoMatrix[[All, m1]]}],
         months[[m1]]],
      {m1, 12}],
   PlotRange -> {{0, 15}, {-0.2, 1.05}},
   Frame -> True,
   FrameLabel -> {{\"autocorrelation \\[Rho]\", None},
                  {\"lead (months from m\\:2080)\", None}},
   GridLines -> {Automatic, {0, 0.5}},
   GridLinesStyle -> Directive[Gray, Dashed],
   PlotLegends -> Placed[months, Right],
   ImageSize -> 900, AspectRatio -> 0.55]"],

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
(spring WWB activity), giving the model an in-built spring barrier. \
The two cycles together:"}],

wlIn["(* Figure 5.2: seasonal cycles of R(t) and sigma_w(t).
   Parameters live at the top of wolfram/rdo.wls. *)
R0 = -0.03; R1 = 0.18; phiR = 9.0;
sigW0 = 0.22; aW = 0.80; phiW = 3.0;
Rseason[t_, m0_]  := R0 + R1 Cos[2 Pi (t + m0 - phiR)/12];
sigWWB[t_, m0_]   := sigW0 (1 + aW Cos[2 Pi (t + m0 - phiW)/12]);
months = {\"Jan\",\"Feb\",\"Mar\",\"Apr\",\"May\",\"Jun\",
          \"Jul\",\"Aug\",\"Sep\",\"Oct\",\"Nov\",\"Dec\"};
Plot[{Rseason[t - 1, 1], sigWWB[t - 1, 1]}, {t, 1, 13},
   PlotStyle -> {RGBColor[0.05, 0.20, 0.55], RGBColor[0.85, 0.32, 0.10]},
   PlotLegends -> {\"R(t)  Bjerknes feedback\", \"\\[Sigma]_w(t)  WWB std-dev\"},
   Frame -> True, Axes -> False,
   FrameTicks -> {{Automatic, None},
      {Table[{m, months[[m]]}, {m, 12}], None}},
   FrameLabel -> {{\"value (month\\[Minus]\\:00b9 or \\[Degree]C)\", None},
                  {\"calendar month\", None}},
   GridLines -> {None, {0}},
   ImageSize -> 720, AspectRatio -> 0.55]"],

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

wlIn["(* Figure 5.3: a single 20-year RDO realisation.
   The full simulate[] above is bundled in wolfram/rdo.wls; the
   constants gamma, rDamp, alpha, sigmaT, sigmaH are the same as in
   Figure 5.2 plus gamma=0.06, rDamp=0.04, alpha=0.15, sigmaT=0.06,
   sigmaH=0.04. *)
Get[FileNameJoin[{NotebookDirectory[], \"..\", \"wolfram\", \"rdo.wls\"}]];
samp = simulate[0.3, 0.6, 1, 12*20, 0.05, 17];
ListLinePlot[Transpose[{samp[\"t\"]/12., samp[\"T\"]}],
   PlotStyle -> Directive[Thickness[0.003], RGBColor[0.83, 0.32, 0.10]],
   Frame -> True,
   FrameLabel -> {{\"T anomaly (\\[Degree]C)\", None}, {\"years\", None}},
   PlotRange -> {Automatic, {-3.5, 3.5}},
   GridLines -> {Automatic, {-0.5, 0, 0.5}},
   ImageSize -> 950, AspectRatio -> 1/3.2]"],

imgCell["rdo_timeseries.png"],
captionCell["Figure 5.3  Single 20-year stochastic realisation of the \
recharge\[Dash]discharge oscillator with seasonally modulated \
coefficients. Period is irregular (3\[Dash]6 years) and amplitude is \
variable, as in observations."],

wlIn["(* Figure 5.4: phase-space orbit of (T, h) from the same run *)
ListLinePlot[Transpose[{samp[\"T\"], samp[\"h\"]}],
   PlotStyle -> Directive[Thickness[0.0012], Opacity[0.55],
      RGBColor[0.20, 0.40, 0.65]],
   Frame -> True,
   FrameLabel -> {{\"thermocline anomaly h\", None},
                  {\"SST anomaly T (\\[Degree]C)\", None}},
   GridLines -> {{0}, {0}},
   ImageSize -> 480, AspectRatio -> 1]"],

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

wlIn["(* Figure 5.5: 4-panel ensemble cone, init Jan/Apr/Jul/Oct.
   forecastEnsemble[m0] runs 500 stochastic copies of simulate[]
   from climatological initial (T,h) for that month and returns
   per-lead percentiles. Full helper in wolfram/rdo.wls. *)
Get[FileNameJoin[{NotebookDirectory[], \"..\", \"wolfram\", \"rdo.wls\"}]];

panel[m0_] := With[{f = forecastEnsemble[m0]},
   Show[
      ListLinePlot[
         {Transpose[{f[\"t\"], f[\"p05\"]}], Transpose[{f[\"t\"], f[\"p95\"]}]},
         Filling -> {1 -> {{2}, Directive[Opacity[0.18],
            RGBColor[0.83, 0.32, 0.10]]}}],
      ListLinePlot[
         {Transpose[{f[\"t\"], f[\"p25\"]}], Transpose[{f[\"t\"], f[\"p75\"]}]},
         Filling -> {1 -> {{2}, Directive[Opacity[0.45],
            RGBColor[0.83, 0.32, 0.10]]}}],
      ListLinePlot[Transpose[{f[\"t\"], f[\"median\"]}],
         PlotStyle -> Directive[Thickness[0.005], Black]],
      Frame -> True, PlotRange -> {{0, 18}, {-3, 3}},
      PlotLabel -> \"init \" <> monthNames[[m0]],
      AspectRatio -> 0.7, ImageSize -> 520]];

Grid[Partition[panel /@ {1, 4, 7, 10}, 2], Spacings -> {1.5, 1.5}]"],

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

wlIn["(* Figure 5.6: ensemble std-dev as f(initial month, lead).
   Runs forecastEnsemble[] for every m0 in 1..12, extracts the per-
   lead StandardDeviation, and plots the {12, 16} matrix as a heatmap. *)
spreadMatrix = Table[
   With[{e = forecastEnsemble[m0, 250, 15, 0.1]},
      e[\"std\"][[Table[FirstPosition[e[\"t\"], _?(# >= tau - 0.05 &)][[1]],
                       {tau, 0, 15}]]]],
   {k, Length @ leadIdx}, {m0, 1, 12}];
ArrayPlot[spreadMatrix,
   ColorFunction -> \"TemperatureMap\",
   Frame -> True,
   FrameLabel -> {{\"lead (months)\", None},
                  {\"initial calendar month m\\:2080\", None}},
   FrameTicks -> {
      {Table[{tau + 1, ToString[tau]}, {tau, 0, 15}], None},
      {Table[{m, months[[m]]}, {m, 12}], None}},
   PlotLegends -> Placed[BarLegend[Automatic,
      LegendLabel -> \"ensemble std (\\[Degree]C)\"], Right],
   Mesh -> All, ImageSize -> 820, AspectRatio -> 1.05]"],

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

para[{ital["Bifurcation structure."], " Linearising about ",
   math["T = 0"], " and substituting ", math["T \[Tilde] e^(\[Lambda] t)"],
   " gives the characteristic equation ",
   math["\[Lambda] = 1 \[Minus] \[Alpha] e^(\[Minus] \[Lambda] \[Delta])"],
   ". This is transcendental and admits infinitely many complex \
eigenvalues. At ", math["\[Alpha] = 1"], " the real part of the \
leading (real) eigenvalue passes through zero \[LongDash] this is the \
pitchfork transition, marking the loss of stability of the trivial \
fixed point. For ", math["\[Alpha] < 1"], " the origin is unstable in \
the deterministic system and the cubic damping ", math["\[Minus] T^3"],
   " saturates trajectories near ", math["T = \[PlusMinus]1"], "; \
adding the delayed feedback at finite ", math["\[Delta]"], " converts \
the would-be stable fixed points into sustained limit-cycle \
oscillations of period ", math["\[TildeTilde] 4 \[Delta]"], " through \
the original Suarez\[Dash]Schopf mechanism. Below we sweep ",
   math["\[Alpha]"], " at fixed ", math["\[Delta] = 3"],
   " and report the amplitude envelope from stochastic runs."}],

wlIn["(* Figure 5.7: bifurcation sweep — min/max of T as a function of
   the wave-coupling alpha, at fixed delta = 3.  simulateDDE[] (shown
   in full below) is the same Euler-Maruyama DDE integrator used for
   the other delayed-osc panels; full implementation in
   wolfram/delayed.wls. *)
Get[FileNameJoin[{NotebookDirectory[], \"..\", \"wolfram\", \"delayed.wls\"}]];

alphaList = Range[0.3, 1.15, 0.05];
bifurc = Table[
   Block[{alphaDel = aL},
      With[{r = simulateDDE[1, 600., 0.1, 0.3, 13]},
         {aL, MinMax @ Drop[r[\"T\"], Round[200./0.1]]}]],
   {aL, alphaList}];
Show[
   ListPlot[{#1, #2[[1]]} & @@@ bifurc,
      PlotStyle -> Directive[PointSize[0.012], RGBColor[0.20, 0.45, 0.75]]],
   ListPlot[{#1, #2[[2]]} & @@@ bifurc,
      PlotStyle -> Directive[PointSize[0.012], RGBColor[0.85, 0.32, 0.10]]],
   Frame -> True,
   FrameLabel -> {{\"min / max of T (steady state)\", None},
                  {\"wave coupling \\[Alpha]\", None}},
   GridLines -> {{0.5, 1.0}, {0}},
   ImageSize -> 720, AspectRatio -> 0.55]"],

imgCell["delayed_bifurcation.png"],
captionCell["Figure 5.7  Min and max of T in long stochastic runs of \
the delayed oscillator, as a function of the wave-coupling ",
   math["\[Alpha]"], ", with ", math["\[Delta] = 3"],
   " (= 6 months) held fixed. The amplitude grows continuously from \
near zero at small ", math["\[Alpha]"],
   " (where the cubic damping holds the trajectory close to \
", math["T = \[PlusMinus]1"], ") to a full \[PlusMinus]1.2 swing at \
", math["\[Alpha] = 0.75\[Dash]1.0"], "."],

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

wlIn["(* Figure 5.8: a 20-year realisation at alpha=0.75, delta=6 mo *)
alphaDel = 0.75; delayTau = 3.0;
samp = simulateDDE[1, 20*12/2., 0.05, 0.3, 7];
ListLinePlot[Transpose[{samp[\"t\"]*2./12., samp[\"T\"]}],
   PlotStyle -> Directive[Thickness[0.003], RGBColor[0.20, 0.45, 0.75]],
   Frame -> True,
   FrameLabel -> {{\"T anomaly\", None}, {\"years\", None}},
   PlotRange -> {Automatic, {-1.6, 1.6}},
   GridLines -> {Automatic, {-0.5, 0, 0.5}},
   ImageSize -> 950, AspectRatio -> 1/3.2]"],

imgCell["delayed_timeseries.png"],
captionCell["Figure 5.8  A 20-year realisation of the delayed \
oscillator with ", math["\[Alpha] = 0.75"], " and ",
   math["\[Delta] = 6"], " months. The system sits below the \
pitchfork threshold ", math["\[Alpha] = 1"],
   " so the deterministic skeleton has unstable origin saturated by \
the cubic; the delayed negative feedback produces sustained \
oscillations of period ", math["\[TildeTilde] 4\[Delta] = 24"],
   " months \[LongDash] shorter than the canonical 3\[Dash]6-year \
ENSO band, but the delay-induced limit-cycle phenomenology is \
correct."],

wlIn["(* Figure 5.9: DDE ensemble cones, 400 members per init month.
   Initial state drawn from a model-internal climatology pool. Same
   percentile-band pattern as Figure 5.5 but the underlying dynamics
   are the limit cycle, not the stochastic-RDO recharge mode. *)
panelDDE[m0_] := With[{f = forecastEnsembleDDE[m0]},
   Show[
      ListLinePlot[{Transpose[{f[\"t\"], f[\"p05\"]}],
                    Transpose[{f[\"t\"], f[\"p95\"]}]},
         Filling -> {1 -> {{2}, Directive[Opacity[0.20],
            RGBColor[0.20, 0.45, 0.75]]}}],
      ListLinePlot[{Transpose[{f[\"t\"], f[\"p25\"]}],
                    Transpose[{f[\"t\"], f[\"p75\"]}]},
         Filling -> {1 -> {{2}, Directive[Opacity[0.45],
            RGBColor[0.20, 0.45, 0.75]]}}],
      ListLinePlot[Transpose[{f[\"t\"], f[\"median\"]}],
         PlotStyle -> Directive[Thickness[0.005], Black]],
      Frame -> True, PlotRange -> {{0, 18}, {-1.6, 1.6}},
      AspectRatio -> 0.7, ImageSize -> 520]];

Grid[Partition[panelDDE /@ {1, 4, 7, 10}, 2], Spacings -> {1.5, 1.5}]"],

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

para[{ital["Caveat on this inversion."], " The expression above \
inverts only the ", bold["deterministic skeleton"], " of the SST \
equation. The actual observed ", math["dT/dt"], " also contains \
contributions from the slow stochastic forcing ",
   sub["\[Sigma]", "T"], " ", sub["\[Xi]", "T"],
   " and \[LongDash] in spring \[LongDash] a substantial \
westerly-wind-burst term ", sigW, " ", sub["\[Xi]", "w"],
   ". Attributing the entire spring warming to ", math["h"],
   " therefore over-estimates the true thermocline anomaly: the \
inversion absorbs whatever spring WWB activity drove part of the \
observed tendency. A real-time SST-only proxy is unavoidable here \
because there is no clean machine-readable feed for the warm-water-\
volume index in the same monthly format as Ni\[NTilde]o 3.4; the \
NOAA PMEL TAO/TRITON mooring array publishes warm-water-volume but \
not in a format we can fetch in pure WL. Readers extending this \
notebook with a proper subsurface-data source should expect the \
forecast median to come down somewhat \[LongDash] our +2 to +3 \
\[Degree]C peak is therefore an ", ital["upper-bound"],
   " estimate."}],

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
record: a Feb\[Dash]Apr Ni\[NTilde]o 3.4 warming in the top decile \
of all such windows since 1950 (ranked 9th of 77), combined with \
substantial residual subsurface heat content from the previous La \
Ni\[NTilde]a recharge. (2) The spring predictability \
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

para[{ital["Data sources and licences."], " All scientific data \
used here are public-domain or open-licence and explicitly cleared \
for reuse, including for educational and commercial work."}],

para[{"\[Bullet] ", bold["NOAA NCEI ERSSTv5"], " (monthly box-mean \
anomalies ", ital["and"], " the 2\[Degree]\[Times]2\[Degree] \
gridded ", mono["ssta"], " fields used in \[Section]3.5). \
US Federal Government work, public domain (17 U.S.C. \[Section]105). \
Recommended citation: Huang, B., P. W. Thorne, V. F. Banzon, T. \
Boyer, G. Chepurin, J. H. Lawrimore, M. J. Menne, T. M. Smith, R. \
S. Vose, & H.-M. Zhang (2017). NOAA Extended Reconstructed Sea \
Surface Temperature (ERSST), Version 5. ", bold["NOAA NCEI"],
   ". doi:10.7289/V5T72FNM."}],

para[{"\[Bullet] ", bold["NOAA PSL"],
   " (the Ni\[NTilde]o 3.4 monthly anomaly text file we parse for \
the box-mean time series). Public domain; redistribution free with \
attribution to the NOAA Physical Sciences Laboratory, Boulder, CO. \
", link["psl.noaa.gov/data/correlation/nina34.anom.data",
   "https://psl.noaa.gov/data/correlation/nina34.anom.data"], "."}],

para[{"\[Bullet] ", bold["NOAA CPC Oceanic Ni\[NTilde]o Index (ONI)"],
   " and the ", bold["ENSO Diagnostic Discussion"], " (May 2026 \
probability table). Public domain. ",
   link["cpc.ncep.noaa.gov", "https://www.cpc.ncep.noaa.gov/"], "."}],

para[{"\[Bullet] ", bold["IRI / CPC ENSO forecast plume"],
   " (the rendered PNG used as Figure 6.2 panel A). Produced by \
Columbia University's International Research Institute for Climate \
and Society; the published plume image is free to use with \
attribution. ",
   link["iri.columbia.edu/our-expertise/climate/forecasts/enso/current/",
     "https://iri.columbia.edu/our-expertise/climate/forecasts/enso/current/"],
   ". (IRI ceased publishing the underlying per-model numerical \
table in 2024; only the rendered PNG remains publicly addressable.)"}],

para[{"\[Bullet] ", bold["OpenAI gpt-image-2"], " was used to \
generate Figures 1.1, 1.2, 3.1, 4.1, 5.1 (the five conceptual \
illustrations). Per OpenAI's Terms of Use, outputs are owned by \
the API caller and are licensed for commercial use; we additionally \
acknowledge the model here for transparency."}],

para[{"\[Bullet] ", bold["BBC News popular-science explainer"],
   " (Simon King, May 2026 \"El Ni\[NTilde]o is coming\") is linked, \
not redistributed; the link is fair use as a contextual reference. \
", link["youtube.com/watch?v=UEseLvpl9ss",
   "https://www.youtube.com/watch?v=UEseLvpl9ss"], "."}],

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
were assembled with the help of Anthropic's Claude Code."}]
}];


(* ============================================================
   §10 Reproducibility
   ============================================================ *)

writeAll[{

hd1["10.  Reproducibility"],

para[{"All source code lives in the public repository ",
   link["github.com/mthiel74/ENSO-emergence",
     "https://github.com/mthiel74/ENSO-emergence"],
   ". The analysis pipeline (live data fetch \[Rule] toy models \
\[Rule] live forecast \[Rule] notebook build) is pure Wolfram \
Language; the one exception is a one-off Python script that calls \
OpenAI's ", mono["gpt-image-2"], " API to regenerate the five \
educational illustrations (Figures 1.1, 1.2, 3.1, 4.1, 5.1) \
\[LongDash] their PNG outputs are committed to the repository so a \
normal rebuild does not invoke Python. To regenerate the notebook \
from scratch on a machine with Wolfram Language 14 installed:"}],

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

# 6. Render the spatial SST-anomaly time-lapse hero
wolframscript -file wolfram/hero.wls

# 7. Rebuild this notebook
wolframscript -file community/build_notebook.wls

# Steps 2-6 are also wrapped in wolfram/run_all.wls."],

para[{"The full pipeline runs in approximately 5 minutes on a 2024 \
MacBook M3. The .nb file (~5 MB with all images embedded) is \
committed to the repository so the notebook is browseable without \
running anything."}],

para[{Style["Generated on " <> DateString[Now, {"MonthName", " ",
    "DayShort", ", ", "Year"}] <> ".",
    FontSlant -> Italic, FontColor -> GrayLevel[0.4]]}]

}];


(* ---- save: native Export[file, Notebook] — same pattern used by
   GaltonBoard, ActualSizeofCountries, HistoricalWeather builders.
   Wolfram's NB writer handles Image / AnimatedImage serialisation,
   raster compression and character escaping automatically. *)

Print["assembled ", Length[$allCells], " cells"];

nbExpr = Notebook[$allCells,
   WindowSize -> {980, 800},
   StyleDefinitions -> "Default.nb"];

TimeConstrained[
   UsingFrontEnd[
      (* Save the notebook via NotebookSave + a CreateDocument hosting
         the cells. This is what makes Export[file.nb, ...] actually
         persist the AnimatedImage / Image cells to disk. *)
      nbObj = CreateDocument[$allCells, Visible -> False,
         WindowSize -> {980, 800},
         StyleDefinitions -> "Default.nb"];
      NotebookSave[nbObj, outNb];
      Print["wrote ", outNb, " (", FileByteCount[outNb], " B)"];
      Quiet @ Check[
         Export[outPdf, nbObj];
         Print["wrote ", outPdf, " (", FileByteCount[outPdf], " B)"],
         Print["warn: PDF export failed"]];
      NotebookClose[nbObj];
   ],
   600,
   Print["warn: notebook save timed out"]
];
