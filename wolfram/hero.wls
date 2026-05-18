#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Hero video for the Wolfram Community post.

    Two stitched parts:

      Part A (10 s)  Spatial time-lapse of equatorial Pacific SST
                     anomaly over the past 12 months, from gridded
                     NOAA ERSSTv5 NetCDF. One frame per month.
                     Overlaid: Niño 3.4 box, current date label,
                     basin-mean ONI value.

      Part B (5 s)   Toy-model 500-member ensemble cone for the next
                     18 months, drawn frame-by-frame, with NOAA
                     probability bars fading in.

    Outputs (under docs/images/):
      hero_spatial.mp4         Part A only, 24-month seamless loop
      hero_spatial_still.png   most-recent frame of Part A (PNG hero)
      hero_combined.mp4        Part A + B stitched
*)

Get[FileNameJoin[{DirectoryName[$InputFileName], "load_data.wl"}]];

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
rawDir   = FileNameJoin[{repoRoot, "data", "raw"}];

If[! DirectoryQ[imgDir],
   CreateDirectory[imgDir, CreateIntermediateDirectories -> True]];

(* ===================== Part A: spatial time-lapse =================== *)

(* List the 15 cached ERSSTv5 month files in chronological order *)
ncFiles = SortBy[
   FileNames["ersst.v5.*.nc", rawDir],
   FileBaseName];
Print["found ", Length[ncFiles], " monthly NetCDF files"];

ncMonth[file_] := Module[{base = FileBaseName[file]},
   With[{ym = StringTake[base, -6]},
     {ToExpression @ StringTake[ym, 4],
      ToExpression @ StringTake[ym, -2]}]];

(* Coordinate axes (constant across files) *)
latAxis = Normal @ Import[First[ncFiles], {"NetCDF", "Data", "lat"}];
lonAxis = Normal @ Import[First[ncFiles], {"NetCDF", "Data", "lon"}];

(* Pacific basin window: lon 130E..290E (= 70W), lat -30..+30 *)
latIdx = Flatten @ Position[latAxis, _?(-30 <= # <= 30 &)];
lonIdx = Flatten @ Position[lonAxis, _?(130 <= # <= 290 &)];
latSub = latAxis[[latIdx]];
lonSub = lonAxis[[lonIdx]];
{nLat, nLon} = {Length[latSub], Length[lonSub]};

Print["window: lat ", MinMax[latSub], " lon ", MinMax[lonSub],
   "  (", nLat, " x ", nLon, ")"];

(* Niño 3.4 box: lat -5..+5, lon 190..240 (= 170W..120W) *)
n34LatIdx = Flatten @ Position[latSub, _?(-5 <= # <= 5 &)];
n34LonIdx = Flatten @ Position[lonSub, _?(190 <= # <= 240 &)];

loadFrame[file_] := Module[{ssta, sub, mean},
   ssta = Normal @ Import[file, {"NetCDF", "Data", "ssta"}];
   sub = ssta[[1, 1, latIdx, lonIdx]];
   mean = Mean @ Select[Flatten[sub[[n34LatIdx, n34LonIdx]]], # > -10 &];
   <|"data" -> sub, "month" -> ncMonth[file], "n34" -> mean|>
];

frames = loadFrame /@ ncFiles;
Print["loaded ", Length[frames], " frames"];

(* Colour function — diverging red/blue, centred at 0, capped at ±3 *)
divergingCmap = Blend[{
   {-3.0, RGBColor[0.05, 0.10, 0.45]},
   {-1.5, RGBColor[0.15, 0.40, 0.75]},
   {-0.5, RGBColor[0.55, 0.75, 0.92]},
   { 0.0, RGBColor[0.97, 0.97, 0.95]},
   { 0.5, RGBColor[0.95, 0.65, 0.45]},
   { 1.5, RGBColor[0.85, 0.20, 0.15]},
   { 3.0, RGBColor[0.45, 0.05, 0.10]}
}, Clip[#, {-3., 3.}]] &;

monthName[m_Integer] := {"Jan","Feb","Mar","Apr","May","Jun",
   "Jul","Aug","Sep","Oct","Nov","Dec"}[[m]];

(* Convert grid lon (0..360) to display lon (-180..180) for tick labels *)
dispLon[L_] := If[L > 180, L - 360, L];

renderFrame[f_] := Module[
   {dat, n34, ym, label, n34Label, fig, frameWidth = 960, frameHeight = 560},
   dat = Normal @ f["data"];
   ym = f["month"];
   n34 = f["n34"];
   label = monthName[ym[[2]]] <> " " <> ToString[ym[[1]]];
   n34Label = ToString[NumberForm[n34, {3, 2}]] <> " \[Degree]C";
   fig = Show[
      ArrayPlot[Reverse[dat],
         ColorFunction -> (
            If[# < -10., GrayLevel[0.78], divergingCmap[#]] &),
         ColorFunctionScaling -> False,
         DataRange -> {{First[lonSub], Last[lonSub]},
                       {First[latSub], Last[latSub]}},
         Frame -> True,
         FrameTicks -> {
            {Table[{l, ToString[l] <> "\[Degree]"},
                {l, -30, 30, 15}],
             None},
            {Table[{l, With[{dl = dispLon[l]},
                  ToString[Abs[dl]] <> "\[Degree]" <>
                  If[dl < 0, "W", "E"]]},
                {l, 140, 280, 30}],
             None}},
         FrameLabel -> None,
         PlotRangePadding -> 0,
         AspectRatio -> nLat/nLon
      ],
      (* Niño 3.4 box overlay — use \[NTilde] explicitly for Niño *)
      Graphics[{
         Directive[EdgeForm[Directive[Thick, Black]], FaceForm[None]],
         Rectangle[{190, -5}, {240, 5}],
         Directive[Black, FontFamily -> "Helvetica", FontSize -> 13,
            FontWeight -> Bold],
         Text[Style["Ni\[NTilde]o 3.4", Black, Bold, 13], {215, 7.5}],
         (* Date + ONI labels in upper-left corner of the panel *)
         Inset[
            Framed[
               Column[{
                  Style[label, FontSize -> 22, FontWeight -> Bold],
                  Style["Ni\[NTilde]o 3.4 anomaly", FontSize -> 10,
                     FontColor -> GrayLevel[0.4]],
                  Style[n34Label,
                     FontSize -> 20, FontWeight -> Bold,
                     FontColor -> If[n34 > 0.5,
                        RGBColor[0.7, 0.1, 0.1],
                        If[n34 < -0.5, RGBColor[0.1, 0.2, 0.6], Black]]]
               }, Alignment -> Center, Spacings -> 0.4],
               Background -> RGBColor[1, 1, 1, 0.85],
               FrameStyle -> GrayLevel[0.6],
               FrameMargins -> 8, RoundingRadius -> 4],
            {137, 24}, {Left, Top}]
      }],
      PlotLabel -> Style[
         "Equatorial Pacific SST anomaly (ERSSTv5, vs 1971\[Dash]2000 climatology)",
         Bold, FontFamily -> "Helvetica", FontSize -> 13],
      ImageSize -> {frameWidth, frameHeight}
   ];
   Rasterize[fig, ImageSize -> {frameWidth, frameHeight},
      Background -> White]
];

Print["rendering ", Length[frames], " spatial frames..."];
renderedFrames = renderFrame /@ frames;
Print["  done"];

(* Save the most recent frame as a still PNG hero *)
Export[FileNameJoin[{imgDir, "hero_spatial_still.png"}], Last[renderedFrames],
   ImageResolution -> 144];
Print["wrote hero_spatial_still.png"];

(* Build a smooth looping video: ~0.8 s per frame, 15 frames = 12 s.
   MP4 is the file format we keep in the repo for downloading;
   GIF is what embeds cleanly into .nb files. *)
animation = AnimatedImage[renderedFrames, FrameRate -> 1.25,
   AnimationRepetitions -> Infinity];

Export[FileNameJoin[{imgDir, "hero_spatial.mp4"}], animation, "MP4"];
Print["wrote hero_spatial.mp4"];

Export[FileNameJoin[{imgDir, "hero_spatial.gif"}],
   AnimatedImage[
      (* downsample each frame to ~640 px wide for size *)
      ImageResize[#, 640] & /@ renderedFrames,
      FrameRate -> 1.25, AnimationRepetitions -> Infinity],
   "GIF"];
Print["wrote hero_spatial.gif (", FileByteCount[
   FileNameJoin[{imgDir, "hero_spatial.gif"}]], " B)"];

(* Add a colorbar legend rendered as a separate PNG so the notebook
   has a static legend caption to refer to. *)
legend = BarLegend[
   {divergingCmap, {-3, 3}},
   LabelStyle -> Directive[FontFamily -> "Helvetica", FontSize -> 11],
   LegendLabel -> Placed["SST anomaly (\[Degree]C)", Above],
   LegendMarkerSize -> 360];
Export[FileNameJoin[{imgDir, "hero_spatial_legend.png"}],
   Rasterize[legend, Background -> White, ImageSize -> 460],
   ImageResolution -> 144];
Print["wrote hero_spatial_legend.png"];
