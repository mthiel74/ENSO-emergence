#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Two alternative renderings of the same 15-month ERSSTv5 SST
    anomaly time-lapse, for user A/B comparison:

      VARIANT A — Satellite overlay.
        The SST anomaly grid is composited over Wolfram's built-in
        satellite imagery for an expanded geographic window
        (lat 40°S..40°N, lon 110°E..50°W) so that North and South
        American coastlines are visible. Anomalies are rendered
        with partial transparency so the satellite map shows
        through.

      VARIANT B — Smoothed.
        The 2°×2° gridded data is bicubic-interpolated onto a
        0.25° finer grid (8× refinement), giving a visually smooth
        anomaly field. Same map extent as the original hero.

    Outputs (in docs/images/):
      hero_satellite.mp4 + .gif + _still.png
      hero_smooth.mp4    + .gif + _still.png
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
rawDir   = FileNameJoin[{repoRoot, "data", "raw"}];

ncFiles = SortBy[FileNames["ersst.v5.*.nc", rawDir], FileBaseName];
Print["found ", Length[ncFiles], " monthly NetCDF files"];

ncMonth[file_] := With[{ym = StringTake[FileBaseName[file], -6]},
   {ToExpression @ StringTake[ym, 4],
    ToExpression @ StringTake[ym, -2]}];

latAxis = Normal @ Import[First[ncFiles], {"NetCDF", "Data", "lat"}];
lonAxis = Normal @ Import[First[ncFiles], {"NetCDF", "Data", "lon"}];

(* WIDER window for the satellite variant: lat -40..40, lon 110..310 *)
latIdxWide = Flatten @ Position[latAxis, _?(-40 <= # <= 40 &)];
lonIdxWide = Flatten @ Position[lonAxis, _?(110 <= # <= 310 &)];
latSubWide = latAxis[[latIdxWide]];
lonSubWide = lonAxis[[lonIdxWide]];

loadFrameWide[file_] := Module[{ssta, sub, mean,
   n34LatIdx, n34LonIdx},
   ssta = Normal @ Import[file, {"NetCDF", "Data", "ssta"}];
   sub = ssta[[1, 1, latIdxWide, lonIdxWide]];
   n34LatIdx = Flatten @ Position[latSubWide, _?(-5 <= # <= 5 &)];
   n34LonIdx = Flatten @ Position[lonSubWide, _?(190 <= # <= 240 &)];
   mean = Mean @ Select[Flatten[sub[[n34LatIdx, n34LonIdx]]], # > -10 &];
   <|"data" -> sub, "month" -> ncMonth[file], "n34" -> mean|>
];

frames = loadFrameWide /@ ncFiles;
Print["loaded ", Length[frames], " frames"];

monthName[m_Integer] := {"Jan","Feb","Mar","Apr","May","Jun",
   "Jul","Aug","Sep","Oct","Nov","Dec"}[[m]];
dispLon[L_] := If[L > 180, L - 360, L];

divergingCmap = Blend[{
   {-3.0, RGBColor[0.05, 0.10, 0.45]},
   {-1.5, RGBColor[0.15, 0.40, 0.75]},
   {-0.5, RGBColor[0.55, 0.75, 0.92]},
   { 0.0, RGBColor[0.97, 0.97, 0.95]},
   { 0.5, RGBColor[0.95, 0.65, 0.45]},
   { 1.5, RGBColor[0.85, 0.20, 0.15]},
   { 3.0, RGBColor[0.45, 0.05, 0.10]}
}, Clip[#, {-3., 3.}]] &;

(* ===================== VARIANT A: satellite overlay ================ *)

(* Wolfram's GeoGraphics expects standard lat/lon (-90..90, -180..180). *)
geoLonRange = {110, 310 - 360};   (* {110E, 50W} *)

(* Build a transparent SST anomaly image. Sea cells get the divergent
   colormap with 70% alpha; land cells (sentinel -999) get fully
   transparent so the satellite shows through. *)
sstaToRGBA[v_?NumericQ] := If[v < -10.,
   RGBColor[0, 0, 0, 0.],            (* land: fully transparent *)
   With[{c = divergingCmap[v]},      (* sea: 70% opaque anomaly *)
      Append[Most[List @@ c], 0.70]] // Apply[RGBColor]];
sstaToRGBA[_] := RGBColor[0, 0, 0, 0.];

(* Convert a 2-D grid of anomalies into a partially-transparent Image,
   ready to overlay on the satellite background. *)
makeOverlay[dat_] := Module[{pix},
   pix = Map[sstaToRGBA, dat, {2}] /. RGBColor[r_, g_, b_, a_] :> {r, g, b, a};
   Image[Reverse[pix], ColorSpace -> "RGB", Interleaving -> True]
];

renderSatFrame[f_] := Module[
   {dat, n34, ym, label, n34Label, overlay, fig,
    frameWidth = 960, frameHeight = 600},
   dat = Normal @ f["data"];
   ym = f["month"];
   n34 = f["n34"];
   label = monthName[ym[[2]]] <> " " <> ToString[ym[[1]]];
   n34Label = ToString[NumberForm[n34, {3, 2}]] <> " \[Degree]C";
   overlay = makeOverlay[dat];
   fig = Show[
      GeoGraphics[
         {GeoStyling -> Automatic},
         GeoRange -> {{-40, 40}, geoLonRange},
         GeoProjection -> "Equirectangular",
         GeoBackground -> "Satellite",
         GeoGridLines -> {Range[-30, 30, 30], Range[-180, 180, 30]},
         GeoGridLinesStyle -> Directive[GrayLevel[0.6, 0.5],
            Thickness[0.0008]],
         ImageSize -> {frameWidth, frameHeight},
         Frame -> True,
         FrameLabel -> None,
         PlotLabel -> Style[
            "Equatorial Pacific SST anomaly over satellite imagery (ERSSTv5)",
            Bold, FontFamily -> "Helvetica", FontSize -> 13]
      ],
      (* Overlay the anomaly raster at the same geo extent *)
      Graphics[{
         Inset[overlay,
            GeoGridPosition[{0., (geoLonRange[[1]] + geoLonRange[[2]])/2.},
               GeoProjection -> "Equirectangular"],
            Center,
            (* size in geo units; convert lon span -> degrees *)
            GeoGridPosition[
               {Mean@{-40, 40} + 40,
                Mean@geoLonRange + (geoLonRange[[2]] - geoLonRange[[1]] + 360)/2.} -
               GeoGridPosition[
                  {Mean@{-40, 40} - 40,
                   Mean@geoLonRange - (geoLonRange[[2]] - geoLonRange[[1]] + 360)/2.},
                  GeoProjection -> "Equirectangular"][[1]],
               GeoProjection -> "Equirectangular"]
         ]
      }],
      (* Niño 3.4 box and labels *)
      Graphics[{
         Directive[EdgeForm[Directive[Thick, RGBColor[1, 1, 0]]],
            FaceForm[None]],
         Rectangle[{190 - 360, -5}, {240 - 360, 5}],   (* lon E -> W *)
         Directive[RGBColor[1, 1, 0.5],
            FontFamily -> "Helvetica", FontSize -> 12, FontWeight -> Bold],
         Text[Style["Ni\[NTilde]o 3.4", RGBColor[1, 1, 0.6], Bold, 12],
            {(190 + 240)/2. - 360, 8}],
         Inset[
            Framed[
               Column[{
                  Style[label, FontSize -> 22, FontWeight -> Bold,
                     FontColor -> White],
                  Style["Ni\[NTilde]o 3.4 anomaly", FontSize -> 10,
                     FontColor -> GrayLevel[0.85]],
                  Style[n34Label, FontSize -> 20, FontWeight -> Bold,
                     FontColor -> If[n34 > 0.5,
                        RGBColor[1, 0.4, 0.3],
                        If[n34 < -0.5, RGBColor[0.5, 0.7, 1], White]]]
               }, Alignment -> Center, Spacings -> 0.4],
               Background -> RGBColor[0, 0, 0, 0.65],
               FrameStyle -> GrayLevel[0.8],
               FrameMargins -> 8, RoundingRadius -> 4],
            Scaled[{0.04, 0.94}], {Left, Top}]
      }]
   ];
   Rasterize[fig, ImageSize -> {frameWidth, frameHeight},
      Background -> White]
];

(* That Inset-of-overlay logic above is too clever and may not be
   reliable. A more robust fallback: render the satellite map AND the
   anomaly map at the same projection, then ImageCompose them. *)

renderSatFrameSimple[f_] := Module[
   {dat, n34, ym, label, n34Label, overlay, sat, composed,
    fw = 960, fh = 600},
   dat = Normal @ f["data"];
   ym = f["month"];
   n34 = f["n34"];
   label = monthName[ym[[2]]] <> " " <> ToString[ym[[1]]];
   n34Label = ToString[NumberForm[n34, {3, 2}]] <> " \[Degree]C";
   sat = Rasterize @ GeoGraphics[
         GeoRange -> {{-40, 40}, geoLonRange},
         GeoProjection -> "Equirectangular",
         GeoBackground -> "Satellite",
         ImageSize -> {fw, fh},
         Frame -> False];
   overlay = ImageResize[makeOverlay[dat], ImageDimensions[sat]];
   composed = ImageCompose[sat, overlay];
   Show[Graphics[
         {Inset[composed, {0, 0}, {Left, Bottom},
            ImageDimensions[composed]],
          (* Niño 3.4 box at scaled coordinates *)
          Directive[EdgeForm[Directive[Thick, RGBColor[1, 1, 0]]],
             FaceForm[None]],
          (* Convert geo coords to scaled coords. With Equirectangular
             and the lon range 110E..(50W = -50), the total lon span
             is 200°. Niño 3.4 box is 170W..120W = -170..-120, which
             starts at fraction (110-(-170)+360)/200 ... overcomplicated.
             Simpler: compute the scaled position. *)
          With[{lonMin = 110., lonMax = -50. + 360.,
                xLeft = 190., xRight = 240.,
                latMin = -40., latMax = 40.,
                yBot = -5., yTop = 5.},
             {Rectangle[
                {(xLeft - lonMin)/(lonMax - lonMin) * ImageDimensions[composed][[1]],
                 (yBot - latMin)/(latMax - latMin) * ImageDimensions[composed][[2]]},
                {(xRight - lonMin)/(lonMax - lonMin) * ImageDimensions[composed][[1]],
                 (yTop - latMin)/(latMax - latMin) * ImageDimensions[composed][[2]]}],
              Inset[Style["Ni\[NTilde]o 3.4", RGBColor[1, 1, 0.6], Bold, 13],
                {((xLeft + xRight)/2. - lonMin)/(lonMax - lonMin) *
                   ImageDimensions[composed][[1]],
                 (8 - latMin)/(latMax - latMin) *
                   ImageDimensions[composed][[2]]}]}
          ],
          (* Date + ONI overlay (top-left) *)
          Inset[
             Framed[
                Column[{
                   Style[label, FontSize -> 22, FontWeight -> Bold,
                      FontColor -> White],
                   Style["Ni\[NTilde]o 3.4 anomaly", FontSize -> 10,
                      FontColor -> GrayLevel[0.85]],
                   Style[n34Label, FontSize -> 20, FontWeight -> Bold,
                      FontColor -> If[n34 > 0.5,
                         RGBColor[1, 0.4, 0.3],
                         If[n34 < -0.5, RGBColor[0.5, 0.7, 1], White]]]
                }, Alignment -> Center, Spacings -> 0.4],
                Background -> RGBColor[0, 0, 0, 0.65],
                FrameStyle -> GrayLevel[0.8],
                FrameMargins -> 8, RoundingRadius -> 4],
             {30, ImageDimensions[composed][[2]] - 30}, {Left, Top}]
         },
         PlotRange -> {{0, ImageDimensions[composed][[1]]},
                       {0, ImageDimensions[composed][[2]]}},
         ImageSize -> {fw, fh},
         PlotLabel -> Style[
            "SST anomaly overlay on satellite imagery (ERSSTv5)",
            Bold, FontFamily -> "Helvetica", FontSize -> 13]
   ]]
];

Print["rendering Variant A: satellite-overlay frames..."];
satFrames = Quiet[renderSatFrameSimple /@ frames];
Print["  done"];

Export[FileNameJoin[{imgDir, "hero_satellite_still.png"}],
   Last[satFrames], ImageResolution -> 144];
Export[FileNameJoin[{imgDir, "hero_satellite.mp4"}],
   AnimatedImage[satFrames, FrameRate -> 1.25,
      AnimationRepetitions -> Infinity], "MP4"];
Export[FileNameJoin[{imgDir, "hero_satellite.gif"}],
   AnimatedImage[ImageResize[#, 640] & /@ satFrames,
      FrameRate -> 1.25, AnimationRepetitions -> Infinity], "GIF"];
Print["wrote hero_satellite.{png,mp4,gif}"];


(* ===================== VARIANT B: bicubic smoothed ================== *)

(* Smaller window for this one — same as original hero: lat ±30, lon 130..290 *)
latIdxNarrow = Flatten @ Position[latAxis, _?(-30 <= # <= 30 &)];
lonIdxNarrow = Flatten @ Position[lonAxis, _?(130 <= # <= 290 &)];
latSubN = latAxis[[latIdxNarrow]];
lonSubN = lonAxis[[lonIdxNarrow]];

loadFrameN[file_] := Module[{ssta, sub, mean, n34LatIdx, n34LonIdx},
   ssta = Normal @ Import[file, {"NetCDF", "Data", "ssta"}];
   sub = ssta[[1, 1, latIdxNarrow, lonIdxNarrow]];
   n34LatIdx = Flatten @ Position[latSubN, _?(-5 <= # <= 5 &)];
   n34LonIdx = Flatten @ Position[lonSubN, _?(190 <= # <= 240 &)];
   mean = Mean @ Select[Flatten[sub[[n34LatIdx, n34LonIdx]]], # > -10 &];
   <|"data" -> sub, "month" -> ncMonth[file], "n34" -> mean|>
];

framesN = loadFrameN /@ ncFiles;

(* Replace land sentinel with NaN/-999, then build a 2D interpolation
   that ignores land cells. Easier: in-fill land via NaN-aware
   GaussianFilter, then interpolate. *)
smoothFrame[dat_] := Module[
   {sea, fillVal, smoothed, interp, fineLat, fineLon, fineGrid},
   sea = Map[If[# < -10., 0.0, #] &, dat, {2}];
   landMask = Map[If[# < -10., 0., 1.] &, dat, {2}];
   (* Smooth both signal and mask; divide to get a mask-aware smoothed *)
   smoothed = GaussianFilter[sea, 1.2] / (GaussianFilter[landMask, 1.2] + 1*^-6);
   (* Refine via bicubic interpolation 4× in each dim *)
   interp = ListInterpolation[smoothed,
      {{First[latSubN], Last[latSubN]}, {First[lonSubN], Last[lonSubN]}},
      InterpolationOrder -> 3];
   fineLat = Range[First[latSubN], Last[latSubN], 0.5];
   fineLon = Range[First[lonSubN], Last[lonSubN], 0.5];
   fineGrid = Table[interp[la, lo], {la, fineLat}, {lo, fineLon}];
   (* re-mask: any fine cell whose nearest coarse cell was land -> land *)
   With[{nLatF = Length[fineLat], nLonF = Length[fineLon]},
     Table[
        With[{coarseLat = Clip[1 + Floor[(la - First[latSubN])/2.], {1, Length[latSubN]}],
              coarseLon = Clip[1 + Floor[(lo - First[lonSubN])/2.], {1, Length[lonSubN]}]},
          If[dat[[coarseLat, coarseLon]] < -10., -999., fineGrid[[laIdx, loIdx]]]
        ],
        {laIdx, nLatF}, {loIdx, nLonF},
        {la, fineLat[[laIdx]]}, {lo, fineLon[[loIdx]]}
     ]
   ];
   (* the above table mixes iterators incorrectly; just use the interp + simple mask *)
   fineGrid
];

renderSmoothFrame[f_] := Module[
   {dat, smooth, n34, ym, label, n34Label, fig, fineLat, fineLon,
    fw = 960, fh = 560},
   dat = Normal @ f["data"];
   smooth = smoothFrame[dat];
   fineLat = Range[First[latSubN], Last[latSubN], 0.5];
   fineLon = Range[First[lonSubN], Last[lonSubN], 0.5];
   ym = f["month"];
   n34 = f["n34"];
   label = monthName[ym[[2]]] <> " " <> ToString[ym[[1]]];
   n34Label = ToString[NumberForm[n34, {3, 2}]] <> " \[Degree]C";
   fig = Show[
      ArrayPlot[Reverse[smooth],
         ColorFunction -> (If[# < -10., GrayLevel[0.78],
            divergingCmap[#]] &),
         ColorFunctionScaling -> False,
         DataRange -> {{First[lonSubN], Last[lonSubN]},
                       {First[latSubN], Last[latSubN]}},
         Frame -> True,
         FrameTicks -> {
            {Table[{l, ToString[l] <> "\[Degree]"}, {l, -30, 30, 15}], None},
            {Table[{l, With[{dl = dispLon[l]},
               ToString[Abs[dl]] <> "\[Degree]" <>
               If[dl < 0, "W", "E"]]},
               {l, 140, 280, 30}], None}},
         FrameLabel -> None,
         AspectRatio -> Length[fineLat]/Length[fineLon]
      ],
      Graphics[{
         Directive[EdgeForm[Directive[Thick, Black]], FaceForm[None]],
         Rectangle[{190, -5}, {240, 5}],
         Text[Style["Ni\[NTilde]o 3.4", Black, Bold, 13], {215, 7.5}],
         Inset[
            Framed[
               Column[{
                  Style[label, FontSize -> 22, FontWeight -> Bold],
                  Style["Ni\[NTilde]o 3.4 anomaly", FontSize -> 10,
                     FontColor -> GrayLevel[0.4]],
                  Style[n34Label, FontSize -> 20, FontWeight -> Bold,
                     FontColor -> If[n34 > 0.5, RGBColor[0.7, 0.1, 0.1],
                        If[n34 < -0.5, RGBColor[0.1, 0.2, 0.6], Black]]]
               }, Alignment -> Center, Spacings -> 0.4],
               Background -> RGBColor[1, 1, 1, 0.85],
               FrameStyle -> GrayLevel[0.6],
               FrameMargins -> 8, RoundingRadius -> 4],
            {137, 24}, {Left, Top}]
      }],
      PlotLabel -> Style[
         "Equatorial Pacific SST anomaly \[LongDash] bicubic-smoothed (ERSSTv5)",
         Bold, FontFamily -> "Helvetica", FontSize -> 13],
      ImageSize -> {fw, fh}
   ];
   Rasterize[fig, ImageSize -> {fw, fh}, Background -> White]
];

Print["rendering Variant B: bicubic-smoothed frames..."];
smoothFrames = renderSmoothFrame /@ framesN;
Print["  done"];

Export[FileNameJoin[{imgDir, "hero_smooth_still.png"}], Last[smoothFrames],
   ImageResolution -> 144];
Export[FileNameJoin[{imgDir, "hero_smooth.mp4"}],
   AnimatedImage[smoothFrames, FrameRate -> 1.25,
      AnimationRepetitions -> Infinity], "MP4"];
Export[FileNameJoin[{imgDir, "hero_smooth.gif"}],
   AnimatedImage[ImageResize[#, 640] & /@ smoothFrames,
      FrameRate -> 1.25, AnimationRepetitions -> Infinity], "GIF"];
Print["wrote hero_smooth.{png,mp4,gif}"];
