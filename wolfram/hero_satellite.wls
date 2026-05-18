#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Variant A v2 — satellite overlay, rewritten.

    Approach: for each 2°×2° grid cell, draw a GeoPolygon (a real
    geographic rectangle) coloured by the SST anomaly with partial
    opacity, on top of GeoGraphics's built-in satellite imagery.
    No Image compositing, no Inset-of-raster — just real geographic
    polygons that GeoGraphics renders at the correct projection.

    Output: docs/images/hero_satellite{,_still.png,.gif,.mp4}.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
rawDir   = FileNameJoin[{repoRoot, "data", "raw"}];

ncFiles = SortBy[FileNames["ersst.v5.*.nc", rawDir], FileBaseName];
ncMonth[file_] := With[{ym = StringTake[FileBaseName[file], -6]},
   {ToExpression @ StringTake[ym, 4],
    ToExpression @ StringTake[ym, -2]}];

latAxis = Normal @ Import[First[ncFiles], {"NetCDF", "Data", "lat"}];
lonAxis = Normal @ Import[First[ncFiles], {"NetCDF", "Data", "lon"}];

(* Wider window: lat 40S..40N, lon 110E..50W (= 110..310 in 0-360 conv) *)
latIdx = Flatten @ Position[latAxis, _?(-40 <= # <= 40 &)];
lonIdx = Flatten @ Position[lonAxis, _?(110 <= # <= 310 &)];
latSub = latAxis[[latIdx]];
lonSub = lonAxis[[lonIdx]];
{nLat, nLon} = {Length[latSub], Length[lonSub]};
Print["window: ", nLat, " lat × ", nLon, " lon"];

loadFrame[file_] := Module[{ssta, sub, mean, n34LatIdx, n34LonIdx},
   ssta = Normal @ Import[file, {"NetCDF", "Data", "ssta"}];
   sub = ssta[[1, 1, latIdx, lonIdx]];
   n34LatIdx = Flatten @ Position[latSub, _?(-5 <= # <= 5 &)];
   n34LonIdx = Flatten @ Position[lonSub, _?(190 <= # <= 240 &)];
   mean = Mean @ Select[Flatten[sub[[n34LatIdx, n34LonIdx]]], # > -10 &];
   <|"data" -> sub, "month" -> ncMonth[file], "n34" -> mean|>
];

frames = loadFrame /@ ncFiles;
Print["loaded ", Length[frames], " frames"];

monthName[m_Integer] := {"Jan","Feb","Mar","Apr","May","Jun",
   "Jul","Aug","Sep","Oct","Nov","Dec"}[[m]];

divergingCmap = Blend[{
   {-3.0, RGBColor[0.05, 0.10, 0.45]}, {-1.5, RGBColor[0.15, 0.40, 0.75]},
   {-0.5, RGBColor[0.55, 0.75, 0.92]}, { 0.0, RGBColor[0.97, 0.97, 0.95]},
   { 0.5, RGBColor[0.95, 0.65, 0.45]}, { 1.5, RGBColor[0.85, 0.20, 0.15]},
   { 3.0, RGBColor[0.45, 0.05, 0.10]}}, Clip[#, {-3., 3.}]] &;

(* Convert a 0..360 longitude into the -180..180 form GeoPolygon
   expects, and clip to the visible window so polygons that wrap the
   antimeridian don't draw across the whole globe. *)
lonTo180[L_] := If[L > 180., L - 360., L];

(* Geographic-rectangle for a 2°×2° cell centred at (lat, lon).
   Returns Nothing if the cell value is masked land. *)
gridCellPoly[lat_, lon_, val_, opacity_:0.65] :=
   If[val < -10., Nothing,
      {Directive[divergingCmap[val], Opacity[opacity], EdgeForm[None]],
       GeoPolygon[{
          {lat - 1., lonTo180[lon - 1.]},
          {lat - 1., lonTo180[lon + 1.]},
          {lat + 1., lonTo180[lon + 1.]},
          {lat + 1., lonTo180[lon - 1.]}}]}];

renderFrame[f_] := Module[
   {dat, n34, ym, label, n34Label, polygons, fig,
    fw = 1100, fh = 580},
   dat = Normal @ f["data"];
   ym = f["month"];
   n34 = f["n34"];
   label = monthName[ym[[2]]] <> " " <> ToString[ym[[1]]];
   n34Label = ToString[NumberForm[n34, {3, 2}]] <> " \[Degree]C";
   polygons = Flatten @ Table[
      gridCellPoly[latSub[[i]], lonSub[[j]], dat[[i, j]]],
      {i, nLat}, {j, nLon}];
   fig = GeoGraphics[
      {
         polygons,
         (* Niño 3.4 box: 5°N-5°S, 170°W-120°W *)
         Directive[Yellow, Thick, AbsoluteThickness[2.5]],
         GeoPath[{{5, -170}, {5, -120}, {-5, -120}, {-5, -170}, {5, -170}}],
         (* Tag the Niño 3.4 box *)
         Directive[RGBColor[1, 1, 0.6], FontFamily -> "Helvetica",
            FontSize -> 13, FontWeight -> Bold],
         Inset[Style["Ni\[NTilde]o 3.4", RGBColor[1, 1, 0.6], Bold, 13],
            GeoPosition[{8.5, -145}]]
      },
      GeoRange -> {{-40, 40}, {110, -50}},
      GeoProjection -> "Equirectangular",
      GeoBackground -> "Satellite",
      GeoGridLines -> {Range[-30, 30, 15], Range[-180, 180, 30]},
      GeoGridLinesStyle -> Directive[GrayLevel[0.7, 0.4],
         Thickness[0.0008]],
      ImageSize -> {fw, fh},
      Frame -> True, FrameStyle -> Black,
      PlotLabel -> Style[
         "Equatorial Pacific SST anomaly (ERSSTv5) over satellite imagery",
         Bold, FontFamily -> "Helvetica", FontSize -> 14]
   ];
   (* Layer the date/anomaly label on top via Show *)
   Show[fig,
      Epilog -> {
         Inset[
            Framed[
               Column[{
                  Style[label, FontSize -> 22, FontWeight -> Bold,
                     FontColor -> White],
                  Style["Ni\[NTilde]o 3.4 anomaly", FontSize -> 10,
                     FontColor -> GrayLevel[0.85]],
                  Style[n34Label, FontSize -> 20, FontWeight -> Bold,
                     FontColor -> If[n34 > 0.5, RGBColor[1, 0.5, 0.35],
                        If[n34 < -0.5, RGBColor[0.5, 0.7, 1], White]]]
               }, Alignment -> Center, Spacings -> 0.4],
               Background -> RGBColor[0, 0, 0, 0.7],
               FrameStyle -> Directive[GrayLevel[0.85], Thickness[0.001]],
               FrameMargins -> 8, RoundingRadius -> 4],
            Scaled[{0.045, 0.94}], {Left, Top}]
      }]
];

Print["rendering ", Length[frames], " satellite-overlay frames..."];
renderedFrames = renderFrame /@ frames;
Print["  rasterising..."];
rasterised = Rasterize[#, ImageSize -> {1100, 580}, Background -> White] & /@
   renderedFrames;
Print["  done"];

Export[FileNameJoin[{imgDir, "hero_satellite_still.png"}],
   Last[rasterised], ImageResolution -> 144];
Print["wrote hero_satellite_still.png"];

Export[FileNameJoin[{imgDir, "hero_satellite.mp4"}],
   AnimatedImage[rasterised, FrameRate -> 1.25,
      AnimationRepetitions -> Infinity], "MP4"];
Print["wrote hero_satellite.mp4"];

Export[FileNameJoin[{imgDir, "hero_satellite.gif"}],
   AnimatedImage[ImageResize[#, 700] & /@ rasterised,
      FrameRate -> 1.25, AnimationRepetitions -> Infinity], "GIF"];
Print["wrote hero_satellite.gif (", FileByteCount[
   FileNameJoin[{imgDir, "hero_satellite.gif"}]], " B)"];
