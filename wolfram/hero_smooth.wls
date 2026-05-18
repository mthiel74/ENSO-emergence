#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Variant B v2 — bicubic-smoothed SST anomaly.

    The 2°×2° gridded ERSSTv5 data is bicubic-interpolated onto a
    finer 0.5°×0.5° grid (4× refinement) so the rendered field is
    smooth instead of pixelated. Land cells are first NaN-extended
    using a mask-normalised Gaussian smoothing so the interpolant
    near the coast does not pick up the -999 sentinel.

    Output: docs/images/hero_smooth{,_still.png,.gif,.mp4}.
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

(* Same window as the original hero: lat ±30, lon 130E..70W *)
latIdx = Flatten @ Position[latAxis, _?(-30 <= # <= 30 &)];
lonIdx = Flatten @ Position[lonAxis, _?(130 <= # <= 290 &)];
latSub = latAxis[[latIdx]];
lonSub = lonAxis[[lonIdx]];
{nLat, nLon} = {Length[latSub], Length[lonSub]};

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

divergingCmap = Blend[{
   {-3.0, RGBColor[0.05, 0.10, 0.45]}, {-1.5, RGBColor[0.15, 0.40, 0.75]},
   {-0.5, RGBColor[0.55, 0.75, 0.92]}, { 0.0, RGBColor[0.97, 0.97, 0.95]},
   { 0.5, RGBColor[0.95, 0.65, 0.45]}, { 1.5, RGBColor[0.85, 0.20, 0.15]},
   { 3.0, RGBColor[0.45, 0.05, 0.10]}}, Clip[#, {-3., 3.}]] &;

monthName[m_Integer] := {"Jan","Feb","Mar","Apr","May","Jun",
   "Jul","Aug","Sep","Oct","Nov","Dec"}[[m]];
dispLon[L_] := If[L > 180, L - 360, L];

(* Mask-aware smoothing: extend sea-cell values into coastal land
   cells so the interpolant doesn't see the -999 sentinel. *)
smoothData[dat_] := Module[{seaVals, mask, smoothedSea, smoothedMask},
   seaVals    = Map[If[# < -10., 0.0, #] &, dat, {2}];
   mask       = Map[If[# < -10., 0.0, 1.0] &, dat, {2}];
   smoothedSea  = GaussianFilter[seaVals, 1.0];
   smoothedMask = GaussianFilter[mask, 1.0];
   (* Sea-only smoothed field via mask normalisation *)
   smoothedSea / (smoothedMask + 1.*^-6)
];

(* Bicubic interpolation onto a 0.5°×0.5° fine grid (4× refinement). *)
refineData[dat_] := Module[{smoothed, interp, fineLat, fineLon},
   smoothed = smoothData[dat];
   interp = ListInterpolation[smoothed,
      {{First[latSub], Last[latSub]}, {First[lonSub], Last[lonSub]}},
      InterpolationOrder -> 3];
   fineLat = Range[First[latSub], Last[latSub], 0.5];
   fineLon = Range[First[lonSub], Last[lonSub], 0.5];
   Table[interp[la, lo], {la, fineLat}, {lo, fineLon}]
];

(* Coarse land mask, projected to the fine grid via nearest-neighbour. *)
landMaskFine[dat_] := Module[{fineLat, fineLon},
   fineLat = Range[First[latSub], Last[latSub], 0.5];
   fineLon = Range[First[lonSub], Last[lonSub], 0.5];
   Table[
      With[{ci = Clip[Round[1 + (la - First[latSub])/2.], {1, nLat}],
            cj = Clip[Round[1 + (lo - First[lonSub])/2.], {1, nLon}]},
        dat[[ci, cj]] < -10.],
      {la, fineLat}, {lo, fineLon}]
];

renderFrame[f_] := Module[
   {dat, fine, mask, masked, n34, ym, label, n34Label, fig,
    fw = 960, fh = 560},
   dat = Normal @ f["data"];
   fine = refineData[dat];
   mask = landMaskFine[dat];
   masked = MapThread[
      Function[{val, isLand}, If[isLand, -999., val]],
      {fine, mask}, 2];
   ym = f["month"];
   n34 = f["n34"];
   label = monthName[ym[[2]]] <> " " <> ToString[ym[[1]]];
   n34Label = ToString[NumberForm[n34, {3, 2}]] <> " \[Degree]C";
   fig = Show[
      ArrayPlot[Reverse[masked],
         ColorFunction -> (If[# < -10., GrayLevel[0.78],
            divergingCmap[#]] &),
         ColorFunctionScaling -> False,
         DataRange -> {{First[lonSub], Last[lonSub]},
                       {First[latSub], Last[latSub]}},
         Frame -> True,
         FrameTicks -> {
            {Table[{l, ToString[l] <> "\[Degree]"}, {l, -30, 30, 15}],
             None},
            {Table[{l, With[{dl = dispLon[l]},
                  ToString[Abs[dl]] <> "\[Degree]" <>
                  If[dl < 0, "W", "E"]]},
                {l, 140, 280, 30}], None}},
         FrameLabel -> None,
         AspectRatio -> Length[masked]/Length[masked[[1]]]
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

Print["rendering ", Length[frames], " smoothed frames..."];
rendered = renderFrame /@ frames;
Print["  done"];

Export[FileNameJoin[{imgDir, "hero_smooth_still.png"}], Last[rendered],
   ImageResolution -> 144];
Export[FileNameJoin[{imgDir, "hero_smooth.mp4"}],
   AnimatedImage[rendered, FrameRate -> 1.25,
      AnimationRepetitions -> Infinity], "MP4"];
Export[FileNameJoin[{imgDir, "hero_smooth.gif"}],
   AnimatedImage[ImageResize[#, 700] & /@ rendered,
      FrameRate -> 1.25, AnimationRepetitions -> Infinity], "GIF"];
Print["wrote hero_smooth.{png,mp4,gif}"];
