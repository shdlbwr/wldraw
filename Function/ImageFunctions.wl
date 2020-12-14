(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwImageSpherize[img_:Image[Graphics[{}, GridLines->{Table[n, {n, -1, 1, .25}], Table[n, {n, -1, 1, .25}]}, PlotRangePadding -> None]], amount_:1, res_:100]:= 
	Block[{image, grid, f, cf},
		
		image = If[Head[img] === Image, img, Rasterize[img, ImageResolution->$dwImageResolution]];
	
		(* grid with distortion *)
		grid = Flatten[Table[{i, j}, {i, 0, 1, 1/(4 - 1)}, {j, 0, 1, 1/(4 - 1)}], 1];
		grid[[6]] = grid[[6]] - amount {.1, .1};
		grid[[7]] = grid[[7]] + amount {-.1, .1};
		grid[[10]] = grid[[10]] + amount {.1, -.1};
		grid[[11]] = grid[[11]] + amount {.1, .1};
	
		(* color interpolation *)
		cf = BSplineFunction[ImageData[image], SplineDegree -> 1];
	
		(* distortion function *)
		f = Interpolation[Flatten[Table[{{N[1/(4 - 1)] i, N[1/(4 - 1)] j}, grid[[4 i + j + 1]]}, {i, 0, 4 - 1}, {j, 0, 4 - 1}], 1], Method -> "Spline", InterpolationOrder -> 2];
		
		(* final image *)
		ParametricPlot[f[u, v], {u, 0, 1}, {v, 0, 1}, ColorFunction -> (cf[1 - #4, #3] &), Axes -> False, BoundaryStyle -> None, Frame -> False, ImageSize -> 400, MaxRecursion -> 0, Mesh -> None, PlotPoints -> res, PlotRangePadding -> 0]
	
	]
	
dwImageSpiralFunction[pt_, rot_, size_, centerfirst_] := 
	With[{s = {.5, .5}}, 
		Block[{r, a, an},
			r = Norm[pt - s]; 
			a = ArcTan@@((pt + $MachineEpsilon) - s);
			an = a + If[centerfirst, .1 (rot/(r + $MachineEpsilon)), rot*r];
			s + size*r {Cos[an], Sin[an]}
		]
	];
	
Options[dwImageSpiral] := {"Rotate" -> 1 Pi, "Size" -> .7, "TwistCenterFirst" -> False};
dwImageSpiral[img_, OptionsPattern[]] :=
	Block[{dim = Min[ImageDimensions[img]]},
		ImageTransformation[ImageCrop[img, {dim, dim}], dwImageSpiralFunction[#, OptionValue["Rotate"], OptionValue["Size"], OptionValue["TwistCenterFirst"]]&]
  ]
	
Options[dwImageBox]:= {"Rotate"->True};
dwImageBox[n_, OptionsPattern[]]:=
	Block[{reduce = 1/$dwOverallSize, 
		size = $dwStyle[[n,3]]*Rasterize[Rotate[dwRenderImageObject[n], If[OptionValue["Rotate"], $dwStyle[[n,1]], 0]], "RasterSize", ImageFormattingWidth->Infinity, ImageResolution->$dwImageResolution]},
		{$dwP[[n,1]] - reduce*.5size, $dwP[[n,1]] + reduce*.5size}
	]
	
dwReplaceImage[sel_]:=
	CreateDialog[
		DynamicModule[{left = 0, top = 0, right = 0, bottom = 0,
			horiz = 0, vert = 0, scaleh = 1, scalev = 1, shearh = 0,
			shearv = 0, tilth = 0, tiltv = 0, zoom = 1, img = $dwStyle[[sel, 2]]},
			Pane[
				Dynamic@Column[{
					Grid[{
						{
							Column[{
								InputField[Dynamic[img], ImageSize->{90, 90}],
								Pane[Style["Delete above and paste new image or graphics", 10, Italic, LineIndent->0], ImageSize->90],
								"",
								Row[{Checkbox[Dynamic@$dwStyle[[sel,5]]]," remove background"}]
							}], 
							Spacer[20],
							Pane[
								If[Head[img] =!= Image,
									
									Overlay[{
										Dynamic@ImageTrim[Rasterize[img, Background->None, ImageSize->{470,300}, ImageResolution->$dwImageResolution], {{left, 1-top}, {1-right, bottom}}, DataRange -> {{0, 1}, {0, 1}}],
										Graphics[{White, AbsoluteDashing[{1, 3}], Line[{{0, -1}, {0, 1}}], Line[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, 1}}], Line[{{-1, 1}, {1, -1}}]}, 
										Background -> None, ImageSize -> {470, 300}, PlotRange -> 1]
									}],
									
									Overlay[{
										Dynamic@ImageTrim[Show[ImagePerspectiveTransformation[img, {{scaleh, Tan[shearh*Pi/180], horiz}, {Tan[shearv*Pi/180], scalev, vert}, {Tan[tiltv*Pi/180], Tan[tilth*Pi/180], 2-zoom}}, 
											Background->Transparent, Masking->All], ImageSize->{470,300}], {{left, 1-top}, {1-right, bottom}}, DataRange -> {{0, 1}, {0, 1}}],
										Graphics[{White, AbsoluteDashing[{1, 3}], Line[{{0, -1}, {0, 1}}], Line[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, 1}}], Line[{{-1, 1}, {1, -1}}]}, 
										Background -> None, ImageSize -> {470, 300}, PlotRange -> 1]
									}]
									
								],
							ImageSize->{470,300}]
						}
					}, Alignment->{Left,Top}, Background->{None, None}, Spacings->{0,0}],
					Grid[{
						{""},
						{Style["CROP", Bold, 15]},
						{Grid[{
							{"left ", Slider[Dynamic@left, {0,1}], Pane[Dynamic@left, 50], "  bottom ", Slider[Dynamic@bottom, {0,1}], Pane[Dynamic@bottom, 50]},
							{"right ", Slider[Dynamic@right, {0,1}], Pane[Dynamic@right, 50], "  top ", Slider[Dynamic@top, {0,1}], Pane[Dynamic@top, 50]}
						}, Alignment->{Right,Left}]},
						{""},
						{Style["DISTORT", Bold, 15]},
						{Grid[{
							{"horiz ", Slider[Dynamic@horiz, {-2,2}], Pane[Dynamic@horiz, 50], "  shear h ", Slider[Dynamic@shearh, {-89,89,1}], Pane[Dynamic[shearh], 50]},
							{"vert ", Slider[Dynamic@vert, {-2,2}], Pane[Dynamic@vert, 50], "  shear v ", Slider[Dynamic@shearv, {-89,89,1}], Pane[Dynamic[shearv], 50]},
							{"scale ", Slider[Dynamic@zoom, {0,1.5}], Pane[Dynamic@zoom, 50], "  tilt h ", Slider[Dynamic@tilth, {-89,89,1}], Pane[Dynamic@tilth, 50]},
							{"scale h ", Slider[Dynamic@scaleh, {.1,2}], Pane[Dynamic@scaleh, 50], "  tilt v ", Slider[Dynamic@tiltv, {-89,89,1}], Pane[Dynamic@tiltv, 50]},
							{"scale v ", Slider[Dynamic@scalev, {.1,2}], Pane[Dynamic@scalev, 50], Null}
						}, Alignment->{Right,Left}]},
						{""},
						{Style["RESIZE", Bold, 15]},
						{Row[{
							Button["25%",$dwStyle[[sel,3]] = .25; dwUpdateBoundingBox[{sel}]], 
							Button["50%",$dwStyle[[sel,3]] = .5; dwUpdateBoundingBox[{sel}]], 
							Button["75%",$dwStyle[[sel,3]] = .75; dwUpdateBoundingBox[{sel}]],
							Button["100%",$dwStyle[[sel,3]] = 1; dwUpdateBoundingBox[{sel}]], 
							Button["125%",$dwStyle[[sel,3]] = 1.25; dwUpdateBoundingBox[{sel}]], 
							Button["150%",$dwStyle[[sel,3]] = 1.5; dwUpdateBoundingBox[{sel}]], 
							Button["175%",$dwStyle[[sel,3]] = 1.75; dwUpdateBoundingBox[{sel}]], 
							Button["200%",$dwStyle[[sel,3]] = 2; dwUpdateBoundingBox[{sel}]]
						}]}
					}, Spacings->{0,0}],
					Row[{
						Button["Reset", left = 0; top = 0; right = 0; bottom = 0; horiz = 0; vert = 0; 
							scaleh = 1; scalev = 1; shearh = 0; shearv = 0; tilth = 0; tiltv = 0; zoom = 1],
						Button["Cancel", DialogReturn[]],
						DefaultButton[
							If[img === Null,
								
								$dwStyle[[sel, 2]] = $dwImageFilterImageDefault,
								
								(*If[Head[$dwStyle[[sel, 2]]] =!= Image,
									$dwStyle[[sel, 2]] = Rasterize[Show[$dwStyle[[sel, 2]], ImageSize->ImageDimensions[$dwStyle[[sel, 2]]], ImageResolution->$dwImageResolution], Background->None]
								];*)
								img = If[Head[img] =!= Image, Rasterize[img], img];
								$dwStyle[[sel, 2]] = Rasterize[Show[img, ImageSize->ImageDimensions[img]], ImageResolution->$dwImageResolution, Background->None];
								If[Total[ImageDimensions[$dwStyle[[sel, 2]]]] > 2*$dwImagePastedMaxSize,
									(* resize *)
									$dwStyle[[sel, 2]] = 
										ImageResize[
											ImageTrim[$dwStyle[[sel, 2]], {{left, 1-top}, {1-right, bottom}}, DataRange -> {{0, 1}, {0, 1}}], 
										{{$dwImagePastedMaxSize},{$dwImagePastedMaxSize}}];
									$dwStyle[[sel, 3]] = 1,
									(* do not resize *)
									$dwStyle[[sel, 2]] = 
										ImageTrim[
											ImagePerspectiveTransformation[$dwStyle[[sel, 2]], {{scaleh, Tan[shearh*Pi/180], horiz}, {Tan[shearv*Pi/180], scalev, vert}, {Tan[tiltv*Pi/180], Tan[tilth*Pi/180], 2-zoom}}, Background->Transparent, Masking->All], 
										{{left, 1-top}, {1-right, bottom}}, DataRange -> {{0, 1}, {0, 1}}]
								]
							];
							dwUpdateBoundingBox[{sel}];
							DialogReturn[]
						]
					}]
				},Alignment->Center],
			ImageSize->620]
		],
	Background->LightGray, WindowTitle->"Image", Modal->True]
	
Options[dwCreateImageFromGraphics] = {"SeparateObjects"->False, "UseImageFilter"->True, "Pad"->0};

dwCreateImageFromGraphics[backgroundColor_:None, OptionsPattern[]]:=
	Block[{s = Sort[$dwSelected], plotRangeSave = $dwPlotRange, center, pad = OptionValue["Pad"], temp, saveRotation, saveScale},
		dwSetUndo[];
		dwConvertPointSelectionToLayerSelection[];
		If[OptionValue["SeparateObjects"],
			
			(* separate objects *)
			Do[
				$dwSelected = {sn};
				center = dwFindCenter[Flatten[$dwBoundingBoxes[[{sn}]], 1]];
				pad = If[pad == 0,
						If[Cases[$dwStyle[[s]], _AbsoluteThickness, Infinity] === {}, 0, (Max[#[[1]]&/@Cases[$dwStyle[[s]], _AbsoluteThickness, Infinity]]/216)]+.05,
						pad
					];
				(* create image *)
				If[$dwHead[[sn]] === Image,
					
					temp = ImagePad[Darker[$dwStyle[[sn, 2]], 1], pad, backgroundColor];
					saveRotation = $dwStyle[[sn, 1]];
					saveScale = $dwStyle[[sn, 3]],
					
					$dwPlotRange = dwFindPlotRange[{sn}, "Padding"->pad];
					temp = Rasterize[dwWLDrawToGraphics["LayerNumbers"->{sn}], Background->backgroundColor, ImageResolution->$dwImageResolution];
					saveRotation = 0;
					saveScale = 1
				];
				dwNewEmptyLayer["Form"->"image", "Head"->Image, "SetUndo"->False];
				$dwStyle[[-1,1]] = saveRotation;
				$dwStyle[[-1,2]] = temp;
				$dwStyle[[-1,3]] = saveScale;
				If[!OptionValue["UseImageFilter"], $dwStyle[[-1,4]] = $dwImageFilterValuesDefault, Nothing];
				$dwP[[-1]] = {center};
				dwUpdateBoundingBox[{-1}];
				$dwStyleMode = "image";
				(* delete graphics *)
				$dwSelected = {sn};
				dwDeleteLayer["SetUndo"->False],
			{sn, Reverse@s}];
			$dwPointQuantity = Length[Flatten[$dwP, 1]],
			
			(* together in one object *)
			center = dwFindCenter[Flatten[$dwBoundingBoxes[[s]], 1]];
			pad = If[pad == 0,
					If[Cases[$dwStyle[[s]], _AbsoluteThickness, Infinity] === {}, 0, (Max[#[[1]]&/@Cases[$dwStyle[[s]], _AbsoluteThickness, Infinity]]/216)]+.05,
					pad
				];
			(* create image *)
			$dwPlotRange = dwFindPlotRange[s, "Padding"->pad];
			temp = Rasterize[dwWLDrawToGraphics["LayerNumbers"->s], Background->backgroundColor, ImageResolution->$dwImageResolution];
			dwNewEmptyLayer["Form"->"image", "Head"->Image, "SetUndo"->False];
			$dwStyle[[-1,2]] = temp;
			$dwP[[-1]] = {center};
			dwUpdateBoundingBox[{-1}];
			$dwStyleMode = "image";
			(* delete graphics *)
			$dwSelected = s;
			dwDeleteLayer["SetUndo"->False]
		];
		dwDeleteEmptyLayers[];
		$dwPlotRange = plotRangeSave;
		$dwPointQuantity = Length[Flatten[$dwP, 1]];
		(* set selection *)
		$dwSelected = {Length[$dwP]}
	]

End[] (* End Private Context *)

EndPackage[]