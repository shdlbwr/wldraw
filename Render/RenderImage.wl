(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderImage[n_]:=
	Block[{temp, filterNum, secondObjNum},
		
		If[Head[$dwStyle[[n,2]]] === Image,
			If[$dwStyle[[n,4]] =!= {} && $dwStyle[[n,4,1,1]] =!= "None",
							
				(* check for filters using additional objects *)
				Which[
					(* Blend *)
					Flatten[Position[$dwStyle[[n]],{"Blend",__},Infinity]] =!= {},
						If[n == 1 || !$dwShowImageBlends,
							Rotate[Inset[dwRenderImageObject[n], $dwP[[n,1]], Automatic, $dwStyle[[n,3]]*(ImageDimensions[$dwStyle[[n, 2]]][[1]]/$dwImageResAdjustment)], $dwStyle[[n,1]]],
							dwBlendImage[n]
						],
						
					(* ObjectMask *)
					Flatten[Position[$dwStyle[[n]],{"Mask",__},Infinity]] =!= {},
						filterNum = Position[$dwStyle[[n]], {"Mask", __}, Infinity][[1]];
						secondObjNum = $dwStyle[[Sequence@@Join[{n}, filterNum, {2}]]];
						secondObjNum = If[secondObjNum > Length[$dwP], filterNum, secondObjNum];
						If[secondObjNum == n,
							(* same object *)
							Rotate[Inset[dwRenderImageObject[n], $dwP[[n,1]], Automatic, $dwStyle[[n,3]]*(ImageDimensions[$dwStyle[[n, 2]]][[1]]/$dwImageResAdjustment)], $dwStyle[[n,1]]],
							(* different object *)
							dwMaskImage[n, secondObjNum]
						],
					
					True,
						Rotate[Inset[
							If[$dwStyle[[n,5]], 
								RemoveBackground[dwRenderImageObject[n]], 
								dwRenderImageObject[n]
							], 
							$dwP[[n,1]], 
							Automatic, 
							$dwStyle[[n,3]]*(ImageDimensions[$dwStyle[[n, 2]]][[1]]/$dwImageResAdjustment)], 
							$dwStyle[[n,1]]
						]
				],
				
				(* unfiltered objects *)
				Rotate[Inset[
					If[$dwStyle[[n,5]], 
						RemoveBackground[dwRenderImageObject[n]], 
						dwRenderImageObject[n]
					], 
					$dwP[[n,1]], 
					Automatic, 
					$dwStyle[[n,3]]*(ImageDimensions[$dwStyle[[n, 2]]][[1]]/$dwImageResAdjustment)], 
					$dwStyle[[n,1]]
				]
			],
			
			(* insert graphics - this should not be needed but here just in case*)
			Rotate[Inset[
				If[$dwStyle[[n,5]], 
					RemoveBackground[dwRenderImageObject[n]], 
					dwRenderImageObject[n]
				], 
				$dwP[[n,1]], 
				Automatic, 
				$dwStyle[[n,3]]*(ImageDimensions[$dwStyle[[n, 2]]][[1]]/$dwImageResAdjustment)], 
				$dwStyle[[n,1]]
			]
		]
	]

(* blends image with all layers below *)
dwBlendImage[n_]:=
	Block[{temp, temp2, plotrange},
		temp2 = {$dwMode, $dwShowImageBlends};
		$dwMode = "wireframe";
		$dwShowImageBlends = False;
		plotrange = RegionBounds[Point[$dwBoundingBoxes[[n]]]];
		temp = 
			ImageMultiply[
				(* blend image *)
				Graphics[
					Rotate[
						Inset[
							dwRenderImageObject[n],
							$dwP[[n, 1]], 
							Automatic, 
							$dwStyle[[n, 3]]*(ImageDimensions[$dwStyle[[n, 2]]][[1]]/$dwImageResAdjustment)
							], 
						$dwStyle[[n, 1]]
					],
					ImageSize->Rasterize[Rotate[$dwStyle[[n, 2]], $dwStyle[[n, 1]]], "RasterSize", ImageResolution->$dwImageResolution],
					PlotRangePadding->None, PlotRange->plotrange
				],
				(* all objects below blend image *)
				Rasterize[
					Show[
						dwWLDrawToGraphics["LayerNumbers"->Range[1, n-1], "PlotRange"->plotrange, 
							"ImageSize"->Rasterize[Rotate[$dwStyle[[n, 2]], $dwStyle[[n, 1]]], "RasterSize", ImageResolution->$dwImageResolution]],
						PlotRangePadding->None
					]/.{Inset[ins_, p1_, p2_] :> Inset[ins, p1, p2, 1]},
					Background->Transparent
				]
			];
		$dwShowImageBlends = temp2[[2]];
		$dwMode = temp2[[1]];
		Inset[
			If[$dwStyle[[n,5]], 
				RemoveBackground[temp], 
				temp
			],
			$dwP[[n, 1]], 
			Automatic, 
			ManhattanDistance[Sequence@@plotrange[[1]]]
		]
	]

(* image can be masked by image, shape or text -  text mask resizes with exported image size while text will not *)
dwMaskImage[n_, maskn_]:=
	Block[{	temp, objectsize, masksize, plotrange},
		
		{objectsize, masksize, plotrange} = {
			ImageDimensions[$dwStyle[[n, 2]]][[1]],
			If[$dwHead[[maskn]] === Image, (* only used if mask is image *)
				ImageDimensions[$dwStyle[[maskn, 2]]][[1]],
				{0,0}
			],
			RegionBounds[Point[$dwBoundingBoxes[[n]]]]
		};
		
		temp = SetAlphaChannel[
					
					(* image *)
					Graphics[
						Rotate[
							Inset[
								dwRenderImageObject[n], 
								$dwP[[n, 1]], 
								Automatic,
								$dwStyle[[n, 3]]*(objectsize/$dwImageResAdjustment)], 
							$dwStyle[[n, 1]]
						], 
						PlotRangePadding->None, PlotRange->plotrange
					],
					
					(* mask *)
					Switch[$dwHead[[maskn]],
						Image,
							Graphics[
								Rotate[
									Inset[dwRenderImageObject[maskn], 
										$dwP[[maskn, 1]], 
										Automatic, 
										$dwStyle[[maskn, 3]]*(masksize/$dwImageResAdjustment)],
									$dwStyle[[maskn, 1]]
								], 
								PlotRangePadding->None, PlotRange->plotrange
							],
						Text,
							Graphics[
								Inset[
									ImageEffect[
										Rasterize[
											dwWLDrawToGraphics["LayerNumbers"->{maskn}, "RenderHiddenLayers"->True, "PlotRange"->RegionBounds[Point[$dwBoundingBoxes[[maskn]]]]], 
											ImageResolution->$dwImageResolution
										],
										{"Solarization", 0}
									], 
									dwFindCenter[Transpose[RegionBounds[Point[$dwBoundingBoxes[[maskn]]]]]], 
									Automatic, 
									2
								],
								Background->Black, PlotRangePadding->None, PlotRange->plotrange
							],
						_,
							Graphics[
								Inset[ImageEffect[Rasterize[
									Graphics[{Black, 
										Polygon[
											Switch[$dwHead[[maskn]],
												BezierCurve, 
													dwBezierDiscretizeList[$dwP[[maskn]], 40],
												BSplineCurve, 
													dwBSplineDiscretizeList[$dwP[[maskn]], 40],
												_,
													$dwP[[maskn]]
											]
										]}, PlotRangePadding->None, PlotRange->plotrange
									], ImageResolution->$dwImageResolution], {"Solarization", 0}], 
									dwFindCenter[Transpose[plotrange]], 
									Automatic, 
									ManhattanDistance[Sequence@@plotrange[[1]]]
								],
								PlotRangePadding->None, PlotRange->plotrange
							]
					]
				];
		Inset[
			If[$dwStyle[[n,5]], 
				RemoveBackground[temp], 
				temp
			],
			$dwP[[n, 1]] + (dwFindCenter[Transpose[plotrange]] - $dwP[[n, 1]]), 
			Automatic,
			ManhattanDistance[Sequence@@plotrange[[1]]]
		]
	]
	

dwRenderImageObject[n_]:=
	Block[{temp,temp1,squiggle,ctr=1,cd},
		If[MemberQ[{Image}, Head[$dwStyle[[n,2]]](*Head[$dwStyle[[n,2]]]*)],
			
			(* convert image to rgb since some filters use only rgb color space; bad image if Rasterize used here *)
			temp = ColorConvert[$dwStyle[[n,2]],"RGB"];
			
			Do[temp1 = 
				Switch[$dwStyle[[n,4,n2,1]],
					"AutoAdjust",
						ImageAdjust[temp],
					"Blend"|"Mask"|"None",
						temp,
					"BalancedBinarize",
						LocalAdaptiveBinarize[temp,$dwStyle[[n,4,n2,2]]],
					Blur,
						GaussianFilter[temp, $dwStyle[[n,4,n2,2]], Padding->None],
					"BlurRemoval",
						ImageDeconvolve[temp,GaussianMatrix[$dwStyle[[n,4,n2,2]]]],
					"Brightness",
						ImageAdjust[temp, {0,$dwStyle[[n,4,n2,2]]}],
					"Charcoal",
						ImageEffect[temp,{"Charcoal",IntegerPart[$dwStyle[[n,4,n2,2]]]}, RandomSeeding->1],
					"Color",
						ImageMultiply[temp,$dwStyle[[n,4,n2,5]],$dwStyle[[n,4,n2,2]]],
					"ColorBoosting",
						ImageEffect[temp,"ColorBoosting"],
					"Colorize",
						Colorize[temp, ColorFunction->(Blend[$dwStyle[[n,4,n2,5]][[Table[5-$dwStyle[[n,4,n2,2]]+ctn, {ctn, $dwStyle[[n,4,n2,2]]}]]],#]&)],
					"Comics",
						ImageEffect[temp,{"Comics",{.5-.49$dwStyle[[n,4,n2,2]],.5+.49$dwStyle[[n,4,n2,2]]}}],
					"Contrast",
						ImageAdjust[temp,$dwStyle[[n,4,n2,2]]],
					CurvatureFlowFilter,
						CurvatureFlowFilter[temp,$dwStyle[[n,4,n2,2]]+$MachineEpsilon],
					"DarkenLightPixels",
						Opening[temp,$dwStyle[[n,4,n2,2]]],
					"Decolorization",
						ImageEffect[temp,"Decolorization"],
					"Deuteranopia",
						ImageEffect[temp,"Deuteranopia"],
					"EdgeStylization",
						ImageEffect[temp,{"EdgeStylization",$dwStyle[[n,4,n2,2]]}, RandomSeeding->1],
					"Embossing",
						ImageEffect[temp,{"Embossing",$dwStyle[[n,4,n2,2]],Pi/4}],
					"FrameFaded",
						ImageEffect[temp,{"FadedFrame",Scaled[$dwStyle[[n,4,n2,2]]/$dwStyle[[n,4,n2,4]]],All}],
					"FrameTorn",
						ImageEffect[temp,{"TornFrame",Scaled[$dwStyle[[n,4,n2,2]]],$dwStyle[[n,4,n2,5]], .2}, RandomSeeding->12],
					"FrameVignette",
						ImageEffect[temp,{"Vignette",$dwStyle[[n,4,n2,2]]*ImageDimensions[$dwStyle[[n,2]]][[1]]*$dwStyle[[n,3]],$dwStyle[[n,4,n2,5]]}],
					"FrameWave",
						ImageEffect[temp,{"Frame",Cos[#/($dwStyle[[n,4,n2,2]]+$MachineEpsilon)] &, Scaled[.1], {$dwStyle[[n,4,n2,5]]}}],
					"Gamma",
						ImageAdjust[temp, {0,0,$dwStyle[[n,4,n2,2]]+$MachineEpsilon}],
					"GaussianNoise",
						ImageEffect[temp,{"GaussianNoise",$dwStyle[[n,4,n2,2]]}, RandomSeeding->1],
					GeometricMeanFilter|HarmonicMeanFilter|MedianFilter,
						$dwStyle[[n,4,n2,1]][temp,IntegerPart[$dwStyle[[n,4,n2,2]]]],
					GradientFilter|GradientOrientationFilter|LaplacianGaussianFilter|RidgeFilter,
						ImageAdjust[$dwStyle[[n,4,n2,1]][temp,$dwStyle[[n,4,n2,2]]]],
					"Grayscale",
						ImageAdjust[ImageAdjust[ColorConvert[temp,"Grayscale"]],{0,0,(2-2*$dwStyle[[n,4,n2,2]])+$MachineEpsilon}],
					"HandDrawn",
						ctr=1;
						squiggle=Partition[Riffle[
								Flatten[ImageData[15Blur[SeedRandom[$dwStyle[[n,4,n2,5]]];RandomImage[{-1,1},ImageDimensions[temp]], 10(2.5 - $dwStyle[[n,4,n2,2]])]]],
								Flatten[ImageData[15Blur[SeedRandom[$dwStyle[[n,4,n2,5]]+1];RandomImage[{-1,1},ImageDimensions[temp]], 10(2.5 - $dwStyle[[n,4,n2,2]])]]]],
							2,2];
						ImageTransformation[temp,(#+squiggle[[Min[ctr++, Length[squiggle]]]])&, DataRange->Full, Padding->White],
					HighpassFilter,
						ImageAdjust[$dwStyle[[n,4,n2,1]][RemoveAlphaChannel[temp], $dwStyle[[n,4,n2,2]]]],
					"Increase",
						ImageFilter[Max,temp,$dwStyle[[n,4,n2,2]]],
					"Invert",
						ImageEffect[temp,{"Solarization",1-$dwStyle[[n,4,n2,2]]}],
					"IsolateColor",
						If[ColorQ[FindMatchingColor[$dwStyle[[n,2]], $dwStyle[[n,4,n2,5]]]],
							cd = FillingTransform@SelectComponents[Binarize[ColorDetect[$dwStyle[[n,2]], ColorsNear[FindMatchingColor[$dwStyle[[n,2]], $dwStyle[[n,4,n2,5]]], $dwStyle[[n,4,n2,2]]]]], "Count", 9999];
							HighlightImage[$dwStyle[[n,2]], {EdgeForm[], cd}, "Desaturate"],
							$dwStyle[[n,2]]
						],
					"Jitter",
						ImageEffect[temp,{"Jitter",$dwStyle[[n,4,n2,2]]}, RandomSeeding->1],
					"KeepColor",
						RemoveBackground[temp, {"Foreground",{$dwStyle[[n,4,n2,5]], $dwStyle[[n,4,n2,2]]}}],
					"LightenDarkPixels",
						Closing[temp,$dwStyle[[n,4,n2,2]]],
					"MotionBlur",
						ImageEffect[temp,{"MotionBlur",IntegerPart[$dwStyle[[n,4,n2,2]]],$dwStyle[[n,4,n2,5]]}],
					"Noise",
						ImageEffect[temp,{$dwStyle[[n,4,n2,5]],$dwStyle[[n,4,n2,2]]}, RandomSeeding->1],
					"NoiseRemoval",
						TotalVariationFilter[temp,$dwStyle[[n,4,n2,2]],Method->"Poisson"],
					"OilPainting",
						ImageEffect[temp,{"OilPainting",IntegerPart[$dwStyle[[n,4,n2,2]]]}, RandomSeeding->1],
					"Opacity",
						SetAlphaChannel[temp, Darker[AlphaChannel[$dwStyle[[n,2]]], 1-$dwStyle[[n,4,n2,2]]]],
					"PoissonNoise",
						ImageEffect[temp,{"PoissonNoise",$dwStyle[[n,4,n2,2]]}, RandomSeeding->1],
					"Posterization",
						ImageEffect[temp,{"Posterization",IntegerPart[$dwStyle[[n,4,n2,2]]]}],
					"Protanopia",
						ImageEffect[temp,"Protanopia"],
					RangeFilter,
						$dwStyle[[n,4,n2,1]][RemoveAlphaChannel[temp],$dwStyle[[n,4,n2,2]]],
					"Reduce",
						ImageFilter[Min,temp,$dwStyle[[n,4,n2,2]]],
					"RemoveColor",
						RemoveBackground[temp, {"Background",{$dwStyle[[n,4,n2,5]], $dwStyle[[n,4,n2,2]]}}],
					"ReplaceColor",
						ColorReplace[temp, $dwStyle[[n,4,n2,5,1]]->$dwStyle[[n,4,n2,5,2]], $dwStyle[[n,4,n2,2]]],
					"SaltPepperNoise",
						ImageEffect[temp,{"SaltPepperNoise",$dwStyle[[n,4,n2,2]]}, RandomSeeding->1],
					"Sepia",
						ImageEffect[temp,"Sepia"],
					"Spherize",
						dwImageSpherize[temp, $dwStyle[[n,4,n2,2]], $dwStyle[[n,4,n2,6]]],
					StandardDeviationFilter,
						ImageAdjust[$dwStyle[[n,4,n2,1]][RemoveAlphaChannel[temp], IntegerPart[$dwStyle[[n,4,n2,2]]]]],
					"Stippling",
						ImageEffect[temp,{"Stippling",IntegerPart[$dwStyle[[n,4,n2,2]]]}, RandomSeeding->1],
					"Tritanopia",
						ImageEffect[temp,"Tritanopia"],
					"TwistInner",
						dwImageSpiral[temp, "Rotate" -> $dwStyle[[n,4,n2,2]], "Size" -> .7, "TwistCenterFirst" -> True],
					"TwistOuter",
						dwImageSpiral[temp, "Rotate" -> $dwStyle[[n,4,n2,2]], "Size" -> .7, "TwistCenterFirst" -> False],
					"WeightedCorner",
						ImageTransformation[temp, #^$dwStyle[[n,4,n2,2]] &],
					_,
						$dwStyle[[n,4,n2,1]][temp,$dwStyle[[n,4,n2,2]]]
				];
				temp = ColorConvert[temp1,"RGB"],
			{n2, Length[$dwStyle[[n,4]]]}]
		];
		Return[temp]
	]

End[] (* End Private Context *)

EndPackage[]