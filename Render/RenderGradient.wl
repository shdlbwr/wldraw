(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderGradient[n_]:=
	If[$dwStyle[[n,8,1]] =!= None,
		Switch[$dwStyle[[n,8,1]],
			"Solid",
				Texture[
					Graphics[{
						$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]],1,1]], Thickness[1], 
						Line[{{0,.5},{1,.5}}]},
					ImagePadding->0,PlotRangePadding->0]],
			"Horizontal",
				Texture[
					Graphics[{Thickness[1],Line[
						Table[{a,.5},{a,#[[1]]&/@$dwStyle[[n,8,2]]}],
						VertexColors->(#[[2]]&/@$dwStyle[[n,8,2]])]},
					ImagePadding->0,PlotRangePadding->0]
				],
			"Vertical",
				Texture[
					Graphics[{Thickness[1],Line[
						Table[{.5,a},{a,#[[1]]&/@$dwStyle[[n,8,2]]}],
						VertexColors->(#[[2]]&/@$dwStyle[[n,8,2]])]},
					ImagePadding->0,PlotRangePadding->0]
				],
			"Angle",
				(*If[MemberQ[{88, 89, 91, 92}, $dwStyle[[n,8,3]]], $dwStyle[[n,8,3]] = 90];(* fix for distorted gradient *)
				If[MemberQ[{268, 269, 271, 272}, $dwStyle[[n,8,3]]], $dwStyle[[n,8,3]] = 270];(* fix for distorted gradient *)*)
				Texture[Graphics[{Thickness[4],
					Rotate[Line[Join[{{.5, -.0001}}, Table[{.5, a} ,{a, #[[1]]&/@$dwStyle[[n,8,2]]}], {{.5,1.0001}}],
					VertexColors->(Join[{$dwStyle[[n,8,2,1,2]]}, #[[2]]&/@$dwStyle[[n,8,2]], {$dwStyle[[n,8,2,4,2]]}])], $dwStyle[[n,8,3]]\[Degree]]},
				ImagePadding->0, PlotRange->{$dwStyle[[n,8,4]],$dwStyle[[n,8,4]]}, PlotRangePadding->0]],
			"Radial",
				Texture[
					Graphics[{
						$dwStyle[[n,8,2,4,2]],
						Rectangle[{-1,-1},{1,1}],
						Polygon[Join[{$dwStyle[[n,9]][[{1,2}]]},Table[$dwStyle[[n,9,3]]{Nest[Sin,Sin[x],$dwStyle[[n,9,4]]],Nest[Sin,Cos[x],$dwStyle[[n,9,4]]]},{x,0,2Pi,2Pi/50}]],
							VertexColors->Flatten[{$dwStyle[[n,8,2,1,2]],Table[$dwStyle[[n,8,2,4,2]],{51}]}]
						]},
					ImagePadding->0,PlotRangePadding->0,PlotRange->1]
				],
			"EdgeDark",
				If[MemberQ[Flatten[$dwCompoundPathLayers], n],
					
					(* first ImageTake moves texture down, up, right or left; second ImageTake solves issue of dark spots where image does not have enough edge padding *)
					Texture[
						ImageTake[
							
							ImageTake[
								ImageMultiply[GaussianFilter[ColorNegate[ImageAdjust[GradientFilter[Graphics[
									FilledCurve[
										Table[
											If[$dwHead[[s]] === BezierCurve,
												{$dwHead[[s]][Most[$dwP[[s]]]]},
												{$dwHead[[s]][$dwP[[s]]]}
											],
										{s,$dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]]}]
									], PlotRangePadding->None, ImagePadding->$dwDefaultGradientPad], $dwStyle[[n,9,1]], Method->{"DerivativeKernel"->"Sobel"}]]], $dwStyle[[n,9,2]]], ColorConvert[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]], "RGB"]
								], {$dwDefaultGradientPad, -$dwDefaultGradientPad}, {$dwDefaultGradientPad, -$dwDefaultGradientPad}
							],
								
							If[$dwStyle[[n,9,3]] < 0, {1, $dwStyle[[n,9,3]]}, {$dwStyle[[n,9,3]], -1}],
							If[$dwStyle[[n,9,4]] < 0, {1, $dwStyle[[n,9,4]]}, {$dwStyle[[n,9,4]], -1}]
						]
					],
					
					If[$dwHead[[n]]===BezierCurve,
						Texture[
							ImageTake[
								ImageTake[
									ImageMultiply[GaussianFilter[ColorNegate[ImageAdjust[GradientFilter[Graphics[{FilledCurve[{$dwHead[[n]][Most[$dwP[[n]]]]}]}, PlotRangePadding->None,ImagePadding->$dwDefaultGradientPad], $dwStyle[[n,9,1]], Method->{"DerivativeKernel"->"Sobel"}]]], $dwStyle[[n,9,2]]], ColorConvert[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]], "RGB"]], 
									{$dwDefaultGradientPad, -$dwDefaultGradientPad}, {$dwDefaultGradientPad, -$dwDefaultGradientPad}
								],
								If[$dwStyle[[n,9,3]] < 0, {1, $dwStyle[[n,9,3]]}, {$dwStyle[[n,9,3]], -1}],
								If[$dwStyle[[n,9,4]] < 0, {1, $dwStyle[[n,9,4]]}, {$dwStyle[[n,9,4]], -1}]
							]
						],
						Texture[
							ImageTake[
								ImageTake[
									ImageMultiply[GaussianFilter[ColorNegate[ImageAdjust[GradientFilter[Graphics[{$dwHead[[n]][$dwP[[n]]]}, PlotRangePadding->None,ImagePadding->$dwDefaultGradientPad], $dwStyle[[n,9,1]], Method->{"DerivativeKernel"->"Sobel"}]]], $dwStyle[[n,9,2]]], ColorConvert[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]], "RGB"]], 
									{$dwDefaultGradientPad, -$dwDefaultGradientPad}, {$dwDefaultGradientPad, -$dwDefaultGradientPad}
								],
								If[$dwStyle[[n,9,3]] < 0, {1, $dwStyle[[n,9,3]]}, {$dwStyle[[n,9,3]], -1}],
								If[$dwStyle[[n,9,4]] < 0, {1, $dwStyle[[n,9,4]]}, {$dwStyle[[n,9,4]], -1}]
							]
						]
					]
				],
			"EdgeFade",
				Block[{
						colorlight=Lighter[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],$dwStyle[[n,8,2,1,1]]],
						color=$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],
						colordark=Darker[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],$dwStyle[[n,8,2,2,1]]]
					},
					
					If[MemberQ[Flatten[$dwCompoundPathLayers], n],
						
						Texture[ColorReplace[SetAlphaChannel[Blur[Graphics[{Opacity[1],
							FilledCurve[
								Table[
									If[$dwHead[[s]] === BezierCurve,
										{$dwHead[[s]][Most[$dwP[[s]]]]},
										{$dwHead[[s]][$dwP[[s]]]}
									],
								{s,$dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]]}]
							]},PlotRangePadding->None],$dwStyle[[n,9,1]]],ImageAdjust[ImageClip[Blur[ColorNegate[Graphics[{
							FilledCurve[
								Table[
									If[$dwHead[[s]] === BezierCurve,
										{$dwHead[[s]][Most[$dwP[[s]]]]},
										{$dwHead[[s]][$dwP[[s]]]}
									],
								{s,$dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]]}]
							]},PlotRangePadding->None]],$dwStyle[[n,9,1]]],{1-$dwStyle[[n,9,2]],2}]]],Black->$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]]]],
						
						If[$dwHead[[n]]===BezierCurve,
							Texture[ColorReplace[SetAlphaChannel[Blur[Graphics[{Opacity[1],FilledCurve[{$dwHead[[n]][Most[$dwP[[n]]]]}]},PlotRangePadding->None],$dwStyle[[n,9,1]]],ImageAdjust[ImageClip[Blur[ColorNegate[Graphics[{FilledCurve[{$dwHead[[n]][Most[$dwP[[n]]]]}]},PlotRangePadding->None]],$dwStyle[[n,9,1]]],{1-$dwStyle[[n,9,2]],2}]]],Black->$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]]]],
							Texture[ColorReplace[SetAlphaChannel[Blur[Graphics[{Opacity[1],Polygon[$dwP[[n]]]},PlotRangePadding->None],$dwStyle[[n,9,1]]],ImageAdjust[ImageClip[Blur[ColorNegate[Graphics[Polygon[$dwP[[n]]],PlotRangePadding->None]],$dwStyle[[n,9,1]]],{1-$dwStyle[[n,9,2]],1}]]],Black->$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]]]]
						]
					]
				],
			"Emboss"|"EmbossReverse",
				Block[{
						colorlight=Lighter[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],$dwStyle[[n,8,2,3,1]]],
						color=$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],
						colordark=Darker[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],$dwStyle[[n,8,2,4,1]]]
					},
					
					If[MemberQ[Flatten[$dwCompoundPathLayers], n],
						
						Texture[
							ImageTake[
								Colorize[
									Sharpen[
										ImageAdjust[
											Plus[Sequence@@DerivativeFilter[Graphics[{EdgeForm[{Thickness[$dwStyle[[n,8,2,2,1]]],GrayLevel[1-$dwStyle[[n,9,2]]]}],GrayLevel[$dwStyle[[n,9,2]]],
												FilledCurve[
													Table[
														If[$dwHead[[s]] === BezierCurve,
															{$dwHead[[s]][Most[$dwP[[s]]]]},
															{$dwHead[[s]][$dwP[[s]]]}
														],
													{s,$dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]]}]
												]},PlotRangePadding->None,ImagePadding->$dwDefaultGradientPad],{{1,0},{0,1}}, $dwStyle[[n,9,1]]]]
										],
										$dwStyle[[n,8,2,1,1]]
									], ColorFunction->(Blend[{colorlight, color, colordark}, #]&)
								],{$dwDefaultGradientPad, -$dwDefaultGradientPad}, {$dwDefaultGradientPad, -$dwDefaultGradientPad}
							]
						],
						
						If[$dwHead[[n]]===BezierCurve,
							Texture[
								ImageTake[
									Colorize[
										Sharpen[
											ImageAdjust[
												Plus[Sequence@@DerivativeFilter[Graphics[{EdgeForm[{Thickness[$dwStyle[[n,8,2,2,1]]],GrayLevel[1-$dwStyle[[n,9,2]]]}],GrayLevel[$dwStyle[[n,9,2]]],FilledCurve[{$dwHead[[n]][Most[$dwP[[n]]]]}]},PlotRangePadding->None,ImagePadding->$dwDefaultGradientPad],{{1,0},{0,1}}, $dwStyle[[n,9,1]]]]
											],
											$dwStyle[[n,8,2,1,1]]
										], ColorFunction->(Blend[{colorlight, color, colordark}, #]&)
									],{$dwDefaultGradientPad, -$dwDefaultGradientPad}, {$dwDefaultGradientPad, -$dwDefaultGradientPad}
								]
							],
							Texture[
								ImageTake[
									Colorize[
										Sharpen[
											ImageAdjust[
												Plus[Sequence@@DerivativeFilter[Graphics[{EdgeForm[{Thickness[$dwStyle[[n,8,2,2,1]]],GrayLevel[1-$dwStyle[[n,9,2]]]}],GrayLevel[$dwStyle[[n,9,2]]],$dwHead[[n]][$dwP[[n]]]},PlotRangePadding->None,ImagePadding->$dwDefaultGradientPad],{{1,0},{0,1}}, $dwStyle[[n,9,1]]]]
											],
											$dwStyle[[n,8,2,1,1]]
										], ColorFunction->(Blend[{colorlight, color, colordark}, #]&)
									],{$dwDefaultGradientPad, -$dwDefaultGradientPad}, {$dwDefaultGradientPad, -$dwDefaultGradientPad}
								]
							]
						]
					]
				],
			"Noise",
				Block[{
						colorlight=Lighter[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],$dwStyle[[n,9,2]]],
						colordark=$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]]
					},
					If[$dwHead[[n]]===BezierCurve,
						Texture[Colorize[ImageEffect[Graphics[{$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]], FilledCurve[{$dwHead[[n]][Most[$dwP[[n]]]]}]}, Background->$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]]], {"SaltPepperNoise",{$dwStyle[[n,9,1]],0}}], ColorFunction->(Blend[{colorlight, colordark}, #]&)]],
						Texture[Colorize[ImageEffect[Graphics[{$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]], Polygon[$dwP[[n]]]}, Background->$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]]], {"SaltPepperNoise",{$dwStyle[[n,9,1]],0}}], ColorFunction->(Blend[{colorlight, colordark}, #]&)]]
					]
				],
			"Rim"|"RimReverse",
				Block[{
						colorlight=Lighter[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],$dwStyle[[n,8,2,1,1]]],
						color=$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],
						colordark=Darker[$dwStyle[[n,Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],$dwStyle[[n,8,2,2,1]]]
					},
					
					If[MemberQ[Flatten[$dwCompoundPathLayers], n],
						
						Texture[
							ImageTake[
								Colorize[Blur[ImageEffect[Graphics[{
									FilledCurve[
										Table[
											If[$dwHead[[s]] === BezierCurve,
												{$dwHead[[s]][Most[$dwP[[s]]]]},
												{$dwHead[[s]][$dwP[[s]]]}
											],
										{s,$dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]]}]
									]
									},PlotRangePadding->None,ImagePadding->$dwDefaultGradientPad], {"Embossing", 3, $dwStyle[[n,9,2]]}], $dwStyle[[n,9,1]]], ColorFunction->(Blend[{colorlight, color, colordark}, #]&)
								],{$dwDefaultGradientPad, -$dwDefaultGradientPad}, {$dwDefaultGradientPad, -$dwDefaultGradientPad}
							]
						],
						
						If[$dwHead[[n]]===BezierCurve,
							Texture[
								ImageTake[
									Colorize[Blur[ImageEffect[Graphics[{FilledCurve[{$dwHead[[n]][Most[$dwP[[n]]]]}]},PlotRangePadding->None,ImagePadding->$dwDefaultGradientPad], {"Embossing", 3, $dwStyle[[n,9,2]]}], $dwStyle[[n,9,1]]], ColorFunction->(Blend[{colorlight, color, colordark}, #]&)],
									{$dwDefaultGradientPad, -$dwDefaultGradientPad}, {$dwDefaultGradientPad, -$dwDefaultGradientPad}
								]
							],
							Texture[
								ImageTake[
									Colorize[Blur[ImageEffect[Graphics[{Polygon[$dwP[[n]]]},PlotRangePadding->None,ImagePadding->$dwDefaultGradientPad], {"Embossing", 3, $dwStyle[[n,9,2]]}], $dwStyle[[n,9,1]]], ColorFunction->(Blend[{colorlight, color, colordark}, #]&)],
									{$dwDefaultGradientPad, -$dwDefaultGradientPad}, {$dwDefaultGradientPad, -$dwDefaultGradientPad}
								]
							]
						]
					]
				],
			_,{}
		],
	{}]

End[] (* End Private Context *)

EndPackage[]