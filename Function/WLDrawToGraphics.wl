(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(* 
	same as "allLayers" Table of RenderLayersNotAddingOrMovingPoints.m except:
	1. remove Annotation wrappers
	2. add If[CurrentValue[$dwCommandKey], Nothing, < gradient|style code >] to gradient and style 
	
	If changes to "insert compound paths" section, copy and paste appropriate parts 
*)
Options[dwWLDrawToGraphics] = {"LayerNumbers"->"All", "RenderHiddenLayers"->False, "PlotRange"->Automatic, 
	"ImageSize"->Automatic};
dwWLDrawToGraphics[OptionsPattern[]]:=
	Block[{layerNumbers, allLayers, compoundPath = Table[FilledCurve[{}], Length[$dwCompoundPathLayers]], pts, blendPts, imageSize, dash, combinedlayers, deleteLayers},
		
		{imageSize, layerNumbers} = {
			OptionValue["ImageSize"],
			If[OptionValue["LayerNumbers"] === "All", Range[Length[$dwP]], Flatten[{OptionValue["LayerNumbers"]}]]
		};
		
		allLayers = Table[
			If[$dwP[[n]] =!= {} && (MemberQ[layerNumbers, n] && !MemberQ[If[OptionValue["RenderHiddenLayers"], {}, $dwHideLayers], n]),
				{
					If[CurrentValue[$dwCommandKey], Nothing,
						If[($dwHead[[n]] === Polygon && $dwStyle[[n,8,1]] =!= None) || ($dwHead[[n]] === BezierCurve && (Length[$dwP[[n]]] > 2 && $dwStyle[[n,1]])),
							$dwObjectGradients[[n]],
							Nothing
						]
					],
					If[CurrentValue[$dwCommandKey], Nothing,
						If[FreeQ[{Text, Image, "Expression", "Text3D"}, $dwHead[[n]]] && FreeQ[Flatten[$dwCompoundPathLayers], n],
							Sequence@@$dwStyle[[n]][[$dwStyleStart;;-1]],
							Nothing
						]
					], 
					Switch[$dwHead[[n]],
						"Expression",
							dwRenderExpression[n],
						Image,
							dwRenderImage[n],
						"Text3D",
							dwAxoText[$dwStyle[[n,2]], $dwStyle[[n]][[Join[Range[4,11], {15}]]], "Center"->$dwP[[n,1]], "Tilt"->$dwStyle[[n,1,1]], "Turn"->$dwStyle[[n,1,2]], "Direction"->$dwStyle[[n,1,3]], "RotateOrder"->$dwStyle[[n,1,4]], "AxisRotation"->{{$dwStyle[[n,1,5,3]],"Top"},{$dwStyle[[n,1,5,1]],"Right"},{$dwStyle[[n,1,5,2]],"Left"}}],
						Text,
							If[$dwStyle[[n]][[12]]=!=None,
								dwTextOnCurve[
									ToString[$dwStyle[[n,2]]],
									BSplineFunction[$dwP[[$dwStyle[[n,12]]]],SplineDegree->$dwStyle[[$dwStyle[[n,12]],11]],SplineClosed->$dwStyle[[$dwStyle[[n,12]],10]]], 
									$dwStyle[[n,Join[Range[4,11], {15}]]], 
									$dwStyle[[n,3,2]],
									"KerningByCharacter"->If[$dwTextOnCurveKerningOn, $dwTextOnCurveKerning, {}],
									"KernCharacters"->If[$dwTextOnCurveKerningOn, $dwTextOnCurveKerningChars, {}],
									"Position"->$dwStyle[[n,14]],
									"Spacing"->$dwStyle[[n,13]]
								],
								Rotate[Text[Style[Sequence@@($dwStyle[[n]][[Join[Range[2,11], {15}]]])], $dwP[[n,1]], $dwStyle[[n,3]]], $dwStyle[[n,1]], $dwP[[n,1]]]
							],
						Polygon,
							If[$dwStyle[[n,8,1]] =!= None,
								Switch[$dwStyle[[n,8,1]],
									"OpacityGradient",
										pts = DeleteDuplicates[$dwP[[n]]];
										Polygon[pts, VertexColors->Table[Opacity[Rescale[If[n1<=Length[pts]/2,(n1-1),(Length[pts]-n1)],{0,(Length[pts]-1)/2}],Blend[$dwStyle[[n,8,2]],If[n1<=Length[pts]/2,n1/(Length[pts]/2),((Length[pts])-(n1-1))/(Length[pts]/2)]]],{n1,Length[pts]}]],
									{"BlendGradient", _, _, _},
										If[n != $dwStyle[[n,8,1,2]],
											blendPts = dwBlends[$dwBlendResolution, "ReturnPoints"->True, "BlendObjects"->{n, $dwStyle[[n,8,1,2]]}, "Quantity"->$dwStyle[[n,8,1,3]], "Radiate"->$dwStyle[[n,8,1,4]]];
											dash = {$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]],
												$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]]};
											dash = Switch[dash,
												{{_,_},{}}, {dash[[1]], {0, dash[[1,2]]}},
												{{},{_,_}}, {{0, dash[[2,2]]}, dash[[2]]},
												_, dash];
												
											Join[
												{If[n < $dwStyle[[n,8,1,2]], Polygon[$dwP[[n]]], {}]},
												Table[Flatten[{
													If[n > $dwStyle[[n,8,1,2]],
														{
															Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
															Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
															EdgeForm[{
																Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
															}]
														},{
															Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
															Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
															EdgeForm[{
																Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																AbsoluteThickness[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																AbsoluteDashing[dash[[1]] + bpn(dash[[2]]-dash[[1]])/(Length[blendPts]+1)]
															}]
														}
													],
													Polygon[blendPts[[bpn]]]
												}], {bpn, Length[blendPts]}],
												{If[n > $dwStyle[[n,8,1,2]], Polygon[$dwP[[n]]], {}]}
											],
											Polygon[$dwP[[n]]]
										],
									_,
										Polygon[$dwP[[n]], VertexTextureCoordinates->(Partition[Riffle[Rescale[Table[First@p,{p,$dwP[[n]]}]],Rescale[Table[Last@p,{p,$dwP[[n]]}]]],2]/.{0.->.01, 1.->.99})]
								],
								Polygon[$dwP[[n]]]
							],
						Arrow|Line,
							If[$dwHead[[n]] === Arrow || $dwStyle[[n,2]],
								Switch[$dwLineGradients[[n,1]],
									"2ColorLineGradient",
										Arrow[Line[$dwP[[n]], VertexColors->Table[Blend[Table[$dwLineGradients[[n,2,gn,2]], {gn, {1,4}}], (n2-1)/(Length[$dwP[[n]]]-1)], {n2, Length[$dwP[[n]]]}]]],
									"3ColorLineGradient",
										Arrow[Line[$dwP[[n]], VertexColors->Table[Blend[Table[$dwLineGradients[[n,2,gn,2]], {gn, {1,2,4}}], (n2-1)/(Length[$dwP[[n]]]-1)], {n2, Length[$dwP[[n]]]}]]],
									"4ColorLineGradient",
										Arrow[Line[$dwP[[n]], VertexColors->Table[Blend[Table[$dwLineGradients[[n,2,gn,2]], {gn, 4}], (n2-1)/(Length[$dwP[[n]]]-1)], {n2, Length[$dwP[[n]]]}]]],
									_,
										Arrow[$dwP[[n]]]
								],
								If[MemberQ[Flatten[$dwCompoundPathLayers], n],
									AppendTo[compoundPath[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]][[1]], Line[$dwP[[n]]]];,
									Switch[$dwLineGradients[[n,1]],
										"2ColorLineGradient",
											Line[$dwP[[n]], VertexColors->Table[Blend[Table[$dwLineGradients[[n,2,gn,2]], {gn, {1,4}}], (n2-1)/(Length[$dwP[[n]]]-1)], {n2, Length[$dwP[[n]]]}]],
										"3ColorLineGradient",
											Line[$dwP[[n]], VertexColors->Table[Blend[Table[$dwLineGradients[[n,2,gn,2]], {gn, {1,2,4}}], (n2-1)/(Length[$dwP[[n]]]-1)], {n2, Length[$dwP[[n]]]}]],
										"4ColorLineGradient",
											Line[$dwP[[n]], VertexColors->Table[Blend[Table[$dwLineGradients[[n,2,gn,2]], {gn, 4}], (n2-1)/(Length[$dwP[[n]]]-1)], {n2, Length[$dwP[[n]]]}]],
										_,
											Line[$dwP[[n]]]
									]
								]
							],
						BezierCurve,
							Which[
								!$dwStyle[[n,1]] && $dwStyle[[n,2]],
									Arrow[BezierCurve[Most[$dwP[[n]]]]],
								!$dwStyle[[n,1]],
									(* not filled *)
									If[MemberQ[Flatten[$dwCompoundPathLayers], n],
										(* compound path *)
										AppendTo[compoundPath[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]][[1]], 
											If[compoundPath[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]][[1]] === {},
												(* first path *)
												BezierCurve[If[Length[$dwP[[n]]] > 2, Most[$dwP[[n]]], $dwP[[n]]]],
												(* not first path *)
												BezierCurve[If[Length[$dwP[[n]]] > 2, Join[Table[$dwP[[n, 1]], 2], Most[$dwP[[n]]]], $dwP[[n]]]]
											]
										];,
										(* not compound path *)
										Switch[$dwStyle[[n,8,1]],
											{"BlendGradient", _, _, _},
												If[n != $dwStyle[[n,8,1,2]],
													blendPts = dwBlends[$dwBlendResolution, "ReturnPoints"->True, "BlendObjects"->{n, $dwStyle[[n,8,1,2]]}, "Quantity"->$dwStyle[[n,8,1,3]], "Radiate"->$dwStyle[[n,8,1,4]]];
													dash = {$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]],
														$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]]};
													dash = Switch[dash,
														{{_,_},{}}, {dash[[1]],{dash[[1,1]],0}},
														{{},{_,_}}, {{dash[[2,1]],0},dash[[2]]},
														_, dash];
										
													Join[
														{If[n < $dwStyle[[n,8,1,2]], BezierCurve[Most[$dwP[[n]]]], {}]},
														Table[Flatten[{
															If[n > $dwStyle[[n,8,1,2]],
																{
																	Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																	Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																	(*StrokeForm[*){
																		Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																		Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																		AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																		AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
																	}(*]*)
																},{
																	Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																	Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																	(*StrokeForm[*){
																		Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																		Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																		AbsoluteThickness[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																		AbsoluteDashing[dash[[1]] + bpn(dash[[2]]-dash[[1]])/(Length[blendPts]+1)]
																	}(*]*)
																}
															],
															BezierCurve[Most[blendPts[[bpn]]]]
														}], {bpn, Length[blendPts]}],
														{If[n > $dwStyle[[n,8,1,2]], BezierCurve[Most[$dwP[[n]]]], {}]}
													],
													BezierCurve[Most[$dwP[[n]]]]
												],
											_,
												BezierCurve[If[Length[$dwP[[n]]] > 2, Most[$dwP[[n]]], $dwP[[n]]]]
										]
									],
								True,
									(* filled *)
									If[Length[$dwP[[n]]] > 2 && ($dwStyle[[n,8,1]] =!= None || (Flatten[Position[$dwCompoundPathLayers, n]] =!= {} && $dwStyle[[$dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, n]][[1]], 1]],8,1]] =!= None)),
										(* gradient *)
										If[MemberQ[Flatten[$dwCompoundPathLayers], n],
											(* compound path *)
											AppendTo[compoundPath[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]][[1]], 
												If[compoundPath[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]][[1]] === {},
													(* first path *)
													BezierCurve[Most[$dwP[[n]]]],
													(* not first path *)
													BezierCurve[Join[Table[$dwP[[n, 1]], 3], Most[$dwP[[n]]]]]
												]
											];,
											(* not compound path *)
											If[$dwStyle[[n,8,1]] =!= None,
												Switch[$dwStyle[[n,8,1]],
													{"BlendGradient", _, _, _},
														If[n != $dwStyle[[n,8,1,2]],
															blendPts = dwBlends[$dwBlendResolution, "ReturnPoints"->True, "BlendObjects"->{n, $dwStyle[[n,8,1,2]]}, "Quantity"->$dwStyle[[n,8,1,3]], "Radiate"->$dwStyle[[n,8,1,4]]];
															dash = {$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]],
																$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]]};
															dash = Switch[dash,
																{{_,_},{}}, {dash[[1]],{dash[[1,1]],0}},
																{{},{_,_}}, {{dash[[2,1]],0},dash[[2]]},
																_, dash];
												
															Join[
																{If[n < $dwStyle[[n,8,1,2]], FilledCurve[{BezierCurve[Most[$dwP[[n]]]]}], {}]},
																Table[Flatten[{
																	If[n > $dwStyle[[n,8,1,2]],
																		{
																			Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																			Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																			EdgeForm[{
																				Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																				AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
																			}]
																		},{
																			Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																			Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																			EdgeForm[{
																				Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				AbsoluteThickness[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																				AbsoluteDashing[dash[[1]] + bpn(dash[[2]]-dash[[1]])/(Length[blendPts]+1)]
																			}]
																		}
																	],
																	FilledCurve[{BezierCurve[Most[blendPts[[bpn]]]]}]
																}], {bpn, Length[blendPts]}],
																{If[n > $dwStyle[[n,8,1,2]], FilledCurve[{BezierCurve[Most[$dwP[[n]]]]}], {}]}
															],
															FilledCurve[{BezierCurve[Most[$dwP[[n]]]]}]
														],
													_,
														FilledCurve[{BezierCurve[Most[$dwP[[n]]]]}, VertexTextureCoordinates->(Partition[Riffle[Rescale[Table[First@p,{p,Most[$dwP[[n]]]}]],Rescale[Table[Last@p,{p,Most[$dwP[[n]]]}]]],2]/.{0.->.01, 1.->.99})]
												],
												FilledCurve[{BezierCurve[Most[$dwP[[n]]]]}, VertexTextureCoordinates->(Partition[Riffle[Rescale[Table[First@p,{p,Most[$dwP[[n]]]}]],Rescale[Table[Last@p,{p,Most[$dwP[[n]]]}]]],2]/.{0.->.01, 1.->.99})]
											]
										],
										(* no gradient *)
										If[MemberQ[Flatten[$dwCompoundPathLayers], n],
											(* compound path *)
											AppendTo[compoundPath[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]][[1]], 
												If[compoundPath[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]][[1]] === {},
													(* first path *)
													BezierCurve[If[Length[$dwP[[n]]] > 2, Most[$dwP[[n]]], $dwP[[n]]]],
													(* not first path *)
													BezierCurve[If[Length[$dwP[[n]]] > 2, Join[Table[$dwP[[n, 1]], 2], Most[$dwP[[n]]]], $dwP[[n]]]]
												]
											];,
											(* not compound path *)
											FilledCurve[BezierCurve[If[Length[$dwP[[n]]] > 2, Most[$dwP[[n]]], $dwP[[n]]]]]
										]
									]
							], 
						BSplineCurve, 
							If[Length[$dwP[[n]]] > 1,
								Which[
									!$dwStyle[[n,1]] && $dwStyle[[n,2]],
										Arrow[BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]],
									!$dwStyle[[n,1]],
										If[MemberQ[Flatten[$dwCompoundPathLayers], n],
											AppendTo[compoundPath[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]][[1]], BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]];,
											
											Switch[$dwStyle[[n,8,1]],
												{"BlendGradient", _, _, _},
													If[n != $dwStyle[[n,8,1,2]],
														blendPts = dwBlends[$dwBlendResolution, "ReturnPoints"->True, "BlendObjects"->{n, $dwStyle[[n,8,1,2]]}, "Quantity"->$dwStyle[[n,8,1,3]], "Radiate"->$dwStyle[[n,8,1,4]]];
														dash = {$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]],
															$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]]};
														dash = Switch[dash,
															{{_,_},{}}, {dash[[1]],{dash[[1,1]],0}},
															{{},{_,_}}, {{dash[[2,1]],0},dash[[2]]},
															_, dash];
														
														Join[
															{If[n < $dwStyle[[n,8,1,2]], BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]], {}]},
															Table[Flatten[{
																If[n > $dwStyle[[n,8,1,2]],
																	{
																		Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																		Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																		(*StrokeForm[*){
																			Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																			Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																			AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																			AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
																		}(*]*)
																	},{
																		Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																		Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																		(*StrokeForm[*){
																			Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																			Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																			AbsoluteThickness[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																			AbsoluteDashing[dash[[1]] + bpn(dash[[2]]-dash[[1]])/(Length[blendPts]+1)]
																		}(*]*)
																	}
																],
																BSplineCurve[blendPts[[bpn]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]
															}], {bpn, Length[blendPts]}],
															{If[n > $dwStyle[[n,8,1,2]], BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]], {}]}
														],
														BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]
													],
												_,
													BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]
											]
										],
									True,
										If[MemberQ[Flatten[$dwCompoundPathLayers], n],
											AppendTo[compoundPath[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]][[1]], BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]];,
											
											If[$dwStyle[[n,8,1]] =!= None,
												Switch[$dwStyle[[n,8,1]],
													{"BlendGradient", _, _, _},
														If[n != $dwStyle[[n,8,1,2]],
															blendPts = dwBlends[$dwBlendResolution, "ReturnPoints"->True, "BlendObjects"->{n, $dwStyle[[n,8,1,2]]}, "Quantity"->$dwStyle[[n,8,1,3]], "Radiate"->$dwStyle[[n,8,1,4]]];
															dash = {$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]],
																$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]]};
															dash = Switch[dash,
																{{_,_},{}}, {dash[[1]],{dash[[1,1]],0}},
																{{},{_,_}}, {{dash[[2,1]],0},dash[[2]]},
																_, dash];
															
															Join[
																{If[n < $dwStyle[[n,8,1,2]], FilledCurve[BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]], {}]},
																Table[Flatten[{
																	If[n > $dwStyle[[n,8,1,2]],
																		{
																			Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																			Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																			EdgeForm[{
																				Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																				AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
																			}]
																		},{
																			Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																			Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																			EdgeForm[{
																				Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],bpn/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				AbsoluteThickness[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																				AbsoluteDashing[dash[[1]] + bpn(dash[[2]]-dash[[1]])/(Length[blendPts]+1)]
																			}]
																		}
																	],
																	FilledCurve[BSplineCurve[blendPts[[bpn]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]]
																}], {bpn, Length[blendPts]}],
																{If[n > $dwStyle[[n,8,1,2]], FilledCurve[BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]], {}]}
															],
															FilledCurve[BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]]
														],
													_,
														FilledCurve[BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]]
												],
												FilledCurve[BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]]
											]
										]
								],
								Nothing
							], 
						_,
							$dwHead[[n]][$dwP[[n]]]
					]
				}, {}], 
		{n, Length[$dwP]}];
		(* insert compound paths *)
		If[allLayers =!= {} && $dwCompoundPathLayers =!= {},
			If[Length[layerNumbers] == Length[$dwP],
				
				(* all layers *)
				Do[
					allLayers = Insert[allLayers, {
			 			If[!CurrentValue[$dwCommandKey], $dwObjectGradients[[$dwCompoundPathLayers[[n,1]]]], Nothing],
			 			Sequence@@$dwStyle[[$dwCompoundPathLayers[[n,1]]]][[$dwStyleStart;;-1]], 
				 		If[!CurrentValue[$dwCommandKey] && MemberQ[FreeQ[{None},$dwStyle[[#,8,1]]]&/@$dwCompoundPathLayers[[n]], True],
				 			
				 			(* texture *)
				 			minmax = {
				 				MinMax[Flatten[Table[#[[1]]&/@$dwP[[cpn]], {cpn, $dwCompoundPathLayers[[n]]}]]],
				 				MinMax[Flatten[Table[#[[2]]&/@$dwP[[cpn]], {cpn, $dwCompoundPathLayers[[n]]}]]]
				 			};
				 			Append[List/@#&/@compoundPath[[n]], VertexTextureCoordinates->Table[
				 				If[$dwHead[[cpn]] === BezierCurve,
				 					If[cpn == $dwCompoundPathLayers[[n,1]],
				 						Most[Partition[Riffle[Rescale[Table[First@p,{p, $dwP[[cpn]]}], minmax[[1]]], Rescale[Table[Last@p,{p, $dwP[[cpn]]}], minmax[[2]]]], 2]],
				 						Partition[Riffle[Rescale[Table[First@p,{p, Join[Table[$dwP[[cpn, 1]], 3], Most[$dwP[[cpn]]]]}], minmax[[1]]], Rescale[Table[Last@p,{p, Join[Table[$dwP[[cpn, 1]], 3], Most[$dwP[[cpn]]]]}], minmax[[2]]]], 2]
				 					],
				 					Partition[Riffle[Rescale[Table[First@p,{p, $dwP[[cpn]]}], minmax[[1]]], Rescale[Table[Last@p,{p, $dwP[[cpn]]}], minmax[[2]]]], 2]
				 				],
				 				{cpn, $dwCompoundPathLayers[[n]]}]
				 			],
				 			
				 			(* no texture *)
				 			compoundPath[[n]]
				 		]
				 	}, $dwCompoundPathLayers[[n]][[-1]]],
				{n, Length[$dwCompoundPathLayers]}],
				
				(* given layer numbers *)
				Do[
			 		allLayers = Insert[allLayers, 
			 			If[Intersection[layerNumbers, $dwCompoundPathLayers[[n]]] =!= {},
			 				{Sequence@@$dwStyle[[$dwCompoundPathLayers[[n]][[1]]]][[$dwStyleStart;;-1]], compoundPath[[n]]},
			 				{}
			 			], $dwCompoundPathLayers[[n]][[-1]]],
				{n, Length[$dwCompoundPathLayers]}]
			],
			Nothing
		];
		
		(* style removal by default settings - do not remove default faceform and strokeform *)
		(* AbsoluteDashing[___] removal causes issues with end of path cap if BezierCurve end corner points are not exactly the same *)
		(*allLayers = allLayers/.{AbsoluteDashing[{}]->Sequence[]};*)
		
		(* style removal by shape type *)
		allLayers = 
			Table[
				Which[
					Cases[layer, _Line] =!= {},
						Replace[layer, {
							FaceForm[___]|Arrowheads[___]|AbsoluteDashing[{}]|AbsolutePointSize[___]->Sequence[],
							StrokeForm[sf_]:>Sequence@@sf
						}, 2],
					Cases[layer, _BSplineCurve|_BezierCurve] =!= {},
						Replace[layer, {
							FaceForm[___]|Arrowheads[___]|AbsolutePointSize[___]->Sequence[]
							(*StrokeForm[sf_]:>Sequence@@sf*)
						}, 2],
					Cases[layer, _Arrow] =!= {},
						Replace[layer, {
							FaceForm[___]|AbsoluteDashing[{}]|AbsolutePointSize[___]->Sequence[],
							StrokeForm[sf_]:>Sequence@@sf
						}, 2],
					Cases[layer, _Point] =!= {},
						Replace[layer, FaceForm[___]|StrokeForm[___]|Arrowheads[___]|AbsoluteDashing[___]|CapForm[___]|JoinForm[___]->Sequence[], 2],
					Cases[layer, _Polygon] =!= {},
						Replace[layer, {
							Arrowheads[___]|AbsolutePointSize[___]|AbsoluteDashing[{}]|AbsoluteThickness[___]|CapForm[___]|JoinForm[___]->Sequence[],
							StrokeForm->EdgeForm
						}, 2, Heads->True],
					Cases[layer, _FilledCurve] =!= {},
						Replace[layer, {
							Arrowheads[___]|AbsolutePointSize[___]|CapForm[___]|JoinForm[___]|AbsoluteThickness[___]->Sequence[],
							StrokeForm->EdgeForm
						}, 2, Heads->True],
					True,
						layer
				], 
			{layer, allLayers}];
		
		(* combine objects with identical styles *)
		deleteLayers = {};
		If[Length[allLayers] > 1,
			combinedlayers = 2;
			Do[
				If[(Head[allLayers[[ln]]] === List && Head[allLayers[[ln - 1]]] === List),
					If[(Length[allLayers[[ln]]] >= combinedlayers && Length[allLayers[[ln - 1]]] >= 2),
						If[allLayers[[ln, 1;;-combinedlayers]] === allLayers[[ln - 1, 1;;-2]],
							allLayers[[ln - 1]] = Join[allLayers[[ln - 1]], allLayers[[ln, -(combinedlayers-1);;-1]]];
							AppendTo[deleteLayers, ln];
							++combinedlayers,
							combinedlayers = 2
						],
						combinedlayers = 2
					],
					combinedlayers = 2
				], 
			{ln, Length[allLayers], 2, -1}]
		];
		
		allLayers = Delete[allLayers, List/@deleteLayers];
		allLayers = allLayers/.{{}->Sequence[]};
		allLayers = allLayers/.{AbsoluteDashing[]->AbsoluteDashing[{}]};
			
		(* final graphics with default settings added *)
		Graphics[allLayers,
			Sequence@@If[CurrentValue[$dwCommandKey] || CurrentValue["ShiftKey"], {},
				{
					PlotRange->If[OptionValue["PlotRange"] === Automatic, {{Min[#[[1]]&/@$dwPlotRange],Max[#[[1]]&/@$dwPlotRange]},{Min[#[[2]]&/@$dwPlotRange],Max[#[[2]]&/@$dwPlotRange]}}, OptionValue["PlotRange"]],
					ImageSize->If[OptionValue["ImageSize"] === Automatic, ($dwOverallSize/(2$dwCanvasMouseSpeed)){Max[#[[1]]&/@$dwPlotRange]-Min[#[[1]]&/@$dwPlotRange],Max[#[[2]]&/@$dwPlotRange]-Min[#[[2]]&/@$dwPlotRange]}, OptionValue["ImageSize"]]
				}
			]
		]
	]

End[] (* End Private Context *)

EndPackage[]