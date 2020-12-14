(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderNotAddingOrMovingPoints[]:=
	Block[{allLayers, compoundPath = Table[FilledCurve[{}], Length[$dwCompoundPathLayers]], minmax, pts, blendPts, dash},
		allLayers = Table[
			If[!MemberQ[$dwHideLayers, n] && $dwP[[n]] =!= {},
				Annotation[
					{
						If[($dwHead[[n]] === Polygon && $dwStyle[[n,8,1]] =!= None) || ($dwHead[[n]] === BezierCurve && (Length[$dwP[[n]]] > 2 && $dwStyle[[n,1]])),
							$dwObjectGradients[[n]],
							Nothing
						],
						If[FreeQ[{Text, Image, "Expression", "Text3D"}, $dwHead[[n]]](* && FreeQ[Flatten[$dwCompoundPathLayers], n]*),
							Sequence@@$dwStyle[[n]][[$dwStyleStart;;-1]],
							Nothing
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
											Polygon[pts, VertexColors->Table[Opacity[Rescale[If[n1<=Length[pts]/2,(n1-1),(Length[pts]-n1)],{0,1}],Blend[$dwStyle[[n,8,2]],If[n1<=Length[pts]/2,n1/(Length[pts]/2),((Length[pts])-(n1-1))/(Length[pts]/2)]]],{n1,Length[pts]}]],
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
																Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																StrokeForm[{
																	Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																	Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																	AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																	AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
																}]
															},{
																Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																StrokeForm[{
																	Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
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
																		StrokeForm[{
																			Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																			Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																			AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																			AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
																		}]
																	},{
																		Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																		Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																		StrokeForm[{
																			Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																			Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																			AbsoluteThickness[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																			AbsoluteDashing[dash[[1]] + bpn(dash[[2]]-dash[[1]])/(Length[blendPts]+1)]
																		}]
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
																				Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				StrokeForm[{
																					Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																					Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																					AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																					AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
																				}]
																			},{
																				Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				StrokeForm[{
																					Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
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
								If[Length[$dwP[[n]]] > 2,
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
																			StrokeForm[{
																				Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																				AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
																			}]
																		},{
																			Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																			Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																			StrokeForm[{
																				Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				AbsoluteThickness[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																				AbsoluteDashing[dash[[1]] + bpn(dash[[2]]-dash[[1]])/(Length[blendPts]+1)]
																			}]
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
																				Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				StrokeForm[{
																					Blend[ColorConvert[{$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																					Opacity[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																					AbsoluteThickness[$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]] + bpn($dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]]-$dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,3,1]])/(Length[blendPts]+1)],
																					AbsoluteDashing[dash[[2]] + bpn(dash[[1]]-dash[[2]])/(Length[blendPts]+1)]
																				}]
																			},{
																				Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
																				Opacity[$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]] + bpn($dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]-$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]])/(Length[blendPts]+1)],
																				StrokeForm[{
																					Blend[ColorConvert[{$dwStyle[[n,Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], $dwStyle[[$dwStyle[[n,8,1,2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]},"LAB"],(bpn)/(Length[blendPts]+1)],
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
									Switch[Length[$dwP[[n]]],
										1, Point[$dwP[[n]]],
										2, Line[$dwP[[n]]],
										_, {}
									]
								], 
							_,
								$dwHead[[n]][$dwP[[n]]]
						]
					}, n, "Mouse"]
			], 
		{n, Length[$dwP]}];
		(* insert compound paths *)
		If[allLayers =!= {} && $dwCompoundPathLayers =!= {},
			Do[
			 	allLayers = Insert[allLayers, 
			 		Annotation[{
						$dwObjectGradients[[$dwCompoundPathLayers[[n,1]]]],
			 			Sequence@@$dwStyle[[$dwCompoundPathLayers[[n,1]]]][[$dwStyleStart;;-1]], 
				 		If[MemberQ[FreeQ[{None},$dwStyle[[#,8,1]]]&/@$dwCompoundPathLayers[[n]], True],
				 			
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
				 	}, $dwCompoundPathLayers[[n,1]], "Mouse"], $dwCompoundPathLayers[[n,-1]]],
			{n, Length[$dwCompoundPathLayers]}],
			Nothing
		];
		Flatten[allLayers]
	]

End[] (* End Private Context *)

EndPackage[]