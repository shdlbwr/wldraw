(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeShadow[]:=
	Block[{},
		If[$dwSelected =!= {},
			dwConvertPointSelectionToLayerSelection[];
			(*If[CurrentValue[$dwCommandKey],*)
				
				(* dialog *)
				CreateDialog[
					DynamicModule[{temp = {}, plotRange, preview = {}, selectOrdered = Sort[$dwSelected],
						p1, angle, p2, p3, ctr, rot, rotOpp, final, nearestDistance = 10, vectorShadow = False},
						
						$dwShapeStylePreviewOrigin = $dwShapeStylePreviewOriginStart = {0,0};
						$dwShapeStylePreviewDimensionsMagnify = $dwShapeStylePreviewDimensionsMagnifyStart = .75;
				
						Pane[Column[{
						plotRange = 1;(* change plot range of preview *)
						EventHandler[
							MouseAppearance[
								Dynamic[Magnify[
									Graphics[{
										
										(* axes and box *)
										{LightGray, InfiniteLine[{{0,0},{1,0}}], InfiniteLine[{{0,0},{0,1}}], Line[{{-1,-1},{1,-1},{1,1},{-1,1},{-1,-1}}]},
									
										(* get objects *)
										preview = dwRenderNotAddingOrMovingPoints[];
										
										(* get shadows *)
										temp = 
											Table[
												If[vectorShadow && FreeQ[Table[MemberQ[{Polygon, BezierCurve, BSplineCurve}, $dwHead[[s]]], {s, selectOrdered}], False],
													
													(* vector shadow *)
													p1 = dwDecreasePointsByImage[$dwP[[n]], $dwHead[[n]], n, "ReturnPoints"->True, "FillHoles"->0];
													p1 = If[Depth[p1] > 3, p1[[Sequence@@Table[1, Depth[p1] - 3]]], p1];
													p2 = $dwImageShadowOffset + # & /@ p1;
													angle = -ArcTan[ $dwImageShadowOffset[[1]]/($dwImageShadowOffset[[2]] + $MachineEpsilon)] + Pi/2;
													ctr = dwFindCenter[#] & /@ {p1, p2};
													rot = RotationTransform[angle, #] & /@ ctr;
													rotOpp = RotationTransform[angle + Pi, #] & /@ ctr;
													p3 = Flatten[{
													    RegionNearest[Polygon[p1], rot[[1]][{ctr[[1]] + {0, nearestDistance}}]],
													    RegionNearest[Polygon[p2], rot[[2]][{ctr[[2]] + {0, nearestDistance}}]],
													    RegionNearest[Polygon[p2], rotOpp[[2]][{ctr[[2]] + {0, nearestDistance}}]],
													    RegionNearest[Polygon[p1], rotOpp[[1]][{ctr[[1]] + {0, nearestDistance}}]]
													    }, 1];
													final = BooleanRegion[Or, Polygon[#] & /@ {p2, p3}];
													final = If[MemberQ[{BooleanRegion}, Head[final]], Polygon[{{0,0},{0,0}}], final];
													final/.{Or|BooleanRegion|EmptyRegion->Polygon[{{0,0},{0,0}}]};
													Graphics[{	
															
															(* shadow style *)
															Opacity[$dwImageShadowOpacity], $dwImageShadowColor,
																
															(* shadow object *)
															Switch[Head[final], 
																Region | BoundaryMeshRegion | MeshRegion,
																	Polygon[{Flatten[Join[#[[1]] & /@ MeshPrimitives[final, 1]], 1]}], 
																_, 
																	final
															]
														},
														Background->None, ImagePadding->0, PlotRange->plotRange, PlotRangePadding->0
													],
													
													(* image shadow *)
													Blur[
														Erosion[
															Rasterize[
																Graphics[
																			
																	Flatten@{
																		
																		(* shadow style *)
																		Switch[$dwHead[[n]],
																			$dwShapeSymbolsAlternates,
																				{
																					ReplacePart[$dwStyle[[n]], 
																						{
																							{Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]], 1, 1}->Black, 
																							{Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]], 1, 2, 1}->1, 
																							{Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]], 1, 1}->Black, 
																							{Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]], 1, 2, 1}->1
																						}][[$dwStyleStart;;-1]]
																				},
																			_,
																				{}
																		], 
																		
																		(* shadow object *)
																		Switch[$dwHead[[n]],
																			BezierCurve,
																				If[$dwStyle[[n,1]],
																					FilledCurve[BezierCurve[Most[$dwP[[n]]]]],
																					$dwHead[[n]][Most[$dwP[[n]]]]
																				],
																			BSplineCurve,
																				If[$dwStyle[[n,1]],
																					FilledCurve[BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]],
																					BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]
																				],
																			Image,
																				Inset[Rotate[Show[Darker[$dwStyle[[n,2]], 1], ImageSize->($dwStyle[[n,3]]*ImageDimensions[$dwStyle[[n,2]]])], $dwStyle[[n,1]]], $dwP[[n,1]]],
																			Text,
																				If[$dwStyle[[n,12]]=!=None,
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
																					Rotate[Text[Style[Sequence@@($dwStyle[[n]][[Join[Range[2,11], {15}]]]), Black], $dwP[[n,1]], $dwStyle[[n,3]]], $dwStyle[[n,1]], $dwP[[n,1]]]
																				],
																			_,
																				$dwHead[[n]][$dwP[[n]]]
																		]},
																		Background->White, ImagePadding->0, PlotRange->plotRange, PlotRangePadding->0
																		(*ImageSize->(($dwShapeStylePreviewDimensions/$dwShapeStylePreviewSize)[[1]]*$dwShapeStyleMouseSpeed)*($dwShapeStyleMouseSpeed*$dwOverallSize)*)
																	], 
																	ImageResolution->$dwImageResolution
																], 
															$dwImageShadowSpread
														], 
														$dwImageShadowBlur
													]
												], 
											{n, selectOrdered}];
										(* combine objects and shadows *)
										Do[
											preview = 
												If[vectorShadow && FreeQ[Table[MemberQ[{Polygon, BezierCurve, BSplineCurve}, $dwHead[[s]]], {s, selectOrdered}], False],
													Insert[preview, Inset[temp[[n]], ctr[[1]], ctr[[1]], 2plotRange], selectOrdered[[n]]],
													Insert[preview, Inset[SetAlphaChannel[ImageAdd[temp[[n]], $dwImageShadowColor], Darker[ColorNegate[temp[[n]]], 1-$dwImageShadowOpacity]], $dwImageShadowOffset, Automatic, 2plotRange], selectOrdered[[n]]]
												],
										{n, Reverse@Range[Length[selectOrdered]]}];
										
										(* display *)
										preview
										
									}, Background->White, ImageSize->Full, PlotRange->(.75/$dwShapeStylePreviewDimensionsMagnify)*((({{-$dwShapeStyleMouseSpeed,$dwShapeStyleMouseSpeed},{-$dwShapeStyleMouseSpeed,$dwShapeStyleMouseSpeed}}*$dwShapeStylePreviewDimensions)/($dwShapeStylePreviewSize))-$dwShapeStylePreviewOrigin)], 
								$dwShapeStylePreviewDimensionsMagnify], TrackedSymbols :> {vectorShadow, $dwImageShadowColor, $dwImageShadowOpacity, $dwImageShadowBlur, $dwImageShadowOffset, $dwImageShadowSpread, $dwShapeStylePreviewDimensionsMagnify, $dwShapeStyleMouseSpeed, $dwShapeStylePreviewDimensions, $dwShapeStylePreviewSize, $dwShapeStylePreviewOrigin}, SynchronousUpdating->False],
		
							Dynamic@If[CurrentValue[$dwCommandKey] && CurrentValue[$dwOptionKey], $dwIconZoomDrag, $dwCursorCanvas],
							Scaled[{.5,.5}]
						],
							{
								"MouseDown" :> (If[Head[MousePosition["GraphicsScaled"]] === List, $dwClickPtScaled = MousePosition["GraphicsScaled"]]),
								"MouseDragged" :> (
										If[Head[MousePosition["GraphicsScaled"]] === List,
											If[CurrentValue[$dwCommandKey] && CurrentValue[$dwOptionKey],
												$dwShapeStylePreviewDimensionsMagnify = Min[8, Max[.1, $dwShapeStylePreviewDimensionsMagnifyStart + (2*($dwShapeStylePreviewDimensions/$dwShapeStylePreviewSize)(MousePosition["GraphicsScaled"] - $dwClickPtScaled))[[2]]]],
												$dwShapeStylePreviewOrigin = $dwShapeStylePreviewOriginStart + ($dwShapeStylePreviewDimensions/$dwShapeStylePreviewSize)(MousePosition["GraphicsScaled"] - $dwClickPtScaled)
											]
										]),
								"MouseClicked" :> (If[CurrentValue[$dwCommandKey] && !CurrentValue[$dwOptionKey],
										$dwShapeStylePreviewOrigin = $dwShapeStylePreviewOriginStart = {0,0};
										$dwShapeStylePreviewDimensionsMagnify = $dwShapeStylePreviewDimensionsMagnifyStart = .75
									]),
								"MouseUp" :> (
										$dwShapeStylePreviewOriginStart = $dwShapeStylePreviewOrigin;
										$dwShapeStylePreviewDimensionsMagnifyStart = $dwShapeStylePreviewDimensionsMagnify
									)
							}
						],
						
						"",
						Row[{
							ColorSetter[Dynamic@$dwImageShadowColor],
							Spacer[20], Checkbox[Dynamic@vectorShadow]," polygon shadow"
							}],
						Grid[{
							{"opacity", Slider[Dynamic@$dwImageShadowOpacity, {0, 1, .025}, ContinuousAction -> False],
								Row[{ 
									Button["<", $dwImageShadowOpacity = Max[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowOpacity - .025], 0], Appearance->"Palette"],
									Button[">", $dwImageShadowOpacity = Min[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowOpacity + .025], 1], Appearance->"Palette"]
								}],
								Dynamic@$dwImageShadowOpacity},
							{"horizontal", Slider[Dynamic@$dwImageShadowOffset[[1]],{-1, 1, .025}, ContinuousAction -> False], 
								Row[{ 
									Button["<", $dwImageShadowOffset[[1]] = Max[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowOffset[[1]] - .025], -1], Appearance->"Palette"],
									Button[">", $dwImageShadowOffset[[1]] = Min[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowOffset[[1]] + .025], 1], Appearance->"Palette"]
								}],
								Dynamic@$dwImageShadowOffset[[1]]},
							{"vertical", Slider[Dynamic@$dwImageShadowOffset[[2]],{-1, 1, .025}, ContinuousAction -> False], 
								Row[{ 
									Button["<", $dwImageShadowOffset[[2]] = Max[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowOffset[[2]] - .025], -1], Appearance->"Palette"],
									Button[">", $dwImageShadowOffset[[2]] = Min[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowOffset[[2]] + .025], 1], Appearance->"Palette"]
								}],
								Dynamic@$dwImageShadowOffset[[2]]},
							{Null,"BITMAP SHADOW", SpanFromLeft, SpanFromLeft},
							{"blur", Slider[Dynamic@$dwImageShadowBlur, {0, 10, .025}, ContinuousAction -> False], 
								Row[{ 
									Button["<", $dwImageShadowBlur = Max[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowBlur - .025], 0], Appearance->"Palette"],
									Button[">", $dwImageShadowBlur = Min[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowBlur + .025], 10], Appearance->"Palette"]
								}],
								Dynamic@$dwImageShadowBlur},
							{"spread", Slider[Dynamic@$dwImageShadowSpread,{0, 5, .025}, ContinuousAction -> False], 
								Row[{ 
									Button["<", $dwImageShadowSpread = Max[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowSpread - .025], 0], Appearance->"Palette"],
									Button[">", $dwImageShadowSpread = Min[If[CurrentValue[$dwCommandKey], 0, $dwImageShadowSpread + .025], 5], Appearance->"Palette"]
								}],
								Dynamic@$dwImageShadowSpread}
						}, Alignment->{{Right, Left, Left, Left, Left}}],
						Row@{
							Button["reset", $dwImageShadowColor = Black; $dwImageShadowOpacity = 0.4; $dwImageShadowBlur = 6; $dwImageShadowOffset = {0.025, -0.025}; $dwImageShadowSpread = 0],
							Button["center", $dwImageShadowOffset = {0, 0}]
							},
						Row@{
							CancelButton[DialogReturn[]],
							DefaultButton[message = "Creating shadows..."; dwShapeShadowToCanvas[vectorShadow]; DialogReturn[]]
							}
					}, Alignment->Center],Alignment->Center,ImageSize->400]
				], Background->LightGray, WindowTitle->"Shadow",Modal->True](*,
				
				(* add shadow *)
				dwShapeShadowToCanvas[]
				
			]*),
			
			MessageDialog["Select object(s) to cast shadow."]
		];
	] 

dwShapeShadowToCanvas[vectorShadow_:True]:=
	Block[{saveOffset = $dwDupeOffset, saveSelected = Reverse@Sort[$dwSelected], length = Length[$dwP], temp,
		p1, angle, p2, p3, ctr, rot, rotOpp, final, nearestDistance = 10, finalSelection = {}},
		If[$dwSelected =!= {},
			
			dwSetUndo[];
			$dwDupeOffset = {0,0};
			dwDuplicateLayer["SetUndo"->False];
			
			If[vectorShadow && FreeQ[Table[MemberQ[{Polygon, BezierCurve, BSplineCurve}, $dwHead[[n]]], {n, Range[length+1, Length[$dwP]]}], False],
				
				(* set styles; add points *)
				Do[
					$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,1]] = Black;
					$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,2,1]] = 0;
					$dwStyle[[s,Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]],1,1]] = $dwImageShadowColor;
					$dwStyle[[s,Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]],1,2,1]] = $dwImageShadowOpacity;
					p1 = dwDecreasePointsByImage[$dwP[[s]], $dwHead[[s]], s, "ReturnPoints"->True, "LightHoles"->True, "FillHoles"->0][[1]];
					p2 = $dwImageShadowOffset + # & /@ p1;
					angle = -ArcTan[ $dwImageShadowOffset[[1]]/($dwImageShadowOffset[[2]] + $MachineEpsilon)] + Pi/2;
					ctr = dwFindCenter[#] & /@ {p1, p2};
					rot = RotationTransform[angle, #] & /@ ctr;
					rotOpp = RotationTransform[angle + Pi, #] & /@ ctr;
					p3 = Flatten[{
					    RegionNearest[Polygon[p1], rot[[1]][{ctr[[1]] + {0, nearestDistance}}]],
					    RegionNearest[Polygon[p2], rot[[2]][{ctr[[2]] + {0, nearestDistance}}]],
					    RegionNearest[Polygon[p2], rotOpp[[2]][{ctr[[2]] + {0, nearestDistance}}]],
					    RegionNearest[Polygon[p1], rotOpp[[1]][{ctr[[1]] + {0, nearestDistance}}]]
					    }, 1];
					final = BooleanRegion[Or, Polygon[#] & /@ {p2, p3}];
					final = final/.{Or|BooleanRegion|EmptyRegion->{{0,0}}};
					$dwHead[[s]] = Polygon;
					$dwP[[s]] = Switch[Head[final], 
						Region | BoundaryMeshRegion | MeshRegion,
							final = {Flatten[Join[#[[1]] & /@ MeshPrimitives[final, 1]], 1]};
							final = dwDecreasePointsByImage[If[Depth[final] > 3, final[[Sequence@@Table[1, Depth[final] - 3]]], final], Polygon, 0, "ReturnPoints"->True, "LightHoles"->True, "FillHoles"->0][[1]];
							If[final === Null, {{0,0}}, final], 
						_, 
							final[[1]]
					],
				{s, $dwSelected}],
			
				(* set styles; convert to image; add blur and opacity *)
				Do[
					Switch[$dwHead[[s]],
						$dwShapeSymbolsAlternates,
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,1]] = Black;
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,2,1]] = 1;
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]],1,1]] = Black;
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]],1,2,1]] = 1,
						_,
							Nothing
					],
				{s, $dwSelected}];
				dwCreateImageFromGraphics[White, "SeparateObjects"->True, "UseImageFilter"->False, "Pad"->0];
				Do[
					temp = Blur[Erosion[ImagePad[$dwStyle[[s,2]], If[$dwHead[[saveSelected[[s - length]]]] === Image, 15, 5], White], $dwImageShadowSpread], $dwImageShadowBlur];
					$dwStyle[[s,2]] = SetAlphaChannel[ImageAdd[Darker[temp, 1], $dwImageShadowColor], Darker[ColorNegate[temp], 1-$dwImageShadowOpacity]];
					$dwP[[s,1]] += $dwImageShadowOffset,
				{s, Range[length+1, Length[$dwP]]}]
			];
			
			(* update bounding boxes *)
			dwUpdateBoundingBox[Range[length+1, Length[$dwP]]];
			(* move layers *)
			If[vectorShadow && FreeQ[Table[MemberQ[{Polygon, BezierCurve, BSplineCurve}, $dwHead[[n]]], {n, Range[length+1, Length[$dwP]]}], False],
				
					Do[
						dwMoveLayers[{length+n}, Reverse[saveSelected][[n]] + (n - 1), "SetUndo"->False],
					{n, Length[saveSelected]}],
					
					Do[
						dwMoveLayers[{length+n}, saveSelected[[n]], "SetUndo"->False],
					{n, Length[saveSelected]}]
				];
			

			(* updates *)
			$dwDupeOffset = saveOffset,
			
			MessageDialog["Select object(s) to cast shadow."]
		];
	]

End[] (* End Private Context *)

EndPackage[]