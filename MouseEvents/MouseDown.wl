(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwMouseDown[]:=
	DynamicModule[{deletepos, deletePrecision = .015, mp, mps, msano, ma, angle, nearestPoint, pos, temp, temp2},
		dwSetUndo[];
		mp = MousePosition["Graphics"];
		mps = MousePosition["GraphicsScaled"];
		msano = MouseAnnotation[];
		If[{Head[mp], Head[mps]} === {List, List},
			If[FreeQ[{$dwClickPtNoGrid, $dwPreviousClickPtNoGrid, $dwPrevious2ClickPtNoGrid}, Null], $dwShowMouseClickPositions = False];
			$dwPrevious2ClickPtNoGrid = $dwPreviousClickPtNoGrid;
			$dwPreviousClickPtNoGrid = $dwClickPtNoGrid;
			$dwZoomStart = $dwZoomEnd = $dwDragSelectStart = $dwDragSelectEnd = $dwClickPtNoGrid = mp;
			$dwClickPt = If[$dwConstrainHAngle == 0,
							Round[$dwClickPtNoGrid, $dwGridStep],
							Nearest[
								($dwGridSize[[1]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwClickPtNoGrid,$dwClickPtNoGrid+$dwGridSize[[2]]}],InfiniteLine[{{0,0},$dwGridSize[[1]]}]][[1]]/($dwGridSize[[1]]+$MachineEpsilon)][[{1,1}]])
								+($dwGridSize[[2]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwClickPtNoGrid,$dwClickPtNoGrid+$dwGridSize[[1]]}],InfiniteLine[{{0,0},$dwGridSize[[2]]}]][[1]]/($dwGridSize[[2]]+$MachineEpsilon)][[{1,1}]])+#&/@$dwGridSnapMatrix, $dwClickPtNoGrid
							][[1]]
				];
			{$dwZoomStartValue, $dwStyleContentStart, $dwBoundingBoxesStart} = {$dwZoom, $dwStyle, $dwBoundingBoxes};
		];
		If[Head[mps] === List, $dwClickPtScaled = mps];
		{$dwPStart, $dwSelectionBoundaryStart, $dwModeBeforeAction, $dwMessageBarText} = {$dwP, $dwSelectionBoundary, $dwMode, ""};
		
		If[CurrentValue[$dwCommandKey],
			
			(* anytime drag move or zoom *)
			$dwMode = "anytimemove",
			
			Switch[$dwMode,
		
				"draw",
					If[$dwSelected =!= {}, 
						
						(* existing object *)
						If[$dwP[[$dwSelected[[1]]]] === {} && Length[msano] == 2,
							If[MemberQ[{1, Length[$dwP[[msano[[1]]]]]}, msano[[2]]],
								$dwSelected = {msano[[1]]}
							]
						];
						
						(* add points *)
						If[$dwHead[[$dwSelected[[1]]]] === BezierCurve,
							
							If[$dwDrawNewObject && Length[msano] == 2,
								
								If[msano[[2]] == 1,
									$dwP[[$dwSelected[[1]]]] = $dwPStart[[$dwSelected[[1]]]] = Join[Reverse[Most[$dwP[[$dwSelected[[1]]]]]], {$dwP[[$dwSelected[[1]], 1]] + ($dwP[[$dwSelected[[1]], 2]] - $dwP[[$dwSelected[[1]], 1]])}],
									Nothing
								],
								
								Nothing
							],
							
							If[Length@$dwP[[$dwSelected[[1]]]] > 1 && (!$dwDrawNewObject && Length[msano] == 2),
								
								If[msano[[2]] == 1,
									
									(* end point same as first point - create new layer *)
									If[$dwHead[[$dwSelected[[1]]]] === BSplineCurve,
										$dwStyle[[$dwSelected[[1]],10]] = True,
										AppendTo[$dwP[[$dwSelected[[1]]]], $dwClickPt]
									];
									dwNewEmptyLayer[],
									
									Nothing
								],
								
								(* draw *)
								Which[
									$dwP[[$dwSelected[[1]]]] === {},
										$dwP[[$dwSelected[[1]]]] = {$dwClickPt},
									Length[msano] == 2 && msano[[2]] == 1,
										$dwP[[$dwSelected[[1]]]] = $dwPStart[[$dwSelected[[1]]]] = Reverse[$dwP[[$dwSelected[[1]]]]],
									Length[msano] == 2 && msano[[2]] == Length[$dwP[[$dwSelected[[1]]]]],
										Nothing,
									True,
										AppendTo[$dwP[[$dwSelected[[1]]]], 
											(* constrain click *)
											If[CurrentValue["ShiftKey"] && Length[$dwP[[$dwSelected[[1]]]]] =!= {},
												(* angle constraint *)
												angle = ToPolarCoordinates[$dwClickPtNoGrid - $dwP[[$dwSelected[[1]],-1]]][[2]]180/Pi;
												ma = $dwClickPtNoGrid - $dwP[[$dwSelected[[1]],-1]];
												$dwP[[$dwSelected[[1]],-1]] +
													If[45 < Abs[angle +
															If[angle >= 0, -($dwConstrainHAngle),
																If[angle > ($dwConstrainHAngle) - 180, -($dwConstrainHAngle), 360 - ($dwConstrainHAngle)]
															]
														] < 135,
															{-(ma[[2]])*Tan[($dwConstrainVAngle + $MachineEpsilon) \[Degree]], ma[[2]]},
															{ma[[1]], (ma[[1]])*Tan[($dwConstrainHAngle + $MachineEpsilon) \[Degree]]}
													],
												(* no constraint *)
												$dwClickPt
											]
										]
								]
							]
						];
						$dwDrawNewObject = False;
						$dwPointModeSelections = {};
						(* update bounding box *)
						dwUpdateBoundingBox[$dwSelected[[{1}]]],
						
						Nothing
					],
					
				"plotrange",
					$dwSelectedPlotRange = {MouseAnnotation[]},
					
				"preview"|"wireframe",
					ma = MouseAnnotation[];
					If[ma =!= Null && !$dwShowMouseClickPositions,
						If[StringQ[ma],
							
							(* selection box *)
							dwSelectionBoxTransform[],
							
							(* object selection *)
							temp = 
								If[$dwGroupLayers =!= {} && !CurrentValue[$dwOptionKey], 
									Union[Flatten[Table[If[MemberQ[g, ma], g, ma], {g, $dwGroupLayers}]]],
									{ma}
								];
							If[CurrentValue["ShiftKey"],
								$dwSelected = 
									If[MemberQ[$dwSelected, ma], 
										Complement[$dwSelected, temp], 
										DeleteDuplicates[Join[$dwSelected, temp]]
									],
								If[FreeQ[$dwSelected, ma], $dwSelected = temp]
							];
							If[$dwSelected =!= {},
								
								(* add compound paths containing selected objects *)
								temp=DeleteDuplicates[#[[1]]&/@Flatten[Table[Position[$dwCompoundPathLayers,s],{s,$dwSelected}],1]];
								$dwSelected = DeleteDuplicates[Flatten[Join[$dwSelected, $dwCompoundPathLayers[[temp]]]]];
								
								(* update $dwStyleMode *)
								$dwStyleMode = 
									Switch[$dwHead[[$dwSelected[[1]]]],
										Image, "image",
										"Text3D", "text",
										Text, "text",
										Line, "stroke",
										Arrow, "arrow",
										Point, "point",
										_, "fill"
									];
									
								(* update selection boundary to resolve issue of dragging object not selected by clicking mouse *)
								If[Length[$dwSelected] == 1, $dwSelectionBoundaryStart = $dwSelectionBoundary = ({{#[[1]],#[[2]]},{Mean[{#[[1]],#[[3]]}],#[[2]]},{#[[3]],#[[2]]},{#[[3]],Mean[{#[[2]],#[[4]]}]},{#[[3]],#[[4]]},{Mean[{#[[1]],#[[3]]}],#[[4]]},{#[[1]],#[[4]]},{#[[1]],Mean[{#[[2]],#[[4]]}]}}&/@{Flatten[$dwBoundingBoxes[[$dwSelected[[1]]]][[{3,1}]]]})[[1]], Nothing];
							]
						],
						If[CurrentValue["ShiftKey"],
							Nothing,
							$dwSelected = {}
						]
					],
			
				"toggleCornerPt",
					dwToggleCornerPt[],
				
				"splitPoint",
					If[$dwSelected =!= {},
						If[Union[#<(deletePrecision/$dwZoom)&/@Abs[mp-Nearest[$dwP[[$dwSelected[[1]]]],mp][[1]]]] === {True},
							
							(* returns {{n}} *)
							pos = Flatten[Position[$dwP[[$dwSelected[[1]]]],Nearest[$dwP[[$dwSelected[[1]]]],mp][[1]]]];
							pos = If[Length[pos] > 1, pos[[{1}]], pos];
							
							(* delete point *)
							If[$dwHead[[$dwSelected[[1]]]] === BezierCurve,
								If[Length[$dwP[[$dwSelected[[1]]]]] > 8 && Mod[pos[[1]] + 2, 3] == 0,(* 8 for case of closed curve; do not include handles *)
									deletepos = 
										Which[
											pos[[1]] === 1, (* first point *)
												If[$dwP[[$dwSelected[[1]],1]] === $dwP[[$dwSelected[[1]],-2]], (* closed path *)
													{pos,pos+1,pos+2,{Length[$dwP[[$dwSelected[[1]]]]]},{Length[$dwP[[$dwSelected[[1]]]]]-1},{Length[$dwP[[$dwSelected[[1]]]]]-2}},
													{pos,pos+1,pos+2}
												],
											pos[[1]] === 2, (* first handle *)
												If[$dwP[[$dwSelected[[1]],1]] === $dwP[[$dwSelected[[1]],-2]], (* closed path *)
													{pos-1,pos,pos+1,{Length[$dwP[[$dwSelected[[1]]]]]},{Length[$dwP[[$dwSelected[[1]]]]]-1},{Length[$dwP[[$dwSelected[[1]]]]]-2}},
													{pos-1,pos,pos+1}
												],
											MemberQ[{Length[$dwP[[$dwSelected[[1]]]]],Length[$dwP[[$dwSelected[[1]]]]]-1,Length[$dwP[[$dwSelected[[1]]]]]-2}, pos[[1]]], (* last point or handles *)
												If[$dwP[[$dwSelected[[1]],1]] === $dwP[[$dwSelected[[1]],-2]], (* closed path *)
													{{1},{2},{3},{Length[$dwP[[$dwSelected[[1]]]]]},{Length[$dwP[[$dwSelected[[1]]]]]-1},{Length[$dwP[[$dwSelected[[1]]]]]-2}},
													{{Length[$dwP[[$dwSelected[[1]]]]]},{Length[$dwP[[$dwSelected[[1]]]]]-1},{Length[$dwP[[$dwSelected[[1]]]]]-2}}
												],
											Mod[pos[[1]],3] === 0, (* before handle *)
												{pos,pos+1,pos+2},
											Mod[pos[[1]]+1,3] === 0, (* after handle *)
												{pos-2,pos-1,pos},
											True, 				(* main point *)
												{pos-1,pos,pos+1}
										];
									$dwP[[$dwSelected[[1]]]] = Delete[$dwP[[$dwSelected[[1]]]],deletepos],
									If[Length[$dwP[[$dwSelected[[1]]]]] <= 8, MessageDialog["Cannot delete any more points from BezierCurve."], Nothing]
								],
								If[Length[$dwP[[$dwSelected[[1]]]]] == 1,
									$dwMode = "preview"; dwDeleteLayer[],
									$dwP[[$dwSelected[[1]]]] = Delete[$dwP[[$dwSelected[[1]]]],{pos}]
								]
							],
							
							(* split point *)
							Switch[$dwHead[[$dwSelected[[1]]]],
								Point,
									AppendTo[$dwP[[$dwSelected[[1]]]], mp],
								BezierCurve,
									pos = mp;
									If[Head[pos] == List,
										$dwP[[$dwSelected[[1]]]] = dwAddBezierCurvePoint[$dwP[[$dwSelected[[1]]]], {pos}]
									],
								_,
									Do[
										If[Round[RegionNearest[Line[$dwP[[$dwSelected[[1]]]][[{n,If[n==Length@$dwP[[$dwSelected[[1]]]],1,n+1]}]]],mp/.None->{{0,0}}],.01] ==
											Round[RegionNearest[Line[Join[$dwP[[$dwSelected[[1]]]],{$dwP[[$dwSelected[[1]],1]]}]],mp/.None->{{0,0}}],.01],
											
											If[n==Length@$dwP[[$dwSelected[[1]]]],
												(* end point *)
												AppendTo[$dwP[[$dwSelected[[1]]]],RegionNearest[Line[Join[$dwP[[$dwSelected[[1]]]],{$dwP[[$dwSelected[[1]],1]]}]],mp/.None->{{0,0}}]],
												(* not end point *)
												$dwP[[$dwSelected[[1]]]] = Insert[$dwP[[$dwSelected[[1]]]],RegionNearest[Line[Join[$dwP[[$dwSelected[[1]]]],{$dwP[[$dwSelected[[1]],1]]}]],mp/.None->{{0,0}}],n+1]
											];
											
											Break[]
										],
									{n,Length@$dwP[[$dwSelected[[1]]]]}]
							]
						]
					],
				"splitShape1",
					(* reset selected point to first point *)
					If[(FreeQ[{BezierCurve,BSplineCurve}, $dwHead[[$dwSelected[[1]]]]] && Length@$dwP[[$dwSelected[[1]]]] > 2) || 
						(MemberQ[{BezierCurve,BSplineCurve}, $dwHead[[$dwSelected[[1]]]]] && Length@$dwP[[$dwSelected[[1]]]] > 5),
						
						nearestPoint = First@Nearest[$dwP[[$dwSelected[[1]]]], mp/.None->{{0,0}}];
						nearestPoint = 
							If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[1]]]]], 
								3IntegerPart[First[Flatten[Position[$dwP[[$dwSelected[[1]]]], nearestPoint, Infinity]]]/3]+1, 
								First[Flatten[Position[$dwP[[$dwSelected[[1]]]], nearestPoint, Infinity]]]
							];
						$dwP[[$dwSelected[[1]]]] = 
							If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[1]]]]] && $dwP[[$dwSelected[[1]],1]] === $dwP[[$dwSelected[[1]],-2]],
								Join[$dwP[[$dwSelected[[1]]]][[nearestPoint;;-1]], $dwP[[$dwSelected[[1]]]][[3;;nearestPoint+1]]],
								RotateLeft[$dwP[[$dwSelected[[1]]]], nearestPoint-1]
							];
						$dwMode="splitShape2",
						
						MessageDialog["Not enough points to split object."];
						$dwMode=$dwCurrentPreviewMode
					],
				"splitShape2",
					(* split shape *)
					If[(FreeQ[{BezierCurve,BSplineCurve}, $dwHead[[$dwSelected[[1]]]]] && Length@$dwP[[$dwSelected[[1]]]] > 2) || 
						(MemberQ[{BezierCurve,BSplineCurve}, $dwHead[[$dwSelected[[1]]]]] && Length@$dwP[[$dwSelected[[1]]]] > 5),
						
						temp = $dwSelected[[1]];
						nearestPoint = First@Nearest[$dwP[[$dwSelected[[1]]]], mp/.None->{{0,0}}];
						nearestPoint = 
							If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[1]]]]], 
								3IntegerPart[First[Flatten[Position[$dwP[[$dwSelected[[1]]]], nearestPoint, Infinity]]]/3]+1, 
								First[Flatten[Position[$dwP[[$dwSelected[[1]]]], nearestPoint, Infinity]]]
							];
						temp2 = $dwDupeOffset;
						$dwDupeOffset = {0,0};
						dwDuplicateLayer["ChangeMode"->False];
						$dwP[[temp]] = Take[$dwP[[$dwSelected[[1]]]], If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[1]]]]], nearestPoint+1, nearestPoint]];
						$dwP[[$dwSelected[[1]]]] = Drop[$dwP[[$dwSelected[[1]]]], {
							Switch[$dwHead[[$dwSelected[[1]]]],
								Polygon, 
									2,
								_,
									1
							], nearestPoint - 1}];
						$dwDupeOffset = temp2;
						dwUpdateBoundingBox[{temp, $dwSelected[[1]]}];
						$dwMode="splitShapeDone",
						
						MessageDialog["Not enough points to split object."];
						$dwMode=$dwCurrentPreviewMode
					],
					
				_,
					ma = $dwCurrentMouseAnnotation = MouseAnnotation[];
					If[(ma =!= Null && Head[ma] =!= Symbol) || (CurrentValue["ShiftKey"] && Head[ma] =!= Symbol),
						
						Switch[$dwMode,
							"point",
							
								If[ma =!= Null && !$dwShowMouseClickPositions,
									If[StringQ[ma],
										
										(* selection box *)
										dwSelectionBoxTransform[],
										
										(* find all selected objects *)
										If[$dwLimitPointSelection,
											(* limit objects *)
											If[Head[ma] === Integer,
												$dwPointModeSelections = 
													If[CurrentValue["ShiftKey"],
														If[MemberQ[$dwPointModeSelections, ma],
															(* remove object from selection *)
															ReplacePart[$dwPointModeSelections, Position[$dwPointModeSelections, ma]->Nothing],
															(* insert object to selection if not image or text *)
															If[MemberQ[$dwShapeSymbols, $dwHead[[ma]]],
																DeleteDuplicates[Join[$dwPointModeSelections, {ma}]],
																$dwPointModeSelections
															]
														],
														{ma}
												];
												(* unselect any points so they will not move *)
												If[CurrentValue["ShiftKey"],
													Nothing,
													$dwSelected = {}
												]
											],
											(* use all objects - should never reach here since $dwLimitPointSelection should always be true *)
											$dwPointModeSelections = Table[If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], n, Nothing], {n, Range[Length[$dwP]]}]
										];
										(* current selections within selected objects *)
										temp = Flatten[Table[Cases[$dwSelected, {n, _}], {n, $dwPointModeSelections}], 1];
										(* insert or remove new point selection *)
										If[Head[ma] === List,
											(* point selection *)
											$dwSelected = 
												If[CurrentValue["ShiftKey"],
													If[MemberQ[$dwSelected, ma],
														(* remove point from selection *)
														ReplacePart[temp, Position[temp, ma]->Nothing],
														(* insert point to selection *)
														If[$dwHead[[ma[[1]]]] =!= BezierCurve,
															Join[temp, {ma}],
															(* if BezierCurve handle select single handle *)
															If[Mod[ma[[2]] + 2, 3] == 0,
																Join[temp, {ma}],
																{ma}
															]
														]
													],
													(* point is selection *)
													If[FreeQ[$dwSelected, ma], {ma}, $dwSelected]
												],
											(* object selection *)
											Nothing
										]
									],
									Nothing
								],
							_,
								If[CurrentValue["ShiftKey"],
									temp = If[Head[ma] =!= List, Union[Flatten[Table[Table[If[MemberQ[g, m], g, Nothing], {m, ma}], {g, $dwGroupLayers}]]], {ma}]; (* collect groups of selected *)
									$dwSelected = 
										If[MemberQ[$dwSelected, ma], 
											Complement[$dwSelected, temp], 
											Join[$dwSelected, temp]
										],
									If[FreeQ[$dwSelected, ma], $dwSelected = {ma}]
								]
						],
						
						If[CurrentValue["ShiftKey"],
							Nothing,
							$dwSelected = {}
						]
					]
			]
				
		];
		If[$dwSelected =!= {}, dwUpdateTransformCenter[]; dwMouseDownBezier[]]
	]

dwMouseDownBezier[]:=
	DynamicModule[{angle, mp},
		If[$dwHead[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1, 1]], $dwSelected[[1]]]]] === BezierCurve && $dwMode === "draw",
			
			(* continue an existing object *)
			If[$dwP[[$dwSelected[[1]]]] === {} && MouseAnnotation[] =!= Null,
				$dwMouseDownOnExistingBezierPoint = True;
				If[CurrentValue[$dwOptionKey],
					$dwSelected = {MouseAnnotation[][[1]]},
					If[MouseAnnotation[][[2]] == Length[$dwP[[MouseAnnotation[][[1]]]]] - 1,(* last main point *)
						$dwSelected = {MouseAnnotation[][[1]]};
						If[Length[$dwP[[$dwSelected[[1]]]]] > 2,
							$dwP[[$dwSelected[[1]]]] = $dwP[[$dwSelected[[1]]]][[;;-4]],(* remove last point and handles *)
							$dwP[[$dwSelected[[1]]]] = {}
						]
					]
				]
			];
			
			(* create new layer if path endpoints same *)
	 		If[Length@$dwP[[$dwSelected[[1]]]] > 5 && $dwP[[$dwSelected[[1]],1]] === $dwP[[$dwSelected[[1]],-2]],
				dwNewEmptyLayer[]
			];
			
			(* add points and handles if not dragging on previous main point *)
			If[FreeQ[If[Length[$dwP[[$dwSelected[[1]]]]] < 2, Null, Round[$dwP[[$dwSelected[[1]],-2]],  Max[$dwGridStep, 2$dwMinClosedBezierDistance]]], Round[$dwClickPt,  Max[$dwGridStep, 2$dwMinClosedBezierDistance]]],
				Do[AppendTo[$dwP[[$dwSelected[[1]]]], 
					If[Length@$dwP[[$dwSelected[[1]]]] > 0, 
						If[EuclideanDistance[$dwP[[$dwSelected[[1]],1]], $dwClickPt] <= Max[$dwGridStep, $dwMinClosedBezierDistance], 
							$dwP[[$dwSelected[[1]],1]],
							(* constrain click point *)
							If[CurrentValue["ShiftKey"],
								
								(* angle constraint *)
								angle = If[n==1, ToPolarCoordinates[$dwClickPtNoGrid - $dwP[[$dwSelected[[1]],-n-1]]][[2]]180/Pi, angle];
								mp = If[n==1, $dwClickPtNoGrid - $dwP[[$dwSelected[[1]],-n-1]], mp];
								$dwP[[$dwSelected[[1]],-n-1]] +
									If[45 < Abs[angle +
											If[angle >= 0, -($dwConstrainHAngle),
												If[angle > ($dwConstrainHAngle) - 180, -($dwConstrainHAngle), 360 - ($dwConstrainHAngle)]
											]
										] < 135,
											{-(mp[[2]])*Tan[($dwConstrainVAngle + $MachineEpsilon) \[Degree]], mp[[2]]},
											{mp[[1]], (mp[[1]])*Tan[($dwConstrainHAngle + $MachineEpsilon) \[Degree]]}
									],
								(* no constraint *)
								$dwClickPt
							]
						],
						$dwClickPt
					]], 
				{n, 1, If[$dwP[[$dwSelected[[1]]]] === {}, 2, 3]}],
				
				$dwMouseDownOnExistingBezierPoint = True
			],
			 Nothing
		]
	]

End[] (* End Private Context *)

EndPackage[]