(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
	

dwObjectPointTools[]:=

	Module[{temp, temp2},
		{
			(* insert/delete point *)
			Tooltip[Dynamic@Button[$dwIconSplitPoint,
				If[$dwSelected =!= {} || $dwPointModeSelections =!= {},
					
					If[$dwMode === "point" && $dwPointModeSelections =!= {},
						$dwSelected = $dwPointModeSelections[[{1}]]
					];
					
					If[FreeQ[{Image,Text}, $dwHead[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1,1]], $dwSelected[[1]]]]]],
						dwConvertPointSelectionToLayerSelection[];
						$dwMode="splitPoint";
						$dwPointModeSelections = If[$dwSelected =!= {}, $dwSelected[[1]], {}];
						$dwSelected = If[$dwSelected =!= {}, {$dwSelected[[1]]}, {}];
						$dwPointQuantity = Length[Flatten[$dwP, 1]],
						
						$dwSelected = {};
						MessageDialog["Tool does not work with Image or Text."]
					],
					
					MessageDialog["Object not selected."]
				],
				Background->If[$dwMode === "splitPoint", $dwButtonHighlight, $dwButtonBackgroundColor], $dwButtonStyle],
			"Click line to insert point\nClick existing point to delete", TooltipDelay->$dwTooltipDelay],
			
			(* delete duplicate points *)
			Tooltip[Button[$dwIconDeleteDupePts,
				If[$dwSelected =!= {},
					
					If[FreeQ[{Image,Text}, $dwHead[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1,1]], $dwSelected[[1]]]]]],
						dwSetUndo[];
						Do[If[FreeQ[{BezierCurve} ,$dwHead[[s]]],
							$dwP[[s]] = DeleteDuplicates[$dwP[[s]], Round[#1,.01] == Round[#2,.01]&]], {s, $dwSelected}];
						$dwPointQuantity = Length[Flatten[$dwP, 1]],
						
						$dwSelected={};
						MessageDialog["Tool does not work with Image or Text."]
					],
					
					MessageDialog["Object not selected."]
				], 
				$dwButtonStyle],
			"Delete duplicate points", TooltipDelay->$dwTooltipDelay],

			(* split path *)
			Tooltip[Dynamic@Button[$dwIconSplitPath,
				(*dwDeleteEmptyLayers[];*) (* causes selected point object to be unselected *)
				If[$dwSelected =!= {},
					
					If[CurrentValue[$dwCommandKey],
							
						(* split entire path into segments *)
						If[FreeQ[{Image,Text,BezierCurve,BSplineCurve}, $dwHead[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1,1]], $dwSelected[[1]]]]]],
							
							dwConvertPointSelectionToLayerSelection[];
							If[$dwSelected === {},
								
								MessageDialog["No object selected to split."],
								
								temp = $dwSelected;
								temp2 = Length[$dwP];
								Do[
									Switch[$dwHead[[s]],
										Polygon, 
											If[$dwP[[s, 1]] =!= $dwP[[s, -1]],
												$dwP[[s]] = Join[$dwP[[s]], {$dwP[[s, 1]]}]
											];
											Do[
												dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
												$dwP[[-1]] = $dwP[[s, s1;;s1 + 1]];
												$dwStyle[[-1]] = $dwStyle[[s]],
											{s1, Length[$dwP[[s]]] - 1}],
										Line|Arrow, 
											Do[
												dwNewEmptyLayer["Head"->$dwHead[[s]], "SetUndo"->False];
												$dwP[[-1]] = $dwP[[s, s1;;s1 + 1]];
												$dwStyle[[-1]] = $dwStyle[[s]],
											{s1, Length[$dwP[[s]]] - 1}],
										_,
											MessageDialog["Cannot split all segments of BezierCurve or BSplineCurve."]
									],
								{s, temp}];
								dwUpdateBoundingBox[Range[temp2 + 1, Length[$dwP]]];
								$dwSelected = temp;
								dwDeleteLayer[];
								$dwSelected = {Length[$dwP]};
								$dwObjectQuantity = Length[$dwP];
								$dwPointQuantity = Length[Flatten[$dwP, 1]]
							],
						
							$dwSelected={};
							MessageDialog["Cannot split BezierCurve, BSplineCurve, Image or Text into segments."]
						],
						
						(* split path *)
						If[FreeQ[{Image,Text}, $dwHead[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1,1]], $dwSelected[[1]]]]]],
							dwConvertPointSelectionToLayerSelection[];
							If[$dwSelected === {},
								
								MessageDialog["No object selected to split."],
									
								$dwMode = 
									Switch[$dwHead[[$dwSelected[[1]]]],
										Polygon, 
											"splitShape1",
										BezierCurve, 
											If[$dwP[[$dwSelected[[1]],1]] === $dwP[[$dwSelected[[1]],-2]], "splitShape1", "splitShape2"],
										BSplineCurve,
											If[$dwStyle[[$dwSelected[[1]],10]], "splitShape1", "splitShape2"], 
										_,
											"splitShape2"
									];
								$dwSelected = {$dwSelected[[1]]}
							];
							$dwPointQuantity = Length[Flatten[$dwP, 1]],
						
							$dwSelected={};
							MessageDialog["Tool does not work with Image or Text."]
						]
						
					],
					
					MessageDialog["Object not selected."]
				],
				Background->If[MemberQ[{"splitShape1", "splitShape2"}, $dwMode], $dwButtonHighlight, $dwButtonBackgroundColor], $dwButtonStyle],
			"Click existing point to split open path\nClick two existing points to split closed path\n"<>$dwCommandKey<>"-click tool to split entire selected path into segments", TooltipDelay->$dwTooltipDelay],
			
			(* join lines *)
			Tooltip[Button[$dwIconJoinPaths,
				Block[{p1, p2, selected},
					If[CurrentValue[$dwCommandKey],
						
						
						(* ---------- CONNECT ALL objects with same end points ---------- *)
						(* must use all objects since object deletion changes object number *)
						If[$dwP =!= {} && Length[$dwP] > 1,
							dwSetUndo[];
							dwConvertPointSelectionToLayerSelection[];
							Do[
								If[MemberQ[$dwShapeSymbols, $dwHead[[n]]],
									If[n <= Length[$dwP],
										Do[
											If[MemberQ[$dwShapeSymbols, $dwHead[[n1]]],
												dwJoinTwoObjects[n, n1, "ConnectSamePointsOnly"->True]
											], 
										{n1, n, 1, -1}]
									]
								], 
							{n, Most[Reverse[Range[Length[$dwP]]]]}]
						];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						$dwSelected = {},
						
						If[Length[$dwSelected] == 1,
							
							(* ---------- CONNECT SINGLE OBJECT end points ---------- *)
							dwConvertPointSelectionToLayerSelection[];
							If[$dwHead[[$dwSelected[[1]]]] === BezierCurve,
								If[$dwP[[$dwSelected[[1]], 1]] =!= $dwP[[$dwSelected[[1]], -2]],
									$dwP[[$dwSelected[[1]]]] = Join[$dwP[[$dwSelected[[1]]]], $dwP[[$dwSelected[[1]]]][[{1,1,2}]]],
									$dwP[[$dwSelected[[1]],-1]] = $dwP[[$dwSelected[[1]],2]];(* fix cases where last handle (invisible) does not match first handle *)
									MessageDialog["Path is already closed."]
								],
								If[$dwP[[$dwSelected[[1]], 1]] =!= $dwP[[$dwSelected[[1]], -1]],
									$dwP[[$dwSelected[[1]]]] = Join[$dwP[[$dwSelected[[1]]]], {$dwP[[$dwSelected[[1]], 1]]}],
									MessageDialog["Path is already closed."]
								]
							],
							
							(* ---------- CONNECT SELECTED end points or nearest end points of two selected objects ---------- *)
							(* remove Image and Text *)
							$dwSelected = Table[If[MemberQ[$dwShapeSymbols, $dwHead[[If[Length[$dwSelected[[1]]] == 2, s[[1]], s]]]], s, Nothing], {s, $dwSelected}];
							If[$dwSelected =!= {} && Length[$dwSelected] > 1,
							
								dwSetUndo[];
								(* check selections *)
								{p1, p2} = Sort[$dwSelected[[;;2]]];
								
								If[Head[p1] === Integer, 
									
									(* ---------- TWO OBJECTS SELECTED ---------- *)
									dwJoinTwoObjects[p1, p2],
									
									(* ---------- TWO POINTS SELECTED ---------- *)
									If[(MemberQ[{1, Length[$dwP[[p1[[1]]]]]}, p1[[2]]] || ($dwHead[[p1[[1]]]] === BezierCurve && p1[[2]] == Length[$dwP[[p1[[1]]]]] - 1)) && 
										(MemberQ[{1, Length[$dwP[[p2[[1]]]]]}, p2[[2]]] || ($dwHead[[p2[[1]]]] === BezierCurve && p2[[2]] == Length[$dwP[[p2[[1]]]]] - 1)),
										If[p1[[1]] === p2[[1]],
											
											(* close single path *)
											If[MemberQ[{Line, Arrow}, $dwHead[[p1[[1]]]]],
												$dwP[[p1[[1]]]] = Join[$dwP[[p1[[1]]]], $dwP[[p1[[1]]]][[{1}]]],
												MessageDialog["Cannot close a "<>ToString[$dwHead[[p1[[1]]]]]<>"."]
											],
												
											(* join two paths *)
											(* higher layer first *)
											If[p1[[1]] < p2[[1]], temp = p1; p1 = p2; p2 = temp];
											(* if bezier and non-bezier make sure bezier is last *)
											If[$dwHead[[p2[[1]]]] === BezierCurve && $dwHead[[p1[[1]]]] =!= BezierCurve, temp = p1; p1 = p2; p2 = temp];
											(* join path points *)
											If[p1[[2]] == 1, (* p1 first path point *)
												If[p2[[2]] == 1, (* p2 also first path point *)
													
													$dwP[[p1[[1]]]] = 
														If[{$dwHead[[p1[[1]]]], $dwHead[[p2[[1]]]]} === {BezierCurve, BezierCurve},
															Join[Reverse@Most[Join[{$dwP[[p2[[1]], 1]]}, $dwP[[p2[[1]]]]]], Join[{$dwP[[p1[[1]], 1]]}, $dwP[[p1[[1]]]]]],
															Join[Reverse@$dwP[[p2[[1]]]], $dwP[[p1[[1]]]]]
														],

													$dwP[[p1[[1]]]] = 
														If[{$dwHead[[p1[[1]]]], $dwHead[[p2[[1]]]]} === {BezierCurve, BezierCurve},
															Join[$dwP[[p2[[1]]]], Join[{$dwP[[p1[[1]], 1]]}, $dwP[[p1[[1]]]]]],
															Join[$dwP[[p2[[1]]]], $dwP[[p1[[1]]]]]
														]
												],
												(* p1 not first path point *)
												If[p2[[2]] == 1, (* p2 first path point *)

													$dwP[[p1[[1]]]] = 
														If[{$dwHead[[p1[[1]]]], $dwHead[[p2[[1]]]]} === {BezierCurve, BezierCurve},
															Join[$dwP[[p1[[1]]]], Join[{$dwP[[p2[[1]], 1]]}, $dwP[[p2[[1]]]]]],
															Join[$dwP[[p1[[1]]]], $dwP[[p2[[1]]]]]
														],

													$dwP[[p1[[1]]]] = 
														If[{$dwHead[[p1[[1]]]], $dwHead[[p2[[1]]]]} === {BezierCurve, BezierCurve},
															Join[$dwP[[p1[[1]]]], Join[Reverse@$dwP[[p2[[1]]]], {$dwP[[p2[[1]], 1]]}]],
															Join[$dwP[[p1[[1]]]], Reverse@$dwP[[p2[[1]]]]]
														]
												]
											];
											(* check bezier length *)
											If[$dwHead[[p1[[1]]]] === BezierCurve,
												$dwP[[p1[[1]]]] = $dwP[[p1[[1]]]][[;;3IntegerPart[(Length[$dwP[[p1[[1]]]]]+1)/3]-1]]
											];
											(* delete layer *)
											$dwSelected = {p2[[1]]};
											$dwMode = $dwCurrentPreviewMode;
											dwDeleteLayer["SetUndo"->False];
											(* update *)
											$dwSelected = {If[p1[[1]] > p2[[1]], p1[[1]] - 1, p1[[1]]]};
											$dwPointQuantity = Length[Flatten[$dwP, 1]];
											dwUpdateBoundingBox[$dwSelected[[{1}]]]
										],
										
										MessageDialog["Please select two path end points."]
									]
								],
								
								MessageDialog["Please select two path end points or two objects that are not Image or Text."]
							]
						]
					]
					
				], Method -> "Queued", $dwButtonStyle],
			"Behavior depends on selection:\n   TWO END POINTS: Connect end points of path(s)\n   TWO OBJECTS: Connect nearest end points of two selected objects\n   SINGLE OBJECT: Close selected object\n-----\n"<>$dwCommandKey<>"-click to connect all paths (selected, unselected and hidden) sharing identical end point positions", TooltipDelay->$dwTooltipDelay],
			
			(* increase points *)
			Tooltip[Button[$dwIconIncreasePts,
				dwConvertPointSelectionToLayerSelection[];
				If[$dwSelected =!= {},
					dwIncreasePoints[],
					MessageDialog["Object not selected."]
				],
			$dwButtonStyle], "Increase points\n"<>$dwCommandKey<>"-click to increase with position adjustment", TooltipDelay->$dwTooltipDelay],
			
			(* decrease points *)
			Tooltip[Button[$dwIconDecreasePts,
				dwConvertPointSelectionToLayerSelection[];
				If[$dwSelected =!= {},
					dwDecreasePoints[],
					MessageDialog["Object not selected."]
				],
			$dwButtonStyle], "Decrease points", TooltipDelay->$dwTooltipDelay],
		
			(* spline discretize *)
			Tooltip[Button[$dwIconBezierDiscretize,
				DynamicModule[{f, pts},
					If[$dwSelected =!= {},
						
						If[MemberQ[{BezierCurve,BSplineCurve}, $dwHead[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1,1]], $dwSelected[[1]]]]]],
							
							CreateDialog[Pane[Dynamic@Column[{
									Dynamic@Graphics[Table[{
										Sequence@@($dwStyle[[s]][[$dwStyleStart;;-1]]/.{AbsoluteThickness[_]->AbsoluteThickness[1]}), 
										pts = Switch[$dwHead[[s]],
												BezierCurve,
													If[Length[$dwP[[s]]] > 4,
														pts = dwBezierDiscretizeList[$dwP[[s]], $dwDiscretizeResolution/3];
														dwSimplifyPts[pts, 2],
														$dwP[[s]]
													],
												BSplineCurve,
													If[Length[$dwP[[s]]] > 2,
														f = BSplineFunction[$dwP[[s]], SplineClosed->If[s==0||$dwStyle[[s,10]],True,False], SplineDegree->If[s==0,$dwDefaultSplineDegree,$dwStyle[[s,11]]]];
														pts = Table[f[pn], {pn, 0, 1, 1/$dwDiscretizeResolution}];
														dwSimplifyPts[pts, 2],
														$dwP[[s]]
													],
												_,
													$dwP[[s]]
											];
										If[$dwStyle[[s,1]], Polygon, Line][pts],
										{Black, Point[pts]}
									}, {s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}], Background->White, ImageSize->{300,300}],
									Row@{PopupMenu[Dynamic@$dwDiscretizeResolution,{10->"Rough",20->"Low",40->"Medium",80->"High",160->"Extreme",320->"Ultra"}]," resolution"},
									"",
									Style["Use descrease points tool to remove points", Italic],
									"",
									Row@{
										CancelButton[DialogReturn[]],
										DefaultButton[
											dwSetUndo[];
											dwConvertPointSelectionToLayerSelection[];
											dwDiscretizeCurve["Simplify"->True];
											dwConvertLayerSelectionToPointSelection[];
											$dwPointQuantity = Length[Flatten[$dwP, 1]];
											$dwSelected = {};
											DialogReturn[]
										]}},
								Alignment->Center],Alignment->Center,ImageSize->300],WindowTitle->"Discretize",Modal->True],
							
							$dwSelected={};
							MessageDialog["Select BezierCurve or BSplineCurve."]
						],
						
						MessageDialog["Object not selected."]
					]],$dwButtonStyle],
			"Discretize BezierCurves or BSplineCurves...", TooltipDelay->$dwTooltipDelay],
			
			(* toggle spline corner point *)
			Tooltip[Dynamic@Button[$dwIconToggleCornerPt,
				If[$dwSelected =!= {} || $dwPointModeSelections =!= {},
					
					If[FreeQ[{Image,Text}, $dwHead[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1,1]], $dwSelected[[1]]]]]],
						If[MemberQ[{"preview", "wireframe", "draw"}, $dwMode],
							dwConvertLayerSelectionToPointSelection[]
						];
						$dwMode="toggleCornerPt";
						$dwSelected = Which[$dwP =!= {}, {}, $dwSelected === {} && $dwP =!= {}, {1}, True, $dwSelected[[{1}]]],
						
						$dwSelected={};
						MessageDialog["Tool does not work with Image or Text."]
					],
					
					MessageDialog["Object not selected."]
				],
				Background->If[$dwMode === "toggleCornerPt", $dwButtonHighlight,$dwButtonBackgroundColor], $dwButtonStyle],
			"Click Bezier or BSpline point to toggle corner|smooth"],
			
			(* reverse path direction *)
			Tooltip[Button[$dwIconReverseDirection,
				If[$dwSelected =!= {},
					
					dwSetUndo[];
					Do[
						If[$dwHead[[s]] === BezierCurve,
							$dwP[[s]] = Join[Rest@Reverse[$dwP[[s]]], {If[$dwP[[s,-1]] === $dwP[[s,2]], $dwP[[s,-3]], $dwP[[s,1]] + ($dwP[[s,1]] - $dwP[[s,2]])]}],
							$dwP[[s]] = Reverse[$dwP[[s]]]
						],
					{s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}],
					
					MessageDialog["Object not selected."]
				], 
				$dwButtonStyle],
			"Reverse path direction"]
			
		}
	]
	
dwDecreasePoints[]:=
	Block[{temp, bezierTemp},
		If[FreeQ[{Image,Text}, $dwHead[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1,1]], $dwSelected[[1]]]]]],
			dwSetUndo[];
			dwConvertLayerSelectionToPointSelection[];
			
			temp = Table[$dwP[[n]], {n, $dwPointModeSelections}];
			CreateDialog[Pane[Column[{
					Dynamic@Graphics[
						Table[{
							FaceForm[GrayLevel[.7]], StrokeForm[Black], 
							Switch[$dwHead[[$dwPointModeSelections[[n]]]],
								BSplineCurve,
									BSplineCurve[temp[[n]], SplineClosed->$dwStyle[[$dwPointModeSelections[[n]], 10]], SplineDegree->$dwStyle[[$dwPointModeSelections[[n]], 11]]],
								BezierCurve,
									BezierCurve[Most[temp[[n]]]],
								_,
									$dwHead[[$dwPointModeSelections[[n]]]][temp[[n]]]
							],
							PointSize[Medium], Point[If[$dwHead[[$dwPointModeSelections[[n]]]] === BezierCurve, Most[temp[[n]]], temp[[n]]]]
						}, {n, Length[$dwPointModeSelections]}],
					Background->White, ImageSize->{300,300}],
					Row@{"total points: ", Dynamic@Total[Length[#]&/@temp]},
					Row@{Dynamic@Button["original", temp = Table[$dwP[[n]], {n, $dwPointModeSelections}], ImageSize->150]},
					Row@{Button["remove half", temp = dwDecreasePointsMethod["remove half"], ImageSize->150]},
					Row@{Style["BezierCurve uses 'remove half' method only.", Italic]},
					Row@{Button["slope", temp = dwDecreasePointsMethod["slope"], ImageSize->150],
						EventHandler[PopupMenu[Dynamic@$dwPointDecreaseRate, {100->"None", 1->"Very Low", .7->"Low", .5->"Medium", .3->"High", .1->"Very High"}], 
						{"MouseClicked":>(temp = dwDecreasePointsMethod["slope"])}, PassEventsDown->True]},
					Row@{Button["smooth", temp = dwDecreasePointsMethod["smooth"], ImageSize->150],
						EventHandler[PopupMenu[Dynamic@$dwPointSmoothDecreaseRate, {0->"Very Low", 1.5->"Low", 3->"Medium", 4.5->"High", 6->"Very High"}],
						{"MouseClicked":>(temp = dwDecreasePointsMethod["smooth"])}, PassEventsDown->True]},
					Row@{Style["'smooth' method requires a few seconds to process", Italic]},
					Row@{
						CancelButton[dwConvertPointSelectionToLayerSelection[]; DialogReturn[]],
						DefaultButton[
							Do[
								$dwP[[$dwPointModeSelections[[n]]]] = temp[[n]],
							{n, Length[$dwPointModeSelections]}];
							dwUpdateBoundingBox[Range[Length[$dwPointModeSelections]]];
							dwConvertPointSelectionToLayerSelection[];
							dwDeleteEmptyLayers[];(* needed to remove empty layers created from "smooth" method returning Null; replacing Null then removing replacement does not work *)
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							DialogReturn[]
						]}
				}, Alignment->Center],Alignment->Center,ImageSize->310],WindowTitle->"Decrease points",Modal->True],
			
			$dwSelected={};
			MessageDialog["Tool does not work with Image or Text."]
		]
	]
	
dwDecreasePointsMethod[type_:"remove half"]:=
	Block[{temp},
		Table[
			If[$dwHead[[n]] === BezierCurve,
				If[Length@$dwP[[n]] > 8,
					temp = PadLeft[$dwP[[n]], Length[$dwP[[n]]]+3];
					Join[temp[[{4,5}]], Flatten[Table[temp[[{tn,tn+1,tn+2}]], {tn, 3, Length[temp],6}], 1][[4;;-1]]],
					$dwP[[n]]
				],
				Switch[type,
					"slope",
						If[Length@$dwP[[n]] > 5,
							dwSimplifyPts[$dwP[[n]], $dwPointDecreaseRate],
							Take[$dwP[[n]],{1,-1,2}]
						],
					"smooth",
						temp = dwDecreasePointsByImage[$dwP[[n]], $dwHead[[n]], n, "ReturnPoints"->True]/.{Null->{{}}};
						If[Depth[temp] > 3,
							SortBy[temp, Length][[-1]],
							SortBy[temp, Length]
						],
					_,
						Take[$dwP[[n]],{1,-1,2}]
				]
			],
		{n, $dwPointModeSelections}]
	]
	
dwIncreasePoints[]:=
	DynamicModule[{temp, discretizedPts},
		If[FreeQ[($dwHead[[#]]&/@Table[If[Length[$dwSelected[[n]]] > 1, $dwSelected[[n,1]], $dwSelected[[n]]], {n, Length[$dwSelected]}]), Image|Text],
			dwSetUndo[];
			dwConvertLayerSelectionToPointSelection[];
			If[CurrentValue[$dwCommandKey],
				
				(* interpolate with adjustment *)
				If[MemberQ[Table[$dwHead[[s]], {s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}], BezierCurve],
					
					MessageDialog["New points of BezierCurve are not adjustable so no points added."];
					dwConvertPointSelectionToLayerSelection[],
					
					(* add closing point to selected Polygon objects if missing *)
					Do[If[$dwHead[[s]] === Polygon && $dwP[[s,1]] =!= $dwP[[s,-1]], AppendTo[$dwP[[s]], $dwP[[s,1]]], Nothing], {s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}];
					$dwPointIncreaseMoveAmount = 0;
					$dwPointIncreaseQuantity = 1;
					CreateDialog[Pane[Column[{
						Dynamic@Graphics[
							temp = $dwP;
							Do[
								Do[
									If[Length@temp[[s]] > 1,
										If[$dwPointEqualIncrease,
											temp[[s]] = dwAddEqualSpacedPoints[s, $dwPointEqualIncreaseQuantity],
											temp[[s]] = Riffle[temp[[s]], Table[(Mean[temp[[s]][[n;;n+1]]] + Cross[Append[temp[[s,n+1]] - Mean[temp[[s]][[n;;n+1]]], 0], {0, 0, 6$dwPointIncreaseMoveAmount/(2^q)}][[{1, 2}]]), {n, Length[temp[[s]]]-1}]]
										],
										Nothing
									],
								{q, $dwPointIncreaseQuantity}],
							{s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}];
							Table[{
								FaceForm[GrayLevel[.7]], StrokeForm[Black], 
								If[$dwHead[[s]] === BSplineCurve, $dwHead[[s]][temp[[s]], 
									SplineDegree->$dwStyle[[s,11]], SplineClosed->$dwStyle[[s,10]]], 
									$dwHead[[s]][temp[[s]]]
								], PointSize[Medium], Point[Table[temp[[s,n]],{n, Length[temp[[s]]]}]]
							}, {s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}],
						Background->White, ImageSize->{300,300}],
						Row@{Checkbox[Dynamic@$dwPointEqualIncrease], "equal spaced points"},
						Dynamic@If[$dwPointEqualIncrease,
							Row@{"quantity ",Slider[Dynamic@$dwPointEqualIncreaseQuantity,{1,100,1}]},
							Row@{"position ",Slider[Dynamic@$dwPointIncreaseMoveAmount,{-1,1}]}
						],
						Dynamic@If[$dwPointEqualIncrease,
							"",
							Row@{"quantity ",Slider[Dynamic@$dwPointIncreaseQuantity,{1,4,1}]," ", Dynamic[2^$dwPointIncreaseQuantity-1]}
						],
						Row@{
							Button["Reset",$dwPointIncreaseMoveAmount = 0; $dwPointIncreaseQuantity = 1],
							CancelButton[DialogReturn[]],
							DefaultButton[
								Do[
									$dwP[[s]] = temp[[s]],
								{s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}];
								dwUpdateBoundingBox[If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]];
								$dwPointQuantity = Length[Flatten[$dwP, 1]];
								dwConvertPointSelectionToLayerSelection[];
								DialogReturn[]
							]}
					}, Alignment->Center],Alignment->Center,ImageSize->310],WindowTitle->"Increase points",Modal->True]
				],
				
				(* interpolate *)
				Do[
					If[Length@$dwP[[s]] > 1,
						If[$dwHead[[s]] === BezierCurve,
							If[Length[$dwP[[s]]] > 4,
								$dwP[[s]] = dwAddBezierCurvePoint[$dwP[[s]], Table[discretizedPts = dwBezierDiscretizeList[$dwP[[s, n-3;;n]], 16*$dwDiscretizeResolution]; discretizedPts[[IntegerPart[Length[discretizedPts]/2]]], {n, Length[$dwP[[s]]]-1, 4, -3}]],
								Nothing
							]
							(* old method adds points between control points *)
							(*$dwP[[s]]=Join[$dwP[[s]],$dwP[[s]][[{1}]]];*)
							(*$dwP[[s]]=Most@Riffle[$dwP[[s]],Table[Median[{$dwP[[s,n]],$dwP[[s,n+1]]}],{n,1,Length@$dwP[[s]]-1}]];
							If[Length@$dwP[[s]] > 7,
								$dwP[[s]]=Take[$dwP[[s]],3IntegerPart[Length@$dwP[[s]]/3]-1]
							]*),
							
							$dwP[[s]]=Join[$dwP[[s]],$dwP[[s]][[{1}]]];
							$dwP[[s]]=Most@Riffle[$dwP[[s]],Table[Median[{$dwP[[s,n]],$dwP[[s,n+1]]}],{n,1,Length@$dwP[[s]]-1}]];
							$dwP[[s]]=If[MemberQ[{Arrow, Line, Point}, $dwHead[[s]]] || (MemberQ[{BSplineCurve}, $dwHead[[s]]] && !$dwStyle[[s,10]]), Most[$dwP[[s]]], $dwP[[s]]]
						]
					],
				{s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}];
				$dwPointQuantity = Length[Flatten[$dwP, 1]];
				dwUpdateBoundingBox[$dwPointModeSelections];
				$dwSelected={}
			],
			
			MessageDialog["Tool does not work with Image or Text."]
		]
	]
	
Options[dwJoinTwoObjects] = {"ConnectSamePointsOnly"->False};

dwJoinTwoObjects[objNum1_, objNum2_, OptionsPattern[]]:=
	Block[{distance, objDeleted = False, sameOnly = OptionValue["ConnectSamePointsOnly"]},				
		
		If[objNum1 != objNum2 && (Length[$dwP] >= objNum1 && Length[$dwP] >= objNum2),
			If[$dwHead[[objNum1]] === BezierCurve && $dwHead[[objNum2]] === BezierCurve,
			
				(* ---------- BEZIERCURVE  ---------- *)
				(* find closest end points *)
				distance = {
					EuclideanDistance[$dwP[[objNum1,1]],$dwP[[objNum2,1]]],
					EuclideanDistance[$dwP[[objNum1,1]],$dwP[[objNum2,-2]]],
					EuclideanDistance[$dwP[[objNum1,-2]],$dwP[[objNum2,1]]],
					EuclideanDistance[$dwP[[objNum1,-2]],$dwP[[objNum2,-2]]]
				};
				If[Chop[Min[distance]] < .01,
					
					objDeleted = True;
					(* both objects contain same point so add part of second object points to first object *)
					Switch[Flatten[Position[distance, Min[distance]]][[1]],
						1, $dwP[[objNum1]] = Join[Reverse[$dwP[[objNum2]]][[2;;-1]], $dwP[[objNum1]][[2;;-1]]],
						2, $dwP[[objNum1]] = Join[$dwP[[objNum2]][[;;-3]], $dwP[[objNum1]]],
						3, $dwP[[objNum1]] = Join[$dwP[[objNum1]][[;;-3]], $dwP[[objNum2]]],
						_, $dwP[[objNum1]] = Join[$dwP[[objNum1]][[;;-3]], Reverse[$dwP[[objNum2]]][[2;;-1]], {$dwP[[objNum2,1]]+($dwP[[objNum2,1]]-$dwP[[objNum2,2]])}]
					],
					
					If[sameOnly,
						Nothing,
						
						objDeleted = True;
						(* add second object points to first object *)
						Switch[Flatten[Position[distance, Min[distance]]][[1]],
							1, $dwP[[objNum1]] = Join[Reverse[$dwP[[objNum2]]][[2;;-1]], {$dwP[[objNum2]][[1]]+($dwP[[objNum2]][[1]]-$dwP[[objNum2]][[2]])}, {$dwP[[objNum1]][[1]]+($dwP[[objNum1]][[1]]-$dwP[[objNum1]][[2]])}, $dwP[[objNum1]]],
							2, $dwP[[objNum1]] = Join[$dwP[[objNum2]], {$dwP[[objNum1]][[1]]+($dwP[[objNum1]][[1]]-$dwP[[objNum1]][[2]])}, $dwP[[objNum1]]],
							3, $dwP[[objNum1]] = Join[$dwP[[objNum1]], {$dwP[[objNum2,1]]+($dwP[[objNum2,1]]-$dwP[[objNum2,2]])}, $dwP[[objNum2]]],
							_, $dwP[[objNum1]] = Join[$dwP[[objNum1]], Reverse[$dwP[[objNum2]]], {$dwP[[objNum2,1]]+($dwP[[objNum2,1]]-$dwP[[objNum2,2]])} ]
						]
					]
				];
				(* if closed path, last point = second point *)
				If[Round[$dwP[[objNum1,1]], $dwMinClosedBezierDistance] === Round[$dwP[[objNum1,-2]], $dwMinClosedBezierDistance],
					$dwP[[objNum1,1]] = $dwP[[objNum1,-2]];
					$dwP[[objNum1,-1]] = $dwP[[objNum1,2]]
				],
				
				(* ---------- NON - BEZIERCURVE  ---------- *)
				(* find closest end points *)
				distance = {
					EuclideanDistance[$dwP[[objNum1,1]],$dwP[[objNum2,1]]],
					EuclideanDistance[$dwP[[objNum1,1]],$dwP[[objNum2,-1]]],
					EuclideanDistance[$dwP[[objNum1,-1]],$dwP[[objNum2,1]]],
					EuclideanDistance[$dwP[[objNum1,-1]],$dwP[[objNum2,-1]]]
				};
				If[Chop[Min[distance]] < .01,
					
					objDeleted = True;
					(* both objects contain same point so add part of second object points to first object *)
					Switch[Flatten[Position[distance, Min[distance]]][[1]],
						1, $dwP[[objNum1]] = Join[Most[Reverse[$dwP[[objNum2]]]], $dwP[[objNum1]]],
						2, $dwP[[objNum1]] = Join[Most[$dwP[[objNum2]]], $dwP[[objNum1]]],
						3, $dwP[[objNum1]] = Join[$dwP[[objNum1]], Rest[$dwP[[objNum2]]]],
						_, $dwP[[objNum1]] = Join[$dwP[[objNum1]], Rest[Reverse[$dwP[[objNum2]]]]]
					],
					
					If[sameOnly,
						Nothing,
						
						objDeleted = True;
						(* add second object points to first object *)
						Switch[Flatten[Position[distance, Min[distance]]][[1]],
							1, $dwP[[objNum1]] = Join[Reverse[$dwP[[objNum2]]], $dwP[[objNum1]]],
							2, $dwP[[objNum1]] = Join[$dwP[[objNum2]], $dwP[[objNum1]]],
							3, $dwP[[objNum1]] = Join[$dwP[[objNum1]], $dwP[[objNum2]]],
							_, $dwP[[objNum1]] = Join[$dwP[[objNum1]], Reverse[$dwP[[objNum2]]]]
						]
					]
				]
			];
			(* updates *)
			If[objDeleted,
				(* delete layer *)
				$dwSelected = {objNum2};
				$dwMode = $dwCurrentPreviewMode;
				dwDeleteLayer["SetUndo"->False];
				(* update *)
				$dwSelected = {If[objNum1 > objNum2, objNum1 - 1, objNum1]};
				dwUpdateBoundingBox[$dwSelected[[{1}]]];
			]
		]
	]

End[] (* End Private Context *)

EndPackage[]