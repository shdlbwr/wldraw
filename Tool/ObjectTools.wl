(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwObjectTools[]:=
	Module[{closed, temp},
		{
			(* duplicate *)
			Tooltip[Button[$dwIconObjectDupe,
				If[$dwSelected =!= {},
					If[CurrentValue[$dwCommandKey],
						CreateDialog[
							Pane[Column[{
									Pane[Row@{" h ",Slider[Dynamic@$dwDupeOffset[[1]],{-2,2,$dwGridStep}],
										Button["<",If[CurrentValue[$dwCommandKey], $dwDupeOffset[[1]]=0, $dwDupeOffset[[1]]-=$dwGridStep],ImageSize->12,Appearance->"Palette"],
										Button[">",If[CurrentValue[$dwCommandKey], $dwDupeOffset[[1]]=0, $dwDupeOffset[[1]]+=$dwGridStep],ImageSize->12,Appearance->"Palette"],
										Spacer[5],Dynamic@Round[$dwDupeOffset[[1]],.005]},
									ImageSize->280],
									Pane[Row@{" v ",Dynamic@Slider[Dynamic@$dwDupeOffset[[2]],{-2,2,$dwGridStep}],
										Button["<",If[CurrentValue[$dwCommandKey], $dwDupeOffset[[2]]=0, $dwDupeOffset[[2]]-=$dwGridStep],ImageSize->12,Appearance->"Palette"],
										Button[">",If[CurrentValue[$dwCommandKey], $dwDupeOffset[[2]]=0, $dwDupeOffset[[2]]+=$dwGridStep],ImageSize->12,Appearance->"Palette"],
										Spacer[5],Dynamic@Round[$dwDupeOffset[[2]],.005]},
									ImageSize->280],
									Row@{"quantity ",PopupMenu[Dynamic@$dwDupeQuantity,Range[20]]},
									Row@{CancelButton[DialogReturn[]],DefaultButton[dwSetUndo[];Do[dwDuplicateLayer["SetUndo"->False],{$dwDupeQuantity}];DialogReturn[]]}
								}, Alignment->Center],
							ImageSize->280], Background->LightGray, WindowTitle->"Duplicate", Modal->True],
							dwSetUndo[];
							dwDuplicateLayer[]
					],
					MessageDialog["Select object to duplicate."]
				],
				$dwButtonStyle],
			"Duplicate selected\n"<>$dwCommandKey<>"-click to set distance and quantity", TooltipDelay->$dwTooltipDelay],

			(* layer order *)
			Tooltip[Dynamic[Button[$dwIconMoveToFront,
				DynamicModule[{},
					dwSetUndo[];
					dwConvertPointSelectionToLayerSelection[];
					Which[
						CurrentValue[$dwCommandKey],
							dwMoveLayers[$dwSelected, 1, "SetUndo"->False],
						CurrentValue[$dwOptionKey],
							dwMoveLayers[$dwSelected, Max[$dwSelected], "SetUndo"->False],
						CurrentValue[$dwShiftKey],
							dwMoveLayers[$dwSelected, Min[$dwSelected], "SetUndo"->False],
						True,
							dwMoveLayers[$dwSelected, Length[$dwP], "SetUndo"->False]
					]
				],$dwButtonStyle](*, TrackedSymbols :> {$dwSelected, $dwGroupLayers}*)],
			"Move selected to front\n"<>$dwCommandKey<>"-click to move to back\n"<>$dwOptionKey<>"-move all selected to front selection\n"<>$dwShiftKey<>"-move all selected to back selection", TooltipDelay->$dwTooltipDelay],
			
			(* group *)
			Tooltip[Button[$dwIconGroupShape,
					Module[{groups,delete},
						If[MemberQ[{"preview", "wireframe"}, $dwMode],
							dwSetUndo[];
							If[CurrentValue[$dwCommandKey],
								delete=List/@DeleteDuplicates[#[[1]]&/@Flatten[Table[Position[$dwGroupLayers,s],{s,$dwSelected}],1]];
								$dwGroupLayers=Delete[$dwGroupLayers,delete],
						
								(* collect and remove groups containing selections *)
								If[Length[$dwSelected] > 1,
									groups=DeleteDuplicates[#[[1]]&/@Flatten[Table[Position[$dwGroupLayers,s],{s,$dwSelected}],1]];
									$dwGroupLayers=Delete[$dwGroupLayers,List/@groups];
									(* add new groups *)
									$dwGroupLayers=Join[$dwGroupLayers,{$dwSelected}]/.{}->Sequence[],
								MessageDialog["Choose two or more objects to group together."]
								]
							]
						]
					],
				$dwButtonStyle],
			"Group selected objects\n"<>$dwCommandKey<>"-click to ungroup", TooltipDelay->$dwTooltipDelay],
			
			(* combine objects *)
			Tooltip[Button[$dwIconCombineShapes, dwShapeCombineIntersect[], $dwButtonStyle],
			"Combine selected objects", TooltipDelay->$dwTooltipDelay],
			
			(* intersect objects *)
			Tooltip[Button[$dwIconIntersectShapes, dwShapeCombineIntersect["Process"->RegionIntersection], $dwButtonStyle],
			"Intersect selected objects", TooltipDelay->$dwTooltipDelay],
			
			(* blend objects *)
			Tooltip[Dynamic@Button[$dwIconBlendShapes,
				If[(Length@$dwSelected == 1 && MemberQ[Flatten[{$dwStyle[[$dwSelected[[1]],8,1]]}], "BlendGradient"]) ||
					(Length@$dwSelected == 2 && FreeQ[FreeQ[{Image, Text}, $dwHead[[#]]]&/@$dwSelected, False]),
					CreateDialog[
						DynamicModule[{quantity = 1, reverse = False, radiate = 0, blendFrom, blendTo, blendToList = If[MemberQ[$dwShapeSymbols, $dwHead[[#]]], #, Nothing]&/@Range[Length[$dwP]]},
								
								dwSetUndo[];
								dwConvertPointSelectionToLayerSelection[];
								If[Length@$dwSelected == 1,
									$dwSelected = Join[$dwSelected, {$dwStyle[[$dwSelected[[1]],8,1,2]]}];
									quantity = $dwStyle[[$dwSelected[[1]],8,1,3]]
								];
								{blendFrom, blendTo} = Sort@$dwSelected;
								Pane[
									Column[{
										Dynamic@Graphics[Flatten[{FaceForm[{Opacity[0]}], EdgeForm[Black],
											(* first layer *)
											$dwHead[[blendFrom]][Sequence@@If[$dwHead[[blendFrom]] === BezierCurve, {Most[$dwP[[blendFrom]]]}, {$dwP[[blendFrom]],Sequence@@If[$dwHead[[blendFrom]] === BSplineCurve, {SplineClosed->$dwStyle[[blendFrom,10]], SplineDegree->$dwStyle[[blendFrom,11]]}, {}]}]],
											(* blend layers *)
											If[blendFrom =!= blendTo,
												If[Length[$dwP[[blendFrom]]] == Length[$dwP[[blendTo]]] && $dwHead[[blendFrom]] === $dwHead[[blendTo]],
													$dwHead[[blendFrom]][If[$dwHead[[blendFrom]] === BezierCurve, Most[#], #]]&/@dwBlends[$dwBlendResolution, "Quantity"->quantity, "ReturnPoints"->True, "ReverseDirection"->reverse, "Radiate"->radiate],
													Polygon[#]&/@dwBlends[$dwBlendResolution, "Quantity"->quantity, "ReturnPoints"->True, "ReverseDirection"->reverse, "Radiate"->radiate]
												],
												{}
											],
											(* last layer *)
											$dwHead[[blendTo]][Sequence@@If[$dwHead[[blendTo]] === BezierCurve, {Most[$dwP[[blendTo]]]}, {$dwP[[blendTo]],Sequence@@If[$dwHead[[blendTo]] === BSplineCurve, {SplineClosed->$dwStyle[[blendTo,10]], SplineDegree->$dwStyle[[blendTo,11]]}, {}]}]]}],
										Background->White, ImageSize->{300,200}],
										Row@{"Target object ", ActionMenu[Dynamic@blendTo, With[{bon = #},bon:>($dwStyle[[blendFrom,8,1,2]] = bon; blendTo = bon)]&/@blendToList, Appearance->"PopupMenu"],
											Spacer[30], "Blends ", InputField[Dynamic@quantity, Number, FieldSize->3]},
										Row@{"resolution ", Slider[Dynamic@$dwBlendResolution,{1,120,1}, ImageSize->200], " ", Dynamic@$dwBlendResolution},
										Row@{"radiate ", Slider[Dynamic@radiate, {-1,1,.1}, ImageSize->200], " ", Dynamic@radiate},
										Row@{Checkbox[Dynamic@reverse], " reverse path direction of first object"},
										Row@{
												Button["Cancel", 
													DialogReturn[]
												],
												Button["Expose blend", 
													$dwStyle[[blendFrom,8,1]] = None;
													If[blendFrom =!= blendTo,
														dwBlends[$dwBlendResolution, "Quantity"->quantity, "ReverseDirection"->reverse, "Radiate"->radiate]
													];
													$dwObjectQuantity = Length[$dwP]; 
													$dwPointQuantity = Length[Flatten[$dwP, 1]];
													DialogReturn[]
												],
												DefaultButton["Blend", 
													If[blendFrom === blendTo,
														$dwStyle[[blendFrom,8,1]] = None,
														If[reverse, $dwP[[blendFrom]] = Reverse@$dwP[[blendFrom]]];
														$dwStyle[[blendFrom,8,1]] = {"BlendGradient", blendTo, quantity, 0}
													];
													DialogReturn[]
												]
											}}, 
									Alignment->Center]
								]
						], Background->LightGray, WindowTitle->"Blend", Modal->True],
							
					MessageDialog["Select two shapes to blend."]
				],
					
			$dwButtonStyle],
			"Blend two selected shapes", TooltipDelay->$dwTooltipDelay],
			
			(* reflect *)
			Tooltip[Dynamic@Button[$dwIconReflectH,
				Block[{center},
					dwSetUndo[];
					dwConvertPointSelectionToLayerSelection[];
					If[CurrentValue[$dwCommandKey],
						(* vertical *)
						center = {0,1}dwFindSide[Flatten[Table[$dwP[[s]],{s,$dwSelected}],1],$dwTransformOrigin];
						Do[
							Switch[$dwHead[[s]],
								Image,
									$dwStyle[[s,2]] = ImageTransformation[$dwStyle[[s,2]], {#[[1]], 1 - #[[2]]} &],
								_,
									center = If[$dwTransformEach,{0,1}dwFindSide[$dwP[[s]],$dwTransformOrigin],center];$dwP[[s]]=#-2({0,#[[2]]}-center)&/@$dwP[[s]]
							],
						{s,$dwSelected}];
						dwUpdateBoundingBox[$dwSelected],
						(* horizontal *)
						center = {1,0}dwFindSide[Flatten[Table[$dwP[[s]],{s,$dwSelected}],1],$dwTransformOrigin];
						Do[
							Switch[$dwHead[[s]],
								Image,
									$dwStyle[[s,2]] = ImageTransformation[$dwStyle[[s,2]], {1 - #[[1]], #[[2]]} &],
								_,
									center = If[$dwTransformEach,{1,0}dwFindSide[$dwP[[s]],$dwTransformOrigin],center];$dwP[[s]]=#-2({#[[1]],0}-center)&/@$dwP[[s]]
							],
						{s,$dwSelected}];
						dwUpdateBoundingBox[$dwSelected]
					]
				],$dwButtonStyle],
			"Reflect horizontal\n"<>$dwCommandKey<>"-click to reflect vertical", TooltipDelay->$dwTooltipDelay],
			
			(* mirror *)
			Tooltip[Dynamic@Button[$dwIconMirrorH,
				Block[{center},
					dwSetUndo[];
					dwConvertPointSelectionToLayerSelection[];
					If[CurrentValue[$dwCommandKey],
						(* vertical *)
						center={0,1}dwFindCenter[Flatten[Table[$dwP[[s]],{s, $dwSelected}],1]];
						Do[center=If[$dwTransformEach,{0,1}dwFindCenter[$dwP[[s]]],center]; $dwP[[s]] = Join[$dwP[[s]],Reverse[#-2({0,#[[2]]}-center)&/@$dwP[[s]]]],{s, $dwSelected}];
						dwUpdateBoundingBox[$dwSelected],
						(* horizontal *)
						center = {1,0}dwFindCenter[Flatten[Table[$dwP[[s]], {s, $dwSelected}],1]];
						Do[center = If[$dwTransformEach, {1,0}dwFindCenter[$dwP[[s]]], center]; $dwP[[s]] = Join[$dwP[[s]], Reverse[(#-2({#[[1]],0}-center)&/@$dwP[[s]])]], {s, $dwSelected}];
						dwUpdateBoundingBox[$dwSelected]
					]
				],$dwButtonStyle],
			"Mirror horizontal\n"<>$dwCommandKey<>"-click to mirror vertical", TooltipDelay->$dwTooltipDelay],

			(* shear *)
				Tooltip[Dynamic@Button[$dwIconShearShape,
					dwConvertPointSelectionToLayerSelection[];
					(* remove unwanted heads *)
					$dwSelected = Table[If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], n, Nothing], {n, $dwSelected}];
					If[$dwSelected === {},
						
						MessageDialog["Shear is for Line, Arrow, Polygon, BezierCurve, BSplineCurve and Point."],
								
						CreateDialog[
							DynamicModule[{func},
								Pane[Column[{
									Row@{Dynamic@Graphics[Table[
											func=ShearingTransform[$dwShear Degree,Switch[$dwShearDirection,"Horizontal",{1,0},_,{0,1}],Switch[$dwShearDirection,"Horizontal",{0,1},_,{1,0}],If[$dwTransformEach,dwFindSide[$dwP[[s]],$dwTransformOrigin], 
												If[$dwP=!={{}}, dwFindSide[Flatten[Table[$dwP[[s]],{s, If[Length[$dwSelected] > 3, $dwSelected[[Range[3]]], $dwSelected]}],1], $dwTransformOrigin], {0,0}]]];
											{Sequence@@$dwStyle[[s]][[$dwStyleStart;;-1]],$dwHead[[s]][func[#]&/@If[$dwHead[[s]]===BezierCurve,Most[$dwP[[s]]],$dwP[[s]]]]},{s, If[Length[$dwSelected] > 3, $dwSelected[[Range[3]]], $dwSelected]}], 
											Background->White, ImageSize->{{260},{260}}, PlotRange->dwFindPlotRange[If[Length[$dwSelected] > 3, $dwSelected[[Range[3]]], $dwSelected], "BoundaryForm"->False, "Padding"->.2(*, "EqualSides"->False*)]]},
									Row@{Dynamic@Slider[Dynamic@$dwShear,{-45,45,1}],
									Button["<",$dwShear=Min[Max[$dwShear-=1,-45],45],ImageSize->12,Appearance->"Palette"],
									Button[">",$dwShear=Min[Max[$dwShear+=1,-45],45],ImageSize->12,Appearance->"Palette"],
									Spacer[5],Dynamic@$dwShear},
									Grid[{Button[#,$dwShear=#,Appearance->"Palette"]&/@Range[0,45,5]},Spacings->{0,0}],
									Row@{PopupMenu[Dynamic@$dwShearDirection,{"Vertical","Horizontal"}]},
									Row@{Checkbox[Dynamic@$dwTransformEach], " shear each",Spacer[20],"origin: ",Dynamic@dwTransformOrigin[]},
									Row@{Button["Duplicate",dwDuplicateLayer[];
											$dwSelectionCenter = If[$dwP=!={{}}, dwFindSide[Flatten[Table[$dwP[[s]],{s, If[Length[$dwSelected] > 3, $dwSelected[[Range[3]]], $dwSelected]}],1], $dwTransformOrigin], {0,0}];
											Do[
												func=ShearingTransform[$dwShear Degree,Switch[$dwShearDirection,"Horizontal",{1,0},_,{0,1}],Switch[$dwShearDirection,"Horizontal",{0,1},_,{1,0}],If[$dwTransformEach,dwFindSide[$dwP[[s]],$dwTransformOrigin], $dwSelectionCenter]];
												$dwP[[s]]=func[#]&/@$dwP[[s]],
											{s,$dwSelected}];
											dwUpdateBoundingBox[$dwSelected];
											DialogReturn[]
											],
										CancelButton[DialogReturn[]],
										DefaultButton[dwSetUndo[];
											$dwSelectionCenter = If[$dwP=!={{}}, dwFindSide[Flatten[Table[$dwP[[s]],{s, If[Length[$dwSelected] > 3, $dwSelected[[Range[3]]], $dwSelected]}],1], $dwTransformOrigin], {0,0}];
											Do[
												func=ShearingTransform[$dwShear Degree,Switch[$dwShearDirection,"Horizontal",{1,0},_,{0,1}],Switch[$dwShearDirection,"Horizontal",{0,1},_,{1,0}],If[$dwTransformEach,dwFindSide[$dwP[[s]],$dwTransformOrigin], $dwSelectionCenter]];
												$dwP[[s]]=func[#]&/@$dwP[[s]],
											{s,$dwSelected}];
											dwUpdateBoundingBox[$dwSelected];
											DialogReturn[]
										]}
									}, Alignment->Center], ImageSize->260]
							], 
						Background->LightGray, WindowTitle->"Shear", Modal->True]
					], $dwButtonStyle],
				"Shear", TooltipDelay->$dwTooltipDelay],
			
			(* compound path *)
			Tooltip[Button[$dwIconCompoundPath,
					Module[{groups,delete},
						(* parse selection *)
						dwConvertPointSelectionToLayerSelection[];
						$dwSelected = If[MemberQ[$dwShapeSymbols, $dwHead[[#]]], #, Nothing]&/@$dwSelected;
						(* create compound paths *)
						If[$dwSelected === {} || Length[$dwSelected] < 2,
							MessageDialog["CompoundPath needs two Line, BezierCurve or BSplineCurve objects."],
							
							(* convert polygon, arrow and point to line *)
							Do[If[MemberQ[{Polygon, Arrow, Point}, $dwHead[[s]]], $dwHead[[s]] = Line, Nothing], {s, $dwSelected}];
							(* collect compound path list *)
							If[CurrentValue[$dwCommandKey],
								
								(* remove compound paths *)
								delete=List/@DeleteDuplicates[#[[1]]&/@Flatten[Table[Position[$dwCompoundPathLayers,s],{s,$dwSelected}],1]];
								$dwCompoundPathLayers=Delete[$dwCompoundPathLayers,delete],
						
								(* collect and remove groups containing selections *)
								groups=DeleteDuplicates[#[[1]]&/@Flatten[Table[Position[$dwCompoundPathLayers,s],{s,$dwSelected}],1]];
								$dwCompoundPathLayers=Delete[$dwCompoundPathLayers,List/@groups];
								(* add new groups *)
								$dwCompoundPathLayers=Join[$dwCompoundPathLayers,{Sort[$dwSelected]}]/.{}->Sequence[];
								(* set fill to True *)
								If[MemberQ[{Line, BezierCurve, BSplineCurve}, $dwHead[[$dwSelected[[1]]]]], $dwStyle[[$dwSelected[[1]], 1]] = True, Nothing];
								(* update *)
								dwUpdateSelected[$dwSelected[[1]]]
							];
							(* updates *)
							$dwStyleMode = "fill";
							dwUpdateAllBoundingBoxes[]
						]
					],
				$dwButtonStyle],
			"Compound object\n-----\nClick to create\n"<>$dwCommandKey<>"-click to remove\nFor Line, BezierCurve and BSplineCurve only", TooltipDelay->$dwTooltipDelay],
			
			(* round corners *)
			Tooltip[Dynamic@Button[$dwIconRoundCorners,
				dwConvertPointSelectionToLayerSelection[];
				(* remove image and text objects *)
				$dwSelected=(If[MemberQ[{Image, Text}, $dwHead[[#]]], Null, #]&/@$dwSelected)/.Null->Sequence[];
				If[$dwSelected === {},
					MessageDialog["Round shapes of Line, Arrow, Polygon, BezierCurve, BSplineCurve or Point."],
					CreateDialog[
						Pane[
							Column[{
								If[Length[$dwP[[$dwSelected[[1]]]]] < 3,
									"Object needs to contain at least 3 points.",
									closed = Which[
													$dwP[[$dwSelected[[1]],1]] =!= $dwP[[$dwSelected[[1]],-1]] && MemberQ[{Line, Arrow, Point}, $dwHead[[$dwSelected[[1]]]]],
														False,
													True,
														True 
												];
									Dynamic@Graphics[{GrayLevel[.7],StrokeForm[Black],If[closed, FilledCurve, Sequence][BezierCurve[Most[
										dwRoundCorners[
											DeleteDuplicates[$dwP[[$dwSelected[[1]]]],Round[#1,.01]==Round[#2,.01]&],
											$dwRoundCorner,
											"Closed"->closed
										]
										]]]},ImageSize->{200,200}]
								],
								Row@{Pane["radius ",ImageSize->50,Alignment->Right],Slider[Dynamic@$dwRoundCorner,{.01,.5,.005}],Spacer[5],Pane[Dynamic@$dwRoundCorner,ImageSize->30,Alignment->Left]},
								Row@{
									Button["Cancel", DialogReturn[]],
									DefaultButton[
										dwSetUndo[];
										Do[
											If[Length[$dwP[[s]]] < 3,
												Nothing,
												closed = 
													Which[
														$dwP[[s,1]] =!= $dwP[[s,-1]] && MemberQ[{Line, Arrow, Point}, $dwHead[[s]]],
															False,
														True,
															True 
													];
												$dwHead[[s]] = BezierCurve;
												$dwP[[s]] = DeleteDuplicates[$dwP[[s]], Round[#1,.01] == Round[#2,.01]&];
												temp = dwRoundCorners[$dwP[[s]], $dwRoundCorner, "Closed"->closed];
												$dwP[[s]] = If[closed, Join[temp, {temp[[1]]+(temp[[1]]-temp[[2]])}, temp[[;;2]]], temp];
												$dwStyle[[s,1]] = If[closed, True, False]
										],{s,$dwSelected}];
										dwUpdateBoundingBox[$dwSelected];
										DialogReturn[]
									]
								}},
							Alignment->Center],
					ImageSize->300],
					Background->LightGray, WindowTitle->"Round corners", Modal->True]
				], $dwButtonStyle],
			"Round corners", TooltipDelay->$dwTooltipDelay],
			
			(* distort *)
			Tooltip[Dynamic@Button[$dwIconDistortShape,
				Block[{},
					dwSetUndo[];
					dwConvertPointSelectionToLayerSelection[];
					dwDistortShape[];
					$dwStyleMode = "fill"
				],$dwButtonStyle],
			"Distort selected objects...", TooltipDelay->$dwTooltipDelay],
			
			(* object shadows *)
			Tooltip[Button[$dwIconObjectShadow, 
				dwSetUndo[];
				dwShapeShadow[];
				$dwStyleMode = "fill",
				$dwButtonStyle],
			"Create shadow for selected objects", TooltipDelay->$dwTooltipDelay],
			
			(* offset path *)
			Tooltip[Button[$dwIconOffsetLines,
					DynamicModule[{g,final,p,finalP,x,y,quantity=1},
						(* remove image and text objects *)
						dwConvertPointSelectionToLayerSelection[];
						$dwSelected=(If[FreeQ[{Image, Text}, $dwHead[[#]]], #, Null]&/@$dwSelected)/.Null->Sequence[];
						If[$dwSelected === {},
							MessageDialog["Offset path of Line, Arrow, Polygon, BezierCurve, BSplineCurve or Point."],
							dwSetUndo[];
							CreateDialog[Pane[Column[{
								Row@{"Distance ",PopupMenu[Dynamic@$dwOffsetSize,Join[Range[.01,.10,.01],{.15,.2,.25,.3,.35,.4,.45,.5}],ImageSize->100]},
								Row@{"Resolution ",PopupMenu[Dynamic@$dwOffsetRes,{.01->"Extreme",.02->"Very high",.03->"High",.04->"Medium",.05->"Low"},ImageSize->100]},
								Row@{"Quantity ",PopupMenu[Dynamic@quantity,Range[10],ImageSize->100]},
								Row@{Checkbox[Dynamic@$dwOffsetRemovePts]," Remove excess points"},
								Row@{Button["Cancel",DialogReturn[]],
									DefaultButton["Offset",
										If[$dwP[[$dwSelected[[1]]]] =!= {} && Length@$dwP[[$dwSelected[[1]]]] > 2,
											Do[
												g = BoundaryDiscretizeGraphics[Polygon[$dwP[[If[q == 1,$dwSelected[[1]],-1]]]]];
												final = Quiet@BoundaryDiscretizeRegion[ImplicitRegion[RegionDistance[g,{x,y}]<=$dwOffsetSize,{x,y}],MaxCellMeasure->$dwOffsetRes];
												p = MeshPrimitives[final,2][[1,1]];
												(* combine straight lines *)
												If[$dwOffsetRemovePts === True,
													finalP = {p[[1]]};
													Do[If[Min[Abs@Round[(p[[n,2]]-p[[n-1,2]])/(Chop[(p[[n,1]]-p[[n-1,1]])]+$MachineEpsilon),.0001],20]===Min[Abs@Round[(p[[n+1,2]]-p[[n,2]])/(Chop[(p[[n+1,1]]-p[[n,1]])]+$MachineEpsilon),.0001],20],Nothing,AppendTo[finalP,p[[n]]]],{n,2,Length@p-1}];
													AppendTo[finalP,p[[-1]]],
													finalP = p
												];
												(* create new layer *)
												dwNewEmptyLayer["SetUndo"->False];
												$dwP[[-1]] = finalP;
												dwUpdateGradients[{-1}];
												dwUpdateBoundingBox[{-1}],
											{q, quantity}];
											$dwPointQuantity = Length[Flatten[$dwP, 1]];
										];
										DialogReturn[]
									]}},Alignment->Right],ImageSize->160],
							Background->LightGray, WindowTitle->"Offset shape boundary",Modal->True]
						]
					]
				,$dwButtonStyle],
			"Offset shape boundary", TooltipDelay->$dwTooltipDelay],
			
			(* connector *)
			Tooltip[
				ActionMenu[Pane[$dwIconConnector],
				{
					Graphics[{GrayLevel[.6],Rectangle[{-1,-1},{-.5,-.5}],Rectangle[{1,-1},{.5,-.5}],Rectangle[{-1,1},{-.5,.5}],Line[{{-.75,-.75},{.75,.75}}],Line[{{-.75,.75},{.75,-.75}}],Black,CapForm["Butt"],Line[{{0,0},{.5,.5}}],Rectangle[{1,1},{.5,.5}],Red,Rectangle[{-.25,-.25},{.25,.25}]},ImageSize->30]:>(dwSetUndo[]; $dwConnectorForm = 1; dwConnector[]),
					Delimiter, Graphics[{GrayLevel[.6],Rectangle[{-1,-1},{-.5,-.5}],Rectangle[{1,-1},{.5,-.5}],Rectangle[{-1,1},{-.5,.5}],Line[{{0,0},{0,.75}}],Line[{{-.75,.75},{.75,.75}}],Line[{{0,0},{0,-.75}}],Line[{{-.75,-.75},{.75,-.75}}],Black,CapForm["Butt"],Line[{{0,0},{0,.75},{.5,.75}}],Rectangle[{1,1},{.5,.5}],Red,Rectangle[{-.25,-.25},{.25,.25}]},ImageSize->30]:>(dwSetUndo[]; $dwConnectorForm = 2; dwConnector[]),
					Delimiter, Graphics[{GrayLevel[.6],Rectangle[{-1,-1},{-.5,-.5}],Rectangle[{1,-1},{.5,-.5}],Rectangle[{-1,1},{-.5,.5}],Line[{{0,0},{.75,0}}],Line[{{.75,-.75},{.75,.75}}],Line[{{0,0},{-.75,0}}],Line[{{-.75,-.75},{-.75,.75}}],Black,CapForm["Butt"],Line[{{0,0},{.75,0},{.75,.5}}],Rectangle[{1,1},{.5,.5}],Red,Rectangle[{-.25,-.25},{.25,.25}]},ImageSize->30]:>(dwSetUndo[]; $dwConnectorForm = 3; dwConnector[]),
					Delimiter, Graphics[{GrayLevel[.6],Rectangle[{-1,-1.5},{-.5,-1}],Rectangle[{.5,-1.5},{1,-1}],Rectangle[{-1,1.5},{-.5,1}],Line[{{0,-.625},{0,0.625}}],Line[{{-.75,1},{-.75,.625},{.75,.625},{.75,1}}],Line[{{-.75,-1},{-.75,-.625},{.75,-.625},{.75,-1}}],Black,CapForm["Butt"],Line[{{0,0},{0,.625},{.75,.625},{.75,1}}],Rectangle[{1,1.5},{.5,1}],Red,Rectangle[{-.25,-.25},{.25,.25}]},AspectRatio->1,ImageSize->30]:>(dwSetUndo[]; $dwConnectorForm = 5; dwConnector[]),
					Delimiter, Graphics[{GrayLevel[.6],Rectangle[{-1.5,-1},{-1,-.5}],Rectangle[{1,-1},{1.5,-.5}],Rectangle[{-1.5,1},{-1,.5}],Line[{{-.625,0},{.625,0}}],Line[{{1,.75},{.625,.75},{.625,-.75},{1,-.75}}],Line[{{-1,.75},{-.625,.75},{-.625,-.75},{-1,-.75}}],Black,CapForm["Butt"],Line[{{0,0},{.625,0},{.625,.75},{1,.75}}],Rectangle[{1,1},{1.5,.5}],Red,Rectangle[{-.25,-.25},{.25,.25}]},AspectRatio->1,ImageSize->30]:>(dwSetUndo[]; $dwConnectorForm = 4; dwConnector[])
				}, $dwButtonStyle],
			"Create connector for two selected objects in order of selection", TooltipDelay->$dwTooltipDelay],
			
			(* align *)
			Tooltip[
				ActionMenu[Pane[$dwIconAlign],
				{
					"Center":>(dwSetUndo[]; dwAlign["Alignment"->"Horizontal", "SetUndo"->False]; dwAlign["Alignment"->"Vertical", "SetUndo"->False]),
					"Left":>(dwAlign["Alignment"->"Left"]),
					"Right":>(dwAlign["Alignment"->"Right"]),
					"Bottom":>(dwAlign["Alignment"->"Bottom"]),
					"Top":>(dwAlign["Alignment"->"Top"]),
					"Horizontal center":>(dwAlign["Alignment"->"Horizontal"]),
					"Vertical center":>(dwAlign["Alignment"->"Vertical"]),
					"Balance horizontal space":>(dwAlign["Alignment"->"HorizontalSpace"]),
					"Balance vertical space":>(dwAlign["Alignment"->"VerticalSpace"]),
					"Align to grid...":>(dwAlignShapeToGridDialog[])
				},$dwButtonStyle],
			Dynamic@Row[{"Align ", If[$dwMode==="point","points","objects"], "…"}], TooltipDelay->$dwTooltipDelay],
			
			(* center objects on canvas *)
			Tooltip[Button[$dwIconCenterObject, 
				dwSetUndo[];
				dwCenterObjects[],
				$dwButtonStyle],
			"Center selected objects on canvas", TooltipDelay->$dwTooltipDelay],
			
			(* snap to grid *)
			Tooltip[Dynamic@Button[$dwIconSnapPts,
				Block[{dif},
					dwSetUndo[];
					If[$dwMode === "point",
						Do[
							If[s[[2]] != 0,
								If[$dwHead[[s[[1]]]] === BezierCurve,
									dif=If[$dwConstrainHAngle == 0,
										Round[$dwP[[Sequence@@s]],$dwGridStep]-$dwP[[Sequence@@s]],
										Nearest[
											(($dwGridSize[[1]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwP[[Sequence@@s]],$dwP[[Sequence@@s]]+$dwGridSize[[2]]}],InfiniteLine[{{0,0},$dwGridSize[[1]]}]][[1]]/($dwGridSize[[1]]+$MachineEpsilon)][[{1,1}]])
											+($dwGridSize[[2]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwP[[Sequence@@s]],$dwP[[Sequence@@s]]+$dwGridSize[[1]]}],InfiniteLine[{{0,0},$dwGridSize[[2]]}]][[1]]/($dwGridSize[[2]]+$MachineEpsilon)][[{1,1}]]))+#&/@$dwGridSnapMatrix, $dwP[[Sequence@@s]]
										][[1]]-$dwP[[Sequence@@s]]
									];
									(* move first point if it is last point and path is closed *)
									If[s[[2]] == Length[$dwP[[s[[1]]]]] - 1 && $dwP[[s[[1]], 1]] === $dwP[[s[[1]], -2]], $dwP[[s[[1]], 1]]+=dif];
									$dwP[[Sequence@@s]]+=dif;
									(* move handles if selected is main point *)
									If[Mod[s[[2]] + 2, 3] == 0,
											
										(* move handles *)
										If[s[[2]] > 1, $dwP[[s[[1]],s[[2]]-1]]+=dif];
										If[s[[2]]+1 <= Length@$dwP[[s[[1]]]], $dwP[[s[[1]],s[[2]]+1]]+=dif];
										
										(* if last point and path closed, move first handle with handles if closed path *)
										If[s[[2]] == Length[$dwP[[s[[1]]]]] - 1 && $dwP[[s[[1]], 1]] === $dwP[[s[[1]], -2]],
											$dwP[[s[[1]],2]]+=dif
										]
									],
									$dwP[[Sequence@@s]]=
										If[$dwConstrainHAngle == 0,
											Round[$dwP[[Sequence@@s]],$dwGridStep],
											Nearest[
												($dwGridSize[[1]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwP[[Sequence@@s]],$dwP[[Sequence@@s]]+$dwGridSize[[2]]}],InfiniteLine[{{0,0},$dwGridSize[[1]]}]][[1]]/($dwGridSize[[1]]+$MachineEpsilon)][[{1,1}]])
												+($dwGridSize[[2]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwP[[Sequence@@s]],$dwP[[Sequence@@s]]+$dwGridSize[[1]]}],InfiniteLine[{{0,0},$dwGridSize[[2]]}]][[1]]/($dwGridSize[[2]]+$MachineEpsilon)][[{1,1}]])+#&/@$dwGridSnapMatrix, $dwP[[Sequence@@s]]
											][[1]]
										]
								],
								
								Nothing
							],
						{s, $dwSelected}];
						dwUpdateBoundingBox[$dwSelected],
						
						(* snap objects to grid *)
						Do[
							dif = 
								If[$dwConstrainHAngle == 0,
									dwFindCenter[$dwP[[s]]] - Round[dwFindCenter[$dwP[[s]]], $dwGridStep],
									(dwFindCenter[$dwP[[s]]] - 
										(Nearest[
											Table[($dwGridSize[[1]]*IntegerPart[RegionIntersection[InfiniteLine[{#,#+$dwGridSize[[2]]}],InfiniteLine[{{0,0},$dwGridSize[[1]]}]][[1]]/($dwGridSize[[1]]+$MachineEpsilon)][[{1,1}]]) +
												($dwGridSize[[2]]*IntegerPart[RegionIntersection[InfiniteLine[{#,#+$dwGridSize[[1]]}],InfiniteLine[{{0,0},$dwGridSize[[2]]}]][[1]]/($dwGridSize[[2]]+$MachineEpsilon)][[{1,1}]]) + n, {n, $dwGridSnapMatrix}], 
											#
										][[1]])&/@{dwFindCenter[$dwP[[s]]]})[[1]]
								];
							$dwP[[s]] = (# - dif)&/@$dwP[[s]],
						{s, $dwSelected}];
						
						(* snap points to grid *)
						(*Do[
							If[$dwHead[[s]] === BezierCurve,
								Do[dif=
									If[$dwConstrainHAngle == 0,
										Round[$dwP[[s,n]],$dwGridStep]-$dwP[[s,n]],
										Nearest[
											(($dwGridSize[[1]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwP[[s,n]],$dwP[[s,n]]+$dwGridSize[[2]]}],InfiniteLine[{{0,0},$dwGridSize[[1]]}]][[1]]/($dwGridSize[[1]]+$MachineEpsilon)][[{1,1}]])
											+($dwGridSize[[2]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwP[[s,n]],$dwP[[s,n]]+$dwGridSize[[1]]}],InfiniteLine[{{0,0},$dwGridSize[[2]]}]][[1]]/($dwGridSize[[2]]+$MachineEpsilon)][[{1,1}]]))+#&/@$dwGridSnapMatrix, $dwP[[s,n]]
										][[1]]-$dwP[[s,n]]
									];
									$dwP[[s,n]]+=dif;
									If[n>1,$dwP[[s,n-1]]+=dif];
									If[Length@$dwP[[s]]>=n+1,$dwP[[s,n+1]]+=dif],
								{n,1,Length@$dwP[[s]],3}],
								$dwP[[s]]=If[$dwConstrainHAngle == 0,
									Round[#,$dwGridStep]&/@$dwP[[s]],
									Table[
										Nearest[
											($dwGridSize[[1]]*IntegerPart[RegionIntersection[InfiniteLine[{sp,sp+$dwGridSize[[2]]}],InfiniteLine[{{0,0},$dwGridSize[[1]]}]][[1]]/($dwGridSize[[1]]+$MachineEpsilon)][[{1,1}]])
											+ ($dwGridSize[[2]]*IntegerPart[RegionIntersection[InfiniteLine[{sp,sp+$dwGridSize[[1]]}],InfiniteLine[{{0,0},$dwGridSize[[2]]}]][[1]]/($dwGridSize[[2]]+$MachineEpsilon)][[{1,1}]])+#&/@$dwGridSnapMatrix, sp
										][[1]], {sp, $dwP[[s]]}]
								]
							],
						{s, $dwSelected}];*)
						
						dwUpdateBoundingBox[$dwSelected]
					]
				],$dwButtonStyle],
			Dynamic@If[$dwMode === "point",
				"Snap selected points to grid",
				"Snap selected objects to grid"
			], TooltipDelay->$dwTooltipDelay]
			
		}
	]

End[] (* End Private Context *)

EndPackage[]