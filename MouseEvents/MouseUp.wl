(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwMouseUp[]:=
	If[$dwMode =!= "canvas" && $dwDisableTransforms(*(CurrentValue[$dwCommandKey] && !CurrentValue[$dwOptionKey])*),
		
		(* anytime drag move or zoom *)
		$dwOriginStart = $dwOrigin;
		$dwMode = $dwModeBeforeAction;
		$dwSynchronousUpdatingInfo = Automatic;
		dwUpdateBoundingBox[$dwSelected];
		$dwDisableTransforms = False,
		
		$dwMouseDownOnExistingBezierPoint = False;
		Which[
			
			CurrentValue[$dwCommandKey] && CurrentValue[$dwOptionKey],
				$dwMode = $dwModeBeforeAction,
				
			True,
				(* use mode *)
				Switch[$dwMode,
					"canvas",
						$dwOriginStart = $dwOrigin,
					"preview"|"wireframe",
						Block[{bounds, newObjs, temp},
							If[($dwSelected === {} || CurrentValue["ShiftKey"]) && $dwDragSelectStart =!= $dwDragSelectEnd,
								bounds = Partition[Riffle[Sequence@@CoordinateBounds[{$dwDragSelectStart,$dwDragSelectEnd}]], 2];
								newObjs = Table[If[$dwP[[n]] =!= {} && RegionWithin[Rectangle[Sequence@@bounds], Rectangle[Sequence@@Partition[Riffle[Sequence@@CoordinateBounds[$dwP[[n]]]], 2]]], n, Nothing], {n, Complement[Range[Length[$dwP]], $dwHideLayers, {}]}];
								$dwSelected = 
									If[CurrentValue["ShiftKey"],
										Complement[Join[$dwSelected, newObjs], Intersection[$dwSelected, newObjs]],
										newObjs
									];
								(* include complete compound paths *)
								temp=DeleteDuplicates[#[[1]]&/@Flatten[Table[Position[$dwCompoundPathLayers,s],{s,$dwSelected}],1]];
								$dwSelected = DeleteDuplicates[Flatten[Join[$dwSelected, $dwCompoundPathLayers[[temp]]]]];
							];
							$dwDragSelectStart = $dwDragSelectEnd = Null
						],
					"anytimemove"|"move",
						If[Length[Flatten[$dwP]]/2 < $dwNumPtsToReplaceObjWithBoxes,
							Nothing,
							If[$dwSelected =!= {},
								If[$dwConstrainHAngle == 0,
									Do[$dwP[[n]] = (Round[$dwMousePos, $dwGridStep] - $dwClickPt) + # &/@$dwPStart[[n]], {n, $dwSelected}],
									snapMouseLoc = 
										Nearest[
											($dwGridSize[[1]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwMousePos,$dwMousePos+$dwGridSize[[2]]}],InfiniteLine[{{0,0},$dwGridSize[[1]]}]][[1]]/($dwGridSize[[1]]+$MachineEpsilon)][[{1,1}]])
											+($dwGridSize[[2]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwMousePos,$dwMousePos+$dwGridSize[[1]]}],InfiniteLine[{{0,0},$dwGridSize[[2]]}]][[1]]/($dwGridSize[[2]]+$MachineEpsilon)][[{1,1}]])+#&/@$dwGridSnapMatrix, $dwMousePos
										][[1]];
									Do[$dwP[[n]] = If[$dwAxoVectorZActive,
										(* z axis*)
										{0, Round[($dwMousePos[[2]] - $dwClickPt[[2]]),$dwGridSize[[3, 2]]]} + # &/@$dwPStart[[n]],
										(* non-z axis *)
										((snapMouseLoc - $dwClickPt) + #)&/@$dwPStart[[n]]
									], {n, $dwSelected}]
								],
								Nothing
							]
						];
						$dwMode = $dwCurrentPreviewMode;
						dwUpdateBoundingBox[$dwSelected],
					"point"|"pointwireframe",
						$dwMode = "point";
						Block[{bounds,rm,newPts},
							If[$dwDragSelectStart =!= $dwDragSelectEnd,
								bounds = Partition[Riffle[Sequence@@CoordinateBounds[{$dwDragSelectStart,$dwDragSelectEnd}]], 2];
								rm = RegionMember[Rectangle[Sequence@@bounds]];
								(* remove text and images *)
								$dwPointModeSelections = Table[If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], n, Nothing], {n, $dwPointModeSelections}];
								$dwSelected = 
									If[CurrentValue["ShiftKey"],
										newPts = {$dwPointModeSelections[[#[[1]]]], #[[2]]}&/@Position[rm[Table[$dwP[[#, n]], {n, Length[$dwP[[#]]]}]]&/@$dwPointModeSelections, True];
										Complement[Join[$dwSelected, newPts], Intersection[$dwSelected, newPts]],
										{$dwPointModeSelections[[#[[1]]]], #[[2]]}&/@Position[rm[Table[$dwP[[#, n]], {n, Length[$dwP[[#]]]}]]&/@$dwPointModeSelections, True]
									];
								(* remove BezierCurve handles *)
								$dwSelected = If[$dwHead[[#[[1]]]] === BezierCurve, If[Mod[#[[2]] + 2, 3] == 0, #, Nothing], #]&/@$dwSelected,
								
								If[FreeQ[$dwSelected, 0], dwUpdateBoundingBox[$dwSelected]]
							];
							$dwDragSelectStart = $dwDragSelectEnd = Null
						],
					"splitShapeDone",
						$dwMode=$dwCurrentPreviewMode,
					"transform", 
						If[Length[Flatten[$dwP]]/2 < $dwNumPtsToReplaceObjWithBoxes,
							Nothing,
							dwMouseDragTransformPts[$dwSelectionBoundaryBoxNumber]
						];
						$dwMode = If[Length[$dwSelected[[1]]] > 1, "point", $dwCurrentPreviewMode];
						$dwSelectionBoundaryBoxNumber = Null;
						$dwRotateAngle = $dwRotateStart = 0;
						dwUpdateBoundingBox[$dwSelected],
					"zoomlayer"|"zoomwireframe",
						Which[
							CurrentValue[$dwCommandKey] && !CurrentValue[$dwOptionKey],(* moving canvas so do nothing *)
								Nothing
								(* default zoom *)
								(*$dwZoom = 1;
								$dwZoomStart = $dwZoomEnd = -$dwOriginOffset*),
							CurrentValue[$dwOptionKey] && !CurrentValue[$dwCommandKey],
								(* click zoom out *)
								$dwZoom = $dwZoomSteps[[Max[Flatten[Position[$dwZoomSteps,Nearest[$dwZoomSteps,$dwZoom][[1]]]][[1]]-1,1]]];
								$dwOrigin = $dwOriginStart = $dwZoomStart = $dwZoomEnd = -MousePosition["Graphics"] - $dwOriginOffset,
							True,
								If[EuclideanDistance[$dwZoomEnd, $dwZoomStart] < .1,
									(* click zoom in *)
									$dwZoom = $dwZoomSteps[[Min[Flatten[Position[$dwZoomSteps,Nearest[$dwZoomSteps,$dwZoom][[1]]]][[1]]+1,Length@$dwZoomSteps]]];
									$dwOrigin = $dwOriginStart = $dwZoomStart = $dwZoomEnd = -MousePosition["Graphics"] - $dwOriginOffset,
									(* drag zoom in *)
									$dwZoom = $dwCanvasMouseSpeed/Max[.5 Max[Abs[$dwZoomEnd - $dwZoomStart]], 100($dwCanvasMouseSpeed/$dwZoomMax)];
									$dwOrigin = $dwOriginStart = -dwFindCenter[{$dwZoomStart, $dwZoomEnd}] - $dwOriginOffset
								];
						];
						$dwZoomStart = $dwZoomEnd,
					_,
						$dwOriginStart = $dwOrigin
				];
				$dwCurrentMouseAnnotation = Null;
		];
		dwUpdateTransformOrigin[]
	]

End[] (* End Private Context *)

EndPackage[]