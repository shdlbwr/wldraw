(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwMouseDrag[]:=
	DynamicModule[{angle, mp, mpc, mps, snapMouseLoc, zoomStrength = 10, clickPt},
		mp = mpc = MousePosition["Graphics"];
		mps = MousePosition["GraphicsScaled"];
		If[{Head[mp],Head[mps]} === {List,List},
			(* needed to prevent anytimemove and anytimezoom from executing if moving object *)
			If[$dwMode === "move" && CurrentValue[$dwCommandKey],
				$dwClickPtScaled = mps,
				Nothing
			];
			Which[
				
				(* drag zoom - anytime the command and option keys are down *)
				CurrentValue[$dwCommandKey] && CurrentValue[$dwOptionKey],
					$dwSynchronousUpdatingInfo = False;
					$dwDisableTransforms = True;
					$dwZoom = Min[Max[$dwZoomStartValue + zoomStrength*(mps[[2]] - $dwClickPtScaled[[2]]), $dwZoomSteps[[1]]], $dwZoomSteps[[-1]]],
					
				(* drag move - anytime the command key is down *)
				CurrentValue[$dwCommandKey] &&!CurrentValue[$dwOptionKey],
					$dwSynchronousUpdatingInfo = False;
					$dwDisableTransforms = True;
					If[CurrentValue[EvaluationNotebook[], WindowSize] === {Full, Full},
						$dwWindowSize = #[[2]]&/@CurrentValue[ScreenRectangle];
						$dwOrigin = $dwOriginStart + (($dwWindowSize-{$dwToolWidth,$dwStyleHeight})/($dwOverallSize/(4/$dwZoom)))(mps - $dwClickPtScaled),
						$dwOrigin = $dwOriginStart + ((CurrentValue[EvaluationNotebook[], WindowSize]-{$dwToolWidth,$dwStyleHeight})/($dwOverallSize/(4/$dwZoom)))(mps - $dwClickPtScaled)
					],
					
				True,
					If[!$dwDisableTransforms,
						
						(* constrain drag *)
						If[CurrentValue["ShiftKey"],
							(* angle constraint *)
							clickPt = 
								If[Head[$dwSelected] === List && $dwSelected =!= {},
									If[Length[$dwSelected] == 1 && (Length[$dwSelected[[1]]] == 2 && $dwHead[[$dwSelected[[1,1]]]] === BezierCurve),
										Which[
											Mod[$dwSelected[[1,2]] + 2, 3] == 2,
												(* handle before *)
												$dwP[[$dwSelected[[1,1]], $dwSelected[[1,2]] + 1]],
											Mod[$dwSelected[[1,2]] + 2, 3] == 1,
												(* handle after *)
												$dwP[[$dwSelected[[1,1]], $dwSelected[[1,2]] - 1]],
											True,
												$dwClickPt
										],
										$dwClickPt
									],
									$dwClickPt
								];
							angle = ToPolarCoordinates[mpc - clickPt][[2]]180/Pi;
							mpc = mpc - clickPt;
							$dwMousePos = clickPt + 
								If[45 < Abs[angle +
											If[angle >= 0, -($dwConstrainHAngle),
												If[angle > ($dwConstrainHAngle) - 180, -($dwConstrainHAngle), 360 - ($dwConstrainHAngle)]
											]
										] < 135,
										
										{-(mpc[[2]])*Tan[$dwConstrainVAngle \[Degree]], mpc[[2]]},
										
										{mpc[[1]], (mpc[[1]])*Tan[$dwConstrainHAngle \[Degree]]}
								],
								
							(* no constraint *)
							$dwMousePos = mp
						];
						
						(* mode *)
						Switch[$dwMode,
							"draw",
								If[Head[$dwSelected] === List && $dwSelected =!= {}, 
									(* bezier handles only *)
									If[$dwHead[[$dwSelected[[1]]]] === BezierCurve && !CurrentValue[$dwCommandKey],(* command key checks for moving canvas, not corner point *)
										(* handle after *)
										If[$dwP[[$dwSelected[[1]],1]] == $dwP[[$dwSelected[[1]],-2]] && Length[$dwP[[$dwSelected[[1]]]]] > 2,
											$dwP[[$dwSelected[[1]],-1]] = $dwP[[$dwSelected[[1]], 1]] + (($dwMousePos - If[CurrentValue["ShiftKey"], $dwClickPt, $dwClickPtNoGrid](*$dwP[[$dwSelected[[1]], 1]]*)) *
												(
													EuclideanDistance[$dwP[[$dwSelected[[1]], 1]], $dwPStart[[$dwSelected[[1]], 2]]] /
													(EuclideanDistance[$dwP[[$dwSelected[[1]], 1]], $dwMousePos]+$MachineEpsilon)
												)),
											$dwP[[$dwSelected[[1]],-1]] = $dwP[[$dwSelected[[1]],-2]] + ($dwMousePos - If[CurrentValue["ShiftKey"], $dwClickPt, $dwClickPtNoGrid])
										];
										If[Length[$dwP[[$dwSelected[[1]]]]] > 2,
											(* handle before - move if not corner point *)
											If[!CurrentValue[$dwOptionKey],
												Which[
													$dwMouseDownOnExistingBezierPoint,
														(* existing point retains handle length *)
														$dwP[[$dwSelected[[1]],-3]] = $dwP[[$dwSelected[[1]], -2]] - (($dwMousePos - If[CurrentValue["ShiftKey"], $dwClickPt, $dwClickPtNoGrid]) *
															(
																(EuclideanDistance[$dwP[[$dwSelected[[1]], -2]], $dwPStart[[$dwSelected[[1]], -3]]] /
																 (EuclideanDistance[$dwP[[$dwSelected[[1]], -2]], $dwPStart[[$dwSelected[[1]], -1]]]+$MachineEpsilon)) *
																(EuclideanDistance[$dwP[[$dwSelected[[1]], -2]], $dwPStart[[$dwSelected[[1]], -1]]] / 
																 (EuclideanDistance[$dwP[[$dwSelected[[1]], -2]], $dwP[[$dwSelected[[1]], -1]]]+$MachineEpsilon))
															)),
													True,
														(* match after handle length *)
														$dwP[[$dwSelected[[1]],-3]] = $dwP[[$dwSelected[[1]],-2]] - ($dwMousePos - If[CurrentValue["ShiftKey"], $dwClickPt, $dwClickPtNoGrid])
												]
											];
											(* handle after - check for closed curve *)
											If[$dwP[[$dwSelected[[1]],1]] == $dwP[[$dwSelected[[1]],-2]],
												If[CurrentValue[$dwOptionKey],
													(* corner point *)
													$dwP[[$dwSelected[[1]],-1]] = $dwP[[$dwSelected[[1]],1]] - ($dwMousePos - $dwClickPtNoGrid);
													$dwP[[$dwSelected[[1]],-3]] = $dwP[[$dwSelected[[1]],-2]] - ($dwMousePos - If[CurrentValue["ShiftKey"], $dwClickPt, $dwClickPtNoGrid]);
													$dwP[[$dwSelected[[1]],-1]] = $dwP[[$dwSelected[[1]],2]],
													(* smooth point *)
													$dwP[[$dwSelected[[1]],2]] = $dwP[[$dwSelected[[1]],-1]]
												]
											]
										];
										dwUpdateBoundingBox[$dwSelected[[{1}]]]
									],
									Nothing
								],
							"preview"|"wireframe",
								If[(Head[$dwSelected] === List && $dwSelected === {}) || CurrentValue["ShiftKey"],
									$dwDragSelectEnd = If[!$dwShowMouseClickPositions, mp, $dwDragSelectEnd],
									$dwMode = "move" (* drag non-annotated unstylized graphics *)
								],
							"anytimemove"|"move",
								If[Length[Flatten[$dwP]]/2 < $dwNumPtsToReplaceObjWithBoxes,
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
									],
									Nothing
								],
							"transform",
								If[Head[$dwSelected] === List && $dwSelected =!= {},
									If[Length[Flatten[$dwP]]/2 < $dwNumPtsToReplaceObjWithBoxes,
										dwMouseDragTransformPts[$dwSelectionBoundaryBoxNumber],
										dwMouseDragTransform[$dwSelectionBoundaryBoxNumber]
									],
									Nothing
								],
							"canvas",
								If[CurrentValue[EvaluationNotebook[], WindowSize] === {Full, Full},
									$dwWindowSize = #[[2]]&/@CurrentValue[ScreenRectangle];
									$dwOrigin = $dwOriginStart + (($dwWindowSize-{$dwToolWidth,$dwStyleHeight})/($dwOverallSize/(4/$dwZoom)))(mps - $dwClickPtScaled),
									$dwOrigin = $dwOriginStart + ((CurrentValue[EvaluationNotebook[], WindowSize]-{$dwToolWidth,$dwStyleHeight})/($dwOverallSize/(4/$dwZoom)))(mps - $dwClickPtScaled)
								],
							"zoomlayer"|"zoomwireframe",
								$dwZoomEnd = mp,
							"plotrange",
								If[$dwSelectedPlotRange =!= {Null}, 
									$dwPlotRange = ReplacePart[$dwPlotRange, $dwSelectedPlotRange[[1]]->Round[mp, $dwGridStep]],
									Nothing
								],
							"point"|"pointwireframe",
								$dwMode = "pointwireframe";
								If[(Head[$dwSelected] === List && $dwSelected === {}) || $dwCurrentMouseAnnotation === Null,
									
									(* drag select *)
									$dwDragSelectEnd = mp,
									
									(* move selected points *)
									If[Head[$dwSelected] === List && $dwSelected =!= {},
										Do[
											Switch[$dwHead[[n[[1]]]],
												BezierCurve,
													dwMouseDragBezier[n],
												_,
													If[$dwConstrainHAngle == 0,
														$dwP[[Sequence@@n]] = Round[$dwMousePos, $dwGridStep] - $dwClickPt + $dwPStart[[Sequence@@n]],
														$dwP[[Sequence@@n]] = 
															Nearest[
																(($dwGridSize[[1]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwMousePos,$dwMousePos+$dwGridSize[[2]]}],InfiniteLine[{{0,0},$dwGridSize[[1]]}]][[1]]/($dwGridSize[[1]]+$MachineEpsilon)][[{1,1}]])
																+($dwGridSize[[2]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwMousePos,$dwMousePos+$dwGridSize[[1]]}],InfiniteLine[{{0,0},$dwGridSize[[2]]}]][[1]]/($dwGridSize[[2]]+$MachineEpsilon)][[{1,1}]])+#)&/@$dwGridSnapMatrix, $dwMousePos
															][[1]] - $dwClickPt + $dwPStart[[Sequence@@n]]
													];
													If[n[[2]] === Length[$dwP[[n[[1]]]]] && $dwPStart[[n[[1]],1]] === $dwPStart[[n[[1]],-1]],
														$dwP[[n[[1]],1]] = $dwP[[n[[1]],-1]]
													]
											], 
										{n, $dwSelected}]
									],
									Nothing
								],
							_,
								Nothing
						],
						Nothing
					]
			],
			
			$dwSelected = {}
		]
	]

End[] (* End Private Context *)

EndPackage[]