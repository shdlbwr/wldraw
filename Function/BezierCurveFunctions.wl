(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(* data is list of discretized path points *)
dwBezierHandleCalculationF[t_, pts_] := 
	Sum[pts[[i + 1]] BernsteinBasis[3, i, t], {i, 0, 3}]

(* data is list of four points on a path {start point, point1, point2, end point} *)
dwBezierHandleCalculation[data_] :=
	Block[{dist, param, pts, sol, a, b, c, d},
		dist = Accumulate[Table[EuclideanDistance[data[[i]], data[[i + 1]]], {i, 3}]];
		param = Prepend[dist/Last[dist], 0];
		pts = {First[data], {a, b}, {c, d}, Last[data]};
		sol = Quiet@Solve[{dwBezierHandleCalculationF[param[[2]], pts] == data[[2]], dwBezierHandleCalculationF[param[[3]], pts] == data[[3]]}, {a, b, c, d}];
		If[Dimensions[sol] === {1,4}, N[pts/.sol], {}]
	]
	
dwAddBezierCurvePoint[path_, newPtPosList_]:=
	Block[{finalPath = path, nearestPoint, nearestPointNum, disretizedPts, disretizedPtsNum, newHandles1, newHandles2, handleBefore, handleAfter, mainPtBeforeNum, newMainPtNum},
		
		(* discretize entire path; find nearest point to mouse position; find control points for path between first main point before and new point then new point and first main point after *)
		disretizedPts = dwBezierDiscretizeList[path, 16*$dwDiscretizeResolution]; (* increase multiplier to increase number of discretize points *)
		
		Do[
			nearestPointNum = Flatten[Position[disretizedPts, Nearest[disretizedPts, newPtPosList[[npp]]][[1]]]][[1]];
			nearestPoint = disretizedPts[[nearestPointNum]];
			disretizedPtsNum = Union[Flatten[Position[Round[Chop[disretizedPts],.0001],Round[Chop[#],.0001]]&/@path[[1;;-1;;3]]]];
			
			If[FreeQ[disretizedPtsNum, nearestPointNum] && (disretizedPtsNum[[1]] < nearestPointNum < disretizedPtsNum[[-1]]),
				
				disretizedPtsNum = Sort[DeleteDuplicates[Join[disretizedPtsNum, {nearestPointNum}]]];
				disretizedPtsNum = Join[Table[If[disretizedPtsNum[[n]] + 1 == disretizedPtsNum[[n + 1]], Nothing, disretizedPtsNum[[n]]], {n, Length[disretizedPtsNum] - 1}], disretizedPtsNum[[{-1}]]];(* remove handles that have same position as main point *)
				newMainPtNum = Flatten[Position[disretizedPtsNum, nearestPointNum]][[1]]; (* location of new main point in main points only *)
				mainPtBeforeNum = 3*(newMainPtNum-1)-2; (* location of main point before in all path points *)
			
				(* debug - check starting values *)
				(*MessageDialog[{Length[disretizedPts], disretizedPtsNum, nearestPointNum, newMainPtNum, mainPtBeforeNum}];*)
				
				newHandles1 = dwBezierHandleCalculation[disretizedPts[[Join[{disretizedPtsNum[[newMainPtNum-1]]}, Table[disretizedPtsNum[[newMainPtNum-1]] + n*IntegerPart[Subtract[disretizedPtsNum[[newMainPtNum]], disretizedPtsNum[[newMainPtNum-1]]]/3], {n, 2}], {disretizedPtsNum[[newMainPtNum]]}]]]];
				newHandles2 = dwBezierHandleCalculation[disretizedPts[[Join[{disretizedPtsNum[[newMainPtNum]]}, Table[disretizedPtsNum[[newMainPtNum]] + n*IntegerPart[Subtract[disretizedPtsNum[[newMainPtNum+1]], disretizedPtsNum[[newMainPtNum]]]/3], {n, 2}], {disretizedPtsNum[[newMainPtNum+1]]}]]]];
				
				(* adjust new handles if not a corner point *)
				If[path[[mainPtBeforeNum]] =!= path[[mainPtBeforeNum + 1]],
					newHandles1[[1,2]] = path[[mainPtBeforeNum]] + ((path[[mainPtBeforeNum+1]] - path[[mainPtBeforeNum]])*(EuclideanDistance[newHandles1[[1,2]], path[[mainPtBeforeNum]]]/EuclideanDistance[path[[mainPtBeforeNum+1]], path[[mainPtBeforeNum]]]+$MachineEpsilon));
				];
				If[path[[mainPtBeforeNum + 2]] =!= path[[mainPtBeforeNum + 3]],
					newHandles2[[1,3]] = path[[mainPtBeforeNum+3]] + ((path[[mainPtBeforeNum+2]] - path[[mainPtBeforeNum+3]])*(EuclideanDistance[newHandles2[[1,3]], path[[mainPtBeforeNum+3]]]/EuclideanDistance[path[[mainPtBeforeNum+2]], path[[mainPtBeforeNum+3]]]+$MachineEpsilon));
				];
				
				(* new smooth point - align new point handles by averaging *)
				handleBefore = nearestPoint - (newHandles2[[1,2]] - nearestPoint)*(EuclideanDistance[nearestPoint, newHandles1[[1,3]]]/EuclideanDistance[nearestPoint, newHandles2[[1,2]]]+$MachineEpsilon);
				handleAfter = nearestPoint - (newHandles1[[1,3]] - nearestPoint)*(EuclideanDistance[nearestPoint, newHandles2[[1,2]]]/EuclideanDistance[nearestPoint, newHandles1[[1,3]]]+$MachineEpsilon);
				newHandles1[[1,3]] = newHandles1[[1,3]] + (Subtract[nearestPointNum,disretizedPtsNum[[1]]]/Subtract[Sequence@@disretizedPtsNum[[{-1,1}]]])*(handleBefore - newHandles1[[1,3]]); (* before new point *)
				newHandles2[[1,2]] = newHandles2[[1,2]] + ((Subtract[Sequence@@disretizedPtsNum[[{-1,1}]]]-Subtract[nearestPointNum,disretizedPtsNum[[1]]])/Subtract[Sequence@@disretizedPtsNum[[{-1,1}]]])*(handleAfter - newHandles2[[1,2]]); (* after new point *)
				
				If[newHandles1 =!= {} && newHandles2 =!= {},
					
					(* first part of curve *)
					finalPath = ReplacePart[finalPath, (mainPtBeforeNum+1)->newHandles1[[1,2]]]; (* replace handle after starting main point *)
					finalPath = Insert[finalPath, nearestPoint, (mainPtBeforeNum+2)]; (* add new main point *)
					finalPath = Insert[finalPath, newHandles1[[1,3]], (mainPtBeforeNum+2)]; (* add new before handle *)
					(* second part of curve *)
					finalPath = ReplacePart[finalPath, (mainPtBeforeNum+4)->newHandles2[[1,3]]]; (* replace handle before ending main point *)
					finalPath = Insert[finalPath, newHandles2[[1,2]], (mainPtBeforeNum+4)]; (* add new after handle *)
					(* if closed path the last point is same as second *)
					If[finalPath[[1]] === finalPath[[-2]],
						finalPath[[-1]] = finalPath[[2]]
					],
					
					Nothing
				],
				
				Nothing
			],
		{npp, Length[newPtPosList]}];
		
		(* return new BezierCurve points *)
		finalPath
	]

End[] (* End Private Context *)

EndPackage[]