(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwMouseDragBezier[n_]:=
	If[$dwSelected =!= {} && (Length[$dwSelected] > 1 || Mod[n[[2]] + 2, 3] == 0),

		(* ---------- MAIN POINT or MULTI POINT/HANDLE DRAG ---------- *)
		
		(* main point *)
		$dwP[[Sequence@@n]] =
			If[$dwConstrainHAngle == 0 || Total[Flatten@$dwGridSize] < 2*$dwGridStepNone,
				Round[$dwMousePos, $dwGridStep] - $dwClickPt + $dwPStart[[Sequence@@n]],
				Nearest[
					((($dwGridSize[[1]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwMousePos,$dwMousePos+$dwGridSize[[2]]}],InfiniteLine[{{0,0},$dwGridSize[[1]]}]][[1]]/($dwGridSize[[1]]+$MachineEpsilon)][[{1,1}]])
					+($dwGridSize[[2]]*IntegerPart[RegionIntersection[InfiniteLine[{$dwMousePos,$dwMousePos+$dwGridSize[[1]]}],InfiniteLine[{{0,0},$dwGridSize[[2]]}]][[1]]/($dwGridSize[[2]]+$MachineEpsilon)][[{1,1}]])+#))&/@$dwGridSnapMatrix, $dwMousePos
				][[1]] - $dwClickPt + $dwPStart[[Sequence@@n]]
			];
		(* handle before - linear movement *)
		If[n[[2]] > 1 && Mod[n[[2]] + 2, 3] == 0, 
			$dwP[[n[[1]], n[[2]]-1]] = 
				If[$dwConstrainHAngle == 0,
					Round[$dwMousePos, $dwGridStep] - $dwClickPt + $dwPStart[[n[[1]], n[[2]]-1]],
					$dwP[[Sequence@@n]] + ($dwPStart[[n[[1]], n[[2]]-1]] - $dwPStart[[Sequence@@n]])
				]
		];
		(* handle after - linear movement *)
		If[n[[2]]+1 <= Length[$dwP[[n[[1]]]]] && Mod[n[[2]] + 2, 3] == 0, 
			$dwP[[n[[1]], n[[2]]+1]] = 
				If[$dwConstrainHAngle == 0,
					Round[$dwMousePos, $dwGridStep] - $dwClickPt + $dwPStart[[n[[1]], n[[2]]+1]],
					$dwP[[Sequence@@n]] + ($dwPStart[[n[[1]], n[[2]]+1]] - $dwPStart[[Sequence@@n]])
				]
		];
		
		(* closed curve first point is same as second to last point and second point is same as last point *)
		If[Length[$dwP[[n[[1]]]]] > 5 && (EuclideanDistance[$dwPStart[[n[[1]],1]], $dwPStart[[n[[1]],-2]]] <= Max[$dwGridStep, $dwMinClosedBezierDistance/$dwZoom] || 
			EuclideanDistance[$dwP[[n[[1]],1]], $dwP[[n[[1]],-2]]] <= Max[$dwGridStep, $dwMinClosedBezierDistance/$dwZoom]),
			$dwP[[n[[1]],1]] = $dwP[[n[[1]],-2]];
			$dwP[[n[[1]],2]] = $dwP[[n[[1]],-1]];
		],
		
		(* ---------- SINGLE HANDLE DRAG ---------- *)
		
		(* handle *)
		$dwP[[Sequence@@n]] = $dwMousePos;
			
		(* opposite handle *)
		If[!CurrentValue[$dwOptionKey] && Length[$dwP[[n[[1]]]]] > 2 && n[[2]] > 2,
			If[Mod[n[[2]], 3] == 0,
				
				(* if dragging smooth before handle move after handle *)
				If[Abs[Subtract[Sequence@@(Max[Min[Divide[#[[1]], #[[2]] + $MachineEpsilon],1000],-1000]&/@{$dwPStart[[Sequence@@n]] - $dwPStart[[n[[1]], n[[2]] + 1]], $dwPStart[[Sequence@@n]] - $dwPStart[[n[[1]], n[[2]] + 2]]})]] < .1,
					$dwP[[n[[1]], n[[2]]+2]] = 
						$dwP[[n[[1]], n[[2]]+1]] - (($dwP[[Sequence@@n]] - $dwP[[n[[1]], n[[2]]+1]]) *
						(
							(EuclideanDistance[$dwP[[n[[1]], n[[2]]+1]], $dwPStart[[n[[1]], n[[2]]+2]]] /
							 (EuclideanDistance[$dwP[[n[[1]], n[[2]]+1]], $dwPStart[[Sequence@@n]]]+$MachineEpsilon)) *
							(EuclideanDistance[$dwP[[n[[1]], n[[2]]+1]], $dwPStart[[Sequence@@n]]] / 
							 (EuclideanDistance[$dwP[[n[[1]], n[[2]]+1]], $dwP[[Sequence@@n]]]+$MachineEpsilon))
						)),
					
					Nothing
				],
					
				(* else move smooth before handle *)
				If[Abs[Subtract[Sequence@@(Max[Min[Divide[#[[1]], #[[2]] + $MachineEpsilon],1000],-1000]&/@{$dwPStart[[Sequence@@n]] - $dwPStart[[n[[1]], n[[2]] - 1]], $dwPStart[[Sequence@@n]] - $dwPStart[[n[[1]], n[[2]] - 2]]})]] < .1,
					$dwP[[n[[1]], n[[2]]-2]] = 
						$dwP[[n[[1]], n[[2]]-1]] - (($dwP[[Sequence@@n]] - $dwP[[n[[1]], n[[2]]-1]]) *
						(
							(EuclideanDistance[$dwP[[n[[1]], n[[2]]-1]], $dwPStart[[n[[1]], n[[2]]-2]]] /
							 (EuclideanDistance[$dwP[[n[[1]], n[[2]]-1]], $dwPStart[[Sequence@@n]]]+$MachineEpsilon)) *
							(EuclideanDistance[$dwP[[n[[1]], n[[2]]-1]], $dwPStart[[Sequence@@n]]] / 
							 (EuclideanDistance[$dwP[[n[[1]], n[[2]]-1]], $dwP[[Sequence@@n]]]+$MachineEpsilon))
						)),
					Nothing
				]
			]
		];
		
		(* if closed curve, second point is same as last point *)
		If[$dwP[[n[[1]],1]] == $dwP[[n[[1]],-2]],
			$dwP[[n[[1]],2]] = $dwP[[n[[1]],-1]]
		]
	]

End[] (* End Private Context *)

EndPackage[]