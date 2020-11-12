(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwToggleCornerPt[]:=
	Block[{ma, temp, nearestPoint},
		ma = MouseAnnotation[];
		(* ma is {layer, point} *)
		If[ma =!= Null,
			(* if closed path and selection is first main point, change to last main point *)
			If[ma[[2]] === 1 && $dwP[[ma[[1]], 1]] === $dwP[[ma[[1]], -2]],
				ma[[2]] = Length[$dwP[[ma[[1]]]]] - 1
			];
			If[$dwHead[[ma[[1]]]] === BSplineCurve,
				(* duplicate single points or delete one duplicate points *)
				Which[
					(* selected first point - corner point not possible *)
					ma[[2]] == 1,
						$dwP[[ma[[1]]]] = Insert[$dwP[[ma[[1]]]], $dwP[[Sequence@@ma]], 1],
					(* previous point same as selected point *)
					$dwP[[Sequence@@ma]] === $dwP[[ma[[1]], ma[[2]] - 1]],
						$dwP[[ma[[1]]]] = Delete[$dwP[[ma[[1]]]], ma[[2]]],
					True,
						$dwP[[ma[[1]]]] = Insert[$dwP[[ma[[1]]]], $dwP[[Sequence@@ma]], ma[[2]]]
				],
				
				(* BezierCurve *)
				If[$dwHead[[ma[[1]]]] === BezierCurve,
					If[Length@$dwP[[ma[[1]]]] > If[$dwP[[ma[[1]], 1]] === $dwP[[ma[[1]], -2]], 8, 7],
						If[Mod[ma[[2]] + 2, 3] == 0,
							nearestPoint = ma[[2]];
							If[$dwP[[ma[[1]],nearestPoint]]===$dwP[[ma[[1]],nearestPoint-1]]||$dwP[[ma[[1]],nearestPoint]]===$dwP[[ma[[1]],nearestPoint+1]],
								
								(* change to smooth point *)
								temp={
									(* handle before *)
									If[4<=nearestPoint<=Length[$dwP[[ma[[1]]]]]-3,
										($dwP[[ma[[1]],nearestPoint-3]]-$dwP[[ma[[1]],nearestPoint+3]])/2,
										If[$dwP[[ma[[1]],1]]===$dwP[[ma[[1]],-1]],
											($dwP[[ma[[1]],-4]]-$dwP[[ma[[1]],4]])/2,
											($dwP[[ma[[1]],If[Mod[Length[$dwP[[ma[[1]]]]],3]==1,-4,-5]]]-$dwP[[ma[[1]],1]])/2
										]
									],
									(* handle after *)
									If[4<=nearestPoint<=Length[$dwP[[ma[[1]]]]]-3,
										($dwP[[ma[[1]],nearestPoint+3]]-$dwP[[ma[[1]],nearestPoint-3]])/2,
										If[$dwP[[ma[[1]],1]]===$dwP[[ma[[1]],-1]],
											($dwP[[ma[[1]],4]]-$dwP[[ma[[1]],-4]])/2,
											If[nearestPoint==1,
												($dwP[[ma[[1]],4]]-$dwP[[ma[[1]],-2]])/2,
												($dwP[[ma[[1]],1]]-$dwP[[ma[[1]],If[Mod[Length[$dwP[[ma[[1]]]]],3]==1,-4,-5]]])/2
											]
										]
									]
								};
								If[nearestPoint>=4,$dwP[[ma[[1]],nearestPoint-1]]=.37temp[[1]]+$dwP[[ma[[1]],nearestPoint]]];
								If[nearestPoint<=Length@$dwP[[ma[[1]]]]-1,$dwP[[ma[[1]],nearestPoint+1]]=.37temp[[2]]+$dwP[[ma[[1]],nearestPoint]]];
								If[$dwP[[ma[[1]],nearestPoint]]==$dwP[[ma[[1]],-1]],$dwP[[ma[[1]],-2]]=.37temp[[1]]+$dwP[[ma[[1]],nearestPoint]]],
								
								(* change to corner point *)
								If[nearestPoint>1,$dwP[[ma[[1]],nearestPoint-1]]=$dwP[[ma[[1]],nearestPoint]]];
								If[nearestPoint<Length@$dwP[[ma[[1]]]],$dwP[[ma[[1]],nearestPoint+1]]=$dwP[[ma[[1]],nearestPoint]]];
								If[$dwP[[ma[[1]],nearestPoint]]==$dwP[[ma[[1]],-1]],$dwP[[ma[[1]],-2]]=$dwP[[ma[[1]],nearestPoint]]];
								
								(* if closed path *)
								If[$dwP[[ma[[1]], 1]] === $dwP[[ma[[1]], -2]],
									$dwP[[ma[[1]], 2]] = $dwP[[ma[[1]], -1]]
								]
							],
							MessageDialog["Click a point not a handle."]
						],
						MessageDialog["Object does not have enough points."]
					],
					MessageDialog["Click points of BezierCurve or BSplineCurve."]
				]
			];
			dwUpdateBoundingBox[{ma[[1]]}]
		]
	]

End[] (* End Private Context *)

EndPackage[]