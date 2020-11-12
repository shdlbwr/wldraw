(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
	
dwConnector[]:=
	Block[{pts1, pts2, ctr1, ctr2, a, b, resolution = 60, xminmax, yminmax, xminmax2, yminmax2, boundarySpace = .15},
		(* check selection *)
		dwConvertPointSelectionToLayerSelection[];
		If[Length[$dwSelected] < 2,
			
			MessageDialog["Select 2 objects to connect."],
			
			(* collect object points and center *)
			{pts1, pts2} = 
				Chop[
					Switch[$dwHead[[$dwSelected[[#]]]],
						Image|Text,
							$dwBoundingBoxes[[$dwSelected[[#]]]],
							BSplineCurve,
								dwBSplineDiscretizeList[$dwP[[$dwSelected[[#]]]], resolution, $dwSelected[[#]]],
							BezierCurve,
								dwBezierDiscretizeList[$dwP[[$dwSelected[[#]]]], resolution],
						_,
							$dwP[[$dwSelected[[#]]]]
					]
				]&/@{1,2};
			{pts1, pts2} = If[#[[1]] =!= #[[-1]], Join[#, {#[[1]]}], #]&/@{pts1, pts2};
			{ctr1, ctr2} = Chop[dwFindCenter[#]]&/@{pts1, pts2};
			(* check if objects are within horizontal or vertical space of each other *)
			xminmax = MinMax[#[[1]]&/@pts1];
			yminmax = MinMax[#[[2]]&/@pts1];
			xminmax2 = MinMax[#[[1]]&/@pts2];
			yminmax2 = MinMax[#[[2]]&/@pts2];
			(* create connector *)
			dwNewEmptyLayer["Head"->Arrow];
			Which[
				(* straight if vertically or horizontally aligned *)
				ctr1[[1]] === ctr2[[1]] || ctr1[[2]] === ctr2[[2]],
				
					a = RegionIntersection[Line[pts1], Line[{ctr1, ctr2}]];
					b = RegionIntersection[Line[pts2], Line[{ctr1, ctr2}]],
					
				(* straight if within vertical space *)
				MemberQ[{2,3,4}, $dwConnectorForm] && (xminmax2[[1]] >= xminmax[[1]]-boundarySpace && xminmax2[[2]] <= xminmax[[2]]+boundarySpace || xminmax[[1]] >= xminmax2[[1]]-boundarySpace && xminmax[[2]] <= xminmax2[[2]]+boundarySpace),
					a = RegionIntersection[Line[pts1], Line[{ctr1, ctr2}]];
					b = RegionIntersection[Line[pts2], Line[{ctr1, ctr2}]],
					
				(* straight if within horizontal space *)
				MemberQ[{2,3,5}, $dwConnectorForm] && (yminmax2[[1]] >= yminmax[[1]]-boundarySpace && yminmax2[[2]] <= yminmax[[2]]+boundarySpace || yminmax[[1]] >= yminmax2[[1]]-boundarySpace && yminmax[[2]] <= yminmax2[[2]]+boundarySpace),
					a = RegionIntersection[Line[pts1], Line[{ctr1, ctr2}]];
					b = RegionIntersection[Line[pts2], Line[{ctr1, ctr2}]],
				
				True,
					a = Switch[$dwConnectorForm,
							2|5,
								RegionIntersection[Line[pts1], Line[{ctr1, {ctr1[[1]], ctr2[[2]]}}]],
							3|4,
								RegionIntersection[Line[pts1], Line[{ctr1, {ctr2[[1]], ctr1[[2]]}}]],
							_,
								RegionIntersection[Line[pts1], Line[{ctr1, ctr2}]]
						];
					b = Switch[$dwConnectorForm,
							2|4,
								RegionIntersection[Line[pts2], Line[{ctr2, {ctr1[[1]], ctr2[[2]]}}]],
							3|5,
								RegionIntersection[Line[pts2], Line[{ctr2, {ctr2[[1]], ctr1[[2]]}}]],
							_,
								RegionIntersection[Line[pts2], Line[{ctr1, ctr2}]]
						]
			];
			(* use center if not a point *)
			a = If[Head[a] =!= Point, ctr1, a[[1,1]]];
			b = If[Head[b] =!= Point, ctr2, b[[1,1]]];
			(* create connector points *)
			$dwP[[-1]] = 
				Which[
					(* straight if vertically or horizontally aligned *)
					a[[1]] === b[[1]] || a[[2]] === b[[2]],
						{a, b},
					(* straight if within vertical space *)
					MemberQ[{2,3,4}, $dwConnectorForm] && (xminmax2[[1]] >= xminmax[[1]]-boundarySpace && xminmax2[[2]] <= xminmax[[2]]+boundarySpace || xminmax[[1]] >= xminmax2[[1]]-boundarySpace && xminmax[[2]] <= xminmax2[[2]]+boundarySpace),
						{a, b},
					(* straight if within horizontal space *)
					MemberQ[{2,3,5}, $dwConnectorForm] && (yminmax2[[1]] >= yminmax[[1]]-boundarySpace && yminmax2[[2]] <= yminmax[[2]]+boundarySpace || yminmax[[1]] >= yminmax2[[1]]-boundarySpace && yminmax[[2]] <= yminmax2[[2]]+boundarySpace),
						{a, b},
					True,
						(* else use form *)
						Switch[$dwConnectorForm,
							1,(* straight *)
								{a, b},
							2,(* vertical *)
								{a, {a[[1]], b[[2]]}, b},
							3,(* horizontal *)
								{a, {b[[1]], a[[2]]}, b},
							4,(* zig zag vertical *)
								{a, {a[[1]] + (b[[1]] - a[[1]])/2, a[[2]]}, {a[[1]] + (b[[1]] - a[[1]])/2, b[[2]]}, b},
							5,(* zig zag horizontal *)
								{a, {a[[1]], a[[2]] + (b[[2]] - a[[2]])/2}, {b[[1]], a[[2]] + (b[[2]] - a[[2]])/2}, b},
							_,
								$dwP[[-1]]
						]
				];
			$dwPointQuantity = Length[Flatten[$dwP, 1]];
			dwUpdateBoundingBox[{-1}];
		]
	]

End[] (* End Private Context *)

EndPackage[]