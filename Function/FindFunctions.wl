(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwFindPlotRange] = {"BoundaryForm"->True, "ForSetBoundary"->False, "Padding"->$dwSelectionBoundaryPad/$dwZoom};
dwFindPlotRange[layerNumbers_:{}, OptionsPattern[]]:=
	Block[{pts, a, b, c, d},
		(* collect points and discretize bezier curves *)
		pts = Flatten[
			Table[
				If[!MemberQ[$dwHideLayers, n],
					Switch[$dwHead[[n]],
						Text,
							If[$dwStyle[[n,12]] === None,
								dwTextBox[n],
								dwBSplineDiscretizeList[$dwP[[$dwStyle[[n,12]]]], 20, $dwStyle[[n,12]]]
							],
						Image,
							If[OptionValue["ForSetBoundary"],
								($dwP[[n,1]] + 4(# - $dwP[[n,1]]))&/@dwImageBox[n, "Rotate"->True],
								dwImageBox[n, "Rotate"->False]
							],
						BSplineCurve,
							dwBSplineDiscretizeList[$dwP[[n]], Max[Length[$dwP[[n]]],40]-1, n],
						_,
							If[$dwHead[[n]] === BezierCurve && Length[$dwP[[n]]] > 4,
								dwBezierDiscretizeList[$dwP[[n]], 40],
								$dwP[[n]]
							]
					],{}
				], 
			{n, layerNumbers}], 1];
		(* calculate plot range *)
		If[pts =!= {},
			{{a,b},{c,d}} = CoordinateBounds[pts, OptionValue["Padding"]];(* second input is margin size *)
			If[OptionValue["BoundaryForm"],
				If[pts=!={} && Length@pts > 1,
					{{a,c},{b,d}},
					{{-1,-1},{1,1}}
				],
				If[pts=!={} && Length@pts > 1,
					{{a,b},{c,d}},
					{{-1,1},{11,1}}
				]
			],
			$dwPlotRange
		]
	]

dwFindCenter[pts_:{{0,0}}, discretize_:None, n_:None]:=
	Block[{ptsFinal},
		Switch[Length@DeleteDuplicates[pts],
			0, {0,0},
			1, DeleteDuplicates[pts][[1]],
			2, RegionCentroid[Line[DeleteDuplicates[pts]]],
			_, ptsFinal = If[n =!= None, $dwBoundingBoxes[[n]], DeleteDuplicates[pts]];
				RegionCentroid[ConvexHullMesh[ptsFinal]](* error with self intersecting Polygon; Line and Point not accurate *)
		]
	]

(* no single points or duplicates; works best for evenly spaced points *)
dwFindCenterFast[pts_:{{{0,0},{1,1}}}]:=
	Table[Mean[CoordinateBoundingBox[pts[[n]]]], {n, Length[pts]}]

(* returns list of compound path centers; object center used if not a compound path *)
dwFindCompoundPathCenter[objectList_]:=
	Block[{cpList, final = {}},
		Do[
			cpList = If[FreeQ[$dwCompoundPathLayers, n], {n}, $dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]]];
			AppendTo[final, dwFindCenter[Join[Sequence@@Table[$dwP[[each]], {each, cpList}]]]],
		{n, objectList}];
		final
	]
	
dwFindSide[pts_:{{0,0}}, side_:{Left,Center}, discretize_:None, n_:None]:=
	Block[{min, max, ptsFinal},
		Switch[Length@DeleteDuplicates[pts],
			0, {0,0},
			1, DeleteDuplicates[pts][[1]],
			_,
				If[side==={0,0},
					{0,0},
					ptsFinal = If[n =!= None, $dwBoundingBoxes[[n]], DeleteDuplicates[pts]];
					min = {Min[#[[1]]&/@ptsFinal],Min[#[[2]]&/@ptsFinal]};
					max = {Max[#[[1]]&/@ptsFinal],Max[#[[2]]&/@ptsFinal]};
					If[side === {Center, Center},
						If[Length[ptsFinal] > 2, 
							RegionCentroid[ConvexHullMesh[ptsFinal]],(* error with self intersecting Polygon; Line and Point not accurate *)
							RegionCentroid[Line[ptsFinal]]
						],
						{	
							Switch[side[[1]],
								Center,
									Switch[side[[2]],
										Bottom|Top|Center, min[[1]]+(max[[1]]-min[[1]])/2,
										_, min[[2]]+(max[[2]]-min[[2]])/2
									],
								Left, min[[1]],
								Right, max[[1]],
								Bottom, min[[2]],
								Top, max[[2]]
							],
							Switch[side[[2]],
								Center,
									Switch[side[[1]],
										Bottom|Top, min[[1]]+(max[[1]]-min[[1]])/2,
										_, min[[2]]+(max[[2]]-min[[2]])/2
									],
								Left, min[[1]],
								Right, max[[1]],
								Bottom, min[[2]],
								Top, max[[2]]
							]
						}
					]
				]
		]
	]

End[] (* End Private Context *)

EndPackage[]