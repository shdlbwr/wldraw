(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
		

dwUpdateAllBoundingBoxes[]:=
	dwUpdateBoundingBox[Range[Length[$dwP]]]
	
dwUpdateBoundingBox[list_]:= 
	Block[{pts, a, b, c, d},
		Do[
			pts = Switch[$dwHead[[n]],
					Line|Arrow|Polygon|Point,
						$dwP[[n]],
					BezierCurve,
						If[Length[$dwP[[n]]] > 4, dwBezierDiscretizeList[$dwP[[n]], 20], $dwP[[n]]],
					BSplineCurve,
						If[Length[$dwP[[n]]] > 3, dwBSplineDiscretizeList[$dwP[[n]], Max[Length[$dwP[[n]]],20]-1, n], $dwP[[n]]],
					"Text3D",
						dwText3DBox[n],
					Text,
						If[$dwStyle[[n,12]] === None,
							dwTextBox[n],
							dwBSplineDiscretizeList[$dwP[[$dwStyle[[n,12]]]], 20, $dwStyle[[n,12]]]
						],
					Image,
						($dwP[[n,1]] + 4(# - $dwP[[n,1]]))&/@dwImageBox[n],
					"Expression",
						($dwP[[n,1]] + 4(# - $dwP[[n,1]]))&/@dwExpressionBox[n],
					Point,
						Switch[Length[$dwP[[n]]],
							0, {{0,0}},
							1, Table[$dwP[[n,1]] + side*.00265Max[$dwStyle[[n,7]], 4], {side, {{1,1}, {-1,1}, {-1,-1}, {1,-1}}}],
							_, $dwP[[n]]
						],
					_,
						{}
				];
			If[pts =!= {},
				{{a,c},{b,d}} = CoordinateBounds[pts,0];(* second input is margin size *)
				$dwBoundingBoxes[[n]] = {{a,b},{c,b},{c,d},{a,d}}
			],
		{n, If[Head[#] === List, #[[1]], #]&/@list}]
	]

dwUpdateGradients[objNumList_]:= 
	Do[$dwObjectGradients[[n]] = 
		If[MemberQ[{Image,Text}, $dwHead[[n]]],
			{},
			If[$dwStyle[[n,8,1]] =!= None,
				dwRenderGradient[n],
				{}
			]
		], 
	{n, objNumList}]
	
(*
	Finds layer numbers in reordered index:
		orderedList is the list to be updated: {1,2,...} or {{1,2,...},{3,4,...}}
		layers is the layers to move: {1,2,...}
		newLayer is the new position for layers: 1
		updated list is returned: {1,2,...} or {{1,2,...},{3,4,...}} 
*)

Options[dwUpdateSelected] = {"StylesToUpdate"->All};
dwUpdateSelected[sel_,OptionsPattern[]]:=
	Block[{update = OptionValue["StylesToUpdate"]},
		Do[
			Which[
				SubsetQ[$dwShapeSymbols, {$dwHead[[s]], $dwHead[[sel]]}],
					Switch[update,
						"showFill",
							$dwStyle[[s,1]] = $dwStyle[[sel,1]],
						"fillColor",
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]],1,1]] = $dwStyle[[sel,Flatten[Position[$dwStyle[[sel]], FaceForm[_]]][[1]],1,1]];
							dwUpdateGradients[{s}],
						"animateFillColor",
							$dwAnimate[[s,2,Flatten[Position[$dwAnimate[[s,2]], FaceForm[_]]][[1]],1,1]] = $dwAnimate[[sel,2,Flatten[Position[$dwAnimate[[sel,2]], FaceForm[_]]][[1]],1,1]],
						"fillOpacity",
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]],1,2,1]] = $dwStyle[[sel,Flatten[Position[$dwStyle[[sel]], FaceForm[_]]][[1]],1,2,1]],
						"strokeColor",
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,1]] = $dwStyle[[sel,Flatten[Position[$dwStyle[[sel]], StrokeForm[_]]][[1]],1,1]],
						"animateStrokeColor",
							$dwAnimate[[s,2,Flatten[Position[$dwAnimate[[s,2]], StrokeForm[_]]][[1]],1,1]] = $dwAnimate[[sel,2,Flatten[Position[$dwAnimate[[sel,2]], StrokeForm[_]]][[1]],1,1]],
						"strokeOpacity",
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,2,1]] = $dwStyle[[sel,Flatten[Position[$dwStyle[[sel]], StrokeForm[_]]][[1]],1,2,1]],
						"strokeThickness",
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,3,1]] = $dwStyle[[sel,Flatten[Position[$dwStyle[[sel]], StrokeForm[_]]][[1]],1,3,1]],
						"strokeDashing",
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,4,1]] = $dwStyle[[sel,Flatten[Position[$dwStyle[[sel]], StrokeForm[_]]][[1]],1,4,1]],
						"strokeCapForm",
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,5,1]] = $dwStyle[[sel,Flatten[Position[$dwStyle[[sel]], StrokeForm[_]]][[1]],1,5,1]],
						"strokeJoinForm",
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]],1,6,1]] = $dwStyle[[sel,Flatten[Position[$dwStyle[[sel]], StrokeForm[_]]][[1]],1,6,1]],
						"lineGradient",
							$dwLineGradients[[s]] = $dwLineGradients[[sel]],
						"pathClosed",
							$dwStyle[[s,10]] = $dwStyle[[sel,10]],
						"pathDegree",
							$dwStyle[[s,11]] = $dwStyle[[sel,11]],
						"showArrows",
							$dwStyle[[s,2]] = $dwStyle[[sel,2]],
						"arrowheadSize",
							$dwStyle[[s,3]] = $dwStyle[[sel,3]];
							$dwStyle[[s,6]] = $dwStyle[[sel,6]];
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], Arrowheads[_]]][[1]],1]] = dwChangeArrow[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], Arrowheads[_]]][[1]],1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]],
						"arrowheadStart",
							$dwStyle[[s,4]] = $dwStyle[[sel,4]];
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], Arrowheads[_]]][[1]],1]] = dwChangeArrow[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], Arrowheads[_]]][[1]],1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]],
						"arrowheadEnd",
							$dwStyle[[s,5]] = $dwStyle[[sel,5]];
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], Arrowheads[_]]][[1]],1]] = dwChangeArrow[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], Arrowheads[_]]][[1]],1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]],
						"arrowheadPosition",
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], Arrowheads[_]]][[1]],1]] = dwChangeArrow[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], Arrowheads[_]]][[1]],1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]],
						"pointColor",
							$dwStyle[[s,$dwStyleStart]] = $dwStyle[[sel,$dwStyleStart]],
						"pointOpacity",
							$dwStyle[[s,$dwStyleStart+1,1]] = $dwStyle[[sel,$dwStyleStart+1,1]],
						"pointSize",
							$dwStyle[[s,7]] = $dwStyle[[sel,7]];
							$dwStyle[[s,Flatten[Position[$dwStyle[[s]], AbsolutePointSize[_]]][[1]],1]] = $dwStyle[[sel,Flatten[Position[$dwStyle[[sel]], AbsolutePointSize[_]]][[1]],1]],
						_,
							(* entire style *)
							$dwStyle[[s]] = $dwStyle[[sel]];
							$dwLineGradients[[s]] = $dwLineGradients[[sel]];
							dwUpdateGradients[{s}]
					],
				SubsetQ[{Text}, {$dwHead[[s]], $dwHead[[sel]]}],
					Switch[update,
						"fontSize",
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], FontSize]][[1]]]][[2]] = $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontSize]][[1]]]][[2]];
							dwUpdateBoundingBox[{s}],
						"fontLeading",
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], LineSpacing]][[1]]]][[2]] = $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], LineSpacing]][[1]]]][[2]];
							dwUpdateBoundingBox[{s}],
						"fontColor",
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], FontColor]][[1]]]][[2]] = $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontColor]][[1]]]][[2]],
						"fontFamily",
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], FontFamily]][[1]]]][[2]] = $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontFamily]][[1]]]][[2]];
							dwUpdateBoundingBox[{s}],
						"fontOpacity",
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], FontOpacity]][[1]]]][[2]] = $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontOpacity]][[1]]]][[2]],
						"fontAlignment",
							$dwStyle[[s, 3]] = $dwStyle[[sel, 3]];
							dwUpdateBoundingBox[{s}],
						"fontWeight",
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], FontWeight]][[1]]]][[2]] = $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontWeight]][[1]]]][[2]];
							dwUpdateBoundingBox[{s}],
						"fontSlant",
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], FontSlant]][[1]]]][[2]] = $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontSlant]][[1]]]][[2]];
							dwUpdateBoundingBox[{s}],
						"fontTracking",
							$dwStyle[[s, Flatten[Position[$dwStyle[[s]], FontTracking]][[1]]]][[2]] = $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontTracking]][[1]]]][[2]];
							dwUpdateBoundingBox[{s}],
						_,
							$dwStyle[[s]][[4;;-1]] = $dwStyle[[sel]][[4;;-1]]
					],
				SubsetQ[{Image}, {$dwHead[[s]], $dwHead[[sel]]}],
					Switch[update,
						_,
							$dwStyle[[s]][[4;;5]] = $dwStyle[[sel]][[4;;5]]
					],
				True,
					Nothing
			],
		{s, Union[Flatten[
			If[MemberQ[Flatten[$dwCompoundPathLayers], #],
				$dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, #]][[1]]]],
				#
			]&/@If[$dwSelected === {}, {sel}, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]]]]}];
		dwUpdateBoundingBox[Union[Flatten[
			If[MemberQ[Flatten[$dwCompoundPathLayers], #],
				$dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, #]][[1]]]],
				#
			]&/@If[$dwSelected === {}, {sel}, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]]]]]
	]
	
Options[dwUpdateOrderOfLayersAfterMove] = {"ReturnUpdatedList"->True};
dwUpdateOrderOfLayersAfterMove[orderedListOfAllLayers_:{}, layersToMove_:{}, moveToLayer_:0, OptionsPattern[]]:=
	Block[{neworder, layerSorted, layerDelete},
		Which[
			orderedListOfAllLayers === {},
				{},
			layersToMove === {},
				orderedListOfAllLayers,
			moveToLayer != 0,
				layerSorted = Sort[DeleteDuplicates[layersToMove]];
				layerDelete = List/@Table[If[n >= moveToLayer, n + 1, n],{n, layerSorted}];
				neworder=Flatten[Delete[Insert[Range[Length[$dwP]], layerSorted, moveToLayer + If[moveToLayer > Max[layerSorted], 1, 0]], layerDelete]];
				If[OptionValue["ReturnUpdatedList"],
					If[Head[orderedListOfAllLayers[[1]]] === List,
						Table[Flatten[Position[neworder,n2]][[1]], {n, Length[orderedListOfAllLayers]}, {n2, orderedListOfAllLayers[[n]]}],
						Flatten[Table[Position[neworder,n], {n, orderedListOfAllLayers}]]
					],
					neworder
				],
			True,
				orderedListOfAllLayers
		]
	]
	
dwUpdateTextOnCurveAfterDelete[deletedLayer_]:=
	Do[
		Which[
			$dwStyle[[n,12]] > deletedLayer,
				$dwStyle[[n,12]] -= 1,
			$dwStyle[[n,12]] === deletedLayer,
				$dwStyle[[n,12]] = None,
			True,
				Nothing
		], 
	{n, Flatten[Position[$dwHead, Text, Infinity]]}]
	
dwUpdateGroupLayersAfterDelete[deletedLayer_]:=
	$dwGroupLayers = 
		If[Length[#] < 2, Nothing, #]&/@Table[Table[
			Which[
				$dwGroupLayers[[gln,gln2]] > deletedLayer,
					$dwGroupLayers[[gln,gln2]] - 1,
				$dwGroupLayers[[gln,gln2]] === deletedLayer,
					Nothing,
				True,
					$dwGroupLayers[[gln,gln2]]
			],
		{gln2, Length[$dwGroupLayers[[gln]]]}], {gln,Length[$dwGroupLayers]}]
	
dwUpdateHideLayersAfterDelete[deletedLayer_]:=
	$dwHideLayers = 
		Table[
			Which[
				$dwHideLayers[[hn]] > deletedLayer, 
					$dwHideLayers[[hn]] - 1, 
				$dwHideLayers[[hn]] === deletedLayer, 
					Nothing, 
				True, 
					$dwHideLayers[[hn]]
			], 
		{hn, Length[$dwHideLayers]}]

End[] (* End Private Context *)

EndPackage[]