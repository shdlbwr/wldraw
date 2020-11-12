(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(* Options "BlendObjects" and "Quantity" are for functions which need to manually provide object and quantity. Blend tool does not. *)
Options[dwBlends]={"ReturnPoints"->False, "ReverseDirection"->False, "BlendObjects"->Automatic, "Quantity"->1, "Radiate"->0};
dwBlends[resolution_:60, OptionsPattern[]]:=
	Block[{addedPts,layer1Pts,layer2Pts,layers,fillColor,edgeColor,fillOpacity,edgeOpacity,thick,gradient,linegradient,dash,ptSize,start,
		reverse, blendObjects, blendQuantity, radiate},
		{reverse, blendObjects, blendQuantity, radiate} = 
			{OptionValue["ReverseDirection"], OptionValue["BlendObjects"], OptionValue["Quantity"], OptionValue["Radiate"]};
		
		layers = If[Head[blendObjects] === List, Sort@blendObjects, Sort@$dwSelected];
		fillColor={$dwStyle[[layers[[1]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]], 
			$dwStyle[[layers[[2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,1]]};
		edgeColor={$dwStyle[[layers[[1]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]], 
			$dwStyle[[layers[[2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,1]]};
		fillOpacity={$dwStyle[[layers[[1]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]], 
			$dwStyle[[layers[[2]],Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]],1,2,1]]};
		edgeOpacity={$dwStyle[[layers[[1]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]], 
			$dwStyle[[layers[[2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]],1,2,1]]};
		thick={$dwStyle[[layers[[1]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,3,1]],
			$dwStyle[[layers[[2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,3,1]]};
		gradient={$dwStyle[[layers[[1]],8,2]],$dwStyle[[layers[[2]],8,2]]};
		linegradient={$dwLineGradients[[layers[[1]],2]],$dwLineGradients[[layers[[2]],2]]};
		dash={$dwStyle[[layers[[1]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]],
			$dwStyle[[layers[[2]],Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]]]][[1,1,4,1]]};
		dash=Switch[dash,
			{{_,_},{}}, {dash[[1]], {0, dash[[1,2]]}},
			{{},{_,_}}, {{0, dash[[2,2]]}, dash[[2]]},
			_, dash];
		ptSize={$dwStyle[[layers[[1]],Flatten[Position[$dwFullDefaultStyle, _AbsolutePointSize, Infinity]]]][[1, 1]],
			$dwStyle[[layers[[2]]]][[Flatten[Position[$dwFullDefaultStyle, _AbsolutePointSize, Infinity]]]][[1, 1]]};
		
		(* create points *)
		If[Length[$dwP[[layers[[1]]]]] == Length[$dwP[[layers[[2]]]]] && $dwHead[[layers[[1]]]] === $dwHead[[layers[[2]]]],
			
			(* SAME heads and number of points *)
			{layer1Pts, layer2Pts} = {$dwP[[layers[[1]]]], $dwP[[layers[[2]]]]},
			
			(* DIFFERENT head or number of points *)
			(* discretize *)
			{layer1Pts, layer2Pts} = 
				(*DeleteDuplicates@*)Switch[$dwHead[[#]],
					BSplineCurve,
						dwBSplineDiscretizeList[$dwP[[#]], resolution, #],(* resolution *)
					BezierCurve,
						dwBezierDiscretizeList[$dwP[[#]], resolution],(* resolution *)
					_,
						(* close Polygon - code causes continuous evaluation *)
						If[$dwHead[[#]] === Polygon && $dwP[[#]][[1]] =!= $dwP[[#]][[-1]],
							$dwP[[#]] = Join[$dwP[[#]],$dwP[[#]][[{1}]]]
						];
						If[Length@$dwP[[#]] > 1,
							
							dwAddEqualSpacedPoints[#, resolution],
							
							Nothing
						]
				]&/@{layers[[1]], layers[[2]]};
			(* equalize number of points *)
			If[Length[layer1Pts] > Length[layer2Pts],
				layer1Pts = Table[layer1Pts[[Round[n]]], {n, 1, Length[layer1Pts], Length[layer1Pts]/Length[layer2Pts]}],
				layer2Pts = Table[layer2Pts[[Round[n]]], {n, 1, Length[layer2Pts], Length[layer2Pts]/Length[layer1Pts]}]
			];
			(* set starting position *)
			start = Flatten[Position[layer1Pts, RegionNearest[Point[layer1Pts], $dwBoundingBoxes[[layers[[1]], 1]]]]][[1]];
			layer1Pts = Join[layer1Pts, layer1Pts][[start;;start + Length[layer1Pts] - 1]];
			start = Flatten[Position[layer2Pts, RegionNearest[Point[layer2Pts], $dwBoundingBoxes[[layers[[2]], 1]]]]][[1]];
			layer2Pts = Join[layer2Pts, layer2Pts][[start;;start + Length[layer2Pts] - 1]];
		];
		
		layer1Pts = If[reverse, Reverse[layer1Pts], layer1Pts];
		
		addedPts = Table[
					Table[
						layer1Pts[[pt]]+((1 - radiate) (n/(blendQuantity + 1)) + ((radiate/(blendQuantity + 2)) ((n + n^2 )/(blendQuantity + 1))))(layer2Pts[[pt]]-layer1Pts[[pt]]),
					{pt, Min[Length[layer1Pts], Length[layer2Pts]]}],
				{n,Range[blendQuantity]}];
		
		If[OptionValue["ReturnPoints"],
			
			Return[addedPts],
			
			$dwP = Join[$dwP[[1;;layers[[1]]]], addedPts, $dwP[[layers[[1]]+1;;-1]]];
			(* update global variables *)
			Do[
				$dwHead = Insert[$dwHead, If[Length[$dwP[[layers[[1]]]]] == Length[$dwP[[layers[[2]]]]] && $dwHead[[layers[[1]]]] === $dwHead[[layers[[2]]]], $dwHead[[layers[[1]]]], Polygon], layers[[1]]+n];
				$dwStyle = Insert[$dwStyle, $dwStyle[[layers[[1]]]], layers[[1]]+n];
				$dwBoundingBoxes = Insert[$dwBoundingBoxes, {}, layers[[1]]+n];
				(* blend styles *)
				$dwStyle[[layers[[1]]+n, Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]], 1, 1]] = ColorConvert[Blend[ColorConvert[fillColor,"LAB"],(n+1)/(blendQuantity+2)],"RGB"];
				$dwStyle[[layers[[1]]+n, Flatten[Position[$dwFullDefaultStyle, _FaceForm, Infinity]][[1]], 1, 2]] = Opacity[fillOpacity[[1]] + n(fillOpacity[[2]]-fillOpacity[[1]])/(blendQuantity+1)];
				$dwStyle[[layers[[1]]+n, Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]], 1, 1]] = ColorConvert[Blend[ColorConvert[edgeColor,"LAB"],(n+1)/(blendQuantity+2)],"RGB"];
				$dwStyle[[layers[[1]]+n, Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]], 1, 2]] = Opacity[edgeOpacity[[1]] + n(edgeOpacity[[2]]-edgeOpacity[[1]])/(blendQuantity+1)];
				$dwStyle[[layers[[1]]+n, Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]], 1, 3]] = AbsoluteThickness[thick[[1]] + n(thick[[2]]-thick[[1]])/(blendQuantity+1)];
				$dwStyle[[layers[[1]]+n, Flatten[Position[$dwFullDefaultStyle, _StrokeForm, Infinity]][[1]], 1, 4]] = AbsoluteDashing[dash[[1]] + n(dash[[2]]-dash[[1]])/(blendQuantity+1)];
				$dwStyle[[layers[[1]]+n, Flatten[Position[$dwFullDefaultStyle, _AbsolutePointSize, Infinity]][[1]]]] = AbsolutePointSize[ptSize[[1]] + n(ptSize[[2]]-ptSize[[1]])/(blendQuantity+1)];
				$dwStyle[[layers[[1]]+n, 8, 2]] = Table[{Mean[{$dwStyle[[layers[[1]], 8, 2, gn, 1]], $dwStyle[[layers[[2]], 8, 2, gn, 1]]}], ColorConvert[Blend[{ColorConvert[gradient[[1]][[gn]][[2]],"LAB"],ColorConvert[gradient[[2]][[gn]][[2]],"LAB"]},(n+1)/(blendQuantity+2)],"RGB"]}, {gn, Length[gradient[[1]]]}];
				(* blend animate - {pts, style, rotation, scale, peak, ramp, arrowSpeed, motion function points (always last)} *)
				$dwAnimate = Insert[$dwAnimate, $dwAnimate[[layers[[1]]]], layers[[1]]+n];
				$dwAnimate[[layers[[1]]+n, 1]] = {};
				$dwAnimate[[layers[[1]]+n, 2]] = $dwStyle[[layers[[1]]+n]];
				$dwAnimate[[layers[[1]]+n, 3]] = $dwAnimate[[layers[[1]], 3]] + n($dwAnimate[[layers[[2]]+n, 3]]-$dwAnimate[[layers[[1]], 3]])/(blendQuantity+1);
				$dwAnimate[[layers[[1]]+n, 4]] = $dwAnimate[[layers[[1]], 4]] + n($dwAnimate[[layers[[2]]+n, 4]]-$dwAnimate[[layers[[1]], 4]])/(blendQuantity+1);
				$dwAnimate[[layers[[1]]+n, 5]] = $dwAnimate[[layers[[1]], 5]] + n($dwAnimate[[layers[[2]]+n, 5]]-$dwAnimate[[layers[[1]], 5]])/(blendQuantity+1);
				$dwAnimate[[layers[[1]]+n, 6]] = $dwAnimate[[layers[[1]], 6]] + n($dwAnimate[[layers[[2]]+n, 6]]-$dwAnimate[[layers[[1]], 6]])/(blendQuantity+1);
				$dwAnimate[[layers[[1]]+n, 7]] = $dwAnimate[[layers[[1]], 7]] + n($dwAnimate[[layers[[2]]+n, 7]]-$dwAnimate[[layers[[1]], 7]])/(blendQuantity+1);
				$dwAnimate[[layers[[1]]+n, 8]] = $dwAnimate[[layers[[1]], 8]] + n($dwAnimate[[layers[[2]]+n, 8]]-$dwAnimate[[layers[[1]], 8]])/(blendQuantity+1);
				$dwAnimate[[layers[[1]]+n, 9]] = $dwAnimate[[layers[[1]], 9]] + n($dwAnimate[[layers[[2]]+n, 9]]-$dwAnimate[[layers[[1]], 9]])/(blendQuantity+1);
				$dwAnimate[[layers[[1]]+n, 10]] = $dwAnimate[[layers[[1]], 10]] + n($dwAnimate[[layers[[2]]+n, 10]]-$dwAnimate[[layers[[1]], 10]])/(blendQuantity+1);
				$dwAnimate[[layers[[1]]+n, -1]] = $dwAnimate[[layers[[1]], -1]] + n($dwAnimate[[layers[[2]]+n, -1]]-$dwAnimate[[layers[[1]], -1]])/(blendQuantity+1),
			{n, Range[blendQuantity]}];
			dwUpdateBoundingBox[Range[layers[[1]]+1, layers[[1]]+blendQuantity]];
			(* update groups *)
			$dwGroupLayers = Table[If[g2 > layers[[1]], g2+blendQuantity, g2], {g, $dwGroupLayers}, {g2, g}];
			$dwGroupLayers = If[Length[#] > 1, #, Nothing]&/@Join[$dwGroupLayers/.{layers[[1]]|(layers[[2]]+blendQuantity)->Nothing}, {Join[Range[layers[[1]] ,layers[[1]]+blendQuantity], {layers[[2]]+blendQuantity}]}];
			(* update hidden layers *)
			$dwHideLayers = If[# > layers[[1]], # + blendQuantity, #]&/@$dwHideLayers;
			(* update object gradients *)
			Do[$dwObjectGradients = Insert[$dwObjectGradients, $dwObjectGradients[[layers[[1]]]], layers[[1]]+n], {n, Range[blendQuantity]}];
			dwUpdateGradients[Range[Length[$dwP]]];
			(* update line gradients *)
			Do[$dwLineGradients = Insert[$dwLineGradients, $dwLineGradients[[layers[[1]]]], layers[[1]]+n], {n, Range[blendQuantity]}];
			Do[
				$dwLineGradients[[layers[[1]]+n, 2]] = Table[{Mean[{$dwLineGradients[[layers[[1]], 2, gn, 1]], $dwLineGradients[[layers[[2]], 2, gn, 1]]}], Blend[{ColorConvert[linegradient[[1]][[gn]][[2]],"LAB"],ColorConvert[linegradient[[2]][[gn]][[2]],"LAB"]},(n+1)/(blendQuantity+2)]}, {gn, Length[linegradient[[1]]]}],
			{n, Range[blendQuantity]}];
			(* update selections *)
			$dwSelected = Join[Range[layers[[1]],layers[[1]]+blendQuantity], {layers[[2]]+blendQuantity}];
			$dwMode=$dwCurrentPreviewMode
		];
	]
	
dwAddEqualSpacedPoints[objectNumber_:0, resolution_:60]:=
	Block[{perimeter},
		perimeter = Perimeter[Polygon[$dwP[[objectNumber]]]];
		perimeter = If[MemberQ[{0, Undefined}, perimeter],
			Total[Table[EuclideanDistance[Sequence@@$dwP[[objectNumber]][[n;;n+1]]],{n,Length[$dwP[[objectNumber]]]-1}]],
			perimeter
		];
		Flatten[
			Table[
				Join[
					{$dwP[[objectNumber,n]]},
					Table[
						$dwP[[objectNumber,n]] + 
						($dwP[[objectNumber,n+1]] - $dwP[[objectNumber,n]])(n2/Round[resolution(EuclideanDistance[Sequence@@$dwP[[objectNumber]][[n;;n+1]]]/perimeter)]), 
					{n2, 1, Round[resolution(EuclideanDistance[Sequence@@$dwP[[objectNumber]][[n;;n+1]]]/perimeter)]}], 
					{$dwP[[objectNumber,n+1]]}
				],
			{n, Length[$dwP[[objectNumber]]]-1}],
		1]
	]
	
dwAddEqualSpacedPointsObject[pts_:{}, resolution_:60]:=
	Block[{perimeter},
		perimeter = Perimeter[Polygon[pts]];
		perimeter = If[MemberQ[{0, Undefined}, perimeter],
			Total[Table[EuclideanDistance[Sequence@@pts[[n;;n+1]]],{n,Length[pts]-1}]],
			perimeter
		];
		Flatten[
			Table[
				Join[
					{pts[[n]]},
					Table[
						pts[[n]] + 
						(pts[[n+1]] - pts[[n]])(n2/Round[resolution(EuclideanDistance[Sequence@@pts[[n;;n+1]]]/perimeter)]), 
					{n2, 1, Round[resolution(EuclideanDistance[Sequence@@pts[[n;;n+1]]]/perimeter)]}], 
					{pts[[n+1]]}
				],
			{n, Length[pts]-1}],
		1]
	]

End[] (* End Private Context *)

EndPackage[]