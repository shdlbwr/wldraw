(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwSelectionBoxTransform[]:=
	If[MemberQ[{"s1","s2","s3","s4","s5","s6","s7","s8"}, MouseAnnotation[]],

		(* scale and rotate *)
		$dwSelectionBoundaryBoxNumber =  MouseAnnotation[];
		$dwMode = "transform";
		(* remove pad from $dwSelectionBoundaryStart here since pad is needed for speed when rendering transforms *)
		$dwSelectionCenter = dwFindSide[#+{
			Switch[$dwTransformOrigin[[1]],
				Left,
					($dwSelectionBoundaryPad/$dwZoom),
				Right,
					-($dwSelectionBoundaryPad/$dwZoom),
				_,
					0
				],
			Switch[$dwTransformOrigin[[2]],
				Top,
					-($dwSelectionBoundaryPad/$dwZoom),
				Bottom,
					($dwSelectionBoundaryPad/$dwZoom),
				_,
					0
				]}&/@$dwSelectionBoundaryStart, $dwTransformOrigin];
		$dwRotateStart = ToPolarCoordinates[$dwClickPtNoGrid - $dwSelectionCenter + $MachineEpsilon][[2]]
	]
	
dwCenterObject[s_]:=
	Block[{},
		dwConvertPointSelectionToLayerSelection[];
		$dwOrigin = $dwOriginStart = {0,0};
		$dwP[[s]] = Flatten[dwCenterPoints[{$dwP[[s]]}],1]; 
		dwUpdateBoundingBox[{s}]
	]
	
dwCenterObjects[]:=
	Block[{},
		dwConvertPointSelectionToLayerSelection[];
		$dwOrigin = $dwOriginStart = If[$dwConstrainHAngle < 0, (* 3D center *) {0,0}, {0,0}];
		Do[$dwP[[s]] = Flatten[dwCenterPoints[{$dwP[[s]]}],1], {s,$dwSelected}];
		dwUpdateBoundingBox[$dwSelected];
	]

dwChangeArrow[symbol_, size_, start_, end_, g_, quantity_]:=
	Which[
		Cases[{symbol},Small|Medium|Large]=!={} && Cases[{size},Small|Medium|Large]=!={},
			{{size,end}},
		Cases[{symbol},Small|Medium|Large]=!={} && Cases[{size},Small|Medium|Large]==={},
			{{0.000001,end,g}},
		Cases[{symbol},{{-Small|-Medium|-Large,_}}|{{_?Negative,_,_}}]=!={} && Cases[{size},Small|Medium|Large]=!={},
			{{-size,start}},
		Cases[{symbol},{{-Small|-Medium|-Large,_}}|{{_?Negative,_,_}}]=!={} && Cases[{size},Small|Medium|Large]==={},
			{{-0.000001,start,g}},
		Cases[{symbol},{{Small|Medium|Large,_}}|{{_,_,_}}]=!={} && Cases[{size},Small|Medium|Large]=!={},
			{{size,end}},
		Cases[{symbol},{{Small|Medium|Large,_}}|{{_,_,_}}]=!={} && Cases[{size},Small|Medium|Large]==={},
			{{0.000001,end,g}},
		Cases[{symbol},{{_,_},{_,_}}|{{_,_,_},{_,_,_}}]=!={} && Cases[{size},Small|Medium|Large]=!={},
			{{-size,start},{size,end}},
		Cases[{symbol},{{_,_},{_,_}}|{{_,_,_},{_,_,_}}]=!={} && Cases[{size},Small|Medium|Large]==={},
			{{-0.000001,start,g},{0.000001,end,g}},
		Cases[{symbol},{{_,_},{_,_},___}|{{_,_,_},{_,_,_},___}]=!={} && Cases[{size},Small|Medium|Large]=!={},
			Table[{size, start + (end-start)*(n/quantity)}, {n, quantity}],
		Cases[{symbol},{{_,_},{_,_},___}|{{_,_,_},{_,_,_},___}]=!={} && Cases[{size},Small|Medium|Large]==={},
			Table[{0.000001, start + (end-start)*(n/quantity), g}, {n, quantity}],
		True,
			{}
	]

dwConstrainAngleMenu[]:= ActionMenu[Dynamic@If[$dwConstrainHAngle < 0, "3D", $dwConstrainHAngle], Join[{"3D":>(dwUpdateAxonometricAngles[]; dwSetGridSize[]),Delimiter}, Table[With[{n=n}, n:>($dwConstrainHAngle = n; $dwConstrainVAngle = n; dwSetGridSize[])], {n, 0, 90, 5}]], Appearance->"PopupMenu"]

Options[dwCalculatePathPositionByObjectSize] = {"Position"->0.5, "ObjectDirection"->1, (* -1 to reverse object direction *) "ObjectSizeInPrinterPoints"->5};
dwCalculatePathPositionByObjectSize[n_:{}, OptionsPattern[]]:=
	Block[{sum = 0, pts, pos, dir, size},
		{pos, dir, size} = {OptionValue["Position"], OptionValue["ObjectDirection"], OptionValue["ObjectSizeInPrinterPoints"]};
		If[pts =!= {},
			(* get points *)
			pts = If[MemberQ[{BezierCurve, BSplineCurve}, $dwHead[[n]]], dwDiscretizeCurveByNamedSelection[n], $dwP[[n]]];
			(* calculate distance *)
			Do[sum += EuclideanDistance[pts[[p]], pts[[p+1]]] ,{p, Length[pts]-1}];
			(* calculate position *)
			pos + dir(.08size)*(.1/(sum+$MachineEpsilon))
		]
	]
	
dwCenterPoints[pts_:{{}}]:=
	Block[{ctr},
		Table[
			ctr=dwFindCenter[If[$dwTransformEach, pts[[n]], $dwSelectionBoundary]];
			#-ctr&/@pts[[n]],
		{n, Length@pts}]
	]

Options[dwRoundCorners] = {"Closed"->True};
dwRoundCorners[pts_, radius_, OptionsPattern[]]:=
	Block[{finalPts},
		finalPts = 
			Flatten[{
				Table[{
					pts[[n]]+1.5(radius/EuclideanDistance[pts[[If[n==1,-1,n-1]]],pts[[n]]])(pts[[If[n==1,-1,n-1]]]-pts[[n]]),
					pts[[n]]+(radius/EuclideanDistance[pts[[If[n==1,-1,n-1]]],pts[[n]]])(pts[[If[n==1,-1,n-1]]]-pts[[n]]),
					pts[[n]]+.5(radius/EuclideanDistance[pts[[If[n==1,-1,n-1]]],pts[[n]]])(pts[[If[n==1,-1,n-1]]]-pts[[n]]),
					pts[[n]]+.5(radius/EuclideanDistance[pts[[If[n==Length[pts],1,n+1]]],pts[[n]]])(pts[[If[n==Length[pts],1,n+1]]]-pts[[n]]),
					pts[[n]]+(radius/EuclideanDistance[pts[[If[n==Length[pts],1,n+1]]],pts[[n]]])(pts[[If[n==Length[pts],1,n+1]]]-pts[[n]]),
					pts[[n]]+1.5(radius/EuclideanDistance[pts[[If[n==Length[pts],1,n+1]]],pts[[n]]])(pts[[If[n==Length[pts],1,n+1]]]-pts[[n]])},
				{n, Length[pts]}]
			},2][[2;;-1]];
		If[OptionValue["Closed"],
			finalPts,
			finalPts = Join[pts[[{1}]], pts[[{1}]], finalPts[[6;;-7]], pts[[{-1}]], pts[[{-1}]], pts[[{-1}]]]
		]
	]
	
dwSetGridSize[]:= 
	Block[{r = RotationTransform[$dwConstrainHAngle \[Degree]]},
		$dwGridSize = 
			Which[
				$dwConstrainHAngle == 0,
					{{$dwGridStep, 0}, {0, $dwGridStep}},
				$dwConstrainHAngle < 0,
					Join[
						-(dwAxo[{{{0, 0}, {$dwGridStep, 0}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1, {2}]])/.{Indeterminate->0.},
						(dwAxo[{{{0, 0}, {$dwGridStep, 0}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Right"][[1, {2}]])/.{Indeterminate->0.},
						(dwAxo[{{{0, 0}, {0, $dwGridStep}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1, {2}]])/.{Indeterminate->0.}],
				True,
					{r[{$dwGridStep, 0}], r[{0, $dwGridStep}]}
			];
		$dwGridSnapMatrix = Flatten[Table[# + y*$dwGridSize[[2]]&/@Flatten[Table[x*-#, {x, -1, 1}]&/@$dwGridSize[[{1}]], 1], {y, -1, 1}], 1];
	]

End[] (* End Private Context *)

EndPackage[]