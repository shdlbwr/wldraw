(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwMouseDragTransform[n_]:=
	DynamicModule[{rotate, scale},
		If[MemberQ[{"s1","s3","s5","s7"}, n],
			
			(* rotate *)
			$dwRotateAngle = Round[ToPolarCoordinates[MousePosition["Graphics"] - $dwSelectionCenter + $MachineEpsilon][[2]] - $dwRotateStart, Pi/(180/If[CurrentValue["ShiftKey"], 0.5, $dwRotateStep])];
			rotate = RotationTransform[$dwRotateAngle, $dwSelectionCenter];
			
			$dwSelectionBoundary = rotate[$dwSelectionBoundaryStart],
		
			(* scale *)
			$dwScale = Round[Abs[(MousePosition["Graphics"] - $dwSelectionCenter)]/Abs[($dwClickPtNoGrid - $dwSelectionCenter + $MachineEpsilon)], If[CurrentValue["ShiftKey"], 1, $dwScaleStep]/100];
			$dwScale = 
				If[MemberQ[{"s2","s6"}, n], 
					{If[CurrentValue[$dwOptionKey], 1, $dwScale[[2]]], $dwScale[[2]]}, 
					{$dwScale[[1]], If[CurrentValue[$dwOptionKey], 1, $dwScale[[1]]]}
				];
			scale = ScalingTransform[$dwScale, $dwSelectionCenter];
			
			$dwSelectionBoundary = scale[$dwSelectionBoundaryStart]
		]
		
	]

dwMouseDragTransformPts[n_]:=
	DynamicModule[{rotate, scale, eachTransform, handles},
		If[MemberQ[{"s1","s3","s5","s7"}, n],
			
			(* rotate *)
			$dwRotateAngle = Round[ToPolarCoordinates[MousePosition["Graphics"] - $dwSelectionCenter + $MachineEpsilon][[2]] - $dwRotateStart, Pi/(180/If[CurrentValue["ShiftKey"], 0.5, $dwRotateStep])];
			rotate = RotationTransform[$dwRotateAngle, $dwSelectionCenter];
			If[$dwTransformEach === True,
				eachTransform = Table[RotationTransform[$dwRotateAngle, pn], {pn, $dwTransformEachCenter}];
				
				If[Length[$dwSelected[[1]]] > 1,
					
					(* point *)
					Do[
						$dwP[[Sequence@@$dwSelected[[pn]]]] = eachTransform[[pn]][$dwPStart[[Sequence@@$dwSelected[[pn]]]]],
					{pn, Length[$dwSelected]}];
					(* gather BezierCurve handles *)
					handles = 
						DeleteDuplicates[Flatten[Table[
							If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[pn,1]]]]],
								{{$dwSelected[[pn,1]], $dwSelected[[pn,2]]-1}, {$dwSelected[[pn,1]], $dwSelected[[pn,2]]+1}},
								Nothing
							],
						{pn, Length[$dwSelected]}], 1]];
					(* add first point and BezierCurve handle if last point selected *)
					handles = 
						DeleteDuplicates[Join[handles,
							Flatten[Table[
								If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[pn,1]]]]] && ($dwSelected[[pn, 2]] == Length[$dwP[[$dwSelected[[pn, 1]]]]]-1 && $dwPStart[[$dwSelected[[pn, 1]], 1]] === $dwPStart[[$dwSelected[[pn, 1]], -2]]),
									{{$dwSelected[[pn,1]], 1}, {$dwSelected[[pn,1]], 2}},
									Nothing
								],
							{pn, Length[$dwSelected]}],1]
						]]/.{({_,0})->Sequence[]};
					(* move BezierCurve handles *)
					Do[
						$dwP[[Sequence@@handles[[pn]]]] = eachTransform[[Flatten[Position[#[[1]]&/@$dwSelected, handles[[pn, 1]]]][[1]]]][$dwPStart[[Sequence@@handles[[pn]]]]],
					{pn, Length[handles]}],
					
					(* object *)
					Do[
						If[MemberQ[{Text, Image, "Expression"}, $dwHead[[$dwSelected[[pn]]]]], 
							$dwStyle[[$dwSelected[[pn]],1]] = $dwStyleContentStart[[$dwSelected[[pn]],1]] + $dwRotateAngle
						];
						$dwP[[$dwSelected[[pn]]]] = eachTransform[[pn]][$dwPStart[[$dwSelected[[pn]]]]],
					{pn, Length[eachTransform]}]
				],
				
				If[Length[$dwSelected[[1]]] > 1,
					
					(* point *)
					Do[
						$dwP[[Sequence@@$dwSelected[[pn]]]] = rotate[$dwPStart[[Sequence@@$dwSelected[[pn]]]]],
					{pn, Length[$dwSelected]}];
					(* gather BezierCurve handles *)
					handles = 
						DeleteDuplicates[Flatten[Table[
							If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[pn,1]]]]],
								{{$dwSelected[[pn,1]], $dwSelected[[pn,2]]-1}, $dwSelected[[pn]], {$dwSelected[[pn,1]], $dwSelected[[pn,2]]+1}},
								Nothing
							],
						{pn, Length[$dwSelected]}], 1]];
					(* add first point and BezierCurve handle if last point selected *)
					handles = 
						DeleteDuplicates[Join[handles,
							Flatten[Table[
								If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[pn,1]]]]] && ($dwSelected[[pn, 2]] == Length[$dwP[[$dwSelected[[pn, 1]]]]]-1 && $dwPStart[[$dwSelected[[pn, 1]], 1]] === $dwPStart[[$dwSelected[[pn, 1]], -2]]),
									{{$dwSelected[[pn,1]], 1}, {$dwSelected[[pn,1]], 2}},
									Nothing
								],
							{pn, Length[$dwSelected]}],1]
						]]/.{({_,0})->Sequence[]};
					(* move BezierCurve handles *)
					Do[
						$dwP[[Sequence@@handles[[pn]]]] = rotate[$dwPStart[[Sequence@@handles[[pn]]]]];,
					{pn, Length[handles]}],
					
					(* object *)
					Do[
						If[MemberQ[{Text, Image, "Expression"}, $dwHead[[$dwSelected[[pn]]]]], 
							$dwStyle[[$dwSelected[[pn]],1]] = $dwStyleContentStart[[$dwSelected[[pn]],1]] + $dwRotateAngle
						];
						$dwP[[$dwSelected[[pn]]]] = rotate[$dwPStart[[$dwSelected[[pn]]]]],
					{pn, Length[$dwSelected]}]
				]
			];
			$dwSelectionBoundary = rotate[$dwSelectionBoundaryStart],
		
			(* scale *)
			$dwScale = Round[Abs[(MousePosition["Graphics"] - $dwSelectionCenter)]/Abs[($dwClickPtNoGrid - $dwSelectionCenter + $MachineEpsilon)], If[CurrentValue["ShiftKey"], 1, $dwScaleStep]/100];
			$dwScale = 
				If[MemberQ[{"s2","s6"}, n], 
					{If[CurrentValue[$dwOptionKey], 1, $dwScale[[2]]], $dwScale[[2]]}, 
					{$dwScale[[1]], If[CurrentValue[$dwOptionKey], 1, $dwScale[[1]]]}
				];
			scale = ScalingTransform[$dwScale, $dwSelectionCenter];
			If[$dwTransformEach === True,
				eachTransform = Table[ScalingTransform[$dwScale, pn], {pn, $dwTransformEachCenter}];
				
				If[Length[$dwSelected[[1]]] > 1,
					
					(* point *)
					Do[
						$dwP[[Sequence@@$dwSelected[[pn]]]] = eachTransform[[pn]][$dwPStart[[Sequence@@$dwSelected[[pn]]]]],
					{pn, Length[$dwSelected]}];
					(* gather BezierCurve handles *)
					handles = 
						DeleteDuplicates[Flatten[Table[
							If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[pn,1]]]]],
								{{$dwSelected[[pn,1]], $dwSelected[[pn,2]]-1}, {$dwSelected[[pn,1]], $dwSelected[[pn,2]]+1}},
								Nothing
							],
						{pn, Length[$dwSelected]}], 1]];
					(* add first point and BezierCurve handle if last point selected *)
					handles = 
						DeleteDuplicates[Join[handles,
							Flatten[Table[
								If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[pn,1]]]]] && ($dwSelected[[pn, 2]] == Length[$dwP[[$dwSelected[[pn, 1]]]]]-1 && $dwPStart[[$dwSelected[[pn, 1]], 1]] === $dwPStart[[$dwSelected[[pn, 1]], -2]]),
									{{$dwSelected[[pn,1]], 1}, {$dwSelected[[pn,1]], 2}},
									Nothing
								],
							{pn, Length[$dwSelected]}],1]
						]]/.{({_,0})->Sequence[]};
					(* move BezierCurve handles *)
					Do[
						$dwP[[Sequence@@handles[[pn]]]] = eachTransform[[Flatten[Position[#[[1]]&/@$dwSelected, handles[[pn, 1]]]][[1]]]][$dwPStart[[Sequence@@handles[[pn]]]]],
					{pn, Length[handles]}],
					
					(* object *)
					Do[$dwP[[$dwSelected[[pn]]]] = eachTransform[[pn]][$dwPStart[[$dwSelected[[pn]]]]], {pn, Length[eachTransform]}]
				],
				
				If[Length[$dwSelected[[1]]] > 1,
					
					(* point *)
					Do[
						$dwP[[Sequence@@$dwSelected[[pn]]]] = scale[$dwPStart[[Sequence@@$dwSelected[[pn]]]]],
					{pn, Length[$dwSelected]}];
					(* gather BezierCurve handles *)
					handles = 
						DeleteDuplicates[Flatten[Table[
							If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[pn,1]]]]],
								{{$dwSelected[[pn,1]], $dwSelected[[pn,2]]-1}, $dwSelected[[pn]], {$dwSelected[[pn,1]], $dwSelected[[pn,2]]+1}},
								Nothing
							],
						{pn, Length[$dwSelected]}], 1]];
					(* add first point and BezierCurve handle if last point selected *)
					handles = 
						DeleteDuplicates[Join[handles,
							Flatten[Table[
								If[MemberQ[{BezierCurve}, $dwHead[[$dwSelected[[pn,1]]]]] && ($dwSelected[[pn, 2]] == Length[$dwP[[$dwSelected[[pn, 1]]]]]-1 && $dwPStart[[$dwSelected[[pn, 1]], 1]] === $dwPStart[[$dwSelected[[pn, 1]], -2]]),
									{{$dwSelected[[pn,1]], 1}, {$dwSelected[[pn,1]], 2}},
									Nothing
								],
							{pn, Length[$dwSelected]}],1]
						]]/.{({_,0})->Sequence[]};
					(* move BezierCurve handles *)
					Do[
						$dwP[[Sequence@@handles[[pn]]]] = scale[$dwPStart[[Sequence@@handles[[pn]]]]];,
					{pn, Length[handles]}],
					
					(* object *)
					Do[$dwP[[pn]] = scale[$dwPStart[[pn]]], {pn, $dwSelected}]
				]
			];
			(* scale images & expressions *)
			Do[
				If[MemberQ[{Image, "Expression"}, $dwHead[[pn]]],
					$dwStyle[[pn,3]] = $dwStyleContentStart[[pn,3]]*If[$dwScale[[1]] < 1||$dwScale[[2]] < 1, Min[Sequence@@$dwScale], Max[Sequence@@$dwScale]];
					$dwBoundingBoxes[[pn]] = scale[$dwBoundingBoxesStart[[pn]]]
				], {pn, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}];
			
			$dwSelectionBoundary = scale[$dwSelectionBoundaryStart]
		]
		
	]

End[] (* End Private Context *)

EndPackage[]