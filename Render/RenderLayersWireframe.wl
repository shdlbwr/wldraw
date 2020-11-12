(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderWireframe[]:=
	Join[{AbsoluteThickness[1/$dwZoom]},
		Table[
			{
				If[!MemberQ[$dwHideLayers, n] && $dwP[[n]] =!= {},
					Switch[$dwHead[[n]],
						Image|"Expression",
							Line[($dwP[[n,1]]-If[$dwPStart === Null, {0,0}, $dwPStart[[n,1]]])+#&/@Join[$dwBoundingBoxes[[n]],{$dwBoundingBoxes[[n,1]]}]],
						"Text3D",
							Text[Style[Sequence@@($dwStyle[[n]][[Join[Range[2,11], {15}]]])]/.{(FontColor->_)->(FontColor->Black)}, $dwP[[n,1]], $dwStyle[[n,3]]],
						Text,
							If[$dwStyle[[n]][[12]]===None,
								Rotate[Text[Style[Sequence@@($dwStyle[[n]][[Join[Range[2,11], {15}]]])]/.{(FontColor->_)->(FontColor->Black)}, $dwP[[n,1]], $dwStyle[[n,3]]], $dwStyle[[n,1]], $dwP[[n,1]]],
								{}
							],
						Arrow,
							Line[$dwP[[n]]],
						Polygon,
							Line[Join[$dwP[[n]], {$dwP[[n,1]]}]],
						BezierCurve,
							If[MemberQ[{"anytimemove","move","transform"}, $dwMode] && Length[$dwP[[n]]] > $dwMaxSplineMovePoints,
								{GrayLevel[0,1], Line[Table[$dwP[[n,pn]],{pn, 1, Length[$dwP[[n]]], 2*Round[Length[$dwP[[n]]]/$dwMaxSplineMovePoints]}]]},
								{
									BezierCurve[If[Length[$dwP[[n]]] > 2, Most[$dwP[[n]]], $dwP[[n]]]],
									If[MemberQ[{"anytimemove","canvas","move","plotrange","transform","zoomwireframe"}, $dwMode] || Length[$dwSelected] > 1, {}, dwPointsAndHandles[n]]
								}
							],
						BSplineCurve,
							If[MemberQ[{"anytimemove","move","transform"}, $dwMode] && Length[$dwP[[n]]] > $dwMaxSplineMovePoints,
								{GrayLevel[0,1], Line[Table[$dwP[[n,pn]],{pn, 1, Length[$dwP[[n]]], 2*Round[Length[$dwP[[n]]]/$dwMaxSplineMovePoints]}]]},
								If[Length[$dwP[[n]]] > 2,
									{Thin, BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]},
									Switch[Length[$dwP[[n]]],
										1, Point[$dwP[[n]]],
										2, Line[$dwP[[n]]],
										_, {}
									]
								]
							],
						Point,
							{AbsolutePointSize[$dwStyle[[n,7]]], $dwHead[[n]][$dwP[[n]]]},
						_,
							$dwHead[[n]][$dwP[[n]]]
					]
				],
				Switch[$dwMode,
					"anytimemove"|"canvas"|"move"|"plotrange"|"transform",
						{},
					"preview",
						If[$dwShapeStylePreviewShowPoints, dwPointsAndHandles[n], {}],
					"pointwireframe",
						If[MemberQ[$dwPointModeSelections, n], dwPointsAndHandles[n], {}],
					_,
						If[$dwSelected =!= {}, dwPointsAndHandles[n], {}]
				]
			}, 
		{n, Length[$dwP]}]
	]

End[] (* End Private Context *)

EndPackage[]