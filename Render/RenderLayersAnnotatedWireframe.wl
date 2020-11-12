(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderAnnotatedWireframe[]:=
	Join[{AbsoluteThickness[1/$dwZoom]},
		Table[
			If[!MemberQ[$dwHideLayers, n] && $dwP[[n]] =!= {},
				Annotation[
					Switch[$dwHead[[n]],
						Image|"Expression",
							Line[Join[$dwBoundingBoxes[[n]],{$dwBoundingBoxes[[n,1]]}]],
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
							BezierCurve[If[Length[$dwP[[n]]] > 2, Most[$dwP[[n]]], $dwP[[n]]]], 
						BSplineCurve, 
							If[Length[$dwP[[n]]] > 2,
								{Thin, BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]]},
								Switch[Length[$dwP[[n]]],
									1, Point[$dwP[[n]]],
									2, Line[$dwP[[n]]],
									_, {}
								]
							], 
						Point,
							{AbsolutePointSize[$dwStyle[[n,7]]], $dwHead[[n]][$dwP[[n]]]},
						_,
							$dwHead[[n]][$dwP[[n]]]
					],
				n, "Mouse"]
			], 
		{n, Length[$dwP]}]
	]

End[] (* End Private Context *)

EndPackage[]