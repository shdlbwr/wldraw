(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderWireframeObjectForMenu[n_]:=
	{
		If[!MemberQ[$dwHideLayers, n] && $dwP[[n]] =!= {},
			Switch[$dwHead[[n]],
				Image,
					Line[($dwP[[n,1]]-If[$dwPStart === Null, {0,0}, $dwPStart[[n,1]]])+#&/@Join[$dwBoundingBoxes[[n]],{$dwBoundingBoxes[[n,1]]}]],
				Text,
					If[$dwStyle[[n]][[12]]===None,
						Rotate[Text[Style[Sequence@@($dwStyle[[n]][[Join[Range[2,11], {15}]]])]/.{(FontColor->_)->(FontColor->Black)}, $dwStyle[[n,3]]], $dwStyle[[n,1]], $dwP[[n,1]]],
						{}
					],
				Arrow,
					Line[$dwP[[n]]],
				Polygon,
					Line[Join[$dwP[[n]], {$dwP[[n,1]]}]],
				BezierCurve,
					BezierCurve[If[Length[$dwP[[n]]] > 2, Most[$dwP[[n]]], $dwP[[n]]]], 
				BSplineCurve, 
					If[Length[$dwP[[n]]] > 1,
						BSplineCurve[$dwP[[n]], SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]],
						Nothing
					], 
				_,
					$dwHead[[n]][$dwP[[n]]]
			]
		]
	}

End[] (* End Private Context *)

EndPackage[]