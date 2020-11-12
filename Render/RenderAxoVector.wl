(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderAxoVector[]:=
	If[$dwConstrainHAngle < 0,
		If[$dwSelected =!= {} && MemberQ[{"preview","wireframe"}, $dwMode],
			EventHandler[{
				AbsoluteThickness[3/$dwZoom],
				Arrowheads[{-22.5/$dwWindowSize[[1]], 22.5/$dwWindowSize[[1]]}], 
				If[$dwAxoVectorZActive, Gray, $dwAxoVectorColor], 
				Arrow[dwFindCenter[Flatten[Table[$dwBoundingBoxes[[n]], {n, $dwSelected}], 1]] + (#/$dwZoom)&/@(.4{(1/$dwGridStep)*$dwGridSize[[2]],{0,0}, (1/$dwGridStep)*$dwGridSize[[1]]})], 
				If[$dwAxoVectorZActive, $dwAxoVectorColor, Gray], 
				Arrowheads[22.5/$dwWindowSize[[1]]], 
				Arrow[dwFindCenter[Flatten[Table[$dwBoundingBoxes[[n]], {n, $dwSelected}], 1]] + (#/$dwZoom)&/@{{0,0},{0,.4}}]
			},
				{
					"MouseDown":>(If[$dwAxoVectorZActive, $dwAxoVectorZActive=False, $dwAxoVectorZActive=True])
				}, PassEventsUp->False
			]
		]
	]

End[] (* End Private Context *)

EndPackage[]