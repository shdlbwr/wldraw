(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwRenderLayerBoxes]:= {"MovingPoints"->True};

dwRenderLayerBoxes[OptionsPattern[]]:=
	Block[{pts},
		If[$dwSelected =!= {} && $dwSelectionBoundaryStart =!= {},
			{
				AbsoluteThickness[1/$dwZoom],
				If[OptionValue["MovingPoints"],
					Line[((Round[$dwMousePos, $dwGridStep] - $dwClickPt) + #)&/@$dwSelectionBoundaryStart[[{1, 3, 5, 7, 1}]]],
					Line[$dwSelectionBoundaryStart[[{1, 3, 5, 7, 1}]]]
				]
			},
			{}
		]
	]

End[] (* End Private Context *)

EndPackage[]