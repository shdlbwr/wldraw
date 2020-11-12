(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwMouseClicked[]:=
	Block[{s = $dwSelected},
		If[MemberQ[{"preview","wireframe"}, $dwMode] && Length[s] == 1,
			If[CurrentValue["MouseClickCount"] == 2 && s =!= {},
				Switch[$dwHead[[s[[1]]]],
					"Expression",
						dwReplaceExpression[s[[1]]],
					Image,
						dwReplaceImage[s[[1]]],
					Text|"Text3D",
						dwReplaceText[s[[1]]],
					_,
						Nothing
				],
				Nothing
			]
		]
	]

End[] (* End Private Context *)

EndPackage[]