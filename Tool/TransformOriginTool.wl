(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwUpdateTransformOrigin[]:= 
	$dwTransformEachCenter = 
		If[FreeQ[{{}, {{}}}, $dwP] && $dwSelected =!= {},
			Table[dwFindSide[$dwP[[s]], $dwTransformOrigin, $dwHead[[s]], s],{s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}],
			{0,0}
		]

dwTransformOrigin[]:=
	Dynamic@Tooltip[
		Graphics[
			Table[With[{b=buttonParts},
				Inset[
					EventHandler[
						Graphics[{b[[1]]},ImageSize->(14/$dwZoom)],
						{
							"MouseDown" :> (Null),
							"MouseClicked":>($dwTransformOrigin=b[[2]]; dwUpdateTransformOrigin[]),
							"MouseDragged" :> (Null),
							"MouseUp" :> (Null)
						},
						PassEventsUp->False
					], buttonParts[[3]], {0,0}, 1/3
				]],
			{buttonParts,
				{{If[$dwTransformOrigin===#[[1]],GrayLevel[1],Gray], Polygon[{{0,0},{1,0},{1,1},{0,1}}]}, #[[1]], #[[2]]}&/@{
					{{Left,Top}, {0,2/3}},
					{{Center,Top}, {1/3,2/3}},
					{{Right,Top}, {2/3,2/3}},
					{{Left,Center}, {0,1/3}},
					{{Center,Center}, {1/3,1/3}},
					{{Right,Center}, {2/3,1/3}},
					{{Left,Bottom}, {0,0}},
					{{Center,Bottom}, {1/3,0}},
					{{Right,Bottom}, {2/3,0}}
				}
			}],ImagePadding->{{0,0},{0,0}}, ImageSize->36(*/$dwZoom*), PlotRange->{{0,1},{0,1}}],
	If[$dwTransformEach,
		"Set origin for scale and rotation.\nClick one of the nine boxes to set the position relative to each selected object.",
		"Set origin for scale and rotation.\nClick one of the nine boxes to set the position relative to all selected objects."
	], TooltipDelay->$dwTooltipDelay]

End[] (* End Private Context *)

EndPackage[]