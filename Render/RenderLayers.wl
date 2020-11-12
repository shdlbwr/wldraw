(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderLayers[]:=
	Switch[$dwMode,
		"canvas"|"plotrange"|"pointwireframe",
			dwRenderWireframe[],
		"anytimemove"|"zoomwireframe",
			If[Length[Flatten[$dwP]]/2 > $dwNumPtsToReplaceObjWithBoxes,
				dwRenderLayerBoxes["MovingPoints"->False],
				dwRenderWireframe[]
			],
		"move",
			If[Length[Flatten[$dwP]]/2 > $dwNumPtsToReplaceObjWithBoxes,
				dwRenderLayerBoxes[],
				dwRenderWireframe[]
			],
		"transform",
			If[Length[Flatten[$dwP]]/2 > $dwNumPtsToReplaceObjWithBoxes,
				Nothing,
				dwRenderWireframe[]
			],
		"wireframe",
			dwRenderAnnotatedWireframe[],
		"preview"|"zoomlayer",
			dwRenderNotAddingOrMovingPoints[],
		"point",
			$dwCurrentPreviewMode = "wireframe";
			dwRenderAddingOrMovingPointsWireframe[],
		_,
			If[$dwCurrentPreviewMode === "preview",
				dwRenderAddingOrMovingPoints[],
				dwRenderAddingOrMovingPointsWireframe[]
			]
	]

End[] (* End Private Context *)

EndPackage[]