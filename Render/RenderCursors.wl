(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderCursors[]:=
	Dynamic@Which[
		CurrentValue[$dwCommandKey] && !CurrentValue[$dwOptionKey],
			$dwCursorCanvas,
		CurrentValue[$dwCommandKey] && CurrentValue[$dwOptionKey],
			$dwIconZoomDrag,
		True,
			Switch[$dwMode,
				"canvas", $dwCursorCanvas,
				"text", $dwCursorText,
				"plotrange", $dwCursorPlotRange,
				"point", $dwCursorPoint,
				"draw"|"splitPoint"|"toggleCornerPt", $dwCursorDraw,
				"zoomlayer"|"zoomwireframe", $dwIconZoom,
				_, $dwCursorSelect
			]
		]

End[] (* End Private Context *)

EndPackage[]