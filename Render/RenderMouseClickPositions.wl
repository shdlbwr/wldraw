(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderMouseClickPositions[]:=
	If[$dwShowMouseClickPositions,
		{
			If[$dwPrevious2ClickPtNoGrid =!= Null, Inset[Graphics[{AbsoluteThickness[1/$dwZoom], Red, Line[{{-1,0}, {1,0}}], Line[{{0,-1}, {0,1}}]}], Round[$dwPrevious2ClickPtNoGrid, $dwGridStep], {0,0}, Offset[15/$dwZoom]], Nothing],
			If[$dwPreviousClickPtNoGrid =!= Null, Inset[Graphics[{AbsoluteThickness[1/$dwZoom], Line[{{-1,0}, {1,0}}], Line[{{0,-1}, {0,1}}]}], Round[$dwPreviousClickPtNoGrid, $dwGridStep], {0,0}, Offset[15/$dwZoom]], Nothing],
			If[$dwClickPtNoGrid =!= Null, Inset[Graphics[{AbsoluteThickness[1/$dwZoom], Line[{{-1,0}, {1,0}}], Line[{{0,-1}, {0,1}}]}], Round[$dwClickPtNoGrid, $dwGridStep], {0,0}, Offset[15/$dwZoom]], Nothing]
		},
		{}
	]

End[] (* End Private Context *)

EndPackage[]