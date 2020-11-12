(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderPlotRange[]:=
	If[$dwMode === "plotrange",
		{AbsoluteThickness[1/$dwZoom], $dwPlotRangeColor,
			Line[{$dwPlotRange[[1]],{$dwPlotRange[[2,1]],$dwPlotRange[[1,2]]},$dwPlotRange[[2]],{$dwPlotRange[[1,1]],$dwPlotRange[[2,2]]},$dwPlotRange[[1]]}],EdgeForm[{AbsoluteThickness[1/$dwZoom],$dwPlotRangeColor}],
			Table[Annotation[Disk[$dwPlotRange[[n]],Offset[4/$dwZoom]], n, "Mouse"], {n,Length@$dwPlotRange}]},
		{}
	]

End[] (* End Private Context *)

EndPackage[]