(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderZoom[]:=
	If[MemberQ[{"zoomlayer","zoomwireframe"},$dwMode] && ($dwZoomStart=!=$dwZoomEnd || (CurrentValue[$dwCommandKey] && CurrentValue[$dwOptionKey])),
		{Opacity[0],CapForm["Butt"],EdgeForm[{AbsoluteThickness[1/$dwZoom],AbsoluteDashing[{2,2}/$dwZoom],Gray}],Rectangle[$dwZoomStart,$dwZoomEnd]},
		{}
	]

End[] (* End Private Context *)

EndPackage[]