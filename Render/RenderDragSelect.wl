(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderDragSelect[]:=
	If[MemberQ[{"preview","wireframe","point","pointwireframe"},$dwMode] && 
		($dwDragSelectStart=!=$dwDragSelectEnd && !CurrentValue[$dwCommandKey](*(!CurrentValue[$dwCommandKey] && !(CurrentValue[$dwCommandKey] && CurrentValue[$dwOptionKey]))*)),
		{Opacity[0],CapForm["Butt"],EdgeForm[{AbsoluteThickness[1/$dwZoom],AbsoluteDashing[{3,3}/$dwZoom],Gray}],Rectangle[$dwDragSelectStart,$dwDragSelectEnd]},{}
	]

End[] (* End Private Context *)

EndPackage[]