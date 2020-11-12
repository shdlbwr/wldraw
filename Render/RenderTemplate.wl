(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderTemplate[] :=
	Flatten@{If[$dwTemplateRender && (ImageQ[$dwTemplate[[1]]] || MemberQ[{Graphics, Graphics3D}, Head[$dwTemplate[[1]]]]),
		Rotate[
			Inset[ImageResize[ImageAdjust[$dwTemplate[[1]],{0,0,1},{0,1},{$dwTemplate[[5]],1}], {{240}, {240}}], 
			$dwTemplate[[3]], ImageScaled[{.5, .5}], 2 $dwTemplate[[2]]], $dwTemplate[[4]] Degree]
    ]}

End[] (* End Private Context *)

EndPackage[]