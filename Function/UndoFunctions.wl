(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwSetUndo[]:=
	Block[{},
		{$dwUndo, $dwRedo} = {DeleteDuplicates[Append[$dwUndo, dwGetCurrentState[]]], DeleteDuplicates[Prepend[$dwRedo, $dwUndo[[-1]]]]};
		(* check limits *)
		If[Length[$dwUndo] > $dwUndoLimit, $dwUndo = Rest[$dwUndo], Nothing]
	]
	
dwGetCurrentState[]:= {$dwP, $dwHead, $dwStyle, $dwPlotRange, $dwGroupLayers, $dwCompoundPathLayers, $dwBoundingBoxes, $dwObjectGradients, $dwLineGradients, $dwAnimate, $dwMode, $dwSelected, $dwPointModeSelections, $dwCurrentPreviewMode, $dwObjectQuantity, $dwPointQuantity, $dwHideLayers}

dwSetCurrentState[var_,n_]:=
	If[Length[ReleaseHold[var]]>=1,
		{$dwP, $dwHead, $dwStyle, $dwPlotRange, $dwGroupLayers, $dwCompoundPathLayers, $dwBoundingBoxes, $dwObjectGradients, $dwLineGradients, $dwAnimate, $dwMode, $dwSelected, $dwPointModeSelections, $dwCurrentPreviewMode, $dwObjectQuantity, $dwPointQuantity, $dwHideLayers} = ReleaseHold[var][[n,Range[17]]];
	]

End[] (* End Private Context *)

EndPackage[]