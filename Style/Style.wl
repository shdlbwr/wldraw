(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwStyleControls[]:=
	Block[{sel, noStyleMargin = 12, fontsize = 10, opts = {Appearance->"Frameless"}},
		If[$dwSelected === {},
			
			Row[{}],
			
			sel = If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1, 1]], $dwSelected[[1]]];
			Pane[
				Row[{
					Grid[{
						{Row[Flatten[{dwStyleMode[sel, opts]}]]},
						{Row[{Spacer[{0,30}],
							Switch[$dwStyleMode,
								"text",
									dwStyleText[sel, noStyleMargin, fontsize],
								"fill",
									dwStyleFill[sel, noStyleMargin, fontsize],
								"stroke",
									dwStyleStroke[sel, noStyleMargin, fontsize],
								"arrow",
									dwStyleArrow[sel, noStyleMargin, fontsize],
								"point",
									dwStylePoint[sel, noStyleMargin, fontsize],
								"image",
									dwStyleImage[sel, noStyleMargin, fontsize],
								_,
									Row[{}]
							]}]
						}
					}, Alignment->{Left, Bottom}, Background->None, Spacings->{0,0}]
				}],
			Background->GrayLevel[.75], BaselinePosition->Scaled[.35], FrameMargins->0, ImageSize->Full]
		]
	]

End[] (* End Private Context *)

EndPackage[]