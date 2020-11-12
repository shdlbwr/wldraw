(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

WLDraw`$dwIconBezierDiscretize = Graphics[{$dwToolColor,Line[{{Sin[Pi/4],Cos[Pi/4]},{1/3,1.33}}],Line[{{Sin[7Pi/4],Cos[7Pi/4]},{-1/3,1.33}}],Circle[{0,0},1,{Pi/4,3Pi/4}],Point[{{Sin[Pi/4],Cos[Pi/4]},{Sin[7Pi/4],Cos[7Pi/4]},{1/3,1.33},{-1/3,1.33}}],$dwToolHiliteColor,Circle[{0,-.5},1,{Pi/4,3Pi/4}],Point[Table[{Sin[n],Cos[n]-.5},{n,Pi/4,-Pi/4,-Pi/8}]]},ImageSize->{24,24}]

WLDraw`$dwIconToggleCornerPt = Graphics[{Text[Style["CORNER",6,$dwToolHiliteColor],{0,1},{0,-.7}],$dwToolHiliteColor,Point[{{0,.5}}],Line[{{0,0},{0,.5},{.5,.5}}],$dwToolColor,Point[{{-.19,.7}}],Circle[{.5,0},1,{Pi/1.7,Pi/1.1}]},ImageSize->{27,27}];

WLDraw`$dwIconIncreasePts=Graphics[{$dwToolColor, Circle[],Point[Table[{Sin[n],Cos[n]},{n,0,2Pi,Pi/2}]],$dwToolHiliteColor,Point[Table[{Sin[n],Cos[n]},{n,Pi/4,2Pi,Pi/2}]],$dwToolColor,Text[Style["+",14,$dwToolHiliteColor],{0,0}]},ImageSize->{21,21}];

WLDraw`$dwIconDecreasePts=Graphics[{$dwToolColor, Circle[],$dwToolColor,Point[Table[{Sin[n],Cos[n]},{n,0,2Pi,Pi/2}]],$dwToolHiliteColor,Point[Table[{Sin[n],Cos[n]},{n,Pi/4,2Pi,Pi/2}]],$dwToolColor,Text[Style["-",14,$dwToolHiliteColor],{0,0}]},ImageSize->{21,21}];

WLDraw`$dwIconSplitPoint = Graphics[{$dwToolColor,Line[{{0,1},{1,1}}],Text[Style["INSERT\nDELETE",LineSpacing->{0,7},6,$dwToolHiliteColor],{.5,1.75},{0,0}],Point[{{0,1},{1,1}}],$dwToolHiliteColor,Point[{{.5,1}}]},PlotRange->{{-.5,1.5},{.5,2.5}},ImageSize->{24,24}];

WLDraw`$dwIconJoinPaths = Graphics[{$dwToolColor,Text[Style["JOIN",6,$dwToolHiliteColor],{.5,1},{0,-.5}],$dwToolColor,Point[{{0,0},{1/3,0},{1,0},{2/3,0}}], Line[{{0,0},{1/3,0},{1/3,.75}}],Line[{{1,0},{2/3,0},{2/3,.75}}],$dwToolHiliteColor,Point[{{1/3,.75},{2/3,.75}}],Line[{{1/3,.75},{2/3,.75}}]},ImageSize->{24,24}];

WLDraw`$dwIconDeleteDupePts = Graphics[{$dwToolColor,Text[Style["DELETE\nDUPE\nPOINTS",LineSpacing->{0,7},6,$dwToolHiliteColor],{0,0},{0,0}]},ImageSize->{24,24}];

WLDraw`$dwIconSplitPath = Graphics[{EdgeForm[$dwToolColor],$dwToolColor,Line[{{0,0},{0,.75},{.5,.75}}],Text[Style["SPLIT",6,$dwToolHiliteColor],{.5,1},{0,-.375}],Point[{{0,0},{0,.75}}],$dwToolHiliteColor,Line[{{.5,.75},{1,.75},{1,.375},{1,0}}],Point[{{.5,.75},{1,.75},{1,0}}]},ImageSize->{24,24}];

WLDraw`$dwIconReverseDirection = Graphics[{$dwToolColor, Line[{{-.2, 0}, {1.2, 0}}], Point[{{-.2, 0}, {1.2, 0}}], $dwToolHiliteColor, Arrowheads[Small], Arrow[{{0, .3}, {1, .3}}], Arrow[{{1, -.3}, {0, -.3}}]}, ImageSize -> {24, 24}];
   
End[] (* End Private Context *)

EndPackage[]