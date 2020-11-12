(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *)

WLDraw`$dwIconStyleArrow = Dynamic@Graphics[{If[$dwStyleMode === "arrow", $dwStyleControlColor, Black], Arrowheads[Medium], Arrow[{{0,0},{1,1}}]}, ImageSize -> {18, 18}]; 

WLDraw`$dwIconStyleFill = Dynamic@Graphics[{If[$dwStyleMode === "fill", $dwStyleControlColor, Black], EdgeForm[If[$dwStyleMode === "fill", $dwStyleControlColor, Black]], Rectangle[]}, ImageSize -> {18, 18}];

WLDraw`$dwIconStyleStroke = Dynamic@Graphics[{EdgeForm[If[$dwStyleMode === "stroke", $dwStyleControlColor, Black]], Opacity[0], Rectangle[]}, ImageSize -> {18, 18}];

WLDraw`$dwIconStyleText = Dynamic@Graphics[{If[$dwStyleMode === "text", $dwStyleControlColor, Black], Text[Style["T", 18], {.5,.5}]}, ImageSize -> {18, 18}];

WLDraw`$dwIconStylePoint = Dynamic@Graphics[{If[$dwStyleMode === "point", $dwStyleControlColor, Black], PointSize[Large], Point[{0,0}]}, ImageSize -> {18, 18}];

WLDraw`$dwIconGradientSettings = Graphics[{Thickness[.5],Line[{{-1,.6},{1,.6}},VertexColors->{Black,LightGray}],Thickness[.1],Line[{{-1,-.5},{1,-.5}}],EdgeForm[Black],White,Disk[{0,-.5},.35]},ImageSize->{18,18},PlotRange->1];

WLDraw`$dwIconStyleImage = Dynamic@Image[ArrayPlot[{{.1,.2,.3},{.4,.5,.6},{.7,.8,.9}},Background->None,Frame->If[$dwStyleMode === "image", True, False],FrameStyle->If[$dwStyleMode === "image", $dwStyleControlColor, Black],ImageSize->18,PlotRange->{0,1}]];

WLDraw`$dwIconPresetNoFill = Graphics[{Opacity[0],EdgeForm[Black],Disk[],Style[Rectangle[{0,0},{2,2}],Antialiasing->True]},ImageSize->{18,18}];

WLDraw`$dwIconPresetEdgeAndFill = Graphics[{Gray,EdgeForm[Black],Disk[],Style[Rectangle[{0,0},{2,2}],Antialiasing->True]},ImageSize->{18,18}];

WLDraw`$dwIconPresetNoEdge = Graphics[{Gray,EdgeForm[],Disk[],Style[Rectangle[{0,0},{2,2}],Antialiasing->True]},ImageSize->{18,18}]

End[] (* End Private Context *)

EndPackage[]