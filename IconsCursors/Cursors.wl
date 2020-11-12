(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

WLDraw`$dwCursorCanvas = Graphics[{Black,Line[{{-0.3, -1.}, {-0.35, -0.75}, {-0.65, -0.55}, {-0.97, -0.108}, {-0.91, 0.077}, {-0.75, 0.1}, {-0.45, -0.2}, {-0.45, 0.1}, {-0.7, 0.7}, {-0.6327, 0.8914}, {-0.45, 0.85}, {-0.2, 0.25}, {-0.15, 0.9}, {0.0093, 0.9841}, {0.15, 0.9}, {0.1883, 0.2557}, {0.35, 0.8}, {0.534, 0.879}, {0.65, 0.75}, {0.55, 0.15}, {0.75, 0.6}, {0.9, 0.6}, {0.95, 0.45}, {0.85, 0.}, {0.8, -0.45}, {0.5525, -0.8246}, {0.5525, -1.016}}]},ImageSize->18];

WLDraw`$dwCursorText = Style[Graphics[{Style[Text["T",{1,-1},{1,-.5}],8],Line[{{0,1},{0,-1}}],Line[{{-1,0},{1,0}}]},ImageSize->18,ImagePadding->None],Antialiasing->False];

WLDraw`$dwCursorDraw = Style[Graphics[{Line[{{0,1},{0,-1}}],Line[{{-1,0},{1,0}}]},ImageSize->18,ImagePadding->None],Antialiasing->False];

WLDraw`$dwCursorPoint = Block[{cursorHole=.35},Style[Graphics[{Style[Circle[{0,0},cursorHole],Antialiasing->True],Line[{{0,1},{0,cursorHole}}],Line[{{0,-cursorHole},{0,-1}}],Line[{{1,0},{cursorHole,0}}],Line[{{-cursorHole,0},{-1,0}}]},ImageSize->18,ImagePadding->None],Antialiasing->False]];

WLDraw`$dwCursorPlotRange = Block[{cursorHole=.35},Style[Graphics[{Darker[$dwPlotRangeColor],Style[Circle[{0,0},2cursorHole],Antialiasing->True],Line[{{0,1},{0,cursorHole}}],Line[{{0,-cursorHole},{0,-1}}],Line[{{1,0},{cursorHole,0}}],Line[{{-cursorHole,0},{-1,0}}]},ImageSize->18,ImagePadding->None],Antialiasing->False]];

WLDraw`$dwCursorSelect = Graphics[{Opacity[0],Polygon[{{1., 1.}, {1., -1.}, {-1., -1.}, {-1., 1.}}],Opacity[1],Black,Polygon[{{-0.03929491924311229, 0.05131397208144145}, {0.5955127018922195, -0.44820508075688764}, {0.3540063509461097, -0.5299038105676657}, {0.5540063509461097, -0.876313972081441}, {0.4241025403784439, -0.951313972081441}, {0.2241025403784439, -0.6049038105676657}, {0.03259618943233425, -0.7732050807568875}, {-0.03929491924311229, 0.05131397208144145}}]},ImageSize->{36,36}];

End[] (* End Private Context *)

EndPackage[]