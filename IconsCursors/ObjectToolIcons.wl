(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

WLDraw`$dwIconGroupShape = Graphics[{$dwToolColor,Rectangle[{0,0},{1,1}],Disk[{1.5,1.5},.6],$dwHiliteColors,Rectangle[{-.5,-.5},{0,0}],Rectangle[{-.5,2.5},{0,2}],Rectangle[{2,2},{2.5,2.5}],Rectangle[{2,0},{2.5,-.5}],Opacity[0],EdgeForm[{CapForm["Butt"],$dwHiliteColors}],Rectangle[{-.5,-.5},{2.5,2.5}]},ImageSize->{21,21}];

WLDraw`$dwIconObjectDupe = Graphics[{EdgeForm[$dwToolColor],$dwButtonBackgroundColor,Rectangle[{-1,-1},{1,1}],EdgeForm[$dwToolHiliteColor],Rectangle[{-.5,-.5},{1.5,1.5}],Text[Style["+",14,$dwToolHiliteColor],-{.3,.6},-{1,1}]},ImageSize->{23,23}];

WLDraw`$dwIconMoveToFront = Style[Graphics[{$dwToolColor,EdgeForm[{}],Polygon[{{1.,1.},{0.3,1.},{0.3,0.1},{1.,0.1}}],Polygon[{{-0.8,-0.3},{0.8,-0.3},{0.8,-1.},{-0.8,-1.}}],Polygon[{{-1.,0.},{0.,0.},{0.,0.8},{-1.,0.8}}],EdgeForm[$dwToolColor],$dwToolHiliteColor,Polygon[{{-0.5,0.6},{0.6,0.6},{0.6,-0.7},{-0.5,-0.7}}]},ImageSize->21],Antialiasing->False];

WLDraw`$dwIconMoveToBack = Graphics[{$dwToolHiliteColor,EdgeForm[$dwToolColor],Rectangle[{.3,.3},{1.2,1.2}],$dwToolColor,Rectangle[{0,0},{.7,.7}],Rectangle[{.8,.8},{1.5,1.5}]},ImageSize->18];

WLDraw`$dwIconSnapPts = Graphics[{$dwToolColor,Table[{Line[{{n,-1},{n,1}}],Line[{{-1,n},{1,n}}]},{n,-1,1,2/3}],
						{$dwToolColor, EdgeForm[$dwToolColor],Polygon[.9#&/@{{0., 0.85},{0.25, 0.8},{0.45, 0.7},{0.6, 0.55},{0.7, 0.35},{0.75, 0.1},{0.7,-0.15},{0.45,-0.8},{0.1,-0.8},{0.35,-0.15},{0.4, 0.},{0.4, 0.2},{0.35, 0.35},{0.2, 0.45},{0., 0.5},{-0.2, 0.45},{-0.35, 0.35},{-0.4, 0.2},{-0.4, 0.},{-0.35,-0.15},{-0.1,-0.8},{-0.45,-0.8},{-0.7,-0.15},{-0.75, 0.1},{-0.7, 0.35},{-0.6, 0.55},{-0.45, 0.7},{-0.25, 0.8},{0., 0.85}}]},
						{$dwToolHiliteColor,EdgeForm[], Polygon[.9#&/@{{-0.45,-0.8},{-0.7,-0.15},{-0.35,-0.15},{-0.1,-0.8}}]},
						{$dwToolHiliteColor,EdgeForm[], Polygon[.9#&/@{{0.35,-0.15},{0.7,-0.15},{0.45,-0.8},{0.1,-0.8}}]}},ImageSize->{21,21}];

WLDraw`$dwIconReflectH = Graphics[{$dwButtonBackgroundColor, EdgeForm[$dwToolColor],Polygon[{{-1.2,-1.},{-0.2,0},{-1.2,1.}}],$dwToolColor, EdgeForm[$dwToolHiliteColor],Polygon[{{1.2,-1.},{0.2, 0},{1.2, 1.}}],$dwToolColor,CapForm["Butt"],Line[{{0,-1},{0,1}}]},ImageSize->{18,18}];

WLDraw`$dwIconReflectV = Graphics[{$dwButtonBackgroundColor, EdgeForm[$dwToolColor],Polygon[{{-1., 1.2},{0., 0.2},{1., 1.2}}],$dwToolColor, EdgeForm[$dwToolHiliteColor],Polygon[{{-1.,-1.2},{0.,-0.2},{1.,-1.2}}],$dwToolColor,CapForm["Butt"],Line[{{-1,0},{1,0}}]},ImageSize->{18,18}];

WLDraw`$dwIconCenterObject = Graphics[{$dwToolColor,Line[{{0,0},{1,0},{1,1},{0,1},{0,0}}],$dwToolHiliteColor,Arrowheads[.35],Arrow[{{0,.5},{.5,.5}}],Arrow[{{.5,0},{.5,.5}}],Arrow[{{1,.5},{.5,.5}}],Arrow[{{.5,1},{.5,.5}}]},ImageSize->{21,21}];

WLDraw`$dwIconAlign = Graphics[Style[{$dwToolHiliteColor,Line[{{0,1},{0,-1}}],Opacity[0],EdgeForm[$dwToolColor],Rectangle[{.3,-.9},{2,-.2}],Rectangle[{.3,.2},{1.5,.9}]},Antialiasing->False],ImageSize->{21,21}];

WLDraw`$dwIconMirrorH = Graphics[{$dwButtonBackgroundColor,EdgeForm[$dwToolHiliteColor],Polygon[{{0.,1.},{-1.2,1.},{-1.2,0.8},{-0.8,0.6},{-0.8,0.2},{-1.4,0.1},{-1.4,-0.1},{-0.8,-0.2},{-0.8,-0.6},{-1.2,-0.8},{-1.2,-1.},{0.,-1.}}],$dwButtonBackgroundColor,EdgeForm[$dwToolHiliteColor],Polygon[{{0.,1.},{1.2,1.},{1.2,0.8},{0.8,0.6},{0.8,0.2},{1.4,0.1},{1.4,-0.1},{0.8,-0.2},{0.8,-0.6},{1.2,-0.8},{1.2,-1.},{0.,-1.}}],$dwToolColor,Line[{{0,-1.5},{0,1.5}}]},ImageSize->{21,21}];
    
WLDraw`$dwIconRoundCorners = Graphics[{{$dwButtonBackgroundColor,EdgeForm[{AbsoluteThickness[2],$dwToolHiliteColor}],Rectangle[{0,0},{1,1},RoundingRadius->.25]},{Darker[$dwToolColor],AbsoluteThickness[2],Line[{{.3,1},{.7,1}}],Line[{{.3,0},{.7,0}}],Line[{{0,.3},{0,.7}}],Line[{{1,.3},{1,.7}}]}},ImageSize->{18,18}];

WLDraw`$dwIconCombineShapes = Graphics[{{$dwButtonBackgroundColor,EdgeForm[{$dwToolHiliteColor,AbsoluteThickness[2]}],Disk[{1,-1},1.2]},$dwButtonBackgroundColor,EdgeForm[{$dwToolColor}],Rectangle[{-1,-1},{1,1}],$dwToolColor,Circle[{1,-1},1.2,{Pi,Pi/2}],AbsoluteThickness[2],$dwToolHiliteColor,Line[{{-.2,-1},{-1,-1},{-1,1},{1,1},{1,.2}}]},ImageSize->{21,21}];

WLDraw`$dwIconIntersectShapes = Graphics[{$dwButtonBackgroundColor,EdgeForm[{AbsoluteThickness[1.6],$dwToolColor}],Disk[{1,-1},1.2],Style[Rectangle[{-1,-1},{1,1}], Antialiasing->True],EdgeForm[{$dwToolHiliteColor}],$dwToolHiliteColor,Disk[{1,-1},1.2,{Pi,Pi/2}]}, ImageSize->{21,21}];

WLDraw`$dwIconBlendShapes = Graphics[Flatten@{$dwButtonBackgroundColor,EdgeForm[$dwToolColor],Polygon[{{-1,-1},{1,-1},{1,1},{-1,1},{-1,-1}}],EdgeForm[{$dwToolHiliteColor,AbsoluteThickness[2]}],FilledCurve@BSplineCurve[{1,-1}+#&/@{{-1,-1},{0,-1},{1,-1},{1,0},{1,1},{0,1},{-1,1},{-1,0}},SplineClosed->True,SplineDegree->2],EdgeForm[{$dwToolColor,AbsoluteThickness[1]}],Disk[{2,-2},1]},AspectRatio->1,ImageSize->{21,21}];

WLDraw`$dwIconOffsetLines = Graphics[{$dwToolColor,Text[Style["OFFSET",6,$dwToolHiliteColor],{.5,1.3},{0,-1}],$dwToolColor,Polygon[{{0,0},{1,0},{.5,.83},{0,0}}],$dwToolHiliteColor,Line[{-.35,-.2}+1.7#&/@{{0,0},{1,0},{.5,.83},{0,0}}]},ImageSize->{24,24}];

WLDraw`$dwIconCompoundPath = Block[{p=Table[{Sin[x],Cos[x]},{x,0,2Pi,Pi/10}]},Graphics[{$dwToolColor,FilledCurve[{Line[p],Line[.5p]}],$dwToolHiliteColor,Line[p],Line[.5p]},ImageSize->{21,21}]];

WLDraw`$dwIconShearShape = Graphics[{{EdgeForm[$dwToolColor],Opacity[0],Polygon[{{0,0},{1,0},{1.5,1},{.5,1}}]},$dwToolHiliteColor,Arrowheads[Small],Arrow[{{-.5,1},{.5,1}}]},ImageSize->{24,24}];

WLDraw`$dwIconObjectShadow = Graphics[{Table[{Darker[$dwButtonBackgroundColor, n], Polygon[(1.25 - 1 n) CirclePoints[4]]}, {n, 0, .5, .1}], Lighter[$dwButtonBackgroundColor, .5], Polygon[{-1/3, 1/3} + # & /@ CirclePoints[4]]}, ImageSize -> {24, 24}];

WLDraw`$dwIconConnector = Graphics[{$dwToolHiliteColor,Arrowheads[.4],Arrow[{{1,.5},{2.5,.5},{2.5,-1.25}}],$dwToolColor,Polygon[{{0,0},{1,0},{1,1},{0,1}}],Polygon[{2,-2}+#&/@{{0,0},{1,0},{1,1},{0,1}}]},ImageSize->{21,21}]

WLDraw`$dwIconDistortShape = Graphics[{$dwToolColor,Polygon[{{-0.4, 0.4}, {0., 0.175}, {0.4, 0.4}, {0.175, 0.}, {0.4, -0.4}, {0., -0.175}, {-0.4, -0.4}, {-0.175, 0.}}]},ImageSize->{24,24}]

End[] (* End Private Context *)

EndPackage[]