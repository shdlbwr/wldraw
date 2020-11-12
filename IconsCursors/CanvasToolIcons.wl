(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

WLDraw`$dwIconObjectDelete = Graphics[{Red, AbsoluteThickness[3], Rotate[{Line[{{-1, 0}, {1, 0}}], Line[{{0, -1}, {0, 1}}]}, Pi/4, {0, 0}]}, ImageSize -> {18, 18}];

WLDraw`$dwIconObjectSelect = Graphics[{$dwToolHiliteColor,Polygon[{{-0.5566987298107782, 1.0026279441628827}, {0.7129165124598853, 0.0035898384862245378}, {0.22990381056766584, -0.15980762113533162},  {0.6299038105676658, -0.8526279441628823}, {0.37009618943233424, -1.0026279441628823}, {-0.02990381056766575, -0.3098076211353317},  {-0.4129165124598851, -0.6464101615137754},{-0.5566987298107782, 1.0026279441628827}}]},ImageSize->{18,18}];

WLDraw`$dwIconPointSelect = Graphics[{$dwToolHiliteColor,Circle[{-0.5566987298107782, 1.0026279441628827},.3],$dwToolColor,Polygon[{{-0.5566987298107782, 1.0026279441628827}, {0.7129165124598853, 0.0035898384862245378}, {0.22990381056766584, -0.15980762113533162},  {0.6299038105676658, -0.8526279441628823}, {0.37009618943233424, -1.0026279441628823}, {-0.02990381056766575, -0.3098076211353317},  {-0.4129165124598851, -0.6464101615137754},{-0.5566987298107782, 1.0026279441628827}}]},ImageSize->{20,20}];

WLDraw`$dwIconSelectionMenu = Graphics[{$dwToolHiliteColor,Polygon[{{0,1},{.75,.25},{1.5,1}}],$dwToolColor,Polygon[{{-0.5566987298107782, 1.0026279441628827}, {0.7129165124598853, 0.0035898384862245378}, {0.22990381056766584, -0.15980762113533162},  {0.6299038105676658, -0.8526279441628823}, {0.37009618943233424, -1.0026279441628823}, {-0.02990381056766575, -0.3098076211353317},  {-0.4129165124598851, -0.6464101615137754},{-0.5566987298107782, 1.0026279441628827}}]},ImageSize->{18,18}];

WLDraw`$dwIconZoom = Dynamic@Graphics[{$dwToolColor,CapForm["Round"],AbsoluteThickness[2],Line[{{.25,.25},{.45,.45}}],Circle[{.75,.75},.4],AbsoluteThickness[4],Line[{{0,0},{.25,.25}}],AbsoluteThickness[1],Line[{{.6,.75},{.9,.75}}],If[MemberQ[{"zoomlayer","zoomwireframe"},$dwMode] && CurrentValue[$dwOptionKey],{},Line[{{.75,.6},{.75,.9}}]]},ImageSize->18];

WLDraw`$dwIconZoomDrag = Graphics[{$dwToolColor,CapForm["Round"],AbsoluteThickness[2],Line[{{.25,.25},{.45,.45}}],Circle[{.75,.75},.4],AbsoluteThickness[4],Line[{{0,0},{.25,.25}}]},ImageSize->18];

WLDraw`$dwIconMoveCanvas = Graphics[{$dwToolColor,Line[{{-0.3, -1.},{-0.35, -0.75}, {-0.65, -0.55}, {-0.9722222222222225, -0.10853708526234561}, {-0.9104938271604941, 0.07664809992283954}, {-0.75, 0.1}, {-0.45, -0.2}, {-0.45, 0.1}, {-0.7, 0.7}, {-0.6327160493827164, 0.8914629147376542},  {-0.45, 0.85}, {-0.2, 0.25}, {-0.15, 0.9}, {0.009259259259258634, 0.9840555073302468}, {0.15, 0.9}, {0.18827160493827133, 0.25566044560185186}, {0.35, 0.8}, {0.5339506172839501, 0.8791172357253085}, {0.65, 0.75}, {0.55, 0.15},{0.75, 0.6}, {0.9, 0.6}, {0.95, 0.45}, {0.85, 0.}, {0.8, -0.45}, {0.5524691358024687, -0.8245864679783949}, {0.5524691358024687, -1.0159444926697527}}]},ImageSize->{18,18}]

WLDraw`$dwIconPlotRange = Graphics[{$dwToolColor,Rectangle[{.2,.1},{.5,.4}],Disk[{.75,.35},.15],Polygon[{.3,.6}+.4#&/@{{0,0},{1,0},{.5,.77}}],$dwPlotRangeColor,Line[{{0,0},{1,0},{1,1},{0,1},{0,0}}],Point[{{0,0},{1,1}}]},ImagePadding->2,ImageSize->24];

WLDraw`$dwIconTemplate = Graphics[{$dwToolColor,Line[{{0.5,0.3},{1.,0.}}],BezierCurve[{{0.5,0.9},{0.72,0.9},{0.9,0.72},{0.9,0.5},{0.9,0.28},{0.72,0.1},{0.5,0.1},{0.28,0.1},{0.1,0.28},{0.1,0.5},{0.1,0.72},{0.28,0.9},{0.5,0.9}}],Line[{{0.3,0.7},{0.,1.}}],Line[{{0.7,0.7},{1.,1.}}],Line[{{0.,0.},{0.,1.},{1.,1.},{1.,0.},{0.,0.},{0.5,0.3}}],White,Line[{{0.5,0.7},{0.7,0.7}}],Line[{{0.5,0.3},{0.5,0.7},{0.3,0.7}}]},ImagePadding->1,ImageSize->22];

WLDraw`$dwIconAutoDraw = Graphics[{Inset[ArrayPlot[{{0,0,1,0,0,0},{0,1,1,1,1,0},{1,1,1,1,1,1},{0,1,1,1,1,0},{0,0,0,1,0,0}},ColorFunction->(Blend[{GrayLevel[1,0],GrayLevel[.5,1]},#]&),AspectRatio->1,Frame->None,ImageMargins->None,ImagePadding->None],{0,0},Automatic,2],$dwToolHiliteColor,Circle[{0,0},1.1]},AspectRatio->1,ImagePadding->1,ImageSize->22];

WLDraw`$dwIconClipboard=Graphics[{EdgeForm[$dwToolColor],$dwButtonBackgroundColor,Rectangle[{-.4,0},{.4,1}],Polygon[{0,.8}+.15#&/@Join[{{1,-1},{-1,-1}},Table[{Sin[x],Cos[x]},{x,-Pi/2,Pi/2,Pi/180}]]],Rectangle[{-.25,.65},{.25,.8}],Text[Style["C",$dwToolHiliteColor,10],{0,.3}]},ImageSize->{22,22}];

WLDraw`$dwIconSaveFile = Graphics[{{JoinForm["Butt"],EdgeForm[$dwToolColor],$dwButtonBackgroundColor,Polygon[{{0,0},{.8,0},{.8,.7},{.5,1},{0,1}}],$dwToolColor,Line[{{.8,.7},{.5,.7},{.5,1}}],Text[Style["S",$dwToolHiliteColor,10],{.4,.4}]}},ImageSize->{22,22}];

WLDraw`$dwIconOpenFile = Graphics[{{JoinForm["Butt"],EdgeForm[$dwToolColor],$dwButtonBackgroundColor,Polygon[{{0,0},{.8,0},{.8,.7},{.5,1},{0,1}}],$dwToolColor,Line[{{.8,.7},{.5,.7},{.5,1}}]},Text[Style["O",$dwToolHiliteColor,10],{.4,.4}]},ImageSize->{22,22}];

WLDraw`$dwIconExportFile = Graphics[{{JoinForm["Butt"],EdgeForm[$dwToolColor],$dwButtonBackgroundColor,Polygon[{{0,0},{.8,0},{.8,.7},{.5,1},{0,1}}],$dwToolColor,Line[{{.8,.7},{.5,.7},{.5,1}}]},Text[Style["E",$dwToolHiliteColor,10],{.4,.4}]},ImageSize->{22,22}];

WLDraw`$dwIconImportFile = Graphics[{{JoinForm["Butt"],EdgeForm[$dwToolColor],$dwButtonBackgroundColor,Polygon[{{0,0},{.8,0},{.8,.7},{.5,1},{0,1}}],$dwToolColor,Line[{{.8,.7},{.5,.7},{.5,1}}]},Text[Style["I",$dwToolHiliteColor,10],{.4,.4}]},ImageSize->{22,22}];

WLDraw`$dwIconWireframeSelect = Graphics[{$dwToolHiliteColor,Line[{{-0.5566987298107782, 1.0026279441628827}, {0.7129165124598853, 0.0035898384862245378}, {0.22990381056766584, -0.15980762113533162},  {0.6299038105676658, -0.8526279441628823}, {0.37009618943233424, -1.0026279441628823}, {-0.02990381056766575, -0.3098076211353317},  {-0.4129165124598851, -0.6464101615137754},{-0.5566987298107782, 1.0026279441628827}}]},ImageSize->{18,18}];

WLDraw`$dwIconModelicaExport = Graphics[{{JoinForm["Butt"],EdgeForm[$dwToolColor],$dwButtonBackgroundColor,Polygon[{{0,0},{.8,0},{.8,.7},{.5,1},{0,1}}],$dwToolColor,Line[{{.8,.7},{.5,.7},{.5,1}}]},Text[Style["M",$dwToolHiliteColor,10],{.4,.4}]},ImageSize->{22,22}];

WLDraw`$dwIconPreview = Graphics[{White,EdgeForm[{}],FilledCurve@BSplineCurve[{{-1.,0.},{0.,1},{1.,0.},{1.,0.},{0.,-1},{-1.,0.},{-1.,0.}},SplineClosed->True,SplineDegree->3],Hue[.58],EdgeForm[{}],FilledCurve@BezierCurve[1.2#&/@{{0.,0.41},{0.2255,0.41},{0.41,0.2255},{0.41,0.},{0.41,-0.2255},{0.2255,-0.41},{0.,-0.41},{-0.2255,-0.41},{-0.41,-0.2255},{-0.41,0.},{-0.41,0.2255},{-0.2255,0.41},{0.,0.41}}],White,FilledCurve@BezierCurve[.6#&/@{{0.,0.41},{0.2255,0.41},{0.41,0.2255},{0.41,0.},{0.41,-0.2255},{0.2255,-0.41},{0.,-0.41},{-0.2255,-0.41},{-0.41,-0.2255},{-0.41,0.},{-0.41,0.2255},{-0.2255,0.41},{0.,0.41}}]},ImageSize->{30,30}];

WLDraw`$dwIconWireframe = Graphics[{AbsoluteThickness[1.5],White,BSplineCurve[{{-1.,0.},{0.,1},{1.,0.},{1.,0.},{0.,-1},{-1.,0.},{-1.,0.}},SplineClosed->True,SplineDegree->3],BezierCurve[1.2#&/@{{0.,0.41},{0.2255,0.41},{0.41,0.2255},{0.41,0.},{0.41,-0.2255},{0.2255,-0.41},{0.,-0.41},{-0.2255,-0.41},{-0.41,-0.2255},{-0.41,0.},{-0.41,0.2255},{-0.2255,0.41},{0.,0.41}}],BezierCurve[.6#&/@{{0.,0.41},{0.2255,0.41},{0.41,0.2255},{0.41,0.},{0.41,-0.2255},{0.2255,-0.41},{0.,-0.41},{-0.2255,-0.41},{-0.41,-0.2255},{-0.41,0.},{-0.41,0.2255},{-0.2255,0.41},{0.,0.41}}]},ImageSize->{30,30}];

WLDraw`$dwIconAnimate = Graphics[{$dwToolColor,GrayLevel[1,.25],Disk[{1,0}],GrayLevel[1,.5],Disk[{2,0}],GrayLevel[1,.75],Disk[{3,0}],GrayLevel[1,1],Disk[{4,0}]},ImageSize->{24,24}];

WLDraw`$dwIconInsertExpression = Graphics[{$dwToolColor,Style[Rectangle[{-1,-1},{1,1}],Antialiasing->False],Text[Style["E",White,FontSize->Scaled[.75]],{0,0}]},ImageSize->18];

WLDraw`$dwIconRedo = Style["REDO", White, 8];

WLDraw`$dwIconUndo = Style["UNDO", White, 8];

End[] (* End Private Context *)

EndPackage[]