(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 



(* ---------- examples ---------- *)
dwExampleAngleArrow[]:=
{"Auto position - Rotation arrow","",
dwExampleAngleArrowPreview[],"",
Row[{"1. Choose ", Graphics[{EdgeForm[GrayLevel[.3]],LightGray,Polygon[#*CirclePoints[5]&/@{1,2/3,1/3}]},ImageSize->24]," from the object preset popup menu ", dwObjectPresets[]/.{(ImageSize->_)->(ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth})}, " then click 'OK'."}],
Row[{"2. Choose 'Grid off' from ", dwGridPopupMenu[],"."}],
Row[{"3. Choose 'Show position markers' from ", dwGridPopupMenu[],"."}],
Row[{"4. Click three positions of pentagon:   ",Graphics[{EdgeForm[Gray],FaceForm[GrayLevel[0.8]],Polygon[{{0.118,-0.162},{0.19,0.062},{0.,0.2},{-0.19,0.062},{-0.118,-0.162}}],Black,Line[{{0.118,-0.162},{-0.118,-0.162},{-0.19,0.062}}],Red,{PointSize[Medium],Point[{{0.118,-0.162},{-0.19,0.062},{-0.118,-0.162}}],Black,Inset[Style["1\norigin",LineSpacing->{0,9},FontFamily->"SourceSansPro",FontSize->9],{-0.1625,-0.21875},{0,0}],Inset[Style["2\nstart",LineSpacing->{0,9},FontFamily->"SourceSansPro",FontSize->9],{0.165625,-0.21875},{0,0}],Inset[Style["3\nend",LineSpacing->{0,9},FontFamily->"SourceSansPro",FontSize->9],{-0.25,0.03125},{0,0}],Arrowheads[Small],Gray,Arrow[Table[-{.12,.155}+.15{Cos[x],Sin[x]},{x,0,.6Pi,Pi/20}]]}},ImageSize->72]}],
Row[{"5. Choose ", $dwIconAngleArrow," from the object preset popup menu ", dwObjectPresets[]/.{(ImageSize->_)->(ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth})}, "."}],
"6. Click '2' of 'show segments' to deselect it.
7. Set 'size' to 0.1 then click 'OK'.",
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwExampleSpringPreview[],2/3],"Spring"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleSpring[]}, Appearance->None]
}}, Spacings->{2,0}]
}

dwExampleSpring[]:=
{"Auto position - Spring","",
dwExampleSpringPreview[],"",
Row[{"1. Choose 'Show position markers' from ", dwGridPopupMenu[],"."}],
Row[{"2. Click spring start and end positions on canvas."}],
Row[{"3. Choose ", $dwIconCoilSpring/.{Gray->GrayLevel[.3]}," from the object preset popup menu ", dwObjectLibrary[]/.{(ImageSize->_)->(ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth})}, " then click 'OK'."}],
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwExampleAngleArrowPreview[],2/3],"Rotation angle"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleAngleArrow[]}, Appearance->None]
}}, Spacings->{2,0}]
}



(* ---------- previews ---------- *)
dwExampleAngleArrowPreview[]:=Graphics[{{EdgeForm[GrayLevel[0]],FaceForm[GrayLevel[0.8]],Polygon[{{0.118,-0.162},{0.19,0.062},{0.,0.2},{-0.19,0.062},{-0.118,-0.162}}]},{Arrowheads[Small],Arrow[{{-0.012,-0.164},{-0.013,-0.154},{-0.015,-0.144},{-0.017,-0.134},{-0.021,-0.124},{-0.026,-0.114},{-0.032,-0.106},{-0.038,-0.098},{-0.046,-0.09},{-0.054,-0.084},{-0.062,-0.078},{-0.072,-0.073},{-0.082,-0.069},{-0.092,-0.067},{-0.102,-0.065},{-0.113,-0.065},{-0.123,-0.065},{-0.133,-0.067},{-0.143,-0.07}}]}},ImageSize->80]
dwExampleSpringPreview[]:=Graphics[{{GrayLevel[0],AbsolutePointSize[4],AbsoluteThickness[4],Opacity[1],Dashing[{}],CapForm["Butt"],JoinForm["Round"],Arrowheads[Medium],EdgeForm[{GrayLevel[0],Opacity[1],AbsoluteThickness[4],Dashing[{}],CapForm["Butt"],JoinForm["Round"]}],FaceForm[GrayLevel[0.8]],Line[{{0.004205702904397368,-0.0012238585367082833},{0.06128729176919477,0.017261059982729036},{0.10700935188713362,-0.031000235803976168},{0.12451379804526201,0.10080352808180304},{0.21595791828113964,0.00428093650839261},{0.23346236443926804,0.1360847003941718},{0.3249064846751457,0.03956210882076141},{0.34241093083327406,0.1713658727065406},{0.43385505106915173,0.07484328113313019},{0.4513594972272801,0.2066470450189094},{0.5428036174631579,0.11012445344549901},{0.5515558405422221,0.17602633538838858},{0.6086374294070194,0.1945112539078259}}]},{GrayLevel[0],AbsolutePointSize[4],AbsoluteThickness[2],Opacity[1],Dashing[{}],CapForm["Round"],JoinForm["Round"],Arrowheads[Medium],StrokeForm[GrayLevel[1]],EdgeForm[{GrayLevel[1],Opacity[1],AbsoluteThickness[2],Dashing[{}],CapForm["Round"],JoinForm["Round"]}],FaceForm[GrayLevel[0.8]],Line[{{0.004205702904397368,-0.0012238585367082833},{0.06128729176919477,0.017261059982729036}}],Line[{{0.10700935188713362,-0.031000235803976168},{0.12451379804526201,0.10080352808180304}}],Line[{{0.21595791828113964,0.00428093650839261},{0.23346236443926804,0.1360847003941718}}],Line[{{0.3249064846751457,0.03956210882076141},{0.34241093083327406,0.1713658727065406}}],Line[{{0.43385505106915173,0.07484328113313019},{0.4513594972272801,0.2066470450189094}}],Line[{{0.5428036174631579,0.11012445344549901},{0.5515558405422221,0.17602633538838858}}],Line[{{0.5515558405422221,0.17602633538838858},{0.6086374294070194,0.1945112539078259}}]}},ImageSize->100]

End[] (* End Private Context *)

EndPackage[]