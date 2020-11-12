(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwStyleStroke[sel_, noStyleMargin_, fontsize_]:=
	DynamicModule[{pos},
		If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line, Arrow}, $dwHead[[sel]]],
			pos = Flatten[Position[$dwStyle[[sel]], StrokeForm[_]]][[1]];
			Grid[{{Null,
				
				Column[{
					EventHandler[
						ColorSetter[Dynamic[($dwStyle[[sel,pos,1,1]])], 
							ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight+1}], 
						{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"strokeColor"])
					}, PassEventsDown->True],
					Style["COLOR", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					EventHandler[
						InputField[Dynamic[($dwStyle[[sel,pos,1,2,1]])], Number, ImageSize->{50,$dwStyleButtonHeight-1}], 
						{"KeyDown":>(
							$dwStyle[[sel,pos,1,2,1]] = 
								If[NumberQ[$dwStyle[[sel,pos,1,2,1]]],
									If[0 <= $dwStyle[[sel,pos,1,2,1]] <= 1, $dwStyle[[sel,pos,1,2,1]], 1],
									1
								]; dwUpdateSelected[sel, "StylesToUpdate"->"strokeOpacity"]
							)
					}, PassEventsDown->True],
					Style["OPACITY", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					EventHandler[
						InputField[Dynamic[($dwStyle[[sel,pos,1,3,1]])], Number, ImageSize->{50,$dwStyleButtonHeight-1}], 
						{"KeyDown":>(
							$dwStyle[[sel,pos,1,3,1]] = 
								If[NumberQ[$dwStyle[[sel,pos,1,3,1]]],
									If[$dwStyle[[sel,pos,1,3,1]] >= 0, $dwStyle[[sel,pos,1,3,1]], 1],
									1
								]; dwUpdateSelected[sel, "StylesToUpdate"->"strokeThickness"]
							)
					}, PassEventsDown->True],
					Style["THICK", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					ActionMenu[Dynamic[If[$dwSelected === {}, "", Graphics[{CapForm["Butt"], AbsoluteThickness[$dwStyle[[sel,pos,1,3,1]]], AbsoluteDashing[$dwStyle[[sel,pos,1,4,1]]],Line[{{0,.1},{1,.1}}]},ImageSize->{64,18}]]],
						With[{dash = #}, 
							Graphics[{CapForm["Butt"], AbsoluteThickness[$dwStyle[[sel,pos,1,3,1]]], AbsoluteDashing[dash],Line[{{0,.1},{1,.1}}]},ImageSize->{64,18}]:>($dwStyle[[sel,pos,1,4,1]] = dash; $dwStyle[[sel,pos,1,5,1]] = "Butt"; dwUpdateSelected[sel, "StylesToUpdate"->"strokeDashing"])
						]&/@{{}, {18,4}, {12,4}, {8,3}, {4,2}, {2,2}
					}, Appearance->"Palette", Alignment->Center, ImageSize->{66, $dwStyleButtonHeight}],
					Style["DASHING", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					EventHandler[
						Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel,pos,1,5,1]]]], 
							{"Butt", "Round", "Square"}, 
							Appearance->"Palette", Alignment->Center, ImageSize->{66, $dwStyleButtonHeight}]], 
						{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"strokeCapForm"])
					}, PassEventsDown->True],
					Style["CAPS", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					EventHandler[
						Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel,pos,1,6,1]]]], 
							{"Bevel", "Round", "Miter"}, 
							Appearance->"Palette", Alignment->Center, ImageSize->{66, $dwStyleButtonHeight}]], 
						{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"strokeJoinForm"])
					}, PassEventsDown->True],
					Style["JOINS", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				dwColorPalettes["stroke"],
				
				Column[{
					Button["L", dwSetUndo[];
						Do[
							If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line, Arrow}, $dwHead[[s]]],
								pos = Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]];
								$dwStyle[[s,pos,1,1]] = Lighter[$dwStyle[[s,pos,1,1]], .2],
								Nothing
							],
						{s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}],
						Appearance->"Palette", ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight+1}
					],
					Style["LIGHTER", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					Button["D", dwSetUndo[];
						Do[
							If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line, Arrow}, $dwHead[[s]]],
								pos = Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]];
								$dwStyle[[s,pos,1,1]] = Darker[$dwStyle[[s,pos,1,1]], .2],
								Nothing
							],
						{s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}],
						Appearance->"Palette", ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight+1}
					],
					Style["DARKER", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				If[MemberQ[{Line}, $dwHead[[sel]]],
					Column[{
						Row[{dwLineGradientSettings[sel], dwGradientType[sel]}],
						Style["---- LINE GRADIENT ----", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
					}, Alignment->Center, Spacings->.1],
					Nothing
				]
			}}, Alignment->Top, Spacings->{0, 0}],
		Pane[Style["Stroke style settings not available for selected object.", 12, LightGray], ImageMargins->noStyleMargin]]
	]

End[] (* End Private Context *)

EndPackage[]