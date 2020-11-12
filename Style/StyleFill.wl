(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwStyleFill[sel_, noStyleMargin_, fontsize_]:=
	DynamicModule[{pos},
		If[MemberQ[{BSplineCurve, BezierCurve, Polygon}, $dwHead[[sel]]] || MemberQ[Flatten[$dwCompoundPathLayers], sel],
			pos = Flatten[Position[$dwStyle[[sel]], FaceForm[_]]][[1]];
			Grid[{{Null,
				
				If[$dwHead[[sel]] =!= Polygon,
					Column[{
						EventHandler[
							PopupMenu[Dynamic@$dwStyle[[sel,1]], {True->Graphics[{Disk[{0,0},{1,.5}]}, ImageSize->30], False->Graphics[{Circle[{0,0},{1,.5}]}, ImageSize->30]}, 
								Appearance->"Palette", Alignment->Center, ImageSize->{2$dwStyleButtonHeight, $dwStyleButtonHeight},
								Enabled->If[MemberQ[{BSplineCurve, BezierCurve}, $dwHead[[sel]]] && FreeQ[Flatten[$dwCompoundPathLayers], sel], True, False]], 
							{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"showFill"])
						}, PassEventsDown->True],
						Style[Dynamic@If[$dwStyle[[sel,1]], "FILLED", "NO FILL"], fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
					}, Alignment->Center, Spacings->.1],
					Nothing
				],
				
				Column[{
					EventHandler[
						ColorSetter[Dynamic[($dwStyle[[sel,pos,1,1]])], 
							ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight+1}], 
						{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"fillColor"])
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
								]; dwUpdateSelected[sel, "StylesToUpdate"->"fillOpacity"]
							)
					}, PassEventsDown->True],
					Style["OPACITY", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				(*If[$dwHead[[sel]] =!= BSplineCurve,*)
					Column[{
						Row[{dwGradientSettings[sel], dwGradientType[sel]}],
						Style["------- GRADIENT -------", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
					}, Alignment->Center, Spacings->.1],
					(*Nothing
				],*)
				
				If[MemberQ[{BSplineCurve}, $dwHead[[sel]]],
					Column[{
						EventHandler[
							PopupMenu[Dynamic@$dwStyle[[sel,10]], {True->Graphics[{Circle[{0,0},{1,.5}]}, ImageSize->30], False->Graphics[{Circle[{0,0},{1,1},{0,-Pi}]}, ImageSize->30]}, 
								Appearance->"Palette", Alignment->Center, ImageSize->{2$dwStyleButtonHeight, $dwStyleButtonHeight}], 
							{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"pathClosed"])
						}, PassEventsDown->True],
						Style[Dynamic@If[$dwStyle[[sel,10]], "CLOSED", "OPEN"], fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
					}, Alignment->Center, Spacings->.1],
					Nothing
				],
				
				If[MemberQ[{BSplineCurve}, $dwHead[[sel]]],
					Column[{
						EventHandler[
							PopupMenu[Dynamic@$dwStyle[[sel,11]], Range[3], 
								Appearance->"Palette", Alignment->Center, ImageSize->{2$dwStyleButtonHeight, $dwStyleButtonHeight}], 
							{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"pathDegree"])
						}, PassEventsDown->True],
						Style["DEGREE", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
					}, Alignment->Center, Spacings->.1],
					Nothing
				],
				
				dwColorPalettes["fill"],
				
				Column[{
					(* include Line for compound paths *)
					Button["L", dwSetUndo[];
						Do[
							If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line}, $dwHead[[s]]],
								pos = Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]];
								$dwStyle[[s,pos,1,1]] = Lighter[$dwStyle[[s,pos,1,1]], .2],
								Nothing
							],
						{s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}];
						dwUpdateGradients[If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]],
						Appearance->"Palette", ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight+1}
					],
					Style["LIGHTER", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					(* include Line for compound paths *)
					Button["D", dwSetUndo[];
						Do[
							If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line}, $dwHead[[s]]],
								pos = Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]];
								$dwStyle[[s,pos,1,1]] = Darker[$dwStyle[[s,pos,1,1]], .2],
								Nothing
							],
						{s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}];
						dwUpdateGradients[If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]],
						Appearance->"Palette", ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight+1}
					],
					Style["DARKER", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1]
				
			}}, Alignment->Top, Spacings->{0, 0}],
		Pane[Style["Fill style settings not available for selected object.", 12, LightGray], ImageMargins->noStyleMargin]]
	]

End[] (* End Private Context *)

EndPackage[]