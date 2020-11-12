(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwStylePoint[sel_, noStyleMargin_, fontsize_]:=
	DynamicModule[{pos},
		If[MemberQ[{Point}, $dwHead[[sel]]],
		pos = Flatten[Position[$dwStyle[[sel]], AbsolutePointSize[_]]][[1]];
			Grid[{{Null,
				
				Column[{
					EventHandler[
						ColorSetter[Dynamic[($dwStyle[[sel,$dwStyleStart]])], 
							ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight}], 
						{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"pointColor"])
					}, PassEventsDown->True],
					Style["COLOR", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					EventHandler[
						InputField[Dynamic[($dwStyle[[sel,$dwStyleStart+1,1]])], Number, ImageSize->{50,$dwStyleButtonHeight-2}], 
						{"KeyDown":>(
							$dwStyle[[sel,$dwStyleStart+1,1]] = 
								If[NumberQ[$dwStyle[[sel,$dwStyleStart+1,1]]],
									If[0 <= $dwStyle[[sel,$dwStyleStart+1,1]] <= 1, $dwStyle[[sel,$dwStyleStart+1,1]], 1],
									1
								]; dwUpdateSelected[sel, "StylesToUpdate"->"pointOpacity"]
							)
					}, PassEventsDown->True],
					Style["OPACITY", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					EventHandler[
					InputField[Dynamic[($dwStyle[[sel,7]])], Number, ImageSize->{50,$dwStyleButtonHeight-2}], 
					{"KeyDown":>(
						$dwStyle[[sel,7]] = 
							If[NumberQ[$dwStyle[[sel,7]]],
								If[$dwStyle[[sel,7]] >= 0, $dwStyle[[sel,7]], $dwPointSize],
								$dwPointSize
							]; $dwStyle[[sel,pos,1]] = $dwStyle[[sel,7]]; dwUpdateSelected[sel, "StylesToUpdate"->"pointSize"]
						)
				}, PassEventsDown->True],
					Style["SIZE", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				dwColorPalettes["point"],
				
				Column[{
					Button["L", dwSetUndo[];
						Do[
							If[MemberQ[{Point}, $dwHead[[s]]],
								$dwStyle[[s,$dwStyleStart]] = Lighter[$dwStyle[[s,$dwStyleStart]], .2],
								Nothing
							],
						{s, $dwSelected}],
						Appearance->"Palette", ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight+1}
					],
					Style["LIGHTER", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					Button["D", dwSetUndo[];
						Do[
							If[MemberQ[{Point}, $dwHead[[s]]],
								$dwStyle[[s,$dwStyleStart]] = Darker[$dwStyle[[s,$dwStyleStart]], .2],
								Nothing
							],
						{s, $dwSelected}],
						Appearance->"Palette", ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight+1}
					],
					Style["DARKER", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1]
				
			}}, Alignment->Top, Spacings->{0, 0}],
		Pane[Style["Point style settings not available for selected object.", 12, LightGray], ImageMargins->noStyleMargin]]
	]

End[] (* End Private Context *)

EndPackage[]