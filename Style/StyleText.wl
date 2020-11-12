(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwStyleText[sel_, noStyleMargin_, fontsize_]:=
	If[MemberQ[{Text, "Text3D"}, $dwHead[[sel]]],
		Grid[{{
			Null,
			Column[{
				EventHandler[
					ColorSetter[Dynamic[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontColor]][[1]]]][[2]]], 
						ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight}], 
					{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"fontColor"])
				}, PassEventsDown->True],
				Style["COLOR", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
			}, Alignment->Center, Spacings->.1],
			Column[{
				EventHandler[
					Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontFamily]][[1]]]][[2]]]], 
						Join[$dwFontFamilies, {Delimiter}, $dwStylesheetTextFormats], 
						Appearance->"Palette", Alignment->Center, ImageSize->{132, $dwStyleButtonHeight}]], 
					{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"fontFamily"])
				}, PassEventsDown->True],
				Style["FAMILY", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
			}, Alignment->Center, Spacings->.1],
			Column[{
				EventHandler[
					Dynamic[InputField[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontSize]][[1]]]][[2]]]], Number, ImageSize->{50,$dwStyleButtonHeight-2}]], 
					{"KeyDown":>(
						$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontSize]][[1]]]][[2]] = 
							If[NumberQ[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontSize]][[1]]]][[2]]],
								Abs[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontSize]][[1]]]][[2]]],
								11
							]; dwUpdateSelected[sel, "StylesToUpdate"->"fontSize"]
						)
				}, PassEventsDown->True],
				Style["SIZE", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
			}, Alignment->Center, Spacings->.1],
			Column[{
				EventHandler[
					Dynamic[InputField[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], LineSpacing]][[1]]]][[2]][[2]]]], Number, ImageSize->{50,$dwStyleButtonHeight-2}]], 
					{"KeyDown":>(
						$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], LineSpacing]][[1]]]][[2]][[2]] = 
							If[NumberQ[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], LineSpacing]][[1]]]][[2]][[2]]],
								Abs[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], LineSpacing]][[1]]]][[2]][[2]]],
								12
							];
						dwUpdateSelected[sel, "StylesToUpdate"->"fontLeading"]
					)
				}, PassEventsDown->True],
				Style["LEADING", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
			}, Alignment->Center, Spacings->.1],
			Column[{
				EventHandler[
					Dynamic[InputField[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontOpacity]][[1]]]][[2]]]], Number, ImageSize->{50,$dwStyleButtonHeight-2}]], 
					{"KeyDown":>(
						$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontOpacity]][[1]]]][[2]] = 
							If[NumberQ[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontOpacity]][[1]]]][[2]]],
								Abs[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontOpacity]][[1]]]][[2]]],
								1
							];
						dwUpdateSelected[sel, "StylesToUpdate"->"fontOpacity"]
					)
				}, PassEventsDown->True],
				Style["OPACITY", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
			}, Alignment->Center, Spacings->.1],
			Column[{
				Dynamic[ActionMenu[If[$dwSelected === {}, "",
						Dynamic[Switch[$dwStyle[[sel, 3]],
							{0,0},"Center",
							{-1,0},"Left",
							{1,0},"Right",
							{0,1},"Top",
							{0,-1},"Bottom",
							{-1,1},"TopLeft",
							{1,1},"TopRight",
							{-1,-1},"BottomLeft",
							{1,-1},"BottomRight",
							_,"Irregular"
						]]],
					{
						"Center":>($dwStyle[[sel, 3]] = {0,0}; $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], TextAlignment]][[1]]]][[2]] = Center; dwUpdateBoundingBox[{sel}]; dwUpdateSelected[sel, "StylesToUpdate"->"fontAlignment"]),
						"Left":>($dwStyle[[sel, 3]] = {-1,0}; $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], TextAlignment]][[1]]]][[2]] = Left; dwUpdateBoundingBox[{sel}]; dwUpdateSelected[sel, "StylesToUpdate"->"fontAlignment"]),
						"Right":>($dwStyle[[sel, 3]] = {1,0}; $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], TextAlignment]][[1]]]][[2]] = Right; dwUpdateBoundingBox[{sel}]; dwUpdateSelected[sel, "StylesToUpdate"->"fontAlignment"]),
						"Top":>($dwStyle[[sel, 3]] = {0,1}; $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], TextAlignment]][[1]]]][[2]] = Center; dwUpdateBoundingBox[{sel}]; dwUpdateSelected[sel, "StylesToUpdate"->"fontAlignment"]),
						"Bottom":>($dwStyle[[sel, 3]] = {0,-1}; $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], TextAlignment]][[1]]]][[2]] = Center; dwUpdateBoundingBox[{sel}]; dwUpdateSelected[sel, "StylesToUpdate"->"fontAlignment"]),
						"TopLeft":>($dwStyle[[sel, 3]] = {-1,1}; $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], TextAlignment]][[1]]]][[2]] = Left; dwUpdateBoundingBox[{sel}]; dwUpdateSelected[sel, "StylesToUpdate"->"fontAlignment"]),
						"TopRight":>($dwStyle[[sel, 3]] = {1,1}; $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], TextAlignment]][[1]]]][[2]] = Right; dwUpdateBoundingBox[{sel}]; dwUpdateSelected[sel, "StylesToUpdate"->"fontAlignment"]),
						"BottomLeft":>($dwStyle[[sel, 3]] = {-1,-1}; $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], TextAlignment]][[1]]]][[2]] = Left; dwUpdateBoundingBox[{sel}]; dwUpdateSelected[sel, "StylesToUpdate"->"fontAlignment"]),
						"BottomRight":>($dwStyle[[sel, 3]] = {1,-1}; $dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], TextAlignment]][[1]]]][[2]] = Right; dwUpdateBoundingBox[{sel}]; dwUpdateSelected[sel, "StylesToUpdate"->"fontAlignment"])
					},
				Appearance->"Palette", ImageSize->{99, $dwStyleButtonHeight}]],
				Style["ALIGNMENT", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
			}, Alignment->Center, Spacings->.1],
			Column[{
				EventHandler[
					Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontWeight]][[1]]]][[2]]]], 
						{Plain,Bold,Delimiter,"Thin","Light","Medium","SemiBold","Heavy","Black","Fat"}, 
						Appearance->"Palette", Alignment->Center, ImageSize->{66, $dwStyleButtonHeight}]], 
					{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"fontWeight"])
				}, PassEventsDown->True],
				Style["WEIGHT", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
			}, Alignment->Center, Spacings->.1],
			Column[{
				EventHandler[
					Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontSlant]][[1]]]][[2]]]], 
						{Plain,Italic,Delimiter,"Oblique"}, 
						Appearance->"Palette", Alignment->Center, ImageSize->{66, $dwStyleButtonHeight}]], 
					{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"fontSlant"])
				}, PassEventsDown->True],
				Style["SLANT", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
			}, Alignment->Center, Spacings->.1],
			Column[{
				EventHandler[
					Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel, Flatten[Position[$dwStyle[[sel]], FontTracking]][[1]]]][[2]]]], 
						{"Plain","Condensed","Extended",Delimiter,"Narrow","Compressed","SemiCondensed","Wide"}, 
						Appearance->"Palette", Alignment->Center, ImageSize->{99, $dwStyleButtonHeight}]], 
					{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"fontTracking"])
				}, PassEventsDown->True],
				Style["TRACKING", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
			}, Alignment->Center, Spacings->.1]
						
		}}, Alignment->Top, Spacings->{0, 0}],
	Pane[Style["Text style settings not available for selected object.", 12, LightGray], ImageMargins->noStyleMargin]]

End[] (* End Private Context *)

EndPackage[]