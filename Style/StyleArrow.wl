(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwStyleArrow[sel_, noStyleMargin_, fontsize_]:=
	DynamicModule[{pos},
		If[MemberQ[{BSplineCurve, BezierCurve, Line, Arrow}, $dwHead[[sel]]],
			pos = Flatten[Position[$dwStyle[[sel]], Arrowheads[_]]][[1]];
			Grid[{{Null,
				
				If[$dwHead[[sel]] =!= Arrow,
					Column[{
						EventHandler[
							PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel,2]]]], {True,False}, 
								Appearance->"Palette", Alignment->Center, ImageSize->{3$dwStyleButtonHeight, $dwStyleButtonHeight},
								Enabled->If[!$dwStyle[[sel,1]] || $dwHead[[sel]] === Line, True, False]], 
							{"MouseClicked":>(dwUpdateSelected[sel, "StylesToUpdate"->"showArrows"])
						}, PassEventsDown->True],
						Style["ARROWHEADS", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
					}, Alignment->Center, Spacings->.1],
					Nothing
				],
				
				Column[{
					ActionMenu[Dynamic[If[$dwSelected === {}, "", 
							If[MemberQ[{Small,Medium,Large}, $dwStyle[[sel,3]]],
							Graphics[{Arrowheads[{{$dwStyle[[sel,3]],1}}],Arrow[{{0,.5},{1,.5}}]},ImageSize->36],
							Show[$dwStyle[[sel,3]],Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.8,.8}},ImageSize->36]
					]]],
						{
							Row[{Graphics[{Arrowheads[{{Small,1}}],Arrow[{{0,.5},{1,.5}}]},ImageSize->36],Style[" SMALL", 8, Hue[.58]]}]:>($dwStyle[[sel,3]] = Small; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], Small, $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwSMArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.3,.3}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwSMArrowhead; $dwStyle[[sel,6]] = $dwSMArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwSMArrowhead, $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Row[{Graphics[{Arrowheads[{{Medium,1}}],Arrow[{{0,.5},{1,.5}}]},ImageSize->36],Style[" MEDIUM", 8, Hue[.58]]}]:>($dwStyle[[sel,3]] = Medium; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], Medium, $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwMLArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.5,.5}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwMLArrowhead; $dwStyle[[sel,6]] = $dwMLArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwMLArrowhead, $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Row[{Graphics[{Arrowheads[{{Large,1}}],Arrow[{{0,.5},{1,.5}}]},ImageSize->36],Style[" LARGE", 8, Hue[.58]]}]:>($dwStyle[[sel,3]] = Large; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], Large, $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwXLArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.7,.7}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwXLArrowhead; $dwStyle[[sel,6]] = $dwXLArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwXLArrowhead, $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwXXLArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.8,.8}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwXXLArrowhead; $dwStyle[[sel,6]] = $dwXXLArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwXXLArrowhead, $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Delimiter,
							Show[$dwSShortArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.2,.2}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwSShortArrowhead; $dwStyle[[sel,6]] = $dwSShortArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwSMShortArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.3,.3}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwSMShortArrowhead; $dwStyle[[sel,6]] = $dwSMShortArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwMShortArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.4,.4}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwMShortArrowhead; $dwStyle[[sel,6]] = $dwMShortArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwMLShortArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.5,.5}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwMLShortArrowhead; $dwStyle[[sel,6]] = $dwMLShortArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwLShortArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.6,.6}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwLShortArrowhead; $dwStyle[[sel,6]] = $dwLShortArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwXLShortArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.7,.7}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwXLShortArrowhead; $dwStyle[[sel,6]] = $dwXLShortArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwXXLShortArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.8,.8}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwXXLShortArrowhead; $dwStyle[[sel,6]] = $dwXXLShortArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Delimiter,
							Show[$dwSOpenArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.2,.2}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwSOpenArrowhead; $dwStyle[[sel,6]] = $dwSOpenArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwSMOpenArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.3,.3}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwSMOpenArrowhead; $dwStyle[[sel,6]] = $dwSMOpenArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwMOpenArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.4,.4}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwMOpenArrowhead; $dwStyle[[sel,6]] = $dwMOpenArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwMLOpenArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.5,.5}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwMLOpenArrowhead; $dwStyle[[sel,6]] = $dwMLOpenArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwLOpenArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.6,.6}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwLOpenArrowhead; $dwStyle[[sel,6]] = $dwLOpenArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwXLOpenArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.7,.7}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwXLOpenArrowhead; $dwStyle[[sel,6]] = $dwXLOpenArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"]),
							Show[$dwXXLOpenArrowhead,Graphics[Line[{{-1,0},{0,0}}]],PlotRange->{Automatic,{-.8,.8}},ImageSize->36]:>($dwStyle[[sel,3]] = $dwXXLOpenArrowhead; $dwStyle[[sel,6]] = $dwXXLOpenArrowhead; $dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadSize"])
						}, Appearance->"Palette", Alignment->Left, ImageSize->{3$dwStyleButtonHeight, $dwStyleButtonHeight}],
					Style["SIZE", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					EventHandler[
						Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel,pos,1]]]],
							{
								{}->Graphics[{Arrowheads[{}], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{64,18}],
								
								If[Head[$dwStyle[[sel,3]]] === Graphics,
									{{0.000001, $dwStyle[[sel,5]], $dwStyle[[sel,6]]}},
									If[MemberQ[{Small,Medium,Large},$dwStyle[[sel,pos,1]]],
										$dwStyle[[sel,3]],
										{{$dwStyle[[sel,3]], $dwStyle[[sel,5]]}}
									]
								]->Graphics[{Arrowheads[{{Medium,1}}], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{64,18}],
								
								If[Head[$dwStyle[[sel,3]]] === Graphics,
									{{-0.000001, $dwStyle[[sel,4]], $dwStyle[[sel,6]]}},
									{{-$dwStyle[[sel,3]], $dwStyle[[sel,4]]}}
								]->Graphics[{Arrowheads[{{-Medium, 0}}], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{64,18}],
								
								If[Head[$dwStyle[[sel,3]]] === Graphics,
									{{-0.000001, $dwStyle[[sel,4]], $dwStyle[[sel,6]]}, {0.000001, $dwStyle[[sel,5]], $dwStyle[[sel,6]]}},
									{{-$dwStyle[[sel,3]],$dwStyle[[sel,4]]}, {$dwStyle[[sel,3]],$dwStyle[[sel,5]]}}
								]->Graphics[{Arrowheads[{{-Medium,0},{Medium,1}}], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{64,18}],
								
								If[Head[$dwStyle[[sel,3]]] === Graphics,
									Table[{0.000001, $dwStyle[[sel,4]] + ($dwStyle[[sel,5]]-$dwStyle[[sel,4]])*(n/$dwStyle[[sel,12]]), $dwStyle[[sel,6]]}, {n, $dwStyle[[sel,12]]}],
									Table[{$dwStyle[[sel,3]], $dwStyle[[sel,4]] + ($dwStyle[[sel,5]]-$dwStyle[[sel,4]])*(n/$dwStyle[[sel,12]])}, {n, $dwStyle[[sel,12]]}]
								]->Graphics[{Arrowheads[Table[{Medium, n/3}, {n, 3}]], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{64,18}]
								
							}, Appearance->"Palette", Alignment->Center, ImageSize->{66, $dwStyleButtonHeight}
						]],{
							"MouseUp":>($dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadPosition"])
						}, PassEventsDown->True],
						Style["POSITION", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Column[{
					EventHandler[
						Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[sel,12]]]], Join[Range[3,10], Range[15,50,5]], 
							Appearance->"Palette", Alignment->Center, ImageSize->{66, $dwStyleButtonHeight}]], 
						{"MouseUp":>(
							($dwStyle[[sel,pos,1]]) = 
								If[Head[$dwStyle[[sel,3]]] === Graphics,
									Table[{0.000001, $dwStyle[[sel,4]] + ($dwStyle[[sel,5]]-$dwStyle[[sel,4]])*(n/$dwStyle[[sel,12]]), $dwStyle[[sel,6]]}, {n, $dwStyle[[sel,12]]}],
									Table[{$dwStyle[[sel,3]], $dwStyle[[sel,4]] + ($dwStyle[[sel,5]]-$dwStyle[[sel,4]])*(n/$dwStyle[[sel,12]])}, {n, $dwStyle[[sel,12]]}]
								];
							dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadPosition"]
						)
					}, PassEventsDown->True],
					Style["QUANTITY", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Spacer[10],
				Column[{
					EventHandler[
						Slider[Dynamic[$dwStyle[[sel,4]]], {0, 1, .01}, ContinuousAction->False, ImageSize->Tiny], 
						{"MouseUp":>($dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadStart"])
					}, PassEventsDown->True],
					Style["START", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				Spacer[10],
				Column[{
					EventHandler[
						Slider[Dynamic[$dwStyle[[sel,5]]], {0, 1, .01}, ContinuousAction->False, ImageSize->Tiny], 
						{"MouseUp":>($dwStyle[[sel,pos,1]] = dwChangeArrow[$dwStyle[[sel,pos,1]], $dwStyle[[sel,3]], $dwStyle[[sel,4]], $dwStyle[[sel,5]], $dwStyle[[sel,6]], $dwStyle[[sel,12]]]; dwUpdateSelected[sel, "StylesToUpdate"->"arrowheadStart"])
					}, PassEventsDown->True],
					Style["END", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
				}, Alignment->Center, Spacings->.1],
				
				If[MemberQ[{Arrow}, $dwHead[[sel]]],
					Column[{
						Row[{dwLineGradientSettings[sel], dwGradientType[sel]}],
						Style["---- LINE GRADIENT ----", fontsize, $dwStyleControlHeadColor, ShowStringCharacters->False]
					}, Alignment->Center, Spacings->.1],
					Nothing
				]
				
			}}, Spacings->{0, 0}],
		Pane[Style["Arrow style settings not available for selected object.", 12, LightGray], ImageMargins->noStyleMargin]]
	]

End[] (* End Private Context *)

EndPackage[]