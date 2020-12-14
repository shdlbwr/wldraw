(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwHelp[]:=
	Tooltip[
		Button[Style["?",18,White],
			$dwHelpTopic = dwHelpTopic[][[1]];
			CreateDialog[
				Pane[
					Style[Grid[{{
						Framed[Column[{
							Column[{
								Button[Style["Overview", LightGray], $dwHelpTopic = dwHelpTopic[][[1]], ImageSize->{132,22}, Background->Hue[.58, .9, .9], $dwButtonStyle, FrameMargins->0],
								Button[Style["Quick start", LightGray], $dwHelpTopic = {Null, dwHelpQuickStart[]}, ImageSize->{132,22}, $dwButtonStyle, FrameMargins->0],
								Button[Style["Examples", LightGray], $dwHelpTopic = {Null, dwHelpExamples[]}, ImageSize->{132,22}, Background->GrayLevel[.4], $dwButtonStyle, FrameMargins->0, ImageMargins->{{0,0},{5,0}}]
							}, Spacings->0],
							(* tool button grid - used Dynamic method below to hide tools on first appearance because Dynamics on entire Column was slow *)
							Dynamic@If[$dwHelpTools === "",
								Button[Style["Show tools", LightGray], 
									$dwHelpTools = 
										Grid[
											Partition[
												(If[Head[#] === List,
													Button[#[[1]], $dwHelpTopic = {#[[1]], #[[2]]}, $dwButtonStyle, FrameMargins->0],
													#
												]&/@Join[
														{Style["\n-----CANVAS------", 12, LightGray, LineSpacing->{0,6}], SpanFromLeft, SpanFromLeft, SpanFromLeft}, dwHelpTopic[], dwHelpCanvasTools[], Table[Button["", Null, $dwButtonStyle], 1], 
														{Style["\n--DRAW / OBJECTS--", 12, LightGray, LineSpacing->{0,6}], SpanFromLeft, SpanFromLeft, SpanFromLeft}, dwHelpObjects[], 
														{Style["\n------TOOLS------", 12, LightGray, LineSpacing->{0,6}], SpanFromLeft, SpanFromLeft, SpanFromLeft}, dwHelpObjectTools[], {Button["", Null, $dwButtonStyle]}, dwHelpObjectPointTools[]
													])
											,4,4,1,{}],
										Spacings->{0,0}], 
									ImageSize->{132,22}, $dwButtonStyle, FrameMargins->0],
								Dynamic@$dwHelpTools
							],
							(* style navigation buttons *)
							Dynamic@If[$dwHelpTools === "",
								Column[{
									dwHelpNavigationButtons[],
									Graphics[{}, ImageSize->{10, 526}](* vertical size of empty space when tools not visible *)
								}],
								dwHelpNavigationButtons[]
							]
						}, Alignment->Center, Spacings->0], FrameStyle->Gray],
						Spacer[10],
						Dynamic@Column[{
							Row[{
								If[$dwHelpTopic[[1]] === Null, 
									Nothing, 
									Row[{Button[$dwHelpTopic[[1]], Null, 
											If[MemberQ[{"Preset objects","Draw path","Text","Distort 2D object to axonometric 3D","Auto draw","Image","World shapes","Pattern objects"}, $dwHelpTopic[[2,1]]], 
												ImageSize->{66,33}, 
												{}
											], 
										Sequence@@$dwButtonStyle, FrameMargins->0], 
								Spacer[5]
								}]], Style[$dwHelpTopic[[2,1]], 18, Bold]}],
							Sequence@@$dwHelpTopic[[2]][[2;;-1]]}, Dividers->{None, 2->{AbsoluteThickness[3], GrayLevel[1]}}]
					}}, Alignment->{Left, Top}, Background->{{$dwButtonBackgroundColor, None}}, Spacings->{0,0}], FontFamily->"Arial", 11, LineSpacing->{0,14}],
				ImageSize->{610, 685}],
			WindowTitle->"WL-Draw Help", Deployed->True, Background->$dwHelpBackgroundColor];
			$dwShowHelpTools = True,
		Background->Hue[.58, .9, .9], BaselinePosition->Scaled[.4], $dwButtonStyle],
	"Help topics"]
	
dwHelpNavigationButtons[]:=
	Framed[
		Pane[
			Row[Button[#,$dwHelpTopic = {GraphicsGrid[Partition[{Graphics[{EdgeForm[LightGray], Opacity[0], Rectangle[]}, ImageSize -> {18, 18}],Graphics[{LightGray, Arrowheads[Small], Arrow[{{0, 0}, {1, 1}}]}, ImageSize -> {18, 18}],Graphics[{LightGray,PointSize[Large], Point[{0, 0}]}],Style["T",12,LightGray]},2],ImageSize->24,Spacings->Scaled[.25]], dwHelpStyleMode[]}, Appearance->"Frameless"]&/@{$dwIconStyleFill,$dwIconStyleStroke,$dwIconStyleArrow,$dwIconStylePoint,$dwIconStyleText,$dwIconStyleImage}], 
		Alignment->Center, Background->Gray, ImageSize->{Automatic(*114*),23}],
	Background->Gray, FrameMargins->{{8,8},{5,5}}, FrameStyle->Gray]
		
dwHelpQuickStart[]:=
	{"Quick start", "",
Style["Create and style an object", Sequence@@$dwHelpSubheadStyle],
Row[{"1. Choose ", Graphics[{Gray,Polygon[{{0,0},{1,0},{1,1},{0,1}}]},ImageSize->24]," from the object preset popup menu ", dwObjectPresets[]/.{(ImageSize->_)->(ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth})}}],
Row[{"2. Choose Line from the object type popup menu ", dwHead[]}],
Row[{"3. Click the stroke style color button ", Dynamic[If[$dwSelected==={}||$dwStyleMode=!="stroke",ColorSetter[ImageSize->{48,24}],ColorSetter[Dynamic[($dwStyle[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1, 1]], $dwSelected[[1]]],Flatten[Position[$dwStyle[[If[Length[$dwSelected[[1]]] > 1, $dwSelected[[1, 1]], $dwSelected[[1]]]]], StrokeForm[_]]][[1]]]][[1,1]])], ImageSize->{48,24}]]], " to choose a new color."}],
Row[{"4. Choose ", Graphics[{Arrowheads[Medium],Arrow[{{0,.1},{1,.1}}]},ImageSize->36]," from the style preset popup menu ", dwStylePresets[], " to add an arrow head."}],
"","","",
Style["Move or delete points", Sequence@@$dwHelpSubheadStyle],
Row[{"1. Click ", Button[$dwIconPointSelect, dwConvertLayerSelectionToPointSelection[], $dwButtonStyle, FrameMargins->0], " then click an object to display its points."}],
"2. Click a black point. The point becomes a red circle to show it is selected.",
"3. Drag the selected point to a new location. The point snaps to the grid as you drag.",
Row[{"4. Click ", Button[$dwIconObjectDelete, Null, $dwButtonStyle, FrameMargins->0], " to delete selected points."}],
"","","",
Style["Change grid spacing", Sequence@@$dwHelpSubheadStyle],
Row[{"Choose 'Grid off' from ", dwGridPopupMenu[], " to turn off grid."}],
"","","",
Style["Copy objects and paste into another notebook", Sequence@@$dwHelpSubheadStyle],
Row[{StringTake[$dwCommandKey, {1,-4}]<>"-click ", dwSetBoundary[], 
" to set object boundary then click ", Button[$dwIconClipboard, CopyToClipboard[dwWLDrawToGraphics[]],$dwButtonStyle], 
" to copy to clipboard."}],
"Paste into new notebook. Drag selection box to resize or Show[<graphics>, ImageSize->200].
Use Magnify[<graphics>, 2] to resize the line weights, fonts, arrowheads, etc. with the size.",
"","","",
Style["What's next? Explore the tools", Sequence@@$dwHelpSubheadStyle],
"Click a tool icon at left to view its documentation. Click 'Show tools' if icons are missing."
}
		
dwHelpTopic[]:=
	{
		{Graphics[{Text[Style["?",18,White]]},Background->Hue[.58, .9, .9],$dwButtonStyle], dwHelpOverview[]}
	}
	
dwHelpOverview[]:= {"Overview", "",
Grid[{
	{
		Graphics[{
			GrayLevel[.5], Polygon[{{-1., 0.75}, {1., 0.75}, {1., 1.}, {-1., 1.}}],
			GrayLevel[.4], Polygon[{{-1., 0.6}, {1., 0.6}, {1., 0.75}, {-1., 0.75}}],
			GrayLevel[.3], Polygon[{{-1., -1.}, {-0.6, -1.}, {-0.6, 0.6}, {-1., 0.6}}],
			GrayLevel[.9], Polygon[{{-0.6, -1.}, {1., -1.}, {1., 0.6}, {-0.6, 0.6}}],
			(*GrayLevel[.5], Polygon[{{.9, -1.}, {1., -1.}, {1., -.9}, {.9, -.9}}],*)
			{EdgeForm[{$dwPlotRangeColor}], Opacity[0], Rectangle[{-.3, -.6}, {.7, .2}]},
			White,
			Inset[Style["selected object style", 10], {0., 0.875}], 
			Inset[Style["canvas interaction tools", 10], {0., 0.675}], 
			Inset[Style["draw\ntools", 10], {-0.8, -0.1}], 
			Black, Inset[Style["canvas for\ndrawing objects", 10], {0.2, -0.2}],
			Text[Style["boundary", 10], {0.2, 0.35}, {0, -1}], Arrowheads[Small], Arrow[{{.2, .35}, {.2, .2}}]}, 
		ImageSize -> 200],
		Column[{
			Grid[{
				Button[#[[1]], $dwHelpTopic = #, $dwButtonStyle, FrameMargins->0]&/@{
					{$dwIconAddPoint, dwHelpDraw[]},
					{$dwIconObjectSelect, dwHelpObjectSelection[]},
					{$dwIconPointSelect, dwHelpPointSelection[]},
					{$dwIconPlotRange, dwHelpBoundary[]},
					{$dwIconClipboard, dwHelpCopy[]}
				}}, Spacings->.5],
			Style["Start by learning to use these tools.", 10, Italic],
			Spacer[{1,20}],
			Graphics[{
				EdgeForm[Black],GrayLevel[.8],Polygon[{{-1.,-.5},{1.,-.5},{1.,.5},{-1.,.5}}],
				Hue[.58],Line[{{-1.1,-.6},{1.1,-.6},{1.1,.6},{-1.1,.6},{-1.1,-.6}}],
				EdgeForm[Hue[.58]],White,Disk[#,.075]&/@{{-1.1,-.6},{0,-.6},{1.1,-.6},{1.1,0},{1.1,.6},{-1.1,.6},{0,.6},{-1.1,-.6},{-1.1,0}},
				Black,Arrowheads[{-Small,Small}],
				Arrow[Table[{1.1,.6}+.3{Sin[x],Cos[x]},{x,0,Pi/2,Pi/20}]],
				Arrow[Table[{-1.1,.6}+.3{Sin[x],Cos[x]},{x,0,-Pi/2,-Pi/20}]],
				Arrow[Table[{1.1,-.6}+.3{Sin[x],Cos[x]},{x,Pi/2,Pi,Pi/20}]],
				Arrow[Table[{-1.1,-.6}+.3{Sin[x],Cos[x]},{x,-Pi/2,-Pi,-Pi/20}]],
				Arrow[{{1.5,0},{.7,0}}],Arrow[-{{1.5,0},{.7,0}}],
				Arrow[{{0,1},{0,.2}}],Arrow[-{{0,1},{0,.2}}],
				Text["rotate",{-1.2,-1.1}],Text["scale",{0,-1.1}],Text["rotate",{1.2,-1.1}],
				Text["rotate",{-1.2,1.1}],Text["scale",{0,1.1}],Text["rotate",{1.2,1.1}],
				Rotate[Text["scale",{-1.6,0}],Pi/2],Rotate[Text["scale",{1.6,0}],Pi/2],
				Text["object",{0,0}]
			},ImageSize->160]
		}]
	},{
		Style["The window is divided into four sections.\nBoundary is trim area of final graphic.", 11, Italic],
		Style["Rotate by dragging the corner of the blue\nselection box. Scale by dragging midpoint.", 11,Italic]
	}}, Alignment->Left, Spacings->{2, Automatic}],
	"",
Style["WL-Draw is a vector drawing program to use within the Wolfram Language. 
All tools contain tooltips with the tool name and instructions if needed. 
Click the tool buttons at left for more detailed information.", 12, LineSpacing->{0,18,1}],
"",
Style["Features", Sequence@@$dwHelpSubheadStyle],
"- Preset objects and styles
- Animation and isometric drawing tools
- Pan and zoom canvas while drawing
- Multi-level image filtering
- Full array of selection tools",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Move canvas anytime by pressing "<>StringTake[$dwCommandKey, {1,-4}]<>" while dragging canvas.
- Zoom anytime by pressing "<>StringTake[$dwCommandKey, {1,-4}]<>" and "<>StringTake[$dwOptionKey, {1,-4}]<>" with vertical canvas drag.
- Message bar at bottom of window displays info such as distance and quantities.
- Measure distance|angle by clicking two canvas positions.",
Row[{"- Click ", Button[$dwIconObjectSelect, Null, $dwButtonStyle, FrameMargins->0]," to select objects. Click ",Button[$dwIconPointSelect, Null, $dwButtonStyle, FrameMargins->0]," to select object points."}],
Row[{"- Click ", Button[$dwIconPreview, Null, $dwButtonStyle, FrameMargins->0]," to toggle between preview and wireframe mode."}],
"- Work in wireframe mode for a responsive interface when creating illustrations.
- Menu bar choices and keyboard shortcuts are not supported."
}

dwHelpStyleMode[]:= {"Style navigation", "",
"The style section displays settings for a selected object.
A row of navigation icons is provided to help navigate the style settings.
Click a navigation icon to reveal its settings. The clicked icon is highlighted white.
The navigation icons include:",
Row[{Spacer[{40, 40}],
	Framed[
		GraphicsGrid[{{Labeled[$dwIconStyleFill, "Fill"], Labeled[$dwIconStyleStroke, "Stroke"], Labeled[$dwIconStyleArrow, "Arrow"], Labeled[$dwIconStylePoint, "Point"], Labeled[$dwIconStyleText, "Text"], Labeled[$dwIconStyleImage, "Image"]}}, ImageSize->220], 
	Background->GrayLevel[.8], FrameStyle->GrayLevel[.8]]
}],
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
Column[{"- Choose a style preset from the PRESETS menu.
- Arrows may be used by BezierCurve and BSplineCurve when not filled.
- Stretch a fill gradient over several objects by making the objects a compound path.
- Choose a color palette from the popup menu then click a palette color.",
Framed[dwColorPalettes["fill",Enabled->False], Background->$dwButtonBackgroundColor, FrameStyle->$dwButtonBackgroundColor]}]
}

End[] (* End Private Context *)

EndPackage[]