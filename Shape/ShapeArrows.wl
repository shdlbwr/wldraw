(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapePolygonBentArrow[]:=
	CreateDialog[
		DynamicModule[{arrowLength = 3, arrowWidth = 1, headAngle = 45, shaftWidthFlipped = .5, flange = 0, scale = .25, bendHeight = 1, bendSlant = 1, bendSpread = .12, bendPosition = 1.4, adjustCurve = .1, shaftLength = .5, shaftWidth = .5},
			Pane[
				Grid[{
					{Dynamic@Show[Graphics[{
						(* axes and frame *)
						{GrayLevel[.8], 
							InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}],
							Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}] 
							(*{Thin, Gray, Table[{InfiniteLine[{{n, -1}, {n+2, 1}}], InfiniteLine[{{n, 1}, {n+2, -1}}]}, {n, -3.5, 1.5, .25}]}*)
						},
						(* arrow *)
						shaftLength = Max[(arrowLength - .5arrowWidth*Tan[headAngle Degree])/arrowLength, 0]+$MachineEpsilon;
						shaftWidth = 1 - shaftWidthFlipped;
						GrayLevel[.7],EdgeForm[Black],FilledCurve[BezierCurve[scale{
							(* arrowhead bottom *)
							{arrowLength/2,0},{arrowLength/2,0},
							{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},
							{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
							(* shaft bottom *)
							{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,-bendSpread+adjustCurve*bendHeight,bendSpread],(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,-bendSpread+adjustCurve*bendHeight,bendSpread],(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
							{-arrowLength/2+bendPosition+If[bendHeight>0,-bendSpread-adjustCurve*bendHeight,bendSpread],(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
							{-arrowLength/2+bendPosition+If[bendHeight>0,-bendSpread,bendSpread-adjustCurve*bendHeight],bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
							{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,-bendSpread,adjustCurve*bendHeight+bendSpread],bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
							{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,-bendSpread,adjustCurve*bendHeight+bendSpread],bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
							(* end *)
							{-arrowLength/2,bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
							{-arrowLength/2,bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
							(* shaft top *)
							{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,bendSpread-adjustCurve*bendHeight,-bendSpread],bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
							{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,bendSpread-adjustCurve*bendHeight,-bendSpread],bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
							{-arrowLength/2+bendPosition+If[bendHeight>0,bendSpread+adjustCurve*bendHeight,-bendSpread],bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
							{-arrowLength/2+bendPosition+If[bendHeight>0,bendSpread,-bendSpread+adjustCurve*bendHeight],(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
							{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,bendSpread,-bendSpread-adjustCurve*bendHeight],(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,bendSpread,-bendSpread-adjustCurve*bendHeight],(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
							(* arrowhead top *)
							{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
							{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2},{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2},{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2},
							{arrowLength/2,0},{arrowLength/2,0}
						}]]},
						Background->White,ImageSize->{300,200},PlotRange->{1.5{-1.1,1.1},{-1.1,1.1}}],
						Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]], PlotRange->1.1],SpanFromLeft,SpanFromLeft},
					{"scale ",Slider[Dynamic@scale,{.1,1,.01}],Pane[Dynamic@scale,ImageSize->30]},
					{"length ",Slider[Dynamic@arrowLength,{2,8,.05}],Pane[Dynamic[arrowLength],ImageSize->30]},
					{"thickness ",Slider[Dynamic@shaftWidthFlipped,{0,1,.05}],Pane[Dynamic@shaftWidthFlipped,ImageSize->30]},
					{"head size ",Slider[Dynamic@arrowWidth,{.05,1,.05}],Pane[Dynamic@arrowWidth,ImageSize->30]},
					{"head angle ",Slider[Dynamic@headAngle,{0,89,1}],Pane[Dynamic@headAngle,ImageSize->30]},
					{"head flange ",Dynamic@Slider[Dynamic@flange,{0, shaftWidth((1-shaftLength)((.25arrowLength)/shaftLength))/(.25arrowLength), .01}],Pane[Dynamic@Round[flange,.01],ImageSize->30]},
					{"bend height ",Slider[Dynamic@bendHeight,{-2, 2, .05}],Pane[Dynamic@bendHeight,ImageSize->30]},
					{"bend position ",Slider[Dynamic@bendPosition,{0, Dynamic@arrowLength, .05}],Pane[Dynamic@bendPosition,ImageSize->30]},
					{"bend slant ",Slider[Dynamic@bendSlant,{0, Dynamic@arrowLength, .05}],Pane[Dynamic@bendSlant,ImageSize->30]},
					{"bend spread ",Slider[Dynamic@bendSpread,{0, Dynamic@arrowWidth, .01}],Pane[Dynamic@bendSpread,ImageSize->30]},
					{"bend curve ",Slider[Dynamic@adjustCurve,{0, 1, .005}],Pane[Dynamic@adjustCurve,ImageSize->30]},
					{Null,
						Grid[{{
							Button[dwShapePolygonBentArrowPreview[8, 1, 45, .5, 0, 1, 5, .1, 4, .1],
								arrowLength = 8; arrowWidth = 1; headAngle = 45; shaftWidthFlipped = .5; flange = 0; bendHeight = 1; bendSlant = 5; bendSpread = .1; bendPosition = 4; adjustCurve = .1, $dwPresetButtonStyle],
							Button[dwShapePolygonBentArrowPreview[6, 1, 45, .5, 0, 1, 3, .1, 3, .1],
								arrowLength = 6; arrowWidth = 1; headAngle = 45; shaftWidthFlipped = .5; flange = 0; bendHeight = 1; bendSlant = 3; bendSpread = .1; bendPosition = 3; adjustCurve = .1, $dwPresetButtonStyle],
							Button[dwShapePolygonBentArrowPreview[4, 1, 45, .5, 0, 1, 1, .1, 1.9, .1],
								arrowLength = 4; arrowWidth = 1; headAngle = 45; shaftWidthFlipped = .5; flange = 0; bendHeight = 1; bendSlant = 1; bendSpread = .1; bendPosition = 1.9; adjustCurve = .1, $dwPresetButtonStyle],
							Button[dwShapePolygonBentArrowPreview[3, 1, 45, .5, 0, 1, 1, .12, 1.4, .1],
								arrowLength = 3; arrowWidth = 1; headAngle = 45; shaftWidthFlipped = .5; flange = 0; bendHeight = 1; bendSlant = 1; bendSpread = .12; bendPosition = 1.4; adjustCurve = .1, $dwPresetButtonStyle],
							Button[dwShapePolygonBentArrowPreview[3, 1, 45, .5, 0, 1, 0, .25, 1.4, 0],
								arrowLength = 3; arrowWidth = 1; headAngle = 45; shaftWidthFlipped = .5; flange = 0; bendHeight = 1; bendSlant = 0; bendSpread = .25; bendPosition = 1.4; adjustCurve = 0, $dwPresetButtonStyle]
						}},Spacings->{0,0}],
						Null},
					{Null,
						Row[{CancelButton[DialogReturn[]],
						DefaultButton[
							(* add to canvas *)	
							dwNewEmptyLayer["Head"->BezierCurve];
							$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
							$dwP[[$dwSelected[[1]]]] = scale{
									(* arrowhead bottom *)
									{arrowLength/2,0},{arrowLength/2,0},
									{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},
									{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
									(* shaft bottom *)
									{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,-bendSpread+adjustCurve*bendHeight,bendSpread],(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,-bendSpread+adjustCurve*bendHeight,bendSpread],(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
									{-arrowLength/2+bendPosition+If[bendHeight>0,-bendSpread-adjustCurve*bendHeight,bendSpread],(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
									{-arrowLength/2+bendPosition+If[bendHeight>0,-bendSpread,bendSpread-adjustCurve*bendHeight],bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
									{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,-bendSpread,adjustCurve*bendHeight+bendSpread],bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
									{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,-bendSpread,adjustCurve*bendHeight+bendSpread],bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
									(* end *)
									{-arrowLength/2,bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
									{-arrowLength/2,bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
									(* shaft top *)
									{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,bendSpread-adjustCurve*bendHeight,-bendSpread],bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
									{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,bendSpread-adjustCurve*bendHeight,-bendSpread],bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
									{-arrowLength/2+bendPosition+If[bendHeight>0,bendSpread+adjustCurve*bendHeight,-bendSpread],bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
									{-arrowLength/2+bendPosition+If[bendHeight>0,bendSpread,-bendSpread+adjustCurve*bendHeight],(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
									{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,bendSpread,-bendSpread-adjustCurve*bendHeight],(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,bendSpread,-bendSpread-adjustCurve*bendHeight],(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
									(* arrowhead top *)
									{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
									{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2},{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2},{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2},
									{arrowLength/2,0},{arrowLength/2,0}
								};
							dwUpdateBoundingBox[$dwSelected[[{1}]]];
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							DialogReturn[]
						]}],
						Null}}
				, Alignment->{{Right,Center,Left}}],
			ImageSize->320]
		],
	Background->LightGray, WindowTitle->"Bent Arrow",Modal->True]
	
dwShapePolygonBentArrowPreview[arrowLength_:3, arrowWidth_:1, headAngle_:45, shaftWidthFlipped_:.5, flange_:0, bendHeight_:1, bendSlant_:2, bendSpread_:.12, bendPosition_:1.4, adjustCurve_:.1]:=
	Block[{adjust = 2, shaftWidth = .5, shaftLength = .5},
		shaftLength = Max[(arrowLength - .5arrowWidth*Tan[headAngle Degree])/arrowLength, 0]+$MachineEpsilon;
		shaftWidth = 1 - shaftWidthFlipped;
		Graphics[{GrayLevel[.7], EdgeForm[Black], FilledCurve[BezierCurve[{
				(* arrowhead bottom *)
				{arrowLength/2,0},{arrowLength/2,0},
				{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},
				{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
				(* shaft bottom *)
				{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,-bendSpread+adjustCurve*bendHeight,bendSpread],(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,-bendSpread+adjustCurve*bendHeight,bendSpread],(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
				{-arrowLength/2+bendPosition+If[bendHeight>0,-bendSpread-adjustCurve*bendHeight,bendSpread],(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
				{-arrowLength/2+bendPosition+If[bendHeight>0,-bendSpread,bendSpread-adjustCurve*bendHeight],bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
				{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,-bendSpread,adjustCurve*bendHeight+bendSpread],bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
				{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,-bendSpread,adjustCurve*bendHeight+bendSpread],bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
				(* end *)
				{-arrowLength/2,bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
				{-arrowLength/2,bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{-arrowLength/2,bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
				(* shaft top *)
				{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,bendSpread-adjustCurve*bendHeight,-bendSpread],bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
				{-arrowLength/2+bendPosition-bendSlant/2+If[bendHeight>0,bendSpread-adjustCurve*bendHeight,-bendSpread],bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
				{-arrowLength/2+bendPosition+If[bendHeight>0,bendSpread+adjustCurve*bendHeight,-bendSpread],bendHeight+(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
				{-arrowLength/2+bendPosition+If[bendHeight>0,bendSpread,-bendSpread+adjustCurve*bendHeight],(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
				{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,bendSpread,-bendSpread-adjustCurve*bendHeight],(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{-arrowLength/2+bendPosition+bendSlant/2+If[bendHeight>0,bendSpread,-bendSpread-adjustCurve*bendHeight],(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
				(* arrowhead top *)
				{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
				{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2},{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2},{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2},
				{arrowLength/2,0},{arrowLength/2,0}
			}]]
		}]
	]

dwShapePolygonSplitArrow[]:=
	CreateDialog[
		DynamicModule[{length = 1.2, arrowWidth = .4, shaftWidth = .5, headAngle = 45, flange = 0, scale = .25, arrowQuantity = 2, flipArrowhead = False, handleLength, handleLengthLong, h = 1.4, height, curve = .1, removeTail = False, shaftLength = .5, temp},
			Pane[
				Grid[{
					{Dynamic@Show[
						Graphics[{
							{	(* axes and frame *)
								GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]}, 
								(*{Thin, Gray, Table[{InfiniteLine[{{n, -1}, {n+2, 1}}], InfiniteLine[{{n, 1}, {n+2, -1}}]}, {n, -3.5, 1.5, .25}]},*)
								(* arrow *)
								shaftLength = Min[(.5 arrowWidth*Tan[headAngle Degree])/arrowWidth, length]+$MachineEpsilon;
								height = h*arrowQuantity/2; handleLength = .75curve; handleLengthLong = .66(curve + (arrowWidth*shaftWidth));
								GrayLevel[.8],EdgeForm[Black],FilledCurve@BezierCurve[If[flipArrowhead,-2.5scale,2.5scale]{(*startshaft*)If[removeTail,Nothing,Sequence@@{Sequence@@If[flipArrowhead,{{-length/2,0},{-length/2,0},{-length/2+(arrowWidth*shaftLength),arrowWidth/2},{-length/2+(arrowWidth*shaftLength),arrowWidth/2},{-length/2+(arrowWidth*shaftLength),arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth*shaftWidth/2}},{{-length/2,arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2}}],{-(arrowWidth*shaftWidth/2)-curve,(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve,(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve+handleLength,(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2),(arrowWidth*shaftWidth/2)+curve-handleLength},{-(arrowWidth*shaftWidth/2),(arrowWidth*shaftWidth/2)+curve},{-(arrowWidth*shaftWidth/2),(arrowWidth*shaftWidth/2)+curve},(*startarrowshaft*){-(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve}}],{-(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve},{-(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve+handleLengthLong},{(arrowWidth*shaftWidth/2)+curve-handleLengthLong,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},(*startarrowhead*)Sequence@@If[flipArrowhead,{{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)}},{{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(arrowWidth*shaftLength),(height/2)},{length/2-(arrowWidth*shaftLength),(height/2)},{length/2-(arrowWidth*shaftLength),(height/2)},{length/2,(height/2)-(arrowWidth/2)},{length/2,(height/2)-(arrowWidth/2)},{length/2,(height/2)-(arrowWidth/2)},{length/2-(arrowWidth*shaftLength),(height/2)-arrowWidth},{length/2-(arrowWidth*shaftLength),(height/2)-arrowWidth},{length/2-(arrowWidth*shaftLength),(height/2)-arrowWidth},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)}}],(*startarrowshaft*){(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve-handleLength,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve+handleLength},{(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve},{(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve},(*middlearrows*)Sequence@@If[arrowQuantity>2,temp=(height-arrowWidth)/(arrowQuantity-1);Flatten[Table[List[(*shaft*){(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2+curve},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2+curve},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2+curve-handleLength},{(arrowWidth*shaftWidth/2)+curve-handleLength,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},(*head*)Sequence@@If[flipArrowhead,{{length/2,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2}},{{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp+arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp+arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp+arrowWidth/2},{length/2,((height-arrowWidth)/2)-n*temp},{length/2,((height-arrowWidth)/2)-n*temp},{length/2,((height-arrowWidth)/2)-n*temp},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-arrowWidth/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-(arrowWidth*shaftWidth)/2}}],(*shaft*){(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve-handleLength,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)-((arrowWidth*shaftWidth)/2)-curve+handleLength},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)-((arrowWidth*shaftWidth)/2)-curve},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)-((arrowWidth*shaftWidth)/2)-curve}],{n,arrowQuantity-2}],1],{}],(*endarrowshaft*){(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},{(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},{(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve-handleLength},{(arrowWidth*shaftWidth/2)+curve-handleLength,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},(*endarrowhead*)Sequence@@If[flipArrowhead,{{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2}},{{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2-(arrowWidth*shaftLength),-(height/2)+arrowWidth},{length/2-(arrowWidth*shaftLength),-(height/2)+arrowWidth},{length/2-(arrowWidth*shaftLength),-(height/2)+arrowWidth},{length/2,-(height/2)+(arrowWidth/2)},{length/2,-(height/2)+(arrowWidth/2)},{length/2,-(height/2)+(arrowWidth/2)},{length/2-(arrowWidth*shaftLength),-(height/2)},{length/2-(arrowWidth*shaftLength),-(height/2)},{length/2-(arrowWidth*shaftLength),-(height/2)},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2}}],(*endarrowshaft*){(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve-handleLengthLong,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{-(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve-handleLengthLong},{-(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},{-(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},If[removeTail,Nothing,Sequence@@{(*startshaft*){-(arrowWidth*shaftWidth/2),-(arrowWidth*shaftWidth/2)-curve},{-(arrowWidth*shaftWidth/2),-(arrowWidth*shaftWidth/2)-curve},{-(arrowWidth*shaftWidth/2),-(arrowWidth*shaftWidth/2)-curve+handleLength},{-(arrowWidth*shaftWidth/2)-curve+handleLength,-(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve,-(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve,-(arrowWidth/2)shaftWidth},Sequence@@If[flipArrowhead,{{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth*shaftWidth/2},{-length/2+(arrowWidth*shaftLength),-arrowWidth/2},{-length/2+(arrowWidth*shaftLength),-arrowWidth/2},{-length/2+(arrowWidth*shaftLength),-arrowWidth/2},{-length/2,0},{-length/2,0},{-length/2,0}},{{-length/2,-arrowWidth*shaftWidth/2},{-length/2,-arrowWidth*shaftWidth/2},{-length/2,-arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2}}]}]}]
							},
							Background->White,ImageSize->{300,200},PlotRange->{1.5{-1.1,1.1},{-1.1,1.1}}],
						Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]], PlotRange->1.1],SpanFromLeft,SpanFromLeft},
					{"             scale ",Slider[Dynamic@scale,{.1,1,.01}],Pane[Dynamic@scale,ImageSize->30]},
					{"           length ",Slider[Dynamic@length,{.4,4,.05}],Pane[Dynamic[2.5length],ImageSize->30]},
					{"           height ",Slider[Dynamic@h,{.4,4,.05}],Pane[Dynamic[2.5h],ImageSize->30]},
					{"     thickness ",Slider[Dynamic@shaftWidth,{0.1,1,.05}],Pane[Dynamic@shaftWidth,ImageSize->30]},
					{"          curve ",Slider[Dynamic@curve,{0,.2,.01}],Pane[Dynamic@curve,ImageSize->30]},
					{"    head size ",Slider[Dynamic@arrowWidth,{.1,.5,.05}],Pane[Dynamic@arrowWidth,ImageSize->30]},
					{"   head angle ",Slider[Dynamic@headAngle,{0,89,1}],Pane[Dynamic@headAngle,ImageSize->30]},
					{"  head flange ",Dynamic@Slider[Dynamic@flange,{0, ((1-shaftWidth) (length - (shaftLength*arrowWidth)))/(length-(shaftLength*arrowWidth)), .01}],Pane[Dynamic@Round[flange,.01],ImageSize->30]},
					{"        quantity ",Slider[Dynamic@arrowQuantity,{2, 10, 1}],Pane[Dynamic@arrowQuantity,ImageSize->30]},
					{Row[{Checkbox[Dynamic@removeTail], " remove tail     ", Checkbox[Dynamic@flipArrowhead], " flip tail"}], SpanFromLeft, SpanFromLeft},
					{Grid[{{
							Button[dwShapePolygonSplitArrowPreview[1.2, .5, 45, .5, 0, 2, False, .1, 1.4, .1],
								length = 1.2; arrowWidth = .5; headAngle = 45; shaftWidth = .5; flange = 0; arrowQuantity = 2; flipArrowhead = False; handleLength = .1; h = 1.4; curve = .1, $dwPresetButtonStyle],
							Button[Show[dwShapePolygonSplitArrowPreview[1.2, .5, 45, .5, 0, 2, True, .1, 1.4, .1], ImagePadding->3],
								length = 1.2; arrowWidth = .5; headAngle = 45; shaftWidth = .5; flange = 0; arrowQuantity = 2; flipArrowhead = True; handleLength = .1; h = 1.4; curve = .1, $dwPresetButtonStyle],
							Button[dwShapePolygonSplitArrowPreview[1.2, .4, 45, .5, 0, 2, False, .1, 1.4, .1],
								length = 1.2; arrowWidth = .4; headAngle = 45; shaftWidth = .5; flange = 0; arrowQuantity = 2; flipArrowhead = False; handleLength = .1; h = 1.4; curve = .1, $dwPresetButtonStyle],
							Button[Show[dwShapePolygonSplitArrowPreview[1.2, .4, 45, .5, 0, 2, True, .1, 1.4, .1], ImagePadding->2],
								length = 1.2; arrowWidth = .4; headAngle = 45; shaftWidth = .5; flange = 0; arrowQuantity = 2; flipArrowhead = True; handleLength = .1; h = 1.4; curve = .1, $dwPresetButtonStyle],
							Button[dwShapePolygonSplitArrowPreview[1.2, .3, 45, .5, 0, 2, False, .1, 1.4, .1],
								length = 1.2; arrowWidth = .3; headAngle = 45; shaftWidth = .5; flange = 0; arrowQuantity = 2; flipArrowhead = False; handleLength = .1; h = 1.4; curve = .1, $dwPresetButtonStyle],
							Button[Show[dwShapePolygonSplitArrowPreview[1.2, .3, 45, .5, 0, 2, True, .1, 1.4, .1], ImagePadding->2],
								length = 1.2; arrowWidth = .3; headAngle = 45; shaftWidth = .5; flange = 0; arrowQuantity = 2; flipArrowhead = True; handleLength = .1; h = 1.4; curve = .1, $dwPresetButtonStyle],
							Button[dwShapePolygonSplitArrowPreview[1.2, .2, 45, .5, 0, 2, False, .1, 1.4, .1],
								length = 1.2; arrowWidth = .2; headAngle = 45; shaftWidth = .5; flange = 0; arrowQuantity = 2; flipArrowhead = False; handleLength = .1; h = 1.4; curve = .1, $dwPresetButtonStyle],
							Button[Show[dwShapePolygonSplitArrowPreview[1.2, .2, 45, .5, 0, 2, True, .1, 1.4, .1], ImagePadding->1],
								length = 1.2; arrowWidth = .2; headAngle = 45; shaftWidth = .5; flange = 0; arrowQuantity = 2; flipArrowhead = True; handleLength = .1; h = 1.4; curve = .1, $dwPresetButtonStyle]
						}},Spacings->{0,0}], SpanFromLeft, SpanFromLeft},
					{Row[{CancelButton[DialogReturn[]],
						DefaultButton[
							(* add to canvas *)	
							dwNewEmptyLayer["Head"->BezierCurve];
							$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
							$dwP[[$dwSelected[[1]]]] = If[flipArrowhead,-2.5scale,2.5scale]{(*startshaft*)If[removeTail,Nothing,Sequence@@{Sequence@@If[flipArrowhead,{{-length/2,0},{-length/2,0},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth*shaftWidth/2}},{{-length/2,arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2}}],{-(arrowWidth*shaftWidth/2)-curve,(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve,(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve+handleLength,(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2),(arrowWidth*shaftWidth/2)+curve-handleLength},{-(arrowWidth*shaftWidth/2),(arrowWidth*shaftWidth/2)+curve},{-(arrowWidth*shaftWidth/2),(arrowWidth*shaftWidth/2)+curve},(*startarrowshaft*){-(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve}}],{-(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve},{-(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve+handleLengthLong},{(arrowWidth*shaftWidth/2)+curve-handleLengthLong,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},(*startarrowhead*)Sequence@@If[flipArrowhead,{{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)}},{{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(arrowWidth*shaftLength),(height/2)},{length/2-(arrowWidth*shaftLength),(height/2)},{length/2-(arrowWidth*shaftLength),(height/2)},{length/2,(height/2)-(arrowWidth/2)},{length/2,(height/2)-(arrowWidth/2)},{length/2,(height/2)-(arrowWidth/2)},{length/2-(arrowWidth*shaftLength),(height/2)-arrowWidth},{length/2-(arrowWidth*shaftLength),(height/2)-arrowWidth},{length/2-(arrowWidth*shaftLength),(height/2)-arrowWidth},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)}}],(*startarrowshaft*){(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve-handleLength,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve+handleLength},{(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve},{(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve},(*middlearrows*)Sequence@@If[arrowQuantity>2,temp=(height-arrowWidth)/(arrowQuantity-1);Flatten[Table[List[(*shaft*){(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2+curve},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2+curve},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2+curve-handleLength},{(arrowWidth*shaftWidth/2)+curve-handleLength,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},(*head*)Sequence@@If[flipArrowhead,{{length/2,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2}},{{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp+arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp+arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp+arrowWidth/2},{length/2,((height-arrowWidth)/2)-n*temp},{length/2,((height-arrowWidth)/2)-n*temp},{length/2,((height-arrowWidth)/2)-n*temp},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-arrowWidth/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-(arrowWidth*shaftWidth)/2}}],(*shaft*){(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve-handleLength,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)-((arrowWidth*shaftWidth)/2)-curve+handleLength},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)-((arrowWidth*shaftWidth)/2)-curve},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)-((arrowWidth*shaftWidth)/2)-curve}],{n,arrowQuantity-2}],1],{}],(*endarrowshaft*){(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},{(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},{(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve-handleLength},{(arrowWidth*shaftWidth/2)+curve-handleLength,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},(*endarrowhead*)Sequence@@If[flipArrowhead,{{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2}},{{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2-(arrowWidth*shaftLength),-(height/2)+arrowWidth},{length/2-(arrowWidth*shaftLength),-(height/2)+arrowWidth},{length/2-(arrowWidth*shaftLength),-(height/2)+arrowWidth},{length/2,-(height/2)+(arrowWidth/2)},{length/2,-(height/2)+(arrowWidth/2)},{length/2,-(height/2)+(arrowWidth/2)},{length/2-(arrowWidth*shaftLength),-(height/2)},{length/2-(arrowWidth*shaftLength),-(height/2)},{length/2-(arrowWidth*shaftLength),-(height/2)},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2}}],(*endarrowshaft*){(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve-handleLengthLong,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{-(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve-handleLengthLong},{-(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},{-(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},If[removeTail,Nothing,Sequence@@{(*startshaft*){-(arrowWidth*shaftWidth/2),-(arrowWidth*shaftWidth/2)-curve},{-(arrowWidth*shaftWidth/2),-(arrowWidth*shaftWidth/2)-curve},{-(arrowWidth*shaftWidth/2),-(arrowWidth*shaftWidth/2)-curve+handleLength},{-(arrowWidth*shaftWidth/2)-curve+handleLength,-(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve,-(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve,-(arrowWidth/2)shaftWidth},Sequence@@If[flipArrowhead,{{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth/2},{-length/2,0},{-length/2,0},{-length/2,0}},{{-length/2,-arrowWidth*shaftWidth/2},{-length/2,-arrowWidth*shaftWidth/2},{-length/2,-arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2}}]}]};
							dwUpdateBoundingBox[$dwSelected[[{1}]]];
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							DialogReturn[]
						]}], SpanFromLeft, SpanFromLeft}}
				,Alignment->Center],
			ImageSize->320]
		],
	Background->LightGray, WindowTitle->"Split Arrow",Modal->True]

dwShapePolygonSplitArrowPreview[length_:1.6, arrowWidth_:.4, headAngle_:45, shaftWidth_:.5, flange_:0, arrowQuantity_:2, flipArrowhead_:False, handleLength_:.1, h_:1.4, curve_:.1, removeTail_:False]:=
	Block[{height = h*arrowQuantity/2, handleLengthLong = .1 + .2shaftWidth, shaftLength = .5, temp},
		shaftLength = Min[(.5 arrowWidth*Tan[headAngle Degree])/arrowWidth, length]+$MachineEpsilon;
		Graphics[{GrayLevel[.8], EdgeForm[Black], FilledCurve@BezierCurve[If[flipArrowhead,-1,1]{(*startshaft*)If[removeTail,Nothing,Sequence@@{Sequence@@If[flipArrowhead,{{-length/2,0},{-length/2,0},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),arrowWidth*shaftWidth/2}},{{-length/2,arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2}}],{-(arrowWidth*shaftWidth/2)-curve,(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve,(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve+handleLength,(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2),(arrowWidth*shaftWidth/2)+curve-handleLength},{-(arrowWidth*shaftWidth/2),(arrowWidth*shaftWidth/2)+curve},{-(arrowWidth*shaftWidth/2),(arrowWidth*shaftWidth/2)+curve},(*startarrowshaft*){-(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve}}],{-(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve},{-(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve+handleLengthLong},{(arrowWidth*shaftWidth/2)+curve-handleLengthLong,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},(*startarrowhead*)Sequence@@If[flipArrowhead,{{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)}},{{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(arrowWidth*shaftLength),(height/2)},{length/2-(arrowWidth*shaftLength),(height/2)},{length/2-(arrowWidth*shaftLength),(height/2)},{length/2,(height/2)-(arrowWidth/2)},{length/2,(height/2)-(arrowWidth/2)},{length/2,(height/2)-(arrowWidth/2)},{length/2-(arrowWidth*shaftLength),(height/2)-arrowWidth},{length/2-(arrowWidth*shaftLength),(height/2)-arrowWidth},{length/2-(arrowWidth*shaftLength),(height/2)-arrowWidth},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)}}],(*startarrowshaft*){(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve-handleLength,(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve+handleLength},{(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve},{(arrowWidth*shaftWidth/2),(height/2)-(arrowWidth-(arrowWidth*shaftWidth))/2-(arrowWidth*shaftWidth)-curve},(*middlearrows*)Sequence@@If[arrowQuantity>2,temp=(height-arrowWidth)/(arrowQuantity-1);Flatten[Table[List[(*shaft*){(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2+curve},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2+curve},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2+curve-handleLength},{(arrowWidth*shaftWidth/2)+curve-handleLength,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},(*head*)Sequence@@If[flipArrowhead,{{length/2,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{length/2,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2}},{{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2-n*temp)+(arrowWidth*shaftWidth)/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp+arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp+arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp+arrowWidth/2},{length/2,((height-arrowWidth)/2)-n*temp},{length/2,((height-arrowWidth)/2)-n*temp},{length/2,((height-arrowWidth)/2)-n*temp},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-arrowWidth/2},{length/2-(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-arrowWidth/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-(arrowWidth*shaftWidth)/2},{length/2-(1-flange)(arrowWidth*shaftLength),((height-arrowWidth)/2)-n*temp-(arrowWidth*shaftWidth)/2}}],(*shaft*){(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2)+curve-handleLength,((height-arrowWidth)/2-n*temp)-(arrowWidth*shaftWidth)/2},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)-((arrowWidth*shaftWidth)/2)-curve+handleLength},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)-((arrowWidth*shaftWidth)/2)-curve},{(arrowWidth*shaftWidth/2),((height-arrowWidth)/2-n*temp)-((arrowWidth*shaftWidth)/2)-curve}],{n,arrowQuantity-2}],1],{}],(*endarrowshaft*){(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},{(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},{(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve-handleLength},{(arrowWidth*shaftWidth/2)+curve-handleLength,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},(*endarrowhead*)Sequence@@If[flipArrowhead,{{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2}},{{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)},{length/2-(arrowWidth*shaftLength),-(height/2)+arrowWidth},{length/2-(arrowWidth*shaftLength),-(height/2)+arrowWidth},{length/2-(arrowWidth*shaftLength),-(height/2)+arrowWidth},{length/2,-(height/2)+(arrowWidth/2)},{length/2,-(height/2)+(arrowWidth/2)},{length/2,-(height/2)+(arrowWidth/2)},{length/2-(arrowWidth*shaftLength),-(height/2)},{length/2-(arrowWidth*shaftLength),-(height/2)},{length/2-(arrowWidth*shaftLength),-(height/2)},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{length/2-(1-flange)(arrowWidth*shaftLength),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2}}],(*endarrowshaft*){(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{(arrowWidth*shaftWidth/2)+curve-handleLengthLong,-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2},{-(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve-handleLengthLong},{-(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},{-(arrowWidth*shaftWidth/2),-(height/2)+(arrowWidth-(arrowWidth*shaftWidth))/2+(arrowWidth*shaftWidth)+curve},If[removeTail,Nothing,Sequence@@{(*startshaft*){-(arrowWidth*shaftWidth/2),-(arrowWidth*shaftWidth/2)-curve},{-(arrowWidth*shaftWidth/2),-(arrowWidth*shaftWidth/2)-curve},{-(arrowWidth*shaftWidth/2),-(arrowWidth*shaftWidth/2)-curve+handleLength},{-(arrowWidth*shaftWidth/2)-curve+handleLength,-(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve,-(arrowWidth/2)shaftWidth},{-(arrowWidth*shaftWidth/2)-curve,-(arrowWidth/2)shaftWidth},Sequence@@If[flipArrowhead,{{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth*shaftWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth/2},{-length/2+(1-flange)(arrowWidth*shaftLength),-arrowWidth/2},{-length/2,0},{-length/2,0},{-length/2,0}},{{-length/2,-arrowWidth*shaftWidth/2},{-length/2,-arrowWidth*shaftWidth/2},{-length/2,-arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2},{-length/2,arrowWidth*shaftWidth/2}}]}]}]}]
	]

dwShapePolygonSwoopArrow[]:=
	CreateDialog[
		DynamicModule[{scale = .25, arrowLength = 2, arrowWidth = 1, headAngle = 50, shaftLength = .65, shaftWidth = .5, swoop = 5, curve = 1.5, flange = 0, rotate = 0, loc = {0,0}, rotateTransform, distance = 1},
	
			(* use mouseclicks as input *)
			If[$dwShowMouseClickPositions,
				If[$dwPreviousClickPtNoGrid =!= Null && $dwClickPtNoGrid =!= Null,
					loc = Round[$dwClickPtNoGrid, $dwGridStep];
					distance = 2/3(EuclideanDistance[Round[$dwClickPtNoGrid, $dwGridStep], Round[$dwPreviousClickPtNoGrid, $dwGridStep]])
				],
				loc = {scale*arrowLength, 0}
			];
			
			Pane[
				Grid[{
					{Dynamic@Show[Graphics[{
							
								arrowLength = If[$dwShowMouseClickPositions, distance/scale, arrowLength];
								rotate = (Quiet@ToPolarCoordinates[Subtract[Sequence@@(#-{0,0}&/@Round[{$dwClickPtNoGrid,$dwPreviousClickPtNoGrid}, $dwGridStep])]]/.{Indeterminate->0})[[2]];
								rotateTransform = If[$dwShowMouseClickPositions,
									RotationTransform[(rotate - Pi/2 + ((14.5+swoop)*Pi/180) 2Pi), scale{arrowLength/2, 0}],
									RotationTransform[0, scale{arrowLength/2, 0}]
								];
							
								(* axes and frame *)
								{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
								(*{Thin, Gray, Table[{InfiniteLine[{{n, -1}, {n+2, 1}}], InfiniteLine[{{n, 1}, {n+2, -1}}]}, {n, -3.5, 1.5, .25}]},*)
								(* arrow *)
								shaftLength = Max[(arrowLength - .5arrowWidth*Tan[headAngle Degree])/arrowLength, 0]+$MachineEpsilon;
								GrayLevel[.7], EdgeForm[Black], FilledCurve[BezierCurve[Join[
										(loc+{scale(-arrowLength/2), 0})+#&/@rotateTransform[scale{
										Sequence@@Table[{arrowLength/2,0}, 2],
										Sequence@@Table[{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2}, 3],
										Sequence@@Table[{flange(arrowLength-(arrowLength shaftLength))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)}, 2],
										{(arrowLength/2-2(arrowLength-(arrowLength shaftLength/curve))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)}}],
										If[$dwShowMouseClickPositions,
											Table[Round[$dwPreviousClickPtNoGrid, $dwGridStep], 3],
											(loc+{scale(-arrowLength/2), 0})+#&/@rotateTransform[scale Table[-arrowLength{Sin[((14.5+swoop)*Pi/180) 2Pi],Cos[((14.5+swoop)*Pi/180) 2Pi]}, 3]
											]
										],
										(loc+{scale(-arrowLength/2), 0})+#&/@rotateTransform[scale{{arrowLength/2-2(arrowLength-(arrowLength shaftLength/curve)),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
										Sequence@@Table[{flange(arrowLength-(arrowLength shaftLength))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)}, 2],
										Sequence@@Table[{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2}, 3],
										Sequence@@Table[{arrowLength/2,0}, 3]
									}]]
								]]
							},Background->White,ImageSize->{300,200},PlotRange->{1.5{-1.1,1.1},{-1.1,1.1}}],
						Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]], PlotRange->1.1],SpanFromLeft,SpanFromLeft},
					{"               scale ",Slider[Dynamic@scale,{.1,1,.01}],Pane[Dynamic@scale,ImageSize->30]},
					{"             length ",Slider[Dynamic@arrowLength,{.375,3,.001}],Pane[Dynamic[Round[4/3arrowLength,.01]],ImageSize->30]},
					{"     thickness ",Slider[Dynamic@shaftWidth,{0,1,.05}],Pane[Dynamic@shaftWidth,ImageSize->30]},
					{"         swoop ",Slider[Dynamic@swoop,{-10,10,1}],Pane[Dynamic@swoop,ImageSize->30]},
					{"         curve ",Slider[Dynamic@curve,{1, 10, .1}],Pane[Dynamic@curve,ImageSize->30]},
					{"  head size ",Slider[Dynamic@arrowWidth,{.05,1,.05}],Pane[Dynamic@arrowWidth,ImageSize->30]},
					{"   head angle ",Slider[Dynamic@headAngle,{0,89,1}],Pane[Dynamic@headAngle,ImageSize->30]},
					{"  head flange ",Dynamic@Slider[Dynamic@flange,{0, ((shaftWidth) (arrowLength - (shaftLength*arrowWidth)))/(arrowLength-(shaftLength*arrowWidth)), .01}],Pane[Dynamic@Round[flange,.01],ImageSize->30]},
					{Grid[{{
							Button[dwShapePolygonSwoopArrowPreview[],
								arrowLength = 2; arrowWidth = 1; headAngle = 50; shaftWidth = .5; swoop = 5; curve = 1.5; flange = 0, $dwPresetButtonStyle],
							Button[dwShapePolygonSwoopArrowPreview[2, 1, 60, .5, 5, 1.5, 0],
								arrowLength = 2; arrowWidth = 1; headAngle = 60; shaftWidth = .5; swoop = 5; curve = 1.5; flange = 0, $dwPresetButtonStyle],
							Button[dwShapePolygonSwoopArrowPreview[2, .5, 65, .6, 5, 1.5, 0],
								arrowLength = 2; arrowWidth = .5; headAngle = 65; shaftWidth = .6; swoop = 5; curve = 1.5; flange = 0, $dwPresetButtonStyle],
							Button[dwShapePolygonSwoopArrowPreview[2, .75, 65, .7, 5, 1.5, 0.4],
								arrowLength = 2; arrowWidth = .75; headAngle = 65; shaftWidth = .7; swoop = 5; curve = 1.5; flange = 0.4, $dwPresetButtonStyle],
							Button[dwShapePolygonSwoopArrowPreview[2, 1, 50, .7, 5, 1.5, 0.4],
								arrowLength = 2; arrowWidth = 1; headAngle = 50; shaftWidth = .7; swoop = 5; curve = 1.5; flange = 0.4, $dwPresetButtonStyle]
						}},Spacings->{0,0}],SpanFromLeft,SpanFromLeft},
					{Row[{CancelButton[$dwShowMouseClickPositions = False; DialogReturn[]],
						DefaultButton[
							(* add to canvas *)	
							dwNewEmptyLayer["Head"->BezierCurve];
							$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
							$dwP[[$dwSelected[[1]]]] = Join[
									(loc+{scale(-arrowLength/2), 0})+#&/@rotateTransform[scale{
									Sequence@@Table[{arrowLength/2,0}, 2],
									Sequence@@Table[{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2}, 3],
									Sequence@@Table[{flange(arrowLength-(arrowLength shaftLength))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)}, 2],
									{(arrowLength/2-2(arrowLength-(arrowLength shaftLength/curve))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)}}],
									If[$dwShowMouseClickPositions,
										Table[Round[$dwPreviousClickPtNoGrid, $dwGridStep], 3],
										(loc+{scale(-arrowLength/2), 0})+#&/@rotateTransform[scale Table[-arrowLength{Sin[((14.5+swoop)*Pi/180) 2Pi],Cos[((14.5+swoop)*Pi/180) 2Pi]}, 3]
										]
									],
									(loc+{scale(-arrowLength/2), 0})+#&/@rotateTransform[scale{{arrowLength/2-2(arrowLength-(arrowLength shaftLength/curve)),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
									Sequence@@Table[{flange(arrowLength-(arrowLength shaftLength))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)}, 2],
									Sequence@@Table[{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2}, 3],
									Sequence@@Table[{arrowLength/2,0}, 3]
								}]];
							dwUpdateBoundingBox[$dwSelected[[{1}]]];
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							$dwShowMouseClickPositions = False;
							DialogReturn[]
						]}],SpanFromLeft,SpanFromLeft}}
				,Alignment->Center],
			ImageSize->320]
		],
	Background->LightGray, WindowTitle->"Swoop Arrow",Modal->True]
	
dwShapePolygonSwoopArrowPreview[arrowLength_:2, arrowWidth_:1, headAngle_:50, shaftWidth_:.5, swoop_:5, curve_:1.5, flange_:0]:=
	Block[{shaftLength = .65},
		shaftLength = Max[(arrowLength - .5arrowWidth*Tan[headAngle Degree])/arrowLength, 0]+$MachineEpsilon;
		Graphics[{
			GrayLevel[.7], EdgeForm[Black], FilledCurve[BezierCurve[{
			Sequence@@Table[{arrowLength/2,0}, 2],
			Sequence@@Table[{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2}, 3],
			Sequence@@Table[{flange(arrowLength-(arrowLength shaftLength))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)}, 2],
			{(arrowLength/2-2(arrowLength-(arrowLength shaftLength/curve))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},
			Sequence@@Table[-arrowLength{Sin[((14.5+swoop)*Pi/180) 2Pi],Cos[((14.5+swoop)*Pi/180) 2Pi]}, 3],
			{arrowLength/2-2(arrowLength-(arrowLength shaftLength/curve)),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},
			Sequence@@Table[{flange(arrowLength-(arrowLength shaftLength))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)}, 2],
			Sequence@@Table[{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2}, 3],
			Sequence@@Table[{arrowLength/2,0}, 3]
		}]]}]
	]

dwShapePolygonArrow[]:=
	CreateDialog[
		DynamicModule[{arrowLength = 1, arrowWidth = 1, shaftLength = .5, shaftWidthFlipped = .5, flange = 0, scale = .25, headAngle = 45, shaftWidth, rotate = 0, loc = {0,0}, rotateTransform, distance = 1},
	
			(* use mouseclicks as input *)
			If[$dwShowMouseClickPositions,
				If[$dwPreviousClickPtNoGrid =!= Null && $dwClickPtNoGrid =!= Null,
					loc = Round[$dwPreviousClickPtNoGrid, $dwGridStep];
					rotate = (Quiet@ToPolarCoordinates[Subtract[Sequence@@(#-{0,0}&/@Round[{$dwClickPtNoGrid,$dwPreviousClickPtNoGrid}, $dwGridStep])]]/.{Indeterminate->0})[[2]];
					distance = (EuclideanDistance[Round[$dwClickPtNoGrid, $dwGridStep],Round[$dwPreviousClickPtNoGrid, $dwGridStep]])
				]
			];
			
			Pane[
				Grid[{
					{Dynamic@Show[Graphics[{
						
								arrowLength = If[$dwShowMouseClickPositions, distance/scale, arrowLength];
								rotateTransform = RotationTransform[rotate, scale{-arrowLength/2, 0}];
					
								(* axes and frame *)
								{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
								(*{Thin, Gray, Table[{InfiniteLine[{{n, -1}, {n+2, 1}}], InfiniteLine[{{n, 1}, {n+2, -1}}]}, {n, -3.5, 1.5, .25}]},*)
								(* arrow *)
								shaftLength = Max[(arrowLength - .5arrowWidth*Tan[headAngle Degree])/arrowLength, 0]+$MachineEpsilon;
								shaftWidth = 1 - shaftWidthFlipped;
								GrayLevel[.7],EdgeForm[Black],Polygon[(loc+{scale(arrowLength/2), 0})+#&/@rotateTransform[scale{{arrowLength/2,0},{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2}}]]},
							Background->White,ImageSize->{300,200},PlotRange->{1.5{-1.1,1.1},{-1.1,1.1}}],
						Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]], PlotRange->1.1],SpanFromLeft,SpanFromLeft},
					{"scale ",Slider[Dynamic@scale,{.1,1,.01}],Pane[Dynamic@scale,ImageSize->30]},
					{"length ",Slider[Dynamic@arrowLength,{.5,2,.05}],Pane[Dynamic@arrowLength,ImageSize->30]},
					{"thickness ",Slider[Dynamic@shaftWidthFlipped,{0,1,.05}],Pane[Dynamic@shaftWidthFlipped,ImageSize->30]},
					{"head size ",Slider[Dynamic@arrowWidth,{.05,1,.05}],Pane[Dynamic@arrowWidth,ImageSize->30]},
					{"head angle ",Slider[Dynamic@headAngle,{0,89,1}],Pane[Dynamic@headAngle,ImageSize->30]},
					{"head flange ",Dynamic@Slider[Dynamic@flange,{0, shaftWidth((1-shaftLength)(arrowLength/shaftLength))/arrowLength, .01}],Pane[Dynamic@Round[flange,.01],ImageSize->30]},
					{Null,Grid[{{
							Button[dwShapePolygonArrowPreview[],
								arrowLength = 1; arrowWidth = 1; headAngle = 45; shaftWidthFlipped = .5; flange = 0, $dwPresetButtonStyle],
							Button[dwShapePolygonArrowPreview[2, 1, 45, .5, 0],
								arrowLength = 2; arrowWidth = 1; headAngle = 45; shaftWidthFlipped = .5; flange = 0, $dwPresetButtonStyle],
							Button[dwShapePolygonArrowPreview[2, 1, 60, .5, 0],
								arrowLength = 2; arrowWidth = 1; headAngle = 60; shaftWidthFlipped = .5; flange = 0, $dwPresetButtonStyle],
							Button[dwShapePolygonArrowPreview[2, 1, 60, .4, .15],
								arrowLength = 2; arrowWidth = 1; headAngle = 60; shaftWidthFlipped = .4; flange = .15, $dwPresetButtonStyle],
							Button[dwShapePolygonArrowPreview[2, .6, 70, .35, .15],
								arrowLength = 2; arrowWidth = .6; headAngle = 70; shaftWidthFlipped = .35; flange = .15, $dwPresetButtonStyle]
						}},Spacings->{0,0}],Null},
					{Null,Row[{CancelButton[$dwShowMouseClickPositions = False; DialogReturn[]],
						DefaultButton[
							(* add to canvas *)	
							dwNewEmptyLayer["Head"->Polygon];
							$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
							$dwHead[[$dwSelected[[1]]]] = Polygon;
							$dwP[[$dwSelected[[1]]]] = (loc+{scale(arrowLength/2), 0})+#&/@rotateTransform[scale{{arrowLength/2,0},{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2}}];
							dwUpdateBoundingBox[$dwSelected[[{1}]]];
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							$dwShowMouseClickPositions = False;
							DialogReturn[]
						]}],Null}}
				,Alignment->{{Right, Center, Left}}],
			ImageSize->320]
		],
	Background->LightGray, WindowTitle->"Straight Arrow",Modal->True]
	
dwShapePolygonArrowPreview[arrowLength_:1, arrowWidth_:1, headAngle_:45, shaftWidthFlipped_:.5, flange_:0]:=
	Block[{shaftLength, shaftWidth},
		shaftLength = Max[(arrowLength - .5arrowWidth*Tan[headAngle Degree])/arrowLength, 0];
		shaftWidth = 1 - shaftWidthFlipped;
		Graphics[{GrayLevel[.7], EdgeForm[Black], Polygon[{{arrowLength/2,0},{(arrowLength/2-(arrowLength-(arrowLength shaftLength))),-arrowWidth/2},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,(-arrowWidth/2)+((arrowWidth/2) shaftWidth)},{-arrowLength/2,(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{flange(arrowLength-(arrowLength-(arrowLength shaftLength)))+(arrowLength/2-(arrowLength-(arrowLength shaftLength))),(arrowWidth/2)-((arrowWidth/2) shaftWidth)},{arrowLength/2-(arrowLength-(arrowLength shaftLength)),arrowWidth/2}}]}]
	]

End[] (* End Private Context *)

EndPackage[]
