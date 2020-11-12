(* Wolfram Language Package *)

BeginPackage["WLDraw`"]

Get[FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Load.wl"}]]

(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *)

(* create notebook of GUI *)
CreateDocument[
	ExpressionCell[
		Grid[{
			{Pane[Dynamic[If[$dwSelected === {} || $dwMode === "plotrange", 
					Pane[Style[
						If[$dwP === {}, 
							Row[{Button[$dwIconAddPoint, Null, $dwButtonStyle]," draw a path or ", Button[Show[$dwIconSpiral, ImageSize->28], Null, $dwButtonStyle], " insert an object"}], 
							Row[{Button[$dwIconObjectSelect, Null, $dwButtonStyle], " select an object to display its style"}]
						], GrayLevel[.8], 18], Alignment->{Center,Center}, ImageSize->{Full,Full}],
					If[Head[$dwSelected] === List && $dwSelected =!= {}, 
						If[$dwHead[[Flatten[{$dwSelected[[1]]}][[1]]]] === "Expression",
							Pane[Style["Unable to style expressions", GrayLevel[.8], 18], Alignment->{Center,Center}, ImageSize->{Full,Full}],
							Pane[dwStyleControls[],ImageSize->Full]
						],
						{}
					]
				], SynchronousUpdating->$dwSynchronousUpdating], ImageSize->{{$dwWindowSize[[1]],Scaled[1]}, $dwStyleHeight}], SpanFromLeft},
			{Pane[Row[
				Insert[ dwCanvasTools[], Style[Graphics[{AbsoluteThickness[1.5], Line[{{.1,0},{.1,1}}]},ImageSize->{18,24}], 22], {{9}, {12}, {14}, {18}, {19}, {24}} ]
				], ImageSize->{{$dwWindowSize[[1]],Scaled[1]}, Automatic}], SpanFromLeft},
			{Pane[Column[{
					Grid[Partition[Flatten[{
						Pane[Grid[{{Spacer[5], dwTransformOrigin[], Spacer[4], dwNudgeTool[], Null}}, 
							Background->GrayLevel[.1], Frame->All, FrameStyle->GrayLevel[.1], Spacings->{0,0}], ImageSize->{$dwToolWidth, Automatic}, Background->Green], SpanFromLeft,
						Style["\nDRAW", Hue[.125], 14, Bold, LineSpacing->{1,-10}], SpanFromLeft,
						dwDraw[],
						Style["\nOBJECTS", Hue[.125], 14, Bold, LineSpacing->{1,-10}], SpanFromLeft,
						dwObjects[]
					}], 2, 2, 1, {}], Background->GrayLevel[.1], Spacings->{0,0}],
					Grid[Partition[Flatten[{
						Style["\nTOOLS", Hue[.125], 14, Bold, LineSpacing->{1,-10}], SpanFromLeft, SpanFromLeft,
						Style["\nOBJECT", GrayLevel[1], 9, LineSpacing->{1,-12}], SpanFromLeft, SpanFromLeft,
						dwObjectTools[],
						Style["\nPOINT", GrayLevel[1], 9, LineSpacing->{1,-5}], SpanFromLeft, SpanFromLeft,
						dwObjectPointTools[]
					}], 3, 3, 1, {}], Background->GrayLevel[.1], Spacings->{0,0}]
				}, Background->GrayLevel[.1], Spacings->0], 
				ImageSize->{$dwToolWidth, Automatic}], dwRender[]}
		}, Alignment->{Left, Top}, Background->{$dwButtonBackgroundColor, None, {{1, 1}->GrayLevel[.4]}}, Spacings->{0,0}],
			
		CellMargins->{{0,0},{0,0}}], ShowCellBracket->False, DockedCells->{}, 
		WindowElements->{"StatusArea"}, 
		WindowSize->$dwWindowSize, 
		WindowStatusArea->Dynamic@messageBarText[], 
		WindowTitle->"WL-Draw", Selectable->False
	]
	
messageBarText[]:=
	If[MemberQ[{"wireframe","canvas","plotrange","point","zoomwireframe"},$dwMode], "WIREFRAME     ", "PREVIEW     "] <>
	"Zoom: "<>ToString[IntegerPart[100$dwZoom]] <> "%     " <>
	"Distance: "<>If[$dwPreviousClickPtNoGrid =!= Null && $dwClickPtNoGrid =!= Null, ToString[Round[EuclideanDistance[$dwPreviousClickPtNoGrid, $dwClickPtNoGrid],.01]],"0"] <>
	"     Angle: "<>If[$dwPreviousClickPtNoGrid =!= Null && $dwClickPtNoGrid =!= Null, ToString[Round[(Quiet@ToPolarCoordinates[Subtract[Sequence@@(#-{0, 0}&/@{$dwClickPtNoGrid, $dwPreviousClickPtNoGrid})]]/.{Indeterminate->0})[[2]]*180/Pi, .01]], "0"] <>
	"\[Degree]     Object total: " <> ToString[$dwObjectQuantity] <>
	"     Point total: "<>ToString[$dwPointQuantity] <>
	If[Head[$dwSelected] === List && $dwSelected =!= {}, "     Selected object ID: " <> ToString[$dwSelected[[1]]], ""] <>
	"     ImageSize: "<> ToString[($dwOverallSize/(2$dwCanvasMouseSpeed)){Max[#[[1]]&/@$dwPlotRange]-Min[#[[1]]&/@$dwPlotRange],Max[#[[2]]&/@$dwPlotRange]-Min[#[[2]]&/@$dwPlotRange]}] <>
	"     " <> $dwMessageBarText

End[] (* End Private Context *)

EndPackage[]