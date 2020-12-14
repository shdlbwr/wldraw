(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRender[]:=
	EventHandler[
		
		MouseAppearance[
			     
			Dynamic[Magnify[
				Graphics[{
						(* canvas boundary area *)
						If[Length[$dwPlotRange] == 2,
							{EdgeForm[{Opacity[1, Darker[$dwCanvasBackgroundColor,.15]]}], GrayLevel[1,.5], Polygon[{$dwPlotRange[[1]], {$dwPlotRange[[2,1]],$dwPlotRange[[1,2]]}, $dwPlotRange[[2]], {$dwPlotRange[[1,1]],$dwPlotRange[[2,2]]}}]},
							{}
						],
						
						(* before objects so it does not cover object annotations *)
						dwRenderTemplate[],
						If[FreeQ[{"anytimemove", "move", "zoomwireframe"}, $dwMode](* || $dwGridStep >= .05*),
							dwRenderGrid[$dwGridRenderIncrement],
							{}
						],
						(*If[$dwCurrentPreviewMode === "wireframe" && $dwShowSelectionBox,
							dwRenderSelectionBox[],
							Nothing
						],*)
						(*If[$dwMode === "plotrange",
							Nothing,
							dwRenderPlotRange[]
						],*)
						
						(* user graphics *)
						dwRenderLayers[],
						
						(* extras *)
						If[$dwMode === "plotrange",
							dwRenderPlotRange[],
							{}
						],
						dwRenderGroupBox[],
						dwRenderMouseClickPositions[],
						If[FreeQ[{"anytimemove", "move", "zoomlayer", "zoomwireframe"}, $dwMode] && $dwShowSelectionBox,
							dwRenderSelectionBox[],
							{}
						],
						dwRenderTransformInfo[],
						dwRenderDragSelect[],
						dwRenderZoom[]
						(*dwRenderAxoVector[]*) (* uncomment to constrain 3D movement to horizontal or vertical by click canvas vectors *)
						
					}, Background->$dwCanvasBackgroundColor,
					GridLines->
						If[$dwShowGrid && $dwGridStep*$dwZoom >= .025,
							If[$dwConstrainHAngle >= 0,
								{
									Table[n, {n, -.5$dwGrid2DRange $dwGridRenderIncrement, .5$dwGrid2DRange $dwGridRenderIncrement, $dwGridRenderIncrement $dwGridStep}],
									Table[n, {n, -.5$dwGrid2DRange $dwGridRenderIncrement, .5$dwGrid2DRange $dwGridRenderIncrement, $dwGridRenderIncrement $dwGridStep}]
								},
								None
							],
							None
						],
					GridLinesStyle->Directive[AbsoluteThickness[1/$dwZoom], GrayLevel[.8, .5]],
					PlotRange->If[CurrentValue[EvaluationNotebook[], WindowSize] === {Full, Full},
							$dwWindowSize = #[[2]]&/@CurrentValue[ScreenRectangle];
							((({{-$dwCanvasMouseSpeed,$dwCanvasMouseSpeed},{-$dwCanvasMouseSpeed,$dwCanvasMouseSpeed}}($dwWindowSize-{$dwToolWidth,$dwStyleHeight}))/$dwOverallSize)/$dwZoom)-$dwOrigin-$dwOriginOffset,
							((({{-$dwCanvasMouseSpeed,$dwCanvasMouseSpeed},{-$dwCanvasMouseSpeed,$dwCanvasMouseSpeed}}(CurrentValue[EvaluationNotebook[], WindowSize]-{$dwToolWidth,$dwStyleHeight}))/$dwOverallSize)/$dwZoom)-$dwOrigin-$dwOriginOffset
						], 
					ImageSize->Full
				], 
			$dwZoom], SynchronousUpdating->False(*$dwSynchronousUpdating*)],

			dwRenderCursors[],
			Scaled[{.5,.5}]
		], 
		
		{
			"MouseDown" :> (dwMouseDown[]),
			"MouseDragged" :> (dwMouseDrag[]),
			"MouseClicked" :> (dwMouseClicked[]),
			"MouseUp" :> (dwMouseUp[])
		}
	]

End[] (* End Private Context *)

EndPackage[]