(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwCanvasTools[]:=
	Module[{filename, temp, temp2},
		{
			(* help *)
			dwHelp[],
			
			(* preferences *)
			Tooltip[Button[Style["P",18,White],
				CreateDialog[dwPreferences[], WindowTitle->"Preferences"],
				Background->Hue[.25,.7,.7], $dwButtonStyle, BaselinePosition->Scaled[.4]],
			"Preferences", TooltipDelay->$dwTooltipDelay],
			
			Spacer[20],
			
			(* preview | wireframe *)
			dwPreviewButton[],
			
			Spacer[20],
			
			(* undo *)
			Tooltip[Button[$dwIconUndo,
				If[Length[$dwUndo]>1,
					$dwRedo = DeleteDuplicates[Prepend[$dwRedo, dwGetCurrentState[]]];
					dwSetCurrentState[Hold[$dwUndo], -1];
					$dwUndo = DeleteDuplicates[Delete[$dwUndo, -1]]
				],
				$dwButtonStyle],
			"Undo", TooltipDelay->$dwTooltipDelay],
			
			(* redo *)
			Tooltip[Button[$dwIconRedo,
				If[Length[$dwRedo]>0,
					$dwUndo = DeleteDuplicates[Append[$dwUndo, dwGetCurrentState[]]];
					dwSetCurrentState[Hold[$dwRedo], 1];
					$dwRedo = DeleteDuplicates[Delete[$dwRedo, 1]]
				],
				$dwButtonStyle],
			"Redo", TooltipDelay->$dwTooltipDelay],
			
			(* delete *)
			Tooltip[Button[$dwIconObjectDelete,
				dwSetUndo[];
				If[$dwMode === "toggleCornerPt",
					dwConvertPointSelectionToLayerSelection[]
				];
				dwDeleteLayer["SetUndo"->False];
				If[MemberQ[$dwP,{}], dwDeleteEmptyLayers[]],
				$dwButtonStyle], 
			"Delete selected object or points", TooltipDelay->$dwTooltipDelay],
			
			(* select - do not uncomment Dynamic@ before Button below because it causes intermittent kernel crash *)
			Tooltip[Button[$dwIconObjectSelect,
				dwConvertPointSelectionToLayerSelection[], 
				Background->Dynamic@If[MemberQ[{"preview", "wireframe"}, $dwMode], $dwButtonHighlight, $dwButtonBackgroundColor], $dwButtonStyle], 
			"Object selection\n-----\nClick or drag to select objecta\nShiftKey-click to add objects to selection\n"<>$dwOptionKey<>"-click to select inside group\nDrag to select object inside group", TooltipDelay->$dwTooltipDelay],
			
			(* select point - do not uncomment Dynamic@ before Button below because it causes intermittent kernel crash *)
			Tooltip[Button[$dwIconPointSelect,
				dwConvertLayerSelectionToPointSelection[];
				dwUpdateTransformOrigin[];
				(*$dwSelected = {};*)
				$dwMode = "point",
				Background->Dynamic@If[$dwMode === "point", $dwButtonHighlight, $dwButtonBackgroundColor], $dwButtonStyle],
				Dynamic@If[MemberQ[{BezierCurve}, 
					Which[
						$dwSelected === {}, {},
						Length[$dwSelected[[1]]] == 2, $dwHead[[$dwSelected[[1, 1]]]],
						True, $dwHead[[$dwSelected[[1]]]]
					]
				],
				"Point selection\n-----\nClick or drag to select points\nShiftKey-click to add points to selection\nDrag handle for smooth point\n"<>$dwOptionKey<>"-drag handle for corner point",
				"Point selection\n-----\nClick or drag to select points\nShiftKey-click to add points to selection"
			], TooltipDelay->$dwTooltipDelay],
			
			(* selection menu *)
			Tooltip[dwSelectionMenu[],
			"Selection menu", TooltipDelay->$dwTooltipDelay],
			
			(* zoom *)
			Tooltip[Button[$dwIconZoom,
				Which[
					CurrentValue[$dwCommandKey],
						$dwZoom = 1,
					True,
						If[$dwCurrentPreviewMode === "preview",
							$dwMode="zoomlayer",
							$dwMode="zoomwireframe"
						]
				],
				Background->Dynamic@If[MemberQ[{"zoomlayer","zoomwireframe"},$dwMode],$dwButtonHighlight,$dwButtonBackgroundColor],$dwButtonStyle],
			"Zoom canvas\n-----\nClick canvas to zoom in\n"<>$dwOptionKey<>"-click canvas to zoom out\nDrag canvas for zoom area\n"<>$dwCommandKey<>"-click this button for actual size\n-----------------------------------------------\nZoom anytime without clicking this button by pressing\n"<>$dwCommandKey<>" and "<>$dwOptionKey<>" while dragging canvas vertically\n-----------------------------------------------", TooltipDelay->$dwTooltipDelay],
			
			(* move *)
			Tooltip[Button[$dwIconMoveCanvas,
				If[CurrentValue[$dwCommandKey],
					$dwOrigin = $dwOriginStart = If[$dwConstrainHAngle < 0, (* 3D center *){0,0}, {0,0}],
					$dwPStart = $dwP;(* for case when move tool clicked directly after object tool before mousedown on canvas *)
					$dwMode = "canvas"
				],
				Background->Dynamic@If[$dwMode==="canvas", $dwButtonHighlight, $dwButtonBackgroundColor],
				$dwButtonStyle], 
			"Move canvas\n-----\nDrag canvas to move canvas\n"<>$dwCommandKey<>"-click this button to center canvas\n----------------------------------------\nMove canvas anytime without clicking this button\nby pressing "<>$dwCommandKey<>" while dragging canvas\n----------------------------------------", TooltipDelay->$dwTooltipDelay],
			
			(* boundary *)
			dwSetBoundary[],
			
			(* template *)
			Tooltip[Button[$dwIconTemplate,
  				dwImportTemplate[], $dwButtonStyle], 
  			"Canvas template"],
			
			(* auto draw *)
			DynamicModule[{max,pts,fc,g,p,finalPts,image=Null,imageFinal,invert=False,binarize=.5,length,pos},
				Tooltip[Button[$dwIconAutoDraw,
				dwConvertPointSelectionToLayerSelection[]; 
				CreateDialog[Pane[Column[{
					"Paste bitmap image to auto-draw then press tab key",
					InputField[Dynamic@image,ImageSize->{490,200}],
					Row@{"Invert ",Checkbox[Dynamic@invert]},
					Row@{"Adjust ",Slider[Dynamic@binarize,{0,1}]},
					"GENERATED PREVIEW",
					Dynamic@Grid[{
						{
							If[MemberQ[{Image,Graphics}, Head@image],
								If[invert,ColorNegate,Sequence][
									ImageAdjust[ColorQuantize[Quiet@ImageExposureCombine[{
										HistogramTransform[ColorConvert[ColorConvert[image, "Grayscale"],"Grayscale"],NormalDistribution[1,.1]],
										HistogramTransform[ColorConvert[ColorConvert[image, "Grayscale"],"Grayscale"],NormalDistribution[binarize,(1-.9binarize)]],
										HistogramTransform[ColorConvert[ColorConvert[image, "Grayscale"],"Grayscale"],NormalDistribution[0,.1]],
										ColorNegate[EdgeDetect[image]]
									}],2]]
								],""],
							If[MemberQ[{Image,Graphics}, Head@image],If[invert,Binarize[ColorNegate[image],1-binarize],Binarize[image,1-binarize]],""]
						},
						{
							Button["Single-line",
								If[MemberQ[{Image,Graphics}, Head@image],
									If[Total[ImageDimensions[image]] > 2000,
										image = ImageResize[image, {2000}]
									];
									imageFinal=If[invert,Sequence,ColorNegate][
										ImageAdjust[ColorQuantize[Quiet@ImageExposureCombine[{
											HistogramTransform[ColorConvert[ColorConvert[image, "Grayscale"],"Grayscale"],NormalDistribution[1,.1]],
											HistogramTransform[ColorConvert[ColorConvert[image, "Grayscale"],"Grayscale"],NormalDistribution[binarize,(1-.9binarize)]],
											HistogramTransform[ColorConvert[ColorConvert[image, "Grayscale"],"Grayscale"],NormalDistribution[0,.1]],
											ColorNegate[EdgeDetect[image]]
										}],2]]
									];
									
									pos = PixelValuePositions[ImageAdjust[imageFinal], 1];
									pts = FindShortestTour[pos];
									finalPts = pos[[pts[[2]]]];
									max=.5Max[finalPts];
									length=Length[$dwP];
									dwNewEmptyLayer["Head"->Line];
									$dwP[[-1]]={-1,-1}+(1/max)#&/@finalPts;
									dwUpdateBoundingBox[{-1}];
									$dwPointQuantity = Length[Flatten[$dwP, 1]];
									$dwSelected = {};
									$dwGroupLayers = Join[$dwGroupLayers, {Range[length+2, Length[$dwP]]}],
									Nothing
								];
								DialogReturn[],
								ImageSize->245],
							Button["Autotrace",
								If[MemberQ[{Image,Graphics}, Head@image],
									If[Total[ImageDimensions[image]] > 2000,
										image = ImageResize[image, {2000}]
									];
									imageFinal=If[invert,Binarize[image,binarize],Binarize[ColorNegate[image],binarize]];
									(*----------*)
									g=Show[ImageMesh[imageFinal]];
									pts=Cases[g,_GraphicsComplex,Infinity][[1,1]];
									fc=Cases[g[[1]],_FilledCurve,Infinity];
									fc=Table[Flatten[fc[[n,1]]],{n,Length@fc}];
									p=Cases[g[[1]],_Polygon,Infinity];
									finalPts=Join[Flatten[Table[Flatten[Table[pts[[fc[[n1,n2,n3]]]],{n2,Length@fc[[n1]]},{n3,Length@fc[[n1,n2]]}],1],{n1,Length@fc}],1],
									If[p==={},{},Flatten[Table[pts[[p[[1,n,n2]]]],{n,Length@p[[1]]},{n2,Length@p[[1,n]]}],1]]];
									(*----------*)
									(* the code below did not work for compound shapes so had to use Graphics in the code above *)
									(*p=MeshPrimitives[ImageMesh[imageFinal],2];
									(*----------*)
									finalPts=#[[1]]&/@p;*)
									max=.5Max[finalPts];
									length=Length[$dwP];
									Do[
										dwNewEmptyLayer["Head"->Polygon];
										$dwP[[-1(*$dwSelected[[-1]]*)]]={-1,-1}+(1/max)#&/@finalPts[[n]];
										dwUpdateBoundingBox[{-1}],
									{n, Length@finalPts}];
									$dwPointQuantity = Length[Flatten[$dwP, 1]];
									$dwSelected = {};
									$dwGroupLayers = Join[$dwGroupLayers, {Range[length+2, Length[$dwP]]}],
									Nothing
								];
								DialogReturn[],
								ImageSize->245
							]
						}
					}, Spacings->{Automatic,0}],
					Row[{
						Button["Cancel",DialogReturn[],ImageSize->200]
					}]
				},Alignment->Center],ImageSize->{500,700}],WindowTitle->"AutoShape",Modal->True], $dwButtonStyle],
				"Auto-draw from bitmap image...", TooltipDelay->$dwTooltipDelay]
			],
			
			(* insert expression *)
			Tooltip[Button[$dwIconInsertExpression,
				$dwMode = $dwCurrentPreviewMode;
				$dwStyleMode = "expression";
				dwNewEmptyLayer["Form"->"expression", "Head"->"Expression"];
				dwUpdateBoundingBox[{-1}], $dwButtonStyle],
			"Insert expression...", TooltipDelay->$dwTooltipDelay],

			(* animate *)
			Tooltip[
				Button[$dwIconAnimate,
					If[$dwP === {},
						MessageDialog["No objects to animate."],
						dwAnimate[]
					], $dwButtonStyle],
			"Animate all objects...", TooltipDelay->$dwTooltipDelay],
  			
  			(* save - $dwObjectGradients not saved since it is generated when opened *)
  			Tooltip[Button[$dwIconSaveFile,
  				filename=SystemDialogInput["FileSave", "untitled.wldraw", WindowTitle->"Save WLDraw file"];
				If[filename=!=$Canceled,
					filename=If[!StringMatchQ[filename,"*.wldraw"],filename<>".wldraw",filename];
					Export[Evaluate@filename, Compress[{"WLDraw", $dwP, $dwHead, $dwStyle, $dwPlotRange, $dwTemplate, $dwGroupLayers, $dwHideLayers, $dwCompoundPathLayers, $dwLineGradients, $dwAnimate, {$dwTilt, $dwTurn}}], "Text"]
				], Method->"Queued", $dwButtonStyle],
			"Save WLDraw file", TooltipDelay->$dwTooltipDelay],
  			
  			(* open *)
			Tooltip[Button[$dwIconOpenFile,
  				filename=SystemDialogInput["FileOpen", CurrentValue["NotebookBrowseDirectory"], WindowTitle->"Select a saved WLDraw file to open (xxxx.wldraw)"];
					If[filename=!=$Canceled,
						
						$dwMessageBarText = "Opening file...";
						(*$dwCurrentPreviewMode = "preview"; (* pink box error in wireframe mode but disappears when all objects are loaded *)*)
						Quiet@If[StringMatchQ[filename, {"*.wldraw"}], 
							temp = Uncompress[Import[filename, "Text"]];
							If[ToString[temp[[1]]] === "WLDraw",
								$dwMessageBarText = "Loading objects...";
								dwOpenFile[temp],
								MessageDialog["File is not supported by WLDraw."]
							],
							
							MessageDialog["File is not supported by WLDraw."]
							(*temp = NotebookImport[filename][[1]];
							If[Head[temp] === HoldComplete && temp[[1,1]] === "WLDraw",
								$dwMessageBarText = "Loading objects...";
								dwOpenFile[temp[[1]]],
								MessageDialog["File is not supported by WLDraw."]
							]*)
						];
						$dwMessageBarText = "";
					], Method->"Queued", $dwButtonStyle],
			"Open WLDraw file", TooltipDelay->$dwTooltipDelay],
			
			(* copy to clipboard *)
			Tooltip[Button[$dwIconClipboard,
				temp2 = {$dwMode, $dwShowImageBlends};
				$dwMode = "wireframe";
				$dwShowImageBlends = True;
				temp = dwWLDrawToGraphics["LayerNumbers"->"All", "PlotRange"->Automatic, "ImageSize"->Automatic];
				$dwShowImageBlends = temp2[[2]];
				$dwMode = temp2[[1]];
				(* replace text styles with stylesheet format *)
				Do[(
					temp[[Sequence@@#[[;; Length[#] - 2]]]] = 
						If[name === "NoStyle",
							Extract[temp, #[[;; Length[#] - 2]]][[1]],
							Style[Extract[temp, #[[;; Length[#] - 2]]][[1]], 
								name, 
								Flatten[
									Table[
										Extract[Extract[temp, #[[;; Length[#] - 2]]], Position[Extract[temp, #[[;; Length[#] - 2]]], ext]], 
									{ext, {FontSize->_, FontColor->_, FontOpacity->_, FontSlant->_, FontTracking->_, FontWeight->_, LineSpacing->_, TextAlignment->_}}]
								]
							]
						]
					)&/@Position[temp, name], 
				{name, $dwStylesheetTextFormats}];
				(* remove rotations if zero degrees *)
				temp = temp/. {Rotate[rotatedgraphics_, 0.] :> rotatedgraphics};
				(* copy to clipboard *)
				CopyToClipboard[temp],
				Method->"Queued", $dwButtonStyle], 
			"Copy styled graphics to clipboard\nShiftKey-click to copy styled graphics without boundary or size\n"<>$dwCommandKey<>"-click to copy unstyled graphics without boundary or size", TooltipDelay->$dwTooltipDelay],
  			
  			(* import *)
			Tooltip[Button[$dwIconImportFile,
  				filename=SystemDialogInput["FileOpen", ".nb", WindowTitle->"Select a notebook with graphics to open"];
				If[filename=!=$Canceled,
					$dwCurrentPreviewMode = "preview"; (* pink box error in wireframe mode but disappears when all objects are loaded *)
					dwGraphicsToWLDraw[NotebookImport[filename]]
				], Method->"Queued", $dwButtonStyle],
			"Import notebook graphics\n"<>$dwCommandKey<>"-click to load unstyled and unordered graphics", TooltipDelay->$dwTooltipDelay],
			
			(* SystemModeler icon export *)
			Tooltip[
				Button[$dwIconModelicaExport, dwExportToWSMicon[], Method->"Queued", $dwButtonStyle],
			"SystemModeler icon\n-----\nSave Arrows, Lines, Polygons and BSplineCurves as an icon of a SystemModeler file.", TooltipDelay->$dwTooltipDelay],
			
			(* 2D | 3D grid *)
			Spacer[8], dwGridPopupMenu[]

		}
	]
	
dwPreviewButton[]:=
	Tooltip[Dynamic@Button[If[$dwCurrentPreviewMode === "wireframe" || MemberQ[{"canvas","plotrange"}, $dwMode], $dwIconWireframe, $dwIconPreview],
		If[$dwCurrentPreviewMode === "wireframe" || MemberQ[{"canvas","plotrange"}, $dwMode],
			dwConvertPointSelectionToLayerSelection[];
			$dwMode = $dwCurrentPreviewMode = "preview",
			dwConvertPointSelectionToLayerSelection[];
			$dwMode = $dwCurrentPreviewMode = "wireframe";
		],
		$dwButtonStyle], 
	"Preview / Wireframe toggle", TooltipDelay->$dwTooltipDelay]
	
dwSetBoundary[]:=
	Tooltip[Button[$dwIconPlotRange,
			$dwMode = "plotrange";
			$dwShowPlotRange = True;
			If[CurrentValue[$dwOptionKey],
				CreateDialog[
					DynamicModule[{imageSize = ($dwOverallSize/(2.0$dwCanvasMouseSpeed)){Max[#[[1]]&/@$dwPlotRange]-Min[#[[1]]&/@$dwPlotRange],Max[#[[2]]&/@$dwPlotRange]-Min[#[[2]]&/@$dwPlotRange]}},
						Pane[
							Dynamic@Column[{
								Style["{width, height}", 12],
								Style[InputField[Dynamic[imageSize], ImageSize -> {200, 30}], 12],
								Row[{
									Button["Cancel", DialogReturn[]],
									DefaultButton[
										If[	(* valid image size *)
											MemberQ[{Real, Integer}, Head[#]]&/@imageSize === {True,True},
												$dwPlotRange = Partition[Riffle[Sequence@@({-#/2, #/2}&/@(imageSize/($dwOverallSize/(2 $dwCanvasMouseSpeed))))], 2],
											Nothing
										];
										DialogReturn[]
									]
								}]
							},Alignment->Center],
						ImageSize->200]
					],
				Background->LightGray, WindowTitle->"Enter ImageSize", Modal->True],

				$dwPStart = $dwP;
				Which[
					CurrentValue[$dwCommandKey],
						$dwPlotRange = dwFindPlotRange[Length[$dwP], "ForSetBoundary"->True],
					CurrentValue["ShiftKey"],
						$dwPlotRange = dwFindPlotRange[Length[$dwP], "ForSetBoundary"->True, "Padding"->0],
					True,
						Nothing
				]
			],
		Background->Dynamic@If[$dwMode === "plotrange",$dwButtonHighlight,$dwButtonBackgroundColor],$dwButtonStyle],
	"Resize boundary\n-----\nDrag corner dots of yellow box to resize\n"<>$dwCommandKey<>"-click button to fit boundary to all objects\nShift-click button fit boundary to all objects without padding\n"<>$dwOptionKey<>"-click button to enter image size", TooltipDelay->$dwTooltipDelay]
	
dwGridPopupMenu[]:=
	ActionMenu[Dynamic@If[$dwConstrainHAngle < 0, "3D grid", "2D grid"], 
		{
			"2D grid":>($dwConstrainHAngle = $dwConstrainVAngle = 0; dwSetGridSize[]),
			"3D grid":>(dwUpdateAxonometricAngles[$dwTilt,$dwTurn]; dwSetGridSize[]),
			Delimiter,
			Dynamic@If[$dwGridStep == $dwGridStepNone, "> Grid off", "Grid off"]:>($dwGridStep = $dwGridStepNone; dwSetGridSize[]),
			Dynamic@If[$dwGridStep == $dwGridStepVeryTiny, "> VeryTiny", "VeryTiny"]:>($dwGridStep = $dwGridStepVeryTiny; dwSetGridSize[]),
			Dynamic@If[$dwGridStep == $dwGridStepTiny, "> Tiny", "Tiny"]:>($dwGridStep = $dwGridStepTiny; dwSetGridSize[]),
			Dynamic@If[$dwGridStep == $dwGridStepSmall, "> Dense", "Dense"]:>($dwGridStep = $dwGridStepSmall; dwSetGridSize[]),
			Dynamic@If[$dwGridStep == $dwGridStepMedium, "> Moderate", "Moderate"]:>($dwGridStep = $dwGridStepMedium; dwSetGridSize[]),
			Dynamic@If[$dwGridStep == $dwGridStepLarge, "> Sparse", "Sparse"]:>($dwGridStep = $dwGridStepLarge; dwSetGridSize[]),
			Delimiter,
			Dynamic@If[$dwShowAxes,"Hide axes and grid","Show axes and grid"]:>($dwShowAxes = $dwShowGrid = If[$dwShowAxes, False, True]),
			Delimiter,
			Dynamic@If[$dwShowMouseClickPositions,"Hide position markers","Show position markers"]:>(
				$dwPrevious2ClickPtNoGrid = $dwPreviousClickPtNoGrid = $dwClickPtNoGrid = Null;
				$dwShowMouseClickPositions = If[$dwShowMouseClickPositions, False, True];
				If[$dwShowMouseClickPositions, $dwSelected = {}; $dwMode = $dwCurrentPreviewMode, Nothing]
			)
		}, ImageSize->{99,24}, BaselinePosition->Scaled[.37]]
	
dwPreferences[]:=
	Pane[
		Dynamic@Column[{
		Panel[Grid[{
				{Style["APPEARANCE", 15]},
				{Grid[{
					{Row[{Checkbox[Dynamic@$dwShowGrid], " show grid", Spacer[77], 
						"3D grid range ", PopupMenu[Dynamic@$dwGrid3DRange, Range[4]]}]},
					{Row[{Checkbox[Dynamic@$dwShowAxes], " show axis", Spacer[78], Spacer[{0,18}], 
						Checkbox[Dynamic@$dwFixScaleAspectRatioOpenedFile], " fix imported file"}]}
					}, Alignment->Left, Spacings->{0,0}]}
			}, Alignment->Center], Alignment->Center, ImageSize->330],
		Panel[Grid[{
			{Grid[{{Style["MOVEMENT", 15]},
				{""},
				{Row[{Style["press ShiftKey when dragging to constrain movement", Italic]}]},
				{Row[{"constrain angle ", dwConstrainAngleMenu[], Spacer[30], "nudge multiplier ", PopupMenu[Dynamic@$dwNudgeMultiplier, Range[20]], Spacer[{0,24}]}]}
				}, Alignment->Center, Spacings->{0,0}]}
			}, Alignment->Center, Spacings->{0,0}], Alignment->Center, ImageSize->330],
		Panel[Grid[{
				{Style["TRANSFORM",15]},
				{""},
				{Row[{EventHandler[Checkbox[Dynamic@$dwTransformEach],{"MouseClicked"->(dwUpdateTransformOrigin[])}, PassEventsDown->True], " each object"}]},
				{Row[{Spacer[{0,24}], "rotate step ", PopupMenu[Dynamic@$dwRotateStep, Join[{0.5}, Range[10]]], Spacer[30], " scale step ", PopupMenu[Dynamic@$dwScaleStep, Join[Range[10], {15,20,25}]]}]}
			}, Alignment->Center, Spacings->{0,0}], Alignment->Center, ImageSize->330],
		Panel[Grid[{
			{Grid[{{Style["SELECTION", 15]},
				{""},
				{Row[{Style["object size is based on the area of its bounding box", Italic]}]},
				{Row[{"small object size ", PopupMenu[Dynamic@$dwSmallObjectSize, Join[{.0001, .001}, Range[.01,.1,.01]]],"   large object size ", PopupMenu[Dynamic@$dwLargeObjectSize, Range[.1,1,.1]], Spacer[{0,24}]}]}
				}, Alignment->Center, Spacings->{0,0}]}
			}, Alignment->Center, Spacings->{0,0}], Alignment->Center, ImageSize->330]
		}], 
	ImageSize->330]

End[] (* End Private Context *)

EndPackage[]