(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwOptionSlider]={"Name"->"Size","SliderName"->"size ","Range"->{0,1},"Increment"->.1,"Default"->0};
dwOptionSlider[shape_, OptionsPattern[]]:=
DynamicModule[{},
	With[{n=OptionValue["Name"],sn=OptionValue["SliderName"],r=OptionValue["Range"],i=OptionValue["Increment"],d=OptionValue["Default"]},
		$dwShapeOptions=Options[shape];
		Row[{Pane[sn,50,Alignment->Right],
			Slider[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1]],{r[[1]],r[[2]],i}],Button["<",$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1]]=If[CurrentValue[$dwCommandKey],d,Max[Round[$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1]]-=i,i],r[[1]]]],Appearance->"Palette"],
			Button[">",$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1]]=If[CurrentValue[$dwCommandKey],d,Min[Round[$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1]]+=i,i],r[[2]]]],Appearance->"Palette"],
			" ",Pane[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1]],50]
		}]
	]
]

Options[dwOptionSliderPart]={"Name"->"Size","SliderName"->"size ","Range"->{0,1},"Increment"->.1,"Default"->0,"Part"->1,"ConvertRadiansToDegrees"->False};
dwOptionSliderPart[shape_, OptionsPattern[]]:=
DynamicModule[{},
	With[{n=OptionValue["Name"],sn=OptionValue["SliderName"],r=OptionValue["Range"],i=OptionValue["Increment"],d=OptionValue["Default"],p=OptionValue["Part"],degrees=OptionValue["ConvertRadiansToDegrees"]},
		$dwShapeOptions=Options[shape];
		Row[{Pane[sn,50,Alignment->Right],
			Slider[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p]],{r[[1]],r[[2]],i}],Button["<",$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p]]=If[CurrentValue[$dwCommandKey],d,Max[Round[$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p]]-=i,i],r[[1]]]],Appearance->"Palette"],
			Button[">",$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p]]=If[CurrentValue[$dwCommandKey],d,Min[Round[$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p]]+=i,i],r[[2]]]],Appearance->"Palette"],
			" ",Pane[Dynamic@($dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p]]*If[degrees,180/Pi,1]),50]
		}]
	]
]

Options[dwOptionSliderPart2]={"Name"->"Size","SliderName"->"size ","Range"->{0,1},"Increment"->.1,"Default"->0,"Part1"->1,"Part2"->1};
dwOptionSliderPart2[shape_, OptionsPattern[]]:=
DynamicModule[{$dwShapeOptions = Options[shape]},
	With[{n=OptionValue["Name"],sn=OptionValue["SliderName"],r=OptionValue["Range"],i=OptionValue["Increment"],d=OptionValue["Default"],p1=OptionValue["Part1"],p2=OptionValue["Part2"]},
		$dwShapeOptions=Options[shape];
		Row[{Pane[sn,50,Alignment->Right],
			Slider[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p1,p2]],{r[[1]],r[[2]],i}],Button["<",$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p1,p2]]=If[CurrentValue[$dwCommandKey],d,Max[Round[$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p1,p2]]-=i,i],r[[1]]]],Appearance->"Palette"],
			Button[">",$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p1,p2]]=If[CurrentValue[$dwCommandKey],d,Min[Round[$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p1,p2]]+=i,i],r[[2]]]],Appearance->"Palette"],
			" ",Pane[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,n->_][[1,1]]]][[-1,p1,p2]],50]
		}]
	]
]

(* 	shapes requiring temporary object while dialog is open for updated styles:
		dwAngleArrow, dwDimensionArrow
	if shape needs temporary object
		- add to shapesNeedTemp
		- add to sel (located after shapesNeedTemp line)
		- see "ArrowSize" below for example of populating sel and pos variables
*)
dwShapeStyleDialog[shape_]:=
	CreateDialog[
		DynamicModule[{sel, pos, savedStyles, shapesNeedTemp = {dwAngleArrow, dwDimensionArrow}},
			dwSetUndo[];
			$dwShapeStylePreviewOrigin = $dwShapeStylePreviewOriginStart = {0,0};
			$dwShapeStylePreviewDimensionsMagnify = $dwShapeStylePreviewDimensionsMagnifyStart = .75;
			$dwShapeOptions = Options[shape];
			If[MemberQ[shapesNeedTemp, shape], dwGraphicsToWLDraw[Graphics[shape[$dwShapeOptions]]]/.{StrokeForm->EdgeForm}]; (* create temporary object for arrowheads, etc. *)
			sel = Switch[shape, 
					dwAngleArrow, 
						dwAngleArrowPartTotal[], 
					dwDimensionArrow, 
						dwDimensionArrowPartTotal[], 
					_, 
						{}
				];
			Pane[Column[{
				
				EventHandler[
				MouseAppearance[
					Dynamic@Magnify[Show[
						
							Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]],
								Background->White,
								ImageSize->(($dwShapeStylePreviewDimensions/$dwShapeStylePreviewSize)[[1]]*$dwShapeStyleMouseSpeed)*($dwShapeStyleMouseSpeed*$dwOverallSize), 
								PlotRangePadding->.15,
								PlotRange->(.75/$dwShapeStylePreviewDimensionsMagnify)*((({{-$dwShapeStyleMouseSpeed,$dwShapeStyleMouseSpeed},{-$dwShapeStyleMouseSpeed,$dwShapeStyleMouseSpeed}}*$dwShapeStylePreviewDimensions)/($dwShapeStylePreviewSize))-$dwShapeStylePreviewOrigin)
							]/.{AbsolutePointSize[__]->AbsolutePointSize[0]},
						
							If[MemberQ[{dwShapeCloud, dwShapeFire}, shape],
								(* image *)
								Graphics[{{LightGray, InfiniteLine[{{0,0},{1,0}}], InfiniteLine[{{0,0},{0,1}}], Line[{{-1,-1},{1,-1},{1,1},{-1,1},{-1,-1}}]}, Inset[shape[$dwShapeOptions],{0,0},ImageScaled[{.5,.5}],$dwShapeOptions[[Position[$dwShapeOptions,"Size"->_][[1,1]]]][[-1]]]}],
								
								(* graphics *)
								Graphics[{{LightGray, InfiniteLine[{{0,0},{1,0}}], InfiniteLine[{{0,0},{0,1}}], Line[{{-1,-1},{1,-1},{1,1},{-1,1},{-1,-1}}]}, shape[$dwShapeOptions]}]
							],
							
						ImageSize->Full], $dwShapeStylePreviewDimensionsMagnify],
		
					Dynamic@If[CurrentValue[$dwCommandKey] && CurrentValue[$dwOptionKey], $dwIconZoomDrag, $dwCursorCanvas],
					Scaled[{.5,.5}]
				],
					{
						"MouseDown" :> (If[Head[MousePosition["GraphicsScaled"]] === List, $dwClickPtScaled = MousePosition["GraphicsScaled"]]),
						"MouseDragged" :> (
								If[Head[MousePosition["GraphicsScaled"]] === List,
									If[CurrentValue[$dwCommandKey] && CurrentValue[$dwOptionKey],
										$dwShapeStylePreviewDimensionsMagnify = Min[8, Max[.1, $dwShapeStylePreviewDimensionsMagnifyStart + (2*($dwShapeStylePreviewDimensions/$dwShapeStylePreviewSize)(MousePosition["GraphicsScaled"] - $dwClickPtScaled))[[2]]]],
										$dwShapeStylePreviewOrigin = $dwShapeStylePreviewOriginStart + ($dwShapeStylePreviewDimensions/$dwShapeStylePreviewSize)(MousePosition["GraphicsScaled"] - $dwClickPtScaled)
									]
								]),
						"MouseClicked" :> (If[CurrentValue[$dwCommandKey] && !CurrentValue[$dwOptionKey],
								$dwShapeStylePreviewOrigin = $dwShapeStylePreviewOriginStart = {0,0};
								$dwShapeStylePreviewDimensionsMagnify = $dwShapeStylePreviewDimensionsMagnifyStart = .75
							]),
						"MouseUp" :> (
								$dwShapeStylePreviewOriginStart = $dwShapeStylePreviewOrigin;
								$dwShapeStylePreviewDimensionsMagnifyStart = $dwShapeStylePreviewDimensionsMagnify
							)
					}
				],
				
				Row[{
					(* does not update dynamically so do not use *)(*Checkbox[Dynamic@$dwShapeStylePreviewShowPoints], "show points               ",*)
					Button["restore defaults", 
						$dwClickPtNoGrid = Null; $dwPreviousClickPtNoGrid = Null; $dwShapeOptions = Options[shape]; 
						$dwShapeStylePreviewOrigin = $dwShapeStylePreviewOriginStart = {0,0};
						$dwShapeStylePreviewDimensionsMagnify = $dwShapeStylePreviewDimensionsMagnifyStart = .75;
						sel = Switch[shape, dwAngleArrow, dwAngleArrowPartTotal[], dwDimensionArrow, {-1,-2,-3,-4}, _, {}],
					ImageSize->300]
				}],
				
				Row[{"increment for restricted sliders: ",
					ActionMenu[Dynamic@If[$dwGridStep === $dwGridStepNone, "None", $dwGridStep],{
						"None":>($dwGridStep = $dwGridStepNone),
						$dwGridStepVeryTiny:>($dwGridStep = $dwGridStepVeryTiny),
						$dwGridStepTiny:>($dwGridStep = $dwGridStepTiny),
						$dwGridStepSmall:>($dwGridStep = $dwGridStepSmall),
						$dwGridStepMedium:>($dwGridStep = $dwGridStepMedium),
						$dwGridStepLarge:>($dwGridStep = $dwGridStepLarge)
					},Appearance->"PopupMenu"]
				}],
				
				Pane[
					Grid[Join[
						
						(* Order set by option list order of function *)
						{{If[MemberQ[Options[shape],"Axis"|"AxisLocation"|"AxisOrigin"|"AxisRotation"|"BeginRadialAxis"|"BreakCenter"|"BreakWidth"|"BreakWidthNumber"|"BuildPolygons"|"Closed"|"DrawBackFace"|
							"DrawFrontFace"|"EndLength"|"EndLocation"|"EndRadialAxis"|"Extrude"|"FireEdgeFalloff"|"FireFuel"|"FireGlow"|"FireShape"|"FireShift"|"FireSquash"|"FireStretch"|"FireTurbulence"|"Force"|
							"Grid"|"GridFace"|"GridQuantity"|"GridSize"|"GridType"|"Height"|"InteriorLines"|"Length"|"NodeSize"|"NumberOfCells"|"NumberOfTurns"|"Object"|"Position"|"RadialAxis"|"RandomSeed"|
							"Reflect"|"Rotate"|"Rotate3D"|"Segment"|"Segments"|"ShowInteriorLinesAsBorder"|"ShowSegments"|"Size"|"State"|"Taper"|"Text"|"TickDivisions"|"TickFirstLabelValue"|"Value"|"WhiteInteriorLines"|
							"yPositive",Infinity],
							Dynamic@OpenerView[{"basic",
								Column[Table[
									Switch[opt,
										"Axis",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Axis"->_][[1,1]]]][[-1]]],"Axis"},
										"AxisLocation",
											Grid[{
											{""},
											{Row[{Button[Style["reset",10],$dwShapeOptions[[Position[$dwShapeOptions,"AxisLocation"->_][[1,1]]]][[-1]]={0,0,0},Appearance->"Palette",ImageSize->40]
											}]},
											{dwOptionSliderPart[shape, {"Name"->"AxisLocation","SliderName"->"loc x ","Range"->{-6,6},"Increment"->$dwGridStep,"Default"->0,"Part"->1}]},
											{dwOptionSliderPart[shape, {"Name"->"AxisLocation","SliderName"->"loc y ","Range"->{-6,6},"Increment"->$dwGridStep,"Default"->0,"Part"->2}]},
											{dwOptionSliderPart[shape, {"Name"->"AxisLocation","SliderName"->"loc z ","Range"->{-6,6},"Increment"->$dwGridStep,"Default"->0,"Part"->3}]},
											{""}},Spacings->{0,0},Background->GrayLevel[.8]],
										"AxisOrigin",
											Row[{
												"origin:     h ",Dynamic@PopupMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"AxisOrigin"->_][[1,1]]]][[-1,1]],Range[0,1,1/$dwShapeOptions[[Position[$dwShapeOptions,"TickDivisions"->_][[1,1]]]][[-1,1]]]],
												"     v ",Dynamic@PopupMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"AxisOrigin"->_][[1,1]]]][[-1,2]],Range[0,1,1/$dwShapeOptions[[Position[$dwShapeOptions,"TickDivisions"->_][[1,1]]]][[-1,2]]]]
											}],
										(*"AxisRotation",
											Grid[{
											{""},
											{Row@{"orientation ",SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Direction"->_][[1,1]]]][[-1]],{"Right"->"x","Left"->"y","Top"->"z"}],
											"   dominant axis ",SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"RotateOrder"->_][[1,1]]]][[-1]],{"zxy"->"x","zyx"->"y"}]}},
											{Row@{If[MemberQ[{axoText},$dwStyle[[$dwPn,1]]],Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Distort"->_][[1,1]]]][[-1]]]," distort",Spacer[20]},Nothing],
											Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"MoveBeforeRotate"->_][[1,1]]]][[-1]]]," rotate around origin",Spacer[20],
											Button[Style["reset",10],Do[$dwShapeOptions[[Position[$dwShapeOptions,"AxisRotation"->_][[1,1]]]][[-1,n,1]]=0,{n,3}],Appearance->"Palette",ImageSize->40]
											}},
											{dwOptionSliderPart2[shape, {"Name"->"AxisRotation","SliderName"->"rotate x ","Range"->{-180,180},"Increment"->1,"Default"->0,"Part1"->2,"Part2"->1}]},
											{dwOptionSliderPart2[shape, {"Name"->"AxisRotation","SliderName"->"rotate y ","Range"->{-180,180},"Increment"->1,"Default"->0,"Part1"->3,"Part2"->1}]},
											{dwOptionSliderPart2[shape, {"Name"->"AxisRotation","SliderName"->"rotate z ","Range"->{-180,180},"Increment"->1,"Default"->0,"Part1"->1,"Part2"->1}]},
											{""}},Background->GrayLevel[.8]],*)
										"BackgroundFade",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"BackgroundFade"->_][[1,1]]]][[-1]]],"background fade"},
										"BeginRadialAxis",
											dwOptionSlider[shape, {"Name"->"BeginRadialAxis","SliderName"->"begin ","Range"->{0,2Pi},"Increment"->Pi/180,"Default"->0}],
										"BreakCenter",
											dwOptionSlider[shape, {"Name"->"BreakCenter","SliderName"->"break ctr ","Range"->{0,2},"Increment"->.05,"Default"->.5}],
										"BreakWidth",
											dwOptionSlider[shape, {"Name"->"BreakWidth","SliderName"->"break ","Range"->{0,2},"Increment"->$dwGridStep,"Default"->0}],
										"BreakWidthNumber",
											dwOptionSlider[shape, {"Name"->"BreakWidthNumber","SliderName"->"break ","Range"->{0,1},"Increment"->.05,"Default"->0}],
										(*"BuildPolygons",
											dwOptionSlider[shape, {"Name"->"BuildPolygons","SliderName"->"start ","Range"->{0,Length@$dwP[[$dwPn]]-1},"Increment"->1,"Default"->1}],*)
										"Closed",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Closed"->_][[1,1]]]][[-1]]],"close end"},
										"CloudShape",
											Row[{
												"shape: ", PopupMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"CloudShape"->_][[1,1]]]][[-1]], {Automatic, "rising"}]
											}],
										(*"Detail",
											dwOptionSlider[shape, {"Name"->"Detail","SliderName"->"detail ","Range"->{1, 8},"Increment"->$dwGridStep,"Default"->5}],*)
										"DrawBackFace",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"DrawBackFace"->_][[1,1]]]][[-1]]],"show back face"},
										"DrawFrontFace",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"DrawFrontFace"->_][[1,1]]]][[-1]]],"show front face"},
										"EndLength",
											dwOptionSlider[shape, {"Name"->"EndLength","SliderName"->"end size ","Range"->{0,1},"Increment"->.05,"Default"->.1}],
										"EndLocation",
											Grid[{{dwOptionSliderPart[shape, {"Name"->"EndLocation","SliderName"->"end loc h ","Range"->{-10,10},"Increment"->$dwGridStep,"Default"->1,"Part"->1}]},
											{dwOptionSliderPart[shape, {"Name"->"EndLocation","SliderName"->"end loc v ","Range"->{-10,10},"Increment"->$dwGridStep,"Default"->1,"Part"->2}]}
											},Spacings->{0,0}],
										"EndRadialAxis",
											dwOptionSlider[shape, {"Name"->"EndRadialAxis","SliderName"->"end ","Range"->{0,4Pi},"Increment"->Pi/180,"Default"->2Pi}],
										"Extrude",
											dwOptionSlider[shape, {"Name"->"Extrude","SliderName"->"extrude ","Range"->{$dwGridStep,2},"Increment"->$dwGridStep,"Default"->.5}],
										"FireEdgeFalloff",
											dwOptionSlider[shape, {"Name"->"FireEdgeFalloff","SliderName"->"falloff ","Range"->{0, 10},"Increment"->$dwGridStep,"Default"->2}],
										"FireFuel",
											dwOptionSlider[shape, {"Name"->"FireFuel","SliderName"->"fuel ","Range"->{.01, .03},"Increment"->.0025,"Default"->.0225}],
										"FireGlow",
											dwOptionSlider[shape, {"Name"->"FireGlow","SliderName"->"glow ","Range"->{0, 1},"Increment"->$dwGridStep,"Default"->0}],
										"FireShape",
											dwOptionSlider[shape, {"Name"->"FireShape","SliderName"->"shape ","Range"->{.5, 1},"Increment"->.025,"Default"->.9}],
										"FireShift",
											dwOptionSlider[shape, {"Name"->"FireShift","SliderName"->"shift ","Range"->{-1, 1},"Increment"->.025,"Default"->.125}],
										"FireSquash",
											dwOptionSlider[shape, {"Name"->"FireSquash","SliderName"->"squash ","Range"->{0, 1},"Increment"->$dwGridStep,"Default"->0}],
										"FireSqueeze",
											dwOptionSlider[shape, {"Name"->"FireSqueeze","SliderName"->"squeeze ","Range"->{0, 1},"Increment"->$dwGridStep,"Default"->0}],
										"FireStretch",
											dwOptionSlider[shape, {"Name"->"FireStretch","SliderName"->"stretch ","Range"->{0, 1},"Increment"->$dwGridStep,"Default"->0}],
										"FireTurbulence",
											dwOptionSlider[shape, {"Name"->"FireTurbulence","SliderName"->"turb ","Range"->{7, 15},"Increment"->$dwGridStep,"Default"->10}],
										"Force",
											dwOptionSlider[shape, {"Name"->"Force","SliderName"->"force ","Range"->{0,2},"Increment"->.1,"Default"->0}],
										"Frame",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Frame"->_][[1,1]]]][[-1]]],"frame"},
										(* might cause top of window to disappear because of scrolling issues *)
										"Graphics",
											Column@{"Paste 2d graphics or image",InputField[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Graphics"->_][[1,1]]]][[-1]],ImageSize->{280,150}]},
										"GridFace",
											Row@{Pane["visible ",50,Alignment->Right],TogglerBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"GridFace"->_][[1,1]]]][[-1]],{"xBack","xFront","yBack","yFront","Bottom","Top"}]},
										"Grid",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Grid"->_][[1,1]]]][[-1]]],"grid"},
										"GridOnly",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"GridOnly"->_][[1,1]]]][[-1]]],"grid only"},
										"GridQuantity",
											Grid[{
											{dwOptionSliderPart[shape, {"Name"->"GridQuantity","SliderName"->"Xquantity ","Range"->{1,20},"Increment"->1,"Default"->5,"Part"->1}]},
											{dwOptionSliderPart[shape, {"Name"->"GridQuantity","SliderName"->"Yquantity ","Range"->{1,20},"Increment"->1,"Default"->5,"Part"->2}]},
											{dwOptionSliderPart[shape, {"Name"->"GridQuantity","SliderName"->"Zquantity  ","Range"->{1,20},"Increment"->1,"Default"->5,"Part"->3}]}
											}],
										"GridSize",
											Grid[{
											{dwOptionSliderPart[shape, {"Name"->"GridSize","SliderName"->"Xgridsize ","Range"->{$dwGridStep,2},"Increment"->$dwGridStep,"Default"->1,"Part"->1}]},
											{dwOptionSliderPart[shape, {"Name"->"GridSize","SliderName"->"Ygridsize ","Range"->{$dwGridStep,2},"Increment"->$dwGridStep,"Default"->1,"Part"->2}]},
											{dwOptionSliderPart[shape, {"Name"->"GridSize","SliderName"->"Zgridsize  ","Range"->{$dwGridStep,2},"Increment"->$dwGridStep,"Default"->1,"Part"->3}]}
											}],
										"GridType",
											Row@{"grid type ", SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"GridType"->_][[1,1]]]][[-1]], {"Grid","Horizontal","Vertical","Point","XBar","YBar"}]},
										"Height",
											dwOptionSlider[shape, {"Name"->"Height","SliderName"->"height ","Range"->{-5,5},"Increment"->$dwGridStep,"Default"->1}],
										"InteriorLines",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"InteriorLines"->_][[1,1]]]][[-1]]],"show interior lines (complex shapes may error if unchecked)"},
										"Length",
											dwOptionSlider[shape, {"Name"->"Length","SliderName"->"length ","Range"->{0,4},"Increment"->$dwGridStep,"Default"->1}],
										"NumberOfTurns",
											dwOptionSlider[shape, {"Name"->"NumberOfTurns","SliderName"->"turns ","Range"->{1,10},"Increment"->1,"Default"->3}],
										"NumberOfCells",
											dwOptionSlider[shape, {"Name"->"NumberOfCells","SliderName"->"cells ","Range"->{1,10},"Increment"->1,"Default"->1}],
										"NodeSize",
											dwOptionSlider[shape, {"Name"->"NodeSize","SliderName"->"node size ","Range"->{0,3},"Increment"->.01,"Default"->1}],
										"Object",
											Column@{"Paste 3d object (Sphere[{0,0,0}] not Graphics3D[{Sphere[{0,0,0}]}])",InputField[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Object"->_][[1,1]]]][[-1]],ImageSize->{280,150}]},
										"Position",
											Grid[{{dwOptionSliderPart[shape, {"Name"->"Position","SliderName"->"loc h ","Range"->{-5,5},"Increment"->$dwGridStep,"Default"->0,"Part"->1}]},
											{dwOptionSliderPart[shape, {"Name"->"Position","SliderName"->"loc v ","Range"->{-5,5},"Increment"->$dwGridStep,"Default"->0,"Part"->2}]}
											},Spacings->{0,0},Background->GrayLevel[.8]],
										"RadialAxis",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"RadialAxis"->_][[1,1]]]][[-1]]], "radial axis"},
										"RandomSeed",
											dwOptionSlider[shape, {"Name"->"RandomSeed","SliderName"->"random ","Range"->{1, 20},"Increment"->1,"Default"->1}],
										"Reflect",
											Row@{Pane["reflect: ",Alignment->Right,ImageSize->50],
											Button["None",$dwShapeOptions[[Position[$dwShapeOptions,"Reflect"->_][[1,1]]]][[-1]]=False],
											Button["Horizontal",$dwShapeOptions[[Position[$dwShapeOptions,"Reflect"->_][[1,1]]]][[-1]]={0,1}],
											Button["Vertical",$dwShapeOptions[[Position[$dwShapeOptions,"Reflect"->_][[1,1]]]][[-1]]={1,0}]},
										"Rotate",
											dwOptionSlider[shape, {"Name"->"Rotate","SliderName"->"rotate ","Range"->{-180Degree,180Degree},"Increment"->1Degree,"Default"->0}],
										"Rotate3D",
											Grid[{
											{Row@{"dominant axis ",SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"RotateOrder"->_][[1,1]]]][[-1]],{"zxy"->"x","zyx"->"y"}],Spacer[20],
											Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"MoveBeforeRotate"->_][[1,1]]]][[-1]]]," rotate around origin",
											Spacer[{20,20}],Button[Style["reset",10],$dwShapeOptions[[Position[$dwShapeOptions,"Rotate3D"->_][[1,1]]]][[-1]]={0,0,0},Appearance->"Palette",ImageSize->40]}},
											{dwOptionSliderPart[shape, {"Name"->"Rotate3D","SliderName"->"rotate x ","Range"->{-180Degree,180Degree},"Increment"->1Degree,"Default"->0,"Part"->2}]},
											{dwOptionSliderPart[shape, {"Name"->"Rotate3D","SliderName"->"rotate y ","Range"->{-180Degree,180Degree},"Increment"->1Degree,"Default"->0,"Part"->1}]},
											{dwOptionSliderPart[shape, {"Name"->"Rotate3D","SliderName"->"rotate z ","Range"->{-180Degree,180Degree},"Increment"->1Degree,"Default"->0,"Part"->3}]},
											{""}},Spacings->{0,0},Background->GrayLevel[.8]],
										"Segment",
											Grid[{{dwOptionSliderPart[shape, {"Name"->"Segment","SliderName"->"start ","Range"->{-2Pi,2Pi},"Increment"->Pi/180,"Default"->0,"Part"->1,"ConvertRadiansToDegrees"->True}]},
											{dwOptionSliderPart[shape, {"Name"->"Segment","SliderName"->"end ","Range"->{-2Pi,2Pi},"Increment"->Pi/180,"Default"->0,"Part"->2,"ConvertRadiansToDegrees"->True}]}
											},Spacings->{0,0}],
										"Segments",
											dwOptionSlider[shape, {"Name"->"Segments","SliderName"->"segments ","Range"->{1,12},"Increment"->1,"Default"->3}],
										"ShowInteriorLinesAsBorder",
											Dynamic@If[$dwShapeOptions[[Position[$dwShapeOptions,"WhiteInteriorLines"->_][[1,1]]]][[-1]],
											Row@{Pane["click button\nto show line \nas border ",85,Alignment->Right],
											TogglerBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"ShowInteriorLinesAsBorder"->_][[1,1]]]][[-1]],Range[Min[2.5Length[$dwP[[$dwPn]]],100]],Appearance->"Horizontal"->{Automatic,10}]},
											Spacer[0]],
										"Size",
											dwOptionSlider[shape, {"Name"->"Size","SliderName"->"size ","Range"->{0,3},"Increment"->.01,"Default"->1}],
										"ShowSegments",
											If[$dwShowMouseClickPositions && $dwPrevious2ClickPtNoGrid =!= Null,
												Row@{"show segments ", TogglerBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"ShowSegments"->_][[1,1]]]][[-1]], Range[2]]},
												Nothing
											],
										"State",
											Row@{"state ",SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"State"->_][[1,1]]]][[-1]], Range[4]]},
										"Text",
											Column@{"","Enter unstyled text",InputField[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Text"->_][[1,1]]]][[-1]],Boxes,ImageSize->{280,150}]},
										"TickDivisions",
											Row[{
												"divisions:     h ",PopupMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"TickDivisions"->_][[1,1]]]][[-1,1]],Range[2,12]],
												"     v ",PopupMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"TickDivisions"->_][[1,1]]]][[-1,2]],Range[2,12]]
											}],
										"TickFirstLabelValue",
											Row@{
											"label value:     h ",PopupMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"TickFirstLabelValue"->_][[1,1]]]][[-1,1]],{.001,.01,.1,1,10,100,1000,10000}],
											"     v ",PopupMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"TickFirstLabelValue"->_][[1,1]]]][[-1,2]],{.001,.01,.1,1,10,100,1000,10000}]
											},
										"Value",
											dwOptionSlider[shape, {"Name"->"Value","SliderName"->"value ","Range"->{0,1},"Increment"->.01,"Default"->0}],
										"WhiteInteriorLines",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"WhiteInteriorLines"->_][[1,1]]]][[-1]]],"white interior lines (complex shapes may error if checked)"},
										"yPositive",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"yPositive"->_][[1,1]]]][[-1]]],"show y-axis in positive direction"},
										_,None
									],{opt,Keys[Options[shape]]}]/.None->Sequence[]]
							},True],Null]}},
						
						{{If[MemberQ[Options[shape],"MinorTickLength"|"Ticks"|"TickLabelPadding"|"TickLabels"|"TickLength"|"TickSide",Infinity],
							Dynamic@OpenerView[{"ticks",
								Column[Table[
									Switch[opt,
										"MinorTickLength",
											dwOptionSlider[shape, {"Name"->"MinorTickLength","SliderName"->"sub size ","Range"->{0,1},"Increment"->.025,"Default"->.0125}],
										"Ticks",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Ticks"->_][[1,1]]]][[-1]]],"ticks"},
										"TickLabels",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"TickLabels"->_][[1,1]]]][[-1]]],"tick labels"},
										"TickLabelPadding",
											Grid[{
												{""},
												{dwOptionSliderPart[shape, {"Name"->"TickLabelPadding","SliderName"->"pad h ","Range"->{0,.1},"Increment"->.005,"Default"->.015,"Part"->1}]},
												{dwOptionSliderPart[shape, {"Name"->"TickLabelPadding","SliderName"->"pad v ","Range"->{0,.1},"Increment"->.005,"Default"->.015,"Part"->2}]},
												{""}
											}, Spacings->{0,0},Background->GrayLevel[.8]],
										"TickLength",
											dwOptionSlider[shape, {"Name"->"TickLength","SliderName"->"tick size ","Range"->{0,1},"Increment"->.025,"Default"->.025}],
										"TickSide",
											Row@{"tick side ",SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"TickSide"->_][[1,1]]]][[-1]],{"Inside","Outside","Center"}]},
										_,None
									],{opt,Keys[Options[shape]]}]/.None->Sequence[]]
							}],Null]}},
						
						{{If[MemberQ[Options[shape],"Font"|"FontSize"|"LineSpacing"|"TextAlignment",Infinity],
							Dynamic@OpenerView[{"fonts",
								Column[Table[
									Switch[opt,
										"Font",
										Row@{"font ",PopupMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Font"->_][[1,1]]]][[-1]],{"Source Sans Pro","Times","Arial"}]},
										"FontSize",
										dwOptionSlider[shape, {"Name"->"FontSize","SliderName"->"size ","Range"->{6,72},"Increment"->1,"Default"->12}],
										"LineSpacing",
										dwOptionSlider[shape, {"Name"->"LineSpacing","SliderName"->"space ","Range"->{6,72},"Increment"->1,"Default"->12}],
										"TextAlignment",
										Row@{"align ",PopupMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"TextAlignment"->_][[1,1]]]][[-1]],{{0,0}->"Center",{-1,0}->"Left",{1,0}->"Right",{0,1}->"Top",{0,-1}->"Bottom",{-1,1}->"TopLeft",{1,1}->"TopRight",{-1,-1}->"BottomLeft",{1,-1}->"BottomRight"}]},
										_,None
									],{opt,Keys[Options[shape]]}]/.None->Sequence[]]
							}],Null]}},
						
						{{If[MemberQ[Options[shape],"BarColor"|"BaseColor"|"Color"|"ColorFill"|"EdgeColor"|"FlatShading"|"GridBackground"|"GridBackgroundColor"|"GridStyle",Infinity],
							Dynamic@OpenerView[{"color",
								Column[Table[
									Switch[opt,
										"BarColor",
											Row@{ColorSetter[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"BarColor"->_][[1,1]]]][[-1]]]," bar color"},
										"BaseColor",
											Row@{ColorSetter[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"BaseColor"->_][[1,1]]]][[-1]]]," base color"},
										"Color",
											Row@{ColorSetter[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"Color"->_][[1,1]]]][[-1]]]," color"},
										"ColorFill",
											Row@{ColorSetter[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"ColorFill"->_][[1,1]]]][[-1]]]," color fill"},
										"EdgeColor",
											Row@{ColorSetter[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"EdgeColor"->_][[1,1]]]][[-1]]]," line color"},
										"FlatShading",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"FlatShading"->_][[1,1]]]][[-1]]]," flat shading"},
										"GridBackground",
											Row@{Checkbox[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"GridBackground"->_][[1,1]]]][[-1]]],"grid background"},
										"GridBackgroundColor",
											Row@{ColorSetter[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"GridBackgroundColor"->_][[1,1]]]][[-1]]]," grid background color"},
										"GridStyle",
											Row@{ColorSetter[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"GridStyle"->_][[1,1]]]][[-1]]]," grid color"},
										_,None
									],{opt,Keys[Options[shape]]}]/.None->Sequence[]]
							}],Null]}},
						
						{{If[MemberQ[Options[shape],"ArrowsInside"|"ArrowsOutsideLength"|"ArrowPosition"|"ArrowSize"|"Offset"|"ReverseFlow",Infinity],
							Dynamic@OpenerView[{"arrows",
								Column[Table[
									Switch[opt,
										"ArrowsInside",
											Row@{"arrows inside ",SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"ArrowsInside"->_][[1,1]]]][[-1]],{True,False}]},
										"ArrowsOutsideLength",
											Grid[{{dwOptionSliderPart[shape, {"Name"->"ArrowsOutsideLength","SliderName"->"start ","Range"->{-2,2},"Increment"->$dwGridStep,"Default"->0,"Part"->1}]},
											{dwOptionSliderPart[shape, {"Name"->"ArrowsOutsideLength","SliderName"->"end ","Range"->{-2,2},"Increment"->$dwGridStep,"Default"->0,"Part"->2}]}
											},Spacings->{0,0}],
										"ArrowPosition",
											Row@{"arrow pos ",
												ActionMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"ArrowPosition"->_][[1,1]]]][[-1]],
												{
													"Both":>(pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]]&/@sel; $dwShapeOptions[[Position[$dwShapeOptions,"ArrowPosition"->_][[1,1]]]][[-1]] = "Both";
														($dwStyle[[#,4]] = 0; $dwStyle[[#,5]] = 1)&/@sel; (pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]][[1]]; $dwStyle[[#, pos,1]] = dwChangeArrow[If[$dwShapeOptions[[Position[$dwShapeOptions,"BreakWidthNumber"->_][[1,1]]]][[-1]] == 0, {{-$dwStyle[[#,3]],0},{$dwStyle[[#,3]],1}}, {{$dwStyle[[#,3]],1}}], $dwStyle[[#,3]], 0, 1, $dwStyle[[#,6]], $dwStyle[[#,12]]])&/@sel
													),
													"Start":>(pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]]&/@sel; $dwShapeOptions[[Position[$dwShapeOptions,"ArrowPosition"->_][[1,1]]]][[-1]] = "Start";
														($dwStyle[[#,4]] = 0; $dwStyle[[#,5]] = 1)&/@sel; (pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]][[1]]; $dwStyle[[#, pos,1]] = dwChangeArrow[{{-$dwStyle[[#,3]],0}}, $dwStyle[[#,3]], 0, 1, $dwStyle[[#,6]], $dwStyle[[#,12]]])&/@sel
													),
													"End":>(pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]]&/@sel; $dwShapeOptions[[Position[$dwShapeOptions,"ArrowPosition"->_][[1,1]]]][[-1]] = "End";
														($dwStyle[[#,4]] = 0; $dwStyle[[#,5]] = 1)&/@sel; (pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]][[1]]; $dwStyle[[#, pos,1]] = dwChangeArrow[{{$dwStyle[[#,3]],1}}, $dwStyle[[#,3]], 0, 1, $dwStyle[[#,6]], $dwStyle[[#,12]]])&/@sel
													),
													"None":>(pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]]&/@sel; $dwShapeOptions[[Position[$dwShapeOptions,"ArrowPosition"->_][[1,1]]]][[-1]] = "None";
														($dwStyle[[#,4]] = 0; $dwStyle[[#,5]] = 1)&/@sel; (pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]][[1]]; $dwStyle[[#, pos,1]] = dwChangeArrow[{}, $dwStyle[[#,3]], 0, 1, $dwStyle[[#,6]], $dwStyle[[#,12]]])&/@sel
													)
												}, Appearance->"PopupMenu"]
													
													},
										"ArrowSize",
											Row@{"arrow size ",
												ActionMenu[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"ArrowSize"->_][[1,1]]]][[-1]],
												{
													0:>(pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]]&/@sel; $dwShapeOptions[[Position[$dwShapeOptions,"ArrowSize"->_][[1,1]]]][[-1]] = 0;
														($dwStyle[[#,3]] = 0)&/@sel; (pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]][[1]]; $dwStyle[[#, pos,1]] = dwChangeArrow[$dwStyle[[#,pos,1]], 0, $dwStyle[[#,4]], $dwStyle[[#,5]], $dwStyle[[#,6]], $dwStyle[[#,12]]])&/@sel
													),
													"Small":>(pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]]&/@sel; $dwShapeOptions[[Position[$dwShapeOptions,"ArrowSize"->_][[1,1]]]][[-1]] = Small;
														($dwStyle[[#,3]] = Small)&/@sel; (pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]][[1]]; $dwStyle[[#, pos,1]] = dwChangeArrow[$dwStyle[[#,pos,1]], Small, $dwStyle[[#,4]], $dwStyle[[#,5]], $dwStyle[[#,6]], $dwStyle[[#,12]]])&/@sel
													),
													"Medium":>(pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]]&/@sel; $dwShapeOptions[[Position[$dwShapeOptions,"ArrowSize"->_][[1,1]]]][[-1]] = Medium;
														($dwStyle[[#,3]] = Medium)&/@sel; (pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]][[1]]; $dwStyle[[#, pos,1]] = dwChangeArrow[$dwStyle[[#,pos,1]], Medium, $dwStyle[[#,4]], $dwStyle[[#,5]], $dwStyle[[#,6]], $dwStyle[[#,12]]])&/@sel
													),
													"Large":>(pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]]&/@sel; $dwShapeOptions[[Position[$dwShapeOptions,"ArrowSize"->_][[1,1]]]][[-1]] = Large;
														($dwStyle[[#,3]] = Large)&/@sel; (pos = Flatten[Position[$dwStyle[[#]], Arrowheads[_]]][[1]]; $dwStyle[[#, pos,1]] = dwChangeArrow[$dwStyle[[#,pos,1]], Large, $dwStyle[[#,4]], $dwStyle[[#,5]], $dwStyle[[#,6]], $dwStyle[[#,12]]])&/@sel
													)
												}, Appearance->"PopupMenu"]
													
											},
										"Offset",
											dwOptionSlider[shape, {"Name"->"Offset","SliderName"->"offset ","Range"->{-1,1},"Increment"->$dwGridStep,"Default"->0}],
										"ReverseFlow",
											Row@{"reverse flow ",SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"ReverseFlow"->_][[1,1]]]][[-1]],{True,False}]},
										_,None
									],{opt,Keys[Options[shape]]}]/.None->Sequence[]]
							}],Null]}},
						
						{{If[MemberQ[Options[shape],"CoilHeight"|"EdgeOpacity"|"Gap"|"LineWeight"|"NumberOfCoils"|"Opacity"|"PointSize"|"PointsVisible"|"Resolution"|"ShowEnd"|"Speed"|"Thickness"|"WaveQuantity"|"Width",Infinity],
							Dynamic@OpenerView[{"other",
								Column[Table[
									Switch[opt,
										"CoilHeight",
											dwOptionSlider[shape, {"Name"->"CoilHeight","SliderName"->"coil hght ","Range"->{-2,2},"Increment"->.1,"Default"->1}],
										"EdgeOpacity",
											dwOptionSlider[shape, {"Name"->"EdgeOpacity","SliderName"->"line opac ","Range"->{0,1},"Increment"->.05,"Default"->1}],
										"Gap",
											dwOptionSlider[shape, {"Name"->"Gap","SliderName"->"gap ","Range"->{0,1},"Increment"->.05,"Default"->.4}],
										"LineWeight",
											dwOptionSlider[shape, {"Name"->"LineWeight","SliderName"->"line wght ","Range"->{.5,6},"Increment"->.25,"Default"->1}],
										"NumberOfCoils",
											dwOptionSlider[shape, {"Name"->"NumberOfCoils","SliderName"->"coils ","Range"->{1,12},"Increment"->1,"Default"->5}],
										"Opacity",
											dwOptionSlider[shape, {"Name"->"Opacity","SliderName"->"opacity ","Range"->{0,1},"Increment"->.05,"Default"->1}],
										"PointSize",
											dwOptionSlider[shape, {"Name"->"PointSize","SliderName"->"pt size ","Range"->{0,.2},"Increment"->.01,"Default"->.1}],
										"PointsVisible",
											Row@{"show points ",SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"PointsVisible"->_][[1,1]]]][[-1]],{True,False}]},
										"Resolution",
											dwOptionSlider[shape, {"Name"->"Resolution","SliderName"->"res ","Range"->{1,180},"Increment"->1,"Default"->20}],
										"ShowEnd",
											Row@{"end line",SetterBar[Dynamic@$dwShapeOptions[[Position[$dwShapeOptions,"ShowEnd"->_][[1,1]]]][[-1]],{"Both","Start","End","None"}]},
										"Speed",
											dwOptionSlider[shape, {"Name"->"Speed","SliderName"->"speed ","Range"->{0,20},"Increment"->1,"Default"->1}],
										"Thickness",
											dwOptionSlider[shape, {"Name"->"Thickness","SliderName"->"thickness ","Range"->{.1,12},"Increment"->.1,"Default"->1}],
										"WaveQuantity",
											dwOptionSlider[shape, {"Name"->"WaveQuantity","SliderName"->"waves ","Range"->{1,20},"Increment"->1,"Default"->5}],
										"Width",
											dwOptionSlider[shape, {"Name"->"Width","SliderName"->"width ","Range"->{0,1},"Increment"->.05,"Default"->1}],
										_,None
									],{opt,Keys[Options[shape]]}]/.None->Sequence[]]
							}],Null]}}]/.{Null}->Sequence[],Alignment->Left],
						ImageSize->350],
					Row[dwShapeDialogPresets[shape]],
					Row[{
						CancelButton[
							(* delete temp object *)
							sel = Switch[shape, dwAngleArrow, dwAngleArrowPartTotal[], dwDimensionArrow, dwDimensionArrowPartTotal[], _, {}];
							If[sel =!= {}, dwDeleteLayer[]];
							$dwShowMouseClickPositions = False;
							DialogReturn[]
						],
						DefaultButton[
							sel = Switch[shape, dwAngleArrow, dwAngleArrowPartTotal[], dwDimensionArrow, dwDimensionArrowPartTotal[], _, {}];
							(* save temp styles *)
							If[sel =!= {}, savedStyles = $dwStyle[[#]]&/@sel];
							(* delete temp object *)
							If[sel =!= {}, dwDeleteLayer[]];
							(* create new object *)
							If[MemberQ[{dwShapeCloud, dwShapeFire}, shape],
								
								(* image *)
								$dwStyleMode = "image";
								dwNewEmptyLayer["Form"->"image"];
								$dwStyle[[-1,2]] = shape[$dwShapeOptions];
								dwUpdateBoundingBox[{-1}],
								
								(* graphics *)
								Switch[shape,
									dwShapeTicks,
										dwGraphicsToWLDraw[Graphics[shape[$dwShapeOptions]]/.{StrokeForm->EdgeForm}, "Style"->{
											
											$dwDefaultStrokeStyle[[1,4]],
											$dwDefaultStrokeStyle[[1,3]],
											$dwDefaultPointStyle,
											$dwDefaultArrowStyle,
											$dwDefaultStrokeStyle[[1,5]],
											$dwStrayColor,
											Dashing[{}],
											StrokeForm[{
												$dwShapeOptions[[Position[$dwShapeOptions,"Color"->_][[1,1]]]][[-1]],
												Opacity[1],
												AbsoluteThickness[$dwShapeOptions[[Position[$dwShapeOptions,"LineWeight"->_][[1,1]]]][[-1]]],
												AbsoluteDashing[{}],
												CapForm["Butt"],
												JoinForm["Round"]
												}],
											$dwDefaultFaceStyle,
											$dwDefaultStrokeStyle[[1,6]],
											Opacity[1],
											PointSize[.01],
											Thickness[.01],
											0,
											TextAlignment->Center
										}],
									_,
										dwGraphicsToWLDraw[Graphics[shape[$dwShapeOptions]]/.{StrokeForm->EdgeForm}]
								]
							];
		
							(* set arrowhead styles to temp styles *)
							If[sel =!= {}, 
								pos = Flatten[Position[$dwStyle[[sel[[1]]]], _Arrowheads]][[1]];
								Do[$dwStyle[[sel[[n]], 3]] = savedStyles[[n, 3]], {n, Length[sel]}]; (* update arrowhead size variable *)
								Do[$dwStyle[[sel[[n]], 4]] = savedStyles[[n, 4]], {n, Length[sel]}]; (* update arrowhead start variable *)
								Do[$dwStyle[[sel[[n]], 5]] = savedStyles[[n, 5]], {n, Length[sel]}]; (* update arrowhead end variable *)
								(* update Arrowheads[] *)
								Do[$dwStyle[[sel[[n]], pos,1]] = 
									Switch[shape,
										dwDimensionArrow,
											If[$dwShapeOptions[[Position[$dwShapeOptions,"ArrowPosition"->_][[1,1]]]][[-1]] === "Start",
												{{$dwStyle[[sel[[n]], 3]], 1}},
												{{$dwStyle[[sel[[n]], 3]], 1}}
											],
										dwAngleArrow,
											If[$dwShapeOptions[[Position[$dwShapeOptions,"ArrowPosition"->_][[1,1]]]][[-1]] === "Both" && $dwShapeOptions[[Position[$dwShapeOptions,"BreakWidthNumber"->_][[1,1]]]][[-1]] == 0,
												{{-$dwStyle[[sel[[n]], 3]], 0},{$dwStyle[[sel[[n]], 3]], 1}},
												If[$dwShapeOptions[[Position[$dwShapeOptions,"ArrowPosition"->_][[1,1]]]][[-1]] === "Start",
													{{-$dwStyle[[sel[[n]], 3]], 0}},
													{{$dwStyle[[sel[[n]], 3]], 1}}
												]
											],
										_,
											savedStyles[[n, pos,1]]
									], {n, Length[sel]}],
								Nothing
							];
					
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							$dwShowMouseClickPositions = False;
							DialogReturn[], 
							Method->"Queued"
						]
					}]}
				,Alignment->Center],
			ImageSize->((((.75/$dwShapeStylePreviewDimensionsMagnify)*$dwShapeStylePreviewDimensions)/$dwShapeStylePreviewSize)[[1]]*$dwShapeStyleMouseSpeed)*($dwShapeStyleMouseSpeed*$dwOverallSize)]
		],
	Background->LightGray, WindowTitle->StringDrop[ToString[shape] ,17]<>" options",Modal->True]
										
dwAngleArrowPartTotal[]:=
	If[$dwShowMouseClickPositions && $dwPrevious2ClickPtNoGrid =!= Null,
		(* using mouse click position - check for 1 or 2 arrows *)
		If[$dwShapeOptions[[Position[$dwShapeOptions,"BreakWidthNumber"->_][[1,1]]]][[-1]] == 0,
			If[Length[$dwShapeOptions[[Position[$dwShapeOptions,"ShowSegments"->_][[1,1]]]][[-1]]] == 1, {-1}, {-1,-2}],
			If[Length[$dwShapeOptions[[Position[$dwShapeOptions,"ShowSegments"->_][[1,1]]]][[-1]]] == 1, {-1,-2}, {-1,-2,-3,-4}]
		],
		(* 1 arrow *)
		If[$dwShapeOptions[[Position[$dwShapeOptions,"BreakWidthNumber"->_][[1,1]]]][[-1]] == 0,
			{-1},
			{-1,-2}
		]
	]
	
dwDimensionArrowPartTotal[]:=
	If[$dwShapeOptions[[Position[$dwShapeOptions,"ShowEnd"->_][[1,1]]]][[-1]] === "Both",
		{-1,-2,-3,-4},
		If[$dwShapeOptions[[Position[$dwShapeOptions,"ShowEnd"->_][[1,1]]]][[-1]] === "None",
			{-1,-2},
			{-1,-2,-3}
		]
	]

End[] (* End Private Context *)

EndPackage[]