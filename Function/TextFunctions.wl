(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
	
dwReplaceText[sel_]:=
	CreateDialog[
		DynamicModule[{},
			Pane[
				Dynamic@Column[Flatten[{
					Style[InputField[Dynamic[($dwStyle[[sel, 2]])], ImageSize->{298, 298}], 18],
					
					Switch[$dwHead[[sel]],
						"Text3D",
							Button[Style["CONVERT TO REGULAR TEXT", 8],
								$dwStyle[[sel, 1]] = 0; $dwHead[[sel]] = Text; dwUpdateBoundingBox[{sel}],
								Appearance->"Palette", ImageSize->{300,30}
							],
						Text,
						
							{
								If[$dwStyle[[sel, 12]] === None,
									
									Button[Style["CONVERT TEXT TO GRAPHICS", 8],
										Block[{outlines, size, boundary, boundaryCenter, length = Length[$dwP], lengthTemp = Length[$dwP], selected = $dwSelected, finalSelection, color = {}, ctr = 1},
											dwSetUndo[];
											dwConvertPointSelectionToLayerSelection[];
											$dwSynchronousUpdating = False; (* improve speed for large quantity of graphics *)
											
											(* create outlines *)
											Do[
												If[$dwHead[[s]] === Text,
													outlines = First@ImportString[ExportString[Style[Sequence@@$dwStyle[[s]][[2;;11]]], "PDF"], "TextOutlines"->True];
													size = ImageDimensions[ImageCrop[Rasterize[Style[Sequence@@$dwStyle[[s]][[2;;11]]], ImageResolution->$dwImageResolution]]];
													dwGraphicsToWLDraw[Show[outlines, AspectRatio->Divide[Sequence@@Reverse[size]]], "ExtraScaling"->2size[[1]]/$dwOverallSize, "Group"->False, "SetUndo"->False];
													
													(* set position *)
													boundary = .5{EuclideanDistance[$dwBoundingBoxes[[s, 1]], $dwBoundingBoxes[[s, 2]]],.5EuclideanDistance[$dwBoundingBoxes[[s, 1]], $dwBoundingBoxes[[s, 2]]]};
													Do[
														$dwP[[sl]] = $dwP[[s,1]]+#-($dwStyle[[s,3]]*boundary)&/@$dwP[[sl]];
														AppendTo[color, $dwStyle[[s, Flatten[Position[$dwStyle[[s]], FontColor]][[1]]]][[2]]];,
													{sl, lengthTemp+1, Length[$dwP]}];
													lengthTemp = Length[$dwP],
													Nothing
												],
											{s, $dwSelected}];
											finalSelection = Range[length+1, Length[$dwP]];
											dwUpdateBoundingBox[finalSelection];
											
											(* set style *)
											If[Length[selected] == 1,
												
												(* compound path *)
												Do[
													$dwStyle[[s, Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]], 1, 1]] = color[[1]];
													$dwStyle[[s, Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]], 1, 2, 1]] = 0;
													$dwStyle[[s,8,1]] = "Solid";
													$dwStyle[[s,8,2,1,1]] = 0; 
													$dwStyle[[s,8,2,2,1]] = 1/3;
													$dwStyle[[s,8,2,3,1]] = 2/3;
													$dwStyle[[s,8,2,4,1]] = 1,
												{s, length+1, Length[$dwP]}],
												
												(* no compound path *)
												Do[
													$dwStyle[[s, Flatten[Position[$dwStyle[[s]], FaceForm[_]]][[1]], 1, 1]] = color[[ctr++]];
													$dwStyle[[s, Flatten[Position[$dwStyle[[s]], StrokeForm[_]]][[1]], 1, 2, 1]] = 0,
												{s, length+1, Length[$dwP]}]
											];
											
											(* set compound paths - do not want to create compound paths with multiple text boxes selected*)
											If[Length[selected] == 1,
												$dwCompoundPathLayers = If[Length[$dwP]-length > 1, Join[$dwCompoundPathLayers, {Range[length+1, Length[$dwP]]}], $dwCompoundPathLayers],
												Nothing
											];
											
											(* delete text *)
											$dwSelected = selected;
											dwDeleteLayer["SetUndo"->False];
											
											(* update variables *)
											$dwSelected = If[Length[selected] == 1 && Length[$dwP]-length > 1, $dwCompoundPathLayers[[-1]], finalSelection - Length[selected]];
											$dwGroupLayers = If[Length[selected] == 1, $dwGroupLayers, Join[$dwGroupLayers, {finalSelection - Length[selected]}]];
											$dwStyleMode = "fill";
											dwUpdateGradients[$dwSelected];
											$dwSynchronousUpdating = Automatic; (* return state *)
											DialogReturn[]
										],
									Appearance->"Palette", ImageSize->{300,30}(*, Method->"Queued"*)],
									
									{}
								],
								
								Row[{"BSplineCurve to follow: ", 
									EventHandler[
										PopupMenu[Dynamic@$dwStyle[[sel, 12]], 
											Join[{None,Delimiter}, 
												Table[
													If[$dwHead[[n]] === BSplineCurve && (Length[$dwP[[n]]] > 3 && FreeQ[$dwCompoundPathLayers, n]), 
														n->Graphics[{BSplineCurve[$dwP[[n]], SplineDegree->$dwStyle[[n,11]], SplineClosed->$dwStyle[[n,10]]]},ImageSize->{{50},{50}}], 
														Nothing
													], 
												{n, Length[$dwP]}]
											]
										],
									{"MouseClicked":>(dwUpdateBoundingBox[{sel}])}, PassEventsDown->True]}],
								Style["BSplineCurve must contain a minimum of 4 points.", Italic],
								If[$dwStyle[[sel, 12]] =!= None,
									Grid[{
										{Row[{Button["Flip text", 
												If[$dwStyle[[sel, 12]] =!= None,
													$dwP[[$dwStyle[[sel, 12]]]] = Reverse[$dwP[[$dwStyle[[sel, 12]]]]],
													Nothing
												]], Spacer[10], Checkbox[Dynamic@$dwTextOnCurveKerningOn], " Kerning"}]},
										{Row[{"space ", Slider[Dynamic@$dwStyle[[sel, 13]], {.1,4}]}]},
										(* Length mulitplier is twice the number of divisions in dwTextOnCurve[] *)
										{Row[{"start ", Slider[Dynamic@$dwStyle[[sel, 14]], {0, Max[40Length[Characters[$dwStyle[[sel, 2]]]],500]}]}]}
									}],
									{}
								]
							},
						_,
						
							{}
					],
					Row[{
						CancelButton[DialogReturn[]],
						DefaultButton["Close",dwUpdateBoundingBox[{sel}]; DialogReturn[]]
					}]
				}],Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Text", Modal->True]

dwText3DBox[n_]:=
	Block[{
			reduce = 2/$dwOverallSize, 
			size = ImageDimensions[ImageCrop[Rasterize[
				Graphics[dwAxoText[$dwStyle[[n,2]], $dwStyle[[n]][[Join[Range[4,11], {15}]]], "Center"->$dwP[[n,1]], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}],
				Background->If[ColorConvert[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FontColor]][[1]]]][[-1]], "Grayscale"][[1]] > .5, Black, White]], ImageResolution->$dwImageResolution]]]
		},
		{$dwP[[n,1]] - reduce size, $dwP[[n,1]] + reduce size}
	]

dwTextBox[n_]:=
	Block[{reduce = 4/$dwOverallSize, move, rot, order = 0, 
		size = Rasterize[Rotate[Text[Style[Sequence@@$dwStyle[[n]][[Join[Range[2,11], {15}]]]]], $dwStyle[[n,1]]],"RasterSize", ImageResolution->$dwImageResolution]},
		move = 
			Switch[$dwStyle[[n,3]],
				{-1, 0}, 
					{Cos[$dwStyle[[n,1]]], Sin[$dwStyle[[n,1]]]} + {Interpolation[{0, -.5, 0, .5, 0, -.5, 0, .5, 0}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order], Interpolation[{0, -.5, 0, .5, 0, -.5, 0, .5, 0}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order]} {Sin[$dwStyle[[n,1]]], Cos[$dwStyle[[n,1]]]},
				{1, 0},
					{-Cos[$dwStyle[[n,1]]], -Sin[$dwStyle[[n,1]]]} + {Interpolation[{0, .5, 0, -.5, 0, .5, 0, -.5, 0}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order], Interpolation[{0, .5, 0, -.5, 0, .5, 0, -.5, 0}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order]} {Sin[$dwStyle[[n,1]]], Cos[$dwStyle[[n,1]]]},
				{0, 1},
					{Sin[$dwStyle[[n,1]]], -Cos[$dwStyle[[n,1]]]},
				{0, -1},
					{-Sin[$dwStyle[[n,1]]], Cos[$dwStyle[[n,1]]]},
				{-1, -1},
					{Cos[$dwStyle[[n,1]]], Sin[$dwStyle[[n,1]]]} + {Interpolation[{0, -1.5, -1, -.5, 0, -1.5, -1, -.5, 0}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order], Interpolation[{1, .5, 0, 1.5, 1, .5, 0, 1.5, 1}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order]} {Sin[$dwStyle[[n,1]]], Cos[$dwStyle[[n,1]]]},
				{1, -1},
					{-Cos[$dwStyle[[n,1]]], -Sin[$dwStyle[[n,1]]]} + {Interpolation[{0, -.5, -1, -1.5, 0, -.5, -1, -1.5, 0}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order], Interpolation[{1, 1.5, 0, .5, 1, 1.5, 0, .5, 1}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order]} {Sin[$dwStyle[[n,1]]], Cos[$dwStyle[[n,1]]]},
				{1, 1},
					{-Cos[$dwStyle[[n,1]]], -Sin[$dwStyle[[n,1]]]} + {Interpolation[{0, 1.5, 1, .5, 0, 1.5, 1, .5, 0}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order], Interpolation[{-1, -.5, 0, -1.5, -1, -.5, 0, -1.5, -1}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order]} {Sin[$dwStyle[[n,1]]], Cos[$dwStyle[[n,1]]]},
				{-1, 1},
					{Cos[$dwStyle[[n,1]]], Sin[$dwStyle[[n,1]]]} + {Interpolation[{0, .5, 1, 1.5, 0, .5, 1, 1.5, 0}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order], Interpolation[{-1, -1.5, 0, -.5, -1, -1.5, 0, -.5, -1}, Abs[1 + Mod[$dwStyle[[n, 1]], 2 Pi]], InterpolationOrder->order]} {Sin[$dwStyle[[n,1]]], Cos[$dwStyle[[n,1]]]},
				_,
					{0, 0}
			];
			
			{
				reduce .5 size move + ($dwP[[n,1]] - reduce .5 size),
				reduce .5 size move + ($dwP[[n,1]] + reduce .5 size)
			}
	]

(*dwTextBox[n_]:=
	Block[{reduce = 4/$dwOverallSize, move,
		size = (Rasterize[Rotate[Text[Style[Sequence@@$dwStyle[[n]][[Join[Range[2,11], {15}]]]]], $dwStyle[[n,1]]],"RasterSize", ImageResolution->$dwImageResolution])},
		move = reduce .5 size {Cos[$dwStyle[[n, 1]]], Sin[$dwStyle[[n, 1]]]};
		Switch[$dwStyle[[n,3]],
			{-1,0},
				move + # &/@{$dwP[[n,1]] - reduce .5 size, $dwP[[n,1]] + reduce .5 size},
			{1,0},
				{-1,1}move + # &/@{$dwP[[n,1]] - reduce .5 size, $dwP[[n,1]] + reduce .5 size},
			{0,1},
				{$dwP[[n,1]] - reduce .5 size, $dwP[[n,1]] + reduce .5 size},
			{0,-1},
				{$dwP[[n,1]] - reduce .5 size, $dwP[[n,1]] + reduce .5 size},
			{-1,1},
				{$dwP[[n,1]] - reduce .5 size, $dwP[[n,1]] + reduce .5 size},
			{1,1},
				{$dwP[[n,1]] - reduce .5 size, $dwP[[n,1]] + reduce .5 size},
			{-1,-1},
				{$dwP[[n,1]] - reduce .5 size, $dwP[[n,1]] + reduce .5 size},
			{1,-1},
				{$dwP[[n,1]] - reduce .5 size, $dwP[[n,1]] + reduce .5 size},
			_,(* Center {0,0} *)
				{$dwP[[n,1]] - reduce .5 size, $dwP[[n,1]] + reduce .5 size}
		]
	]*)
	
Options[dwTextOnCurve] = {
		"KernCharacters"->{}, 
		"KerningByCharacter"->{},  
		"Position"->0,
		"Spacing"->1
	};
dwTextOnCurve[string_, curve_ , style_:{}, align_:0, OptionsPattern[]]:=
	Block[{chars=Characters[string], equalSpacePts, points, divisions=20, spaceTotal, space, kernChars, kernByChar},
		{space, kernChars, kernByChar} = {(1/divisions)+Min[Max[OptionValue["Spacing"],0],5], OptionValue["KernCharacters"], OptionValue["KerningByCharacter"]};
		spaceTotal=IntegerPart[OptionValue["Position"]];
		points=Cases[ParametricPlot[curve[t], {t,0,1}, Mesh->Max[(2divisions*Length[chars]),500], MeshFunctions->{"ArcLength"}, MaxRecursion->0, PlotPoints->Max[(2divisions*Length[chars]),500]],_GraphicsComplex|_Point,Infinity];
		equalSpacePts=points[[2,1]][[points[[1,1]]]];
		Table[
			If[n > 1 && MemberQ[kernChars, chars[[n-1]]],
				spaceTotal += Max[IntegerPart[kernByChar[[Flatten[Position[kernByChar, chars[[n-1]]]][[1]], 2]]divisions + (space-1)divisions],1],
				spaceTotal += Max[IntegerPart[divisions + (space-1)divisions],1]
			];
			If[spaceTotal < Length[equalSpacePts],
				With[{angle=ArcTan@@Subtract@@If[spaceTotal==0,{equalSpacePts[[spaceTotal+1]],equalSpacePts[[1]]},{equalSpacePts[[spaceTotal]],equalSpacePts[[spaceTotal-1]]}]},
					Rotate[Text[Style[chars[[n]], Sequence@@style], equalSpacePts[[spaceTotal]], {-1, (2/3)align}], angle, equalSpacePts[[spaceTotal]]]
				],
				Nothing
			],
		{n, 1, Length[chars]}]
	]

End[] (* End Private Context *)

EndPackage[]