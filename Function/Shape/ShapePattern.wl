(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeBarCode[OptionsPattern[]]:=
	CreateDialog[
		DynamicModule[{string = "enter text or numbers", barcodeForm = "QR", barcodeSize = 1},
			
			Column[{
				Dynamic[
					Show[
						Graphics[{
							(* axes and frame *)
							{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
							Inset[BarcodeImage[string, barcodeForm, IntegerPart[275*barcodeSize]], ImageScaled[{.5,.5}], ImageScaled[{.5,.5}], barcodeSize]
						},
						Background->White, ImageSize->{300,300}, PlotRange->1.1]
					]
				],
				"",
				Row@{InputField[Dynamic[string], String, ImageSize->{300,$dwStyleButtonHeight-1}]},
				Row@{"Form: ", PopupMenu[Dynamic[barcodeForm], {"Aztec", "QR"}], 
					Spacer[20], "Size: ", PopupMenu[Dynamic[barcodeSize], Range[.5, 2, .25]]},
				Row[{CancelButton[DialogReturn[]],
					DefaultButton[
						dwNewEmptyLayer["Head"->Image];
						$dwStyle[[-1]] = {0, BarcodeImage[string, barcodeForm, IntegerPart[275*barcodeSize]], 1, {{$dwImageFilterHead, $dwImageFilterValue, $dwImageFilterValueMin, $dwImageFilterValueMax, $dwImageFilterColor, $dwImageFilterExtraValue}}, False};
						$dwP[[-1]] = {{0,0}};
						dwUpdateBoundingBox[{-1}];
						DialogReturn[]
					]}]
			},Alignment->Center]
		], Background->LightGray, WindowTitle->"Barcode", Modal->True]

textDots[]:=
	CreateDialog[
		DynamicModule[{image, pointsize = 6, string = "string", detail = 5, dilate = .5, scale = 5, fontfamily = "Source Sans Pro", fontweight = Bold},
			
			Column[{
				Dynamic[Graphics[{
					image = ImageData@ColorConvert[Rasterize[Graphics[Text[Style[ToString[string], FontSize->72, FontFamily->fontfamily, FontColor->Black, FontWeight->fontweight]],ImageSize->{1000,200}], ImageResolution->$dwImageResolution], "Grayscale"];
					(* axes and frame *)
					{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
					Black, AbsolutePointSize[pointsize/2], 
					Point[Flatten[
						Table[
							If[image[[(Length@image) + 1 - a]][[b]] < dilate,
								scale({b, a}/$dwOverallSize - .5({Length@image[[1]], Length@image}/$dwOverallSize)), 
								Nothing],
						{a, 1, Length@image, 14-detail}, {b, 1, Length@image[[a]], 14-detail}], 
						1]
					]}, Background->White, ImageSize->{300,300}, PlotRange->1.5]],
				Style["Press alt/option key for slow slider.", Italic],
				"",
				Row@{InputField[Dynamic[string], String, ImageSize->{300,$dwStyleButtonHeight-1}]},
				Row@{PopupMenu[Dynamic[fontfamily], $dwFontFamilies], PopupMenu[Dynamic[fontweight], {Plain,Bold,Delimiter,"Thin","Light","Medium","SemiBold","Heavy","Black","Fat"}]},
				Row@{Pane["scale ",ImageSize->50, Alignment->Right], Dynamic@Slider[Dynamic@scale,{1,10,.1}], Spacer[5], Pane[Dynamic@scale, ImageSize->40, Alignment->Left]},
				Row@{Pane["point size ",ImageSize->50, Alignment->Right], Dynamic@Slider[Dynamic@pointsize,{1,20,1}], Spacer[5], Pane[Dynamic@pointsize, ImageSize->40, Alignment->Left]},
				Row@{Pane["detail ",ImageSize->50, Alignment->Right], Dynamic@Slider[Dynamic@detail,{1,10,1}], Spacer[5], Pane[Dynamic@detail, ImageSize->40, Alignment->Left]},
				Row@{Pane["dilate ", ImageSize->50, Alignment->Right], Dynamic@Slider[Dynamic@dilate,{.1,.9,.01}], Spacer[5], Pane[Dynamic@dilate, ImageSize->40, Alignment->Left]},
				Row[{CancelButton[DialogReturn[]],
					DefaultButton[
						dwNewEmptyLayer["Head"->Point];
						$dwStyle[[-1,7]] = pointsize;
						$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], AbsolutePointSize[_]]][[1]]]][[1]]=pointsize;
						$dwP[[-1]] = Flatten[
							Table[
								If[image[[(Length@image) + 1 - a]][[b]] < dilate,
									scale({b, a}/$dwOverallSize - .5({Length@image[[1]], Length@image}/$dwOverallSize)), 
									Nothing],
							{a, 1, Length@image, 14-detail}, {b, 1, Length@image[[a]], 14-detail}], 
							1];
						dwUpdateBoundingBox[{-1}];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						DialogReturn[]
					]}]
			},Alignment->Center]
		], Background->LightGray, WindowTitle->"Text dots", Modal->True]


dwSpiroCurve[]:=
	CreateDialog[
		DynamicModule[{angle = 140, quantity = 18, scale = 1, center, finalPts, finalPtsScale},
			Pane[
				Dynamic@Column[{
					finalPts = AnglePath[ConstantArray[angle \[Degree], quantity]];
					center = dwFindCenter[((1.25/Max[finalPts])finalPts)];
					finalPtsScale = #-center&/@((1.25/Max[finalPts])finalPts);
					Dynamic@Graphics[{
						(* axes and frame *)
						{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
						(* curve *)
						Gray,Line[scale*finalPtsScale]
						},Background->White,ImageSize->{300,300}],
						"",
					Style["Press alt/option key for slow slider.", Italic],
					"",
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->40,Alignment->Left]},
					Row@{Pane["angle ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@angle,{45,179,0.25}],Spacer[5],Pane[Dynamic@angle,ImageSize->40,Alignment->Left]},
					Row@{Pane["quantity ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@quantity,{3,360,1}],Spacer[5],Pane[Dynamic@quantity,ImageSize->40,Alignment->Left]},
					Grid[{{
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[120 \[Degree], 3]]]}, ImageSize->{27, 27}], angle = 120; quantity = 3, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[90 \[Degree], 4]]]}, ImageSize->{27, 27}], angle = 90; quantity = 4, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[144 \[Degree], 5]]]}, ImageSize->{27, 27}], angle = 144; quantity = 5, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[60 \[Degree], 6]]]}, ImageSize->{27, 27}], angle = 60; quantity = 6, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[360/3.5 \[Degree], 7]]]}, ImageSize->{27, 27}], angle = 360/3.5; quantity = 7, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[135 \[Degree], 8]]]}, ImageSize->{27, 27}], angle = 135; quantity = 8, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[80 \[Degree], 9]]]}, ImageSize->{27, 27}], angle = 80; quantity = 9, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[150 \[Degree], 12]]]}, ImageSize->{27, 27}], angle = 150; quantity = 12, $dwPresetButtonStyle]
						},{
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[84 \[Degree], 30]]]}, ImageSize->{27, 27}], angle = 84; quantity = 30, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[100 \[Degree], 18]]]}, ImageSize->{27, 27}], angle = 100; quantity = 18, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[110 \[Degree], 36]]]}, ImageSize->{27, 27}], angle = 110; quantity = 36, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[130 \[Degree], 36]]]}, ImageSize->{27, 27}], angle = 130; quantity = 36, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[140 \[Degree], 18]]]}, ImageSize->{27, 27}], angle = 140; quantity = 18, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[156 \[Degree], 30]]]}, ImageSize->{27, 27}], angle = 156; quantity = 30, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[152 \[Degree], 45]]]}, ImageSize->{27, 27}], angle = 152; quantity = 45, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[148 \[Degree], 90]]]}, ImageSize->{27, 27}], angle = 148; quantity = 90, $dwPresetButtonStyle]
						},{
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[121 \[Degree], 51]]]}, ImageSize->{27, 27}], angle = 121; quantity = 51, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[121.5 \[Degree], 39]]]}, ImageSize->{27, 27}], angle = 121.5; quantity = 39, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[89.5 \[Degree], 92]]]}, ImageSize->{27, 27}], angle = 89.5; quantity = 92, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[89 \[Degree], 40]]]}, ImageSize->{27, 27}], angle = 89; quantity = 40, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[71.5 \[Degree], 50]]]}, ImageSize->{27, 27}], angle = 71.5; quantity = 50, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[145 \[Degree], 35]]]}, ImageSize->{27, 27}], angle = 145; quantity = 35, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[60.5 \[Degree], 60]]]}, ImageSize->{27, 27}], angle = 60.5; quantity = 60, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[61 \[Degree], 30]]]}, ImageSize->{27, 27}], angle = 61; quantity = 30, $dwPresetButtonStyle]
						},{
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[102.5 \[Degree], 56]]]}, ImageSize->{27, 27}], angle = 102.5; quantity = 56, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[154 \[Degree], 70]]]}, ImageSize->{27, 27}], angle = 154; quantity = 70, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[135.5 \[Degree], 56]]]}, ImageSize->{27, 27}], angle = 135.5; quantity = 56, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[135.25 \[Degree], 80]]]}, ImageSize->{27, 27}], angle = 135.25; quantity = 80, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[80.5 \[Degree], 45]]]}, ImageSize->{27, 27}], angle = 80.5; quantity = 45, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[160.25 \[Degree], 81]]]}, ImageSize->{27, 27}], angle = 160.25; quantity = 81, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[130.75 \[Degree], 121]]]}, ImageSize->{27, 27}], angle = 130.75; quantity = 121, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.6], Line[AnglePath[ConstantArray[175 \[Degree], 72]]]}, ImageSize->{27, 27}], angle = 175; quantity = 72, $dwPresetButtonStyle]
					}},Spacings->{0,0}],
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add to canvas *)	
						dwNewEmptyLayer["Head"->Line];
						$dwP[[$dwSelected[[1]]]] = finalPtsScale;
						dwUpdateBoundingBox[$dwSelected[[{1}]]];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Spiro curve",Modal->True]

dwShapeConcentricCircles[]:=
	CreateDialog[
		DynamicModule[{scale = 1, quantity = 15, rotate = 3, steps = 50, thickness = 1, finalPts, finalPtsScale, color1 = Hue[.58], color2 = Black, length = Length[$dwP]},
			Pane[
				Dynamic@Column[{
					finalPts = Join[CirclePoints[{.1#,#},steps],{CirclePoints[{.1#,#},steps][[1]]}]&/@Table[n( rotate Pi/quantity),{n,quantity}];
					finalPtsScale = ((1/Max[finalPts])finalPts);
					Dynamic@Graphics[{
						(* axes and frame *)
						{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
						(* curve *)
						AbsoluteThickness[1+70(thickness/quantity)], CapForm["Round"], Line[scale*#, VertexColors->Table[Blend[{color2,color1},n],{n,0,1,1/steps}]]&/@finalPtsScale
						}, Background->color2, ImageSize->{300,300}],
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row@{Pane["quantity ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@quantity,{1,50,1}],Spacer[5],Pane[Dynamic@quantity,ImageSize->30,Alignment->Left]},
					Row@{Pane["rotate ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@rotate,{.5,2quantity,.5}],Spacer[5],Pane[Dynamic@rotate,ImageSize->30,Alignment->Left]},
					Row@{Pane["steps ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@steps,{4,50,2}],Spacer[5],Pane[Dynamic@steps,ImageSize->30,Alignment->Left]},
					Row@{Pane["thickness ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@thickness,{0,2,.05}],Spacer[5],Pane[Dynamic@thickness,ImageSize->30,Alignment->Left]},
					Row@{ColorSetter[Dynamic@color1], ColorSetter[Dynamic@color2]},
					Grid[{{
						Button[Graphics[{AbsoluteThickness[9(1/1)], CapForm["Round"], Line[.1*#, VertexColors->Table[Blend[{color2,color1},n],{n,0,1,1/50}]]&/@
							(Join[CirclePoints[{.1#,#},50],{CirclePoints[{.1#,#},50][[1]]}]&/@Table[n( .5 Pi/1),{n,1}])}, Background->color2, ImageSize->{27, 27}],
							quantity = 1; rotate = .5; steps = 50; thickness = 1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[9(1/6)], CapForm["Round"], Line[.1*#, VertexColors->Table[Blend[{color2,color1},n],{n,0,1,1/8}]]&/@
							(Join[CirclePoints[{.1#,#},8],{CirclePoints[{.1#,#},8][[1]]}]&/@Table[n( 1.5 Pi/6),{n,6}])}, Background->color2, ImageSize->{27, 27}],
							quantity = 6; rotate = 1.5; steps = 8; thickness = 1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[9(.75/5)], CapForm["Round"], Line[.1*#, VertexColors->Table[Blend[{color2,color1},n],{n,0,1,1/4}]]&/@
							(Join[CirclePoints[{.1#,#},4],{CirclePoints[{.1#,#},4][[1]]}]&/@Table[n( 2.5 Pi/5),{n,5}])}, Background->color2, ImageSize->{27, 27}],
							quantity = 5; rotate = 2.5; steps = 4; thickness = .75, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[9(1/15)], CapForm["Round"], Line[.1*#, VertexColors->Table[Blend[{color2,color1},n],{n,0,1,1/50}]]&/@
							(Join[CirclePoints[{.1#,#},50],{CirclePoints[{.1#,#},50][[1]]}]&/@Table[n( 3 Pi/15),{n,15}])}, Background->color2, ImageSize->{27, 27}],
							quantity = 15; rotate = 3; steps = 50; thickness = 1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[9(.75/15)], CapForm["Round"], Line[.1*#, VertexColors->Table[Blend[{color2,color1},n],{n,0,1,1/50}]]&/@
							(Join[CirclePoints[{.1#,#},50],{CirclePoints[{.1#,#},50][[1]]}]&/@Table[n( 25 Pi/15),{n,15}])}, Background->color2, ImageSize->{27, 27}],
							quantity = 15; rotate = 25; steps = 50; thickness = .75, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[9(.5/25)], CapForm["Round"], Line[.1*#, VertexColors->Table[Blend[{color2,color1},n],{n,0,1,1/50}]]&/@
							(Join[CirclePoints[{.1#,#},50],{CirclePoints[{.1#,#},50][[1]]}]&/@Table[n( 1 Pi/25),{n,25}])}, Background->color2, ImageSize->{27, 27}],
							quantity = 25; rotate = 1; steps = 50; thickness = .5, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[1], CapForm["Round"], Line[.1*#, VertexColors->Table[Blend[{color2,color1},n],{n,0,1,1/50}]]&/@
							(Join[CirclePoints[{.1#,#},50],{CirclePoints[{.1#,#},50][[1]]}]&/@Table[n( 19 Pi/30),{n,30}])}, Background->color2, ImageSize->{27, 27}],
							quantity = 30; rotate = 19; steps = 50; thickness = 0, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[1], CapForm["Round"], Line[.1*#, VertexColors->Table[Blend[{color2,color1},n],{n,0,1,1/10}]]&/@
							(Join[CirclePoints[{.1#,#},10],{CirclePoints[{.1#,#},10][[1]]}]&/@Table[n( 5 Pi/20),{n,20}])}, Background->color2, ImageSize->{27, 27}],
							quantity = 20; rotate = 5; steps = 10; thickness = 0, $dwPresetButtonStyle]
					}},Spacings->{0,0}],
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add background to canvas *)
						dwNewEmptyLayer["Head"->Polygon];
						$dwP[[-1]] = Max[(thickness/quantity^.25)1.6,1.1]scale{{-1,-1},{1,-1},{1,1},{-1,1}};
						$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], FaceForm[_]]][[1]],1,1]] = color2;
						$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]],1,1]] = color2;
						dwUpdateBoundingBox[{-1}];
						(* add lines to canvas *)
						Do[
							dwNewEmptyLayer["Head"->Line];
							$dwStyle[[-1]] = $dwFullDefaultStyle;
							$dwLineGradients[[-1,1]] = "2ColorLineGradient";
							$dwLineGradients[[-1,2,1,2]] = color2;
							$dwLineGradients[[-1,2,2,2]] = Blend[{color2,color1},1/3];
							$dwLineGradients[[-1,2,3,2]] = Blend[{color2,color1},2/3];
							$dwLineGradients[[-1,2,4,2]] = color1;
							$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]],1,3,1]] = 1+100(thickness/quantity);
							$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]],1,5,1]] = "Round";
							$dwP[[-1]] = scale*finalPtsScale[[n]];
							dwUpdateBoundingBox[{-1}],
						{n, Length[finalPtsScale]}];
						$dwGroupLayers = Join[$dwGroupLayers, {Range[length+2, Length[$dwP]]}];(* group lines not background *)
						$dwSelected = Range[length+1, Length[$dwP]];(* select lines and background *)
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Concentric circles",Modal->True]

dwShapeGraph[]:=
	CreateDialog[
		DynamicModule[{position, size, lines, pts, graph, layout = "RadialEmbedding", quantity = 7, scale = 1, length = Length[$dwP]},
			Pane[
				Dynamic@Column[{
					Switch[layout,
						"BipartiteEmbedding",
							size = 1;
							position = {0,0},
						"CircularEmbedding",
							size = 1;
							position = {0,0},
						_,
							size = 2;
							position = {-1,-1}
					];
					graph=Show[Graph[Table[j\[UndirectedEdge]FromDigits[Drop[IntegerDigits[j,2],-1],2],{j,1,quantity}],GraphLayout->layout]];
					pts = graph[[1,1]];
					lines = Flatten[#[[1]] & /@ Cases[graph, _Line|_Arrow, Infinity], 1];
					lines = position+#&/@((size/Max[pts])pts[[#]]&/@lines);
					pts = position+#&/@((size/Max[pts])pts);
					pts = If[layout === "LayeredEmbedding", Delete[pts,2], pts];
					Dynamic@Graphics[{
						(* axes and frame *)
						{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
						(* graph *)
						Gray, AbsolutePointSize[scale*6], Point[scale*pts], Line[scale*lines]
						},Background->White,ImageSize->{200,200}],
					Row@{Pane["layout ",ImageSize->50,Alignment->Right],PopupMenu[Dynamic@layout,{"BalloonEmbedding","BipartiteEmbedding","CircularEmbedding","LayeredEmbedding","RadialEmbedding","SpringElectricalEmbedding","SpringEmbedding"}],Spacer[5],Pane["",ImageSize->30,Alignment->Left]},
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row@{Pane["quantity ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@quantity,{3,127,1}],Spacer[5],Pane[Dynamic@quantity,ImageSize->30,Alignment->Left]},
					Grid[Partition[{
						Button[Graph[Table[j\[UndirectedEdge]FromDigits[Drop[IntegerDigits[j,2],-1],2],{j,1,23}],GraphLayout->"BalloonEmbedding",VertexStyle->Directive[StrokeForm[Gray],Gray],EdgeStyle->Gray, ImageSize->{27, 27}],
							quantity = 23; layout = "BalloonEmbedding", $dwPresetButtonStyle],
						Button[Graph[Table[j\[UndirectedEdge]FromDigits[Drop[IntegerDigits[j,2],-1],2],{j,1,9}],GraphLayout->"BipartiteEmbedding",VertexStyle->Directive[StrokeForm[Gray],Gray],EdgeStyle->Gray, ImageSize->{27, 27}],
							quantity = 9; layout = "BipartiteEmbedding", $dwPresetButtonStyle],
						Button[Graph[Table[j\[UndirectedEdge]FromDigits[Drop[IntegerDigits[j,2],-1],2],{j,1,27}],GraphLayout->"CircularEmbedding",VertexStyle->Directive[StrokeForm[Gray],Gray],EdgeStyle->Gray, ImageSize->{27, 27}],
							quantity = 27; layout = "CircularEmbedding", $dwPresetButtonStyle],
						Button[Graph[Table[j\[UndirectedEdge]FromDigits[Drop[IntegerDigits[j,2],-1],2],{j,1,31}],GraphLayout->"LayeredEmbedding",VertexStyle->Directive[StrokeForm[Gray],Gray],EdgeStyle->Gray, ImageSize->{27, 27}],
							quantity = 31; layout = "LayeredEmbedding", $dwPresetButtonStyle],
						Button[Graph[Table[j\[UndirectedEdge]FromDigits[Drop[IntegerDigits[j,2],-1],2],{j,1,63}],GraphLayout->"RadialEmbedding",VertexStyle->Directive[StrokeForm[Gray],Gray],EdgeStyle->Gray, ImageSize->{27, 27}],
							quantity = 63; layout = "RadialEmbedding", $dwPresetButtonStyle],
						Button[Graph[Table[j\[UndirectedEdge]FromDigits[Drop[IntegerDigits[j,2],-1],2],{j,1,31}],GraphLayout->"SpringElectricalEmbedding",VertexStyle->Directive[StrokeForm[Gray],Gray],EdgeStyle->Gray, ImageSize->{27, 27}],
							quantity = 31; layout = "SpringElectricalEmbedding", $dwPresetButtonStyle],
						Button[Graph[Table[j\[UndirectedEdge]FromDigits[Drop[IntegerDigits[j,2],-1],2],{j,1,35}],GraphLayout->"SpringEmbedding",VertexStyle->Directive[StrokeForm[Gray],Gray],EdgeStyle->Gray, ImageSize->{27, 27}],
							quantity = 35; layout = "SpringEmbedding", $dwPresetButtonStyle]
					},9,9,1,{}],Spacings->{0,0}],
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add lines to canvas *)	
						Do[
							dwNewEmptyLayer["Head"->Line];
							$dwStyle[[-1, Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]],1,1]] = Gray;(* color *)
							$dwStyle[[-1, Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]],1,3,1]] = 1;(* line thickness *)
							$dwP[[-1]] = scale*lines[[n]];
							dwUpdateBoundingBox[{-1}],
						{n, Length[lines]}];
						(* add vertex as single object to canvas *)
						dwNewEmptyLayer["Head"->Point];
						$dwStyle[[-1,13]] = Gray;
						$dwStyle[[-1,7]] = $dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[-1]], AbsolutePointSize[_]]][[1]],1]] = 8;(* point size *)
						$dwP[[-1]] = scale*pts;
						dwUpdateBoundingBox[{-1}];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						$dwGroupLayers = Join[$dwGroupLayers, {Range[length+1, Length[$dwP]-1]}];(* group lines only; vertex are a single object *)
						$dwSelected = Range[length+1, Length[$dwP]];(* select line group and vertex object *)
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Graph",Modal->True]

dwShapeHilbertCurve[]:=
	CreateDialog[
		DynamicModule[{step = 2, scale = 1, finalPts, finalPtsScale},
			Pane[
				Dynamic@Column[{
					finalPts = HilbertCurve[step][[1]];
					finalPtsScale = {-1,-1}+#&/@((2/Max[finalPts])finalPts);
					Dynamic@Graphics[{
						(* axes and frame *)
						{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
						(* curve *)
						Gray,Line[scale*finalPtsScale]
						},Background->White,ImageSize->{200,200}],
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row@{Pane["step ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@step,{1,6,1}],Spacer[5],Pane[Dynamic@step,ImageSize->30,Alignment->Left]},
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add to canvas *)	
						dwNewEmptyLayer["Head"->Line];
						$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
						$dwP[[$dwSelected[[1]]]] = finalPtsScale;
						dwUpdateBoundingBox[$dwSelected[[{1}]]];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Hilbert curve",Modal->True]

dwShapePeanoCurve[]:=
	CreateDialog[
		DynamicModule[{step = 2, scale = 1, finalPts, finalPtsScale},
			Pane[
				Dynamic@Column[{
					finalPts = PeanoCurve[step][[1]];
					finalPtsScale = {-1,-1}+#&/@((2/Max[finalPts])finalPts);
					Dynamic@Graphics[{
						(* axes and frame *)
						{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
						(* curve *)
						Gray,Line[scale*finalPtsScale]
						},Background->White,ImageSize->{200,200}],
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row@{Pane["step ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@step,{1,4,1}],Spacer[5],Pane[Dynamic@step,ImageSize->30,Alignment->Left]},
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add to canvas *)	
						dwNewEmptyLayer["Head"->Line];
						$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
						$dwP[[$dwSelected[[1]]]] = finalPtsScale;
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						dwUpdateBoundingBox[$dwSelected[[{1}]]];
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Peano curve",Modal->True]

dwShapeKochCurve[]:=
	CreateDialog[
		DynamicModule[{rot, p, step = 2, scale = 1, finalPts, finalPtsScale},
			Pane[
				Dynamic@Column[{
					rot = {RotationTransform[Pi, {1/2, 0}], RotationTransform[Pi/3, {0, 0}], RotationTransform[-Pi/3, {1, 0}]};
					p = DeleteDuplicates[Flatten[Table[rot[[n]][KochCurve[step][[1]]], {n, 3}], 1]];
					finalPts = Join[p, p[[{1}]]];
					finalPtsScale = {-1,-1/Sqrt[3]}+#&/@((2/Max[finalPts])finalPts);
					Dynamic@Graphics[{
						(* axes and frame *)
						{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
						(* curve *)
						Gray,Line[scale*finalPtsScale]
						},Background->White,ImageSize->{200,200}],
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row@{Pane["step ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@step,{1,4,1}],Spacer[5],Pane[Dynamic@step,ImageSize->30,Alignment->Left]},
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add to canvas *)	
						dwNewEmptyLayer["Head"->Line];
						$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
						$dwP[[$dwSelected[[1]]]] = finalPtsScale;
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						dwUpdateBoundingBox[$dwSelected[[{1}]]];
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Koch curve",Modal->True]

dwShapeSierpinskiCurve[]:=
	CreateDialog[
		DynamicModule[{step = 2, scale = 1, finalPts, finalPtsScale},
			Pane[
				Dynamic@Column[{
					finalPts = SierpinskiCurve[step][[1]];
					finalPtsScale = {-1,1}+#&/@((2/Max[finalPts])finalPts);
					Dynamic@Graphics[{
						(* axes and frame *)
						{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
						(* curve *)
						Gray,Line[scale*finalPtsScale]
						},Background->White,ImageSize->{200,200}],
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row@{Pane["step ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@step,{1,5,1}],Spacer[5],Pane[Dynamic@step,ImageSize->30,Alignment->Left]},
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add to canvas *)	
						dwNewEmptyLayer["Head"->Line];
						$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
						$dwP[[$dwSelected[[1]]]] = finalPtsScale;
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						dwUpdateBoundingBox[$dwSelected[[{1}]]];
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Sierpinski curve",Modal->True]

dwShapeTreeCurve[]:=
	CreateDialog[
		DynamicModule[{tree, finalThickness, scale = 1, branches = 6, width = 1, height = .8, color = Gray, taper = .04, thickness = .65, finalPts, finalPtsScale, length = Length[$dwP]},
			Pane[
				Dynamic@Column[{
					tree = dwShapeMakeTree[branches, width, taper, thickness, height];
					finalPts = Flatten[#[[2]][[1]]&/@tree,1];
					finalPtsScale = (1/Max[finalPts])finalPts;
					finalThickness = Flatten[Table[100*#[[1]][[1]], Length[#[[2]][[1]]]]&/@tree];
					Dynamic@Graphics[{
						(* axes and frame *)
						{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
						(* tree *)
						color, Table[{AbsoluteThickness[finalThickness[[n]]],Line[scale*finalPtsScale[[n]]]}, {n, Length[finalPtsScale]}]
						},Background->White,ImageSize->{200,200}],
					Row@{ColorSetter[Dynamic@color]},
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row@{Pane["height ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@height,{1.3,.8,-.01}],Spacer[5],Pane[Dynamic@height,ImageSize->30,Alignment->Left]},
					Row@{Pane["width ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@width,{1.2,.75,-.01}],Spacer[5],Pane[Dynamic@width,ImageSize->30,Alignment->Left]},
					Row@{Pane["taper ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@taper,{.03,.15,.01}],Spacer[5],Pane[Dynamic@taper,ImageSize->30,Alignment->Left]},
					Row@{Pane["thickness ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@thickness,{.6,.75,.01}],Spacer[5],Pane[Dynamic@thickness,ImageSize->30,Alignment->Left]},
					Row@{Pane["branches ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@branches,{3,8,1}],Spacer[5],Pane[Dynamic@branches,ImageSize->30,Alignment->Left]},
					Grid[Partition[{
						Button[Graphics[Table[Line[n], {n, Flatten[#[[2]][[1]]&/@dwShapeMakeTree[5, 1, 0.04, 0.65, 0.8],1]}], ImageSize->{27, 27}],
							branches = 5; width = 1; taper = 0.04; thickness = 0.65; height = 0.8, $dwPresetButtonStyle],
						Button[Graphics[Table[Line[n], {n, Flatten[#[[2]][[1]]&/@dwShapeMakeTree[5, 1, 0.04, 0.65, 0.9],1]}], ImageSize->{27, 27}],
							branches = 5; width = 1; taper = 0.04; thickness = 0.65; height = 0.9, $dwPresetButtonStyle],
						Button[Graphics[Table[Line[n], {n, Flatten[#[[2]][[1]]&/@dwShapeMakeTree[5, 0.9, 0.04, 0.65, 1.05],1]}], ImageSize->{27, 27}],
							branches = 5; width = 0.9; taper = 0.04; thickness = 0.65; height = 1.05, $dwPresetButtonStyle],
						Button[Graphics[Table[Line[n], {n, Flatten[#[[2]][[1]]&/@dwShapeMakeTree[5, 0.75, 0.04, 0.65, 1.2],1]}], ImageSize->{27, 27}],
							branches = 5; width = 0.75; taper = 0.04; thickness = 0.65; height = 1.2, $dwPresetButtonStyle],
						Button[Graphics[Table[Line[n], {n, Flatten[#[[2]][[1]]&/@dwShapeMakeTree[6, 0.75, 0.04, 0.65, 1.3],1]}], ImageSize->{27, 27}],
							branches = 6; width = 0.75; taper = 0.04; thickness = 0.65; height = 1.3, $dwPresetButtonStyle],
						Button[Graphics[Table[Line[n], {n, Flatten[#[[2]][[1]]&/@dwShapeMakeTree[7, 0.75, 0.04, 0.65, 1.3],1]}], ImageSize->{27, 27}],
							branches = 7; width = 0.75; taper = 0.04; thickness = 0.65; height = 1.3, $dwPresetButtonStyle]
					},9,9,1,{}],Spacings->{0,0}],
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add to canvas *)	
						Do[
							dwNewEmptyLayer["Head"->Line];
							$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
							$dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]],1,1]] = color;
							$dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]],1,3,1]] = finalThickness[[n]];
							$dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]],1,5,1]] = "Square";
							$dwP[[$dwSelected[[1]]]] = scale*finalPtsScale[[n]];
							dwUpdateBoundingBox[$dwSelected[[{1}]]],
						{n, Length[finalPtsScale]}];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						$dwGroupLayers = Join[$dwGroupLayers, {Range[length+1, Length[$dwP]]}];
						$dwSelected = $dwGroupLayers[[-1]];
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Geometric Tree",Modal->True]
	
dwShapeMakeTree[branches_:6, width_:1, taper_:.04, thickness_:.65, height_:.8]:=
	Block[{m1={Reverse[{-.3,.8}],{-1,width} {-.3,.8}},m2={Reverse[{.3,.8}],{-1,width} {.3,.8}}},
		MapIndexed[{Thickness[taper* thickness^#2[[1]]],Line[#]}&,NestList[Flatten[Map[{{#[[2]],#[[2]]+height*m1.(#[[2]]-#[[1]])},{#[[2]],#[[2]]+height*m2.(#[[2]]-#[[1]])}}&,#],1]&,{{{0,-1},{0,0}}},branches]]
	]

End[] (* End Private Context *)

EndPackage[]