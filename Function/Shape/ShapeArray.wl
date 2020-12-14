(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *)

dwShapeArray[]:=
	CreateDialog[
		DynamicModule[{
			originalLength = Length[$dwP],
			originalSelectionLength,
			originalSelection,
			previewSize=350,
			ctr=0,
			ctr2=0,
			srn=0,
			prn=0,
			columns=3,
			rows=3,
			head,
			angle=0,
			rotate,
			spaceH=.2,
			spaceV=.2,
			offsetH=0,
			offsetV=0,
			offsetHList={},
			offsetVList={},
			origin={0,0},
			clickPtScaled,
			plotrange=1.5,
			plotrangeList={0,0},
			fontMinSize=12,
			fontIncreaseSize=12,
			randomMinSize=1,
			randomQuantity="All",
			heightScale=0,
			widthScale=0,
			selectedShapeUsed=False,textSize,style,shape,shapeStart,shapeMirror,shapeMirrorStart,scaleEach=1,scaleAll=1,
			fill,closeLine=True,mirrorVertical=False,mirrorHorizontal=False,randomColor=False,randomRotate=False,
			randomRot,color=97,colorNum=1,strokeOpacity,fillOpacity,thickness,pointSize,scaleRandom,posRandom,shapeCtr,
			combineShapes=True,changeStyle=False,temp, temp1, oldPos, newPos
			},
			dwConvertPointSelectionToLayerSelection[];
			$dwMode="move";
			$dwSelected = 
				If[$dwSelected =!= {},
					Table[If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], n, Nothing], {n, $dwSelected}],
					{}
				];
			$dwShapeArraySeedAmount = 0;
			originalSelection = Sort[$dwSelected];
			originalSelectionLength = Length[$dwSelected];
			selectedShapeUsed = If[$dwSelected =!= {} && ($dwP[[$dwSelected[[1]]]] =!= {} && MemberQ[$dwShapeSymbols,$dwHead[[$dwSelected[[1]]]]]), True, False];
			thickness = 
				If[$dwSelected =!= {},
					$dwStyle[[#,Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]]]][[1,3,1]]&/@$dwSelected,
					{1}
				];
			pointSize = 
				If[$dwSelected =!= {}, 
					$dwStyle[[#,Flatten[Position[$dwStyle[[$dwSelected[[1]]]], AbsolutePointSize[_]]][[1]]]][[1]]&/@$dwSelected, 
					{4}
				];
			strokeOpacity = 
				If[$dwSelected =!= {},
					$dwStyle[[#, Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]],1,2,1]]&/@$dwSelected,
					{1}
				];
			fillOpacity = 
				If[$dwSelected =!= {},
					$dwStyle[[#, Flatten[Position[$dwStyle[[#]], FaceForm[_]]][[1]],1,2,1]]&/@$dwSelected,
					{1}
				];
			head = 
				If[$dwSelected =!= {},
					$dwHead[[$dwSelected]],
					{Polygon}
				];
			fill = 
				If[$dwSelected =!= {},
					$dwStyle[[#,1]]&/@$dwSelected,
					{True}
				];
			style = If[$dwSelected =!= {}, $dwStyle[[$dwSelected]], {$dwFullDefaultStyle}];
			shapeStart = 
				If[$dwSelected =!= {} && $dwP[[$dwSelected[[1]]]] =!= {},
					$dwP[[$dwSelected]],
					{.1{{-1,-1},{1,-1},{1,1},{-1,1}}}
				];
			rotate = RotationTransform[angle];
			Pane[Dynamic@Column[{
				
				
				(* <<<<<<<<<< TOP >>>>>>>>>> *)
				Panel[
					Row[{Style["Use custom objects by selecting them before opening this tool.", 15, FontSlant->Italic], Spacer[30], "Columns: ", InputField[Dynamic@columns, Number, ImageSize->{30,15}], Spacer[30], "Rows: ", InputField[Dynamic@rows, Number, ImageSize->{30,15}]}],
					Alignment->Center, ImageSize->710],
				
				
				(* <<<<<<<<<< OBJECT >>>>>>>>>> *)
				Panel[
					Column[{
						Style["OBJECT", 15],
						Row[{
							ActionMenu[Dynamic@If[head[[1]] === Text, 
								
									"Random text", 

									Graphics[
										Table[{
											Sequence@@(style[[n, $dwStyleStart;;-1]]/.{AbsolutePointSize[___]->AbsolutePointSize[2]}),
											If[style[[n, 1]] && MemberQ[{BezierCurve, BSplineCurve}, head[[n]]], FilledCurve, Sequence][
												head[[n]][
													Switch[head[[n]],
														BezierCurve, 
															Most[shapeStart[[n]]],
														Line,
															If[closeLine, Join[shapeStart[[n]], {shapeStart[[n, 1]]}], shapeStart[[n]]],
														_, 
															shapeStart[[n]]
													], 
													If[head[[n]] === BSplineCurve,
														SplineClosed->If[closeLine, True, False],
														Null
													]
												]
											]}, 
										{n, Length[shapeStart]}]/.{Null->Sequence[]}, 
									ImageSize->{{24},{24}}]
									
								], {
								(* selected object or default box *)
								If[$dwSelected =!= {},
									
									Graphics[
										Table[{
											Sequence@@($dwStyle[[n, $dwStyleStart;;-1]]/.{AbsolutePointSize[___]->AbsolutePointSize[2]}), 
											If[$dwStyle[[n, 1]] && MemberQ[{BezierCurve, BSplineCurve}, $dwHead[[n]]], FilledCurve, Sequence][
												$dwHead[[n]][
													Switch[$dwHead[[n]],
														BezierCurve,
															Most[$dwP[[n]]],
														Line,
															If[closeLine, Join[$dwP[[n]], {$dwP[[n, 1]]}], $dwP[[n]]],
														_,
															$dwP[[n]]
													], 
													If[$dwHead[[n]] === BSplineCurve,
														SplineClosed->If[closeLine,True,False],
														Null
													]
												]
											]}, 
										{n, $dwSelected}]/.{Null->Sequence[]}, 
									ImageSize->{{24},{24}}],
										
									Graphics[{GrayLevel[.7], EdgeForm[Black], Polygon[{{-1,-1}, {1,-1}, {1,1}, {-1,1}, {-1,-1}}]}, ImageSize->{{24},{24}}]
									
								]:>(
									If[$dwSelected =!= {},
										
										style = $dwStyle[[$dwSelected]]; head = $dwHead[[$dwSelected]]; 
										fill = $dwStyle[[#,1]]&/@$dwSelected; shapeStart = $dwP[[$dwSelected]]; 
										thickness = $dwStyle[[#,Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]]]][[1,3,1]]&/@$dwSelected;
										pointSize = $dwStyle[[#,Flatten[Position[$dwStyle[[$dwSelected[[1]]]], AbsolutePointSize[_]]][[1]]]][[1]]&/@$dwSelected;
										strokeOpacity = $dwStyle[[#, Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]],1,2,1]]&/@$dwSelected;
										mirrorHorizontal = mirrorVertical = False,
										
										style[[1,1]] = True; head[[1]] = Polygon; shapeStart = {.1{{-1,-1}, {1,-1}, {1,1}, {-1,1}}}
									]
								),
								
								Delimiter, (* basic objects *)
								
								(* triangle *)
								Graphics[Line[.5{{0.17320508075688773, -0.1}, {0, 0.2}, {-0.17320508075688773, -0.1}, {0.17320508075688773, -0.1}}], ImageSize->{{24},{24}}]:>(
									style = {$dwFullDefaultStyle}; style[[1,1]] = False; fill[[1]] = False; thickness = {1}; pointSize = {4}; strokeOpacity = {1}; head[[1]] = Line; closeLine = True; shapeStart = {.5{{0.17320508075688773, -0.1}, {0, 0.2}, {-0.17320508075688773, -0.1}}}
								),
								(* box *)
								Graphics[Line[{{-1,-1}, {1,-1}, {1,1}, {-1,1}, {-1,-1}}], ImageSize->{{24},{24}}]:>(
									style = {$dwFullDefaultStyle}; style[[1,1]] = False; head[[1]] = Line; thickness = {1}; pointSize = {4}; strokeOpacity = {1}; closeLine = True; shapeStart = {.1{{-1,-1}, {1,-1}, {1,1}, {-1,1}}}
								),
								(* hexagon *)
								Graphics[Line[.5{{0.1,-0.17320508075688773}, {0.2,0}, {0.1,0.17320508075688773}, {-0.1,0.17320508075688773}, {-0.2,0}, {-0.1,-0.17320508075688773}, {0.1,-0.17320508075688773}}], ImageSize->{{24},{24}}]:>(
									style = {$dwFullDefaultStyle}; style[[1,1]] = False; fill[[1]] = False; thickness = {1}; pointSize = {4}; strokeOpacity = {1}; head[[1]] = Line; closeLine = True; shapeStart = {.5{{0.1,-0.17320508075688773}, {0.2,0}, {0.1,0.17320508075688773}, {-0.1,0.17320508075688773}, {-0.2,0}, {-0.1,-0.17320508075688773}}}
								),
								(* octogon *)
								Graphics[Line[.5{{0.07653668647301796, -0.18477590650225736}, {0.18477590650225736, -0.07653668647301796}, {0.18477590650225736, 0.07653668647301796}, {0.07653668647301796, 0.18477590650225736}, {-0.07653668647301796, 0.18477590650225736}, {-0.18477590650225736, 0.07653668647301796}, {-0.18477590650225736, -0.07653668647301796}, {-0.07653668647301796, -0.18477590650225736}, {0.07653668647301796, -0.18477590650225736}}], ImageSize->{{24},{24}}]:>(
									style = {$dwFullDefaultStyle}; style[[1,1]] = False; fill[[1]] = False; thickness = {1}; pointSize = {4}; strokeOpacity = {1}; head[[1]] = Line; closeLine = True; shapeStart = {.5{{0.07653668647301796, -0.18477590650225736}, {0.18477590650225736, -0.07653668647301796}, {0.18477590650225736, 0.07653668647301796}, {0.07653668647301796, 0.18477590650225736}, {-0.07653668647301796, 0.18477590650225736}, {-0.18477590650225736, 0.07653668647301796}, {-0.18477590650225736, -0.07653668647301796}, {-0.07653668647301796, -0.18477590650225736}}}
								),
								(* circle *)
								Graphics[BezierCurve[.5{{0.,0.2},{0.11,0.2},{0.2,0.11},{0.2,0.},{0.2,-0.11},{0.11,-0.2},{0.,-0.2},{-0.11,-0.2},{-0.2,-0.11},{-0.2,0.},{-0.2,0.11},{-0.11,0.2},{0.,0.2}}], ImageSize->{{24},{24}}]:>(
									style = {$dwFullDefaultStyle}; style[[1,1]] = False; fill[[1]] = False; thickness = {1}; pointSize = {4}; strokeOpacity = {1}; head[[1]] = BezierCurve; shapeStart = {.5{{0.,0.2},{0.11,0.2},{0.2,0.11},{0.2,0.},{0.2,-0.11},{0.11,-0.2},{0.,-0.2},{-0.11,-0.2},{-0.2,-0.11},{-0.2,0.},{-0.2,0.11},{-0.11,0.2},{0.,0.2},{0.11,0.2}}}
								),
								Delimiter, (* partial objects designed to be mirrored *)
								(* partial arc *)
								Framed[Graphics[BezierCurve[{{0, 0.1}, {0, 0.045}, {-0.045, 0}, {-0.1, 0}, {-0.1, 0}}], ImageSize->{{24},{24}}, PlotRange->.1], FrameMargins->0, FrameStyle->LightGray]:>(
									style = {$dwFullDefaultStyle}; style[[1,1]] = False; fill[[1]] = False; thickness = {1}; pointSize = {4}; strokeOpacity = {1}; head[[1]] = BezierCurve; shapeStart = {{{0, 0.1}, {0, 0.045}, {-0.045, 0}, {-0.1, 0}, {-0.1, 0}}}
								),
								(* partial diagonal line *)
								Framed[Graphics[Line[{{-0.1, 0.}, {0., 0.1}}], ImageSize->{{24},{24}}, PlotRange->.1], FrameMargins->0, FrameStyle->LightGray]:>(
									style = {$dwFullDefaultStyle}; style[[1,1]] = False; fill[[1]] = False; thickness = {1}; pointSize = {4}; strokeOpacity = {1}; head[[1]] = Line; shapeStart = {{{-0.1, 0.}, {0., 0.1}}}
								),
								Delimiter,
								(* text *)
								"Random text":>(
									style = {$dwFullDefaultStyle}; style[[1,1]] = False; fill[[1]] = False; head[[1]] = Text; shapeStart = {{{0, 0}}}
								)
							}, Appearance->"PopupMenu", ImageSize->120],
							
							Sequence@@Flatten[{If[head[[1]] === Text,
								
								Nothing,
								
								{
									Spacer[20], Checkbox[Dynamic@mirrorVertical], " Mirror vertical", Spacer[20], Checkbox[Dynamic@mirrorHorizontal], " Mirror horizontal", Spacer[20], 
									PopupMenu[Dynamic@randomQuantity, {"All",2->"1/2",3->"2/3",4->"3/4"}],Grid[{{Slider[(Dynamic@randomMinSize), {1,.1,-.1}, ImageSize->Tiny], " Random size"},{Slider[(Dynamic@$dwShapeArraySeedAmount), {0,1,.01}, ImageSize->Tiny], " Random pos"}}], Spacer[20], PopupMenu[Dynamic@$dwShapeArraySeed, Range[15]], " Seed"
								}]
							}]
						}]
				}, Alignment->Center], Alignment->Center, ImageSize->710],
				
				
				(* <<<<<<<<<< PREVIEW AND POSITION >>>>>>>>>> *)
				Panel[
					Grid[{
						{Column[{
							EventHandler[MouseAppearance[
								shapeMirrorStart = Which[
									mirrorVertical && !mirrorHorizontal,
										Table[{-1,1}#&/@pts, {pts, shapeStart}],
									!mirrorVertical && mirrorHorizontal,
										Table[{1,-1}#&/@pts, {pts, shapeStart}],
									mirrorVertical && mirrorHorizontal,
										Table[{-1,-1}#&/@pts, {pts, shapeStart}],
									True,
										{}
								];
								rotate = RotationTransform[angle*Pi/180];
								colorNum=1;
								Dynamic[Show[
									
									offsetHList=ReplacePart[Table[0,{rows}],Table[n->offsetH,{n,1,rows,2}]];
									offsetVList=ReplacePart[Table[0,{columns}],Table[n->offsetV,{n,1,columns,2}]];
							
									Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]],
									
									Graphics[Flatten@{
									(* axes and frame *)
									{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
									
									(* pattern values *)
									ctr = ctr2 = srn = prn = shapeCtr = 0;
									textSize = Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[Range[fontMinSize,fontMinSize+Max[fontIncreaseSize,1],1/Length[$dwShapeArrayCharacters]], Length[$dwShapeArrayCharacters]]];
									scaleRandom = Evaluate[SeedRandom[$dwShapeArraySeed]; 
										Switch[randomQuantity,
											2,
												Flatten[Table[{1, randomMinSize}, rows*columns]],
											3,
												Flatten[Table[{1, randomMinSize, randomMinSize^2}, rows*columns]],
											4,
												Flatten[Table[{1, randomMinSize, randomMinSize^1.5, randomMinSize^2.5}, rows*columns]],
											_,
												RandomReal[{randomMinSize,1}, 2rows*columns]]
										];
									posRandom = Evaluate[SeedRandom[$dwShapeArraySeed]; 
										Switch[randomQuantity,
											2,
												Flatten[Table[{0, 1}, rows*columns]],
											3,
												Flatten[Table[{0, 1, 1}, rows*columns]],
											4,
												Flatten[Table[{0, 1, 1, 1}, rows*columns]],
											_,
												Table[1, 2rows*columns]]
										];
										
									(* render columns and rows *)
									
									Table[
										If[head[[1]] === Text,
											(* text *)
											If[++ctr > Length[$dwShapeArrayConnectors], ctr = 1];
											If[++ctr2 > Length[$dwShapeArrayCharacters], ctr2 = 1];
											Text[Rotate[Style[$dwShapeArrayCharacters[[ctr2]], 
												FontSize->Round[If[EvenQ[x+y], RotateRight[textSize,x+y], RotateLeft[textSize,x+y]][[ctr2]]], 
												FontFamily->$dwShapeArrayFontFamily,
												If[randomColor, Evaluate@ColorData[color,y+(x-1)*rows], Black]], 
												If[randomRotate, $dwShapeArrayConnectors[[ctr]]*(Pi/2), 0], ({(x - 1) spaceH, -(y - 1) spaceV} + {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2)], 
												scaleAll({(x - 1) spaceH, -(y - 1) spaceV} + {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2), Evaluate[SeedRandom[$dwShapeArraySeed+ctr2]; $dwShapeArraySeedAmount*RandomReal[{-1,1},2]]
											],
												
											(* shape *)
											{
												++prn;
												++srn;
												If[++ctr>Length[$dwShapeArrayConnectors], ctr = 1];
												Sequence@@Table[{
													If[++shapeCtr > Length[shapeStart], shapeCtr = 1];
													If[randomRotate, 
														randomRot = 
															If[shapeMirrorStart === {},
																RotationTransform[$dwShapeArrayConnectors[[ctr]]*(Pi), Mean[DeleteDuplicates[shapeStart[[shapeCtr]]]]],
																RotationTransform[$dwShapeArrayConnectors[[ctr]]*(Pi), Mean[DeleteDuplicates[Join[shapeStart[[shapeCtr]],shapeMirrorStart[[shapeCtr]]]]]]
															], 
														{}
													];
													(* mirror shape *)
													If[randomRotate, 
														
														shape = randomRot[shapeStart[[shapeCtr]]]; 
														If[shapeMirrorStart =!= {},
															shapeMirror = randomRot[shapeMirrorStart[[shapeCtr]]],
															Nothing
														],
														
														shape = shapeStart[[shapeCtr]];
														If[shapeMirrorStart =!= {},
															shapeMirror=shapeMirrorStart[[shapeCtr]],
															Nothing
														]
													];
													If[changeStyle,
														
														If[randomColor,
															{
																AbsolutePointSize[.6*pointSize[[1]]],
																Evaluate@ColorData[color,y+(x-1)*rows],
																Opacity[strokeOpacity[[1]]],
																StrokeForm[{Evaluate@ColorData[color,y+(x-1)*rows], Opacity[strokeOpacity[[1]]]}],
																FaceForm[{Evaluate@ColorData[color,y+(x-1)*rows], Opacity[fillOpacity[[1]]]}]
															},{
																AbsolutePointSize[.6*pointSize[[1]]],
																$dwFullDefaultStyle[[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]][[1]],1,1]], (* point color *)
																StrokeForm[{
																	$dwFullDefaultStyle[[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]][[1]],1,1]],
																	Opacity[strokeOpacity[[1]]]
																}], 
																FaceForm[{
																	$dwFullDefaultStyle[[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]][[1]],1,1]],
																	Opacity[fillOpacity[[1]]]
																}], 
																If[head[[1]] === Point, Opacity[strokeOpacity[[1]]], {}]
															}
														],
														
														style[[sn, $dwStyleStart;;-1]]/.{AbsolutePointSize[aps_]:>AbsolutePointSize[.6*aps]}
													],
													
													If[MemberQ[{BezierCurve,BSplineCurve},head[[sn]]] && fill[[sn]], FilledCurve,Sequence][head[[sn]][scaleAll ({(x - 1) spaceH, -(y - 1) spaceV} + # - {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2 + Evaluate[SeedRandom[$dwShapeArraySeed+ctr]; posRandom[[prn]]*$dwShapeArraySeedAmount*RandomReal[{-1,1},2]])&/@((1-Abs[heightScale]*(If[heightScale<0, rows-(y-1), y]/rows))*(1-Abs[widthScale]*(If[widthScale<0, columns-(x-1), x]/columns))*scaleRandom[[srn]]*scaleEach*rotate[Switch[head[[sn]],Line,If[closeLine&&Length[shape]>2,Join[shape,shape[[{1}]]],shape],BezierCurve,Most[shape],_,shape]]),If[head[[sn]]===BSplineCurve,SplineClosed->If[closeLine,True,False],Null]]/.{Null->Sequence[]}],
													
													If[shapeMirrorStart =!= {},
														If[MemberQ[{BezierCurve,BSplineCurve},head[[sn]]] && fill[[sn]], FilledCurve,Sequence][head[[sn]][scaleAll ({(x - 1) spaceH, -(y - 1) spaceV} + # - {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2 + Evaluate[SeedRandom[$dwShapeArraySeed+ctr]; posRandom[[prn]]*$dwShapeArraySeedAmount*RandomReal[{-1,1},2]])&/@((1-Abs[heightScale]*(If[heightScale<0, rows-(y-1), y]/rows))*(1-Abs[widthScale]*(If[widthScale<0, columns-(x-1), x]/columns))*scaleRandom[[srn]]*scaleEach*rotate[Switch[head[[sn]],Line,If[closeLine&&Length[shapeMirror]>2,Join[shapeMirror,shapeMirror[[{1}]]],shapeMirror],BezierCurve,Most[shapeMirror],_,shapeMirror]]),If[head[[sn]]===BSplineCurve,SplineClosed->If[closeLine,True,False],Null]]/.{Null->Sequence[]}],
														{}
													]
													
												}, {sn, If[combineShapes, Length[shapeStart], 1]}]
											}
										],{x,columns},{y,rows}]
								}],
								
								Background->White,
								ImageSize->{previewSize,If[head[[1]] === Text, .75, 1]*previewSize},
								PlotRange->{1, If[head[[1]] === Text, .75, 1]}{-plotrange plotrangeList[[1]]+{-plotrange,plotrange},-plotrange plotrangeList[[2]]+{-plotrange,plotrange}}
							
							], TrackedSymbols :> {$dwShapeArrayCharacters,$dwShapeArrayConnectors,$dwShapeArrayFontFamily,$dwShapeArraySeed,$dwShapeArraySeedAmount,angle,closeLine,color,columns,fillOpacity,fontIncreaseSize,fontMinSize,head,heightScale,shapeMirrorStart,
									offsetH,offsetHList,offsetV,offsetVList,plotrangeList,pointSize,posRandom,randomColor,randomMinSize,randomRotate,randomQuantity,rotate,rows,scaleAll,scaleEach,scaleRandom,shapeStart,spaceH,spaceV,strokeOpacity,textSize,widthScale,combineShapes,changeStyle}(*, 
									SynchronousUpdating->False*)],
							(* cursor *)
							$dwCursorCanvas],
							(* mouse events *)
							"MouseUp":>(origin=plotrangeList),
							"MouseDown":>(clickPtScaled=MousePosition["GraphicsScaled"]),
							"MouseDragged":>(If[MousePosition["Graphics"]=!=None,
							plotrangeList=origin+2(MousePosition["GraphicsScaled"]-clickPtScaled)])
						],
						"",
						(* text controls *)
						If[head[[1]] === Text,
							Grid[{
								{Row[{
									PopupMenu[Dynamic@$dwShapeArrayFontFamily, $dwFontFamilies],
									ActionMenu[Dynamic@Switch[$dwShapeArrayCharacters,
										Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[CharacterRange["A", "Z"], 26]],
											"upper case",
										Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[Join[CharacterRange["a", "z"], CharacterRange["A", "Z"]], 52]],
											"mixed case",
										Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[Range[0, 9], 10]],
											"0 to 9",
										Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[Range[0, 99], 100]],
											"0 to 99",
										_,
											"lower case"
										],{
											"lower case":>($dwShapeArrayCharacters = Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[CharacterRange["a", "z"], 26]]),
											"upper case":>($dwShapeArrayCharacters = Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[CharacterRange["A", "Z"], 26]]),
											"mixed case":>($dwShapeArrayCharacters = Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[Join[CharacterRange["a", "z"], CharacterRange["A", "Z"]], 52]]),
											"?":>($dwShapeArrayCharacters = Table["?", 20]),
											"0 and 1":>($dwShapeArrayCharacters = Evaluate[SeedRandom[$dwShapeArraySeed]; RandomInteger[1, 20]]),
											"0 to 9":>($dwShapeArrayCharacters = Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[Range[0, 9], 10]]),
											"0 to 99":>($dwShapeArrayCharacters = Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[Range[0, 99], 100]])
										}, Appearance->"PopupMenu"],
									"   seed", PopupMenu[Dynamic@$dwShapeArraySeed, Range[15]]
									}]},
								{Row@{Pane["font size ",60,Alignment->Right],
									Slider[Dynamic@fontMinSize,{1,100,1}],
									Button["<",fontMinSize-=1,Appearance->"Palette"],
									Button[">",fontMinSize+=1,Appearance->"Palette"],
									" ",Pane[Dynamic@fontMinSize,50]}},
								{Row@{Pane["variation ",60,Alignment->Right],
									Slider[Dynamic@fontIncreaseSize,{1,100,1}],
									Button["<",fontIncreaseSize-=1,Appearance->"Palette"],
									Button[">",fontIncreaseSize+=1,Appearance->"Palette"],
									" ",Pane[Dynamic@fontIncreaseSize,50]}},
								{Row@{Pane["position ",60,Alignment->Right],
									Slider[Dynamic@$dwShapeArraySeedAmount,{0,2,.1}],
									Button["<",$dwShapeArraySeedAmount-=.1,Appearance->"Palette"],
									Button[">",$dwShapeArraySeedAmount+=.1,Appearance->"Palette"],
									" ",Pane[Dynamic@$dwShapeArraySeedAmount,50]}}
							}],
							""
						]
					}, Alignment->Center],
					Grid[{
						{Row@{
								If[Length[shapeStart] == 1,
									PopupMenu[Dynamic@head[[1]], Which[head[[1]] === Text, {Text}, Length[shape] < 5,{Text, Line, Arrow, Polygon, BSplineCurve, Point}, True, $dwShapeSymbols]],
									Nothing
								],
								Sequence@@If[MemberQ[{Line,BSplineCurve},head[[1]]],{"  ",Checkbox[Dynamic@closeLine]," close"},{}],
								Sequence@@If[Length[shapeStart]>1, {"  ",Checkbox[Dynamic@combineShapes]," combine"},{}]
							}},
						{Row@{Pane["scale all ",60,Alignment->Right],
							Slider[Dynamic@scaleAll,{.1,2,.05}],
							Button["<",scaleAll=Round[scaleAll-=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							Button[">",scaleAll=Round[scaleAll+=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							" ",Pane[Dynamic@scaleAll,50]}},
						{Row@{Pane["scale each ",60,Alignment->Right],
							Slider[Dynamic@scaleEach,{.1,4,.05}],
							Button["<",scaleEach=Round[scaleEach-=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							Button[">",scaleEach=Round[scaleEach+=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							" ",Pane[Dynamic@scaleEach,50]}},
						{Row@{Pane["rotate each ",60,Alignment->Right],
							Slider[Dynamic@angle,{0,360,1}],
							Button["<",angle=Max[angle-1, 0],Appearance->"Palette"],
							Button[">",angle=Min[angle+1, 360],Appearance->"Palette"],
							" ",Pane[Dynamic@angle,50]}},
						{Row@{Pane["space h ",60,Alignment->Right],
							Slider[Dynamic@spaceH,{-1,1,$dwGridStep}],
							Button["<",spaceH=Round[spaceH-=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							Button[">",spaceH=Round[spaceH+=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							" ",Pane[Dynamic@spaceH,50]}},
						{Row@{Pane["space v ",60,Alignment->Right],
							Slider[Dynamic@spaceV,{-1,1,$dwGridStep}],
							Button["<",spaceV=Round[spaceV-=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							Button[">",spaceV=Round[spaceV+=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							" ",Pane[Dynamic@spaceV,50]}},
						{Row@{Pane["offset h ",60,Alignment->Right],
							Slider[Dynamic@offsetH,{-1,1,$dwGridStep}],
							Button["<",offsetH=Round[offsetH-=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							Button[">",offsetH=Round[offsetH+=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							" ",Pane[Dynamic@offsetH,50]}},
						{Row@{Pane["offset v ",60,Alignment->Right],
							Slider[Dynamic@offsetV,{-1,1,$dwGridStep}],
							Button["<",offsetV=Round[offsetV-=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							Button[">",offsetV=Round[offsetV+=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							" ",Pane[Dynamic@offsetV,50]}},
						{Row@{Pane["ramp h ",60,Alignment->Right],
							Slider[Dynamic@widthScale,{-1,1,$dwGridStep}],
							Button["<",widthScale=Round[widthScale-=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							Button[">",widthScale=Round[widthScale+=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							" ",Pane[Dynamic@widthScale,50]}},
						{Row@{Pane["ramp v ",60,Alignment->Right],
							Slider[Dynamic@heightScale,{-1,1,$dwGridStep}],
							Button["<",heightScale=Round[heightScale-=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							Button[">",heightScale=Round[heightScale+=$dwGridStep,$dwGridStep],Appearance->"Palette"],
							" ",Pane[Dynamic@heightScale,50]}},
						{Row[{"slider increment: ",
							ActionMenu[Dynamic@If[$dwGridStep===$dwGridStepNone,"None",$dwGridStep],{
								"None":>($dwGridStep=$dwGridStepNone),
								$dwGridStepVeryTiny:>($dwGridStep=$dwGridStepVeryTiny),
								$dwGridStepTiny:>($dwGridStep=$dwGridStepTiny),
								$dwGridStepSmall:>($dwGridStep=$dwGridStepSmall),
								$dwGridStepMedium:>($dwGridStep=$dwGridStepMedium),
								$dwGridStepLarge:>($dwGridStep=$dwGridStepLarge)
							},Appearance->"PopupMenu"]}]}
					}]}
				}, Alignment->Center, Spacings->{0,0}](*]*), Alignment->Center, ImageSize->710],
				
				
				(* <<<<<<<<<< PRESETS >>>>>>>>>> *)
				Panel[
					Grid[{
						{"POSITION PRESETS","POSITIONED SHAPE PRESETS"},{
						
						(* position  presets *)
						Row[{
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Polygon[{{-0.5,0.3},{-0.3,0.3},{-0.3,0.5},{-0.5,0.5}}],Polygon[{{-0.5,-0.1},{-0.3,-0.1},{-0.3,0.1},{-0.5,0.1}}],Polygon[{{-0.5,-0.5},{-0.3,-0.5},{-0.3,-0.3},{-0.5,-0.3}}],Polygon[{{-0.1,0.3},{0.1,0.3},{0.1,0.5},{-0.1,0.5}}],Polygon[{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}}],Polygon[{{-0.1,-0.5},{0.1,-0.5},{0.1,-0.3},{-0.1,-0.3}}],Polygon[{{0.3,0.3},{0.5,0.3},{0.5,0.5},{0.3,0.5}}],Polygon[{{0.3,-0.1},{0.5,-0.1},{0.5,0.1},{0.3,0.1}}],Polygon[{{0.3,-0.5},{0.5,-0.5},{0.5,-0.3},{0.3,-0.3}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .4; spaceV = .4; offsetH = 0; offsetV = 0; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Polygon[{{-0.3,0.1},{-0.1,0.1},{-0.1,0.3},{-0.3,0.3}}],Polygon[{{-0.3,-0.1},{-0.1,-0.1},{-0.1,0.1},{-0.3,0.1}}],Polygon[{{-0.3,-0.3},{-0.1,-0.3},{-0.1,-0.1},{-0.3,-0.1}}],Polygon[{{-0.1,0.1},{0.1,0.1},{0.1,0.3},{-0.1,0.3}}],Polygon[{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}}],Polygon[{{-0.1,-0.3},{0.1,-0.3},{0.1,-0.1},{-0.1,-0.1}}],Polygon[{{0.1,0.1},{0.3,0.1},{0.3,0.3},{0.1,0.3}}],Polygon[{{0.1,-0.1},{0.3,-0.1},{0.3,0.1},{0.1,0.1}}],Polygon[{{0.1,-0.3},{0.3,-0.3},{0.3,-0.1},{0.1,-0.1}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .2; spaceV = .2; offsetH = 0; offsetV = 0; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Polygon[{{-0.7,0.3},{-0.5,0.3},{-0.5,0.5},{-0.7,0.5}}],Polygon[{{-0.5,-0.1},{-0.3,-0.1},{-0.3,0.1},{-0.5,0.1}}],Polygon[{{-0.7,-0.5},{-0.5,-0.5},{-0.5,-0.3},{-0.7,-0.3}}],Polygon[{{-0.3,0.3},{-0.1,0.3},{-0.1,0.5},{-0.3,0.5}}],Polygon[{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}}],Polygon[{{-0.3,-0.5},{-0.1,-0.5},{-0.1,-0.3},{-0.3,-0.3}}],Polygon[{{0.1,0.3},{0.3,0.3},{0.3,0.5},{0.1,0.5}}],Polygon[{{0.3,-0.1},{0.5,-0.1},{0.5,0.1},{0.3,0.1}}],Polygon[{{0.1,-0.5},{0.3,-0.5},{0.3,-0.3},{0.1,-0.3}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .4; spaceV = .4; offsetH = .2; offsetV = 0; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Polygon[{{-0.4,0.1},{-0.2,0.1},{-0.2,0.3},{-0.4,0.3}}],Polygon[{{-0.3,-0.1},{-0.1,-0.1},{-0.1,0.1},{-0.3,0.1}}],Polygon[{{-0.4,-0.3},{-0.2,-0.3},{-0.2,-0.1},{-0.4,-0.1}}],Polygon[{{-0.2,0.1},{0.,0.1},{0.,0.3},{-0.2,0.3}}],Polygon[{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}}],Polygon[{{-0.2,-0.3},{0.,-0.3},{0.,-0.1},{-0.2,-0.1}}],Polygon[{{0.,0.1},{0.2,0.1},{0.2,0.3},{0.,0.3}}],Polygon[{{0.1,-0.1},{0.3,-0.1},{0.3,0.1},{0.1,0.1}}],Polygon[{{0.,-0.3},{0.2,-0.3},{0.2,-0.1},{0.,-0.1}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .2; spaceV = .2; offsetH = .1; offsetV = 0; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Polygon[{{-0.7,0.1},{-0.5,0.1},{-0.5,0.3},{-0.7,0.3}}],Polygon[{{-0.5,-0.1},{-0.3,-0.1},{-0.3,0.1},{-0.5,0.1}}],Polygon[{{-0.7,-0.3},{-0.5,-0.3},{-0.5,-0.1},{-0.7,-0.1}}],Polygon[{{-0.3,0.1},{-0.1,0.1},{-0.1,0.3},{-0.3,0.3}}],Polygon[{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}}],Polygon[{{-0.3,-0.3},{-0.1,-0.3},{-0.1,-0.1},{-0.3,-0.1}}],Polygon[{{0.1,0.1},{0.3,0.1},{0.3,0.3},{0.1,0.3}}],Polygon[{{0.3,-0.1},{0.5,-0.1},{0.5,0.1},{0.3,0.1}}],Polygon[{{0.1,-0.3},{0.3,-0.3},{0.3,-0.1},{0.1,-0.1}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .4; spaceV = .2; offsetH = .2; offsetV = 0; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Polygon[{{-0.7,0.},{-0.5,0.},{-0.5,0.2},{-0.7,0.2}}],Polygon[{{-0.5,-0.1},{-0.3,-0.1},{-0.3,0.1},{-0.5,0.1}}],Polygon[{{-0.7,-0.2},{-0.5,-0.2},{-0.5,0.},{-0.7,0.}}],Polygon[{{-0.3,0.},{-0.1,0.},{-0.1,0.2},{-0.3,0.2}}],Polygon[{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}}],Polygon[{{-0.3,-0.2},{-0.1,-0.2},{-0.1,0.},{-0.3,0.}}],Polygon[{{0.1,0.},{0.3,0.},{0.3,0.2},{0.1,0.2}}],Polygon[{{0.3,-0.1},{0.5,-0.1},{0.5,0.1},{0.3,0.1}}],Polygon[{{0.1,-0.2},{0.3,-0.2},{0.3,0.},{0.1,0.}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .4; spaceV = .1; offsetH = .2; offsetV = 0; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Polygon[{{-0.2,0.5},{0.,0.5},{0.,0.7},{-0.2,0.7}}],Polygon[{{-0.2,0.1},{0.,0.1},{0.,0.3},{-0.2,0.3}}],Polygon[{{-0.2,-0.3},{0.,-0.3},{0.,-0.1},{-0.2,-0.1}}],Polygon[{{-0.1,0.3},{0.1,0.3},{0.1,0.5},{-0.1,0.5}}],Polygon[{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}}],Polygon[{{-0.1,-0.5},{0.1,-0.5},{0.1,-0.3},{-0.1,-0.3}}],Polygon[{{0.,0.5},{0.2,0.5},{0.2,0.7},{0.,0.7}}],Polygon[{{0.,0.1},{0.2,0.1},{0.2,0.3},{0.,0.3}}],Polygon[{{0.,-0.3},{0.2,-0.3},{0.2,-0.1},{0.,-0.1}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .1; spaceV = .4; offsetH = 0; offsetV = .2; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Polygon[{{-0.7,0.5},{-0.5,0.5},{-0.5,0.7},{-0.7,0.7}}],Polygon[{{-0.3,0.1},{-0.1,0.1},{-0.1,0.3},{-0.3,0.3}}],Polygon[{{-0.7,-0.3},{-0.5,-0.3},{-0.5,-0.1},{-0.7,-0.1}}],Polygon[{{-0.5,0.3},{-0.3,0.3},{-0.3,0.5},{-0.5,0.5}}],Polygon[{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}}],Polygon[{{-0.5,-0.5},{-0.3,-0.5},{-0.3,-0.3},{-0.5,-0.3}}],Polygon[{{-0.3,0.5},{-0.1,0.5},{-0.1,0.7},{-0.3,0.7}}],Polygon[{{0.1,0.1},{0.3,0.1},{0.3,0.3},{0.1,0.3}}],Polygon[{{-0.3,-0.3},{-0.1,-0.3},{-0.1,-0.1},{-0.3,-0.1}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .2; spaceV = .4; offsetH = .4; offsetV = .2; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Polygon[{{-0.7,0.5},{-0.5,0.5},{-0.5,0.7},{-0.7,0.7}}],Polygon[{{-0.5,0.3},{-0.3,0.3},{-0.3,0.5},{-0.5,0.5}}],Polygon[{{-0.7,0.1},{-0.5,0.1},{-0.5,0.3},{-0.7,0.3}}],Polygon[{{-0.3,0.1},{-0.1,0.1},{-0.1,0.3},{-0.3,0.3}}],Polygon[{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}}],Polygon[{{-0.3,-0.3},{-0.1,-0.3},{-0.1,-0.1},{-0.3,-0.1}}],Polygon[{{0.1,0.5},{0.3,0.5},{0.3,0.7},{0.1,0.7}}],Polygon[{{0.3,0.3},{0.5,0.3},{0.5,0.5},{0.3,0.5}}],Polygon[{{0.1,0.1},{0.3,0.1},{0.3,0.3},{0.1,0.3}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .4; spaceV = .2; offsetH = .2; offsetV = .4; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle],
							Button[Graphics[{GrayLevel[.8],EdgeForm[Black],Table[Polygon[p],{p,Table[n+#&/@{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}},{n,{{-.2,.2},{.2,.2},{0,0},{-.2,-.2},{.2,-.2}}}]}],Table[Polygon[p],{p,Table[n+.5#&/@{{-0.1,-0.1},{0.1,-0.1},{0.1,0.1},{-0.1,0.1}},{n,{{-.2,0},{0,.2},{0,-.2},{.2,0}}}]}]}, ImageSize->{27, 27}], 
								randomQuantity = 2; randomMinSize = .5; randomRotate = False; spaceH = .2; spaceV = .2; offsetH = 0; offsetV = 0; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; mirrorVertical = False; mirrorHorizontal = False, $dwPresetButtonStyle]
						}],
						
						(* shape presets *)
						Row[{Spacer[30],
							Button[Graphics[{BezierCurve[{{-0.2,0.3},{-0.2,0.245},{-0.245,0.2},{-0.3,0.2}}],BezierCurve[{{-0.2,0.1},{-0.2,0.155},{-0.155,0.2},{-0.1,0.2}}],BezierCurve[{{-0.3,0.},{-0.245,0.},{-0.2,-0.045},{-0.2,-0.1}}],BezierCurve[{{-0.1,0},{-0.155,0},{-0.2,0.045},{-0.2,0.1}}],BezierCurve[{{-0.3,-0.2},{-0.245,-0.2},{-0.2,-0.245},{-0.2,-0.3}}],BezierCurve[{{-0.1,-0.2},{-0.155,-0.2},{-0.2,-0.155},{-0.2,-0.1}}],BezierCurve[{{0.,0.3},{0.,0.245},{-0.045,0.2},{-0.1,0.2}}],BezierCurve[{{0.,0.1},{0.,0.155},{0.045,0.2},{0.1,0.2}}],BezierCurve[{{0.,0.1},{0.,0.045},{-0.045,0.},{-0.1,0.}}],BezierCurve[{{0.,-0.1},{0.,-0.045},{0.045,0.},{0.1,0.}}],BezierCurve[{{0.,-0.1},{0.,-0.155},{-0.045,-0.2},{-0.1,-0.2}}],BezierCurve[{{0.,-0.3},{0.,-0.245},{0.045,-0.2},{0.1,-0.2}}],BezierCurve[{{0.1,0.2},{0.155,0.2},{0.2,0.155},{0.2,0.1}}],BezierCurve[{{0.3,0.2},{0.245,0.2},{0.2,0.245},{0.2,0.3}}],BezierCurve[{{0.1,0.},{0.155,0.},{0.2,-0.045},{0.2,-0.1}}],BezierCurve[{{0.3,0.},{0.245,0.},{0.2,0.045},{0.2,0.1}}],BezierCurve[{{0.1,-0.2},{0.155,-0.2},{0.2,-0.245},{0.2,-0.3}}],BezierCurve[{{0.3,-0.2},{0.245,-0.2},{0.2,-0.155},{0.2,-0.1}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = True; spaceH = .2; spaceV = .2; offsetH = 0; offsetV = 0; style[[1,1]] = False; fill[[1]] = False; closeLine = False; heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0;
								head[[1]] = BezierCurve; shapeStart = {{{0, 0.1}, {0, 0.045}, {-0.045, 0}, {-0.1, 0}, {-0.1, 0}}}; scaleEach = 1; angle = 0; $dwShapeArrayConnectors = {0,.5,.5,0,0,0,.5,.5,.5,0,.5}; mirrorVertical = True; mirrorHorizontal = True; strokeOpacity[[1]] = 1, $dwPresetButtonStyle],
							Button[Graphics[{Line[{{-0.3,0.2},{-0.2,0.3}}],Line[{{-0.1,0.2},{-0.2,0.1}}],Line[{{-0.2,-0.1},{-0.3,0.}}],Line[{{-0.2,0.1},{-0.1,-6.123233995736766*^-18}}],Line[{{-0.2,-0.3},{-0.3,-0.2}}],Line[{{-0.2,-0.1},{-0.1,-0.2}}],Line[{{-0.1,0.2},{0.,0.3}}],Line[{{0.1,0.2},{0.,0.1}}],Line[{{-0.1,0.},{0.,0.1}}],Line[{{0.1,0.},{0.,-0.1}}],Line[{{-0.1,-0.2},{0.,-0.1}}],Line[{{0.1,-0.2},{0.,-0.3}}],Line[{{0.2,0.1},{0.1,0.2}}],Line[{{0.2,0.3},{0.3,0.2}}],Line[{{0.2,-0.1},{0.1,0.}}],Line[{{0.2,0.1},{0.3,0.}}],Line[{{0.2,-0.3},{0.1,-0.2}}],Line[{{0.2,-0.1},{0.3,-0.2}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = True; spaceH = .2; spaceV = .2; offsetH = 0; offsetV = 0; style[[1,1]] = False; fill[[1]] = False; closeLine = False;  heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0;
								head[[1]] = Line; shapeStart = {{{-0.1, 0.}, {0., 0.1}}}; scaleEach = 1; angle = 0; $dwShapeArrayConnectors = {0,.5,.5,0,0,0,.5,.5,.5,0,.5}; mirrorVertical = True; mirrorHorizontal = True; strokeOpacity[[1]] = 1, $dwPresetButtonStyle],
							Button[Graphics[{Line[{{-0.25,0.1299},{-0.2,0.2165},{-0.25,0.3031},{-0.35,0.3031},{-0.4,0.2165},{-0.35,0.1299},{-0.25,0.1299}}],Line[{{-0.1,0.0433},{-0.05,0.1299},{-0.1,0.2165},{-0.2,0.2165},{-0.25,0.1299},{-0.2,0.0433},{-0.1,0.0433}}],Line[{{-0.25,-0.0433},{-0.2,0.0433},{-0.25,0.1299},{-0.35,0.1299},{-0.4,0.0433},{-0.35,-0.0433},{-0.25,-0.0433}}],Line[{{-0.1,-0.1299},{-0.05,-0.0433},{-0.1,0.0433},{-0.2,0.0433},{-0.25,-0.0433},{-0.2,-0.1299},{-0.1,-0.1299}}],Line[{{-0.25,-0.2165},{-0.2,-0.1299},{-0.25,-0.0433},{-0.35,-0.0433},{-0.4,-0.1299},{-0.35,-0.2165},{-0.25,-0.2165}}],Line[{{-0.1,-0.3031},{-0.05,-0.2165},{-0.1,-0.1299},{-0.2,-0.1299},{-0.25,-0.2165},{-0.2,-0.3031},{-0.1,-0.3031}}],Line[{{0.05,0.1299},{0.1,0.2165},{0.05,0.3031},{-0.05,0.3031},{-0.1,0.2165},{-0.05,0.1299},{0.05,0.1299}}],Line[{{0.2,0.0433},{0.25,0.1299},{0.2,0.2165},{0.1,0.2165},{0.05,0.1299},{0.1,0.0433},{0.2,0.0433}}],Line[{{0.05,-0.0433},{0.1,0.0433},{0.05,0.1299},{-0.05,0.1299},{-0.1,0.0433},{-0.05,-0.0433},{0.05,-0.0433}}],Line[{{0.2,-0.1299},{0.25,-0.0433},{0.2,0.0433},{0.1,0.0433},{0.05,-0.0433},{0.1,-0.1299},{0.2,-0.1299}}],Line[{{0.05,-0.2165},{0.1,-0.1299},{0.05,-0.0433},{-0.05,-0.0433},{-0.1,-0.1299},{-0.05,-0.2165},{0.05,-0.2165}}],Line[{{0.2,-0.3031},{0.25,-0.2165},{0.2,-0.1299},{0.1,-0.1299},{0.05,-0.2165},{0.1,-0.3031},{0.2,-0.3031}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = False; spaceH = .3; spaceV = .0866025; offsetH = .15; offsetV = 0; style[[1,1]] = False; fill[[1]] = False; closeLine = True;  heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0;
								head[[1]] = Line; shapeStart = {.5{{0.1,-0.17320508075688773}, {0.2,0}, {0.1,0.17320508075688773}, {-0.1,0.17320508075688773}, {-0.2,0}, {-0.1,-0.17320508075688773}}}; scaleEach = 1; angle = 0; $dwShapeArrayConnectors = {0,.5,.5,0,0,0,.5,.5,.5,0,.5}; mirrorVertical = False; mirrorHorizontal = False; strokeOpacity[[1]] = 1, $dwPresetButtonStyle],
							Button[Graphics[{Line[{{-0.45,0.3},{-0.35,0.4}}],Line[{{-0.25,0.3},{-0.35,0.2}}],Line[{{-0.35,0.},{-0.45,0.1}}],Line[{{-0.35,0.2},{-0.25,0.1}}],Line[{{-0.35,-0.2},{-0.45,-0.1}}],Line[{{-0.35,0.},{-0.25,-0.1}}],Line[{{-0.45,-0.3},{-0.35,-0.2}}],Line[{{-0.25,-0.3},{-0.35,-0.4}}],Line[{{-0.35,0.3},{-0.25,0.4}}],Line[{{-0.15,0.3},{-0.25,0.2}}],Line[{{-0.35,0.1},{-0.25,0.2}}],Line[{{-0.15,0.1},{-0.25,0.}}],Line[{{-0.25,-0.2},{-0.35,-0.1}}],Line[{{-0.25,0.},{-0.15,-0.1}}],Line[{{-0.25,-0.4},{-0.35,-0.3}}],Line[{{-0.25,-0.2},{-0.15,-0.3}}],Line[{{-0.15,0.2},{-0.25,0.3}}],Line[{{-0.15,0.4},{-0.05,0.3}}],Line[{{-0.25,0.1},{-0.15,0.2}}],Line[{{-0.05,0.1},{-0.15,0.}}],Line[{{-0.15,-0.2},{-0.25,-0.1}}],Line[{{-0.15,0.},{-0.05,-0.1}}],Line[{{-0.25,-0.3},{-0.15,-0.2}}],Line[{{-0.05,-0.3},{-0.15,-0.4}}],Line[{{-0.05,0.2},{-0.15,0.3}}],Line[{{-0.05,0.4},{0.05,0.3}}],Line[{{-0.05,0.},{-0.15,0.1}}],Line[{{-0.05,0.2},{0.05,0.1}}],Line[{{-0.15,-0.1},{-0.05,0.}}],Line[{{0.05,-0.1},{-0.05,-0.2}}],Line[{{-0.15,-0.3},{-0.05,-0.2}}],Line[{{0.05,-0.3},{-0.05,-0.4}}],Line[{{-0.05,0.3},{0.05,0.4}}],Line[{{0.15,0.3},{0.05,0.2}}],Line[{{0.05,0.},{-0.05,0.1}}],Line[{{0.05,0.2},{0.15,0.1}}],Line[{{0.05,-0.2},{-0.05,-0.1}}],Line[{{0.05,0.},{0.15,-0.1}}],Line[{{0.05,-0.4},{-0.05,-0.3}}],Line[{{0.05,-0.2},{0.15,-0.3}}],Line[{{0.05,0.3},{0.15,0.4}}],Line[{{0.25,0.3},{0.15,0.2}}],Line[{{0.15,0.},{0.05,0.1}}],Line[{{0.15,0.2},{0.25,0.1}}],Line[{{0.05,-0.1},{0.15,0.}}],Line[{{0.25,-0.1},{0.15,-0.2}}],Line[{{0.15,-0.4},{0.05,-0.3}}],Line[{{0.15,-0.2},{0.25,-0.3}}],Line[{{0.25,0.2},{0.15,0.3}}],Line[{{0.25,0.4},{0.35,0.3}}],Line[{{0.15,0.1},{0.25,0.2}}],Line[{{0.35,0.1},{0.25,0.}}],Line[{{0.15,-0.1},{0.25,0.}}],Line[{{0.35,-0.1},{0.25,-0.2}}],Line[{{0.15,-0.3},{0.25,-0.2}}],Line[{{0.35,-0.3},{0.25,-0.4}}],Line[{{0.35,0.2},{0.25,0.3}}],Line[{{0.35,0.4},{0.45,0.3}}],Line[{{0.35,0.},{0.25,0.1}}],Line[{{0.35,0.2},{0.45,0.1}}],Line[{{0.35,-0.2},{0.25,-0.1}}],Line[{{0.35,0.},{0.45,-0.1}}],Line[{{0.25,-0.3},{0.35,-0.2}}],Line[{{0.45,-0.3},{0.35,-0.4}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = True; spaceH = .1; spaceV = .2; offsetH = 0; offsetV = 0; style[[1,1]] = False; fill[[1]] = False; closeLine = False;  heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0;
								head[[1]] = Line; shapeStart = {{{-0.1, 0.}, {0., 0.1}}}; scaleEach = 1; angle = 0; $dwShapeArrayConnectors = {0,.5,.5,0,0,0,.5,.5,.5,0,.5}; mirrorVertical = True; mirrorHorizontal = True; strokeOpacity[[1]] = 1, $dwPresetButtonStyle],
							Button[Graphics[{BezierCurve[{{-0.3,0.4},{-0.3,0.345},{-0.345,0.3},{-0.4,0.3}}],BezierCurve[{{-0.3,0.2},{-0.3,0.255},{-0.255,0.3},{-0.2,0.3}}],BezierCurve[{{-0.4,0.1},{-0.345,0.1},{-0.3,0.055},{-0.3,0.}}],BezierCurve[{{-0.2,0.1},{-0.255,0.1},{-0.3,0.145},{-0.3,0.2}}],BezierCurve[{{-0.4,-0.1},{-0.345,-0.1},{-0.3,-0.145},{-0.3,-0.2}}],BezierCurve[{{-0.2,-0.1},{-0.255,-0.1},{-0.3,-0.055},{-0.3,0.}}],BezierCurve[{{-0.3,-0.2},{-0.3,-0.255},{-0.345,-0.3},{-0.4,-0.3}}],BezierCurve[{{-0.3,-0.4},{-0.3,-0.345},{-0.255,-0.3},{-0.2,-0.3}}],BezierCurve[{{-0.2,0.4},{-0.2,0.345},{-0.245,0.3},{-0.3,0.3}}],BezierCurve[{{-0.2,0.2},{-0.2,0.255},{-0.155,0.3},{-0.1,0.3}}],BezierCurve[{{-0.2,0.2},{-0.2,0.145},{-0.245,0.1},{-0.3,0.1}}],BezierCurve[{{-0.2,0.},{-0.2,0.055},{-0.155,0.1},{-0.1,0.1}}],BezierCurve[{{-0.3,-0.1},{-0.245,-0.1},{-0.2,-0.145},{-0.2,-0.2}}],BezierCurve[{{-0.1,-0.1},{-0.155,-0.1},{-0.2,-0.055},{-0.2,0.}}],BezierCurve[{{-0.3,-0.3},{-0.245,-0.3},{-0.2,-0.345},{-0.2,-0.4}}],BezierCurve[{{-0.1,-0.3},{-0.155,-0.3},{-0.2,-0.255},{-0.2,-0.2}}],BezierCurve[{{-0.2,0.3},{-0.145,0.3},{-0.1,0.255},{-0.1,0.2}}],BezierCurve[{{0.,0.3},{-0.055,0.3},{-0.1,0.345},{-0.1,0.4}}],BezierCurve[{{-0.1,0.2},{-0.1,0.145},{-0.145,0.1},{-0.2,0.1}}],BezierCurve[{{-0.1,0.},{-0.1,0.055},{-0.055,0.1},{0.,0.1}}],BezierCurve[{{-0.2,-0.1},{-0.145,-0.1},{-0.1,-0.145},{-0.1,-0.2}}],BezierCurve[{{0.,-0.1},{-0.055,-0.1},{-0.1,-0.055},{-0.1,0.}}],BezierCurve[{{-0.1,-0.2},{-0.1,-0.255},{-0.145,-0.3},{-0.2,-0.3}}],BezierCurve[{{-0.1,-0.4},{-0.1,-0.345},{-0.055,-0.3},{0.,-0.3}}],BezierCurve[{{-0.1,0.3},{-0.045,0.3},{0.,0.255},{0.,0.2}}],BezierCurve[{{0.1,0.3},{0.045,0.3},{0.,0.345},{0.,0.4}}],BezierCurve[{{-0.1,0.1},{-0.045,0.1},{0.,0.055},{0.,0.}}],BezierCurve[{{0.1,0.1},{0.045,0.1},{0.,0.145},{0.,0.2}}],BezierCurve[{{0.,0.},{0.,-0.055},{-0.045,-0.1},{-0.1,-0.1}}],BezierCurve[{{0.,-0.2},{0.,-0.145},{0.045,-0.1},{0.1,-0.1}}],BezierCurve[{{0.,-0.2},{0.,-0.255},{-0.045,-0.3},{-0.1,-0.3}}],BezierCurve[{{0.,-0.4},{0.,-0.345},{0.045,-0.3},{0.1,-0.3}}],BezierCurve[{{0.1,0.4},{0.1,0.345},{0.055,0.3},{0.,0.3}}],BezierCurve[{{0.1,0.2},{0.1,0.255},{0.145,0.3},{0.2,0.3}}],BezierCurve[{{0.,0.1},{0.055,0.1},{0.1,0.055},{0.1,0.}}],BezierCurve[{{0.2,0.1},{0.145,0.1},{0.1,0.145},{0.1,0.2}}],BezierCurve[{{0.,-0.1},{0.055,-0.1},{0.1,-0.145},{0.1,-0.2}}],BezierCurve[{{0.2,-0.1},{0.145,-0.1},{0.1,-0.055},{0.1,0.}}],BezierCurve[{{0.,-0.3},{0.055,-0.3},{0.1,-0.345},{0.1,-0.4}}],BezierCurve[{{0.2,-0.3},{0.145,-0.3},{0.1,-0.255},{0.1,-0.2}}],BezierCurve[{{0.2,0.4},{0.2,0.345},{0.155,0.3},{0.1,0.3}}],BezierCurve[{{0.2,0.2},{0.2,0.255},{0.245,0.3},{0.3,0.3}}],BezierCurve[{{0.1,0.1},{0.155,0.1},{0.2,0.055},{0.2,0.}}],BezierCurve[{{0.3,0.1},{0.245,0.1},{0.2,0.145},{0.2,0.2}}],BezierCurve[{{0.2,0.},{0.2,-0.055},{0.155,-0.1},{0.1,-0.1}}],BezierCurve[{{0.2,-0.2},{0.2,-0.145},{0.245,-0.1},{0.3,-0.1}}],BezierCurve[{{0.1,-0.3},{0.155,-0.3},{0.2,-0.345},{0.2,-0.4}}],BezierCurve[{{0.3,-0.3},{0.245,-0.3},{0.2,-0.255},{0.2,-0.2}}],BezierCurve[{{0.2,0.3},{0.255,0.3},{0.3,0.255},{0.3,0.2}}],BezierCurve[{{0.4,0.3},{0.345,0.3},{0.3,0.345},{0.3,0.4}}],BezierCurve[{{0.3,0.2},{0.3,0.145},{0.255,0.1},{0.2,0.1}}],BezierCurve[{{0.3,0.},{0.3,0.055},{0.345,0.1},{0.4,0.1}}],BezierCurve[{{0.3,0.},{0.3,-0.055},{0.255,-0.1},{0.2,-0.1}}],BezierCurve[{{0.3,-0.2},{0.3,-0.145},{0.345,-0.1},{0.4,-0.1}}],BezierCurve[{{0.3,-0.2},{0.3,-0.255},{0.255,-0.3},{0.2,-0.3}}],BezierCurve[{{0.3,-0.4},{0.3,-0.345},{0.345,-0.3},{0.4,-0.3}}]}, ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = 1; randomRotate = True; spaceH = .1; spaceV = .2; offsetH = 0; offsetV = 0; style[[1,1]] = False; fill[[1]] = False; closeLine = False;  heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0;
								head[[1]] = BezierCurve; shapeStart = {{{0, 0.1}, {0, 0.045}, {-0.045, 0}, {-0.1, 0}, {-0.1, 0}}}; scaleEach = 1; angle = 0; $dwShapeArrayConnectors = {0,.5,.5,0,0,0,.5,.5,.5,0,.5}; mirrorVertical = True; mirrorHorizontal = True; strokeOpacity[[1]] = 1, $dwPresetButtonStyle],
							Button[Graphics[Table[Circle[{0, 0}, n], {n, {.3, .5, .6, .8, 1}}], ImageSize->{27, 27}], 
								randomQuantity = "All"; randomMinSize = .1; randomRotate = False; spaceH = 0; spaceV = 0; offsetH = 0; offsetV = 0; style[[1,1]] = False; fill[[1]] = False; closeLine = True;  heightScale = 0; widthScale = 0; $dwShapeArraySeedAmount = 0; scaleEach = 1; scaleAll = 1;
								head[[1]] = BezierCurve; shapeStart = {5{{0.,0.2},{0.11,0.2},{0.2,0.11},{0.2,0.},{0.2,-0.11},{0.11,-0.2},{0.,-0.2},{-0.11,-0.2},{-0.2,-0.11},{-0.2,0.},{-0.2,0.11},{-0.11,0.2},{0.,0.2},{0.11,0.2}}}; scaleEach = 1; angle = 0; $dwShapeArrayConnectors = {0,.5,.5,0,0,0,.5,.5,.5,0,.5}; mirrorVertical = False; mirrorHorizontal = False; strokeOpacity[[1]] = 1, $dwPresetButtonStyle],
						Spacer[30]}]
						
						}},Spacings->{0,0}], Alignment->Center, ImageSize->710],
						
						
				(* <<<<<<<<<< STYLE >>>>>>>>>> *)
				Panel[Column[{
					Style["STYLE", 15],
					Row[{Checkbox[Dynamic@changeStyle]," Use styles"}],
					Row@{
					Checkbox[Dynamic@randomColor]," use random color",Spacer[30],
					PopupMenu[Dynamic@color, #->Row[ColorData[#,"ColorList"][[;; 5]]]&/@Range[97,113], Enabled->randomColor], Spacer[30],
					Sequence@@If[head[[1]] =!= Point, {(*Style[*)"fill opacity "(*,If[randomColor,Black,Gray]]*),Slider[Dynamic@fillOpacity[[1]],{0,1,.05}(*,Enabled->randomColor*),ImageSize->Tiny],Spacer[30]}, {}],
					(*Style[*)If[head[[1]] === Point, "point opacity ", "stroke opacity "](*,If[randomColor,Black,Gray]]*),Slider[Dynamic@strokeOpacity[[1]],{0,1,.05}(*,Enabled->randomColor*),ImageSize->Tiny],Spacer[30],
					Sequence@@If[head[[1]] === Point, {(*Style[*)"point size "(*,If[randomColor,Black,Gray]]*),Slider[Dynamic@pointSize[[1]],{1,24,1}(*,Enabled->randomColor*),ImageSize->Tiny]}, {}]
					}
				}, Alignment->Center], Alignment->Center, ImageSize->710],
				
				
				(* <<<<<<<<<< TILE ROTATION >>>>>>>>>> *)
				Panel[Column[{
					Style["TILE ROTATION", 15],
					Row[{Checkbox[Dynamic@randomRotate]," Rotate tiles"}],
					"Enter a repeating list of radians which correspond to the rotation of each square tile: 0.0 = 0 Pi, 0.5 = Pi/2, 1.0 = Pi, 1.5 = 1.5 Pi",
					"The tile order starts at the upper left corner advancing downward then moves to the top of the next column advancing downward, etc.",
					"A custom object works best centered on canvas since it affects tile position. Input below can be as simple as RandomReal[{-0.25, 0.25}, 50].",
					Row[{InputField[Dynamic@$dwShapeArrayConnectors, Enabled->Dynamic@randomRotate, ImageSize->{500,23}], 
						ActionMenu["choose rotation",{
							{0}:>($dwShapeArrayConnectors = {0}),
							{.5}:>($dwShapeArrayConnectors = {.5}),
							{0,.5}:>($dwShapeArrayConnectors = {0,.5}),
							{0,.5,0}:>($dwShapeArrayConnectors = {0,.5,0}),
							{0,.5,.5}:>($dwShapeArrayConnectors = {0,.5,.5}),
							{0,0,.5,.5}:>($dwShapeArrayConnectors = {0,0,.5,.5}),
							{0,0,0,.5,.5}:>($dwShapeArrayConnectors = {0,0,0,.5,.5}),
							{0,0,.5,.5,.5}:>($dwShapeArrayConnectors = {0,0,.5,.5,.5}),
							{0,.5,.5,0,0,0,.5,.5,.5,0,.5}:>($dwShapeArrayConnectors = {0,.5,.5,0,0,0,.5,.5,.5,0,.5}),
							{.25,.75,.75,.25,.25,.25,.75,.75,.75,.25,.75}:>($dwShapeArrayConnectors = {.25,.75,.75,.25,.25,.25,.75,.75,.75,.25,.75}),
							"RandomReal[{-1/3,1/3},50]":>($dwShapeArrayConnectors = RandomReal[{-1/3, 1/3},50]),
							"RandomReal[{-2/3, 2/3}, 50]":>($dwShapeArrayConnectors = RandomReal[{-2/3, 2/3}, 50]),
							"RandomReal[{-3/3, 3/3}, 50]":>($dwShapeArrayConnectors = RandomReal[{-3/3, 3/3}, 50]),
							"RandomReal[{-4/3, 4/3}, 50]":>($dwShapeArrayConnectors = RandomReal[{-4/3, 4/3}, 50]),
							"RandomReal[{-5/3, 5/3}, 50]":>($dwShapeArrayConnectors = RandomReal[{-5/3, 5/3}, 50])
						}, BaselinePosition->Scaled[.45], Enabled->Dynamic@randomRotate]}]
				}, Alignment->Center], Alignment->Center, ImageSize->710],
				
				
				(* <<<<<<<<<< OBJECTS TO CANVAS >>>>>>>>>> *)
				Row[{
					Button["Cancel",
						$dwMessageBarText = ""; 
						$dwMode="preview";
						DialogReturn[],
						ImageSize->100],
					DefaultButton["Create shapes",
						dwSetUndo[];
						$dwMode="preview";(* must be before dwDeleteLayer[] *)
						If[$dwSelected =!= {}, 
							dwDeleteLayer["SetUndo"->False], 
							Nothing
						];
						ctr = prn = srn = shapeCtr = 0;
						offsetHList = ReplacePart[Table[0,{rows}],Table[n->offsetH,{n,1,rows,2}]];
						offsetVList = ReplacePart[Table[0,{columns}],Table[n->offsetV,{n,1,columns,2}]];
						
						If[head[[1]] === Text,
							
							(* text *)
							textSize = Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[Range[fontMinSize,fontMinSize+Max[fontIncreaseSize,1],1/Length[$dwShapeArrayCharacters]], Length[$dwShapeArrayCharacters]]];
							Do[
								If[++ctr > Length[$dwShapeArrayCharacters], ctr=1, ctr];
								dwNewEmptyLayer["Form"->"text", "SetUndo"->False];
								$dwP[[-1]] = {scaleAll ({(x - 1) spaceH, -(y - 1) spaceV} + {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2)};
								$dwStyle[[-1, 1]] = If[randomRotate, $dwShapeArrayConnectors[[ctr]]*(Pi/2), 0];(* rotation *)
								$dwStyle[[-1, 2]] = $dwShapeArrayCharacters[[ctr]];(* text *)
								$dwStyle[[-1, 3]] = Evaluate[SeedRandom[$dwShapeArraySeed+ctr]; $dwShapeArraySeedAmount*RandomReal[{-1,1},2]];(* text aligment *)
								$dwStyle[[-1, Flatten[Position[$dwStyle[[-1]], FontFamily]][[1]]]][[2]] = $dwShapeArrayFontFamily;
								$dwStyle[[-1, Flatten[Position[$dwStyle[[-1]], FontColor]][[1]]]][[2]] = If[randomColor, ColorData[color,y+(x-1)*rows], Black];
								$dwStyle[[-1, Flatten[Position[$dwStyle[[-1]], FontSize]][[1]]]][[2]] = Round[.75($dwOverallSize/325)*If[EvenQ[x+y], RotateRight[textSize,x+y], RotateLeft[textSize,x+y]][[ctr]]](*;
								dwUpdateBoundingBox[{-1}]*),
							{x,columns},{y,rows}],
									
							(* shape *)
							temp1 = -1;
							newPos = Range[(originalLength - originalSelectionLength) + 1, (originalLength - originalSelectionLength) + rows*columns*originalSelectionLength];
							Do[
								++srn;
								++prn;
								++temp1;
								If[++ctr > Length[$dwShapeArrayConnectors], ctr=1, Nothing];
								Do[
									If[++shapeCtr > Length[shapeStart], shapeCtr = 1];
									Do[
										dwNewEmptyLayer["Head"->head[[cs]], "SetUndo"->False];
										If[randomRotate, 
											randomRot = If[shapeMirrorStart === {},
													RotationTransform[$dwShapeArrayConnectors[[ctr]]*(Pi), Mean[DeleteDuplicates[shapeStart[[shapeCtr]]]]],
													RotationTransform[$dwShapeArrayConnectors[[ctr]]*(Pi), Mean[DeleteDuplicates[Join[shapeStart[[shapeCtr]],shapeMirrorStart[[shapeCtr]]]]]]
												];
											shape = randomRot[shapePts],
											shape = shapePts
										];
														
										$dwP[[-1]] = scaleAll ({(x - 1) spaceH, -(y - 1) spaceV} + # - {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2 + Evaluate[SeedRandom[$dwShapeArraySeed+ctr]; posRandom[[prn]]*$dwShapeArraySeedAmount*RandomReal[{-1,1},2]])&/@((1-Abs[heightScale]*(If[heightScale<0, rows-(y-1), y]/rows))*(1-Abs[widthScale]*(If[widthScale<0, columns-(x-1), x]/columns))*scaleRandom[[srn]]*scaleEach*rotate[Switch[head[[1]],Line,If[closeLine&&Length[shape]>2,Join[shape,shape[[{1}]]],shape],BezierCurve,Most[shape],_,shape]]);
										If[head[[1]] === BezierCurve,
											$dwP[[-1]] = Join[$dwP[[-1]], $dwP[[-1]][[{-1}]]],
											Nothing
										];
										If[changeStyle,
											
											If[randomColor,
												
												$dwStyle[[-1,1]] = style[[1,1]];
												$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], FaceForm[_]]][[1]]]][[1]] = {ColorData[color,y+(x-1)*rows],Opacity[fillOpacity[[1]]]};
												$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]]]][[1,1]] = ColorData[color,y+(x-1)*rows];
												$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]]]][[1,2]] = Opacity[strokeOpacity[[1]]];
												$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]]]][[1,3]] = AbsoluteThickness[thickness[[1]]];
												$dwStyle[[-1,$dwStyleStart]]=ColorData[color,y+(x-1)*rows];
												$dwStyle[[-1,$dwStyleStart+1]]=Opacity[strokeOpacity[[1]]];
												$dwStyle[[-1,7]] = pointSize[[1]];
												$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], AbsolutePointSize[_]]][[1]]]][[1]] = pointSize[[1]],
												
												$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]]]][[1,2]] = Opacity[strokeOpacity[[1]]];
												$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], FaceForm[_]]][[1]]]][[1,2]] = Opacity[fillOpacity[[1]]];
												If[head[[cs]] === Point,
													$dwStyle[[-1,7]] = pointSize[[1]];
													$dwStyle[[-1,14]] = Opacity[strokeOpacity[[1]]];
													$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], AbsolutePointSize[_]]][[1]]]][[1]] = pointSize[[1]]
												]
											],
											
											$dwStyle[[-1]] = style[[cs]]
										];
										If[head[[cs]] === BSplineCurve, $dwStyle[[-1,10]] = closeLine, Nothing];
										
										(* update blend *)
										If[Position[$dwStyle[[-1]], "BlendGradient", Infinity] =!= {},
 
											temp = Flatten[Position[$dwStyle[[-1]], "BlendGradient", Infinity]];
											oldPos = $dwStyle[[-1, Sequence@@temp[[;; -2]], 2]];
											
											If[MemberQ[originalSelection, oldPos],
											   
											   (*update target number*)
												$dwStyle[[-1, Sequence@@temp[[;; -2]], 2]] = newPos[[Flatten[Position[originalSelection, oldPos]][[1]]]] + (temp1)*originalSelectionLength,
											   
											   (*remove blend since target not included*)
											   $dwStyle[[-1, Sequence@@temp[[;; -2]]]] = None
											]
										],
										
									{shapePts, If[shapeMirrorStart === {}, {shapeStart[[shapeCtr]]}, {shapeStart[[shapeCtr]], shapeMirrorStart[[shapeCtr]]}]}],
								{cs, If[combineShapes, Length[shapeStart], 1]}],
							{x,columns},{y,rows}]
						];
						dwUpdateGradients[Range[originalLength + If[selectedShapeUsed, -originalSelectionLength+1, 1], Length[$dwP]]];
						(* idea for fast bounding boxes - create bounding box of first shape then move copy to each center instead of discretizing for each which is very slow *)
						dwUpdateBoundingBox[Range[originalLength + If[selectedShapeUsed, -originalSelectionLength+1, 1], Length[$dwP]]];
						$dwObjectQuantity = Length[$dwP];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						$dwGroupLayers = Join[$dwGroupLayers, {Range[originalLength + If[selectedShapeUsed, -originalSelectionLength+1, 1], Length[$dwP]]}];
						$dwSelected = $dwGroupLayers[[-1]];
						$dwMessageBarText = "";
						dwSetUndo[];
						DialogReturn[],
					ImageSize->150(*, Method->"Queued"*)]}](* "Queued" causes kernel crashes so do not uncomment *)
			},Alignment->Center]
		]],
	Background->LightGray, WindowTitle->"Shape array", Modal->True]

Options[dwShapeArrayPreview]={"Rows"->3, "Columns"->3, "RandomRotate"->False, "ShapeMirrorStart"->{}, "Connectors"->{0}, "FontMinimumSize"->6, "FontIncreaseSize"->6};
dwShapeArrayPreview[head_:Polygon, shape_:.1{{-1,-1},{1,-1},{1,1},{-1,1}}, scaleEach_:1, angle_:0, fill_:True, closeLine_:True, spaceH_:.4, spaceV_:.4, offsetH_:0, offsetV_:0, OptionsPattern[]]:=
	Block[{textSize, offsetHList, offsetVList, colList = {3,3,3}, scaleAll = 1, rotate, ctr = 0, ctr2 = 0, strokeOpacity = {1}, fillOpacity = {.5}, color = Black, rows, columns, randomRotate, shapeNew, shapeStart, shapeMirrorStart, connectors, shapeMirror, randomRot, fontMinSize, fontIncreaseSize},
		{rows, columns, randomRotate, shapeMirrorStart, connectors, fontMinSize, fontIncreaseSize} = {OptionValue["Rows"], OptionValue["Columns"], OptionValue["RandomRotate"], OptionValue["ShapeMirrorStart"], OptionValue["Connectors"], OptionValue["FontMinimumSize"], OptionValue["FontIncreaseSize"]};
		shapeStart = shape; 
		offsetHList=ReplacePart[Table[0,{rows}],Table[n->offsetH,{n,1,rows,2}]];(* rows *)
		offsetVList=ReplacePart[Table[0,{columns}],Table[n->offsetV,{n,1,columns,2}]];(* columns *)
		rotate = RotationTransform[angle];
		textSize = Evaluate[SeedRandom[$dwShapeArraySeed]; RandomSample[Range[fontMinSize,fontMinSize+Max[fontIncreaseSize,1],1/Length[$dwShapeArrayCharacters]], Length[$dwShapeArrayCharacters]]];
		Graphics[{FaceForm[GrayLevel[.7]], StrokeForm[Black], 
			Table[
				If[head[[1]] === Text,
					If[++ctr2>Length[$dwShapeArrayCharacters],ctr2=1,ctr2];
					If[randomRotate, 
						Rotate[Text[Style[$dwShapeArrayCharacters[[ctr2]], textSize[[ctr2]]], ({(x - 1) spaceH, -(y - 1) spaceV} + {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2)], connectors[[ctr]]*(Pi/2), ({(x - 1) spaceH, -(y - 1) spaceV} + {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2)],
						Text[Style[$dwShapeArrayCharacters[[ctr2]], textSize[[ctr2]]], ({(x - 1) spaceH, -(y - 1) spaceV} + {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2)]
					],
					
				{
					If[randomRotate, 
						If[++ctr>Length[connectors],ctr=1,ctr];
						randomRot = If[shapeMirrorStart === {},
								RotationTransform[connectors[[ctr]]*(Pi/2), Mean[DeleteDuplicates[shapeStart]]],
								RotationTransform[connectors[[ctr]]*(Pi/2), Mean[DeleteDuplicates[Join[shapeStart,shapeMirrorStart]]]]
							], 
						{}
					];
					If[randomRotate, 
						shapeNew=randomRot[shapeStart]; 
						If[shapeMirrorStart =!= {},
							shapeMirror=randomRot[shapeMirrorStart],
							Nothing
						],
						shapeNew=shapeStart; shapeMirror=shapeMirrorStart
					];
					If[MemberQ[{BezierCurve,BSplineCurve},head[[1]]] && fill[[1]],FilledCurve,Sequence][head[[1]][scaleAll ({(x - 1) spaceH, -(y - 1) spaceV} + # - {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2)&/@(scaleEach*rotate[Switch[head[[1]],Line,If[closeLine&&Length[shapeNew]>2,Join[shapeNew,shapeNew[[{1}]]],shapeNew],BezierCurve,Most[shapeNew],_,shapeNew]]),If[head[[1]]===BSplineCurve,SplineClosed->If[closeLine,True,False],Null]]/.{Null->Sequence[]}],
					If[shapeMirrorStart =!= {},
						If[MemberQ[{BezierCurve,BSplineCurve},head[[1]]] && fill[[1]],FilledCurve,Sequence][head[[1]][scaleAll ({(x - 1) spaceH, -(y - 1) spaceV} + # - {offsetHList[[y]], -offsetVList[[x]]} - {spaceH*(columns - 1), - spaceV*(rows - 1)}/2)&/@(scaleEach*rotate[Switch[head[[1]],Line,If[closeLine&&Length[shapeMirror]>2,Join[shapeMirror,shapeMirror[[{1}]]],shapeMirror],BezierCurve,Most[shapeMirror],_,shapeMirror]]),If[head[[1]]===BSplineCurve,SplineClosed->If[closeLine,True,False],Null]]/.{Null->Sequence[]}],
						{}
					]
				}
			],{x,columns},{y,rows}]
		}, Background->GrayLevel[.92]]
	]

End[] (* End Private Context *)

EndPackage[]