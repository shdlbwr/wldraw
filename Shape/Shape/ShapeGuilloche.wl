(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwGuillocheDialog[]:=
	CreateDialog[
		DynamicModule[{ctr, showAll = True, portionH, portionV, weave1 = 22, loop1 = 1, loop2 = 9, loop3 = 1, amplify = 1, degree = 2, dilate = 0, 
			scale = 1, sector = 5, weave2 = 4, rotate = 0, temp, distort = "corners", distortamount = 1,
			square = {{4,8,0,2,0,1,13,1},{4,8,0,2,-2,1,13,1},{4,8,0,2,2,1,15,7},{5,8,0,2,1.5`,1,9,1},{5,8,0,2,1.5`,1,9,9},{4,8,0,2,2,1,17,9},{5,5,1.5`,21,0,1,9,1},{5,5,1.5`,13,0,1,17,9},{4,5,2,22,4,1,9,9},{4,5,1,22,0,1,9,1},{4,8,0,2,0,1,17,9},{4,5,1,16,2,1,9,1},{4,5,1,16,0,1,9,1},{4,5,1,18,0,1,9,1},{4,5,1,22,0,1,13,1},{4,8,1,4,0,1,13,1},{4,5,1,22,5,1,17,1},{4,5,1,20,5,1,17,9},{4,5,1,14,5,1,17,9},{4,8,1,6,0,1,9,1}},
			presetButtonData = {{4,5,1,22,0,1,9,1},{4,5,1,22,1.5`,1,5,3},{4,8,1,4,0,1,9,1},{4,8,0,1,0,1,13,9},{4,5,1,20,0,1,9,1},{4,5,2,22,0,1,9,1},{4,5,2,22,0,1,13,5},{4,8,0,2,0,1,13,1},{4,8,0,2,2,1,17,9},{4,5,1,22,5,1,17,1},{4,8,1,8,7,1,1,1},{4,5,1,14,5,1,17,17},{4,8,3,8,0,1,17,1},{4,5,1,16,5,1,9,9},{4,8,0,1,0,1,13,1},{4,5,1,16,5,1,9,7},{5,8,0,1,0,1,9,7},{5,5,0,1,0,1,17,9},{5,8,0,2,1.5`,1,9,9},{4,5,1,16,2,1,9,1},{4,5,1,14,5,1,17,9},{4,5,1,22,5,1,5,5},{4,5,1,20,5,1,9,9},{4,5,3,22,0,1,5,1},{4,8,1,9,0,1,13,1},{4,5,2,22,4,1,9,9},{4,5,1,12,5,1,17,17},{4,5,1,14,0,1,13,1},{5,0,0,1,0,1,17,11},{4,5,2,19,5,1,5,5},{4,5,2,21,5,1,5,5},{5,5,0.7`,9,0,1,5,1},{4,8,0,1,0,1,17,5},{4,8,0,1,0,1,13,11},{4,5,1,18,0,1,9,1},{4,8,1,3,0,1,9,1},{4,8,1,4,0,1,9,5},{4,5,1,20,0,1,9,5},{4,5,1,12,0,1,13,5},{4,5,1,16,0,1,13,7},{4,8,0,2,0,1,17,9},{4,5,1,16,0,1,9,1},{4,5,1,18,0,1,9,1},{4,5,1,22,5,1,17,1},{4,5,1,22,0,1,13,1},{4,5,1,20,5,1,17,9},{4,5,1,18,5,1,17,17},{4,8,2,6,0,1,13,7},{4,8,3,4,0,1,9,1},{4,5,1,19,0,1,13,1},{4,5,1,20,5,1,17,9},{5,5,1.5`,21,0,1,9,1},{4,5,1,14,0,1,13,5},{4,5,2,17,0,1,5,5},{4,5,2,20,0,1,5,5},{4,5,2,22,0,1,5,5},{4,8,1,4,0,1,9,9},{4,8,1,20,0,1,5,5},{4,5,1,18,0,1,9,9},{4,5,2,22,0,1,9,9},{4,5,2,8,-5,1,9,1},{4,5,2,14,-5,1,9,1},{4,5,2,18,-5,1,9,1},{4,5,1,10,0,1,13,7},{4,5,1,16,0,1,9,9},{4,5,1,20,0,1,9,9},{4,5,1,22,0,1,13,9},{4,8,2,3,0,1,13,7},{4,5,2,21,0,1,9,7},{4,8,1,4,0,1,9,7},{4,5,2,22,0,1,9,7},{4,5,1,24,0,1,9,7},{4,8,2,2,0,1,9,9},{4,8,1,21,0,1,9,5},{4,8,1,4,0,1,13,9},{5,8,1,7,0,1,5,5},{4,8,1,3,0,1,13,11},{5,8,1,7,0,1,7,5},{4,5,1,13,0,1,17,5},{4,8,2,5,0,1,13,7},{4,5,1,14,0,1,9,9},{4,5,2,18,0,1,17,9},{4,8,3,4,-5,1,9,1},{4,5,1,19,0,1,13,7},{4,5,2,22,0,1,13,7},{4,8,3,4,0,1,13,5},{4,8,2,2,0,1,9,1},{5,0,0,1,0,1,17,5},{4,8,1,3,0,1,13,1},{4,8,1,4,0,1,13,1}}
			},
			Pane[
				Grid[{
					{Grid[{{
						Column[{
							Dynamic@Grid[{
								{Pane["form ", ImageSize->80, Alignment->Right],
									EventHandler[
										SetterBar[Dynamic@weave2,{4,5}], 
										{"MouseClicked":>(If[weave2 == 5 && MemberQ[{0,13,16}, sector], sector = 8, Nothing])}, 
									PassEventsDown->True]
								},
									Which[
										amplify == 0,
											{"",""},
										weave2 == 4,
											{"variant ", SetterBar[Dynamic@sector,{0,5,8,13,16}]},
										True,
											{"variant ", SetterBar[Dynamic@sector,{5,8}]}
									]
								
							}, Alignment->{{Right, Left}}],
							Row@{Pane["amplify ", ImageSize->80, Alignment->Right], Dynamic@Slider[Dynamic@amplify,{0,10,.1}, ContinuousAction->False], 
								Spacer[5],
								Button["<",amplify=Max[amplify-=.1,0],Appearance->"Palette"],
								Button[">",amplify=Min[amplify+=.1,10],Appearance->"Palette"], 
								Spacer[5], 
								Pane[Dynamic@Chop[amplify], ImageSize->40, Alignment->Left]},
							"",
							"-------------------------- WEAVE --------------------------",
							Dynamic@If[amplify == 0,
								"",
								Row@{
									Pane["weave ",ImageSize->80, Alignment->Right], Dynamic@Slider[Dynamic@weave1,{1,24,1}, ContinuousAction->False], 
									Spacer[5],
									Button["<",weave1=Max[weave1-=1,1],Appearance->"Palette"],
									Button[">",weave1=Min[weave1+=1,24],Appearance->"Palette"], 
									Spacer[5], 
									Pane[Dynamic@weave1, ImageSize->40, Alignment->Left]
								}
							],
							Row@{Pane["dilate ", ImageSize->80, Alignment->Right], Dynamic@Slider[Dynamic@dilate,{-10,10,.1}, ContinuousAction->False], 
								Spacer[5],
								Button["<",dilate=If[-1 < dilate <= 1, -1, Max[dilate-=.1, -10]], Appearance->"Palette"],
								Button[">",dilate=If[-1 <= dilate < 1, 1, Min[dilate+=.1, 10]], Appearance->"Palette"], 
								Spacer[5], 
								Pane[Dynamic@Which[dilate >= 1, dilate, dilate <= -1, dilate, True, 0], ImageSize->40, Alignment->Left]},
							Row@{Pane["loop ", ImageSize->80, Alignment->Right], Dynamic@Slider[Dynamic@loop2,{1,20,1}, ContinuousAction->False], 
								Spacer[5],
								Button["<",loop2=Max[loop2-=1,1],Appearance->"Palette"],
								Button[">",loop2=Min[loop2+=1,20],Appearance->"Palette"], 
								Spacer[5], 
								Pane[Dynamic[loop2-1], ImageSize->40, Alignment->Left]},
							Row@{Pane["loop ", ImageSize->80, Alignment->Right], Dynamic@Slider[Dynamic@loop3,{1,20,1}, ContinuousAction->False], 
								Spacer[5],
								Button["<",loop3=Max[loop3-=1,1],Appearance->"Palette"],
								Button[">",loop3=Min[loop3+=1,20],Appearance->"Palette"], 
								Spacer[5], 
								Pane[Dynamic[loop3-1], ImageSize->40, Alignment->Left]},
							"",
							"------------------------ DISTORTION -----------------------",
							Row@{Spacer[80], PopupMenu[Dynamic@distort, {"spherize", "corners", "horizontal", "vertical"}], Spacer[30], Button["reset", distortamount = 0]},
							Row@{Pane["amount ",ImageSize->80, Alignment->Right], Dynamic@Slider[Dynamic@distortamount,{-1,1,.01}], 
								Spacer[5],
								Button["<",distortamount=Max[distortamount-=.1,-1],Appearance->"Palette"],
								Button[">",distortamount=Min[distortamount+=.1,1],Appearance->"Palette"], 
								Spacer[5], 
								Pane[Dynamic@distortamount, ImageSize->40, Alignment->Left]},
							Row@{
								Spacer[30],
								Button["straight", degree = 1, ImageSize->100],
								Button["curve", degree = 2, ImageSize->100],
								Button["tight curve", degree = 3, ImageSize->100]
							}
						},Alignment->Left],
						Grid[{{
							Dynamic@IntervalSlider[Dynamic[portionV], MinMax[#[[2]]&/@dwGuilloche[(6*weave1-3)+(44*sector), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate]]+{-.1,.1}, Appearance->{"Paired", "Vertical"}, Enabled->If[showAll, False, True], ImageSize->{Automatic,300}],
							Dynamic@Graphics[{
								(* weave1 *)
								Thin, BSplineCurve[
									If[showAll,
										dwGuilloche[(44*sector)+(6*weave1-3), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate, "Distort"->distort, "DistortAmount"->distortamount],
										Table[If[portionH[[1]]<p[[1]]<portionH[[2]]&&portionV[[1]]<p[[2]]<portionV[[2]],p,Nothing],{p,dwGuilloche[(6*weave1-3)+(44*sector), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate, "Distort"->distort, "DistortAmount"->distortamount]}]
									], SplineClosed->True, SplineDegree->degree]
								
								},Background->White, ImageSize->{300,300}]
							},{
							Null,
							Dynamic@IntervalSlider[Dynamic[portionH], MinMax[#[[1]]&/@dwGuilloche[(6*weave1-3)+(44*sector), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate]]+{-.1,.1}, Appearance->"Paired", Enabled->If[showAll, False, True], ImageSize->300]
							},{
								Null,
										
								Dynamic@If[showAll,
									Button["show partial pattern",
										portionV = MinMax[#[[2]]&/@dwGuilloche[(44*sector)+(6*weave1-3), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate]]+{-.1,.1};
										portionH = MinMax[#[[1]]&/@dwGuilloche[(44*sector)+(6*weave1-3), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate]]+{-.1,.1};
										showAll = False
									],
									Button["show entire pattern",
										portionV = MinMax[#[[2]]&/@dwGuilloche[(44*sector)+(6*weave1-3), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate]]+{-.1,.1};
										portionH = MinMax[#[[1]]&/@dwGuilloche[(44*sector)+(6*weave1-3), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate]]+{-.1,.1};
										showAll = True
									]
								]
							},{""
						}}, Spacings->{0,0}]
					}}, Alignment->Center, Spacings->{0,0}]
				},{
					If[$dwGuillochePresets === {},
						(* need to fix this file path so it works anywhere *)
						$dwGuillochePresets = MapThread[Import, {Table[$dwFileDirectory<>"Media/guilloche/guilloche-"<>ToString[n]<>".png", {n, Length[presetButtonData]}]}],
						Nothing
					];
					Panel[
						Grid[{
							{"NINETY PRESETS NUMBERED FOR REFERENCE"},
							{
								Pane[
									ctr=1;
									Column[{
										Grid[Partition[
											Table[
												With[{data=pbd, dataDistort = If[MemberQ[square, pbd], 1, 0]},
													Tooltip[
														Button[$dwGuillochePresets[[ctr]], 
														{weave2, sector, amplify, weave1, dilate, loop1, loop2, loop3} = data;
														{scale, degree, showAll, distort, distortamount} = {1, 2, True, "corners", dataDistort};
														{portionV, portionH} = dwUpdatePreview[weave2, sector, amplify, weave1, dilate, loop1, loop2, loop3, scale, rotate, distort, distortamount], ImageSize->{66, 66}, $dwPresetButtonStyle],
													$dwGuillochePresets[[ctr++]]]
												],
											{pbd, presetButtonData[[;;10]]}], 
										10, 10, 1, {}], Spacings->{0,0}],
										Grid[Partition[
											Table[
												With[{data=pbd, dataDistort = If[MemberQ[square, pbd], 1, 0]},
													Tooltip[
														Button[$dwGuillochePresets[[ctr]], 
														{weave2, sector, amplify, weave1, dilate, loop1, loop2, loop3} = data;
														{scale, degree, showAll, distort, distortamount} = {1, 2, True, "corners", dataDistort};
														{portionV, portionH} = dwUpdatePreview[weave2, sector, amplify, weave1, dilate, loop1, loop2, loop3, scale, rotate, distort, distortamount], $dwPresetButtonStyle],
													$dwGuillochePresets[[ctr++]]]
												],
											{pbd, presetButtonData[[11;;-1]]}], 
										20, 20, 1, {}], Spacings->{0,0}]
									}],
								ImageSize->Full]
							}}, Alignment->Top],
						Alignment->Center, ImageSize->690]
					},{
						Grid[{{
							
							CancelButton[DialogReturn[],ImageSize->100],
							
							DefaultButton[
								(* add to canvas *)	
								temp = dwGuilloche[(44*sector)+(6*weave1-3), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate, "Distort"->distort, "DistortAmount"->distortamount];
								temp = If[Max[temp] > 1.5, -1.5+3#&/@Rescale[temp], temp];
								dwNewEmptyLayer["Head"->BSplineCurve];
								$dwStyle[[-1,Flatten[Position[$dwStyle[[-1]], StrokeForm[_]]][[1]],1,3,1]] = 0.25;
								$dwStyle[[-1,1]] = False;
								$dwStyle[[-1,10]] = True;
								$dwStyle[[-1,11]] = degree;
								$dwP[[-1]] = 
									If[showAll,
										temp,
										Table[If[portionH[[1]]<p[[1]]<portionH[[2]]&&portionV[[1]]<p[[2]]<portionV[[2]],p,Nothing],{p,temp}]
									];
								dwUpdateBoundingBox[{-1}];
								$dwPointQuantity = Length[Flatten[$dwP, 1]];
								DialogReturn[],
								ImageSize->100
							]
						}}]
					}}, Alignment->Center], 
				Alignment->Center, ImageSize->720]
		],
	Background->LightGray, WindowTitle->"Guilloche pattern", Modal->True]
	
dwUpdatePreview[weave2_, sector_, amplify_, weave1_, dilate_, loop1_, loop2_, loop3_, scale_, rotate_, distort_, distortamount_]:=
	{
		MinMax[#[[2]]&/@dwGuilloche[(44*sector)+(6*weave1-3), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate, "Distort"->distort, "DistortAmount"->distortamount]]+{-.1,.1},
		MinMax[#[[1]]&/@dwGuilloche[(44*sector)+(6*weave1-3), loop1 2^weave2, loop2 2^weave2, loop3 2^weave2, "Amplify"->amplify, "Dilate"->dilate, "Scale"->scale, "Rotate"->rotate, "Distort"->distort, "DistortAmount"->distortamount]]+{-.1,.1}
	}
	
Options[dwGuilloche] = {"Amplify"->1, "Dilate"->1, "Scale"->1, "Rotate"->0, "Distort"->None, "DistortAmount"->0};
dwGuilloche[shape_:9, loop1_:2, loop2_:4, loop3_:6, OptionsPattern[]]:=
	Block[{amplify, dilate, winding, scale, rotate, distort, amount, pts, mmx, mmy, scalex, scaley, grid, f},
		{amplify, dilate, scale, distort, amount} = {OptionValue["Amplify"], OptionValue["Dilate"], OptionValue["Scale"], OptionValue["Distort"], OptionValue["DistortAmount"]};
		winding = If[amplify == 0, .5, 1];
		rotate = RotationTransform[OptionValue["Rotate"]*Pi/180, {0,0}];
		(* points *)
		pts = rotate[(scale/6)Table[
				Max[dilate, 1] 4{Cos[loop1 t/56.5], Sin[loop1 t/56.5]} +
				-Min[dilate, -1]{Cos[loop2 t/56.5], Sin[loop2 t/56.5]} +
				-Min[dilate, -1]{Cos[loop3 t/56.5], Sin[loop3 t/56.5]} +
				Max[dilate, 1] amplify {Cos[shape t], Sin[shape t]}, 
			{t, 0, winding 226 Pi}]];
		If[distort === None || amount == 0,
			
			(* undistorted points *)
			pts,
			
			
			(* distorted points *)
			{mmx, mmy} = {MinMax[#[[1]]&/@pts], MinMax[#[[2]]&/@pts]};
			scalex = Abs[Subtract[Sequence@@ mmx]];
			scaley = Abs[Subtract[Sequence@@ mmy]];
			(* grid *)
			grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1];
			Switch[distort,
				"corners",
					grid[[1]] = grid[[1]] + Mean[{scalex, scaley}] amount {-.5, -.5};
					grid[[4]] = grid[[4]] + Mean[{scalex, scaley}] amount {-.5, .5};
					grid[[13]] = grid[[13]] + Mean[{scalex, scaley}] amount {.5, -.5};
					grid[[16]] = grid[[16]] + Mean[{scalex, scaley}] amount {.5, .5},
				"horizontal",
					grid[[1]] = grid[[1]] + Mean[{scalex, scaley}] amount {-.2, 0};
					grid[[2]] = grid[[2]] + Mean[{scalex, scaley}] amount {-.2, 0};
					grid[[3]] = grid[[3]] + Mean[{scalex, scaley}] amount {-.2, 0};
					grid[[4]] = grid[[4]] + Mean[{scalex, scaley}] amount {-.2, 0};
					grid[[13]] = grid[[13]] + Mean[{scalex, scaley}] amount {.2, 0};
					grid[[14]] = grid[[14]] + Mean[{scalex, scaley}] amount {.2, 0};
					grid[[15]] = grid[[15]] + Mean[{scalex, scaley}] amount {.2, 0};
					grid[[16]] = grid[[16]] + Mean[{scalex, scaley}] amount {.2, 0},
				"vertical",
					grid[[1]] = grid[[1]] + Mean[{scalex, scaley}] amount {0, -.2};
					grid[[5]] = grid[[5]] + Mean[{scalex, scaley}] amount {0, -.2};
					grid[[9]] = grid[[9]] + Mean[{scalex, scaley}] amount {0, -.2};
					grid[[13]] = grid[[13]] + Mean[{scalex, scaley}] amount {0, -.2};
					grid[[4]] = grid[[4]] + Mean[{scalex, scaley}] amount {0, .2};
					grid[[8]] = grid[[8]] + Mean[{scalex, scaley}] amount {0, .2};
					grid[[12]] = grid[[12]] + Mean[{scalex, scaley}] amount {0, .2};
					grid[[16]] = grid[[16]] + Mean[{scalex, scaley}] amount {0, .2},
				_,
					(* spherize *)
					grid[[6]] = grid[[6]] + Mean[{scalex, scaley}] amount {-.1, -.1};
					grid[[7]] = grid[[7]] + Mean[{scalex, scaley}] amount {-.1, .1};
					grid[[10]] = grid[[10]] + Mean[{scalex, scaley}] amount {.1, -.1};
					grid[[11]] = grid[[11]] + Mean[{scalex, scaley}] amount {.1, .1}
			];
			f = Interpolation[Flatten[Table[{{ N[scalex/3] i, N[scaley/3] j} + {0,0} - .5 {scalex, scaley}, grid[[4 i + j + 1]]}, {i, 0, 3}, {j, 0, 3}], 1], Method -> "Spline", InterpolationOrder -> 2];
			Table[f[Sequence@@p], {p, pts}]
		]
	]

End[] (* End Private Context *)

EndPackage[]