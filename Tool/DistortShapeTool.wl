(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwDistortShape[]:=
	CreateDialog[
		DynamicModule[{distort = "spherize", pts, grid, gridStart, f, mmx, mmy, scalex, scaley, pCtr, amount = 0, sel, 
			distortObjPos = False, objCtr, newCtr, rotate},
			Pane[
				Column[{
					sel = If[Length[$dwP[[#]]] < 2, Nothing, #]&/@$dwSelected;
					$dwSelected = sel;
					pts = If[sel === {}, Table[Join[n#&/@CirclePoints[50], {n{Sin[Pi/50],-Cos[Pi/50]}}], {n, {.1, .5, 1}}], $dwP[[sel]]];
					pCtr = dwFindCenter[Flatten[pts, 1]];
					{mmx, mmy} = {MinMax[#[[1]]&/@Flatten[pts, 1]], MinMax[#[[2]] & /@ Flatten[pts, 1]]};
					scalex = Abs[Subtract[Sequence@@ mmx]];
					scaley = Abs[Subtract[Sequence@@ mmy]];
					grid = gridStart = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1];
					LocatorPane[Dynamic[grid],
						Dynamic@Graphics[{
							AbsoluteThickness[.25],
							(* grid *)
							Switch[distort,
								"corners",
									grid[[1]] = gridStart[[1]] + Mean[{scalex, scaley}] amount {-.5, -.5};
									grid[[4]] = gridStart[[4]] + Mean[{scalex, scaley}] amount {-.5, .5};
									grid[[13]] = gridStart[[13]] + Mean[{scalex, scaley}] amount {.5, -.5};
									grid[[16]] = gridStart[[16]] + Mean[{scalex, scaley}] amount {.5, .5},
								"innertwist",
									rotate = RotationTransform[amount, Mean[gridStart[[{6,11}]]]];
									grid[[6]] = rotate[gridStart[[6]]];
									grid[[7]] = rotate[gridStart[[7]]];
									grid[[10]] = rotate[gridStart[[10]]];
									grid[[11]] = rotate[gridStart[[11]]],
								"vertical",
									grid[[1]] = gridStart[[1]] + Mean[{scalex, scaley}] amount {0, -.2};
									grid[[5]] = gridStart[[5]] + Mean[{scalex, scaley}] amount {0, -.2};
									grid[[9]] = gridStart[[9]] + Mean[{scalex, scaley}] amount {0, -.2};
									grid[[13]] = gridStart[[13]] + Mean[{scalex, scaley}] amount {0, -.2};
									grid[[4]] = gridStart[[4]] + Mean[{scalex, scaley}] amount {0, .2};
									grid[[8]] = gridStart[[8]] + Mean[{scalex, scaley}] amount {0, .2};
									grid[[12]] = gridStart[[12]] + Mean[{scalex, scaley}] amount {0, .2};
									grid[[16]] = gridStart[[16]] + Mean[{scalex, scaley}] amount {0, .2},
								"left",
									grid[[1]] = gridStart[[1]] + Mean[{scalex, scaley}] amount {-1, 0};
									grid[[2]] = gridStart[[2]] + Mean[{scalex, scaley}] amount {-1, 0};
									grid[[3]] = gridStart[[3]] + Mean[{scalex, scaley}] amount {-1, 0};
									grid[[4]] = gridStart[[4]] + Mean[{scalex, scaley}] amount {-1, 0};
									grid[[5]] = gridStart[[5]] + Mean[{scalex, scaley}] amount {-.4, 0};
									grid[[6]] = gridStart[[6]] + Mean[{scalex, scaley}] amount {-.4, 0};
									grid[[7]] = gridStart[[7]] + Mean[{scalex, scaley}] amount {-.4, 0};
									grid[[8]] = gridStart[[8]] + Mean[{scalex, scaley}] amount {-.4, 0};
									grid[[9]] = gridStart[[9]] + Mean[{scalex, scaley}] amount {-.1, 0};
									grid[[10]] = gridStart[[10]] + Mean[{scalex, scaley}] amount {-.1, 0};
									grid[[11]] = gridStart[[11]] + Mean[{scalex, scaley}] amount {-.1, 0};
									grid[[12]] = gridStart[[12]] + Mean[{scalex, scaley}] amount {-.1, 0},
								"right",
									grid[[5]] = gridStart[[5]] + Mean[{scalex, scaley}] amount {.1, 0};
									grid[[6]] = gridStart[[6]] + Mean[{scalex, scaley}] amount {.1, 0};
									grid[[7]] = gridStart[[7]] + Mean[{scalex, scaley}] amount {.1, 0};
									grid[[8]] = gridStart[[8]] + Mean[{scalex, scaley}] amount {.1, 0};
									grid[[9]] = gridStart[[9]] + Mean[{scalex, scaley}] amount {.4, 0};
									grid[[10]] = gridStart[[10]] + Mean[{scalex, scaley}] amount {.4, 0};
									grid[[11]] = gridStart[[11]] + Mean[{scalex, scaley}] amount {.4, 0};
									grid[[12]] = gridStart[[12]] + Mean[{scalex, scaley}] amount {.4, 0};
									grid[[13]] = gridStart[[13]] + Mean[{scalex, scaley}] amount {1, 0};
									grid[[14]] = gridStart[[14]] + Mean[{scalex, scaley}] amount {1, 0};
									grid[[15]] = gridStart[[15]] + Mean[{scalex, scaley}] amount {1, 0};
									grid[[16]] = gridStart[[16]] + Mean[{scalex, scaley}] amount {1, 0},
								"bottom",
									grid[[1]] = gridStart[[1]] + Mean[{scalex, scaley}] amount {0, -1};
									grid[[5]] = gridStart[[5]] + Mean[{scalex, scaley}] amount {0, -1};
									grid[[9]] = gridStart[[9]] + Mean[{scalex, scaley}] amount {0, -1};
									grid[[13]] = gridStart[[13]] + Mean[{scalex, scaley}] amount {0, -1};
									grid[[2]] = gridStart[[2]] + Mean[{scalex, scaley}] amount {0, -.4};
									grid[[6]] = gridStart[[6]] + Mean[{scalex, scaley}] amount {0, -.4};
									grid[[10]] = gridStart[[10]] + Mean[{scalex, scaley}] amount {0, -.4};
									grid[[14]] = gridStart[[14]] + Mean[{scalex, scaley}] amount {0, -.4};
									grid[[3]] = gridStart[[3]] + Mean[{scalex, scaley}] amount {0, -.1};
									grid[[7]] = gridStart[[7]] + Mean[{scalex, scaley}] amount {0, -.1};
									grid[[11]] = gridStart[[11]] + Mean[{scalex, scaley}] amount {0, -.1};
									grid[[15]] = gridStart[[15]] + Mean[{scalex, scaley}] amount {0, -.1},
								"top",
									grid[[2]] = gridStart[[2]] + Mean[{scalex, scaley}] amount {0, .1};
									grid[[6]] = gridStart[[6]] + Mean[{scalex, scaley}] amount {0, .1};
									grid[[10]] = gridStart[[10]] + Mean[{scalex, scaley}] amount {0, .1};
									grid[[14]] = gridStart[[14]] + Mean[{scalex, scaley}] amount {0, .1};
									grid[[3]] = gridStart[[3]] + Mean[{scalex, scaley}] amount {0, .4};
									grid[[7]] = gridStart[[7]] + Mean[{scalex, scaley}] amount {0, .4};
									grid[[11]] = gridStart[[11]] + Mean[{scalex, scaley}] amount {0, .4};
									grid[[15]] = gridStart[[15]] + Mean[{scalex, scaley}] amount {0, .4};
									grid[[4]] = gridStart[[4]] + Mean[{scalex, scaley}] amount {0, 1};
									grid[[8]] = gridStart[[8]] + Mean[{scalex, scaley}] amount {0, 1};
									grid[[12]] = gridStart[[12]] + Mean[{scalex, scaley}] amount {0, 1};
									grid[[16]] = gridStart[[16]] + Mean[{scalex, scaley}] amount {0, 1},
								"spherize",
									grid[[6]] = gridStart[[6]] + Mean[{scalex, scaley}] amount {-.1, -.1};
									grid[[7]] = gridStart[[7]] + Mean[{scalex, scaley}] amount {-.1, .1};
									grid[[10]] = gridStart[[10]] + Mean[{scalex, scaley}] amount {.1, -.1};
									grid[[11]] = gridStart[[11]] + Mean[{scalex, scaley}] amount {.1, .1},
								"outertwist",
									rotate = RotationTransform[amount, Mean[gridStart[[{6,11}]]]];
									grid[[1]] = rotate[gridStart[[1]]];
									grid[[2]] = rotate[gridStart[[2]]];
									grid[[3]] = rotate[gridStart[[3]]];
									grid[[4]] = rotate[gridStart[[4]]];
									grid[[5]] = rotate[gridStart[[5]]];
									grid[[8]] = rotate[gridStart[[8]]];
									grid[[9]] = rotate[gridStart[[9]]];
									grid[[12]] = rotate[gridStart[[12]]];
									grid[[13]] = rotate[gridStart[[13]]];
									grid[[14]] = rotate[gridStart[[14]]];
									grid[[15]] = rotate[gridStart[[15]]];
									grid[[16]] = rotate[gridStart[[16]]],
								"horizontal",
									grid[[1]] = gridStart[[1]] + Mean[{scalex, scaley}] amount {-.2, 0};
									grid[[2]] = gridStart[[2]] + Mean[{scalex, scaley}] amount {-.2, 0};
									grid[[3]] = gridStart[[3]] + Mean[{scalex, scaley}] amount {-.2, 0};
									grid[[4]] = gridStart[[4]] + Mean[{scalex, scaley}] amount {-.2, 0};
									grid[[13]] = gridStart[[13]] + Mean[{scalex, scaley}] amount {.2, 0};
									grid[[14]] = gridStart[[14]] + Mean[{scalex, scaley}] amount {.2, 0};
									grid[[15]] = gridStart[[15]] + Mean[{scalex, scaley}] amount {.2, 0};
									grid[[16]] = gridStart[[16]] + Mean[{scalex, scaley}] amount {.2, 0},
								_,
									Nothing
							];
							(* distortion function *)
							f = Interpolation[Flatten[Table[{{ N[scalex/3] i, N[scaley/3] j} + pCtr - .5 {scalex, scaley}, grid[[4 i + j + 1]]}, {i, 0, 3}, {j, 0, 3}], 1], Method -> "Spline", InterpolationOrder -> 2];
							(* shapes *)
							If[sel === {},
								Table[Line[Table[f[Sequence@@pts[[n, pn]]], {pn, Length[pts[[n]]]}]], {n, Length[pts]}],
								Table[
									Switch[$dwHead[[sel[[n]]]],
										BezierCurve,
											If[distortObjPos,
												objCtr = dwFindCenter[pts[[n]]];
												newCtr = f[Sequence@@objCtr];
												BezierCurve[# + (newCtr - objCtr)&/@Most[pts[[n]]]],
												BezierCurve[Table[f[Sequence@@pts[[n, pn]]], {pn, Length[pts[[n]]] - 1}]]
											],
										BSplineCurve,
											If[distortObjPos,
												objCtr = dwFindCenter[pts[[n]]];
												newCtr = f[Sequence@@objCtr];
												BSplineCurve[# + (newCtr - objCtr)&/@pts[[n]], SplineClosed->$dwStyle[[sel[[n]],10]], SplineDegree->$dwStyle[[sel[[n]],11]]],
												BSplineCurve[Table[f[Sequence@@pts[[n, pn]]], {pn, Length[pts[[n]]]}], SplineClosed->$dwStyle[[sel[[n]],10]], SplineDegree->$dwStyle[[sel[[n]],11]]]
											],
										Polygon,
											If[distortObjPos,
												objCtr = dwFindCenter[pts[[n]]];
												newCtr = f[Sequence@@objCtr];
												Line[Join[# + (newCtr - objCtr)&/@pts[[n]], {pts[[n, 1]] + (newCtr - objCtr)}]],
												Line[Join[Table[f[Sequence@@pts[[n, pn]]], {pn, Length[pts[[n]]]}], {f[Sequence@@pts[[n, 1]]]}]]
											],
										Line|Point|Arrow,
											If[distortObjPos,
												objCtr = dwFindCenter[pts[[n]]];
												newCtr = f[Sequence@@objCtr];
												$dwHead[[sel[[n]]]][# + (newCtr - objCtr)&/@pts[[n]]],
												$dwHead[[sel[[n]]]][Table[f[Sequence@@pts[[n, pn]]], {pn, Length[pts[[n]]]}]]
											],
										_,
											{}
									], 
								{n, Length[pts]}]
							],
							(* mesh *)
							If[MemberQ[{"mesh"}, distort],
								{Red, Line[grid[[{1,2,3,4,8,12,16,15,14,13,9,5,1}]]], Line[grid[[{3,7,11,15}]]], Line[grid[[{2,6,10,14}]]], Line[grid[[{5,6,7,8}]]], Line[grid[[{9,10,11,12}]]]},
								{}
							]
					}, Background->White, ImageSize->{200,200}],
					(* {{xmin, ymin}, {xmax, ymax}, {xsnap, ysnap}} *)
					{{mmx[[1]], mmy[[1]]}, {mmx[[2]], mmy[[2]]}, {scalex/18, scaley/18}},
				TrackedSymbols:>{distort}, Appearance->Dynamic@If[MemberQ[{"mesh"}, distort], Graphics[{Red, PointSize[Medium], Point[{0,0}]}, ImageSize->12], Graphics[{}, ImageSize->1]], PlotRange->2],
					"",
					Row@{Pane["amount ", ImageSize->50, Alignment->Right], Slider[Dynamic@amount, {-1, 1, .05}, Enabled->Dynamic@If[MemberQ[{"mesh"}, distort], False, True]], Spacer[5], Pane[Dynamic@amount, ImageSize->30, Alignment->Left]},
					Row@{ActionMenu[Dynamic@distort, {
						"mesh":>(distort = "mesh"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"spherize":>(distort = "spherize"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"corners":>(distort = "corners"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"innertwist":>(distort = "innertwist"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"outertwist":>(distort = "outertwist"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"horizontal":>(distort = "horizontal"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"vertical":>(distort = "vertical"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"left":>(distort = "left"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"right":>(distort = "right"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"bottom":>(distort = "bottom"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0),
						"top":>(distort = "top"; grid = Flatten[Table[{i, j}, {i, mmx[[1]], mmx[[2]], scalex/3}, {j, mmy[[1]], mmy[[2]], scaley/3}], 1]; amount = 0)
						}], Spacer[5], Checkbox[Dynamic@distortObjPos], "distort object position"},
					"",
					Style["Select objects before opening.", Italic, 15],
					Style["Increase number of object points for smoother results.", Italic],
					Style["Works best when selections have combined aspect ratio of 1.", Italic],
					Style["Does not work with Image or text.", Italic],
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
							dwSetUndo[];
							(* update points *)
							If[$dwSelected === {},
								Do[
									dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
									objCtr = dwFindCenter[pts[[n]]];
									newCtr = f[Sequence@@objCtr];
									$dwP[[-1]] = 
										If[distortObjPos,
											objCtr = dwFindCenter[pts[[n]]];
											newCtr = f[Sequence@@objCtr];
											# + (newCtr - objCtr)&/@pts[[n]],
											Table[f[Sequence@@pts[[n, pn]]], {pn, Length[pts[[n]]]}]
										],
								{n, Length[pts]}],
								Do[
									$dwP[[sel[[n]]]] = 
										If[distortObjPos,
											objCtr = dwFindCenter[pts[[n]]];
											newCtr = f[Sequence@@objCtr];
											# + (newCtr - objCtr)&/@pts[[n]],
											Table[f[Sequence@@pts[[n, pn]]], {pn, Length[pts[[n]]]}]
										], 
								{n, Length[pts]}]
							];
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							dwUpdateAllBoundingBoxes[];
							$dwStyleMode = "fill";
							DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Spherize", Modal->True]

End[] (* End Private Context *)

EndPackage[]