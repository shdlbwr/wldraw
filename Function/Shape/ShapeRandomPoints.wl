(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeRandomPoints[]:=
	CreateDialog[
		DynamicModule[{quantity = 10, seed = 1, shapePts, pts, f, alignment = 0, shapePtsMinMaxX, shapePtsMinMaxY, boolPts, arraypts, shapeSize, finalPts},
			shapePts = 
				If[$dwSelected =!= {}, 
					Switch[$dwHead[[$dwSelected[[1]]]],
						BezierCurve,
							If[Length[$dwP[[$dwSelected[[1]]]]] > 4,
								pts = dwBezierDiscretizeList[$dwP[[$dwSelected[[1]]]], $dwDiscretizeResolution/3];
								dwSimplifyPts[pts, 2],
								$dwP[[$dwSelected[[1]]]]
							],
						BSplineCurve,
							If[Length[$dwP[[$dwSelected[[1]]]]] > 2,
								f = BSplineFunction[$dwP[[$dwSelected[[1]]]], SplineClosed->If[$dwSelected[[1]]==0||$dwStyle[[$dwSelected[[1]],10]],True,False], SplineDegree->If[$dwSelected[[1]]==0,$dwDefaultSplineDegree,$dwStyle[[$dwSelected[[1]],11]]]];
								pts = Table[f[pn], {pn, 0, 1, 1/$dwDiscretizeResolution}];
								dwSimplifyPts[pts, 2],
								$dwP[[$dwSelected[[1]]]]
							],
						Point|Line|Arrow|Polygon, 
							$dwP[[$dwSelected[[1]]]], 
						_, 
							CirclePoints[10]
					],
					
					CirclePoints[10]
				];
			Pane[
				Grid[{
					{"Select object to fill with points before opening dialog.", SpanFromLeft, SpanFromLeft},
					{Dynamic@Show[
						
						If[$dwSelected =!= {},
							Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframeObjectForMenu[$dwSelected[[1]]]]],
							{}
						],
						
						Graphics[{
							(* axes and frame *)
							{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
							(* box *)
							Sequence@@($dwFullDefaultStyle[[$dwStyleStart;;-1]]/.{AbsolutePointSize[ps_]->AbsolutePointSize[.5ps]}),
							SeedRandom[seed];
							If[alignment == 0,
								
								finalPts = RandomPoint[Polygon[shapePts], quantity];
								Point[finalPts],
								
								shapePtsMinMaxX = MinMax[#[[1]]&/@shapePts];
								shapePtsMinMaxY = MinMax[#[[2]]&/@shapePts];
								shapeSize = {Subtract[shapePtsMinMaxX[[2]], shapePtsMinMaxX[[1]]], Subtract[shapePtsMinMaxY[[2]], shapePtsMinMaxY[[1]]]};
								arraypts = Flatten[Table[{x, y}, 
										{x, shapePtsMinMaxX[[1]], shapePtsMinMaxX[[2]], EuclideanDistance[{shapePtsMinMaxX[[1]], shapePtsMinMaxY[[1]]}, {shapePtsMinMaxX[[2]], shapePtsMinMaxY[[2]]}]/Sqrt[quantity]}, 
										{y, shapePtsMinMaxY[[1]], shapePtsMinMaxY[[2]], EuclideanDistance[{shapePtsMinMaxX[[1]], shapePtsMinMaxY[[1]]}, {shapePtsMinMaxX[[2]], shapePtsMinMaxY[[2]]}]/Sqrt[quantity]}
									], 1];
								boolPts = RegionMember[Polygon[shapePts], arraypts];
								finalPts = Table[If[boolPts[[n]], arraypts[[n]] + .5RandomReal[{-(1-alignment), (1-alignment)}, 2], Nothing], {n, Length[boolPts]}];
								Point[finalPts]
							]
							}], 
							
						Background->White, ImageSize->{200,200}, PlotRange->1.1], SpanFromLeft, SpanFromLeft},
						
					{"quantity ", Slider[Dynamic@quantity,{1, 500, 1}], Pane[Dynamic@quantity, ImageSize->30]},
					{"seed ", Slider[Dynamic@seed,{1, 100, 1}], Pane[Dynamic@seed, ImageSize->30]},
					{"alignment ", Slider[Dynamic@alignment, {0, 1, .01}], Pane[Dynamic@alignment, ImageSize->30, ContinuousAction->False]},
					
					{Row[{CancelButton[DialogReturn[]],
						DefaultButton[
							dwNewEmptyLayer["Head"->Point];
							$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
							$dwP[[$dwSelected[[1]]]] = finalPts;
							dwUpdateBoundingBox[$dwSelected[[{1}]]];
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							DialogReturn[]
						]}],SpanFromLeft,SpanFromLeft}
					}, Alignment->Center],
			ImageSize->300]
		], Background->LightGray, WindowTitle->"Random points"
	]

End[] (* End Private Context *)

EndPackage[]