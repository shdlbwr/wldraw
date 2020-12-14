(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeRoughenSelection[]:=
	CreateDialog[
		DynamicModule[{img, img2, reg, shape = .25, detail = .5, smooth = .25, selected, center, pCenter, pts, size, length = Length[$dwP]},
			$dwSelected = If[MemberQ[$dwShapeSymbols, $dwHead[[#]]], #, Nothing]&/@$dwSelected;
			selected = If[$dwSelected === {}, {0}, $dwSelected];
			center = If[$dwSelected === {}, {0,0}, dwFindCenter[Flatten[$dwP[[$dwSelected]], 1]]];
			Pane[
				Grid[{
					{"Select path to roughen before opening dialog.", SpanFromLeft, SpanFromLeft},
					{
						Dynamic[
							img = 
								If[$dwSelected =!= {}, 
									Rasterize[
										Graphics[
											Table[
												If[MemberQ[{BezierCurve, BSplineCurve}, Head[obj]], 
													FilledCurve[obj], 
													Polygon@@obj
												], 
												{obj, Flatten[dwRenderWireframeObjectForMenu[#]&/@$dwSelected]}
											], PlotRangePadding -> .1
										], 
										Background -> White, ImageResolution -> (24 + shape(72-24))
									],
									Rasterize[Graphics[Disk[{0,0},.2], PlotRangePadding -> .1], Background -> White, ImageResolution -> (24 + shape(72-24))]
								];
							img2 = Blur[MedianFilter[ImageEffect[img, {"Jitter", 10detail}, RandomSeeding -> 1], 2], 10smooth];
							reg = ImageMesh[ColorNegate[Binarize[img2]], ImageSize->{280, 280}];
							If[Head[reg] === BoundaryMeshRegion, reg, Style["\nSelect nonlinear path of 3 or more points\n", Italic]]
						], SpanFromLeft, SpanFromLeft},
						
					{"shape ", Slider[Dynamic@shape,{0, 1}, ContinuousAction->False], Pane[Dynamic@shape,ImageSize->30]},
					{"detail ", Slider[Dynamic@detail,{0, 1}, ContinuousAction->False], Pane[Dynamic@detail,ImageSize->30]},
					{"smooth ", Slider[Dynamic@smooth,{0, 1}, ContinuousAction->False], Pane[Dynamic@smooth,ImageSize->30]},
					
					{Row[{
						CancelButton[DialogReturn[]],
						(* add to canvas *)	
						DefaultButton[
							If[Head[reg] === BoundaryMeshRegion,
									dwSetUndo[];
									size = If[selected[[1]] == 0, 
											.005,
											EuclideanDistance[Min[#[[1]]&/@Flatten[$dwP[[selected]], 1]], Max[#[[1]]&/@Flatten[$dwP[[selected]], 1]]]/(Max[#[[1]]&/@MeshCoordinates[reg]])
										];
									pCenter = dwFindCenter[MeshCoordinates[reg]];
									Do[
										dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
										$dwStyle[[-1]] = If[selected[[1]] == 0, $dwFullDefaultStyle, $dwStyle[[selected[[1]]]]];
										Switch[Head[obj],
											FilledCurve,
												pts = MeshCoordinates[reg][[#[[1]]]]&/@Cases[obj, _Line, Infinity];
												pts = size*pts[[Reverse[Sort[Table[{Length[pts[[n]]], n}, {n, Length[pts]}]]][[1, 2]]]],
											Polygon,
												pts = size*(MeshCoordinates[reg][[obj[[1]]]]),
											_,
												pts = {}
										];
										$dwP[[-1]] = # + (center - size*pCenter)&/@pts,
										{obj, Join[Cases[Show[reg], _FilledCurve, Infinity, Heads -> True], Flatten[Table[Polygon[p], {p, #[[1]]}]&/@Cases[Show[reg], _Polygon, Infinity, Heads -> True]]]}
									];
									$dwSelected = Range[length + 1, Length[$dwP]];
									dwUpdateBoundingBox[$dwSelected];
									If[Length[$dwP] - length > 1,
										$dwGroupLayers = Join[$dwGroupLayers, {$dwSelected}],
										Nothing
									];
									$dwPointQuantity = Length[Flatten[$dwP, 1]],
								Nothing
							];
							DialogReturn[]
						]}],SpanFromLeft,SpanFromLeft}
					}, Alignment->Center],
			ImageSize->280]
		], Background->LightGray, WindowTitle->"Roughen path"
	]

End[] (* End Private Context *)

EndPackage[]