(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
	
Options[dwExpressionBox]:= {"Rotate"->True};

dwExpressionBox[n_, OptionsPattern[]]:=
	Block[{reduce = 1/$dwOverallSize, size = Rasterize[Magnify[$dwStyle[[n,2]], $dwStyle[[n,3]]], "RasterSize", ImageResolution->$dwImageResolution, ImageFormattingWidth->Infinity]},
		{$dwP[[n,1]] - reduce .5size, $dwP[[n,1]] + reduce .5size}
	]
	
dwRenderExpression[n_]:=
	Block[{plotrange=RegionBounds[Point[$dwBoundingBoxes[[n]]]]},
		Inset[
			Graphics[
				(*Rotate[*)
					Inset[
						$dwStyle[[n,2]], 
						$dwP[[n,1]],
						Automatic,
						$dwStyle[[n,3]]*(ImageDimensions[Rasterize[$dwStyle[[n, 2]], ImageResolution->$dwImageResolution]][[1]]/$dwImageResAdjustment)
					], 
					(*$dwStyle[[n,1]]
				],*)
				PlotRangePadding->None, 
				PlotRange->plotrange
			], 
			$dwP[[n,1]], 
			Automatic,
			ManhattanDistance[Sequence@@plotrange[[1]]]
		]
	]
	
dwReplaceExpression[sel_]:=
	CreateDialog[
		DynamicModule[{originalObj = $dwStyle[[sel]]},
			Pane[
				Dynamic@Column[{
					Grid[{
						{
							Column[{
								InputField[Dynamic[$dwStyle[[sel, 2]]], ImageSize->{90, 90}],
								Pane[Style["Delete above and paste new image or graphics expression", 10, Italic, LineIndent->0], ImageSize->90]
							}], 
							Spacer[20],
							Pane[
								Dynamic@Show[
									If[MemberQ[Options[Head[$dwStyle[[sel, 2]]]], ImageSize->___],
										 
										$dwStyle[[sel, 2]],
										
										$dwStyle[[sel]] = $dwDefaultExpressionStyle;
										$dwDefaultExpressionStyle[[2]]
									], 
									ImageSize->{470,300}],
							ImageSize->{470,300}]
						}
					}, Alignment->{Left,Top}, Background->{None, None}, Spacings->{0,0}],
					Grid[{
						{""},
						{Style["RESIZE", Bold, 15]},
						{Row[{
							Button["25%",$dwStyle[[sel,3]] = .25; dwUpdateBoundingBox[{sel}]], 
							Button["50%",$dwStyle[[sel,3]] = .5; dwUpdateBoundingBox[{sel}]], 
							Button["75%",$dwStyle[[sel,3]] = .75; dwUpdateBoundingBox[{sel}]],
							Button["100%",$dwStyle[[sel,3]] = 1; dwUpdateBoundingBox[{sel}]], 
							Button["125%",$dwStyle[[sel,3]] = 1.25; dwUpdateBoundingBox[{sel}]], 
							Button["150%",$dwStyle[[sel,3]] = 1.5; dwUpdateBoundingBox[{sel}]], 
							Button["175%",$dwStyle[[sel,3]] = 1.75; dwUpdateBoundingBox[{sel}]], 
							Button["200%",$dwStyle[[sel,3]] = 2; dwUpdateBoundingBox[{sel}]]
						}]}
					}, Spacings->{0,0}],
					Row[{
						Button["Cancel", 
							$dwStyle[[sel]] = originalObj; 
							dwUpdateBoundingBox[{sel}]; 
							DialogReturn[]
						],
						DefaultButton[
							If[$dwStyle[[sel, 2]] === Null,
								
								$dwStyle[[sel]] = $dwDefaultExpressionStyle,
								
								Nothing
							];
							dwUpdateBoundingBox[{sel}];
							DialogReturn[]
						]
					}]
				},Alignment->Center],
			ImageSize->600]
		],
	Background->LightGray, WindowTitle->"Expression", Modal->True]

End[] (* End Private Context *)

EndPackage[]