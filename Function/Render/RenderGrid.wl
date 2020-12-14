(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderGrid[step_]:=
	Block[{r, temp},
		r = RotationTransform[$dwConstrainHAngle \[Degree]];
		Flatten[{AbsoluteThickness[1/$dwZoom],
				(* grid *)
				If[$dwShowGrid && $dwGridStep*$dwZoom >= .05,
					If[$dwConstrainHAngle >= 0,
						
						Nothing
						(* 2D *)
						(*temp = .5$dwGrid2DRange step;*)
						(*Flatten[{
							GrayLevel[.9], 
							If[$dwConstrainHAngle == 0,
								{Table[Line[{{n, -temp}, {n, temp}}], {n,-temp, temp, step $dwGridStep}],
								Table[Line[{{-temp,n}, {temp,n}}], {n,-temp, temp, step $dwGridStep}]},
								{Table[Line[r[{{n,-temp}, {n,temp}}]], {n,-temp, temp, step $dwGridStep}],
								Table[Line[r[{{-temp,n}, {temp,n}}]], {n,-temp, temp, step $dwGridStep}]}
							]
						}]*)
						
						(* 2D dot grid *)
						(*Flatten[{
							GrayLevel[0], Dashing[{0, step*$dwGridStep}],
							
							If[$dwConstrainHAngle == 0,
								{Table[Line[{{-temp,n}, {temp,n}}], {n,-temp, temp, step $dwGridStep}]},
								
								{Table[Line[r[{{n,-temp}, {n,temp}}]], {n,-temp, temp, step $dwGridStep}]}
							]
						}]*),
						
						(* 3D *)
						temp = ($dwGrid3DRange/$dwGridStep)*$dwGridSize;
						{
							GrayLevel[.9],
							Table[{
								Line[{-temp[[1]]+n*$dwGridSize[[2]],temp[[1]]+n*$dwGridSize[[2]]}],
								Line[{-temp[[2]]+n*$dwGridSize[[1]],temp[[2]]+n*$dwGridSize[[1]]}]
								},
							{n, -($dwGrid3DRange/$dwGridStep), ($dwGrid3DRange/$dwGridStep), step}]
						}
					]
				,{}],
				(* axis *)
				If[$dwShowAxes,
					If[$dwConstrainHAngle >= 0,
						(* 2D *)
						{
							Opacity[1, Darker[$dwCanvasBackgroundColor,.15]], Dashing[{}],
							Sequence@@If[$dwConstrainHAngle == 0, 
								{InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}]},
								Nothing
							],
							(*If[$dwConstrainHAngle == 0, 
								Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}],
								Line[r[{{-1,-1},{1,-1},{1,1},{-1,1},{-1,-1}}]]
							],*)
							(* constrain *)
							InfiniteLine[{{0, 0}, {Cos[$dwConstrainHAngle/180*Pi], Sin[$dwConstrainHAngle/180*Pi]}}],
							InfiniteLine[{{0, 0}, {Cos[($dwConstrainVAngle+90)/180*Pi], Sin[($dwConstrainVAngle+90)/180*Pi]}}],
							(* text *)
							Opacity[1, Darker[$dwCanvasBackgroundColor, .3]],
							Sequence@@If[$dwConstrainHAngle == 0,
								{}
								(*{Text[Style["{1, 1}", FontSize->12/$dwZoom], {1,1}, {-1,-1}],
								Text[Style["{1, -1}", FontSize->12/$dwZoom], {1,-1}, {-1,1}],
								Text[Style["{-1, 1}", FontSize->12/$dwZoom], {-1,1}, {1,-1}],
								Text[Style["{-1, -1}", FontSize->12/$dwZoom], {-1,-1}, {1,1}]}*),
								{Text[Style["{1, 1}", FontSize->12/$dwZoom], r[{1,1}], {-1,-1}],
								Text[Style["{1, -1}", FontSize->12/$dwZoom], r[{1,-1}], {-1,1}],
								Text[Style["{-1, 1}", FontSize->12/$dwZoom], r[{-1,1}], {1,-1}],
								Text[Style["{-1, -1}", FontSize->12/$dwZoom],r[ {-1,-1}], {1,1}]}
							]
						},
						(* 3D *)
						{
							Opacity[1, Darker[$dwCanvasBackgroundColor,.15]], 
							InfiniteLine[{{0, 0}, {Cos[$dwConstrainHAngle/180*Pi], Sin[$dwConstrainHAngle/180*Pi]}}], 
							InfiniteLine[{{0, 0}, {Cos[($dwConstrainVAngle+90)/180*Pi], Sin[($dwConstrainVAngle+90)/180*Pi]}}],
							Line[dwAxo[{{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							(* text *)
							Opacity[1, Darker[$dwCanvasBackgroundColor, .3]],
							Text["{1, 1}", dwAxo[{{{1, 1}, {1, 1}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1,1]], {-1,-1}],
							Text["{1, -1}", dwAxo[{{{1, -1}, {1, -1}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1,1]], {-1,1}],
							Text["{-1, 1}", dwAxo[{{{-1, 1}, {-1, 1}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1,1]], {1,-1}],
							Text["{-1, -1}", dwAxo[{{{-1, -1}, {-1, -1}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1,1]], {1,1}]
						}
					],
					{}
				]
			}]
	]

End[] (* End Private Context *)

EndPackage[]