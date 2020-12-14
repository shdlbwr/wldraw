(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeSpiral[]:=
	CreateDialog[
		DynamicModule[{turns = 4, spread = 1, plotPts = 40, scale = .2, finalPts, finalPtsScale},
			Pane[
				Dynamic@Column[{
					finalPts = ParametricPlot[{(u^spread Pi)Cos[u],(u^spread Pi) Sin[u]},{u,.01,turns Pi},PlotPoints->plotPts,MaxRecursion->0,DisplayFunction->(Cases[#[[1]],_Line,Infinity][[1,1]]&)];
					finalPtsScale = 2/(Max[First /@ finalPts] - Min[First /@ finalPts]);
					Dynamic@Show[Graphics[{
							(* axes and frame *)
							{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
							(* spiral *)
							Gray,ParametricPlot[scale*finalPtsScale*{(u^spread Pi)Cos[u],(u^spread Pi) Sin[u]},{u,.01,turns Pi},
							PlotPoints->plotPts,MaxRecursion->0,DisplayFunction->(Cases[#[[1]],_Line,Infinity][[1]]&)]
							},Background->White,ImageSize->{200,200}],
						Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]], PlotRange->1.1],
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}, ContinuousAction->False],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row@{Pane["turns ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@turns,{1,20,1}, ContinuousAction->False],Spacer[5],Pane[Dynamic@turns,ImageSize->30,Alignment->Left]},
					Row@{Pane["spread ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@spread,{0,3,.01}, ContinuousAction->False],Spacer[5],Pane[Dynamic@spread,ImageSize->30,Alignment->Left]},
					Row@{Pane["points ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@plotPts,{5,200,1}, ContinuousAction->False],Spacer[5],Pane[Dynamic@plotPts,ImageSize->30,Alignment->Left]},
					Grid[{{
						Button[Graphics[{ParametricPlot[{(u^1 Pi)Cos[u],(u^1 Pi) Sin[u]},{u,.01,2 Pi},DisplayFunction->(Cases[#[[1]],_Line,Infinity][[1]]&)]}, ImageSize->27],turns=2;spread=1,$dwPresetButtonStyle],
						Button[Graphics[{ParametricPlot[{(u^1 Pi)Cos[u],(u^1 Pi) Sin[u]},{u,.01,4 Pi},DisplayFunction->(Cases[#[[1]],_Line,Infinity][[1]]&)]}, ImageSize->27],turns=4;spread=1,$dwPresetButtonStyle],
						Button[Graphics[{ParametricPlot[{(u^1 Pi)Cos[u],(u^1 Pi) Sin[u]},{u,.01,6 Pi},DisplayFunction->(Cases[#[[1]],_Line,Infinity][[1]]&)]}, ImageSize->27],turns=6;spread=1,$dwPresetButtonStyle],
						Button[Graphics[{ParametricPlot[{(u^2 Pi)Cos[u],(u^2 Pi) Sin[u]},{u,.01,2 Pi},DisplayFunction->(Cases[#[[1]],_Line,Infinity][[1]]&)]}, ImageSize->27],turns=2;spread=2,$dwPresetButtonStyle],
						Button[Graphics[{ParametricPlot[{(u^2 Pi)Cos[u],(u^2 Pi) Sin[u]},{u,.01,4 Pi},DisplayFunction->(Cases[#[[1]],_Line,Infinity][[1]]&)]}, ImageSize->27],turns=4;spread=2,$dwPresetButtonStyle],
						Button[Graphics[{ParametricPlot[{(u^2 Pi)Cos[u],(u^2 Pi) Sin[u]},{u,.01,6 Pi},DisplayFunction->(Cases[#[[1]],_Line,Infinity][[1]]&)]}, ImageSize->27],turns=6;spread=2,$dwPresetButtonStyle]
					}},Spacings->{0,0}],
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add to canvas *)	
						dwNewEmptyLayer["Head"->Line];
						$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
						$dwP[[$dwSelected[[1]]]] = scale*finalPtsScale*# &/@finalPts;
						dwUpdateBoundingBox[$dwSelected[[{1}]]];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						$dwStyleMode = "stroke";
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Spiral",Modal->True]

End[] (* End Private Context *)

EndPackage[]