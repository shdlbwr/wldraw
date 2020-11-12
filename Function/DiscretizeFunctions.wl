(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(* <<<<<<<<<< discretize by named selection >>>>>>>>>> *)

dwDiscretizeCurveByNamedSelection[sel_]:=
		Which[
			$dwHead[[sel]] === BSplineCurve,
				dwBSplineDiscretizeByNamedSelection[sel],
			Length@$dwP[[sel]] < 4,
				$dwP[[sel]],
			$dwHead[[sel]] === BezierCurve,
				dwBezierDiscretizeByNamedSelection[sel],
			True,
				$dwP[[sel]]
		]
	
dwBSplineDiscretizeByNamedSelection[sel_]:=
	Block[{f},
		f = BSplineFunction[$dwP[[sel]], SplineClosed->If[$dwStyle[[sel,10]],True,False], SplineDegree->$dwStyle[[sel,11]]];
		Table[f[n], {n,0, 1, 1/$dwDiscretizeResolution}]
	]
	
dwBezierDiscretizeByNamedSelection[sel_]:=
	Block[{pts},
		If[$dwHead[[sel]] === BezierCurve && Length[$dwP[[sel]]] > 4,
			dwBezierDiscretizeList[$dwP[[sel]], $dwDiscretizeResolution/3],
			$dwP[[sel]]
		]
	]

(* <<<<<<<<<< discretize by selection >>>>>>>>>> *)

Options[dwDiscretizeCurve] = {"Simplify"->False};
dwDiscretizeCurve[OptionsPattern[]]:=
		Do[Which[
			Length@$dwP[[s]] < 4,(* keep 4 because discretized shape of closed BSplineCurve with 3 points is smaller than original *)
				Nothing,
			$dwHead[[s]] === BezierCurve,
				dwBezierDiscretize["Simplify"->OptionValue["Simplify"]],
			$dwHead[[s]] === BSplineCurve,
				dwBSplineDiscretize[],
			True,
				Nothing
		],{s, $dwSelected}]
	
dwBSplineDiscretize[]:=
	Block[{sel, f},
		Do[
			sel = If[Length[$dwSelected[[s]]] > 1, $dwSelected[[s,1]], $dwSelected[[s]]];
			f = BSplineFunction[$dwP[[sel]], SplineClosed->$dwStyle[[sel,10]], SplineDegree->$dwStyle[[sel,11]]];
			$dwP[[sel]] = Table[f[n], {n,0, 1, 1/$dwDiscretizeResolution}];
			$dwHead[[sel]] = If[$dwStyle[[sel,1]], Polygon, Line], 
		{s, Length[$dwSelected]}]
	]
	
Options[dwBezierDiscretize] = {"Simplify"->False};
dwBezierDiscretize[OptionsPattern[]]:=
	Block[{sel, pts},
		Do[
			sel = If[Length[$dwSelected[[s]]] > 1, $dwSelected[[s,1]], $dwSelected[[s]]];
			If[$dwHead[[sel]] === BezierCurve && Length[$dwP[[sel]]] > 4,
				pts = dwBezierDiscretizeList[$dwP[[sel]], $dwDiscretizeResolution/3];
				$dwP[[sel]] = If[OptionValue["Simplify"], dwSimplifyPts[pts, 2], pts];
				$dwHead[[sel]] = If[$dwStyle[[sel,1]], Polygon, Line]
			], 
		{s, Length[$dwSelected]}]
	]

(* <<<<<<<<<< discretize by point list >>>>>>>>>> *)
	
dwBSplineDiscretizeList[bsplineList_:{}, res_:20, n_:0]:=
	Block[{f},
		If[Length[bsplineList] > 1,
			f = BSplineFunction[bsplineList, SplineClosed->If[n==0||$dwStyle[[n,10]],True,False], SplineDegree->If[n==0,$dwDefaultSplineDegree,$dwStyle[[n,11]]]];
			Table[f[pn], {pn, 0, 1, 1/res}],
			bsplineList
		]
	]

dwBezierDiscretizeList[bezierList_:{}, res_:20]:=
	Block[{closed,final},
		closed=If[bezierList[[1]]===bezierList[[-2]],True,False];
		final=DeleteDuplicates[Flatten[Table[dwBezierDiscretizePts[bezierList[[p;;p+3]],res],{p,1,(3IntegerPart[Length@bezierList/3]+1)-3,3}],1]];
		If[closed,Join[final,{final[[1]]}],final]
	]

dwBezierDiscretizePts[bezier4Pts_:{}, res_:20]:=
	Table[(1-n)^3*bezier4Pts[[1]]+3*(1-n)^2*n*bezier4Pts[[2]]+3*(1-n)*n^2*bezier4Pts[[3]]+n^3*bezier4Pts[[4]],{n,0,1,1/(Round[res*ArcLength[Line[Table[(1-n)^3*bezier4Pts[[1]]+3*(1-n)^2*n*bezier4Pts[[2]]+3*(1-n)*n^2*bezier4Pts[[3]]+n^3*bezier4Pts[[4]],{n,0,1,1/10}]]]]+$MachineEpsilon)}]

(* based on slope comparison of two adjoining lines *)
dwSimplifyPts[p_, slopeDifference_:1]:=
	Block[{finalP},
		If[slopeDifference <= 1,
			finalP={p[[1]]};
			Do[
				If[Min[Abs@Round[(p[[n,2]]-p[[n-1,2]])/(Chop[(p[[n,1]]-p[[n-1,1]])]+$MachineEpsilon),.0001], slopeDifference]===
						Min[Abs@Round[(p[[n+1,2]]-p[[n,2]])/(Chop[(p[[n+1,1]]-p[[n,1]])]+$MachineEpsilon),.0001], slopeDifference]
					||
					Max[Abs@Round[(p[[n,2]]-p[[n-1,2]])/(Chop[(p[[n,1]]-p[[n-1,1]])]+$MachineEpsilon),.0001], 1-slopeDifference]===
						Max[Abs@Round[(p[[n+1,2]]-p[[n,2]])/(Chop[(p[[n+1,1]]-p[[n,1]])]+$MachineEpsilon),.0001], 1-slopeDifference],
					Nothing,
					AppendTo[finalP, p[[n]]]
				],
			{n, 2, Length@p-1}];
			AppendTo[finalP, p[[-1]]],
			p
		]
	]
	
Options[dwDecreasePointsByImage]={"ReturnPoints"->False, "LightHoles"->True, "FillHoles"->0};
dwDecreasePointsByImage[ptList_:{}, head_:Polygon, replaceNum_:0, OptionsPattern[]]:=
	Block[{ptList2,pts,bottomLeft,ptmax,max,fc,g,p,finalPts,image,boundaryPts,fillHoles = OptionValue["FillHoles"],temp},
		If[Length[ptList] > 2,
			ptList2=ptList;
			boundaryPts=Switch[head, BezierCurve, dwBezierDiscretizeList[ptList2, 40], BSplineCurve, dwBSplineDiscretizeList[ptList2, 160, replaceNum], _, ptList2];
			Switch[head,
				BezierCurve,
					image=Blur[ColorNegate[Binarize[Graphics[FilledCurve[BezierCurve[ptList2]],PlotRangePadding->None]]],2*$dwPointSmoothDecreaseRate],
				BSplineCurve,
					image=Blur[ColorNegate[Binarize[Graphics[FilledCurve[BSplineCurve[ptList2, SplineClosed->If[replaceNum==0,$dwDefaultSplineClosed, $dwStyle[[replaceNum,10]]], SplineDegree->If[replaceNum==0,$dwDefaultSplineDegree,$dwStyle[[replaceNum,11]]]]],PlotRangePadding->None]]],2*$dwPointSmoothDecreaseRate],
				Line|Arrow|Point,
					image=Blur[ColorNegate[Binarize[Graphics[Polygon[ptList2],PlotRangePadding->None]]],2*$dwPointSmoothDecreaseRate],
				_,
					image=Blur[ColorNegate[Binarize[Graphics[head[ptList2],PlotRangePadding->None]]],2*$dwPointSmoothDecreaseRate]
			];
			image = If[NumberQ[fillHoles] && fillHoles > 0, If[OptionValue["LightHoles"] === True, GeodesicOpening, GeodesicClosing][image, fillHoles], image];
			temp = ImageMesh[image];
			If[Head[temp] === EmptyRegion,
				
				finalPts = {},
				
				g = Show[temp];
				pts=Cases[g,_GraphicsComplex,Infinity][[1,1]];
				fc=Cases[g[[1]],_FilledCurve,Infinity];
				fc=Table[Flatten[fc[[n,1]]],{n,Length@fc}];
				p=Cases[g[[1]],_Polygon,Infinity];
				finalPts=Join[
					Flatten[Table[Flatten[Table[pts[[fc[[n1,n2,n3]]]],{n2,Length@fc[[n1]]},{n3,Length@fc[[n1,n2]]}],1],{n1,Length@fc}],1],
					If[p==={},{},Flatten[Table[pts[[p[[1,n,n2]]]],{n,Length@p[[1]]},{n2,Length@p[[1,n]]}],1]]
				]
			];
			If[finalPts =!= {},
				ptmax=EuclideanDistance[Min[#[[1]]&/@boundaryPts],Max[#[[1]]&/@boundaryPts]];
				max=EuclideanDistance[Min[#[[1]]&/@pts],Max[#[[1]]&/@pts]];
				bottomLeft={Min[#[[1]]&/@boundaryPts],Min[#[[2]]&/@boundaryPts]};
				
				If[OptionValue["ReturnPoints"],
					
					Table[
						bottomLeft+(ptmax/(max+$MachineEpsilon))#&/@finalPts[[n]],
					{n,Length@finalPts}],
					
					If[replaceNum == 0,
						
						(* add new layers only *)
						Do[
							dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
							$dwP[[-1]]=bottomLeft+(ptmax/(max+$MachineEpsilon))#&/@finalPts[[n]],
						{n,Length@finalPts}];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						dwUpdateBoundingBox[Range[Length@$dwP - Length@finalPts + 1, Length@$dwP]],
						
						(* replace selected layer and add new layers *)
						$dwP[[replaceNum]] = bottomLeft+(ptmax/(max+$MachineEpsilon))#&/@finalPts[[1]];
						$dwHead[[replaceNum]] = Polygon;
						dwUpdateBoundingBox[{replaceNum}];
						If[Length[finalPts] > 1,
							Do[
								dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
								$dwP[[-1]]=bottomLeft+(ptmax/(max+$MachineEpsilon))#&/@finalPts[[n]],
							{n,2,Length[finalPts]}];
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							dwUpdateBoundingBox[Range[Length@$dwP - Length@finalPts + 2, Length@$dwP]]
						]
					]
				],
				ptList
			],
			ptList
		]
	]

End[] (* End Private Context *)

EndPackage[]