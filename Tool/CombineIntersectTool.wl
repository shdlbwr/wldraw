(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
	
Options[dwShapeCombineIntersect] = {"Process"->RegionUnion};(* RegionUnion or RegionIntersection *)
dwShapeCombineIntersect[OptionsPattern[]]:=
	DynamicModule[{pts, f},
		
		If[Length[$dwSelected] < 2,
			
			MessageDialog["Select multiple objects. Image, text and compound objects are ignored."],
			
			If[FreeQ[(MemberQ[{BSplineCurve, BezierCurve}, $dwHead[[#]]]&/@$dwSelected), True],
				
				dwCreateCombineIntersection["Process"->OptionValue["Process"]],
				
				CreateDialog[Pane[Column[{
					Dynamic@Graphics[Table[{
						(*$dwStyle[[s]][[$dwStyleStart;;-1]]*)
						StrokeForm[{Black, AbsoluteThickness[1]}], FaceForm[Opacity[0]],
						pts = Switch[$dwHead[[s]],
								BezierCurve,
									If[Length[$dwP[[s]]] > 4,
										pts = dwBezierDiscretizeList[$dwP[[s]], $dwDiscretizeResolution/3];
										dwSimplifyPts[pts, 2],
										$dwP[[s]]
									],
								BSplineCurve,
									If[Length[$dwP[[s]]] > 2,
										f = BSplineFunction[$dwP[[s]], SplineClosed->If[s==0||$dwStyle[[s,10]],True,False], SplineDegree->If[s==0,$dwDefaultSplineDegree,$dwStyle[[n,11]]]];
										pts = Table[f[pn], {pn, 0, 1, 1/$dwDiscretizeResolution}];
										dwSimplifyPts[pts, 2],
										$dwP[[s]]
									],
								_,
									$dwP[[s]]
							];
						If[$dwStyle[[s,1]], Polygon, Line][pts],
						Point[pts]
					}, {s, If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]}], Background->White, ImageSize->{300,300}],
					Row@{PopupMenu[Dynamic@$dwDiscretizeResolution,{10->"Rough",20->"Low",40->"Medium",80->"High",160->"Extreme",320->"Ultra"}]," resolution"},
					"",
					Row@{
						CancelButton[DialogReturn[]],
						DefaultButton[dwCreateCombineIntersection["Process"->OptionValue["Process"]]; DialogReturn[]]
						}
					}, Alignment->Center],
				Alignment->Center,ImageSize->300],WindowTitle->If[process === "RegionUnion", "Combined object", "Intersection object"],Modal->True]
			]
		]
	]

Options[dwCreateCombineIntersection] = {"Process"->RegionUnion};(* RegionUnion or RegionIntersection *)
dwCreateCombineIntersection[OptionsPattern[]]:=
	Block[{combineLines,combineFinal,layers,groupStart,points, process = OptionValue["Process"], temp},
		dwSetUndo[];
		(* remove image and text objects *)
		$dwSelected = (If[FreeQ[{Image, Text}, $dwHead[[#]]], #, Null]&/@$dwSelected)/.Null->Sequence[];
		(* remove compound paths *)
		$dwSelected = Complement[$dwSelected, Flatten[$dwCompoundPathLayers]];
		If[Length@$dwSelected > 1,
			layers = Sort@$dwSelected;
			(* combine selected layers *)
			If[Length@($dwP/.{}->Sequence[]) > 1,
				points = $dwP;
				Do[
					Switch[$dwHead[[n]],
						BezierCurve,
							points[[n]] = dwBezierDiscretizeList[points[[n]], $dwDiscretizeResolution/3],
						BSplineCurve,
							points[[n]] = dwBSplineDiscretizeList[points[[n]], $dwDiscretizeResolution, n],
						_,
							points[[n]] = DeleteDuplicates[points[[n]], Round[#1,.01] == Round[#2,.01]&]
					], 
				{n, layers}];
					
				points = Round[#,.001]&/@points;(* for possible RegionUnion error when combining shapes *)
				temp = process[Sequence @@ (Table[MeshRegion[points[[n]],Polygon[Range[Length@points[[n]]]]],{n,layers}]/.Null->Sequence[])];
				
				If[FreeQ[temp, EmptyRegion],
					
					combineLines = MeshPrimitives[temp,1];
					combineFinal = {{combineLines[[1,1,1]]}};
				
					Do[If[combineLines[[n,1,-1]] == combineLines[[n+1,1,1]],
						AppendTo[combineFinal[[-1]],combineLines[[n+1,1,1]]],
						AppendTo[combineFinal,{combineLines[[n+1,1,1]]}]],
					{n, Length@combineLines-1}];
					(* update variables *)
					$dwP = FlattenAt[Insert[Delete[points, List[#]&/@layers], combineFinal, layers[[1]]], layers[[1]]];
					$dwStyle = FlattenAt[Insert[Delete[$dwStyle,List[#]&/@layers],Table[$dwStyle[[layers[[1]]]],{Length@ combineFinal}],layers[[1]]],layers[[1]]];
					$dwHead = FlattenAt[Insert[Delete[$dwHead,List[#]&/@layers],Table[$dwHead[[layers[[1]]]],{Length@ combineFinal}],layers[[1]]],layers[[1]]];
					$dwObjectGradients = FlattenAt[Insert[Delete[$dwObjectGradients,List[#]&/@layers],Table[$dwObjectGradients[[layers[[1]]]],{Length@ combineFinal}],layers[[1]]],layers[[1]]];
					$dwLineGradients = FlattenAt[Insert[Delete[$dwLineGradients,List[#]&/@layers],Table[$dwLineGradients[[layers[[1]]]],{Length@ combineFinal}],layers[[1]]],layers[[1]]];
					(* fix to not remove higher groups when combine layers > 1 *)
					$dwGroupLayers = If[MemberQ[Table[MemberQ[#,l], {l,layers}], True], Nothing,#]&/@$dwGroupLayers;
					groupStart = $dwGroupLayers;
					If[Length[combineFinal] > 1,
						$dwGroupLayers = Table[If[Max[gl,layers[[1]]]===layers[[1]],gl, Nothing], {gl, $dwGroupLayers}],
						Do[
							Do[
								Do[
									If[
										groupStart[[gln,gln2]]>s,
										$dwGroupLayers[[gln,gln2]]=$dwGroupLayers[[gln,gln2]]-1,
									Nothing],
								{gln2,Length[$dwGroupLayers[[gln]]]}],
							{gln,Length[$dwGroupLayers]}],
						{s,Rest[layers]}]
					];
					$dwSelected[[1]] = layers[[1]];
					$dwSelected = {$dwSelected[[1]]};
					$dwHead[[$dwSelected[[1]]]] = Polygon;
					dwUpdateAllBoundingBoxes[],
					
					MessageDialog["Objects do not intersect."]
				]
			]
		]
	]

End[] (* End Private Context *)

EndPackage[]