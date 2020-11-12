(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwAlign]:={"Alignment"->"Horizontal", "SetUndo"->True};
dwAlign[OptionsPattern[]]:=
	DynamicModule[{alignment,xy,objsToMove,firstObjToMove,sortDistance,maxDistance,totalDistance,extraSpace,ctrStart,ctrEnd,sortCtr,objCtr,sortOrder,runningTotal,adjustStart,centerLine,totalBoundary,allInside},
		{alignment, undo} = {OptionValue["Alignment"], OptionValue["SetUndo"]};
		If[undo, dwSetUndo[], Nothing];
		If[$dwMode==="point",
			
			dwAlignPoints["Alignment"->alignment],
			
			If[(MemberQ[{"HorizontalSpace","VerticalSpace"},alignment] && Length@$dwSelected > 2) || (MemberQ[{"Left","Right","Top","Bottom","Horizontal","Vertical"},alignment] && Length@$dwSelected > 1),
				dwConvertPointSelectionToLayerSelection[];
				xy = Switch[alignment, "HorizontalSpace"|"Left"|"Right"|"Vertical", 1, _, 2];
				sortDistance = Table[{s, {Min[If[xy==1,First/@$dwBoundingBoxes[[s]],Last/@$dwBoundingBoxes[[s]]]], Max[If[xy==1,First/@$dwBoundingBoxes[[s]],Last/@$dwBoundingBoxes[[s]]]]}}, {s, Sort@$dwSelected}];
				sortDistance = Sort[sortDistance, #1[[2,xy]] < #2[[2,xy]]&];
				(*rightCheck = If[FreeQ[Table[If[sortDistance[[1, 2, 1]] < sortDistance[[n, 2, 1]] && sortDistance[[1, 2, 2]] > sortDistance[[n, 2, 2]], True, False], {n, 2, Length[sortDistance]}], False], True, False];
				bottomCheck = If[FreeQ[Table[If[sortDistance[[-1, 2, 1]] < sortDistance[[n, 2, 1]] && sortDistance[[-1, 2, 2]] > sortDistance[[n, 2, 2]], True, False], {n, 1, Length[sortDistance] - 1}], False], True, False];*)
				(*If[MemberQ[{"Right"}, alignment] && !rightCheck, sortDistance = Reverse@sortDistance];
				If[MemberQ[{"Bottom"}, alignment] && bottomCheck, sortDistance = Reverse@sortDistance];*)
				totalBoundary = CoordinateBounds[Join[$dwBoundingBoxes[[#]]&/@$dwSelected], 0];
				allInside = MemberQ[If[FreeQ[sortDistance, #], True, False]&/@totalBoundary, False];
				If[MemberQ[{"Right"}, alignment] && allInside === False, sortDistance = Reverse@sortDistance];
				If[MemberQ[{"Bottom"}, alignment] && allInside === True, sortDistance = Reverse@sortDistance];
				If[MemberQ[{"Top"}, alignment], sortDistance = Reverse@sortDistance];
				sortOrder = Table[sortDistance[[s,1]], {s, Length@sortDistance}];
				ctrStart = dwFindCenter[$dwBoundingBoxes[[sortDistance[[1,1]]]]];
				ctrEnd = dwFindCenter[$dwBoundingBoxes[[sortDistance[[-1,1]]]]];
				sortCtr=Switch[alignment,
					"Horizontal",
						centerLine = ctrStart[[2]] + (ctrEnd[[2]] - ctrStart[[2]])/2;
						Join[{{ctrStart[[1]], centerLine}}, Table[{dwFindCenter[$dwBoundingBoxes[[sortDistance[[n,1]]]]][[1]], centerLine}, {n, Length@sortDistance - 2}],{{ctrEnd[[1]], centerLine}}],
					"Vertical",
						centerLine=ctrStart[[1]]+(ctrEnd[[1]]-ctrStart[[1]])/2;
						Join[{{centerLine,ctrStart[[2]]}},Table[{centerLine,dwFindCenter[$dwBoundingBoxes[[sortDistance[[n,1]]]]][[2]]},{n,Length@sortDistance-1}],{{centerLine,ctrEnd[[1]]}}],
					_,
						Join[{ctrStart},Table[ctrStart+(n (ctrEnd-ctrStart)/(Length@sortDistance-1)),{n,Length@sortDistance-2}],{ctrEnd}]
				];
				totalDistance = Total[Table[(sortDistance[[n,2,2]] - sortDistance[[n,2,1]]), {n, Length@sortDistance}]];
				maxDistance = (sortDistance[[-1,2,2]] - sortDistance[[1,2,1]]);
				extraSpace = If[totalDistance>maxDistance || MemberQ[{"Left","Right","Top","Bottom","Horizontal","Vertical"},alignment], 0, (maxDistance-totalDistance)/(Length@sortDistance - 1)];
				runningTotal = Switch[alignment, "Left"|"Right"|"Top"|"Bottom"|"Horizontal"|"Vertical", 0, _, sortDistance[[1,2,2]] - sortDistance[[1,2,1]]];
				objsToMove = Length@sortDistance - Switch[alignment, "Left"|"Right"|"Top"|"Bottom"|"Horizontal"|"Vertical", 0, _, 1];
				firstObjToMove = Switch[alignment, "Left"|"Right"|"Top"|"Bottom"|"Horizontal"|"Vertical", 1 , _, 2];
			
				If[(maxDistance>totalDistance || MemberQ[{"Left","Right","Top","Bottom"}, alignment]) && FreeQ[{"Horizontal","Vertical"}, alignment],
					Do[
						adjustStart = If[MemberQ[{"Right","Top"}, alignment], sortDistance[[n,2,2]] - sortDistance[[1,2,2]], sortDistance[[n,2,1]] - sortDistance[[1,2,1]]];
						(* points *)
						Do[$dwP[[sortDistance[[n,1]],n2,xy]] += -adjustStart + runningTotal + (n-1) extraSpace, {n2, Length@$dwP[[sortDistance[[n,1]]]]}];
						runningTotal += Switch[alignment, "Left"|"Right"|"Top"|"Bottom"|"Horizontal"|"Vertical", 0, _, sortDistance[[n,2,2]] - sortDistance[[n,2,1]]],
					{n, firstObjToMove, objsToMove}],
					If[ctrStart[[1]] =!= ctrEnd[[1]] || MemberQ[{"Horizontal","Vertical"}, alignment],
						Do[objCtr = dwFindCenter[$dwBoundingBoxes[[sortDistance[[n,1]]]]];
							(* points *)
							Do[$dwP[[sortDistance[[n,1]],n2,xy]] += (sortCtr[[n,xy]] - objCtr[[xy]]), {n2, Length@$dwP[[sortDistance[[n,1]]]]}],
						{n,firstObjToMove,objsToMove}]
					]
				]
			]
		];
		dwUpdateBoundingBox[$dwSelected]
	]
	
Options[dwAlignPoints]:={"Alignment"->"Horizontal"};
dwAlignPoints[OptionsPattern[]]:=
	DynamicModule[{alignment,xy,ptsToMove,firstPtToMove,sortDistance,maxDistance,totalDistance,extraSpace,ctrStart,ctrEnd,sortCtr,objCtr,sortOrder,runningTotal,adjustStart,centerLine},
		alignment=OptionValue["Alignment"];
		If[(MemberQ[{"HorizontalSpace", "VerticalSpace"}, alignment] && Length@$dwSelected > 2) || (MemberQ[{"Left","Right","Top","Bottom","Horizontal","Vertical"}, alignment] && Length@$dwSelected > 1),
			xy = Switch[alignment, "HorizontalSpace"|"Left"|"Right"|"Vertical", 1, _, 2];
			(* take only points of first selected object *)
			sortDistance = {#[[1,2]], #[[2]]}&/@DeleteDuplicates[Table[{s, {Min[If[xy == 1, First@$dwP[[$dwSelected[[1, 1]], s[[2]]]], Last@$dwP[[$dwSelected[[1, 1]], s[[2]]]]]], Max[If[xy==1, First@$dwP[[$dwSelected[[1, 1]], s[[2]]]], Last@$dwP[[$dwSelected[[1, 1]], s[[2]]]]]]}}, {s, Sort[$dwSelected, #1[[2]] < #2[[2]]&]}]];
			sortDistance = Sort[sortDistance, #1[[2,xy]] < #2[[2,xy]]&];
			sortDistance = If[MemberQ[{"Right","Top"}, alignment], Reverse@sortDistance, sortDistance];
			sortOrder = Table[sortDistance[[s,1]], {s, Length@sortDistance}];
			ctrStart = $dwP[[$dwSelected[[1, 1]], sortDistance[[1,1]]]];
			ctrEnd = $dwP[[$dwSelected[[1, 1]], sortDistance[[-1,1]]]];
			sortCtr = Switch[alignment,
				"Horizontal",
					centerLine = ctrStart[[2]] + (ctrEnd[[2]]-ctrStart[[2]])/2;
					Join[{{ctrStart[[1]], centerLine}}, Table[{$dwP[[$dwSelected[[1, 1]], sortDistance[[n,1]],1]], centerLine}, {n,Length@sortDistance - 2}],{{ctrEnd[[1]], centerLine}}],
				"Vertical",
					centerLine = ctrStart[[1]] + (ctrEnd[[1]] - ctrStart[[1]])/2;
					Join[{{centerLine, ctrStart[[2]]}}, Table[{centerLine, $dwP[[$dwSelected[[1, 1]], sortDistance[[n,1]],2]]}, {n, Length@sortDistance-1}], {{centerLine, ctrEnd[[1]]}}],
				_,
					Join[{ctrStart},Table[ctrStart + (n (ctrEnd - ctrStart)/(Length@sortDistance - 1)),{n, Length@sortDistance - 2}],{ctrEnd}]
			];
			totalDistance = Total[Table[(sortDistance[[n,2,2]] - sortDistance[[n,2,1]]), {n, Length@sortDistance}]];
			maxDistance = (sortDistance[[-1,2,2]] - sortDistance[[1,2,1]]);
			extraSpace = If[totalDistance>maxDistance || MemberQ[{"Left","Right","Top","Bottom","Horizontal","Vertical"}, alignment], 0, (maxDistance-totalDistance)/(Length@sortDistance - 1)];
			runningTotal = Switch[alignment, "Left"|"Right"|"Top"|"Bottom"|"Horizontal"|"Vertical", 0, _, sortDistance[[1,2,2]] - sortDistance[[1,2,1]]];
			ptsToMove = Length@sortDistance-Switch[alignment,"Left"|"Right"|"Top"|"Bottom"|"Horizontal"|"Vertical", 0, _, 1];
			firstPtToMove = Switch[alignment, "Left"|"Right"|"Top"|"Bottom"|"Horizontal"|"Vertical", 1, _, 2];
			
			If[(maxDistance>totalDistance || MemberQ[{"Left","Right","Top","Bottom"}, alignment]) && FreeQ[{"Horizontal","Vertical"}, alignment],
				Do[
					adjustStart = If[MemberQ[{"Right","Top"}, alignment], sortDistance[[n,2,2]] - sortDistance[[1,2,2]], sortDistance[[n,2,1]] - sortDistance[[1,2,1]]];
					$dwP[[$dwSelected[[1, 1]], sortDistance[[n,1]],xy]] += -adjustStart + runningTotal + (n - 1) extraSpace; (* point *)
					runningTotal += Switch[alignment, "Left"|"Right"|"Top"|"Bottom"|"Horizontal"|"Vertical", 0, _, sortDistance[[n,2,2]] - sortDistance[[n,2,1]]],
				{n, firstPtToMove, ptsToMove}],
				If[ctrStart[[1]] =!= ctrEnd[[1]] || MemberQ[{"Horizontal","Vertical"}, alignment],
					objCtr = dwFindCenter[Table[$dwP[[$dwSelected[[1, 1]], sortDistance[[n,1]]]], {n, firstPtToMove, ptsToMove}]];
					Do[
						$dwP[[$dwSelected[[1, 1]], sortDistance[[n,1]],xy]] = objCtr[[xy]], (* point *)
					{n, firstPtToMove, ptsToMove}]
				]
		]]
	]
	
dwAlignShapeToGridDialog[] :=
 If[$dwMode === "point",
 	
	Do[$dwP[[Sequence@@n]] = Round[$dwP[[Sequence@@n]], $dwGridStep], {n, $dwSelected}],
	
  CreateDialog[Pane[Grid[{
      {Row@{Button[
          Rotate[Graphics[{Gray, 
             Polygon[{{.25, .2}, {.75, .2}, {.75, .7}, {.25, .7}}], 
             Black, Line[{{0, .1}, {1, .1}}]
             }, ImageSize -> 24], -Pi/2], 
          dwAlignShapeToGrid["Left"]],
         Button[
          Rotate[Graphics[{Gray, 
             Polygon[{{.25, .2}, {.75, .2}, {.75, .7}, {.25, .7}}], 
             Black, Line[{{0, .1}, {1, .1}}]
             }, ImageSize -> 24], Pi/2], 
          dwAlignShapeToGrid["Right"]],
         Button[
          Rotate[Graphics[{Gray, 
             Polygon[{{.25, .2}, {.75, .2}, {.75, .7}, {.25, .7}}], 
             Black, Line[{{0, .1}, {1, .1}}]
             }, ImageSize -> 24], Pi], dwAlignShapeToGrid["Top"]],
         Button[
          Graphics[{Gray, 
            Polygon[{{.25, .2}, {.75, .2}, {.75, .7}, {.25, .7}}], 
            Black, Line[{{0, .1}, {1, .1}}]
            }, ImageSize -> 24], dwAlignShapeToGrid["Bottom"]]
         }},
      {Row@{Button[
          Rotate[Graphics[{Gray, 
             Polygon[{{.4, .1}, {.9, .1}, {.9, .6}, {.4, .6}}], Black,
              Line[{{0, 0}, {1, 0}, {1, 1}}]
             }, ImageSize -> 24], Pi], 
          dwAlignShapeToGrid["TopLeft"]],
         Button[
          Rotate[Graphics[{Gray, 
             Polygon[{{.4, .1}, {.9, .1}, {.9, .6}, {.4, .6}}], Black,
              Line[{{0, 0}, {1, 0}, {1, 1}}]
             }, ImageSize -> 24], Pi/2], 
          dwAlignShapeToGrid["TopRight"]],
         Button[
          Rotate[Graphics[{Gray, 
             Polygon[{{.4, .1}, {.9, .1}, {.9, .6}, {.4, .6}}], Black,
              Line[{{0, 0}, {1, 0}, {1, 1}}]
             }, ImageSize -> 24], -Pi/2], 
          dwAlignShapeToGrid["BottomLeft"]], 
         Button[Graphics[{Gray, 
            Polygon[{{.4, .1}, {.9, .1}, {.9, .6}, {.4, .6}}], Black, 
            Line[{{0, 0}, {1, 0}, {1, 1}}]
            }, ImageSize -> 24], dwAlignShapeToGrid["BottomRight"]]
         }},
      {DefaultButton["Done", DialogReturn[], ImageSize -> 100]}
      }], ImageSize -> 300], 
      Background->LightGray, WindowTitle->"Align to grid", Modal->True]
  ]

dwAlignShapeToGrid[direction_]:=
	Module[{shapePt, shapePtAlign, distance},
		dwSetUndo[];
		Do[
			shapePt = Null;
			Switch[direction,
				"Left",
					shapePt = {Min[First/@$dwP[[n]]], 0},
				"Right",
					shapePt={Max[First/@$dwP[[n]]], 0},
				"Top",
					shapePt = {0, Max[Last/@$dwP[[n]]]},
				"Bottom",
					shapePt = {0, Min[Last/@$dwP[[n]]]},
				"TopLeft",
					shapePt = {Min[First/@$dwP[[n]]], Max[Last/@$dwP[[n]]]},
				"TopRight",
					shapePt = {Max[First/@$dwP[[n]]], Max[Last/@$dwP[[n]]]},
				"BottomLeft",
					shapePt = {Min[First/@$dwP[[n]]], Min[Last/@$dwP[[n]]]},
				"BottomRight",
					shapePt = {Max[First/@$dwP[[n]]], Min[Last/@$dwP[[n]]]},
				_,
				MessageDialog[Row@{ToString[direction]<>" should be Left, Right, Top, Bottom, TopLeft, TopRight, BottomLeft, BottomRight."}]
			];
			If[shapePt =!= Null,
				shapePtAlign = Round[shapePt, $dwGridStep];
				distance = shapePtAlign - shapePt;
				Do[$dwP[[n,n2]] += distance, {n2,Length@$dwP[[n]]}
			];
		],{n, $dwSelected}];
		dwUpdateBoundingBox[$dwSelected]
	]

End[] (* End Private Context *)

EndPackage[]