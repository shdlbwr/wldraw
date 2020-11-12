(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
	
dwSelectionMenu[]:=
	(*Dynamic@*)ActionMenu[Pane[$dwIconSelectionMenu],
		{
			"Select all":>(dwSetUndo[];
				Switch[$dwMode,
					"point",
						$dwSelected = DeleteDuplicates[Flatten[
								Table[
									If[$dwP[[n]] === {}, 
										Nothing,
										If[$dwHead[[n]] === BezierCurve && $dwP[[n,1]] === $dwP[[n,-1]],
											Rest@Table[{n,n2}, {n2, 1, Length[$dwP[[n]]], If[MemberQ[{BezierCurve}, $dwHead[[n]]], 3, 1]}],
											Table[{n,n2}, {n2, 1, Length[$dwP[[n]]], If[MemberQ[{BezierCurve}, $dwHead[[n]]], 3, 1]}]
										]
									],
								{n, $dwPointModeSelections}],
							1]],
					"draw",
						$dwSelected = DeleteDuplicates[Flatten[Table[Table[If[$dwP[[n]]=!={}, {n,n2}, Nothing], {n2, Length[$dwP[[n]]]}], {n, Complement[Range[Length[$dwP]], $dwHideLayers]}],1]];
						$dwPointModeSelections = #[[1]]&/@$dwSelected;
						$dwMode = "point",
					_,
						dwDeleteEmptyLayers[];
						$dwMode = $dwCurrentPreviewMode;
						$dwSelected = Complement[Range[Length[$dwP]], $dwHideLayers]
				]
			),
			If[MemberQ[{"point", "draw"}, $dwMode],
				"Select all remaining object points":>(dwSetUndo[];
					Switch[$dwMode,
						"point",
							$dwSelected = DeleteDuplicates[Flatten[
									Table[
										If[$dwP[[n,1]] === $dwP[[n,-1]],
											Rest@Table[{n,n2}, {n2, 1, Length[$dwP[[n]]], If[MemberQ[{BezierCurve}, $dwHead[[n]]], 3, 1]}],
											Table[{n,n2}, {n2, 1, Length[$dwP[[n]]], If[MemberQ[{BezierCurve}, $dwHead[[n]]], 3, 1]}]
										], 
									{n, #[[1]]&/@$dwSelected}],
								1]],
						"draw",
							$dwSelected = DeleteDuplicates[Flatten[Table[Table[{n,n2}, {n2, 1, Length[$dwP[[n]]], If[MemberQ[{BezierCurve}, $dwHead[[n]]], 3, 1]}], {n, $dwSelected}],1]];
							$dwPointModeSelections = #[[1]]&/@$dwSelected;
							$dwMode = "point",
						_,
							$dwSelected = Range[Length[$dwP]]
					]
				),
				Nothing
			],
			"Show all":>(dwSetUndo[];
						$dwHideLayers = {}
				),
			"Delete all":>(dwSetUndo[];
						$dwMode = $dwCurrentPreviewMode;
						$dwP = $dwStyle = $dwHead = $dwBoundingBoxes = $dwObjectGradients = $dwLineGradients = $dwGroupLayers = $dwCompoundPathLayers = $dwHideLayers = $dwSelected = $dwPointModeSelections = $dwAnimate = {};
						$dwPointQuantity = $dwObjectQuantity = 0
				),
			Delimiter,
			"Select inverse":>(dwSetUndo[];
				Switch[$dwMode,
					"point",
						If[$dwSelected === {},
							$dwSelected = Flatten[Table[Table[If[$dwP[[n]]=!={}, {n,n2}, Nothing], {n2, Length[$dwP[[n]]]}], {n, $dwPointModeSelections}],1],
							$dwSelected = Flatten[Table[Complement[Table[{n,n2}, {n2, Length[$dwP[[n]]]}], $dwSelected], {n, #[[1]]&/@$dwSelected}],1]
						],
					"draw",
						If[$dwSelected === {},
							$dwSelected = Flatten[Table[Table[If[$dwP[[n]]=!={}, {n,n2}, Nothing], {n2, Length[$dwP[[n]]]}], {n, $dwPointModeSelections}],1],
							$dwSelected = Flatten[Table[Complement[Table[{n,n2}, {n2, Length[$dwP[[n]]]}], $dwSelected], {n, $dwSelected}],1]
						];
						$dwMode = "point",
					_,
					$dwSelected = Complement[Range[Length[$dwP]], $dwSelected, $dwHideLayers]
				]),
			Delimiter,
			"Hide selected":>(dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						$dwHideLayers = Union[$dwHideLayers, $dwSelected];
						$dwSelected = {}
				),
			"Hide inverse":>(dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						$dwHideLayers = Union[$dwHideLayers, Complement[Range[Length[$dwP]], $dwSelected]];
				),
			(*"Hide masks":>(dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						$dwHideLayers = Union[$dwHideLayers, 
							Table[
								If[$dwHead[[n]] === Image && Flatten[Position[$dwStyle[[n]],{"Mask",__},Infinity]] =!= {},
									If[$dwStyle[[Sequence@@Join[{n},Position[$dwStyle[[n]],{"Mask",__},Infinity][[1]],{2}]]] =!= n,
										$dwStyle[[Sequence@@Join[{n},Position[$dwStyle[[n]],{"Mask",__},Infinity][[1]],{2}]]],
										Nothing],
								Nothing], 
							{n, Length[$dwP]}],
							Table[
								If[$dwHead[[n]] === Text && $dwStyle[[n,12]] =!= None,
									$dwStyle[[n,12]],
								Nothing], 
							{n, Length[$dwP]}]];
				),*)
			Delimiter,
			"Delete inverse":>(dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						$dwSelected = Complement[Range[Length[$dwP]], $dwSelected, $dwHideLayers];
						dwDeleteLayer["SetUndo"->False];
						If[MemberQ[$dwP,{}], dwDeleteEmptyLayers[]]
				),
			"Delete hidden objects":>(dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						$dwSelected = $dwHideLayers;
						dwDeleteLayer[];
						dwDeleteEmptyLayers[]
				),
			"Delete void objects":>(dwSetUndo[];
						dwDeleteEmptyLayers[]
				),
			Delimiter,
			"Select same:":>(Null),
			"   Object":>(
					If[$dwSelected === {},
						MessageDialog["No selection to match"],
						
						dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						Block[{temp = DeleteDuplicates[Table[$dwHead[[n]], {n, $dwSelected}]]},
							If[SubsetQ[Flatten[$dwCompoundPathLayers], temp],
								(* compound paths only *)
								$dwSelected = Complement[Flatten[$dwCompoundPathLayers], $dwHideLayers],
								(* all objects *)
								$dwSelected = Complement[Table[If[MemberQ[temp, $dwHead[[n]]], n, Nothing], {n, Length[$dwP]}], $dwHideLayers]
							]
						]
					]
				),
			"   Style":>(
					If[$dwSelected === {},
						MessageDialog["No selection to match"],
						
						dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						$dwSelected = Complement[Table[If[$dwStyle[[n]] === $dwStyle[[$dwSelected[[1]]]], n, Nothing], {n, Length[$dwP]}], $dwHideLayers]
					]
				),
			"   Fill color":>(
					If[$dwSelected === {},
						MessageDialog["No selection to match"],
						
						dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						If[MemberQ[{BSplineCurve, BezierCurve, Polygon}, $dwHead[[$dwSelected[[1]]]]],
							If[$dwStyle[[$dwSelected[[1]],8,1]] === None,
								
								$dwSelected = Complement[Table[
									If[MemberQ[{BSplineCurve, BezierCurve, Polygon}, $dwHead[[n]]] && $dwStyle[[n,8,1]] === None,
										If[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1;;2]] === 
										$dwStyle[[$dwSelected[[1]], Flatten[Position[$dwStyle[[$dwSelected[[1]]]], FaceForm[_]]][[1]]]][[1,1;;2]], 
											n, 
											Nothing
										],
										Nothing
									], 
								{n, Length[$dwP]}], $dwHideLayers],
								
								$dwSelected = Complement[Table[
									If[MemberQ[{BSplineCurve, BezierCurve, Polygon}, $dwHead[[n]]] && $dwStyle[[n,8,1]] =!= None,
										If[$dwStyle[[n,8,2]] === $dwStyle[[$dwSelected[[1]],8,2]], 
											n, 
											Nothing
										],
										Nothing
									], 
								{n, Length[$dwP]}], $dwHideLayers]
							]
						]
					]
				),
			"   Stroke color":>(
					If[$dwSelected === {},
						MessageDialog["No selection to match"],
						
						dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line, Arrow}, $dwHead[[$dwSelected[[1]]]]],
							$dwSelected = Complement[Table[
								If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line, Arrow}, $dwHead[[n]]],
									If[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1;;2]] === 
									$dwStyle[[$dwSelected[[1]], Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]]]][[1,1;;2]], 
										n, 
										Nothing
									],
									Nothing
								], 
							{n, Length[$dwP]}], $dwHideLayers]
						]
					]
				),
			"   Stroke thickness":>(
					If[$dwSelected === {},
						MessageDialog["No selection to match"],
						
						dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line, Arrow}, $dwHead[[$dwSelected[[1]]]]],
							$dwSelected = Complement[Table[
								If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line, Arrow}, $dwHead[[n]]],
									If[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2;;3]] === 
									$dwStyle[[$dwSelected[[1]], Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]]]][[1,2;;3]], 
										n, 
										Nothing
									],
									Nothing
								], 
							{n, Length[$dwP]}], $dwHideLayers]
						]
					]
				),
			"   Stroke dashing":>(
					If[$dwSelected === {},
						MessageDialog["No selection to match"],
						
						dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line, Arrow}, $dwHead[[$dwSelected[[1]]]]],
							$dwSelected = Complement[Table[
								If[MemberQ[{BSplineCurve, BezierCurve, Polygon, Line, Arrow}, $dwHead[[n]]],
									If[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1]][[{2, 4}]] === 
									$dwStyle[[$dwSelected[[1]], Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]]]][[1]][[{2, 4}]], 
										n, 
										Nothing
									],
									Nothing
								], 
							{n, Length[$dwP]}], $dwHideLayers]
						]
					]
				),
			"   Text size":>(
					If[$dwSelected === {},
						MessageDialog["No selection to match"],
						
						dwSetUndo[];
						dwConvertPointSelectionToLayerSelection[];
						If[$dwHead[[$dwSelected[[1]]]] === Text,
							$dwSelected = Complement[Table[
								If[$dwHead[[n]] === Text,
									If[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FontSize]][[1]]]][[2]] === 
									$dwStyle[[$dwSelected[[1]], Flatten[Position[$dwStyle[[$dwSelected[[1]]]], FontSize]][[1]]]][[2]], 
										n, 
										Nothing
									],
									Nothing
								], 
							{n, Length[$dwP]}], $dwHideLayers]
						]
					]
				),
			Delimiter,
			"Select small objects":>(
					$dwMessageBarText = "Selecting small objects...";
					dwConvertPointSelectionToLayerSelection[];
					If[$dtMode =!= "preview", $dtMode = "preview"];
					$dwSelected = Table[If[Length[$dwP[[n]]] > 1 && Area[Region[Rectangle[Sequence@@Transpose[CoordinateBounds[$dwP[[n]]]]]]] <= $dwSmallObjectSize, n, Nothing], {n, Length[$dwP]}];
					$dwMessageBarText = ""),
			"Select large objects":>(
					$dwMessageBarText = "Selecting large objects...";
					dwConvertPointSelectionToLayerSelection[];
					If[$dtMode =!= "preview", $dtMode = "preview"];
					$dwSelected = Table[If[Length[$dwP[[n]]] > 1 && Area[Region[Rectangle[Sequence@@Transpose[CoordinateBounds[$dwP[[n]]]]]]] >= $dwLargeObjectSize, n, Nothing], {n, Length[$dwP]}];
					$dwMessageBarText = "")
				
		},$dwButtonStyle]

End[] (* End Private Context *)

EndPackage[]