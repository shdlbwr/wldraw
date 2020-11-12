(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderSelectionBox[]:=
	DynamicModule[{pts, a, b, c, d, selected},
		
		If[$dwSelected =!= {} && FreeQ[$dwSelected, 0],
			
			selected = If[Length[$dwSelected[[1]]] > 1, Flatten[If[Length[#[[1]]] > 1, #[[1,1]], #[[1]]]&/@$dwSelected], $dwSelected];
			Switch[$dwMode,
				"preview"|"wireframe"|"point",
					pts = Flatten[Table[$dwBoundingBoxes[[n]], {n, selected}], 1];
					If[pts =!= {},
						{{a,c},{b,d}} = CoordinateBounds[pts,$dwSelectionBoundaryPad/$dwZoom];(* second input is margin size *)
						$dwSelectionBoundary = {{a,b},{Mean[{a,c}],b},{c,b},{c,Mean[{b,d}]},{c,d},{Mean[{a,c}],d},{a,d},{a,Mean[{b,d}]}};
						Flatten[{
							If[Length[$dwSelected] > 1,
								(* replace Table[...] below with this code to create group boxes instead of individual boxes: 
									Line[{{a,c},{b,d}} = CoordinateBounds[#,(.5$dwSelectionBoundaryPad)/$dwZoom]; {{a,b},{c,b},{c,d},{a,d},{a,b}}]&/@Table[$dwBoundingBoxes[[n]], {n, Complement[$dwSelected, Flatten[$dwGroupLayers]]}] *)
								{AbsoluteThickness[1/$dwZoom], $dwSelectionColor, Table[Line[($dwSelectionBoundaryPad/$dwZoom)*{{-1,-1},{1,-1},{1,1},{-1,1},{-1,-1}} + Join[$dwBoundingBoxes[[n]], $dwBoundingBoxes[[n, {1}]]]], {n, selected}]},
								{}
							],
							(* combined box *)
							{StrokeForm[{AbsoluteThickness[1/$dwZoom], $dwSelectionColor}], Line[Join[$dwSelectionBoundary,$dwSelectionBoundary[[{1}]]]], White, 
								Table[Tooltip[Annotation[Disk[$dwSelectionBoundary[[n]], Offset[4/$dwZoom]], "s"<>ToString[n], "Mouse"], 
									If[OddQ[n], "Drag corner to rotate.\nPress shift key for 0.5° increments", "Drag midpoint to scale.\nPress shift key for 1% increments\nPress option key to scale each direction separately."], TooltipDelay->$dwTooltipDelay], 
								{n, Length[$dwSelectionBoundary]}]}
						}],
						$dwSelectionBoundary = $dwSelectionBoundaryStart = {};
						{}
					],
				"transform",
					{AbsoluteThickness[1/$dwZoom], $dwSelectionColor, Line[Join[$dwSelectionBoundary,$dwSelectionBoundary[[{1}]]]], EdgeForm[{AbsoluteThickness[1/$dwZoom], $dwSelectionColor}], White, Table[Annotation[Disk[$dwSelectionBoundary[[n]], Offset[4/$dwZoom]], "s"<>ToString[n], "Mouse"], {n, Length[$dwSelectionBoundary]}]},
				_,
					{}
			],
			
			{}
		]
	]

End[] (* End Private Context *)

EndPackage[]