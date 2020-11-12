(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwRenderGroupBox[]:=
	DynamicModule[{groups, ptsList, box, a, b, c, d},
		If[MemberQ[{"preview", "wireframe"}, $dwMode] && $dwSelected =!= {},
								
			groups = DeleteDuplicates[#[[1]]&/@Flatten[Table[Position[$dwGroupLayers, s],{s, $dwSelected}], 1]];
			ptsList = Table[Flatten[Table[$dwBoundingBoxes[[n]], {n, g}],1], {g, $dwGroupLayers[[groups]]}];
			
			If[ptsList =!= {},
				Flatten[{StrokeForm[{AbsoluteThickness[1/$dwZoom], $dwHiliteColors}],$dwHiliteColors,
						Table[{
							{{a,c},{b,d}} = CoordinateBounds[pts, .0125/$dwZoom];(* second input is margin size *)
							box = {{a,b},{c,b},{c,d},{a,d},{a,b}};
							Table[Rectangle[box[[p[[1]]]], Offset[p[[2]]/$dwZoom, box[[p[[1]]]]]], {p, {{1,{4,4}}, {2,{-4,4}}, {3,{-4,-4}}, {4,{4,-4}}}}],
							Style[Line[box],Antialiasing->False]},
					{pts, ptsList}]
				}],
				{}
			],
			
			{}
		]
	]

End[] (* End Private Context *)

EndPackage[]