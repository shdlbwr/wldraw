(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwPointsAndHandles[n_]:=
	If[FreeQ[$dwHideLayers, n],
		If[MemberQ[Join[$dwSelected, $dwPointModeSelections], n|{n,_}] && FreeQ[{Text,Image,"Expression","Text3D"}, $dwHead[[n]]],
			Switch[$dwHead[[n]],
				Point,
					dwPointsAndHandlesPoint[n],
				BezierCurve,
					dwPointsAndHandlesBezier[n],
				_,
					dwPointsAndHandlesDefault[n]
			]
		]
	]
		
dwPointsAndHandlesPoint[n_]:=
	Table[
		Annotation[
			If[MemberQ[$dwSelected, {n, pn}] && $dwMode =!= "toggleCornerPt",
				{FaceForm[Opacity[0]], StrokeForm[{AbsoluteThickness[1/$dwZoom], $dwHiliteColors}], Disk[$dwP[[n,pn]], Offset[3]]},
				Flatten[{(*Which[
					MemberQ[$dwSelected, n] && pn === Length[$dwP[[n]]],
						{FaceForm[Opacity[0]], StrokeForm[{AbsoluteThickness[1/$dwZoom], $dwSelectionColor}]},
					pn === Length[$dwP[[n]]] && $dwP[[-1]] === {},
						{FaceForm[Opacity[0]], StrokeForm[{AbsoluteThickness[1/$dwZoom], $dwSelectionColor}]},
					True,
						{FaceForm[Opacity[0]], StrokeForm[{AbsoluteThickness[1/$dwZoom], $dwSelectionColor}]}
					], *)
					Disk[$dwP[[n,pn]], Offset[2/$dwZoom]]
				}]
			], {n, pn}, "Mouse"],
	{pn, Length[$dwP[[n]]]}]
		
dwPointsAndHandlesBezier[n_]:=
	Flatten[{
		If[MemberQ[{"draw"}, $dwMode] || MemberQ[$dwPointModeSelections, n],
			(* render handle bars *)
			Table[Flatten[{GrayLevel[.5,1], AbsoluteThickness[1/$dwZoom], AbsoluteDashing[{}],
				If[Mod[pn, 3] == 0,
					Line[{$dwP[[n,pn]], $dwP[[n,pn+1]]}],
					Line[{$dwP[[n,pn]], $dwP[[n,pn-1]]}]
				]
			}], {pn, Complement[Range[Length[$dwP[[n]]]], Range[1, Length[$dwP[[n]]], 3]]}],
			{}
		],
		If[MemberQ[{"draw"}, $dwMode] || MemberQ[$dwPointModeSelections, n],
			(* render handle points *)
			Table[
				Annotation[{GrayLevel[.5,1], EdgeForm[{GrayLevel[.5,1], AbsoluteThickness[1/$dwZoom], AbsoluteDashing[{}]}], Rectangle[Offset[-{1.5, 1.5}/$dwZoom, $dwP[[n,pn]]], Offset[{1.5, 1.5}/$dwZoom, $dwP[[n,pn]]]]}, {n, pn}, "Mouse"],
			{pn, Complement[Range[Length[$dwP[[n]]]], Range[1, Length[$dwP[[n]]], 3]]}],
			{}
		],
		(* render main points *)
		Table[
			Annotation[
				If[MemberQ[$dwSelected, {n, pn}] && $dwMode =!= "toggleCornerPt",
					{GrayLevel[1,1], StrokeForm[{Opacity[1], AbsoluteThickness[1/$dwZoom], $dwHiliteColors}], Disk[$dwP[[n,pn]], Offset[4/$dwZoom]]},
					{
						Which[
							MemberQ[$dwSelected, n] && pn === Length[$dwP[[n]]]-1,
								$dwHiliteColors,
							(*pn === Length[$dwP[[n]]]-1 && ($dwP[[-1]] === {} && $dwP[[n,1]] =!= $dwP[[n,-2]]),
								$dwSelectionColor,*)
							True,
								Black
							], 
						AbsolutePointSize[If[(pn === Length[$dwP[[n]]] && MemberQ[$dwSelected, n]) || ($dwP[[n]] === {} && MemberQ[$dwSelected, n]), 8/$dwZoom, 5/$dwZoom]], Point[$dwP[[n,pn]]]
					}
				], {n, pn}, "Mouse"],
		{pn, Range[1, Length[$dwP[[n]]], 3]}]
	}, 1]

dwPointsAndHandlesDefault[n_]:=		
	Table[
		If[FreeQ[{Text,Image,"Expression","Text3D"}, $dwHead[[n]]],
			Annotation[
				If[MemberQ[$dwSelected, {n, pn}] && $dwMode =!= "toggleCornerPt",
					{GrayLevel[1,1], StrokeForm[{Opacity[1], AbsoluteThickness[1/$dwZoom], $dwHiliteColors}], Disk[$dwP[[n,pn]], Offset[4/$dwZoom]]},
					{
						Which[
							$dwMode === "splitShape2" && (MemberQ[$dwSelected, n] && pn === 1),
								$dwHiliteColors,
							FreeQ[{"splitShape1","splitShape2","splitPoint"}, $dwMode] && (MemberQ[$dwSelected, n] && pn === Length[$dwP[[n]]]),
								$dwHiliteColors,
							(*MemberQ[{"draw"}, $dwMode] && (pn === Length[$dwP[[n]]] && $dwP[[-1]] === {}),
								$dwSelectionColor,*)
							True,
								Black
						], 
						Switch[$dwMode,
							"splitShape1"|"splitPoint",
								AbsolutePointSize[5/$dwZoom],
							"splitShape2",
								AbsolutePointSize[If[(pn === 1 && MemberQ[$dwSelected, n]), 8/$dwZoom, 5/$dwZoom]],
							_,
								AbsolutePointSize[If[(pn === Length[$dwP[[n]]] && MemberQ[$dwSelected, n]) || ($dwP[[n]] === {} && MemberQ[$dwSelected, n]), 8/$dwZoom, 5/$dwZoom]]
						], Point[$dwP[[n,pn]]]
					}
				], {n, pn}, "Mouse"],
			Nothing
		],
	{pn, Length[$dwP[[n]]]}]

End[] (* End Private Context *)

EndPackage[]