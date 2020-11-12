(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwDimensionArrow]={"Location"->{0,0}, "Size"->1, "Rotate"->0, "Length"->1, "Height"->.1, "BreakWidth"->0, "BreakCenter"->.5,
	"ArrowPosition"->"Both", "ShowEnd"->"Both", "Offset"->0, "Position"->{-1,-1}, "ArrowSize"->Small, "LineWeight"->1, "Color"->Black,
	"Label"->None, "LabelRotation"->True, "LabelOffset"->{0,0}, "ArrowsInside"->True, "ArrowsOutsideLength"->{.5,.5}};

dwDimensionArrow[OptionsPattern[]]:=
	Block[{offset,labelDimension,autoPad=2,fRot,fScale,loc,size,rotate,arrowSize,arrowPos,arrowsInside,arrowsOutsideLength,
		thickness,color,label,labelOffset,labelRot,bWidth,bCtr,length,height,pos,showEnd,p1,p2,posOffset},
		
		{loc,size,rotate,arrowSize,arrowPos,arrowsInside,arrowsOutsideLength,thickness,
			color,label,labelOffset,labelRot,bWidth,bCtr,length,height,pos,showEnd} =
		OptionValue[{"Location","Size","Rotate","ArrowSize","ArrowPosition","ArrowsInside","ArrowsOutsideLength","LineWeight",
			"Color","Label","LabelOffset","LabelRotation","BreakWidth","BreakCenter","Length","Height","Position","ShowEnd"}];
			
		{p1, p2} = If[FreeQ[{$dwClickPtNoGrid, $dwPreviousClickPtNoGrid}, Null], 
			Round[{$dwClickPtNoGrid, $dwPreviousClickPtNoGrid}, $dwGridStep],
			{Null, Null}
		];
			
		(* use mouseclicks as input *)
		If[$dwShowMouseClickPositions,
			If[p2 =!= Null && p1 =!= Null,
				loc = p2;
				rotate = Round[(Quiet@ToPolarCoordinates[Subtract[Sequence@@(#-{0,0}&/@{p1,p2})]]/.{Indeterminate->0})[[2]], .01];
				length = EuclideanDistance[p1,p2]/size
			]
		];
		
		(*loc = loc - .5 {length,height} - .5 pos{length,height};*)
		posOffset = .5 {length,height} + .5 pos{length,height};
		labelDimension = ImageDimensions[Rasterize[label, ImageResolution->$dwImageResolution]];
		bWidth = If[MemberQ[{Automatic, "Start", "End"}, bWidth],
			If[label === None,
				0,
				If[labelRot,
					.5labelDimension[[1]] + autoPad, 
					.5(Abs[labelDimension[[1]]Cos[rotate]] + Abs[labelDimension[[2]]Sin[rotate]]) - .25(Abs[Cos[rotate]Sin[rotate]])(Abs[labelDimension[[1]]Cos[rotate]] + Abs[labelDimension[[2]]Sin[rotate]])+autoPad
				]
			], 
			bWidth
		];
		offset = .5height OptionValue["Offset"];
		
		(* graphics *)
		fRot = RotationTransform[rotate, loc];
		fScale = ScalingTransform[{size,size}, loc];
		
		CopyToClipboard[{p1,p2}];
		
		If[arrowsInside,
			
			{
				StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Round"], JoinForm["Round"]}], Arrowheads[arrowSize],
				If[MemberQ[{"Both", "Start"}, showEnd], Line[fRot[fScale[#]]&/@{(loc - posOffset) + {0,0}, (loc - posOffset) + {0,height}}], {}],
				If[MemberQ[{"Both", "End"}, showEnd], Line[fRot[fScale[#]]&/@{(loc - posOffset) + {length, 0},(loc - posOffset) + {length, height}}], {}],
				If[MemberQ[{"Both", "Start"}, arrowPos], 
					Arrow[fRot[fScale[#]]&/@{{-bWidth,0} + (loc - posOffset) + {bCtr length, .5height + offset}, (loc - posOffset) + {0, .5height+offset}}], 
					Line[fRot[fScale[#]]&/@{{-bWidth,0} + (loc - posOffset) + {bCtr length, .5height + offset}, (loc - posOffset) + {0, .5height+offset}}]
				],
				If[MemberQ[{"Both", "End"}, arrowPos], 
					Arrow[fRot[fScale[#]]&/@{{bWidth,0} + (loc - posOffset) + {bCtr length, .5height+offset}, (loc - posOffset) + {length, .5height + offset}}], 
					Line[fRot[fScale[#]]&/@{{bWidth,0} + (loc - posOffset) + {bCtr length, .5height+offset}, (loc - posOffset) + {length, .5height + offset}}]
				],
				If[label =!= None, 
					Text[
						label, fRot[fScale[#]]&/@((loc - posOffset) + {bCtr length, .5height + offset}), labelOffset, 
						Switch[labelRot, True, {1,0}, False, {Cos[-rotate], Sin[-rotate]}, _, {Cos[labelRot], Sin[labelRot]}]
					],
					{}
				]
			},
			
			{
				StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Round"], JoinForm["Round"]}], Arrowheads[arrowSize],
				If[MemberQ[{"Both", "Start"}, showEnd], Line[fRot[fScale[#]]&/@{(loc - posOffset) + {0,0}, (loc - posOffset) + {0, height}}], {}],
				If[MemberQ[{"Both", "End"}, showEnd], Line[fRot[fScale[#]]&/@{(loc - posOffset) + {length,0}, (loc - posOffset) + {length, height}}], {}],
				If[MemberQ[{"Both", "Start"}, arrowPos],
					Arrow[fRot[fScale[#]]&/@{(loc - posOffset) + {-arrowsOutsideLength[[1]], .5height + offset}, (loc - posOffset) + {0, .5height + offset}}],
					Line[fRot[fScale[#]]&/@{(loc - posOffset) + {-arrowsOutsideLength[[1]], .5height + offset}, (loc - posOffset) + {0, .5height + offset}}]
				],
				If[MemberQ[{"Both", "End"}, arrowPos],
					Arrow[fRot[fScale[#]]&/@{(loc - posOffset) + {length + arrowsOutsideLength[[2]], .5height + offset}, (loc - posOffset) + {length, .5height + offset}}],
					Line[fRot[fScale[#]]&/@{(loc - posOffset) + {length + arrowsOutsideLength[[2]], .5height + offset}, (loc - posOffset) + {length, .5height + offset}}]
				],
				If[label =!= None,
					Text[
						label, fRot[fScale[#]]&/@((loc - posOffset) + 
						Switch[OptionValue["BreakWidth"], 
					"End", {length + arrowsOutsideLength[[2]], .5height + offset}, 
					"Start", {-arrowsOutsideLength[[1]], .5height + offset}, _, {.5 length, .5height + offset}]), labelOffset, 
					Switch[labelRot, True, {1, 0}, False, {Cos[-rotate], Sin[-rotate]}, _, {Cos[labelRot], Sin[labelRot]}]],
					{}
				]
			}
	
		]
	]

End[] (* End Private Context *)

EndPackage[]