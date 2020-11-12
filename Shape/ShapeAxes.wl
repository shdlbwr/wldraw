(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeAxes]={"Location"->{-1,-1},"Size"->2,"LineWeight"->1,"Color"->Black,"TickDivisions"->{10,10},"AxisOrigin"->{0,0},"TickFirstLabelValue"->{1,1},"AxisLabels"->{"x","y"},"ArrowSize"->Medium,"Frame"->False,"Grid"->True,"GridOnly"->False,"GridType"->"Grid","GridStyle"->GrayLevel[.8],"Ticks"->True,"TickLabels"->True,"TickLabelPadding"->{0.015,0.015},"TickLength"->.025,"TickSide"->"Inside","Font"->"Source Sans Pro","FontSize"->11};

dwShapeAxes[OptionsPattern[]]:=
Block[{loc,size,lineWeight,color,origin,labels,arrowSize,divisions,frame,grid,gridOnly,gridType,gridStyle,ticks,tickLabels,tickLabelPadding,tickFirstLabelValue,tickLength,tickSide,font,fontSize,labelPosition,vertAxisLabelOffset=0,divSize,yCounter},
{loc,size,lineWeight,color,origin,labels,arrowSize,divisions,frame,grid,gridOnly,gridType,gridStyle,ticks,tickLabels,tickLabelPadding,tickFirstLabelValue,tickLength,tickSide,font,fontSize}=OptionValue[{"Location","Size","LineWeight","Color","AxisOrigin","AxisLabels","ArrowSize","TickDivisions","Frame","Grid","GridOnly","GridType","GridStyle","Ticks","TickLabels","TickLabelPadding","TickFirstLabelValue","TickLength","TickSide","Font","FontSize"}];
	{
		divSize = #/Max[divisions]&/@divisions;(* use {1,1} to fill -1 to 1 space *)
		origin=Round[origin,1/divisions];
		If[grid || gridOnly,
			Switch[gridType,
				"Point",
					{gridStyle, Point[Flatten[Table[
						Which[
							x == origin[[1]] || y == origin[[2]],
								{},
							frame && MemberQ[{0,1}, x|y],
								{},
							True,
								loc+size{x,y}
						], {x,0,divSize[[1]],divSize[[1]]/divisions[[1]]}, {y,0,divSize[[2]],divSize[[2]]/divisions[[2]]}], 1]/.{{}->Sequence[]}]
					},
				"Horizontal",
					Flatten[{StrokeForm[gridStyle],
						Table[Line[loc+size #&/@{{0,y},{divSize[[1]],y}}],{y,0,divSize[[2]],divSize[[2]]/divisions[[2]]}]
					}],
				"Vertical",
					Flatten[{StrokeForm[gridStyle],
						Table[Line[loc+size #&/@{{x,0},{x,divSize[[2]]}}],{x,0,divSize[[1]],divSize[[1]]/divisions[[1]]}]
					}],
				"XBar",
					{
						{StrokeForm[Opacity[0]],FaceForm[Lighter[gridStyle,.2]], Polygon[loc+size #&/@{{0,0},{divSize[[1]],0},{divSize[[1]],divSize[[2]]},{0,divSize[[2]]}}]},
						Table[{
							StrokeForm[Opacity[0]], FaceForm[gridStyle], Polygon[loc+size #&/@{{0,y},{divSize[[1]],y},{divSize[[1]],y+divSize[[2]]/divisions[[2]]},{0,y+divSize[[2]]/divisions[[2]]}}]
						},{y,0,divSize[[2]]-divSize[[2]]/divisions[[2]],2divSize[[2]]/divisions[[2]]}]
					},
				"YBar",
					{
						{StrokeForm[Opacity[0]],FaceForm[Lighter[gridStyle,.2]], Polygon[loc+size #&/@{{0,0},{divSize[[1]],0},{divSize[[1]],divSize[[2]]},{0,divSize[[2]]}}]},
						Table[{
							StrokeForm[Opacity[0]], FaceForm[gridStyle], Polygon[loc+size #&/@{{x,0},{x,divSize[[1]]},{x+divSize[[1]]/divisions[[1]],divSize[[2]]},{x+divSize[[1]]/divisions[[1]],0}}]
						},{x,0,divSize[[1]]-divSize[[1]]/divisions[[1]],2divSize[[1]]/divisions[[1]]}]
					},
				_,
					(* standard grid *)
					Flatten[{StrokeForm[gridStyle],
						Table[Line[loc+size #&/@{{x,0},{x,divSize[[2]]}}],{x,0,divSize[[1]], divSize[[1]]/divisions[[1]]}],
						Table[Line[loc+size #&/@{{0,y},{divSize[[1]],y}}],{y,0,divSize[[2]], divSize[[2]]/divisions[[2]]}]
					}]
			],
			{}
		],
		If[ticks && !gridOnly,
			Flatten[{StrokeForm[{color,AbsoluteThickness[Max[.5lineWeight,.5]]}],
				If[tickSide==="Center",
					{
						Table[Line[loc+size #&/@{{x,origin[[2]]-.5tickLength/size},{x,origin[[2]]+.5tickLength/size}}],{x,0,divSize[[1]]-divSize[[1]]/divisions[[1]],divSize[[1]]/divisions[[1]]}],
						Table[Line[loc+size #&/@{{origin[[1]]-.5tickLength/size,y},{origin[[1]]+.5tickLength/size,y}}],{y,0,divSize[[2]]-divSize[[2]]/divisions[[2]],divSize[[2]]/divisions[[2]]}]
					},
					{
						If[grid && gridType === "Vertical",
							{},
							Table[Line[loc+size #&/@{{x,origin[[2]]},{x,origin[[2]]+If[tickSide==="Inside",1,-1]tickLength/size}}],{x,0,divSize[[1]]-divSize[[1]]/divisions[[1]],divSize[[1]]/divisions[[1]]}]
						],
						If[grid && gridType === "Horizontal",
							{},
							Table[Line[loc+size #&/@{{origin[[1]],y},{origin[[1]]+If[tickSide==="Inside",1,-1]tickLength/size,y}}],{y,0,divSize[[2]]-divSize[[2]]/divisions[[2]],divSize[[2]]/divisions[[2]]}]
						]
					}
				]
			}],
			{}
		],
		If[tickLabels && !gridOnly,
			labelPosition =
				If[ticks,
					Switch[tickSide, "Center", origin-.5tickLength/size, "Outside", origin-tickLength/size, _, origin],
					origin
				];
			Flatten[{
				(* horizontal *)
				Table[If[origin[[1]]!=(n-1)(divSize[[1]]/divisions[[1]]),
					Inset[Style[ToString[tickFirstLabelValue[[1]](n-(origin[[1]]*divisions[[1]])-1)],FontFamily->font,FontColor->color,FontSize->fontSize],loc+{size(n-1)(divSize[[1]]/divisions[[1]]),size*labelPosition[[2]]-tickLabelPadding[[1]]},{0,1}]
				],{n,If[frame && origin[[2]] != 0, Range[2,divisions[[1]]], divisions[[1]] + If[grid && gridType === "Vertical", 1, 0]]}],
				(* vertical *)
				Table[If[origin[[2]]!=(n-1)(divSize[[2]]/divisions[[2]]),
					Inset[Style[ToString[tickFirstLabelValue[[2]](n-(origin[[2]]*divisions[[2]])-1)],FontFamily->font,FontColor->color,FontSize->fontSize],loc+{size*labelPosition[[1]]-tickLabelPadding[[2]],size(n-1)(divSize[[2]]/divisions[[2]])},{1,vertAxisLabelOffset}]
				],{n,If[frame && origin[[1]] != 0, Range[2,divisions[[2]]], divisions[[2]] + If[grid && gridType === "Horizontal", 1, 0]]}],
				(* origin zero *)
				Switch[origin,
					{0,0},
						Inset[Style["0",FontFamily->font,FontColor->color,FontSize->fontSize],loc+{size*labelPosition[[1]],size*labelPosition[[2]]}-Reverse[tickLabelPadding],{1,1}],
					{_,0},
						Inset[Style["0",FontFamily->font,FontColor->color,FontSize->fontSize],loc+{size*origin[[1]],size*labelPosition[[2]]-tickLabelPadding[[1]]},{0,1}],
					{0,_},
						Inset[Style["0",FontFamily->font,FontColor->color,FontSize->fontSize],loc+{size*labelPosition[[1]]-tickLabelPadding[[2]],size*origin[[2]]},{1,vertAxisLabelOffset}],
					_,
						Inset[Style["0",FontFamily->font,FontColor->color,FontSize->fontSize],loc+{size*labelPosition[[1]],size*labelPosition[[2]]}-Reverse[tickLabelPadding],{1,1}]
				]
			}],
			{}
		],
		If[!gridOnly,
			{
				StrokeForm[{color,AbsoluteThickness[lineWeight]}],Arrowheads[arrowSize],
				If[grid && gridType === "Vertical", {}, Arrow[loc+size #&/@{{0,origin[[2]]},{divSize[[1]],origin[[2]]}}]],
				If[grid && gridType === "Horizontal", {}, Arrow[loc+size #&/@{{origin[[1]],0},{origin[[1]],divSize[[2]]}}]],
				If[frame,Line[loc+size#&/@{{0,0},{divSize[[1]],0},divSize,{0,divSize[[2]]},{0,0}}],{}]
			},
			{}
		],
		If[!gridOnly,
			{
				If[labels=!="",Inset[Style[labels[[1]],FontColor->color],loc+size{divSize[[1]]+.02,origin[[2]]},{-1,0}]],
				If[labels=!="",Inset[Style[labels[[2]],FontColor->color],loc+size{origin[[1]],divSize[[2]]},{0,-1}]]
			},
			{}
		]
	}
]

End[] (* End Private Context *)

EndPackage[]