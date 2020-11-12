(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeTicks] = {
	"Location"->{0, 0}, 
	"Size"->.5, 
	"LineWeight"->1, 
	"Color"->Black,
	"BeginRadialAxis"->0,
	"EndRadialAxis"->2Pi,
	"Axis"->False,
	"RadialAxis"->True,
	"TickDivisions"->{10, 10},
	"TickFirstLabelValue"->{1, 1},
	"TickLabels"->True,
	"TickLabelPadding"->{0.035, 0.0},
	"TickLength"->.25,
	"MinorTickLength"->.125,
	"TickSide"->"Center",
	"Font"->"Source Sans Pro",
	"FontSize"->11
	};

dwShapeTicks[OptionsPattern[]]:=
 
	Module[{loc, size, thickness, color, axis, radialAxis, beginRadialAxis, endRadialAxis, minorTickLength, 
		tickDivisions, tickFirstLabelValue, tickLabels, tickLabelPadding, tickLength, tickSide, font, fontSize},
		
		{loc, size, thickness, color, axis, radialAxis, beginRadialAxis, endRadialAxis, minorTickLength, tickDivisions, 
			tickFirstLabelValue, tickLabels, tickLabelPadding, tickLength, tickSide, font, fontSize} = 
			OptionValue[{"Location", "Size", "LineWeight", "Color", "Axis", "RadialAxis", "BeginRadialAxis", "EndRadialAxis", "MinorTickLength", 
				"TickDivisions", "TickFirstLabelValue", "TickLabels", "TickLabelPadding", "TickLength", "TickSide", "Font", "FontSize"}];\
		
		If[radialAxis && (endRadialAxis - beginRadialAxis) >= Pi/12,
			
			(* radial *)
			{
				Opacity[1],
				StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Round"]}],
				If[axis,
					Line[Table[size{Sin[x], Cos[x]}, {x, beginRadialAxis, endRadialAxis, Pi/360}]],
					{}
				],
				Switch[tickSide,
					"Inside",
						{
							If[(endRadialAxis - beginRadialAxis) == 2Pi, Most, Sequence]@Table[Line[loc+size #&/@{{Sin[x], Cos[x]},{Sin[x]-Sin[x]*tickLength, Cos[x]-Cos[x]*tickLength}}],{x, beginRadialAxis, endRadialAxis, (endRadialAxis - beginRadialAxis)/tickDivisions[[1]]}],
							Delete[
								If[(endRadialAxis - beginRadialAxis) == 2Pi, Most, Sequence]@Table[Line[loc+size #&/@{{Sin[x], Cos[x]},{Sin[x]-Sin[x]*minorTickLength, Cos[x]-Cos[x]*minorTickLength}}],{x, beginRadialAxis, endRadialAxis, (endRadialAxis - beginRadialAxis)/Times[Sequence@@tickDivisions]}],
								Table[{n}, {n, 1, Times[Sequence@@tickDivisions], tickDivisions[[2]]}]
							],
							(* labels *)
							If[tickLabels,
								Table[
									Inset[
										Style[ToString[tickFirstLabelValue[[1]](n-1)], FontFamily->font, FontColor->color, FontSize->fontSize],
										loc+size*{
											Sin[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]-(Sin[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]*tickLength*(1+10tickLabelPadding[[1]])), 
											Cos[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]-(Cos[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]*tickLength*(1+10tickLabelPadding[[1]]))
											},
										{0, 0}
									],{n, tickDivisions[[1]]+If[(endRadialAxis - beginRadialAxis) == 2Pi, 0, 1]}],
								{}
							]
						},
					"Outside",
						{
							If[(endRadialAxis - beginRadialAxis) == 2Pi, Most, Sequence]@Table[Line[loc+size #&/@{{Sin[x]+Sin[x]*tickLength, Cos[x]+Cos[x]*tickLength},{Sin[x], Cos[x]}}],{x, beginRadialAxis, endRadialAxis, (endRadialAxis - beginRadialAxis)/tickDivisions[[1]]}],
							Delete[
								If[(endRadialAxis - beginRadialAxis) == 2Pi, Most, Sequence]@Table[Line[loc+size #&/@{{Sin[x]+Sin[x]*minorTickLength, Cos[x]+Cos[x]*minorTickLength},{Sin[x], Cos[x]}}],{x, beginRadialAxis, endRadialAxis, (endRadialAxis - beginRadialAxis)/Times[Sequence@@tickDivisions]}],
								Table[{n}, {n, 1, Times[Sequence@@tickDivisions], tickDivisions[[2]]}]
							],
							(* labels *)
							If[tickLabels,
								Table[
									Inset[
										Style[ToString[tickFirstLabelValue[[1]](n-1)], FontFamily->font, FontColor->color, FontSize->fontSize],
										loc+size*{
											Sin[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]+(Sin[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]*tickLength*(1+10tickLabelPadding[[1]])), 
											Cos[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]+(Cos[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]*tickLength*(1+10tickLabelPadding[[1]]))
											},
										{0, 0}
									],{n, tickDivisions[[1]]+If[(endRadialAxis - beginRadialAxis) == 2Pi, 0, 1]}],
								{}
							]
						},
					_, (* center *)
						{
							If[(endRadialAxis - beginRadialAxis) == 2Pi, Most, Sequence]@Table[Line[loc+size #&/@{{Sin[x]+Sin[x]*.5tickLength, Cos[x]+Cos[x]*.5tickLength},{Sin[x]-Sin[x]*.5tickLength, Cos[x]-Cos[x]*.5tickLength}}],{x, beginRadialAxis, endRadialAxis, (endRadialAxis - beginRadialAxis)/tickDivisions[[1]]}],
							Delete[
								If[(endRadialAxis - beginRadialAxis) == 2Pi, Most, Sequence]@Table[Line[loc+size #&/@{{Sin[x]+Sin[x]*.5minorTickLength, Cos[x]+Cos[x]*.5minorTickLength},{Sin[x]-Sin[x]*.5minorTickLength, Cos[x]-Cos[x]*.5minorTickLength}}],{x, beginRadialAxis, endRadialAxis, (endRadialAxis - beginRadialAxis)/Times[Sequence@@tickDivisions]}],
								Table[{n}, {n, 1, Times[Sequence@@tickDivisions], tickDivisions[[2]]}]
							],
						(* labels *)
						If[tickLabels,
							Table[
								Inset[
									Style[ToString[tickFirstLabelValue[[1]](n-1)], FontFamily->font, FontColor->color, FontSize->fontSize],
									loc+size*{
										Sin[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]-(Sin[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]*.5tickLength*(1+20tickLabelPadding[[1]])), 
										Cos[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]-(Cos[beginRadialAxis+(n-1)((endRadialAxis - beginRadialAxis)/tickDivisions[[1]])]*.5tickLength*(1+20tickLabelPadding[[1]]))
										},
									{0, 0}
								],{n, tickDivisions[[1]]+If[(endRadialAxis - beginRadialAxis) == 2Pi, 0, 1]}],
							{}
						]
						}
				]
			},
			
			(* linear *)
			{
				Opacity[1],
				StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Round"]}],
				If[axis,
					Line[{{-size, 0}, {size, 0}}],
					{}
				],
				Switch[tickSide,
					"Inside",
						{
							Table[Line[loc+size #&/@{{x, 0},{x, -tickLength}}],{x, -1, 1, 2/tickDivisions[[1]]}],
							Delete[
								Table[Line[loc+size #&/@{{x, 0},{x, -minorTickLength}}],{x, -1, 1, 2/Times[Sequence@@tickDivisions]}],
								Table[{n}, {n, 1, Times[Sequence@@tickDivisions], tickDivisions[[2]]}]
							],
							(* labels *)
							If[tickLabels,
								Table[
									Inset[
										Style[ToString[tickFirstLabelValue[[1]](n-1)], FontFamily->font, FontColor->color, FontSize->fontSize],
										loc+{size*((n-1)(2/tickDivisions[[1]]))-size, -size*(tickLength+tickLabelPadding[[1]])},
										{0,1}
									],{n, tickDivisions[[1]]+1}],
								{}
							]
						},
					"Outside",
						{
							Table[Line[loc+size #&/@{{x, 0},{x, tickLength}}],{x, -1, 1, 2/tickDivisions[[1]]}],
							Delete[
								Table[Line[loc+size #&/@{{x, 0},{x, minorTickLength}}],{x, -1, 1, 2/Times[Sequence@@tickDivisions]}],
								Table[{n}, {n, 1, Times[Sequence@@tickDivisions], tickDivisions[[2]]}]
							],
							(* labels *)
							If[tickLabels,
								Table[
									Inset[
										Style[ToString[tickFirstLabelValue[[1]](n-1)], FontFamily->font, FontColor->color, FontSize->fontSize],
										loc+{size*((n-1)(2/tickDivisions[[1]]))-size, size*(tickLength+tickLabelPadding[[1]])},
										{0,-1}
									],{n, tickDivisions[[1]]+1}],
								{}
							]
						},
					_, (* center *)
						{
							Table[Line[loc+size #&/@{{x, .5tickLength},{x, -.5tickLength}}],{x, -1, 1, 2/tickDivisions[[1]]}],
							Delete[
								Table[Line[loc+size #&/@{{x, .5minorTickLength},{x, -.5minorTickLength}}],{x, -1, 1, 2/Times[Sequence@@tickDivisions]}],
								Table[{n}, {n, 1, Times[Sequence@@tickDivisions], tickDivisions[[2]]}]
							],
							(* labels *)
							If[tickLabels,
								Table[
									Inset[
										Style[ToString[tickFirstLabelValue[[1]](n-1)], FontFamily->font, FontColor->color, FontSize->fontSize],
										loc+{size*((n-1)(2/tickDivisions[[1]]))-size, -size*(.5tickLength+tickLabelPadding[[1]])},
										{0,1}
									],{n, tickDivisions[[1]]+1}],
								{}
							]
						}
				]
			}
		
		]
	]

End[] (* End Private Context *)

EndPackage[]