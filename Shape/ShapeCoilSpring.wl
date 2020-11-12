(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeCoilSpring]={"Location"->{0,0},"Size"->.3,"Rotate"->0,"Force"->0,"Segments"->4,"Height"->.2,"EndLength"->.2,"Thickness"->4,"LineWeight"->1,"Color"->Black};

dwShapeCoilSpring[OptionsPattern[]]:=
Block[{f,s,e,loc,scale,rotate,color,force,segments,height,endLength,thickness,borderThickness,rotateTransform},
	{loc,scale,rotate,color,force,segments,height,endLength,thickness,borderThickness}=OptionValue[{"Location","Size","Rotate","Color","Force","Segments","Height","EndLength","Thickness","LineWeight"}];
	
	(* use mouseclicks as input *)
	If[$dwShowMouseClickPositions,
		If[$dwPreviousClickPtNoGrid =!= Null && $dwClickPtNoGrid =!= Null,
			loc = Round[$dwPreviousClickPtNoGrid, $dwGridStep];
			rotate = (Quiet@ToPolarCoordinates[Subtract[Sequence@@(#-{0,0}&/@Round[{$dwClickPtNoGrid,$dwPreviousClickPtNoGrid}, $dwGridStep])]]/.{Indeterminate->0})[[2]];
			force = (EuclideanDistance[Round[$dwClickPtNoGrid, $dwGridStep],Round[$dwPreviousClickPtNoGrid, $dwGridStep]])/scale - 1
		]
	];
	
	rotateTransform = RotationTransform[rotate, loc];
	f=force+1;
	s=(f-2endLength)/(segments+.5);
	e=endLength+s/4;
	{
		(* color outline *)
		{
			StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Round"]}],
			Line[rotateTransform[Flatten[{{loc+scale{0,0},loc+scale{ endLength,0}},
			Flatten[Table[{loc+scale{ e+a s,-height},loc+scale{e+a s+.5s,height}},{a,0,segments-1}],1],
			{loc+scale{f-e,-height},loc+scale{f-endLength,0},loc+scale{f,0}}},1]]]
		},
		(* white filling *)
		Flatten@{
			StrokeForm[{White, Opacity[1], AbsoluteThickness[thickness-2borderThickness], AbsoluteDashing[{}], CapForm["Round"], JoinForm["Round"]}],
			Line[rotateTransform[{loc+{0,0},loc+scale{ endLength,0}}]],
			Table[Line[rotateTransform[{loc+scale{ e+a s,-height},loc+scale{e+a s+.5s,height}}]],{a,0,segments-1}],
			Line[rotateTransform[{loc+scale{f-e,-height},loc+scale{f-endLength,0}}]],Line[rotateTransform[{loc+scale{f-endLength,0},loc+scale{f,0}}]]
		}
	}
]

End[] (* End Private Context *)

EndPackage[]