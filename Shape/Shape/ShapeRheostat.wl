(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeRheostat] = {"Location"->{0, 0}, "Size"->.1, "Rotate"->0, "LineWeight"->1.5, "Color"->Black, "Width"->1/3, "NumberOfTurns"->3, "ArrowSize"->Medium};

dwShapeRheostat[OptionsPattern[]]:=
 
	Module[{loc, scale, rotate, thickness, color, width, turns, arrowsize, rotateTransform},
		
		{loc, scale, rotate, thickness, color, width, turns, arrowsize} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color", "Width", "NumberOfTurns", "ArrowSize"}];
		rotateTransform = RotationTransform[rotate, loc];
		
		{
			{
				StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Round"], JoinForm["Miter"]}],
				Line[rotateTransform[{-scale width ((2 turns - 1)/2), 0} + #&/@Join[
						{loc + scale {-.5 width, 0}}, 
						Flatten[Table[{loc + scale {width (n - 1), .5}, loc + scale {width n, -.5}}, {n, 1, 2 turns, 2}], 1], 
						{loc + scale {width 2 turns - .5 width, 0}}
					]]
				]
			},
			{
				Arrowheads[arrowsize],
				StrokeForm[{color, Opacity[1], AbsoluteThickness[1], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Round"]}],
				Arrow[rotateTransform[{-scale width ((2 turns - 1)/2), 0} + #&/@Join[
						{loc + scale {-.5 width, -1.25}}, 
						{loc + scale {width 2 turns - .5 width, 1.25}}
					]]]
			}
		}
	]

End[] (* End Private Context *)

EndPackage[]