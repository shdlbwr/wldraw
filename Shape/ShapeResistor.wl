(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeResistor] = {"Location"->{0, 0}, "Size"->.1, "Rotate"->0, "LineWeight"->1.5, "Color"->Black, "Width"->1/3, "NumberOfTurns"->3};

dwShapeResistor[OptionsPattern[]]:=
 
	Module[{loc, scale, rotate, thickness, color, width, turns, rotateTransform},
		
		{loc, scale, rotate, thickness, color, width, turns} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color", "Width", "NumberOfTurns"}];
		rotateTransform = RotationTransform[rotate, loc];
		
		{
			Opacity[1],
			StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Round"], JoinForm["Miter"]}],
			Line[rotateTransform[{-scale width ((2 turns - 1)/2), 0} + #&/@Join[
					{loc + scale {-.5 width, 0}}, 
					Flatten[Table[{loc + scale {width (n - 1), .5}, loc + scale {width n, -.5}}, {n, 1, 2 turns, 2}], 1], 
					{loc + scale {width 2 turns - .5 width, 0}}
				]]
			]
		}
	]

End[] (* End Private Context *)

EndPackage[]