(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeGround] = {"Location"->{0, 0}, "Size"->.075, "Rotate"->0, "LineWeight"->1.5, "Color"->Black};

dwShapeGround[OptionsPattern[]]:=
 
	Module[{loc, scale, rotate, thickness, color, rotateTransform},
		
		{loc, scale, rotate, thickness, color} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color"}];
		rotateTransform = RotationTransform[rotate, loc];
		
		{
			Opacity[1],
			StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Round"]}],
			Array[Line[rotateTransform[{loc + scale {-#/3, .5 (# - 3)}, loc + scale {#/3, .5 (# - 3)}}]]&, 3]
		}
	]

End[] (* End Private Context *)

EndPackage[]