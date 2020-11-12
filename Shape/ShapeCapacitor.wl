(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeCapacitor] = {"Location" -> {0, 0}, "Size" -> .1, "Rotate" -> 0, "LineWeight" -> 1.5, "Color" -> Black};

dwShapeCapacitor[OptionsPattern[]] :=
 
	Module[{loc, scale, rotate, thickness, color, rotateTransform},
		{loc, scale, rotate, thickness, color} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color"}];
		rotateTransform = RotationTransform[rotate, loc];
		
		{
			Opacity[1],
			StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Miter"]}],
			Line[rotateTransform[{loc + scale {-1, .25}, loc + scale {1, .25}}]],
			Line[rotateTransform[{loc + scale {-1, -.25}, loc + scale {1, -.25}}]]
		}
	]

End[] (* End Private Context *)

EndPackage[]