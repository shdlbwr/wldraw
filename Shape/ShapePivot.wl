(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapePivot] = {"Location"->{0, 0}, "Size"->.1, "Rotate"->0, "LineWeight"->1, "Color"->GrayLevel[.8]};

dwShapePivot[OptionsPattern[]]:=
	Block[{loc, scale, rotate, thickness, color, rotateTransform},
		{loc, scale, rotate, thickness, color} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color"}];
		
		rotateTransform = RotationTransform[rotate, loc];
		
		{
			Opacity[1],
			(* pivot *)
			{
				FaceForm[{color, Opacity[1]}], 
				StrokeForm[{Black, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}]}],
				Polygon[
					rotateTransform[Flatten[{
						{loc + scale {.5, -1}, loc + scale {-.5, -1}, loc + scale {-.5, 0}}, 
						Table[loc + .5 scale {Sin[x], Cos[x]}, {x, -Pi/2, Pi/2, Pi/20}]
					}, 1]]
				]
			},{
				Black, PointSize[Medium], Point[{loc + scale {0, 0}}]
			},
			(* base *)
			{
				FaceForm[{color, Opacity[1]}],
				StrokeForm[{Black, Opacity[0]}], 
				Polygon[rotateTransform[{loc + scale {-1, -1.75}, loc + scale {1, -1.75}, loc + scale {1, -1}, loc + scale {-1, -1}}]]
			},{
       			StrokeForm[{Black, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Round"]}],
       			Line[rotateTransform[{loc + scale {-1, -1}, loc + scale {1, -1}}]]
   			}
		}
     ]

End[] (* End Private Context *)

EndPackage[]