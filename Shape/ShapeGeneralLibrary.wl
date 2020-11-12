(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeNode[]:=
	dwGraphicsToWLDraw[
		Graphics[{
			FaceForm[{Black, Opacity[1]}], 
			StrokeForm[{Black, Opacity[1], AbsoluteThickness[1], AbsoluteDashing[{}], CapForm["Round"], JoinForm["Round"]}],
			FilledCurve[BezierCurve[{{0., 0.01}, {0.0055, 0.01}, {0.01, 0.0055}, {0.01, 0.}, {0.01, -0.0055}, {0.0055, -0.01}, {0., -0.01}, {-0.0055, -0.01}, {-0.01, -0.0055}, {-0.01, 0.}, {-0.01, 0.0055}, {-0.0055, 0.01}, {0., 0.01}}]]
		}]
	]

dwShapeOpenNode[]:=
	dwGraphicsToWLDraw[
		Graphics[{
			FaceForm[{White, Opacity[1]}], 
			StrokeForm[{Black, Opacity[1], AbsoluteThickness[1], AbsoluteDashing[{}], CapForm["Round"], JoinForm["Round"]}],
			FilledCurve[BezierCurve[{{0., 0.01}, {0.0055, 0.01}, {0.01, 0.0055}, {0.01, 0.}, {0.01, -0.0055}, {0.0055, -0.01}, {0., -0.01}, {-0.0055, -0.01}, {-0.01, -0.0055}, {-0.01, 0.}, {-0.01, 0.0055}, {-0.0055, 0.01}, {0., 0.01}}]]
		}]
	]

End[] (* End Private Context *)

EndPackage[]
