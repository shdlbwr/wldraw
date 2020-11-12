(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeCurrentSource] = {"Location" -> {0, 0}, "Size" -> .1, "Rotate" -> 0, "LineWeight" -> 1.5, "Color" -> Black, "ColorFill" -> White};

dwShapeCurrentSource[OptionsPattern[]] :=
 
	Module[{loc, scale, rotate, thickness, color, colorFill, rotateTransform},
		{loc, scale, rotate, thickness, color, colorFill} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color", "ColorFill"}];
		rotateTransform = RotationTransform[rotate, loc];
		
		{
			Opacity[1],
			StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Miter"]}],
			FaceForm[{colorFill, Opacity[1]}],
			FilledCurve[{BezierCurve[loc + scale*#&/@{{0,1},{.55,1},{1,.55},{1,0},{1,-.55},{.55,-1},{0,-1},{-.55,-1},{-1,-.55},{-1,0},{-1,.55},{-0.55,1},{0,1}}]}],
			(* arrow *)
			StrokeForm[{color, Opacity[1], AbsoluteThickness[1], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Miter"]}],
			Line[rotateTransform[{loc + scale {-.6, 0}, loc + scale {.6, 0}}]],
			Line[rotateTransform[{loc + scale {.3, .3}, loc + scale {.6, 0}, loc + scale {.3, -.3}}]]
     }]

Options[dwShapeVoltageSource] = {"Location" -> {0, 0}, "Size" -> .1, "Rotate" -> 0, "LineWeight" -> 1.5, "Color" -> Black, "ColorFill" -> White};

dwShapeVoltageSource[OptionsPattern[]] :=
 
	Module[{loc, scale, rotate, thickness, color, colorFill, rotateTransform},
		{loc, scale, rotate, thickness, color, colorFill} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color", "ColorFill"}];
		rotateTransform = RotationTransform[rotate, loc];
		
		{
			Opacity[1],
			StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Miter"]}],
			FaceForm[{colorFill, Opacity[1]}],
			FilledCurve[{BezierCurve[loc + scale*#&/@{{0,1},{.55,1},{1,.55},{1,0},{1,-.55},{.55,-1},{0,-1},{-.55,-1},{-1,-.55},{-1,0},{-1,.55},{-0.55,1},{0,1}}]}],
			(* + - *)
			StrokeForm[{color, Opacity[1], AbsoluteThickness[1], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Miter"]}],
			Line[rotateTransform[loc + scale {0, 0.55}] + scale # &/@ {{-.2, 0}, {.2, 0}}],
			Line[rotateTransform[loc + scale {0, 0.55}] + scale # &/@ {{0, .2}, {0, -.2}}],
			Line[rotateTransform[loc + scale {0, -0.55}] + scale # &/@ {{-.2, 0}, {.2, 0}}]
     }]

Options[dwShapeACVoltageSource] = {"Location" -> {0, 0}, "Size" -> .1, "Rotate" -> 0, "LineWeight" -> 1.5, "Color" -> Black, "ColorFill" -> White};

dwShapeACVoltageSource[OptionsPattern[]] :=
 
	Module[{loc, scale, rotate, thickness, color, colorFill, rotateTransform},
		{loc, scale, rotate, thickness, color, colorFill} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color", "ColorFill"}];
		rotateTransform = RotationTransform[rotate, loc];
		
		{
			Opacity[1],
			StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Miter"]}],
			FaceForm[{colorFill, Opacity[1]}],
			FilledCurve[{BezierCurve[loc + scale*#&/@{{0,1},{.55,1},{1,.55},{1,0},{1,-.55},{.55,-1},{0,-1},{-.55,-1},{-1,-.55},{-1,0},{-1,.55},{-0.55,1},{0,1}}]}],
			(* wave *)
			StrokeForm[{color, Opacity[1], AbsoluteThickness[1], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Miter"]}],
			BSplineCurve[rotateTransform[loc + scale # &/@ {{-.75, 0}, {-.4, .66}, {0, 0}, {.4, -.66}, {.75, 0}}]]
     }]

End[] (* End Private Context *)

EndPackage[]