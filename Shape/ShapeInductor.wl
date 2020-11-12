(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeInductor]= {"Location"->{0, 0}, "Size"->.1, "Rotate"->0, "LineWeight"->1.5, "Color"->Black, "NumberOfCoils"->4};

dwShapeInductor[OptionsPattern[]] := 
 Module[{loc, scale, rotate, thickness, color, numCoils, rotateTransform, top, bottom, quantity}, 
 	
	{loc, scale, rotate, thickness, color, numCoils} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color", "NumberOfCoils"}];
  top = {{0, 0}, {0, 0.44}, {0.225, 0.8}, {0.5, 0.8}, {0.775, 0.8}, {1, 0.44}, {1, 0}};
  bottom = {{1, -0.275}, {0.91, -0.5}, {0.8, -0.5}, {0.69, -0.5}, {0.6, -0.275}};
  quantity = Max[numCoils, 2];
  rotateTransform = RotationTransform[rotate, loc];
  
  {Opacity[1], StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Miter"]}],
   BezierCurve[rotateTransform[
     .75 scale Join[
        Flatten[Table[
          Join[
           {n .6, 0} + # &/@ top,
           If[n < quantity - 1, {n .6, 0} + # &/@ bottom, {}]
           ],
          {n, 0, quantity - 1}],
          1]
       ]
     ]]
   }]

Options[dwShapeInductorCoil]= {"Location"->{0, 0}, "Size"->.1, "Rotate"->0, "LineWeight"->1.5, "Color"->Black, "Height"->1, "NumberOfCoils"->4};

dwShapeInductorCoil[OptionsPattern[]] := 
 Module[{loc, scale, rotate, thickness, color, height, numCoils, rotateTransform, ht, top, bottom, quantity}, 
 	
 	{loc, scale, rotate, thickness, color, height, numCoils} = OptionValue[{"Location", "Size", "Rotate", "LineWeight", "Color", "Height", "NumberOfCoils"}];
  top = {{0, 0}, {0, 0.44}, {0.225, 0.8}, {0.5, 0.8}, {0.775, 0.8}, {1, 0.44}, {1, 0}};
  bottom = {{1, -0.275}, {0.91, -0.5}, {0.8, -0.5}, {0.69, -0.5}, {0.6, -0.275}};
  quantity = Max[numCoils, 2];
  ht = Max[height, 0];
  rotateTransform = RotationTransform[rotate, loc];
  
  {Opacity[1], StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Butt"], JoinForm["Miter"]}],
   BezierCurve[rotateTransform[
     .75 scale Join[
       If[ht > 0, Table[loc + {0, 0}, 3], {}],
        Flatten[Table[
          Join[
           {n .6, height} + # &/@ top,
           If[n < quantity - 1, {n .6, height} + # &/@ bottom, {}]
           ],
          {n, 0, quantity - 1}],
          1],
       If[ht > 0, Table[loc + {.6 (quantity - 1 ) + top[[-1, 1]], 0}, 3], {}]
       ]
     ]]
   }]

End[] (* End Private Context *)

EndPackage[]
