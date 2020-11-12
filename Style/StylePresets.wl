(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwStylePresets[]:=
	DynamicModule[{sel, arrowPosition, temp, palette},
	
		Dynamic@ActionMenu[Style["PRESETS", 8],
			If[$dwSelected === {},
				{},
			sel = If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected];
			(* presets *)
			Switch[$dwHead[[sel[[1]]]],
				
				Image,
					{
						"Vines":> (
							Do[
								If[MemberQ[{Image}, $dwHead[[n]]], 
									$dwStyle[[n, 4]] = {{RidgeFilter, 3., 0, 32, GrayLevel[0], 0}},
									Nothing
								], 
							{n, sel}]),
						"Vines gray":> (
							Do[
								If[MemberQ[{Image}, $dwHead[[n]]], 
									$dwStyle[[n, 4]] = {{"Embossing", 16., 0, 16, GrayLevel[0], 0}, {RidgeFilter, 2., 0, 32, GrayLevel[0], 0}},
									Nothing
								], 
							{n, sel}])
					},
				Text,
					{
						"default":>
							(Do[If[$dwHead[[n]] === Text, $dwStyle[[n]] = $dwDefaultTextStyle], {n, sel}]),
							
						Delimiter,
						Style["Black", Black]:>
							(Do[If[$dwHead[[n]] === Text, $dwStyle[[n, Flatten[Position[$dwStyle[[n]], FontColor]][[1]]]][[2]] = Black], {n, sel}]),
						Style["Gray", Black]:>
							(Do[If[$dwHead[[n]] === Text, $dwStyle[[n, Flatten[Position[$dwStyle[[n]], FontColor]][[1]]]][[2]] = Gray], {n, sel}]),
						Style["White", Black]:>
							(Do[If[$dwHead[[n]] === Text, $dwStyle[[n, Flatten[Position[$dwStyle[[n]], FontColor]][[1]]]][[2]] = White], {n, sel}]),
							
						Delimiter,
						"No rotation":>
							(Do[If[$dwHead[[n]] === Text, $dwStyle[[n,1]] = 0 Degree], {n, $dwSelected}]),
						"rotate 45 degrees":>
							(Do[If[$dwHead[[n]] === Text, $dwStyle[[n,1]] = 45 Degree], {n, $dwSelected}]),
						"rotate 90 degrees":>
							(Do[If[$dwHead[[n]] === Text, $dwStyle[[n,1]] = 90 Degree], {n, $dwSelected}]),
						"rotate 180 degrees":>
							(Do[If[$dwHead[[n]] === Text, $dwStyle[[n,1]] = 180 Degree], {n, $dwSelected}])
					},
				_,
					{
						"default":>
							(Do[If[$dwHead[[n]] =!= Text, $dwStyle[[n]] = $dwFullDefaultStyle], {n, sel}]),
						
						Delimiter,
						
						$dwIconPresetEdgeAndFill:> (
							Do[
								If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] = 1;
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1,
									Nothing
								], 
							{n, sel}]),
							
						$dwIconPresetNoEdge:> (
							Do[
								If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] = 0;
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1,
									Nothing
								], 
							{n, sel}]),
						
						$dwIconPresetNoFill:> (
							Do[
								If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] = 1;
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 0,
									Nothing
								], 
							{n, sel}]),
							
						Delimiter, (* arrowheads *)

						Graphics[{AbsoluteThickness[1], Arrowheads[Medium], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									If[MemberQ[{Polygon, Point}, $dwHead[[n]]], $dwHead[[n]] = Arrow];
									$dwStyle[[n, 1]] = False; (* turn fill off *)
									$dwStyle[[n, 2]] = True; (* turn arrowhead on *)
									(* arrowhead start location *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], Arrowheads[_]]][[1]]]][[1]] = 
										If[Head[$dwStyle[[n,3]]] === Graphics,
											{{0.000001, 0, $dwStyle[[n,6]]}},
											$dwStyle[[n,3]]
										];
									$dwStyle[[n, 4]] = 0; (* arrowhead start location *)
									$dwStyle[[n, 5]] = 1; (* arrowhead start location *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2]] = Opacity[1];
								], {n, sel}];
								$dwStyleMode = "arrow"),

						Graphics[{AbsoluteThickness[1], Arrowheads[{{-Medium,0}}], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									If[MemberQ[{Polygon, Point}, $dwHead[[n]]], $dwHead[[n]] = Arrow];
									$dwStyle[[n, 1]] = False; (* turn fill off *)
									$dwStyle[[n, 2]] = True; (* turn arrowhead on *)
									(* arrowhead start location *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], Arrowheads[_]]][[1]]]][[1]] = 
										If[Head[$dwStyle[[n,3]]] === Graphics,
											{{-0.000001, 0, $dwStyle[[n,6]]}},
											{{-$dwStyle[[n,3]], 0}}
										];
									$dwStyle[[n, 4]] = 0; (* arrowhead start location *)
									$dwStyle[[n, 5]] = 1; (* arrowhead start location *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2]] = Opacity[1];
								], {n, sel}];
								$dwStyleMode = "arrow"),

						Graphics[{AbsoluteThickness[1], Arrowheads[{{-Medium, 0}, {Medium, 1}}], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									If[MemberQ[{Polygon, Point}, $dwHead[[n]]], $dwHead[[n]] = Arrow];
									$dwStyle[[n, 1]] = False; (* turn fill off *)
									$dwStyle[[n, 2]] = True; (* turn arrowhead on *)
									(* arrowhead start location *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], Arrowheads[_]]][[1]]]][[1]] = 
										If[Head[$dwStyle[[n,3]]] === Graphics,
											{{-0.000001, 0, $dwStyle[[n,6]]},{0.000001, 0, $dwStyle[[n,6]]}},
											{{-$dwStyle[[n,3]], 0},{$dwStyle[[n,3]], 1}}
										];
									$dwStyle[[n, 4]] = 0; (* arrowhead start location *)
									$dwStyle[[n, 5]] = 1; (* arrowhead start location *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2]] = Opacity[1];
								], {n, sel}];
								$dwStyleMode = "arrow"),

						Graphics[{AbsoluteThickness[1], Arrowheads[{{Medium,0.65}}], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									If[MemberQ[{Polygon, Point}, $dwHead[[n]]], $dwHead[[n]] = Arrow];
									$dwStyle[[n, 1]] = False; (* turn fill off *)
									$dwStyle[[n, 2]] = True; (* turn arrowhead on *)
									(* arrowhead start location *)
									arrowPosition = dwCalculatePathPositionByObjectSize[n, "Position"->0.5, "ObjectDirection"->1, "ObjectSizeInPrinterPoints"->5];
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], Arrowheads[_]]][[1]]]][[1]] = 
										If[Head[$dwStyle[[n,3]]] === Graphics,
											{{0.000001, arrowPosition, $dwStyle[[n,6]]}},
											{{$dwStyle[[n,3]], arrowPosition}}
										];
									$dwStyle[[n, 4]] = 0; (* arrowhead start location *)
									$dwStyle[[n, 5]] = arrowPosition; (* arrowhead start location *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2]] = Opacity[1];
								], {n, sel}];
								$dwStyleMode = "arrow"),

						Graphics[{AbsoluteThickness[1], Arrowheads[{{-Medium,0.35}}], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									If[MemberQ[{Polygon, Point}, $dwHead[[n]]], $dwHead[[n]] = Arrow];
									$dwStyle[[n, 1]] = False; (* turn fill off *)
									$dwStyle[[n, 2]] = True; (* turn arrowhead on *)
									(* arrowhead start location *)
									arrowPosition = dwCalculatePathPositionByObjectSize[n, "Position"->0.5, "ObjectDirection"->-1, "ObjectSizeInPrinterPoints"->5];
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], Arrowheads[_]]][[1]]]][[1]] = 
										If[Head[$dwStyle[[n,3]]] === Graphics,
											{{-0.000001, arrowPosition, $dwStyle[[n,6]]}},
											{{-$dwStyle[[n,3]], arrowPosition}}
										];
									$dwStyle[[n, 4]] = arrowPosition; (* arrowhead start location *)
									$dwStyle[[n, 5]] = 1; (* arrowhead start location *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2]] = Opacity[1];
								], {n, sel}];
								$dwStyleMode = "arrow"),

						Graphics[{AbsoluteThickness[1], Arrowheads[{{Medium,1/3},{Medium,2/3},{Medium,1}}], Arrow[{{0,.1},{1,.1}}]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									If[MemberQ[{Polygon, Point}, $dwHead[[n]]], $dwHead[[n]] = Arrow];
									$dwStyle[[n, 1]] = False; (* turn fill off *)
									$dwStyle[[n, 2]] = True; (* turn arrowhead on *)
									(* arrowhead locations *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], Arrowheads[_]]][[1]]]][[1]] = 
										If[Head[$dwStyle[[n,3]]] === Graphics,
											Table[{0.000001, $dwStyle[[n,4]] + ($dwStyle[[n,5]]-$dwStyle[[n,4]])*(i/$dwStyle[[n,12]]), $dwStyle[[n,6]]}, {i, $dwStyle[[n,12]]}],
											Table[{$dwStyle[[n,3]], $dwStyle[[n,4]] + ($dwStyle[[n,5]]-$dwStyle[[n,4]])*(i/$dwStyle[[n,12]])}, {i, $dwStyle[[n,12]]}]
										];
									$dwStyle[[n, 4]] = 0; (* arrowhead start location *)
									$dwStyle[[n, 5]] = 1; (* arrowhead start location *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2]] = Opacity[1];
								], {n, sel}];
								$dwStyleMode = "arrow"),
							
						Delimiter, (* gradients *)
						
						(* horizontal *)
						Graphics[{Thickness[1],CapForm["Butt"],Line[{{0,.1},{1,.1}},VertexColors->{GrayLevel[.7],GrayLevel[1]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									$dwStyle[[n, 1]] = True; (* turn fill on *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n,8]] = {"Horizontal",{{0,GrayLevel[.7]},{1/3,GrayLevel[.7+(1-.7)/3]},{2/3,GrayLevel[.7+2(1-.7)/3]},{1,GrayLevel[1]}}}; (* gradient *)
									dwUpdateGradients[n]
								], {n, sel}];
								$dwStyleMode = "fill"),
						Graphics[{Thickness[1],CapForm["Butt"],Line[{{0,.1},{1,.1}},VertexColors->{Gray,GrayLevel[.95]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									$dwStyle[[n, 1]] = True; (* turn fill on *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n,8]] = $dwDefaultGradient;
									$dwStyle[[n,8,1]] = "Horizontal";
									$dwStyle[[n,8,2]] = {{0,GrayLevel[.5]},{1/3,GrayLevel[.5+(.95-.5)/3]},{2/3,GrayLevel[.5+2(.95-.5)/3]},{1,GrayLevel[.95]}};
									dwUpdateGradients[n]
								], {n, sel}];
								$dwStyleMode = "fill"),
						Graphics[{Thickness[1],CapForm["Butt"],Line[{{0,.1},{1,.1}},VertexColors->{GrayLevel[.1],GrayLevel[.9]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									$dwStyle[[n, 1]] = True; (* turn fill on *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n,8]] = $dwDefaultGradient;
									$dwStyle[[n,8,1]] = "Horizontal";
									$dwStyle[[n,8,2]] = {{0,GrayLevel[.1]},{1/3,GrayLevel[.1+(.9-.1)/3]},{2/3,GrayLevel[.1+2(.9-.1)/3]},{1,GrayLevel[.9]}};
									dwUpdateGradients[n]
								], {n, sel}];
								$dwStyleMode = "fill"),
						Graphics[{Thickness[1],CapForm["Butt"],Line[{{0,.1},{1,.1}},VertexColors->{GrayLevel[.1],GrayLevel[.75]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									$dwStyle[[n, 1]] = True; (* turn fill on *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n,8]] = $dwDefaultGradient;
									$dwStyle[[n,8,1]] = "Horizontal";
									$dwStyle[[n,8,2]] = {{0,GrayLevel[.1]},{1/3,GrayLevel[.1+(.75-.1)/3]},{2/3,GrayLevel[.1+2(.75-.1)/3]},{1,GrayLevel[.75]}};
									dwUpdateGradients[n]
								], {n, sel}];
								$dwStyleMode = "fill"),
						(* vertical *)
						Graphics[{Thickness[1],CapForm["Butt"],Line[{{.1,0},{.1,1}},VertexColors->{GrayLevel[.7],GrayLevel[1]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									$dwStyle[[n, 1]] = True; (* turn fill on *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n,8]] = $dwDefaultGradient;
									$dwStyle[[n,8,1]] = "Vertical";
									$dwStyle[[n,8,2]] = {{0,GrayLevel[.7]},{1/3,GrayLevel[.7+(1-.7)/3]},{2/3,GrayLevel[.7+2(1-.7)/3]},{1,GrayLevel[1]}};
									dwUpdateGradients[n]
								], {n, sel}];
								$dwStyleMode = "fill"),
						Graphics[{Thickness[1],CapForm["Butt"],Line[{{.1,0},{.1,1}},VertexColors->{Gray,GrayLevel[.95]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									$dwStyle[[n, 1]] = True; (* turn fill on *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n,8]] = $dwDefaultGradient;
									$dwStyle[[n,8,1]] = "Vertical";
									$dwStyle[[n,8,2]] = {{0,GrayLevel[.5]},{1/3,GrayLevel[.5+(.95-.5)/3]},{2/3,GrayLevel[.5+2(.95-.5)/3]},{1,GrayLevel[.95]}};
									dwUpdateGradients[n]
								], {n, sel}];
								$dwStyleMode = "fill"),
						Graphics[{Thickness[1],CapForm["Butt"],Line[{{.1,0},{.1,1}},VertexColors->{GrayLevel[.1],GrayLevel[.9]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									$dwStyle[[n, 1]] = True; (* turn fill on *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n,8]] = $dwDefaultGradient;
									$dwStyle[[n,8,1]] = "Vertical";
									$dwStyle[[n,8,2]] = {{0,GrayLevel[.1]},{1/3,GrayLevel[.1+(.9-.1)/3]},{2/3,GrayLevel[.1+2(.9-.1)/3]},{1,GrayLevel[.9]}};
									dwUpdateGradients[n]
								], {n, sel}];
								$dwStyleMode = "fill"),
						Graphics[{Thickness[1],CapForm["Butt"],Line[{{.1,0},{.1,1}},VertexColors->{GrayLevel[.1],GrayLevel[.75]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]:>
							(	Do[If[$dwHead[[n]] =!= Text,
									$dwStyle[[n, 1]] = True; (* turn fill on *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n,8]] = $dwDefaultGradient;
									$dwStyle[[n,8,1]] = "Vertical";
									$dwStyle[[n,8,2]] = {{0,GrayLevel[.1]},{1/3,GrayLevel[.1+(.75-.1)/3]},{2/3,GrayLevel[.1+2(.75-.1)/3]},{1,GrayLevel[.75]}};
									dwUpdateGradients[n]
								], {n, sel}];
								$dwStyleMode = "fill"),
							
						Delimiter, (* line thickness *)
						
						Graphics[{Text["0.25",{.5,.1},{0,-1}], AbsoluteThickness[0.25], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,3,1]] = 0.25; (* thickness *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
							
						
						Graphics[{Text["0.50",{.5,.1},{0,-1}], AbsoluteThickness[0.5], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,3,1]] = 0.5; (* thickness *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
							
						
						Graphics[{Text["1.00",{.5,.1},{0,-1}], AbsoluteThickness[1], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,3,1]] = 1; (* thickness *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
							
						
						Graphics[{Text["1.50",{.5,.1},{0,-1}], AbsoluteThickness[1.5], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,3,1]] = 1.5; (* thickness *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
						
						Graphics[{Text["2.00",{.5,.1},{0,-1}], AbsoluteThickness[2], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] = 1; (* opacity *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,3,1]] = 2; (* thickness *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
							
						Delimiter, (* line dashing *)

						Graphics[{AbsoluteThickness[1], AbsoluteDashing[{}], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,4,1]] = {}; (* dashing *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,5,1]] = "Butt"; (* capform *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,6,1]] = "Butt"; (* joinform *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
							
						Graphics[{AbsoluteThickness[1], AbsoluteDashing[{18,4}], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,4,1]] = {18,4}; (* dashing *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,5,1]] = "Butt"; (* capform *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,6,1]] = "Butt"; (* joinform *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
							
						Graphics[{AbsoluteThickness[1], AbsoluteDashing[{12,4}], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,4,1]] = {12,4}; (* dashing *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,5,1]] = "Butt"; (* capform *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,6,1]] = "Butt"; (* joinform *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
							
						Graphics[{AbsoluteThickness[1], AbsoluteDashing[{8,3}], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,4,1]] = {8,3}; (* dashing *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,5,1]] = "Butt"; (* capform *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,6,1]] = "Butt"; (* joinform *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
							
						Graphics[{AbsoluteThickness[1], AbsoluteDashing[{4,2}], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,4,1]] = {4,2}; (* dashing *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,5,1]] = "Butt"; (* capform *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,6,1]] = "Butt"; (* joinform *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
							
						Graphics[{AbsoluteThickness[1], AbsoluteDashing[{2,2}], Style[Line[{{0,.1},{1,.1}}],Antialiasing->True]}, ImageSize->{45,Automatic}]:>
							(	Do[If[$dwHead[[n]] =!= Text, 
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,4,1]] = {2,2}; (* dashing *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,5,1]] = "Butt"; (* capform *)
									$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,6,1]] = "Butt"; (* joinform *)
								], {n, sel}];
								$dwStyleMode = "stroke"),
						
						Delimiter,
						"multi-color":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"Standard", "Quantity"->Length[$dwSelected]];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}]),
						"multi-color bright":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"Bright", "Quantity"->Length[$dwSelected]];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}]),
						"multi-color neon":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"Neon", "Quantity"->Length[$dwSelected]];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}]),
						"multi-color LAB":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"LABHarmony", "Quantity"->Length[$dwSelected]];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}]),
						"multi-color pastel":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"Neon", "Quantity"->Length[$dwSelected], "Shade"->.5];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}]),
						
						Delimiter,
						"multi-color discrete":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Equal", "ColorSpace"->"RGB", "ColorSpectrum"->"Standard", "Quantity"->Length[$dwSelected]];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}]),
						"multi-color bright discrete":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Equal", "ColorSpace"->"RGB", "ColorSpectrum"->"Bright", "Quantity"->Length[$dwSelected]];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}]),
						"multi-color neon discrete":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Equal", "ColorSpace"->"RGB", "ColorSpectrum"->"Neon", "Quantity"->Length[$dwSelected]];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}]),
						"multi-color LAB discrete":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Equal", "ColorSpace"->"RGB", "ColorSpectrum"->"LABHarmony", "Quantity"->Length[$dwSelected]];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}]),
						"multi-color pastel discrete":>(temp=1;
							palette = dwColorPalette[0, "ColorOrder"->"Equal", "ColorSpace"->"RGB", "ColorSpectrum"->"Neon", "Quantity"->Length[$dwSelected], "Shade"->.5];
							Do[
							If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], 
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]] = palette[[temp]];
								$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]] = palette[[temp++]]
								
							], {n, sel}])
			
					}
				]
			],
			
		ContentPadding->False, FrameMargins->0, ImageSize->{60, 26}]
	]

End[] (* End Private Context *)

EndPackage[]