(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeCloud]={"Location"->{0,0}, "Size"->1, "Color"->Gray, "RandomSeed"->1, "CloudShape"->Automatic};

dwShapeCloud[OptionsPattern[]]:=
	Block[{resolution = 180, g, height = 1, loc, size, scale, color, seed, seedStep = 1, shape},
		{scale, color, seed, shape} = OptionValue[{"Size", "Color", "RandomSeed", "CloudShape"}];
		resolution = scale*resolution;
		
		loc := (SeedRandom[seed+seedStep++]; RandomReal[1.5, 3]);
		size := (SeedRandom[seed+seedStep++]; RandomReal[{.5, 1}]);
		
		g = ImageAdjust[
			Blur[
				Rasterize[
					Graphics3D[
						Switch[shape,
							"rising",
								Table[{color, Opacity[.2], Specularity[White, 1], 
									Ellipsoid[{1, 1, .75} loc, size {1, 1, height}], 
									Ellipsoid[{SeedRandom[n + seed]; RandomReal[{.5, 1.5}], -.5, -.25} + {1, 1, 2} loc, size {1, 1, height}], 
									Ellipsoid[{SeedRandom[n + seed]; RandomReal[{1.5, 2.5}], .25, 1} + {1, 1, 1.5} loc, size {1, 1, height}], 
									Ellipsoid[{3, 0, .25} + {1, 1, .75} loc, size {1, 1, height}], 
									Ellipsoid[{1.5, 0, -1.5} + {1, 1, 2} loc, 1 size {1, 1, 1 height}], 
									Ellipsoid[{2, 0, -3} + {1, 1, 2} loc, .5 size {1, 1, 1 height}]}, 
								{n, 40}],
							_,
								Table[{color, Opacity[.2], Specularity[White, 1],
									Ellipsoid[{1, 1, .75} loc, size {1, 1, height}],
									Ellipsoid[{SeedRandom[n+seed];RandomReal[{.5, 1.5}], -.5, -.25} + {1, 1, 1.25} loc, size {1, 1, height}],
									Ellipsoid[{SeedRandom[n+seed];RandomReal[{1.5, 2.5}], .25, 1} + {1, 1, 1} loc, size {1, 1, height}],
									Ellipsoid[{3, 0, .25} + {1, 1, .75} loc, size {1, 1, height}]},
								{n, 40}]
						],(* increase for smoother edge shape *) 
					Lighting -> {{"Ambient", color}, {"Directional", White, ImageScaled[{.5, 1, .5}]}}, ImageSize -> resolution, ViewPoint -> {0, -Infinity, 0}, Boxed -> False], 
				Background -> None, ImageResolution->$dwImageResolution],
			10*(resolution/180)]
		];
		SetAlphaChannel[g, Blur[Erosion[Binarize[g, .05], DiskMatrix[1/(30/resolution)]], 4]]
	 ]

End[] (* End Private Context *)

EndPackage[]