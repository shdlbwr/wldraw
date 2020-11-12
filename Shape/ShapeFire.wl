(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwShapeFire]={"Location"->{0,0}, "Size"->2, "Color"->Black, 
	"FireFuel"->.0225, "FireGlow"->0, "FireShape"->.9, "FireShift"->.125, "FireSquash"->0, "FireSqueeze"->0.2, "FireStretch"->0, "FireTurbulence"->14, "FireEdgeFalloff"->2, 
	"RandomSeed"->1, "BackgroundFade"->False, "Detail"->4};

dwShapeFire[OptionsPattern[]]:=
	Block[{loc, size, color, fuel, seed, turbulence, detail, glow, shape, edgefalloff, shift, stretch, squeeze, squash},
		{size, color, fuel, seed, turbulence, detail, glow, shape, edgefalloff, shift, stretch, squeeze, squash} = 
		OptionValue[{"Size", "Color", "FireFuel", "RandomSeed", "FireTurbulence", "Detail", "FireGlow", "FireShape", "FireEdgeFalloff", "FireShift", "FireStretch", "FireSqueeze", "FireSquash"}];
		
		ImageResize[
			SetAlphaChannel[
				Sharpen[
					CurvatureFlowFilter[
						ImageEffect[
							ImageEffect[
								Rasterize[
									WaveletScalogram[
			 							ContinuousWaveletTransform[
			 								Table[
			 									If[edgefalloff > 0, (((x - (10 shift))^(edgefalloff))(10 - (x - (10 shift)))^(edgefalloff)), 1]*(Sin[x]^(Pi^shape + Abs[Sin[x^shape]])),
			 										{x, (10 shift), (10 shift) + 10, fuel}][[1 + IntegerPart[stretch(2/fuel)];;-1 - IntegerPart[stretch(2/fuel)]]],
			 								MexicanHatWavelet[2], WaveletScale -> 5 fuel
			 							], 
										AspectRatio -> 1, Axes -> False, Background -> color, 
										ColorFunction ->
											(Blend[{
												{0, Opacity[0, color]},
												{.05, Hue[.05, 1, 1, .03]},
												{.1, Hue[.02, 1, 1, .66]},
												{.2 - (.025 glow), Hue[.05, 1, 1, 1]}, 
												{.3 - (.05 glow), Hue[.1, 1, 1]}, 
												{.5 - (.1 glow), Hue[.14, 1, 1]}, 
												{.8 - (.15 glow), Hue[.167, .5, 1]},
												{.9, Hue[.167, .1, 1]}, 
												{1, Hue[.167, 0, 1]}
											}, #] &),
									PlotRangePadding -> {{squeeze (5/fuel), squeeze (5/fuel)}, {0, 20 squash}}],
								ImageResolution->$dwImageResolution], 
							{"Jitter", turbulence}, RandomSeeding -> seed], 
						{"OilPainting", 3}, RandomSeeding -> seed],
					10 - detail], 
				3],
				If[OptionValue["BackgroundFade"],
					Rasterize@Graphics[{Thickness[1], Line[{{.5, 0}, {.5, (1 - .5 squash)*(10 fuel)}, {.5, 1}}, VertexColors->{White, White, Black}]}, ImagePadding -> 0, PlotRangePadding -> 0],
					1
				]
			],
		180*Max[size, .01]]
	 ]

End[] (* End Private Context *)

EndPackage[]