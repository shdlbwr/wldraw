(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwStyleImage[sel_, noStyleMargin_, fontsize_]:=
	Block[{temp},
		Which[
			MemberQ[{Image}, $dwHead[[sel]]],
				Grid[{Join[{Spacer[10]},
					Riffle[
						Table[
							With[{n=n},
								Column[{
									
									Grid[{{
										(* filter *)
										ActionMenu[Dynamic[Which[
												$dwSelected === {}, "None", 
												Length[$dwStyle[[$dwSelected[[1]],4]]] < n, "None", 
												True, $dwStyle[[$dwSelected[[1]],4,n,1]]
											]],{
											
											"Flatten filters":>(dwSetUndo[]; temp = Rasterize[dwRenderImage[sel][[1,1]], Background->None, ImageResolution->$dwImageResolution]; $dwStyle[[$dwSelected[[1]]]] = ReplacePart[$dwDefaultImageStyle, 4->{}]; $dwStyle[[$dwSelected[[1]], 2]] = temp),
											"None":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "None"),
											
											Style["----------------------------",8]:>({}),
											"AutoAdjust":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "AutoAdjust"),
											"Blend":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Blend"),
											"Mask":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Mask"; $dwStyle[[$dwSelected[[1]],4,n,2]] = $dwSelected[[1]](*; $dwStyle[[$dwSelected[[1]],4,n,5]] = None*)),
											"Opacity":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Opacity"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											
											Style["----------------------------",8]:>({}),
											"Brightness":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Brightness"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											"Contrast":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Contrast"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 2; $dwStyle[[$dwSelected[[1]],4,n,2]] = 1),
											Darker:>($dwStyle[[$dwSelected[[1]],4,n,1]] = Darker; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											"Gamma":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Gamma"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 5; $dwStyle[[$dwSelected[[1]],4,n,2]] = 1),
											"Invert":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Invert"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = 1),
											Lighter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = Lighter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											
											Style["----------------------------",8]:>({}),
											"Color":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Color"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 1; $dwStyle[[$dwSelected[[1]],4,n,4]] = 2; $dwStyle[[$dwSelected[[1]],4,n,2]] = 1.5; $dwStyle[[$dwSelected[[1]],4,n,5]] = If[Length[DominantColors[$dwStyle[[$dwSelected[[1]],2]],1]] < 1, Orange, DominantColors[$dwStyle[[$dwSelected[[1]],2]],1][[1]]]),
											"Colorize":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Colorize"; $dwStyle[[$dwSelected[[1]],4,n,2]] = 2; $dwStyle[[$dwSelected[[1]],4,n,5]] = ColorData[106,"ColorList"][[{1, 5, 3, 4, 2}]]),
											"KeepColor":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "KeepColor"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5; $dwStyle[[$dwSelected[[1]],4,n,5]] = Red),
											"RemoveColor":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "RemoveColor"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5; $dwStyle[[$dwSelected[[1]],4,n,5]] = Red),
											"ReplaceColor":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "ReplaceColor"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5; $dwStyle[[$dwSelected[[1]],4,n,5]] = {Black,Red}),
											
											Style["----------------------------",8]:>({}),
											"BalancedBinarize":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "BalancedBinarize"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 1; $dwStyle[[$dwSelected[[1]],4,n,4]] = 16; $dwStyle[[$dwSelected[[1]],4,n,2]] = 8),
											Binarize:>($dwStyle[[$dwSelected[[1]],4,n,1]] = Binarize; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											"Decolorization":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Decolorization"),
											"Grayscale":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Grayscale"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											"Sepia":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Sepia"),
											
											Style["----------------------------",8]:>({}),
											Blur:>($dwStyle[[$dwSelected[[1]],4,n,1]] = Blur; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 16; $dwStyle[[$dwSelected[[1]],4,n,2]] = 8),
											Dilation:>($dwStyle[[$dwSelected[[1]],4,n,1]] = Dilation; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 2; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											Erosion:>($dwStyle[[$dwSelected[[1]],4,n,1]] = Erosion; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 2; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											"Increase":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Increase"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											"MotionBlur":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "MotionBlur"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 1; $dwStyle[[$dwSelected[[1]],4,n,4]] = 32; $dwStyle[[$dwSelected[[1]],4,n,2]] = 8; $dwStyle[[$dwSelected[[1]],4,n,5]] = 0),
											"Noise":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Noise"; $dwStyle[[$dwSelected[[1]],4,n,3]] = .1; $dwStyle[[$dwSelected[[1]],4,n,4]] = .9; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5; $dwStyle[[$dwSelected[[1]],4,n,5]] = "Noise"),
											"NoiseRemoval":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "NoiseRemoval"; $dwStyle[[$dwSelected[[1]],4,n,3]] = .1; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											"Reduce":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Reduce"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											Sharpen:>($dwStyle[[$dwSelected[[1]],4,n,1]] = Sharpen; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 32; $dwStyle[[$dwSelected[[1]],4,n,2]] = 8),
											
											Style["----------------------------",8]:>({}),
											"Charcoal":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Charcoal"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 1; $dwStyle[[$dwSelected[[1]],4,n,4]] = 16; $dwStyle[[$dwSelected[[1]],4,n,2]] = 5),
											"Comics":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Comics"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5),
											EdgeDetect:>($dwStyle[[$dwSelected[[1]],4,n,1]] = EdgeDetect; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											"EdgeStylization":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "EdgeStylization"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 32; $dwStyle[[$dwSelected[[1]],4,n,2]] = 16),
											"Embossing":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Embossing"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 16; $dwStyle[[$dwSelected[[1]],4,n,2]] = 5),
											"HandDrawn":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "HandDrawn"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 1; $dwStyle[[$dwSelected[[1]],4,n,4]] = 2; $dwStyle[[$dwSelected[[1]],4,n,2]] = 1.5; $dwStyle[[$dwSelected[[1]],4,n,5]] = 1),
											"Jitter":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Jitter"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 16; $dwStyle[[$dwSelected[[1]],4,n,2]] = 2),
											"OilPainting":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "OilPainting"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 1; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											"Posterization":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Posterization"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 2; $dwStyle[[$dwSelected[[1]],4,n,4]] = 32; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											"Spherize":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Spherize"; $dwStyle[[$dwSelected[[1]],4,n,3]] = -1; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .5; $dwStyle[[$dwSelected[[1]],4,n,6]] = 100),
											"Stippling":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "Stippling"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 2; $dwStyle[[$dwSelected[[1]],4,n,4]] = 64; $dwStyle[[$dwSelected[[1]],4,n,2]] = 10),
											"TwistInner":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "TwistInner"; $dwStyle[[$dwSelected[[1]],4,n,3]] = -2Pi; $dwStyle[[$dwSelected[[1]],4,n,4]] = 2Pi; $dwStyle[[$dwSelected[[1]],4,n,2]] = 0),
											"TwistOuter":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "TwistOuter"; $dwStyle[[$dwSelected[[1]],4,n,3]] = -2Pi; $dwStyle[[$dwSelected[[1]],4,n,4]] = 2Pi; $dwStyle[[$dwSelected[[1]],4,n,2]] = 0),
											"WeightedCorner":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "WeightedCorner"; $dwStyle[[$dwSelected[[1]],4,n,3]] = .5; $dwStyle[[$dwSelected[[1]],4,n,4]] = 2; $dwStyle[[$dwSelected[[1]],4,n,2]] = 1),
											
											Style["----------------------------",8]:>({}),
											CurvatureFlowFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = CurvatureFlowFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 20; $dwStyle[[$dwSelected[[1]],4,n,2]] = 5),
											GradientFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = GradientFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 32; $dwStyle[[$dwSelected[[1]],4,n,2]] = 8),
											GradientOrientationFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = GradientOrientationFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 32; $dwStyle[[$dwSelected[[1]],4,n,2]] = 8),
											LaplacianGaussianFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = LaplacianGaussianFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 16; $dwStyle[[$dwSelected[[1]],4,n,2]] = 8),
											MedianFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = MedianFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											RidgeFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = RidgeFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 32; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											StandardDeviationFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = StandardDeviationFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 1; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											
											Style["----------------------------",8]:>({}),
											"FrameFaded":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "FrameFaded"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 16; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											"FrameTorn":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "FrameTorn"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .2; $dwStyle[[$dwSelected[[1]],4,n,5]] = Bottom),
											"FrameVignette":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "FrameVignette"; $dwStyle[[$dwSelected[[1]],4,n,3]] = .01; $dwStyle[[$dwSelected[[1]],4,n,4]] = 1; $dwStyle[[$dwSelected[[1]],4,n,2]] = .6; $dwStyle[[$dwSelected[[1]],4,n,5]] = If[DominantColors[$dwStyle[[$dwSelected[[1]],2]]] === {}, Black, DominantColors[$dwStyle[[$dwSelected[[1]],2]],1][[1]]]),
											"FrameWave":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "FrameWave"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 10; $dwStyle[[$dwSelected[[1]],4,n,2]] = 5; $dwStyle[[$dwSelected[[1]],4,n,5]] = Bottom)
											
											
											(* removed filters *)
											(*
											"BlurRemoval":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "BlurRemoval"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 32; $dwStyle[[$dwSelected[[1]],4,n,2]] = 8),
											"LightenDarkPixels":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "LightenDarkPixels"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											"DarkenLightPixels":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "DarkenLightPixels"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											CommonestFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = CommonestFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											EntropyFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = EntropyFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											GeometricMeanFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = GeometricMeanFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 16; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
											HarmonicMeanFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = HarmonicMeanFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 4; $dwStyle[[$dwSelected[[1]],4,n,2]] = 2),
											"IsolateColor":>($dwStyle[[$dwSelected[[1]],4,n,1]] = "IsolateColor"; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = .5; $dwStyle[[$dwSelected[[1]],4,n,2]] = .2; $dwStyle[[$dwSelected[[1]],4,n,5]] = Yellow),
											RangeFilter:>($dwStyle[[$dwSelected[[1]],4,n,1]] = RangeFilter; $dwStyle[[$dwSelected[[1]],4,n,3]] = 0; $dwStyle[[$dwSelected[[1]],4,n,4]] = 8; $dwStyle[[$dwSelected[[1]],4,n,2]] = 4)
											*)
										
										}, ImageSize->{Automatic,$dwStyleButtonHeight}, MenuStyle->{12}],
									
										(* filter remove button *)
										Tooltip[
											Button[Graphics[{AbsoluteThickness[2],GrayLevel[.7],Line[{{-1,-1},{1,1}}],Line[{{-1,1},{1,-1}}]}, ImageSize->{13,13}],
												dwSetUndo[]; $dwStyle[[$dwSelected[[1]],4]] = Delete[$dwStyle[[$dwSelected[[1]],4]], n],
												Appearance->None
											], 
										"Remove image filter", TooltipDelay->$dwTooltipDelay]
									
									}},Alignment->Top],
								
									(* filter controls *)
									Dynamic@If[$dwSelected === {} || Length[$dwStyle[[$dwSelected[[1]],4]]] < n,
										"",
										Which[
											MemberQ[{"AutoAdjust","ColorBoosting","Decolorization","Deuteranopia","None","Protanopia","RemoveBackground","Sepia","Tritanopia"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												"",
											MemberQ[{"Blend"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[{Spacer[{0,14}], Style["copy | paste graphics to preview","Text", 12, White]}],
											MemberQ[{"Color","FrameVignette","IsolateColor","RemoveColor","KeepColor"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[{
													ColorSetter[Dynamic[$dwStyle[[$dwSelected[[1]],4,n,5]]], ImageSize->Tiny], Spacer[5],
													Dynamic[Slider[If[$dwSelected === {}, 0, Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], If[$dwSelected === {}, {0,1}, {$dwStyle[[$dwSelected[[1]],4,n,3]],$dwStyle[[$dwSelected[[1]],4,n,4]]}], ImageSize->{100, 20}, Appearance->Tiny, ContinuousAction->False]],
													" ",Style[Dynamic[If[$dwSelected === {}, "", $dwStyle[[$dwSelected[[1]],4,n,2]]]], 12, White]
												}],
											MemberQ[{"ReplaceColor"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[{
													ColorSetter[Dynamic[$dwStyle[[$dwSelected[[1]],4,n,5,1]]], ImageSize->Tiny],
													ColorSetter[Dynamic[$dwStyle[[$dwSelected[[1]],4,n,5,2]]], ImageSize->Tiny], Spacer[5],
													Dynamic[Slider[If[$dwSelected === {}, 0, Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], If[$dwSelected === {}, {0,1}, {$dwStyle[[$dwSelected[[1]],4,n,3]],$dwStyle[[$dwSelected[[1]],4,n,4]]}], ImageSize->{100, 20}, Appearance->Tiny, ContinuousAction->False]],
													" ",Style[Dynamic[If[$dwSelected === {}, "", $dwStyle[[$dwSelected[[1]],4,n,2]]]], 12, White]
												}],
											MemberQ[{"Colorize"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[Join[{ActionMenu[Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]], {
															2:>($dwStyle[[$dwSelected[[1]],4,n,2]] = 2),
															3:>($dwStyle[[$dwSelected[[1]],4,n,2]] = 3),
															4:>($dwStyle[[$dwSelected[[1]],4,n,2]] = 4),
															5:>($dwStyle[[$dwSelected[[1]],4,n,2]] = 5)
														}, BaselinePosition->Scaled[.4], FrameMargins->0, ImageSize->{30,20}]},
														Table[With[{ctn=ctn}, ColorSetter[Dynamic@$dwStyle[[$dwSelected[[1]],4,n,5,5-$dwStyle[[$dwSelected[[1]],4,n,2]]+ctn]], ImageSize->Tiny]], {ctn, $dwStyle[[$dwSelected[[1]],4,n,2]]}]
													]
												],
											MemberQ[{"ColorTones"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[Reverse[Table[With[{ctn=ctn}, ColorSetter[Dynamic@$dwStyle[[$dwSelected[[1]],4,n,5,ctn]], ImageSize->Tiny]], {ctn, 2}]]],
											MemberQ[{"FrameTorn","FrameWave"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[{Dynamic[ActionMenu[Dynamic[$dwStyle[[$dwSelected[[1]],4,n,5]]], {
														All:>($dwStyle[[$dwSelected[[1]],4,n,5]] = All),
														Left:>($dwStyle[[$dwSelected[[1]],4,n,5]] = Left),
														Right:>($dwStyle[[$dwSelected[[1]],4,n,5]] = Right),
														Bottom:>($dwStyle[[$dwSelected[[1]],4,n,5]] = Bottom),
														Top:>($dwStyle[[$dwSelected[[1]],4,n,5]] = Top)
													}, 
													Appearance->"PopupMenu", FrameMargins->0, ImageSize->{100,20}]],
													Button[Style["- ", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Max[$dwStyle[[$dwSelected[[1]],4,n,2]] - If[MemberQ[{"FrameTorn"}, $dwStyle[[$dwSelected[[1]],4,n,1]]], .05, 1], $dwStyle[[$dwSelected[[1]],4,n,3]]], If[MemberQ[{"FrameTorn"}, $dwStyle[[$dwSelected[[1]],4,n,1]]], .05, 1]], Appearance->None],
													Dynamic[Slider[If[$dwSelected === {}, 0, Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], If[$dwSelected === {}, {0,1}, {$dwStyle[[$dwSelected[[1]],4,n,3]], $dwStyle[[$dwSelected[[1]],4,n,4]]}], ImageSize->{50, 20}, Appearance->Tiny, ContinuousAction->False]],
													Button[Style[" +", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Min[$dwStyle[[$dwSelected[[1]],4,n,2]] + If[MemberQ[{"FrameTorn"}, $dwStyle[[$dwSelected[[1]],4,n,1]]], .05, 1], $dwStyle[[$dwSelected[[1]],4,n,4]]], If[MemberQ[{"FrameTorn"}, $dwStyle[[$dwSelected[[1]],4,n,1]]], .05, 1]], Appearance->None],
													" ",Style[Dynamic[If[$dwSelected === {}, 0, Chop[$dwStyle[[$dwSelected[[1]],4,n,2]]]]], 12, White]
												}],
											MemberQ[{"HandDrawn"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[{
													Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[$dwSelected[[1]],4,n,5]]]], Range[10], ImageSize->Tiny]],
													Button[Style["- ", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Max[$dwStyle[[$dwSelected[[1]],4,n,2]] - .1, $dwStyle[[$dwSelected[[1]],4,n,3]]],.1], Appearance->None],
													Dynamic@Slider[If[$dwSelected === {}, 0, Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], If[$dwSelected === {}, {0,1}, {$dwStyle[[$dwSelected[[1]],4,n,3]],$dwStyle[[$dwSelected[[1]],4,n,4]]}], ImageSize->{100, 20}, Appearance->Tiny, ContinuousAction->False],
													Button[Style[" +", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Min[$dwStyle[[$dwSelected[[1]],4,n,2]] + .1, $dwStyle[[$dwSelected[[1]],4,n,4]]],.1], Appearance->None],
													" ",Style[Dynamic[If[$dwSelected === {}, "", $dwStyle[[$dwSelected[[1]],4,n,2]]]], 12, White]
												}],
											MemberQ[{"Mask"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[{Style["mask: ", $dwStyleControlHeadColor],
													Dynamic[ActionMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], 
														Table[
															With[{num = n, mask = n1}, 
																mask:>(
																	$dwStyle[[$dwSelected[[1]],4,num,2]] = mask;
																	$dwHideLayers = If[mask != $dwSelected[[1]] && MemberQ[{Image, Text}, $dwHead[[mask]]], Join[$dwHideLayers, {mask}], $dwHideLayers];
																	If[FreeQ[{Image, Text}, $dwHead[[mask]]],
																		If[MemberQ[{BSplineCurve, BezierCurve}, $dwHead[[mask]]],
																			$dwStyle[[mask, 1]] = False, (* remove filling for more intuitive selection *)
																			Nothing
																		];
																		$dwStyle[[mask, Flatten[Position[$dwStyle[[mask]], FaceForm[_]]][[1]], 1, 2, 1]] = 0; (* fill opacity *)
																		$dwStyle[[mask, Flatten[Position[$dwStyle[[mask]], StrokeForm[_]]][[1]], 1, 2, 1]] = 0, (* stroke opacity *)
																		Nothing
																	]
																)
															],{n1, Length[$dwP]}], 
													Appearance->"PopupMenu", FrameMargins->0, ImageSize->Tiny]]
													(*Tooltip[ColorSetter[Dynamic@$dwStyle[[$dwSelected[[1]],4,n,5]], ImageSize->Tiny], "Background color for rotated object", TooltipDelay->$dwTooltipDelay]*)
												}],
											MemberQ[{"MotionBlur"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[{Dynamic[ActionMenu[Dynamic[
														Switch[$dwStyle[[$dwSelected[[1]],4,n,5]],
															Pi/2, Graphics[{Arrowheads[Small], Arrow[{{0,-1},{0,1}}]}, ImageSize->18], 
															Pi/4, Graphics[{Arrowheads[Small], Arrow[{{-1,-1},{1,1}}]}, ImageSize->18], 
															-Pi/4, Graphics[{Arrowheads[Small], Arrow[{{-1,1},{1,-1}}]}, ImageSize->18], 
															_, Graphics[{Arrowheads[Small], Arrow[{{-1,0},{1,0}}]}, ImageSize->18]
														]], {
														Graphics[{Arrowheads[Small], Arrow[{{-1,0},{1,0}}]}, ImageSize->18]:>($dwStyle[[$dwSelected[[1]],4,n,5]] = 0),
														Graphics[{Arrowheads[Small], Arrow[{{0,-1},{0,1}}]}, ImageSize->18]:>($dwStyle[[$dwSelected[[1]],4,n,5]] = Pi/2),
														Graphics[{Arrowheads[Small], Arrow[{{-1,-1},{1,1}}]}, ImageSize->18]:>($dwStyle[[$dwSelected[[1]],4,n,5]] = Pi/4),
														Graphics[{Arrowheads[Small], Arrow[{{-1,1},{1,-1}}]}, ImageSize->18]:>($dwStyle[[$dwSelected[[1]],4,n,5]] = -Pi/4)
													}, 
													Appearance->"PopupMenu", FrameMargins->0, ImageSize->{50,20}]],
													" ",
													Button[Style["- ", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Max[$dwStyle[[$dwSelected[[1]],4,n,2]] - .1, $dwStyle[[$dwSelected[[1]],4,n,3]]],.1], Appearance->None],
													Dynamic[Slider[If[$dwSelected === {}, 0, Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], If[$dwSelected === {}, {0,1}, {$dwStyle[[$dwSelected[[1]],4,n,3]], $dwStyle[[$dwSelected[[1]],4,n,4]]}], ImageSize->{100, 20}, Appearance->Tiny, ContinuousAction->False]],
													Button[Style[" +", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Min[$dwStyle[[$dwSelected[[1]],4,n,2]] + .1, $dwStyle[[$dwSelected[[1]],4,n,4]]],.1], Appearance->None],
													" ",Style[Dynamic[If[$dwSelected === {}, 0, Chop[$dwStyle[[$dwSelected[[1]],4,n,2]]]]], 12, White]
												}],
											MemberQ[{"Noise"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[{
													Dynamic[PopupMenu[Dynamic@$dwStyle[[$dwSelected[[1]],4,n,5]], {"GaussianNoise","Noise","PoissonNoise","SaltPepperNoise"}, ImageSize->Tiny]],
													Button[Style["- ", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Max[$dwStyle[[$dwSelected[[1]],4,n,2]] - .1, $dwStyle[[$dwSelected[[1]],4,n,3]]],.1], Appearance->None],
													Dynamic[Slider[If[$dwSelected === {}, 0, Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], If[$dwSelected === {}, {0,1}, {$dwStyle[[$dwSelected[[1]],4,n,3]], $dwStyle[[$dwSelected[[1]],4,n,4]]}], ImageSize->{100, 20}, Appearance->Tiny, ContinuousAction->False]],
													Button[Style[" +", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Min[$dwStyle[[$dwSelected[[1]],4,n,2]] + .1, $dwStyle[[$dwSelected[[1]],4,n,4]]],.1], Appearance->None],
													" ",Style[Dynamic[If[$dwSelected === {}, 0, Chop[$dwStyle[[$dwSelected[[1]],4,n,2]]]]], 12, White]
												}],
											MemberQ[{"Posterization"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],(* slider uses integer step*)
												Row[{
													Button[Style["- ", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Max[$dwStyle[[$dwSelected[[1]],4,n,2]] - 1, $dwStyle[[$dwSelected[[1]],4,n,3]]], 1], Appearance->None],
													Dynamic[Slider[If[$dwSelected === {}, 0, Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], If[$dwSelected === {}, {0,1}, {$dwStyle[[$dwSelected[[1]],4,n,3]], $dwStyle[[$dwSelected[[1]],4,n,4]], 1}], ImageSize->{100, 20}, Appearance->Tiny, ContinuousAction->False]],
													Button[Style[" +", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Min[$dwStyle[[$dwSelected[[1]],4,n,2]] + 1, $dwStyle[[$dwSelected[[1]],4,n,4]]], 1], Appearance->None],
													" ",Style[Dynamic[If[$dwSelected === {}, "", Chop[$dwStyle[[$dwSelected[[1]],4,n,2]]]]], 12, White]
												}],
											MemberQ[{"Spherize"}, $dwStyle[[$dwSelected[[1]],4,n,1]]],
												Row[{
													Dynamic[PopupMenu[If[$dwSelected === {}, "", Dynamic[$dwStyle[[$dwSelected[[1]],4,n,6]]]], {100->"low", 250->"medium", 400->"high"}, ImageSize->Tiny]],
													Button[Style["- ", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Max[$dwStyle[[$dwSelected[[1]],4,n,2]] - .1, $dwStyle[[$dwSelected[[1]],4,n,3]]],.1], Appearance->None],
													Dynamic@Slider[If[$dwSelected === {}, 0, Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], If[$dwSelected === {}, {0,1}, {$dwStyle[[$dwSelected[[1]],4,n,3]],$dwStyle[[$dwSelected[[1]],4,n,4]]}], ImageSize->{100, 20}, Appearance->Tiny, ContinuousAction->False],
													Button[Style[" +", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Min[$dwStyle[[$dwSelected[[1]],4,n,2]] + .1, $dwStyle[[$dwSelected[[1]],4,n,4]]],.1], Appearance->None],
													" ",Style[Dynamic[If[$dwSelected === {}, "", $dwStyle[[$dwSelected[[1]],4,n,2]]]], 12, White]
												}],
											True,
												Row[{
													Button[Style["- ", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Max[$dwStyle[[$dwSelected[[1]],4,n,2]] - .1, $dwStyle[[$dwSelected[[1]],4,n,3]]],.1], Appearance->None],
													Dynamic[Slider[If[$dwSelected === {}, 0, Dynamic[$dwStyle[[$dwSelected[[1]],4,n,2]]]], If[$dwSelected === {}, {0,1}, {$dwStyle[[$dwSelected[[1]],4,n,3]], $dwStyle[[$dwSelected[[1]],4,n,4]]}], ImageSize->{100, 20}, Appearance->Tiny, ContinuousAction->False]],
													Button[Style[" +", White], $dwStyle[[$dwSelected[[1]],4,n,2]] = Round[Min[$dwStyle[[$dwSelected[[1]],4,n,2]] + .1, $dwStyle[[$dwSelected[[1]],4,n,4]]],.1], Appearance->None],
													" ",Style[Dynamic[If[$dwSelected === {}, 0, Round[Chop[$dwStyle[[$dwSelected[[1]],4,n,2]]],.01]]], 12, White]
												}]
										]
									]
								}, Spacings->0, Alignment->Center]
							], 
						{n, Length[$dwStyle[[$dwSelected[[1]],4]]]}], 
						Graphics[Line[{{.1,0},{.1,1}}],ImageSize->{Automatic,45}]
					], 
						{
							Graphics[Line[{{.1,0},{.1,1}}],ImageSize->{Automatic,45}], 
							Tooltip[
								Button[
									Graphics[{AbsoluteThickness[2],GrayLevel[.7],Line[{{-1,0},{1,0}}],Line[{{0,1},{0,-1}}]}, ImageSize->{15,15}], 
									If[Length[$dwStyle[[$dwSelected[[1]],4]]] < $dwImageMultipleFilterMax, dwSetUndo[]; AppendTo[$dwStyle[[$dwSelected[[1]],4]], $dwDefaultImageStyle[[4,1]]]], 
								Appearance->None], 
							"Add image filter (maximum of "<>ToString[$dwImageMultipleFilterMax]<>")", TooltipDelay->$dwTooltipDelay]
					}]
				}, Alignment->Top],
			True,
				If[$dwSelected === {},
					Pane[Style["Image style settings not available for selected object.", 12, LightGray], ImageMargins->noStyleMargin],
					
					Row[{Spacer[10],
						Button[Style[" CREATE IMAGE FROM SELECTED ", 8],
							dwSetUndo[]; dwCreateImageFromGraphics[None],
						Appearance->"Palette", ImageSize->{Automatic, $dwStyleButtonHeight}],
						
						Spacer[5],
						
						Button[Style[" CREATE IMAGE WITH PADDING ", 8],
							dwSetUndo[]; dwCreateImageFromGraphics[White],
						Appearance->"Palette", ImageSize->{Automatic, $dwStyleButtonHeight}]
					}]
				]
		]
	]

End[] (* End Private Context *)

EndPackage[]