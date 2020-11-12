(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwColorPalettes] = {Enabled->True};

dwColorPalettes[form_:"fill", OptionsPattern[]]:=
	Column[{
		Row[{
			Dynamic@ActionMenu[Dynamic@$dwColorPalette,
				#:>($dwColorPalette = #)&/@{	
					"default",
					"standard",
					"doc art",
					"neon",
					"pastel",
					"LABHarmony",
					"monochrome",
					"M10default",
					"business",
					"marketing",
					"scientific",
					"web"
				}, Appearance->"Palette", BaselinePosition->Bottom, ImageSize->{112, $dwStyleButtonHeight}],
				
				Dynamic@Grid[Partition[Flatten[Tooltip[Button[Graphics[{#,Rectangle[]},PlotRangePadding->0,ImagePadding->0],
					Do[If[MemberQ[$dwShapeSymbols, $dwHead[[s]]],
							Switch[form,
								"fill",
									$dwStyle[[If[Head[$dwSelected[[1]]] === List, $dwSelected[[1,1]], $dwSelected[[1]]], Flatten[Position[$dwStyle[[If[Head[$dwSelected[[1]]] === List, $dwSelected[[1,1]], $dwSelected[[1]]]]], FaceForm[_]]][[1]]]][[1,1]] = #;
									dwUpdateSelected[If[Head[$dwSelected[[1]]] === List, $dwSelected[[1,1]], $dwSelected[[1]]], "StylesToUpdate"->"fillColor"],
								"stroke",
									If[MemberQ[$dwShapeSymbols, $dwHead[[s]]], 
										$dwStyle[[If[Head[$dwSelected[[1]]] === List, $dwSelected[[1,1]], $dwSelected[[1]]], Flatten[Position[$dwStyle[[If[Head[$dwSelected[[1]]] === List, $dwSelected[[1,1]], $dwSelected[[1]]]]], StrokeForm[_]]][[1]]]][[1,1]] = #;
										dwUpdateSelected[If[Head[$dwSelected[[1]]] === List, $dwSelected[[1,1]], $dwSelected[[1]]], "StylesToUpdate"->"strokeColor"]
									],
								"point",
									$dwStyle[[If[Head[$dwSelected[[1]]] === List, $dwSelected[[1,1]], $dwSelected[[1]]],$dwStyleStart]] = #;
									dwUpdateSelected[If[Head[$dwSelected[[1]]] === List, $dwSelected[[1,1]], $dwSelected[[1]]], "StylesToUpdate"->"pointColor"],
								_,
									Nothing
							]
						],
					{s, If[Head[$dwSelected[[1]]] === List, #[[1]]&/@$dwSelected, $dwSelected]}],
					ImageSize->{$dwStyleButtonHeight/2,$dwStyleButtonHeight/2},Appearance->None,Enabled->OptionValue[Enabled]], InputForm[#], TooltipDelay->$dwTooltipDelay]&/@(
						Switch[$dwColorPalette,
							"default",
								Join[
									dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"Bright", "Quantity"->12],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}] (* specific numbers used to avoid ...000001 *)
								],
							"warm",
								Flatten[Table[Table[Table[Hue[h,s,b],{h,{0,.03,.06,.09,.12,.15}}],{s,{1,.5}}],{b,{1,.75}}]],
							"cool",
								Flatten[Table[Table[Table[Hue[h,s,b],{h,{.25,.4,.45,.52,.55,.58}}],{s,{1,.5}}],{b,{1,.75}}]],
							"royal",
								Flatten[Table[Table[Table[Hue[h,s,b],{h,{.65,.7,.75,.8,.85,.9}}],{s,{.75,.5}}],{b,{1,.75}}]],
							"monochrome",
								Join[
									dwColorPalette[.15, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"MonochromeStrong", "ColorSpectrumRange" -> {.3, .9}, "Quantity"->6],
									dwColorPalette[.3, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"MonochromeStrong", "ColorSpectrumRange" -> {.3, .9}, "Quantity"->6],
									dwColorPalette[.6, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"MonochromeStrong", "ColorSpectrumRange" -> {.3, .9}, "Quantity"->6],
									dwColorPalette[.8, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"MonochromeStrong", "ColorSpectrumRange" -> {.3, .9}, "Quantity"->6]
								],
							"standard",
								Join[
									dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"Standard", "Quantity"->12],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"bright",
								Join[
									dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"Bright", "Quantity"->12],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"neon",
								Join[
									dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"Neon", "Quantity"->12],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"LABHarmony",
								Join[
									dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"LABHarmony", "Quantity"->12],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"pastel",
								Join[
									dwColorPalette[0, "ColorOrder"->"Sequential", "ColorSpace"->"RGB", "ColorSpectrum"->"Neon", "Quantity"->12, "Shade"->.5],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"M10default",
								Join[
									ColorData[97,"ColorList"][[;;12]],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"business",
								Join[
									ColorData[106,"ColorList"][[;;12]],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"marketing",
								Join[
									ColorData[109,"ColorList"][[;;12]],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"scientific",
								Join[
									ColorData[108,"ColorList"][[;;12]],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"web",
								Join[
									ColorData[112,"ColorList"][[;;12]],
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							"doc art",
								Join[
									{Hue[0, 1, 1], Hue[0, 1, 0.8], Hue[0.6, 0.7, 0.8], Hue[0.25, 0.7, 0.5], Hue[0.8, 0.4, 0.6], Hue[0.1, 1, 0.6], RGBColor[ 0.5, 0.7, 0], RGBColor[ 0.7, 0.1, 0], White, Gray, GrayLevel[.3], Black},
									Table[GrayLevel[n], {n, {0,.1,.2,.3,.4,.5,.6,.7,.8,.85,.9,1}}]
								],
							_,
								{}
						]
					)],12,12,1,{}], Spacings->{0,0}, BaselinePosition->Bottom]
		}],
		Style["|------------- COLOR PALETTE -------------|", 10, $dwStyleControlHeadColor, ShowStringCharacters->False]
	}, Alignment->Center, Spacings->0]

Options[dwColorPalette] = {"Blend"->None, "ColorOrder"->"GoldenRatio", "ColorSpace"->Automatic, "ColorSpectrum"->"Standard",
	"ColorSpectrumRange"->{0,1}, "Cycle"->0, "Form"->"Indexed", "Output"->"Value", "Quantity"->8, "Repeat"->1, "Reverse"->False,
	"Scheme"->Automatic, "Shade"->0, "ShiftGradientMidpoint"->0};

dwColorPalette[color_:0, OptionsPattern[]]:=
Module[{h,s,b,indexed,space,colorInput,colorList,spectrumColors,blendList,defaultSpace="XYZ",
		form=OptionValue["Form"],
		shift=Max[Min[OptionValue["ShiftGradientMidpoint"],Pi/2],-Pi/2],
		blend=OptionValue["Blend"],
		order=OptionValue["ColorOrder"],
		repeat=OptionValue["Repeat"],
		cycle=OptionValue["Cycle"],
		output=OptionValue["Output"],
		quantity=IntegerPart@OptionValue["Quantity"],
		reverse=OptionValue["Reverse"],
		scheme=OptionValue["Scheme"],
		shade=OptionValue["Shade"],
		span=Abs[#]&/@OptionValue["ColorSpectrumRange"],
		spectrum=OptionValue["ColorSpectrum"],
		colorSpaces={"Grayscale","RGB","CMYK","HSB","XYZ","LAB","LCH","LUV"},
		colorSpacesHead={GrayLevel,RGBColor,CMYKColor,Hue,XYZColor,LABColor,LCHColor,LUVColor},
		spectrumStandard={RGBColor[0.9,0.25,0.25],RGBColor[0.95,0.5,0.135],RGBColor[1,0.75,0],RGBColor[0.55,0.7,0.],RGBColor[0,0.7,0.7],RGBColor[0.25,0.5,1],RGBColor[0.5,0.35,1],RGBColor[0.62,0.28,0.7],RGBColor[0.75,0.28,0.5],RGBColor[0.9,0.25,0.25]},
		spectrumBright={RGBColor[1,0.2,0],RGBColor[1,0.6,0],RGBColor[1,0.8,0],RGBColor[0.63,0.8,0.],RGBColor[0.,0.8,0.8],RGBColor[0.3,0.54,1],RGBColor[0.54,0.4,1],RGBColor[0.67,0.255,0.85],RGBColor[0.8,0.1,0.75],RGBColor[1,0.2,0]},
		spectrumNormalBrown={RGBColor[0.9,0.25,0.25],RGBColor[0.8,0.4,0],RGBColor[0.95,0.5,0.135],RGBColor[1,0.75,0],RGBColor[0.55,0.7,0.],RGBColor[0,0.7,0.7],RGBColor[0.25,0.5,1],RGBColor[0.5,0.35,1],RGBColor[0.62,0.28,0.7],RGBColor[0.75,0.28,0.5],RGBColor[0.9,0.25,0.25]},
		spectrumBrightBrown={RGBColor[1,0.2,0],RGBColor[0.8,0.4,0],RGBColor[1,0.6,0],RGBColor[1,0.8,0],RGBColor[0.55,0.7,0.],RGBColor[0,0.7,0.7],RGBColor[0.25,0.5,1],RGBColor[0.5,0.35,1],RGBColor[0.62,0.2,0.8],RGBColor[0.8,0.1,0.75],RGBColor[1,0.2,0]},
		spectrumNeon={RGBColor[1,0,0],RGBColor[1,0.6,0],RGBColor[1,0.9,0],RGBColor[0.5,0.9,0],RGBColor[0,0.85,0.75],RGBColor[0.1,0.64,1],RGBColor[0.583,0.5,1],RGBColor[0.8,.3,1],RGBColor[1,0,.75],RGBColor[1,0,0]},
		spectrumCool={RGBColor[0.65, 0.9, 0.],RGBColor[0.38, 0.888, 0.18],RGBColor[0.11, 0.861, 0.585],RGBColor[0., 0.77, 0.83],RGBColor[0., 0.635, 0.965],RGBColor[0.2, 0.52, 1.],RGBColor[0.47, 0.412, 1.],RGBColor[0.644, 0.352, 1.],RGBColor[0.804, 0.294, 0.995],RGBColor[0.912, 0.132, 0.86]},
		spectrumWarm={RGBColor[0.536, 0.329, 0.91],RGBColor[0.6008, 0.2912, 0.748],RGBColor[0.6694, 0.28, 0.624],RGBColor[0.7396, 0.28, 0.516],RGBColor[0.819, 0.2662, 0.385],RGBColor[0.9, 0.25, 0.25],RGBColor[0.927, 0.385, 0.1879],RGBColor[0.954, 0.52, 0.1242],RGBColor[0.981, 0.655, 0.0513],RGBColor[0.928, 0.742, 0.]}
	},
	
	(* confirm input is list *)
	colorInput=colorList=If[Head[color]=!=List,{color},color];
	
	(* set default color space or match first color *)
	space=If[OptionValue["ColorSpace"]===Automatic,If[MemberQ[colorSpacesHead,Head[colorList[[1]]]],Head[colorList[[1]]],defaultSpace],If[MemberQ[colorSpaces,OptionValue["ColorSpace"]],OptionValue["ColorSpace"],"RGB"]];
	
	(* force span to be 0 to 1 with minimum .05 space between *)
	span=Sort[span];
	span={Min[{Max[{0,span[[1]]}],.95}],Min[{1,span[[2]]}]};
	span=If[Abs@Subtract[Sequence@@span]<.05,Which[span[[1]]<.95,{span[[1]],span[[1]]+.05},span[[2]]>.05,{span[[2]]-.05,span[[2]]}],span];
	
	(* set diverging color if single input number *)
	If[form==="DivergingGradient"&&Length[colorList]==1,
		If[MemberQ[{Integer,Real,Rational},Head[colorList[[1]]]],
			quantity=3;
			colorInput=colorList=
			If[MemberQ[{"Analogic","Analogous"},scheme],
				{colorList[[1]],White,FractionalPart[colorList[[1]]+.25]},
				{colorList[[1]],White,FractionalPart[colorList[[1]]+.5]}
			],
			Nothing
		]
	];
	
	(* monochrome colors by lightening *)
	If[MemberQ[{"Monochrome"}, spectrum],
		If[form === "Gradient", quantity=2, Nothing];
		Switch[Head[colorList[[1]]],
			Integer|Real|Rational,
				colorInput=colorList={colorList[[1]],Lighter[Blend[spectrumNeon,colorList[[1]]],.9]},
			_,
				colorInput=colorList={colorList[[1]],Lighter[colorList[[1]],.9]}
		]
	];
	
	(* monochrome colors by darkcolor->color->lightcolor *)
	If[MemberQ[{"MonochromeStrong"},spectrum],
		If[MemberQ[{"Gradient"},form],quantity=5,Nothing];
		Switch[Head[colorList[[1]]],
			Integer|Real|Rational,
				colorInput=colorList={Darker[Blend[spectrumNeon,colorList[[1]]],.5],Darker[Blend[spectrumNeon,colorList[[1]]],.25],Blend[spectrumNeon,colorList[[1]]],Lighter[Blend[spectrumNeon,colorList[[1]]],.5],Lighter[Blend[spectrumNeon,colorList[[1]]],1]},
			_,
				colorInput=colorList={Darker[colorList[[1]],.5],Darker[colorList[[1]],.25],colorList[[1]],Lighter[colorList[[1]],.5],Lighter[colorList[[1]],1]}
		]
	];
	
	(* set tetrad rectangle colors *)
	If[scheme==="TetradRectangle"&&Length[colorList]==1,
		If[MemberQ[{Integer,Real,Rational},Head[colorList[[1]]]],
			colorInput=colorList=Table[FractionalPart[colorList[[1]]+n],{n,{0,.15,.5,.65}}],
			Nothing
		]
	];
	
	(* set split complementary colors *)
	If[scheme==="SplitComplement"&&Length[colorList]==1,
		If[MemberQ[{Integer,Real,Rational},Head[colorList[[1]]]],
			colorInput=colorList=Table[FractionalPart[colorList[[1]]+n],{n,{0,.425,.575}}],
			Nothing
		]
	];
	
	(* convert input into color values *)
	Switch[Head[colorList[[1]]],
		(* set hue, saturation and brightness values *)
		Integer|Real|Rational,
			h = colorList[[1]];s=1;b=1,
		CMYKColor|GrayLevel|Hue |LABColor| LCHColor|LUVColor|RGBColor|XYZColor,
			{h,s,b} = List@@ColorConvert[colorList[[1]],"HSB"];
		(* set color space *)
		spectrum = 
			Switch[Head[colorList[[1]]],
				CMYKColor, "CMYK",
				GrayLevel, "Grayscale",
				Hue, "HSB",
				LABColor, "LAB",
				LCHColor, "LCH",
				LUVColor, "LUV",
				RGBColor, "RGB",
				XYZColor, "XYZ",
				_, "Standard"
			],
		(*grays when background is GrayLevel*)
		_,
			h=0;s=0;b=0
	];
	
	(* fill spectrum with colors *)
	If[MemberQ[{"Gradient","DivergingGradient"},form] && Length@colorList>1,
		quantity=Length@colorList,
		Nothing
	];
	spectrum = If[spectrum === "Hue", "HSB", spectrum];
	If[spectrum === "Grayscale",
		colorList = If[form==="DivergingGradient",
			ColorConvert[#,"RGB"]&/@{GrayLevel[0],GrayLevel[.5],GrayLevel[1],GrayLevel[.5],GrayLevel[0]},
			ColorConvert[#,"RGB"]&/@{GrayLevel[0],GrayLevel[.5],GrayLevel[1]}
		]
	];
	spectrumColors = 
		If[Length@colorList === 1,
			(* single color input *)
			Switch[spectrum,
				"CMYK"|"HSB"|"LAB"|"LCH"|"LUV"|"RGB"|"XYZ", Table[ColorConvert[Hue[n,s,b],spectrum],{n,0,1,.05}],
				"Grayscale", colorList,
				"LABHarmony", space="LAB"; Reverse@Table[LABColor[.7,.7 s Sin[h+n],.7 s Cos[h+n]],{n,.314Pi,2.314Pi,2Pi/20}],
				"Bright", spectrumBright,
				"Normal+Brown", spectrumNormalBrown,
				"Bright+Brown", spectrumBrightBrown,
				"Neon", spectrumNeon,
				"Cool", spectrumCool,
				"Warm", spectrumWarm,
				"Standard", spectrumStandard,
				_, colorList
			],
			(* multiple color input *)
			Table[
				If[MemberQ[{Integer,Real,Rational}, Head[colorList[[n]]]],
					Switch[spectrum,
						"LABHarmony", space="LAB"; blendList = Reverse@Table[LABColor[.7, .7s Sin[h+n], .7 s Cos[h+n]], {n, .314Pi, 2.314Pi, 2Pi/20}],
						"Bright", blendList = spectrumBright,
						"Normal+Brown", blendList = spectrumNormalBrown,
						"Bright+Brown", blendList = spectrumBrightBrown,
						"Neon", blendList = spectrumNeon,
						"Cool", blendList = spectrumCool,
						"Warm", blendList = spectrumWarm,
						"Standard", blendList = spectrumStandard,
						_, blendList = spectrumStandard
					];
					Blend[blendList,colorList[[n]]],
					colorList[[n]]],
			{n,Length@colorList}]
		];
		
	(* override variables if necessary for scheme *)
	Switch[scheme,
		"Complement", quantity = 2; order = "Equal",
		"Triad"|"SplitComplement", quantity = 3; order = "Sequential",
		"TetradSquare"|"TetradRectangle", quantity = 4; order = "Sequential",
		"Analogic"|"Analogous", h = FractionalPart[h+.5]; span = {.3,.7}
	];
	Switch[form,
		"DivergingGradient",
			spectrumColors = colorList = 
				Switch[Length@colorList,
					1,	{spectrumColors[[1]],White,spectrumColors[[1]]},
					2,	spectrumColors[[{1,2,1}]],
					_,
						If[MemberQ[{"MonochromeStrong","Monochrome"}, OptionValue["ColorSpectrum"]],
							Join[spectrumColors,Rest[Reverse[spectrumColors]]],
							spectrumColors
						]
				];
			order = "Sequential";
			quantity = Length@spectrumColors,
		"Gradient",
			order="Sequential",
		_,
			Nothing
	];
	
	(* override color order *)
	If[output === "ColorFunction"||Length@colorList > 1,
		order = "Sequential"
	];
	
	(* create color order values *)
	order = 
		Switch[order,
			"Equal", Sequence@@(quantity/Position[Array[GCD[quantity,#]&,quantity],1][[If[quantity<3,1,2]]]),
			"EqualReverse", Sequence@@(quantity/Position[Array[GCD[quantity,#]&,quantity],1][[If[quantity<3,1,2]]]),
			"Sequential", quantity,
			"GoldenRatio", GoldenRatio,
			_, order
		];
	
	(* create blended colors *)
	If[blend =!= {} && blend =!= None,
		spectrumColors = Blend[{ColorConvert[#,space],blend[[1]]},blend[[2]]]&/@spectrumColors
	];
	
	(* duplicate spectrum for start location > 0; span ignored if input is list of colors *)
	spectrumColors = 
		If[Length@colorList > 1,
			If[MemberQ[{"DivergingGradient"},form]&&MemberQ[{"Analogic","Analogous"},scheme],
				Table[Blend[ColorConvert[#, space]&/@spectrumColors, n], {n, 0, 1, 1/50}],
				Table[Blend[ColorConvert[#, space]&/@spectrumColors, n], {n, span[[1]], span[[2]], 1/50}]
			],
			Table[
				Blend[
					Join[ColorConvert[#, space]&/@spectrumColors,
					If[MemberQ[{"DivergingGradient"}, form],
						ColorConvert[#, space]&/@spectrumColors,
						Rest[ColorConvert[#, space]&/@spectrumColors]
					]
				], h/2 + n/2],
			{n,span[[1]],span[[2]],1/50}]
		];
	
	(* darken|lighten all colors *)
	spectrumColors = If[shade >= 0, Lighter[#,shade], Darker[#,Abs@shade]]&/@spectrumColors;
	
	(* create palette colors *)
	indexed = 
		If[Length[colorList]>1,
			Table[
				Blend[ColorConvert[#,space]&/@spectrumColors, 
					If[quantity Rescale[step,{1,quantity}]/order > 1,
						FractionalPart[quantity Rescale[step,{1,quantity}]/order],
						quantity Rescale[step,{1,quantity}]/order]
					],
			{step,quantity}],
			Table[
				Blend[ColorConvert[#,space]&/@spectrumColors, FractionalPart[(step-1)/order]], 
			{step,quantity}]
		];
	
	(* repeat palette *)
	If[repeat > 1,
		indexed = Flatten@Table[indexed,{repeat}]
	];
	
	(* shift gradient *)
	Switch[form,
		"Gradient",
			indexed = If[Length@indexed==2,{indexed[[1]],Blend[ColorConvert[#,space]&/@indexed[[{1,2}]],.5],indexed[[2]]},indexed];
			indexed = 
				Table[{
					If[shift < 0,
						((Length[indexed](Rescale[n,{1,Length[indexed]}]/Length[indexed]))^(1-shift)),
						(1-((Length[indexed](Rescale[n,{1,Length[indexed]}]/Length[indexed]))^(1+shift)))
					], 
					indexed[[If[shift<0,n,-n]]]
				}, {n,Length[indexed],1,-1}],
		"DivergingGradient",
			indexed = 
				Switch[Length@indexed,
					1,	{indexed[[1]],Blend[ColorConvert[#,space]&/@{indexed[[1]],White},.5],White,Blend[ColorConvert[#,space]&/@{indexed[[1]],White},.5],indexed[[1]]},
					2,	{indexed[[1]],Blend[ColorConvert[#,space]&/@indexed[[{1,2}]],.5],indexed[[2]],Blend[ColorConvert[#,space]&/@indexed[[{1,2}]],.5],indexed[[1]]},
					3,	{indexed[[1]],Blend[ColorConvert[#,space]&/@indexed[[{1,2}]],.5],indexed[[2]],Blend[ColorConvert[#,space]&/@indexed[[{2,3}]],.5],indexed[[3]]},
					_,
						indexed
				];
			indexed = Table[{Quiet@Interpolation[{(Length[indexed](Rescale[n,{1,Length[indexed]}]/Length[indexed])),(.5+.5Sin[((Length[indexed](Rescale[n,{1,Length[indexed]}]/Length[indexed]))-.5)Pi])}][shift+1],indexed[[n]]},{n,Length[indexed]}],
		_,
			Nothing
	];
	
	(* reverse spectrum *)
	indexed = 
		If[reverse,
			Switch[form,
				"DivergingGradient",
				Switch[OptionValue["ColorSpectrum"],
					"Monochrome",
						{{indexed[[1,1]],indexed[[3,2]]},{indexed[[2,1]],indexed[[2,2]]},{indexed[[3,1]],indexed[[1,2]]},{indexed[[4,1]],indexed[[2,2]]},{indexed[[5,1]],indexed[[3,2]]}},
					"MonochromeStrong",
						{{indexed[[1,1]],indexed[[5,2]]},{indexed[[2,1]],indexed[[4,2]]},{indexed[[3,1]],indexed[[3,2]]},{indexed[[4,1]],indexed[[2,2]]},{indexed[[5,1]],indexed[[1,2]]},{indexed[[6,1]],indexed[[2,2]]},{indexed[[7,1]],indexed[[3,2]]},{indexed[[8,1]],indexed[[4,2]]},{indexed[[9,1]],indexed[[5,2]]}},
					_,
						If[(Length@colorInput<3&&repeat==1)||(Length@colorInput==3&&MemberQ[{Integer,Real,Rational},Head[colorInput[[1]]]]),
							{{indexed[[1,1]],indexed[[3,2]]},{indexed[[2,1]],indexed[[2,2]]},{indexed[[3,1]],indexed[[1,2]]},{indexed[[4,1]],indexed[[2,2]]},{indexed[[5,1]],indexed[[3,2]]}},
							Table[{indexed[[n,1]],indexed[[Length[indexed]-(n-1),2]]},{n,Length[indexed]}]
						]
				],
			"Gradient",
				Table[{indexed[[n,1]],indexed[[Length[indexed]-(n-1),2]]},{n,Length[indexed]}],
			_,
				Reverse@indexed],
			
			indexed
		];
		
	(* gradient cycle *)
	If[cycle!=0,
		If[MemberQ[{"Gradient", "DivergingGradient"}, form],
			indexed=RotateRight[Table[Blend[indexed,n],{n,0,1,.02}],IntegerPart[1.02(cycle/.02)]],
			indexed=RotateRight[indexed,Max[Min[IntegerPart[Round[cycle]],Length@indexed],0]]
		]
	];
	
	(* convert lab harmony to color space *)
	If[spectrum === "LABHarmony" && OptionValue["ColorSpace"] =!= Automatic,
		indexed = ColorConvert[indexed, OptionValue["ColorSpace"]],
		Nothing
	];
	
	(* create output *)
	Switch[output,
	"Image",
		If[MemberQ[{"Gradient","DivergingGradient"},form],
			ArrayPlot[{Table[Blend[indexed,n],{n,0,1,.02}]},ImageSize->{Automatic,16},AspectRatio->1/8,PlotRangePadding->None,Frame->False],
			ArrayPlot[{indexed},ImageSize->{Automatic,16},AspectRatio->1/8,PlotRangePadding->None,Frame->False]
		],
	"ColorFunction",
		With[{index=indexed},Unevaluated[(Blend[index,#3]&)]],
	"Value",
		Switch[form,
			"Gradient"|"DivergingGradient",
				With[{index=indexed}, Unevaluated[(Blend[index,#]&)]],
			_,
				indexed
		],
	_,
		indexed]
]

End[] (* End Private Context *)

EndPackage[]