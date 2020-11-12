(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeDialogPresets[shape_]:=
	Block[{},
		Switch[shape,
			dwShapeTicks,
				{ 
					Button[Graphics[{Gray,Line[{{-.5,0},{-.5,.5}}],Line[{{0,0},{0,.5}}],Line[{{.5,0},{.5,.5}}],Line[{{-.4,0},{-.4,.25}}],Line[{{-.3,0},{-.3,.25}}],Line[{{-.2,0},{-.2,.25}}],Line[{{-.1,0},{-.1,.25}}],Line[{{.1,0},{.1,.25}}],Line[{{.2,0},{.2,.25}}],Line[{{.3,0},{.3,.25}}],Line[{{.4,0},{.4,.25}}]},AspectRatio->1,ImagePadding->6,ImageSize->33],
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = .5;
						$dwShapeOptions[[Position[$dwShapeOptions, "LineWeight"][[1,1]]]][[-1]] = 1;
						$dwShapeOptions[[Position[$dwShapeOptions, "Color"][[1,1]]]][[-1]] = Black;
						$dwShapeOptions[[Position[$dwShapeOptions, "BeginRadialAxis"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "EndRadialAxis"][[1,1]]]][[-1]] = 2Pi;
						$dwShapeOptions[[Position[$dwShapeOptions, "Axis"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "RadialAxis"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickDivisions"][[1,1]]]][[-1]] = {10,10};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickFirstLabelValue"][[1,1]]]][[-1]] = {1,1};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabels"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabelPadding"][[1,1]]]][[-1]] = {0.035, 0.0};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLength"][[1,1]]]][[-1]] = .25;
						$dwShapeOptions[[Position[$dwShapeOptions, "MinorTickLength"][[1,1]]]][[-1]] = .125;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickSide"][[1,1]]]][[-1]] = "Outside",
						ImageSize->{40, 40}, $dwPresetButtonStyle
					],
					Button[Graphics[{Gray,Line[{{-.5,.375},{-.5,-.125}}],Line[{{0,.375},{0,-.125}}],Line[{{.5,.375},{.5,-.125}}],Line[{{-.4,0},{-.4,.25}}],Line[{{-.3,0},{-.3,.25}}],Line[{{-.2,0},{-.2,.25}}],Line[{{-.1,0},{-.1,.25}}],Line[{{.1,0},{.1,.25}}],Line[{{.2,0},{.2,.25}}],Line[{{.3,0},{.3,.25}}],Line[{{.4,0},{.4,.25}}]},AspectRatio->1,ImagePadding->6,ImageSize->33],
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = .5;
						$dwShapeOptions[[Position[$dwShapeOptions, "LineWeight"][[1,1]]]][[-1]] = 1;
						$dwShapeOptions[[Position[$dwShapeOptions, "Color"][[1,1]]]][[-1]] = Black;
						$dwShapeOptions[[Position[$dwShapeOptions, "BeginRadialAxis"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "EndRadialAxis"][[1,1]]]][[-1]] = 2Pi;
						$dwShapeOptions[[Position[$dwShapeOptions, "Axis"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "RadialAxis"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickDivisions"][[1,1]]]][[-1]] = {10,10};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickFirstLabelValue"][[1,1]]]][[-1]] = {1,1};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabels"][[1,1]]]][[-1]] = True;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabelPadding"][[1,1]]]][[-1]] = {0.035, 0.0};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLength"][[1,1]]]][[-1]] = .25;
						$dwShapeOptions[[Position[$dwShapeOptions, "MinorTickLength"][[1,1]]]][[-1]] = .125;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickSide"][[1,1]]]][[-1]] = "Center",
						ImageSize->{40, 40}, $dwPresetButtonStyle
					],
					Button[Graphics[{Gray,Line[{{-.5,.25},{-.5,-.25}}],Line[{{0,.25},{0,-.25}}],Line[{{.5,.25},{.5,-.25}}],Line[{{-.4,0},{-.4,.25}}],Line[{{-.3,0},{-.3,.25}}],Line[{{-.2,0},{-.2,.25}}],Line[{{-.1,0},{-.1,.25}}],Line[{{.1,0},{.1,.25}}],Line[{{.2,0},{.2,.25}}],Line[{{.3,0},{.3,.25}}],Line[{{.4,0},{.4,.25}}]},AspectRatio->1,ImagePadding->6,ImageSize->33],
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = .5;
						$dwShapeOptions[[Position[$dwShapeOptions, "LineWeight"][[1,1]]]][[-1]] = 1;
						$dwShapeOptions[[Position[$dwShapeOptions, "Color"][[1,1]]]][[-1]] = Black;
						$dwShapeOptions[[Position[$dwShapeOptions, "BeginRadialAxis"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "EndRadialAxis"][[1,1]]]][[-1]] = 2Pi;
						$dwShapeOptions[[Position[$dwShapeOptions, "Axis"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "RadialAxis"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickDivisions"][[1,1]]]][[-1]] = {10,10};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickFirstLabelValue"][[1,1]]]][[-1]] = {1,1};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabels"][[1,1]]]][[-1]] = True;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabelPadding"][[1,1]]]][[-1]] = {0.035, 0.0};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLength"][[1,1]]]][[-1]] = .25;
						$dwShapeOptions[[Position[$dwShapeOptions, "MinorTickLength"][[1,1]]]][[-1]] = .125;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickSide"][[1,1]]]][[-1]] = "Inside",
						ImageSize->{40, 40}, $dwPresetButtonStyle
					],
					Button[Graphics[{Gray,Table[Line[{{Sin[x],Cos[x]},.5{Sin[x],Cos[x]}}],{x,0,2Pi-Pi/4,Pi/4}],Table[Line[{.8{Sin[x],Cos[x]},.5{Sin[x],Cos[x]}}],{x,Pi/16,2Pi,Pi/16}]},AspectRatio->1,ImagePadding->0,ImageSize->33],
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = .5;
						$dwShapeOptions[[Position[$dwShapeOptions, "LineWeight"][[1,1]]]][[-1]] = 1;
						$dwShapeOptions[[Position[$dwShapeOptions, "Color"][[1,1]]]][[-1]] = Black;
						$dwShapeOptions[[Position[$dwShapeOptions, "BeginRadialAxis"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "EndRadialAxis"][[1,1]]]][[-1]] = 2Pi;
						$dwShapeOptions[[Position[$dwShapeOptions, "Axis"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "RadialAxis"][[1,1]]]][[-1]] = True;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickDivisions"][[1,1]]]][[-1]] = {10,10};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickFirstLabelValue"][[1,1]]]][[-1]] = {1,1};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabels"][[1,1]]]][[-1]] = True;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabelPadding"][[1,1]]]][[-1]] = {0.035, 0.0};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLength"][[1,1]]]][[-1]] = .25;
						$dwShapeOptions[[Position[$dwShapeOptions, "MinorTickLength"][[1,1]]]][[-1]] = .125;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickSide"][[1,1]]]][[-1]] = "Outside",
						ImageSize->{40, 40}, $dwPresetButtonStyle
					],
					Button[Graphics[{Gray,Table[Line[{{Sin[x],Cos[x]},.5{Sin[x],Cos[x]}}],{x,0,2Pi-Pi/4,Pi/4}],Table[Line[{.65{Sin[x],Cos[x]},.85{Sin[x],Cos[x]}}],{x,Pi/16,2Pi,Pi/16}]},AspectRatio->1,ImagePadding->0,ImageSize->33],
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = .5;
						$dwShapeOptions[[Position[$dwShapeOptions, "LineWeight"][[1,1]]]][[-1]] = 1;
						$dwShapeOptions[[Position[$dwShapeOptions, "Color"][[1,1]]]][[-1]] = Black;
						$dwShapeOptions[[Position[$dwShapeOptions, "BeginRadialAxis"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "EndRadialAxis"][[1,1]]]][[-1]] = 2Pi;
						$dwShapeOptions[[Position[$dwShapeOptions, "Axis"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "RadialAxis"][[1,1]]]][[-1]] = True;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickDivisions"][[1,1]]]][[-1]] = {10,10};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickFirstLabelValue"][[1,1]]]][[-1]] = {1,1};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabels"][[1,1]]]][[-1]] = True;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabelPadding"][[1,1]]]][[-1]] = {0.035, 0.0};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLength"][[1,1]]]][[-1]] = .25;
						$dwShapeOptions[[Position[$dwShapeOptions, "MinorTickLength"][[1,1]]]][[-1]] = .125;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickSide"][[1,1]]]][[-1]] = "Center",
						ImageSize->{40, 40}, $dwPresetButtonStyle
					],
					Button[Graphics[{Gray,Table[Line[{{Sin[x],Cos[x]},.5{Sin[x],Cos[x]}}],{x,0,2Pi-Pi/4,Pi/4}],Table[Line[{{Sin[x],Cos[x]},.75{Sin[x],Cos[x]}}],{x,Pi/16,2Pi,Pi/16}]},AspectRatio->1,ImagePadding->2,ImageSize->33],
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = .5;
						$dwShapeOptions[[Position[$dwShapeOptions, "LineWeight"][[1,1]]]][[-1]] = 1;
						$dwShapeOptions[[Position[$dwShapeOptions, "Color"][[1,1]]]][[-1]] = Black;
						$dwShapeOptions[[Position[$dwShapeOptions, "BeginRadialAxis"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "EndRadialAxis"][[1,1]]]][[-1]] = 2Pi;
						$dwShapeOptions[[Position[$dwShapeOptions, "Axis"][[1,1]]]][[-1]] = False;
						$dwShapeOptions[[Position[$dwShapeOptions, "RadialAxis"][[1,1]]]][[-1]] = True;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickDivisions"][[1,1]]]][[-1]] = {10,10};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickFirstLabelValue"][[1,1]]]][[-1]] = {1,1};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabels"][[1,1]]]][[-1]] = True;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLabelPadding"][[1,1]]]][[-1]] = {0.035, 0.0};
						$dwShapeOptions[[Position[$dwShapeOptions, "TickLength"][[1,1]]]][[-1]] = .25;
						$dwShapeOptions[[Position[$dwShapeOptions, "MinorTickLength"][[1,1]]]][[-1]] = .125;
						$dwShapeOptions[[Position[$dwShapeOptions, "TickSide"][[1,1]]]][[-1]] = "Inside",
						ImageSize->{40, 40}, $dwPresetButtonStyle
					]
				},
			dwShapeFire,
				{
					Button[Show[Import[$dwFileDirectory<>"Media/firepreset-01.png"], ImageSize->54],
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = 2;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireFuel"][[1,1]]]][[-1]] = .0225;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireGlow"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShape"][[1,1]]]][[-1]] = .9;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShift"][[1,1]]]][[-1]] = .125;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSquash"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSqueeze"][[1,1]]]][[-1]] = .2;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireStretch"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireTurbulence"][[1,1]]]][[-1]] = 14;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireEdgeFalloff"][[1,1]]]][[-1]] = 2;
						$dwShapeOptions[[Position[$dwShapeOptions, "RandomSeed"][[1,1]]]][[-1]] = 1,
						ImageSize->{64, 43}, $dwPresetButtonStyle
					],
					Button[Show[Import[$dwFileDirectory<>"Media/firepreset-02.png"], ImageSize->54], 
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = 2;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireFuel"][[1,1]]]][[-1]] = .0225;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireGlow"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShape"][[1,1]]]][[-1]] = .9;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShift"][[1,1]]]][[-1]] = .45;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSquash"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSqueeze"][[1,1]]]][[-1]] = .2;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireStretch"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireTurbulence"][[1,1]]]][[-1]] = 13;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireEdgeFalloff"][[1,1]]]][[-1]] = 3;
						$dwShapeOptions[[Position[$dwShapeOptions, "RandomSeed"][[1,1]]]][[-1]] = 3,
						ImageSize->{64, 43}, $dwPresetButtonStyle
					],
					Button[Show[Import[$dwFileDirectory<>"Media/firepreset-03.png"], ImageSize->54], 
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = 2;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireFuel"][[1,1]]]][[-1]] = .0275;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireGlow"][[1,1]]]][[-1]] = 1;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShape"][[1,1]]]][[-1]] = .775;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShift"][[1,1]]]][[-1]] = .425;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSquash"][[1,1]]]][[-1]] = .2;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSqueeze"][[1,1]]]][[-1]] = .4;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireStretch"][[1,1]]]][[-1]] = .4;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireTurbulence"][[1,1]]]][[-1]] = 13;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireEdgeFalloff"][[1,1]]]][[-1]] = 10;
						$dwShapeOptions[[Position[$dwShapeOptions, "RandomSeed"][[1,1]]]][[-1]] = 1,
						ImageSize->{64, 43}, $dwPresetButtonStyle
					],
					Button[Show[Import[$dwFileDirectory<>"Media/firepreset-04.png"], ImageSize->54],
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = 2;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireFuel"][[1,1]]]][[-1]] = .0275;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireGlow"][[1,1]]]][[-1]] = .5;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShape"][[1,1]]]][[-1]] = .775;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShift"][[1,1]]]][[-1]] = .45;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSquash"][[1,1]]]][[-1]] = .1;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSqueeze"][[1,1]]]][[-1]] = .4;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireStretch"][[1,1]]]][[-1]] = .7;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireTurbulence"][[1,1]]]][[-1]] = 9.8;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireEdgeFalloff"][[1,1]]]][[-1]] = 10;
						$dwShapeOptions[[Position[$dwShapeOptions, "RandomSeed"][[1,1]]]][[-1]] = 1,
						ImageSize->{64, 43}, $dwPresetButtonStyle
					],
					Button[Show[Import[$dwFileDirectory<>"Media/firepreset-05.png"], ImageSize->54],
						$dwShapeOptions[[Position[$dwShapeOptions, "Size"][[1,1]]]][[-1]] = 2;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireFuel"][[1,1]]]][[-1]] = .02;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireGlow"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShape"][[1,1]]]][[-1]] = .95;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireShift"][[1,1]]]][[-1]] = -.025;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSquash"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireSqueeze"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireStretch"][[1,1]]]][[-1]] = 0;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireTurbulence"][[1,1]]]][[-1]] = 10;
						$dwShapeOptions[[Position[$dwShapeOptions, "FireEdgeFalloff"][[1,1]]]][[-1]] = 7;
						$dwShapeOptions[[Position[$dwShapeOptions, "RandomSeed"][[1,1]]]][[-1]] = 4,
						ImageSize->{64, 43}, $dwPresetButtonStyle
					]
				},
			_,
				{}
				
		]
	]

End[] (* End Private Context *)

EndPackage[]