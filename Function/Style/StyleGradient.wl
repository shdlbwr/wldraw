(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwLineGradientSettings[n_]:=
	DynamicModule[{temp},
		Dynamic@Tooltip[Button[$dwIconGradientSettings,
			CreateDialog[Pane[Dynamic@Column[Flatten@{
					Dynamic[Which[
						$dwLineGradients[[n,1]] === "2ColorLineGradient",
							Grid[{
								{Button["Reverse colors", temp = $dwLineGradients[[n,2]]; $dwLineGradients[[n,2,1,2]] = temp[[4,2]]; $dwLineGradients[[n,2,2,2]] = temp[[3,2]]; $dwLineGradients[[n,2,3,2]] = temp[[2,2]]; $dwLineGradients[[n,2,4,2]] = temp[[1,2]]; dwUpdateSelected[n], ImageSize->150]},
								{Row[{
									EventHandler[ColorSetter[Dynamic@$dwLineGradients[[n,2,1,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwLineGradients[[n,2,4,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True]
								}]}
							}],
						$dwLineGradients[[n,1]] === "3ColorLineGradient",
							Grid[{
								{Button["Reverse colors", temp = $dwLineGradients[[n,2]]; $dwLineGradients[[n,2,1,2]] = temp[[4,2]]; $dwLineGradients[[n,2,4,2]] = temp[[1,2]]; dwUpdateSelected[n], ImageSize->150]},
								{Row[{
									EventHandler[ColorSetter[Dynamic@$dwLineGradients[[n,2,1,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwLineGradients[[n,2,2,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwLineGradients[[n,2,4,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True]
								}]},
								{""},
								{Style["Color is affected by quantity and position of line points.", Italic]},
								{""}
							}],
						$dwLineGradients[[n,1]] === "4ColorLineGradient",
							Grid[{
								{Button["Reverse colors", temp = $dwLineGradients[[n,2]]; $dwLineGradients[[n,2,1,2]] = temp[[4,2]]; $dwLineGradients[[n,2,2,2]] = temp[[3,2]]; $dwLineGradients[[n,2,3,2]] = temp[[2,2]]; $dwLineGradients[[n,2,4,2]] = temp[[1,2]]; dwUpdateSelected[n], ImageSize->150]},
								{Row[{
									EventHandler[ColorSetter[Dynamic@$dwLineGradients[[n,2,1,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwLineGradients[[n,2,2,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwLineGradients[[n,2,3,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwLineGradients[[n,2,4,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True]
								}]},
								{""},
								{Style["Color is affected by quantity and position of line points.", Italic]},
								{""}
							}],
					True,
						"No settings to change."
				], SynchronousUpdating->$dwSynchronousUpdating],
				Row@{Pane[Row[{
					DefaultButton["Close", dwSetUndo[]; DialogReturn[],ImageSize->70]
				}],Alignment->Center,ImageSize->280]}},
			Alignment->Center],ImageSize->280],WindowTitle->"Line gradient settings",Modal->True],
		ImageSize->{$dwStyleButtonHeight,$dwStyleButtonHeight}, Background->GrayLevel[.85], BaselinePosition->Bottom], "Gradient settings", TooltipDelay->$dwTooltipDelay]
	]

dwGradientSettings[n_]:=
	Tooltip[Button[$dwIconGradientSettings,
		dwGradientSettingsDialog[n],
	ImageSize->{$dwStyleButtonHeight,$dwStyleButtonHeight}, Background->GrayLevel[.85], BaselinePosition->Bottom], "Gradient settings", TooltipDelay->$dwTooltipDelay]

dwGradientSettingsDialog[n_]:=
	DynamicModule[{size=115, step=.01, temp, colorRotation = 0},
		CreateDialog[Pane[Column[Flatten@{
				Which[
					MemberQ[{None,"Solid"}, $dwStyle[[n,8,1]]],
						"No settings to change.",
					MemberQ[{"OpacityGradient"},$dwStyle[[n,8,1]]],
						$dwPStart = $dwP[[n]];
						Column[{
							Grid[{
								{Button["Reverse colors", temp = $dwStyle[[n,8,2]]; $dwStyle[[n,8,2,1,2]] = temp[[4,2]]; $dwStyle[[n,8,2,2,2]] = temp[[3,2]]; $dwStyle[[n,8,2,3,2]] = temp[[2,2]]; $dwStyle[[n,8,2,4,2]] = temp[[1,2]]; dwUpdateSelected[n]], SpanFromLeft, SpanFromLeft, SpanFromLeft},
								{
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,1,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,2,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,3,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,4,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True]
								},
								{$dwStyle[[n,8,2,1,1]], $dwStyle[[n,8,2,2,1]], $dwStyle[[n,8,2,3,1]], $dwStyle[[n,8,2,4,1]]}
							}],
							Button["even color and space from 0 to 1", $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,2,1]] = 1/3; $dwStyle[[n,8,2,3,1]] = 2/3; $dwStyle[[n,8,2,4,1]] = 1; $dwStyle[[n,8,2,2,2]] = Blend[{$dwStyle[[n,8,2,1,2]] , $dwStyle[[n,8,2,4,2]]},1/3]; $dwStyle[[n,8,2,3,2]] = Blend[{$dwStyle[[n,8,2,1,2]], $dwStyle[[n,8,2,4,2]]},2/3]; dwUpdateSelected[n]],
							"",
							Row[{"CLOCKWISE COLOR ROTATION"(*, IntegerPart[colorRotation*(360/Length[$dwP[[n]]])]*)}],
							Row[{"0 ", 
								EventHandler[
									Slider[Dynamic@colorRotation, {0, Length[$dwP[[n]]], 1}, ContinuousAction->False], 
									{"MouseUp":>($dwP[[n]] = RotateRight[$dwPStart, Length[$dwP[[n]]]-colorRotation])}, 
									PassEventsDown->True
								],
								" 360"
							}],
							Row[{"steps determined by number of path points"}]
							}, Alignment->Center],
					Head[$dwStyle[[n,8,1]]] === List && MemberQ[{"BlendGradient"}, $dwStyle[[n,8,1,1]]],
						Column[{
							Style[Row[{"Blend settings for source object ", n,"."}], 15],
							"",
							Row[{"Target object: ", 
								ActionMenu[Dynamic@$dwStyle[[n,8,1,2]], With[{bon = #},bon:>($dwStyle[[n,8,1,2]] = bon)]&/@Range[Length[$dwP]], Appearance->"PopupMenu"], 
								"          Blends: ", 
								ActionMenu[Dynamic@$dwStyle[[n,8,1,3]], With[{bqn = #},bqn:>($dwStyle[[n,8,1,3]] = bqn)]&/@Join[Range[9], Range[10,50,5]], Appearance->"PopupMenu"]
							}],
							Row[{"resolution ", EventHandler[Slider[Dynamic@$dwBlendResolution, {1,120,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True], " ", Dynamic@$dwBlendResolution}],
							Row[{"radiate ", EventHandler[Slider[Dynamic@$dwStyle[[n,8,1,4]], {-1,1,.1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True], " ", Dynamic@$dwStyle[[n,8,1,4]]}],
							"",
							Pane[Column[{
								"Blend shape and style between source to target objects.",
								"Blended objects are only displayed in preview mode."
							}, Alignment->Center], Alignment->Center, ImageSize->280]
						}, Alignment->Center],
					MemberQ[{"Radial"},$dwStyle[[n,8,1]]],
						Grid[{
							{Button["Reverse colors", temp = $dwStyle[[n,8,2,1,2]]; $dwStyle[[n,8,2,1,2]] = $dwStyle[[n,8,2,4,2]]; $dwStyle[[n,8,2,4,2]] = temp; dwUpdateSelected[n]]},
							{Row[{
								EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,1,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
								EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,4,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True]}]
							},
							{""},
							{"radial center"},
							{EventHandler[Slider2D[Dynamic@$dwStyle[[n,9]][[{1,2}]], {{-.66,-.66},{.66,.66}}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{Row[{"expand ",EventHandler[Slider[Dynamic@$dwStyle[[n,9,3]], {1,4}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]}]},
							{Row[{"square ",EventHandler[Slider[Dynamic@$dwStyle[[n,9,4]], {0,10,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]}]},
							{Row[{Button["reset",$dwStyle[[n,9,1]]=0; $dwStyle[[n,9,2]]=0; $dwStyle[[n,9,3]]=1; $dwStyle[[n,9,4]]=0; dwUpdateSelected[n], ImageSize->120]}]}
						}],
					MemberQ[{"EdgeDark"}, $dwStyle[[n,8,1]]],
						Grid[{
							{Style["Color inherited from fill color.", Italic], SpanFromLeft, SpanFromLeft},
							{"darker",EventHandler[Slider[Dynamic@$dwStyle[[n,9,1]], {1,200}, ContinuousAction->False, ImageSize->175], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True],""},
							{"size",EventHandler[Slider[Dynamic@$dwStyle[[n,9,2]], {1,100}, ContinuousAction->False, ImageSize->175], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True],""},
							{"up",EventHandler[Slider[Dynamic@$dwStyle[[n,9,3]], {50,-50,-1}, ContinuousAction->False, ImageSize->175], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True],"down"},
							{"left",EventHandler[Slider[Dynamic@$dwStyle[[n,9,4]], {50,-50,-1}, ContinuousAction->False, ImageSize->175], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True],"right"},
							{Row[{Button["reset position",$dwStyle[[n,9,3]]=0; $dwStyle[[n,9,4]]=0; dwUpdateSelected[n], ImageSize->120], Button["update gradient", dwUpdateSelected[n], ImageSize->120]}], SpanFromLeft, SpanFromLeft}
						}],
					MemberQ[{"EdgeFade"}, $dwStyle[[n,8,1]]],
						Grid[{
							{Style["Color inherited from fill color.", Italic], SpanFromLeft},
							{"blur",EventHandler[Slider[Dynamic@$dwStyle[[n,9,1]], {0,40}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"size",EventHandler[Slider[Dynamic@$dwStyle[[n,9,2]], {0,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{Button["update gradient", dwUpdateSelected[n], ImageSize->120], SpanFromLeft}
						}],
					MemberQ[{"Emboss", "EmbossReverse"}, $dwStyle[[n,8,1]]],
						Grid[{
							{Style["Color inherited from fill color.", Italic], SpanFromLeft},
							{"emboss",EventHandler[Slider[Dynamic@$dwStyle[[n,9,2]], {0,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"size", EventHandler[Slider[Dynamic@$dwStyle[[n,9,1]], {0,50}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"pad",EventHandler[Slider[Dynamic@$dwStyle[[n,8,2,2,1]], {0,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"sharpen",EventHandler[Slider[Dynamic@$dwStyle[[n,8,2,1,1]], {0,100}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"highlight",EventHandler[Slider[Dynamic@$dwStyle[[n,8,2,3,1]], {0,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"shadow",EventHandler[Slider[Dynamic@$dwStyle[[n,8,2,4,1]], {0,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{Button["update gradient", dwUpdateSelected[n], ImageSize->120], SpanFromLeft},
							{Grid[{{
								Button[Framed[Show[Import[$dwFileDirectory<>"Media/gradientEmboss-1.png"],ImageSize->30], FrameMargins->0], $dwStyle[[n,9,2]] = 0; $dwStyle[[n,9,1]] = 50; $dwStyle[[n,8,2,2,1]] = 0; $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,3,1]] = 1; $dwStyle[[n,8,2,4,1]] = .5; dwUpdateSelected[n], Appearance->None],
								Button[Framed[Show[Import[$dwFileDirectory<>"Media/gradientEmboss-2.png"],ImageSize->30], FrameMargins->0], $dwStyle[[n,9,2]] = 1; $dwStyle[[n,9,1]] = 50; $dwStyle[[n,8,2,2,1]] = 0; $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,3,1]] = 1; $dwStyle[[n,8,2,4,1]] = .5; dwUpdateSelected[n], Appearance->None],
								Button[Framed[Show[Import[$dwFileDirectory<>"Media/gradientEmboss-3.png"],ImageSize->30], FrameMargins->0], $dwStyle[[n,9,2]] = 0; $dwStyle[[n,9,1]] = 25; $dwStyle[[n,8,2,2,1]] = 0; $dwStyle[[n,8,2,1,1]] = 20; $dwStyle[[n,8,2,3,1]] = .75; $dwStyle[[n,8,2,4,1]] = .5; dwUpdateSelected[n], Appearance->None],
								Button[Framed[Show[Import[$dwFileDirectory<>"Media/gradientEmboss-4.png"],ImageSize->30], FrameMargins->0], $dwStyle[[n,9,2]] = 1; $dwStyle[[n,9,1]] = 25; $dwStyle[[n,8,2,2,1]] = 0; $dwStyle[[n,8,2,1,1]] = 20; $dwStyle[[n,8,2,3,1]] = .75; $dwStyle[[n,8,2,4,1]] = .5; dwUpdateSelected[n], Appearance->None],
								Button[Framed[Show[Import[$dwFileDirectory<>"Media/gradientEmboss-5.png"],ImageSize->30], FrameMargins->0], $dwStyle[[n,9,2]] = 0; $dwStyle[[n,9,1]] = 10; $dwStyle[[n,8,2,2,1]] = .25; $dwStyle[[n,8,2,1,1]] = 10; $dwStyle[[n,8,2,3,1]] = .75; $dwStyle[[n,8,2,4,1]] = .5; dwUpdateSelected[n], Appearance->None],
								Button[Framed[Show[Import[$dwFileDirectory<>"Media/gradientEmboss-6.png"],ImageSize->30], FrameMargins->0], $dwStyle[[n,9,2]] = 1; $dwStyle[[n,9,1]] = 10; $dwStyle[[n,8,2,2,1]] = .25; $dwStyle[[n,8,2,1,1]] = 10; $dwStyle[[n,8,2,3,1]] = .75; $dwStyle[[n,8,2,4,1]] = .5; dwUpdateSelected[n], Appearance->None]
							}},Spacings->{1,0}],SpanFromLeft}
						}],
					MemberQ[{"Noise"}, $dwStyle[[n,8,1]]],
						Grid[{
							{Style["Color inherited from fill color.", Italic], SpanFromLeft},
							{"amount",EventHandler[Slider[Dynamic@$dwStyle[[n,9,1]], {0,.99}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"contrast",EventHandler[Slider[Dynamic@$dwStyle[[n,9,2]], {0,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]}
							
						}],
					MemberQ[{"Rim", "RimReverse"}, $dwStyle[[n,8,1]]],
						Grid[{
							{Style["Color inherited from fill color.", Italic], SpanFromLeft},
							{"size",EventHandler[Slider[Dynamic@$dwStyle[[n,9,1]], {0,40}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"angle",EventHandler[Slider[Dynamic@$dwStyle[[n,9,2]], {-Pi,Pi}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"highlight",EventHandler[Slider[Dynamic@$dwStyle[[n,8,2,1,1]], {0,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{"shadow",EventHandler[Slider[Dynamic@$dwStyle[[n,8,2,2,1]], {0,1}, ContinuousAction->False, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]},
							{Button["update gradient", dwUpdateSelected[n], ImageSize->120], SpanFromLeft}
						}],
					MemberQ[{"Angle"}, $dwStyle[[n,8,1]]],
						Column[{
							Grid[{
								{Button["Reverse colors", temp = $dwStyle[[n,8,2]]; $dwStyle[[n,8,2,1,2]] = temp[[4,2]]; $dwStyle[[n,8,2,2,2]] = temp[[3,2]]; $dwStyle[[n,8,2,3,2]] = temp[[2,2]]; $dwStyle[[n,8,2,4,2]] = temp[[1,2]]; dwUpdateSelected[n]], SpanFromLeft, SpanFromLeft, SpanFromLeft},
								{
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,1,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,2,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,3,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,4,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True]
								},
								{$dwStyle[[n,8,2,1,1]], Dynamic@$dwStyle[[n,8,2,2,1]], Dynamic@$dwStyle[[n,8,2,3,1]], $dwStyle[[n,8,2,4,1]]}
							}],
							Button[Row[{"even color from ", $dwStyle[[n,8,2,1,1]], " to ", $dwStyle[[n,8,2,4,1]]}], 
								$dwStyle[[n,8,2,2,1]] = $dwStyle[[n,8,2,1,1]]+(($dwStyle[[n,8,2,4,1]]-$dwStyle[[n,8,2,1,1]])/3); 
								$dwStyle[[n,8,2,3,1]] = $dwStyle[[n,8,2,1,1]]+2(($dwStyle[[n,8,2,4,1]]-$dwStyle[[n,8,2,1,1]])/3); 
								$dwStyle[[n,8,2,2,2]] = Blend[{$dwStyle[[n,8,2,1,2]] , $dwStyle[[n,8,2,4,2]]},1/3]; 
								$dwStyle[[n,8,2,3,2]] = Blend[{$dwStyle[[n,8,2,1,2]], $dwStyle[[n,8,2,4,2]]},2/3]; 
								dwUpdateSelected[n]
							],
							EventHandler[IntervalSlider[Dynamic[{$dwStyle[[n,8,2,2,1]],$dwStyle[[n,8,2,3,1]]}], {0, 1, .05}, Appearance->"UpArrow",ContinuousAction->False,ImageSize->300], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True],
							"",
							Panel[Column[{
								"COLOR RANGE",
								Row[{
									EventHandler[Slider[Dynamic@$dwStyle[[n,8,4,1]], {0,.5}, ContinuousAction->True, ImageSize->Small], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[Slider[Dynamic@$dwStyle[[n,8,4,2]], {.5,1}, ContinuousAction->True, ImageSize->Small], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]
								}],
								"",
								"ANGLE",
								Row[{EventHandler[Slider[Dynamic@$dwStyle[[n,8,3]], {0,360,1}, ContinuousAction->True, ImageSize->Medium], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]," ",Dynamic@$dwStyle[[n,8,3]]}],
								Row[Join[
									{EventHandler[Button["0", $dwStyle[[n,8,3]] = 0, $dwPresetButtonStyle], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True]},
									Table[With[{a = a}, EventHandler[Button["+"<>ToString[a], $dwStyle[[n,8,3]] = Min[Max[$dwStyle[[n,8,3]] += a, 0], 360], $dwPresetButtonStyle], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True]], {a, {1,5,10,15,30,45}}]
								]]
							}, Alignment->Center]]
						}, Alignment->Center],
					True,
						(* settings *)
						Column[{
							Grid[{
								{Button["Reverse colors", temp = $dwStyle[[n,8,2]]; $dwStyle[[n,8,2,1,2]] = temp[[4,2]]; $dwStyle[[n,8,2,2,2]] = temp[[3,2]]; $dwStyle[[n,8,2,3,2]] = temp[[2,2]]; $dwStyle[[n,8,2,4,2]] = temp[[1,2]]; dwUpdateSelected[n]], SpanFromLeft, SpanFromLeft, SpanFromLeft},
								{
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,1,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,2,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,3,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True],
									EventHandler[ColorSetter[Dynamic@$dwStyle[[n,8,2,4,2]]], {"MouseClicked":>(dwUpdateSelected[n])}, PassEventsDown->True]
								},
								{$dwStyle[[n,8,2,1,1]], Dynamic@$dwStyle[[n,8,2,2,1]], Dynamic@$dwStyle[[n,8,2,3,1]], $dwStyle[[n,8,2,4,1]]}
							}],
							Button["even color and space from 0 to 1", 
								$dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,2,1]] = 1/3; $dwStyle[[n,8,2,3,1]] = 2/3; $dwStyle[[n,8,2,4,1]] = 1; 
								$dwStyle[[n,8,2,2,2]] = Blend[{$dwStyle[[n,8,2,1,2]] , $dwStyle[[n,8,2,4,2]]},1/3]; 
								$dwStyle[[n,8,2,3,2]] = Blend[{$dwStyle[[n,8,2,1,2]], $dwStyle[[n,8,2,4,2]]},2/3]; 
								dwUpdateSelected[n]
							],
							EventHandler[IntervalSlider[Dynamic[{$dwStyle[[n,8,2,2,1]],$dwStyle[[n,8,2,3,1]]}], {0, 1, .05}, Appearance->"UpArrow",ContinuousAction->False,ImageSize->300], {"MouseUp":>(dwUpdateSelected[n])}, PassEventsDown->True]
						}, Alignment->Center]
			],
			Row@{Pane[Row[{
				DefaultButton["Close", dwSetUndo[]; DialogReturn[],ImageSize->70]
			}],Alignment->Center,ImageSize->300]}},
		Alignment->Center],ImageSize->300],WindowTitle->"Gradient settings",Modal->True]
	]
	
dwGradientType[n_]:=
	If[MemberQ[{"stroke", "arrow"}, $dwStyleMode],
		
		ActionMenu[Dynamic@If[$dwSelected === {}, "None", $dwLineGradients[[n,1]]],
			{
				None:>(dwSetUndo[]; If[$dwLineGradients[[n]] === {}, $dwLineGradients[[n]] = $dwDefaultGradient]; $dwLineGradients[[n,1]] = None; $dwLineGradients[[n,2,1,1]] = 0; $dwLineGradients[[n,2,2,1]] = 1/3; $dwLineGradients[[n,2,3,1]] = 2/3; $dwLineGradients[[n,2,4,1]] = 1),
				Delimiter,
				"2ColorLineGradient":>(dwSetUndo[]; If[$dwLineGradients[[n]] === {}, $dwLineGradients[[n]] = $dwDefaultGradient]; $dwLineGradients[[n,1]] = "2ColorLineGradient"; $dwLineGradients[[n,2,1,1]] = 0; $dwLineGradients[[n,2,2,1]] = 1/3; $dwLineGradients[[n,2,3,1]] = 2/3; $dwLineGradients[[n,2,4,1]] = 1; dwUpdateSelected[n, "StylesToUpdate"->"lineGradient"]),
				"3ColorLineGradient":>(dwSetUndo[]; If[$dwLineGradients[[n]] === {}, $dwLineGradients[[n]] = $dwDefaultGradient]; $dwLineGradients[[n,1]] = "3ColorLineGradient"; $dwLineGradients[[n,2,1,1]] = 0; $dwLineGradients[[n,2,2,1]] = 1/3; $dwLineGradients[[n,2,3,1]] = 2/3; $dwLineGradients[[n,2,4,1]] = 1; dwUpdateSelected[n, "StylesToUpdate"->"lineGradient"]),
				"4ColorLineGradient":>(dwSetUndo[]; If[$dwLineGradients[[n]] === {}, $dwLineGradients[[n]] = $dwDefaultGradient]; $dwLineGradients[[n,1]] = "4ColorLineGradient"; $dwLineGradients[[n,2,1,1]] = 0; $dwLineGradients[[n,2,2,1]] = 1/3; $dwLineGradients[[n,2,3,1]] = 2/3; $dwLineGradients[[n,2,4,1]] = 1; dwUpdateSelected[n, "StylesToUpdate"->"lineGradient"])
			},
		Enabled->If[MemberQ[Flatten[$dwCompoundPathLayers], n], False, True],
		ImageSize->{140,$dwStyleButtonHeight}, Alignment->Center, Appearance->"Palette", BaselinePosition->Bottom],
		
		ActionMenu[Dynamic@If[$dwSelected === {}, $dwImageFilterHead, If[Head[$dwStyle[[n,8,1]]] === List, $dwStyle[[n,8,1,1]], $dwStyle[[n,8,1]]]],
			Flatten[{
				None:>(dwSetUndo[]; $dwStyle[[n,8,1]] = None; $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,2,1]] = 1/3; $dwStyle[[n,8,2,3,1]] = 2/3; $dwStyle[[n,8,2,4,1]] = 1; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
				"Update selected gradients":>(dwUpdateGradients[$dwSelected]),
				If[MemberQ[{BezierCurve, BSplineCurve, Polygon}, $dwHead[[n]]],
					Delimiter,
					{}
				],
				If[MemberQ[{Polygon}, $dwHead[[n]]],
					"OpacityGradient":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "OpacityGradient"; $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,2,1]] = 1/3; $dwStyle[[n,8,2,3,1]] = 2/3; $dwStyle[[n,8,2,4,1]] = 1; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
					{}
				],
				If[MemberQ[{BezierCurve, BSplineCurve, Polygon}, $dwHead[[n]]],
					"BlendGradient":>(dwSetUndo[]; $dwStyle[[n,8,1]] = {"BlendGradient", n, 1, 0}; dwUpdateGradients[{n}]; dwUpdateSelected[n]; dwGradientSettingsDialog[n]),
					{}
				],
				If[MemberQ[{BezierCurve, Polygon}, $dwHead[[n]]],
					{
						Delimiter,
						"Solid":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "Solid"; $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,2,1]] = 1/3; $dwStyle[[n,8,2,3,1]] = 2/3; $dwStyle[[n,8,2,4,1]] = 1; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"Horizontal":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "Horizontal"; $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,2,1]] = 1/3; $dwStyle[[n,8,2,3,1]] = 2/3; $dwStyle[[n,8,2,4,1]] = 1; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"Vertical":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "Vertical"; $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,2,1]] = 1/3; $dwStyle[[n,8,2,3,1]] = 2/3; $dwStyle[[n,8,2,4,1]] = 1; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"Angle":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "Angle"; $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,2,1]] = 1/3; $dwStyle[[n,8,2,3,1]] = 2/3; $dwStyle[[n,8,2,4,1]] = 1; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"Radial":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "Radial"; $dwStyle[[n,9,1]] = 0; $dwStyle[[n,9,2]] = 0; $dwStyle[[n,9,3]] = 1; $dwStyle[[n,9,4]] = 0; $dwStyle[[n,8,2,1,1]] = 0; $dwStyle[[n,8,2,2,1]] = 1/3; $dwStyle[[n,8,2,3,1]] = 2/3; $dwStyle[[n,8,2,4,1]] = 1; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						Delimiter,
						"EdgeDark":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "EdgeDark"; $dwStyle[[n,9,1]] = 15; $dwStyle[[n,9,2]] = 30; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"EdgeFade":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "EdgeFade"; $dwStyle[[n,9,1]] = 10; $dwStyle[[n,9,2]] = 0.7; $dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] = 0; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"Emboss":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "Emboss"; $dwStyle[[n,9,2]] = 0; $dwStyle[[n,9,1]] = 10; $dwStyle[[n,8,2,2,1]] = 0; $dwStyle[[n,8,2,1,1]] = 0;$dwStyle[[n,8,2,3,1]] = 1;$dwStyle[[n,8,2,4,1]] = .5; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"EmbossReverse":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "EmbossReverse"; $dwStyle[[n,9,2]] = 1; $dwStyle[[n,9,1]] = 10; $dwStyle[[n,8,2,2,1]] = 0; $dwStyle[[n,8,2,1,1]] = 0;$dwStyle[[n,8,2,3,1]] = 1;$dwStyle[[n,8,2,4,1]] = .5; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"Noise":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "Noise"; $dwStyle[[n,9,1]] = .5; $dwStyle[[n,9,2]] = .5; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"Rim":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "Rim"; $dwStyle[[n,9,1]] = 10; $dwStyle[[n,9,2]] = Pi/4; $dwStyle[[n,8,2,1,1]] = 0.75; $dwStyle[[n,8,2,2,1]] = 0.5; dwUpdateGradients[{n}]; dwUpdateSelected[n]),
						"RimReverse":>(dwSetUndo[]; $dwStyle[[n,8,1]] = "RimReverse"; $dwStyle[[n,9,1]] = 10; $dwStyle[[n,9,2]] = -3Pi/4; $dwStyle[[n,8,2,1,1]] = 0.75; $dwStyle[[n,8,2,2,1]] = 0.5; dwUpdateGradients[{n}]; dwUpdateSelected[n])
					},
					{}
				]
			}],
		ImageSize->{120,$dwStyleButtonHeight}, Alignment->Center, Appearance->"Palette", BaselinePosition->Bottom]
	]

End[] (* End Private Context *)

EndPackage[]