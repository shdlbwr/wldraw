(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeLineWave[]:=
	CreateDialog[
		DynamicModule[{cycles = 2, waves = 1, strength = 1, form = Sin, scale = 1, random = 0, seed = 1, pts, ptsScale, min = 0, gain = 1},
			Pane[
				Dynamic@Column[{
					SeedRandom[seed];
					pts = Join[{{0,0}},Table[{x^gain/Pi, (min*Cos[x Pi/12]) + RandomReal[{1 - random, 1}]*If[waves == 0, 1, form[((12cycles-x)/12waves Pi/cycles)]]*If[waves == 0, form, Sin][x Pi/12]},{x,1,12cycles, 2}],{{((12 cycles)^gain)/Pi, 0}}]/.{DirectedInfinity[]->1};
					ptsScale = {2/(Max[First /@ pts] - Min[First /@ pts]), strength*(1/(Max[Last /@ pts] - Min[Last /@ pts]))};
					Dynamic@Show[Graphics[{
							(* axes and frame *)
							{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
							(* wave *)
							Gray,BSplineCurve[{-scale,0}+#&/@(scale*(ptsScale#&/@pts))]
							},Background->White,ImageSize->{200,200}], PlotRange->1.1
						],
					SetterBar[Dynamic@form,{Sin,Cos,Sinh,Sinc,Power,Sec,Csc}],
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row@{Pane["cycles ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@cycles,{1,50,1}],Spacer[5],Pane[Dynamic@(.5cycles),ImageSize->30,Alignment->Left]},
					Row@{Pane["waves ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@waves,{0,5,.25}],Spacer[5],Pane[Dynamic@waves,ImageSize->30,Alignment->Left]},
					Row@{Pane["height ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@strength,{-2,2,.05}],Spacer[5],Pane[Dynamic@strength,ImageSize->30,Alignment->Left]},
					Row@{Pane["minimum ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@min,{0,10,.01}],Spacer[5],Pane[Dynamic@min,ImageSize->30,Alignment->Left]},
					Row@{Pane["gain ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@gain,{.5,3,.01}],Spacer[5],Pane[Dynamic@gain,ImageSize->30,Alignment->Left]},
					"RAMDOMIZE",
					Row@{Pane["random ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@random,{0,1,.05}],Spacer[5],Pane[Dynamic@random,ImageSize->30,Alignment->Left]},
					Row@{Pane["shape ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@seed,{1,40,1}],Spacer[5],Pane[Dynamic@seed,ImageSize->30,Alignment->Left]},
					"PRESETS",
					Grid[{{
						Button[Graphics[{AbsoluteThickness[.5],BSplineCurve[Join[{{0,0}},Table[{x/Pi,Sin[x Pi/6]},{x,1,12*1,2}],{{12/Pi*1,0}}]]}], form=Sin; cycles=2; waves=0; min=0; gain=1; random=0; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.5],BSplineCurve[Join[{{0,0}},Table[{x/Pi,8*Sin[((12*8-x)/12 Pi/8)]*Sin[x Pi/6]},{x,1,12*8,2}],{{12/Pi*8,0}}]]}], form=Sin; cycles=16; waves=1; min=0; gain=1; random=0; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.5],BSplineCurve[Join[{{0,0}},Table[{x/Pi,8*Cos[((12*8-x)/12 Pi/8)]*Sin[x Pi/6]},{x,1,12*8,2}],{{12/Pi*8,0}}]]}], form=Cos; cycles=16; waves=1; min=0; gain=1; random=0; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.5],BSplineCurve[Join[{{0,0}},Table[{x/Pi,.8*Sinh[((12*8-x)/12 Pi/8)]*Sin[x Pi/6]},{x,1,12*8,2}],{{12/Pi*8,0}}]]}], form=Sinh; cycles=16; waves=1; min=0; gain=1; random=0; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.5],BSplineCurve[Join[{{0,0}},Table[{x/Pi,8*Sinc[((12*8-x)/12 Pi/8)]*Sin[x Pi/6]},{x,1,12*8,2}],{{12/Pi*8,0}}]]}], form=Sinc; cycles=16; waves=1; min=0; gain=1; random=0; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.5],BSplineCurve[Join[{{0,0}},Table[{x/Pi,3*Power[((12*8-x)/12 Pi/8)]*Sin[x Pi/6]},{x,1,12*8,2}],{{12/Pi*8,0}}]]}], form=Power; cycles=16; waves=1; min=0; gain=1; random=0; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.5],BSplineCurve[Join[{{0,0}},Table[{x/Pi,1.5*Sec[((12*8-x)/12 Pi/8)]*Sin[x Pi/6]},{x,1,12*8,2}],{{12/Pi*8,0}}]]}], form=Sec; cycles=16; waves=1; min=0; gain=1; random=0; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.5],BSplineCurve[Join[{{0,0}},Table[{x/Pi,1.5*Csc[((12*8-x)/12 Pi/8)]*Sin[x Pi/6]},{x,1,12*8,2}],{{12/Pi*8,0}}]]}], form=Csc; cycles=16; waves=1; min=0; gain=1; random=0; seed=1, $dwPresetButtonStyle]
					},{
						Button[Graphics[{AbsoluteThickness[.25],BSplineCurve[Join[{{0,0}},Table[{x/Pi,SeedRandom[seed];RandomReal[{0,1}]*20*Sin[x Pi/6]},{x,1,12*20,2}],{{12/Pi*20,0}}]]}, AspectRatio -> 2/3], form=Sin; cycles=50; waves=0; min=0; gain=1; random=1; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.25],BSplineCurve[Join[{{0,0}},Table[{x/Pi,SeedRandom[seed];RandomReal[{0,1}]*20*Sin[((12*20-x)/12 Pi/20)]*Sin[x Pi/6]},{x,1,12*20,2}],{{12/Pi*20,0}}]]}, AspectRatio -> 2/3], form=Sin; cycles=50; waves=1; min=0; gain=1; random=1; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.25],BSplineCurve[Join[{{0,0}},Table[{x/Pi,SeedRandom[seed];RandomReal[{0,1}]*20*Cos[((12*20-x)/12 Pi/20)]*Sin[x Pi/6]},{x,1,12*20,2}],{{12/Pi*20,0}}]]}, AspectRatio -> 2/3], form=Cos; cycles=50; waves=1; min=0; gain=1; random=1; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.25],BSplineCurve[Join[{{0,0}},Table[{x/Pi,SeedRandom[seed];RandomReal[{0,1}]*20*Sinh[((12*20-x)/12 Pi/20)]*Sin[x Pi/6]},{x,1,12*20,2}],{{12/Pi*20,0}}]]}, AspectRatio -> 2/3], form=Sinh; cycles=50; waves=1; min=0; gain=1; random=1; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.25],BSplineCurve[Join[{{0,0}},Table[{x/Pi,SeedRandom[seed];RandomReal[{0,1}]*20*Sinc[((12*20-x)/12 Pi/20)]*Sin[x Pi/6]},{x,1,12*20,2}],{{12/Pi*20,0}}]]}, AspectRatio -> 2/3], form=Sinc; cycles=50; waves=1; min=0; gain=1; random=1; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.25],BSplineCurve[Join[{{0,0}},Table[{x/Pi,SeedRandom[seed];RandomReal[{0,1}]*20*Power[((12*20-x)/12 Pi/20)]*Sin[x Pi/6]},{x,1,12*20,2}],{{12/Pi*20,0}}]]}, AspectRatio -> 2/3], form=Power; cycles=50; waves=1; min=0; gain=1; random=1; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.25],BSplineCurve[Join[{{0,0}},Table[{x/Pi,SeedRandom[seed];RandomReal[{0,1}]*20*Sec[((12*20-x)/12 Pi/20)]*Sin[x Pi/6]},{x,1,12*20,2}],{{12/Pi*20,0}}]]}, AspectRatio -> 2/3], form=Sec; cycles=50; waves=1; min=0; gain=1; random=1; seed=1, $dwPresetButtonStyle],
						Button[Graphics[{AbsoluteThickness[.25],BSplineCurve[Join[{{0,0}},Table[{x/Pi,SeedRandom[seed];RandomReal[{0,1}]*20*Csc[((12*20-x)/12 Pi/20)]*Sin[x Pi/6]},{x,1,12*20,2}],{{12/Pi*20,0}}]]}, AspectRatio -> 2/3], form=Csc; cycles=50; waves=1; min=0; gain=1; random=1; seed=1, $dwPresetButtonStyle]
					}},Spacings->{0,0}],
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add to canvas *)	
						dwNewEmptyLayer["Head"->BSplineCurve];
						$dwStyle[[-1]] = $dwFullDefaultStyle;
						$dwStyle[[-1,1]] = False;
						$dwP[[-1]] = {-scale,0}+#&/@(scale*(ptsScale#&/@pts));
						dwUpdateBoundingBox[{-1}];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						$dwStyleMode = "stroke";
						DialogReturn[]
					]}]
				},Alignment->Center],
			ImageSize->300]
		],
	Background->LightGray, WindowTitle->"Line wave",Modal->True]

End[] (* End Private Context *)

EndPackage[]