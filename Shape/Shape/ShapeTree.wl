(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeTreeDialog[]:=
	CreateDialog[
		DynamicModule[{
			lines = {}, complexity = 2, random = 1, angle = 22.5, trunk = .25, thick = 3, minThick = 1, seed = 6, reduce = .25, order = "F[+F-F-F][-F-F-F][+F+F+F][-F+F+F]",
			leafCount = 6, leafCounter, leafQuantity, leafSpace = .03, leafSize = 3, branchcolor = Brown, leafcolor = Hue[.25,.7,.7], scale = 1.25, length = Length[$dwP], adjSize = 1.6, counter, origin},
			Pane[
				Column[{
					Dynamic[
						If[StringLength[order] > 24 && complexity > 2, complexity = 2, Nothing];
						lines = .5*scale*dwShapeTree["Complexity"->complexity, "Angle"->angle, "Seed"->seed, "Order"->order, "LengthReduction"->reduce, "Random"->random];
						lines = Join[{Join[{lines[[1, 1]] + {0, (1 - trunk)*EuclideanDistance[Sequence@@lines[[1]]]}}, lines[[1]][[2 ;; -1]]]}, lines[[2 ;; -1]]];
						lines = Partition[lines, complexity];
						Graphics[{
							
							(* branches *)
							branchcolor, CapForm["Round"],
							{EdgeForm[{branchcolor, JoinForm["Round"], AbsoluteThickness[adjSize*scale*.75thick]}],
								Polygon[{{scale*(-.0025*thick), lines[[1, 1, 1, 2]]}, {scale*(.0025*thick), lines[[1, 1, 1, 2]]}, {0, lines[[1, 1, 2, 2]]}}]},
							Table[
								Table[
									{AbsoluteThickness[adjSize*scale*Min[.8(minThick + (thick-minThick)(((2Length[lines])/((n-1) + n1))/(Length[lines]))), thick]], Line[lines[[n, n1]]]},
								{n1, Length[lines[[n]]]}], 
							{n, Length[lines]}],
							
							(* leaves *)
							leafCounter = 0;
							If[leafCount != 0,
								{leafcolor, AbsolutePointSize[adjSize*scale*leafSize], 
								counter = 1;
								Sequence@@Table[
									SeedRandom[seed + counter++];
									leafQuantity = RandomInteger[{Max[IntegerPart[.5(leafCount/complexity)], 1], Max[IntegerPart[(leafCount/complexity)], 1]}];
									leafCounter += leafQuantity;
									Point[RandomPoint[Disk[p[[-1]], scale*leafSpace], leafQuantity]], 
								{p, Flatten[lines[[2;;-1]], 1]}]},
								{}
							]
							
						},Background->White, Frame->{{False,False},{True,False}}, ImageSize->{330,330}, PlotRange->{{-.5,.5}, dwFindCenter[Flatten[lines, 2]][[2]]+{-.5,.5}}],
						TrackedSymbols :> {complexity, order, seed, random, scale, angle, trunk, thick, minThick, reduce, leafCount, leafSpace, leafSize, branchcolor, leafcolor}
					],
					Dynamic@Row[{"object total: ", Length[Flatten[lines,1]] + If[leafCount == 0, 1, 2], "     point total estimate: ", Length[Flatten[lines,2]] + 3 + leafCounter(*IntegerPart[1.4(leafCount/complexity)*(Length[Flatten[lines,1]] - 1)]*)}],
					Row[{
						Button[$dwShapeTree01, complexity = 2; order = "F[+F-F-F][-F-F-F][+F+F+F][-F+F+F]"; seed = 6; random = 1; scale = 1.25; angle = 22.5; reduce = .25; thick = 3; minThick = 1; 
							trunk = .25; leafCount = 6; leafSpace = .03; leafSize = 3, Appearance->"Palette", ImageSize->{1.2$dwShapeMenuIconSize, $dwShapeMenuIconSize}],
						Button[$dwShapeTree02, complexity = 2; order = "F[-F-F-F][+F-F+F][-F+F-F][+F+F+F]"; seed = 6; random = 1; scale = 1.25; angle = 22.5; reduce = .25; thick = 3; minThick = 1; 
							trunk = .25; leafCount = 6; leafSpace = .03; leafSize = 3, Appearance->"Palette", ImageSize->{1.2$dwShapeMenuIconSize, $dwShapeMenuIconSize}],
						Button[$dwShapeTree03, complexity = 3; order = "F[+F+F-F][-F+F-F]"; seed = 6; random = 1; scale = .75; angle = 30; reduce = .4; thick = 3; minThick = 1; 
							trunk = .25; leafCount = 6; leafSpace = .03; leafSize = 4, Appearance->"Palette", ImageSize->{1.2$dwShapeMenuIconSize, $dwShapeMenuIconSize}],
						Button[$dwShapeTree04, complexity = 3; order = "F[+F+F-F][-F-+F-F]"; seed = 6; random = 1; scale = .75; angle = 22.5; reduce = .4; thick = 3; minThick = 1; 
							trunk = .25; leafCount = 6; leafSpace = .03; leafSize = 4, Appearance->"Palette", ImageSize->{1.2$dwShapeMenuIconSize, $dwShapeMenuIconSize}],
						Button[$dwShapeTree05, complexity = 3; order = "F[+F-+F-F][-F+-F+-F]"; seed = 6; random = 1; scale = .75; angle = 30; reduce = .4; thick = 3; minThick = 1; 
							trunk = .25; leafCount = 6; leafSpace = .03; leafSize = 4, Appearance->"Palette", ImageSize->{1.2$dwShapeMenuIconSize, $dwShapeMenuIconSize}],
						Button[$dwShapeTree06, complexity = 3; order = "F[+F-+F-+F][-F+-F+-F]"; seed = 1; random = 1; scale = .75; angle = 30; reduce = .4; thick = 3; minThick = 1; 
							trunk = .25; leafCount = 6; leafSpace = .03; leafSize = 4, Appearance->"Palette", ImageSize->{1.2$dwShapeMenuIconSize, $dwShapeMenuIconSize}]
					}],
					Row@{"complexity ", Dynamic@SetterBar[Dynamic@complexity, Range[If[StringLength[order] <= 24, 3, 2]]], Spacer[30], ColorSetter[Dynamic@branchcolor], ColorSetter[Dynamic@leafcolor]},
					"",
					"TREE",
					Row@{Pane["seed ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@seed,{1,50,1},ContinuousAction->False],
						Button["<", seed = If[CurrentValue[$dwCommandKey], 1, Max[seed-1,1]], Appearance->"Palette"],Button[">", seed = If[CurrentValue[$dwCommandKey], 1, Min[seed+1, 50]], Appearance->"Palette"],
						Spacer[5], Pane[Dynamic@seed,ImageSize->30,Alignment->Left]},
					Row@{Pane["random ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@random,{0,2,.05},ContinuousAction->False],
						Button["<", random = If[CurrentValue[$dwCommandKey], 1, Max[random-.05,0]], Appearance->"Palette"],Button[">", random = If[CurrentValue[$dwCommandKey], 1, Min[random+.05, 2]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@random,ImageSize->30,Alignment->Left]},
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.5,1.5,.05},ContinuousAction->False],
						Button["<", scale = If[CurrentValue[$dwCommandKey], 1, Max[scale-.05,.5]], Appearance->"Palette"],Button[">", scale = If[CurrentValue[$dwCommandKey], 1, Min[scale+.05, 1.5]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					"BRANCHES",
					Row@{Pane["angle ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@angle,{5,45,.5},ContinuousAction->False],
						Button["<", angle = If[CurrentValue[$dwCommandKey], 22.5, Max[angle-.5,5]], Appearance->"Palette"],Button[">", angle = If[CurrentValue[$dwCommandKey], 22.5, Min[angle+.5, 45]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@angle,ImageSize->30,Alignment->Left]},
					Row@{Pane["length ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@reduce,{.1,.5,.01},ContinuousAction->False],
						Button["<", reduce = If[CurrentValue[$dwCommandKey], .25, Max[reduce-.01,.1]], Appearance->"Palette"],Button[">", reduce = If[CurrentValue[$dwCommandKey], .25, Min[reduce+.01, .5]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@reduce,ImageSize->30,Alignment->Left]},
					Row@{Pane["thick ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@thick,{1,5,.25},ContinuousAction->False],
						Button["<", thick = If[CurrentValue[$dwCommandKey], 3, Max[thick-.25,1]], Appearance->"Palette"],Button[">", thick = If[CurrentValue[$dwCommandKey], 3, Min[thick+.25, 5]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@thick,ImageSize->30,Alignment->Left]},
					Row@{Pane["minThick ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@minThick,{.25,2,.25},ContinuousAction->False],
						Button["<", minThick = If[CurrentValue[$dwCommandKey], 1, Max[minThick-.25,.25]], Appearance->"Palette"],Button[">", minThick = If[CurrentValue[$dwCommandKey], 1, Min[minThick+.25, 2]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@minThick,ImageSize->30,Alignment->Left]},
					Row@{Pane["trunk ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@trunk,{0,1,.01},ContinuousAction->False],
						Button["<", trunk = If[CurrentValue[$dwCommandKey], .25, Max[trunk-.01,0]], Appearance->"Palette"],Button[">", trunk = If[CurrentValue[$dwCommandKey], .25, Min[trunk+.01, 1]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@trunk,ImageSize->30,Alignment->Left]},
					"LEAVES",
					Row@{Pane["quantity ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@leafCount,{0,50,1},ContinuousAction->False],
						Button["<", leafCount = If[CurrentValue[$dwCommandKey], 6, Max[leafCount-1,0]], Appearance->"Palette"],Button[">", leafCount = If[CurrentValue[$dwCommandKey], 6, Min[leafCount+1, 50]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@leafCount,ImageSize->30,Alignment->Left]},
					Row@{Pane["space ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@leafSpace,{.01,.15,.01},ContinuousAction->False],
						Button["<", leafSpace = If[CurrentValue[$dwCommandKey], .03, Max[leafSpace-.01,.01]], Appearance->"Palette"],Button[">", leafSpace = If[CurrentValue[$dwCommandKey], .03, Min[leafSpace+.01, .15]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@leafSpace,ImageSize->30,Alignment->Left]},
					Row@{Pane["size ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@leafSize,{1,6,.25},ContinuousAction->False],
						Button["<", leafSize = If[CurrentValue[$dwCommandKey], 3, Max[leafSize-.25,1]], Appearance->"Palette"],Button[">", leafSize = If[CurrentValue[$dwCommandKey], 3, Min[leafSize+.25, 6]], Appearance->"Palette"],
						Spacer[5],Pane[Dynamic@leafSize,ImageSize->30,Alignment->Left]},
					Row[{
						Button["reset",complexity = 2; random = 1; angle = 22.5; trunk = .25; thick = 3; minThick = 1; seed = 6; reduce = .25; order = "F[+F-F-F][-F-F-F][+F+F+F][-F+F+F]";
							leafCount = 6; leafSpace = .03; leafSize = 3; branchcolor = Brown; leafcolor = Hue[.25,.7,.7]; scale = 1.25],
						CancelButton[DialogReturn[]],
						DefaultButton[
							origin = {0, -2dwFindCenter[Flatten[lines, 2]][[2]]};
							lines = .5*scale*dwShapeTree["Complexity"->complexity, "Angle"->angle, "Seed"->seed, "Order"->order, "LengthReduction"->reduce, "Random"->random, "Origin"->origin];
							lines = Join[{Join[{lines[[1, 1]] + {0, (1 - trunk)*EuclideanDistance[Sequence@@lines[[1]]]}}, lines[[1]][[2 ;; -1]]]}, lines[[2 ;; -1]]];
							lines = Partition[lines, complexity];
							(* roots *)
							dwNewEmptyLayer["Head"->Polygon];
							$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
							$dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[$dwSelected[[1]]]], FaceForm[_]]][[1]],1,1]] = branchcolor;
							$dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]],1,1]] = branchcolor;
							$dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]],1,3,1]] = scale*.75thick;
							$dwP[[$dwSelected[[1]]]] = {{scale*(-.0025*thick), lines[[1, 1, 1, 2]]}, {scale*(.0025*thick), lines[[1, 1, 1, 2]]}, {0, lines[[1, 1, 2, 2]]}};
							dwUpdateBoundingBox[$dwSelected[[{1}]]];
							(* add branches to canvas *)	
							Do[
								Do[
									dwNewEmptyLayer["Head"->Line];
									$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
									$dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]],1,1]] = branchcolor;
									$dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]],1,3,1]] = scale*Min[.8(minThick + (thick-minThick)(((2Length[lines])/((n-1) + n1))/(Length[lines]))), thick];
									$dwP[[$dwSelected[[1]]]] = lines[[n, n1]];
									dwUpdateBoundingBox[$dwSelected[[{1}]]], 
								{n1, Length[lines[[n]]]}], 
							{n, Length[lines]}];
							(* add leaves to canvas *)	
							If[leafCount != 0,
								dwNewEmptyLayer["Head"->Point];
								$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
								$dwStyle[[$dwSelected[[1]],13]] = leafcolor;
								$dwStyle[[$dwSelected[[1]],Flatten[Position[$dwStyle[[$dwSelected[[1]]]], AbsolutePointSize[_]]][[1]],1]] = scale*leafSize;
								counter = 1;
								$dwP[[$dwSelected[[1]]]] = Flatten[Table[
										SeedRandom[seed + counter++];
										RandomPoint[Disk[p[[-1]], scale*leafSpace], RandomInteger[{Max[IntegerPart[.5(leafCount/complexity)], 1], Max[IntegerPart[(leafCount/complexity)], 1]}]], 
									{p, Flatten[lines[[2 ;; -1]], 1]}], 1],
								Nothing
							];
							dwUpdateBoundingBox[$dwSelected[[{1}]]];
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							$dwGroupLayers = Join[$dwGroupLayers, {Range[length+1, Length[$dwP]]}];
							$dwSelected = $dwGroupLayers[[-1]];
							DialogReturn[]
						]
					}]
				},Alignment->Center],
			ImageSize->330]
		],
	Background->LightGray, WindowTitle->"Organic Tree",Modal->True]

(* adapted from code written by Alec Shedelbower *)

dwTurtleRotate[vec_, r_] := N@RotationTransform[r][vec]

Options[dwTurtleDraw] = {"Origin"->{0,0}, "Random"->0};

dwTurtleDraw[string_?StringQ, angle_ : 22.5, constants_ : {}, initDir_ : {0, 1}, seed_:1, reduce_:.8, OptionsPattern[]] :=
	Block[{chars = ToString/@Characters[string], pos = OptionValue["Origin"], random = OptionValue["Random"], dir = Normalize[initDir], 
		stack = {}, lines = {}, dist = 1, next, n = 1, length = 1, rdm},
		rdm = If[random == 0, {1, 1}, {1 - .5random, 1 + .5random}];
		Do[
			SeedRandom[seed+n++];
			Which[
				StringMatchQ[c, "+"],
					dir = dwTurtleRotate[dir, (angle/180*Pi)RandomReal[rdm]],
				StringMatchQ[c, "-"],
					dir = dwTurtleRotate[dir, -(angle/180*Pi)RandomReal[rdm]],
				StringMatchQ[c, "["],
					length *= reduce;
					AppendTo[stack, {pos, dir}],
				StringMatchQ[c, "]"], 
					length *= 1/reduce;
					{pos, dir} = Last[stack];
					stack = Delete[stack, Length[stack]],
				ContainsAny[{"F"}, {c}], 
					next = pos + length*dir*dist*RandomReal[rdm];
					AppendTo[lines, {pos, next}];
					pos = next,
				ContainsNone[constants, {c}],
					Print["Error|Unknown Variable: ", c],
				True,
					Nothing
			], 
		{c, chars}];
		lines
	]

Options[dwShapeTree] = {"Complexity"->4, "Angle"->Pi/8, "Seed"->1, "Order"->"FF-[-F+F+F]+[+F-F-F]", "LengthReduction"->.8, "Origin"->{0,0}, "Random"->0};

dwShapeTree[OptionsPattern[]] :=
	Block[{complexity, angle, seed, order, reduce, origin, random, string},
		{complexity, angle, seed, order, reduce, origin, random} = OptionValue[#]&/@{"Complexity", "Angle", "Seed", "Order", "LengthReduction", "Origin", "Random"};
		string = StringJoin/@SubstitutionSystem[{"F" -> Characters[order] }, {"F"}, complexity];
		
		dwTurtleDraw[Last@string, angle, {}, {0,1}, seed, reduce, "Origin"->origin, "Random"->random]
	]

End[] (* End Private Context *)

EndPackage[]