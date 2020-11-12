(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *)

(* same as section in dwRenderLayers but CurrentValue[$dwCommandKey] added to remove styles and textures *)
(* need to add filtering to remove default styles since they are not needed *)
	
dwOpenFile[data_]:=
	Block[{dataFinal, noObjects, startLength = Length[$dwP], maskobjnum},
		
		dwSetUndo[];
		
		If[data[[1]] =!= "WLDraw",
			
			MessageDialog["The file is not a WLDraw file."],
			
			noObjects = If[$dwP === {}, True, False];
			dataFinal = data;
			
			
			(* ---------- BEGIN UPDATE OF OLD FILES ---------- *)
			
			
			(* check for old filter names *)
			dataFinal = dataFinal/.{"ColorMultiply"->"Color", "ObjectMultiply"->"Blend", "ObjectMask"->"Mask", 
				"RemoveBackground"->"RemoveColor", "RemoveBackgroundFade"->"RemoveColor", "RemoveBackgroundWhite"->"RemoveColor"};
			
			(* check for old files *)
			If[dataFinal[[4]] =!= {},
				Do[
					If[FreeQ[{Text, Image, "Expression"}, dataFinal[[3,n]]],
						If[Length[dataFinal[[4,n]]] < 15,
							(* add new items in reverse order *)
							dataFinal[[4,n]] = Insert[dataFinal[[4,n]], $dwDefaultSplineDegree, 10];
							dataFinal[[4,n]] = Insert[dataFinal[[4,n]], $dwDefaultSplineClosed, 10]
						]
					],
				{n, Length[dataFinal[[4]]]}]
			];
			
			(* ---------- OLD GRADIENT PARAMETERS ---------- *)
			
			Do[
				If[FreeQ[{Text, Image, "Expression"}, dataFinal[[3,n]]],
					If[Length[dataFinal[[4,n,8]]] == 2,
						dataFinal[[4,n,8]] = Join[dataFinal[[4,n,8]], {0, {0,1}}],
						Nothing
					]
				],
			{n, Length[dataFinal[[4]]]}];
			
			(* check for old texture gradient parameters *)
			Do[
				If[FreeQ[{Text, Image, "Expression"}, dataFinal[[3,n]]],
					If[Length[dataFinal[[4,n,9]]] == 2,
						dataFinal[[4,n,9]] = Join[dataFinal[[4,n,9]], {0,0}],
						Nothing
					]
				],
			{n, Length[dataFinal[[4]]]}];
			
			(* check for missing blend parameters *)
			Do[
				If[FreeQ[{Text, Image, "Expression"}, dataFinal[[3,n]]],
					If[MemberQ[Flatten[{dataFinal[[4,n,8,1]]}], "BlendGradient"] && Length[dataFinal[[4,n,8,1]]] == 3,
						dataFinal[[4,n,8,1]] = Join[dataFinal[[4,n,8,1]], {0}],
						Nothing
					]
				],
			{n, Length[dataFinal[[4]]]}];
			
			(* ---------- MISSING TEXT PARAMETERS ---------- *)
			
			Do[
				If[MemberQ[{Text}, dataFinal[[3,n]]],
					If[Length[dataFinal[[4,n]]] == 11,
						Do[AppendTo[dataFinal[[4,n]], tp], {tp, {None, 1, 0, TextAlignment->Center}}],
						Nothing
					]
				],
			{n, Length[dataFinal[[4]]]}];
			
			Do[
				If[MemberQ[{Text}, dataFinal[[3,n]]],
					If[Length[dataFinal[[4,n]]] == 14,
						Do[AppendTo[dataFinal[[4,n]], tp], {tp, {TextAlignment->Center}}],
						Nothing
					]
				],
			{n, Length[dataFinal[[4]]]}];
			
			(* ---------- MISSING ANIMATION PARAMETERS ---------- *)
			
			(* {pts, style, rotation, scale, {begin, peak, peak end, end}, ramp, arrowSpeed, frameTotal, framePerSecond, frameWidth, removeLastFrame, motion function points (always last)} *)
			If[Length[dataFinal] < 11 || dataFinal[[11]] === {},
				Nothing, (* empty list will be added later *)
				
				If[Quiet[ListQ[dataFinal[[11]]]],
					(* check for frame parameters *)
					If[Length[dataFinal[[11,1]]] == 8,
						Do[
							If[Length[dataFinal[[11,n]]] == 8,
								dataFinal[[11,n]] = Join[dataFinal[[11,n]][[;;-2]], {30,30,240,False}, {dataFinal[[11,n,-1]]}]
							],
						{n, Length[dataFinal[[11]]]}]
					];
					(* check scale variable for horizontal and vertical scale *)
					Do[
						If[Length[dataFinal[[11,n,4]]] == 0,
							dataFinal[[11,n,4]] = {dataFinal[[11,n,4]], dataFinal[[11,n,4]]}
						],
					{n, Length[dataFinal[[11]]]}];
					(* check for non-list input for animation peak parameter *)
					Do[
						If[Length[dataFinal[[11,n,5]]] != 4,
							Switch[dataFinal[[11,n,5]],
								0,
									dataFinal[[11,n,5]] = {1, 1, 1, dataFinal[[11,n,8]]},
								1,
									dataFinal[[11,n,5]] = {1, dataFinal[[11,n,8]], dataFinal[[11,n,8]], dataFinal[[11,n,8]]},
								_,
									dataFinal[[11,n,5]] = {1, IntegerPart[dataFinal[[11,n,5]]*dataFinal[[11,n,8]]], IntegerPart[dataFinal[[11,n,5]]*dataFinal[[11,n,8]]], dataFinal[[11,n,8]]}
							]
						],
					{n, Length[dataFinal[[11]]]}];
					(* check for missing removeLastFrame parameter *)
					If[Length[dataFinal[[11,1]]] == 11,
						Do[
							If[Length[dataFinal[[11,n]]] == 11,
								dataFinal[[11,n]] = Join[dataFinal[[11,n]][[;;-2]], {False}, {dataFinal[[11,n,-1]]}]
							],
						{n, Length[dataFinal[[11]]]}]
					];
					(* check for missing origin parameter *)
					If[Length[dataFinal[[11,1]]] == 12,
						Do[
							If[Length[dataFinal[[11,n]]] == 12,
								dataFinal[[11,n]] = Join[dataFinal[[11,n]][[;;-2]], {dataFinal[[11,n,-1,1]]}, {dataFinal[[11,n,-1]]}]
							],
						{n, Length[dataFinal[[11]]]}]
					],
					
					dataFinal[[11]] = {}
				]
			];
			
			(* ---------- MISSING IMAGE PARAMETERS ---------- *)
			
			Do[
				If[MemberQ[{Image}, dataFinal[[3,n]]],
					If[Length[dataFinal[[4,n]]] == 4,
						AppendTo[dataFinal[[4,n]], False],
						Nothing
					]
				],
			{n, Length[dataFinal[[4]]]}];
			
			
			(* ---------- END UPDATE OF OLD FILES ---------- *)
			
				
			(* insert data *)
			If[Head@dataFinal===List,
				$dwSelected = {};
				$dwMode = $dwCurrentPreviewMode = "wireframe";
				If[$dwP === {},
					
					(* no existing objects - for speed increase only *)
					If[$dwTemplate[[1]]===Null, Nothing, dataFinal[[6]] = $dwTemplate];(* keep existing template if not empty *)
					Switch[Length[dataFinal],
						9,
							{$dwP, $dwHead, $dwStyle, $dwPlotRange, $dwTemplate, $dwGroupLayers, $dwHideLayers, $dwCompoundPathLayers} = dataFinal[[2;;-1]];
							$dwLineGradients = Table[$dwDefaultGradient, {n, Length[$dwP]}];
							$dwAnimate = {},
						10,
							{$dwP, $dwHead, $dwStyle, $dwPlotRange, $dwTemplate, $dwGroupLayers, $dwHideLayers, $dwCompoundPathLayers, $dwLineGradients} = dataFinal[[2;;-1]];
							$dwAnimate = {},
						11,
							{$dwP, $dwHead, $dwStyle, $dwPlotRange, $dwTemplate, $dwGroupLayers, $dwHideLayers, $dwCompoundPathLayers, $dwLineGradients, $dwAnimate} = dataFinal[[2;;-1]],
						_,
							{$dwP, $dwHead, $dwStyle, $dwPlotRange, $dwTemplate, $dwGroupLayers, $dwHideLayers, $dwCompoundPathLayers, $dwLineGradients, $dwAnimate, {$dwTilt, $dwTurn}} = dataFinal[[2;;-1]];
							If[$dwConstrainHAngle != 0,
								dwUpdateAxonometricAngles[$dwTilt,$dwTurn];
								dwSetGridSize[]
							]
					];
					Do[
						If[MemberQ[$dwShapeSymbols, $dwHead[[n]]] && Length[$dwStyle[[n]]] < 18,
							$dwStyle[[n]] = Insert[$dwStyle[[n]], $dwArrowheadQuantity, 12],
							Nothing
						], 
					{n, Length[$dwStyle]}];
					If[Length[$dwLineGradients] < Length[$dwP],
						$dwLineGradients = Join[$dwLineGradients, Table[$dwDefaultGradient, {Length[$dwP] - Length[$dwLineGradients]}]]
					];
					$dwObjectGradients = Table[{}, {n, Length[$dwP]}];
					$dwBoundingBoxes = Table[{{0,0},{0,0},{0,0},{0,0}}, {n, Length[$dwP]}];
					dwUpdateGradients[Range[Length[$dwP]]];
					dwUpdateBoundingBox[Range[Length[$dwP]]];
					dwDeleteEmptyLayers[];
					$dwObjectQuantity = Length[$dwP];
					$dwPointQuantity = Length[Flatten[$dwP, 1]],
					
					(* add to existing objects *)
					$dwP=Join[$dwP,dataFinal[[2]]];
					$dwHead=Join[$dwHead,dataFinal[[3]]];
					$dwStyle=Join[$dwStyle,dataFinal[[4]]];
					$dwPlotRange=If[noObjects, dataFinal[[5]], $dwPlotRange];
					$dwTemplate=If[$dwTemplate[[1]]===Null,dataFinal[[6]],$dwTemplate];(* keep existing template if not empty *)
					$dwGroupLayers=Join[$dwGroupLayers,(#+Length[$dwP]-Length[dataFinal[[2]]])&/@dataFinal[[7]]];
					$dwHideLayers=Join[$dwHideLayers,(#+Length[$dwP]-Length[dataFinal[[2]]])&/@dataFinal[[8]]];
					$dwCompoundPathLayers=Join[$dwCompoundPathLayers,(#+Length[$dwP]-Length[dataFinal[[2]]])&/@dataFinal[[9]]];
					$dwObjectGradients=Join[$dwObjectGradients,Table[{},Length[dataFinal[[2]]]]];
					dwUpdateGradients[Range[Length[$dwP]-Length[dataFinal[[2]]]+1,Length[$dwP]]];
					Switch[Length[dataFinal],
						9,
							$dwLineGradients = Table[$dwDefaultGradient, {n, Length[$dwP]}];
							$dwAnimate = {},
						10,
							$dwAnimate = {},
						_,
							$dwLineGradients = Join[$dwLineGradients, dataFinal[[10]]];
							$dwAnimate = Join[$dwAnimate, dataFinal[[11]]];
							If[Length[dataFinal] > 11,
								{$dwTilt, $dwTurn} = dataFinal[[12]];
								If[$dwConstrainHAngle != 0,
									dwUpdateAxonometricAngles[$dwTilt,$dwTurn];
									dwSetGridSize[]
								]
							]
					];
					Do[
						If[MemberQ[$dwShapeSymbols, $dwHead[[n]]] && Length[$dwStyle[[n]]] < 18,
							$dwStyle[[n]] = Insert[$dwStyle[[n]], $dwArrowheadQuantity, 12],
							Nothing
						], 
					{n, Length[$dwStyle]}];
					If[Length[$dwLineGradients] != Length[$dwP],
						$dwLineGradients = Join[$dwLineGradients, Table[$dwDefaultGradient, {Length[$dwP] - Length[$dwLineGradients]}]]
					];
					(* update image filters and gradients relying on other object *)
					Do[
						If[pos[[1]] > startLength, (* not original mask *)
							maskobjnum = $dwStyle[[Sequence@@ReplacePart[pos,-1->2]]];
								(* new mask object number *)
								$dwStyle = ReplacePart[$dwStyle, ReplacePart[pos,-1->2]->startLength + $dwStyle[[Sequence@@ReplacePart[pos,-1->2]]]],
							Nothing
						], 
						{pos, Flatten[Position[$dwStyle,#,Infinity]&/@{"Mask", "BlendGradient"}, 1]}
					];
					(*$dwSelected = Range[Length@$dwP-Length@dataFinal[[2]]+1,Length@$dwP];*)
					$dwBoundingBoxes = Join[$dwBoundingBoxes, Table[{{0,0},{0,0},{0,0},{0,0}}, {n, Length[$dwP]}]];
					dwUpdateBoundingBox[Range[Length[$dwP]]];
					dwDeleteEmptyLayers[];
					$dwObjectQuantity = Length[$dwP];
					$dwPointQuantity = Length[Flatten[$dwP, 1]]
				]
			]
		]
	]
	
dwImportTemplate[]:=
	CreateDialog[
		Pane[Column[{
				"Paste graphics or image",InputField[Dynamic@$dwTemplate[[1]],ImageSize->{290,150}],
				Row@{Pane["horz ",30,Alignment->Right],Slider[Dynamic@$dwTemplate[[3,1]],{-10,10}],
				Button["<",$dwTemplate[[3,1]]=If[CurrentValue[$dwCommandKey],0,Round[$dwTemplate[[3,1]]-$dwGridStepNone,$dwGridStepNone]];If[$dwTemplate[[3,1]]<-10,$dwTemplate[[3,1]]=-10],ImageSize->12,Appearance->"Palette"],
				Button[">",$dwTemplate[[3,1]]=If[CurrentValue[$dwCommandKey],0,Round[$dwTemplate[[3,1]]+$dwGridStepNone,$dwGridStepNone]];If[$dwTemplate[[3,1]]>10,$dwTemplate[[3,1]]=10],ImageSize->12,Appearance->"Palette"],
				" ",Pane[Dynamic@Round[$dwTemplate[[3,1]],.001],40,Alignment->Left]},
				Row@{Pane["vert ",30,Alignment->Right],Slider[Dynamic@$dwTemplate[[3,2]],{-10,10}],
				Button["<",$dwTemplate[[3,2]]=If[CurrentValue[$dwCommandKey],0,Round[$dwTemplate[[3,2]]-$dwGridStepNone,$dwGridStepNone]];If[$dwTemplate[[3,2]]<-10,$dwTemplate[[3,2]]=-10],ImageSize->12,Appearance->"Palette"],
				Button[">",$dwTemplate[[3,2]]=If[CurrentValue[$dwCommandKey],0,Round[$dwTemplate[[3,2]]+$dwGridStepNone,$dwGridStepNone]];If[$dwTemplate[[3,2]]>10,$dwTemplate[[3,2]]=10],ImageSize->12,Appearance->"Palette"],
				" ",Pane[Dynamic@Round[$dwTemplate[[3,2]],.001],40,Alignment->Left]},
				Row@{Pane["size ",30,Alignment->Right],Slider[Dynamic@$dwTemplate[[2]],{.1,2}],
				Button["<",$dwTemplate[[2]]=If[CurrentValue[$dwCommandKey],1,Round[$dwTemplate[[2]]-$dwGridStepNone,$dwGridStepNone]];If[$dwTemplate[[2]]<.1,$dwTemplate[[2]]=.1],ImageSize->12,Appearance->"Palette"],
				Button[">",$dwTemplate[[2]]=If[CurrentValue[$dwCommandKey],1,Round[$dwTemplate[[2]]+$dwGridStepNone,$dwGridStepNone]];If[$dwTemplate[[2]]>20,$dwTemplate[[2]]=20],ImageSize->12,Appearance->"Palette"],
				" ",Pane[Dynamic@Round[$dwTemplate[[2]],.001],40,Alignment->Left]},
				Row@{Pane["rot ",30,Alignment->Right],Slider[Dynamic@$dwTemplate[[4]],{-180,180,1}],
				Button["<",$dwTemplate[[4]]=If[CurrentValue[$dwCommandKey],0,$dwTemplate[[4]]-1];If[$dwTemplate[[4]]<-180,$dwTemplate[[4]]=-180],ImageSize->12,Appearance->"Palette"],
				Button[">",$dwTemplate[[4]]=If[CurrentValue[$dwCommandKey],0,$dwTemplate[[4]]+1];If[$dwTemplate[[4]]>180,$dwTemplate[[4]]=180],ImageSize->12,Appearance->"Palette"],
				" ",Pane[Dynamic@$dwTemplate[[4]],40,Alignment->Left]},
				Row@{Pane["fade ",30,Alignment->Right],Slider[Dynamic@$dwTemplate[[5]],{0,1,.1}],
				Button["<",$dwTemplate[[5]]=If[CurrentValue[$dwCommandKey],0,$dwTemplate[[5]]-.1];If[$dwTemplate[[5]]<0,$dwTemplate[[5]]=0],ImageSize->12,Appearance->"Palette"],
				Button[">",$dwTemplate[[5]]=If[CurrentValue[$dwCommandKey],0,$dwTemplate[[5]]+.1];If[$dwTemplate[[5]]>1,$dwTemplate[[5]]=1],ImageSize->12,Appearance->"Palette"],
				" ",Pane[Dynamic@$dwTemplate[[5]],40,Alignment->Left]},
				Row@{Button[Style["USE CAMERA IMAGE", 8], $dwTemplate[[1]] = ImageResize[CurrentImage[], {{$dwImageCameraMaxSize},{$dwImageCameraMaxSize}}],Appearance->"Palette", ImageSize->{100, 26}], 
					Spacer[30],Checkbox[Dynamic@$dwTemplateRender],"show template"},
				Row@{DefaultButton["Done",DialogReturn[],ImageSize->80]}},
			Alignment->Center],
		ImageSize->300],
	Background->LightGray, WindowTitle->"Template", Modal->True]

End[] (* End Private Context *)

EndPackage[]