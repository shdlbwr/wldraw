(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwConvertPointSelectionToLayerSelection] = {"ChangeMode"->True};
dwConvertPointSelectionToLayerSelection[OptionsPattern[]]:=
	If[$dwSelected =!= {},
		If[Length[$dwSelected[[1]]] > 1,
			$dwSelected = DeleteDuplicates[#[[1]]&/@$dwSelected]
		];
		If[OptionValue["ChangeMode"] === True, $dwMode = $dwCurrentPreviewMode];,
		If[MemberQ[{"point"}, $dwMode],
			$dwSelected = $dwPointModeSelections
		];
		If[OptionValue["ChangeMode"] === True, $dwMode = $dwCurrentPreviewMode];
	]

dwConvertLayerSelectionToPointSelection[]:=
	If[$dwSelected =!= {} && FreeQ[{"point"}, $dwMode],
		(* remove image and text selections *)
		$dwSelected = Table[If[MemberQ[$dwShapeSymbols, $dwHead[[n]]], n, Nothing], {n, $dwSelected}];
		(* convert object to point selection *)
		If[$dwSelected =!= {} && $dwMode =!= "point",
			$dwPointModeSelections = Union[Flatten[
				If[MemberQ[Flatten[$dwCompoundPathLayers], #],
					$dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, #]][[1]]]],
					#
				]&/@If[Length[$dwSelected[[1]]] > 1, #[[1]]&/@$dwSelected, $dwSelected]]];
			$dwSelected = {#, 0}&/@$dwPointModeSelections;
			$dwMode = "point",
			$dwMode = "point"
		]
	]

Options[dwNewEmptyLayer] = {"Form"->"shape", "Head"->Automatic, "SetUndo"->True};
dwNewEmptyLayer[OptionsPattern[]] :=
	DynamicModule[{form, head, undo},
		{form, head, undo} = {OptionValue["Form"], OptionValue["Head"], OptionValue["SetUndo"]};
		If[undo, dwSetUndo[], Nothing];
		(*If[$dwP =!= {} && Flatten[Position[$dwP,{}]] =!= {}, dwDeleteEmptyLayers[]];*)(* reminder that this will not work because selection changes *)
		
		(* convert selection *)
		$dwSelected = {
			Which[
				$dwSelected === {},
					1,
				Length[$dwSelected[[1]]] > 1,
					$dwSelected[[1,1]],
				True,
					$dwSelected[[1]]
			]};
		
		(* add empty layer *)
		If[$dwP =!= {},
			(* catch scenarios to use previous style *)
			Which[
				form === "expression",
				
					AppendTo[$dwP, {{0,0}}];
					AppendTo[$dwHead, "Expression"];
					AppendTo[$dwStyle, $dwDefaultExpressionStyle],
					
				form === "text" && $dwHead[[$dwSelected[[1]]]] === Text,
				
					AppendTo[$dwP, {{0,0}}];
					AppendTo[$dwHead, Text];
					AppendTo[$dwStyle, $dwStyle[[$dwSelected[[1]]]]],
					
				form === "text" && $dwHead[[$dwSelected[[1]]]] =!= Text,
				
					AppendTo[$dwP, {{0,0}}];
					AppendTo[$dwHead, Text];
					AppendTo[$dwStyle, $dwDefaultTextStyle],
					
				FreeQ[{"text","image"}, form] && $dwHead[[$dwSelected[[1]]]] === Text,
				
					AppendTo[$dwP, {}];
					AppendTo[$dwHead, If[head === Automatic, $dwDefaultHead, head]];
					AppendTo[$dwStyle, $dwFullDefaultStyle],
					
				form === "image" && $dwHead[[$dwSelected[[1]]]] === Image,
				
					AppendTo[$dwP, {{0,0}}];
					AppendTo[$dwHead, Image];
					AppendTo[$dwStyle, $dwDefaultImageStyle],
					
				form === "image" && $dwHead[[$dwSelected[[1]]]] =!= Image,
					AppendTo[$dwP, {{0,0}}];
					AppendTo[$dwHead, Image];
					AppendTo[$dwStyle, $dwDefaultImageStyle],
					
				FreeQ[{"text","image"}, form] && $dwHead[[$dwSelected[[1]]]] === Image,
				
					AppendTo[$dwP, {}];
					AppendTo[$dwHead, If[head === Automatic, $dwDefaultHead, head]];
					AppendTo[$dwStyle, $dwFullDefaultStyle],
					
					
				FreeQ[{"text","image","expression"}, form] && $dwHead[[$dwSelected[[1]]]] === "Expression",
				
					AppendTo[$dwP, {}];
					AppendTo[$dwHead, If[head === Automatic, $dwDefaultHead, head]];
					AppendTo[$dwStyle, $dwFullDefaultStyle],
					
					
				FreeQ[{"text","image","expression"}, form] && $dwHead[[$dwSelected[[1]]]] === "Text3D",
				
					AppendTo[$dwP, {}];
					AppendTo[$dwHead, If[head === Automatic, $dwDefaultHead, head]];
					AppendTo[$dwStyle, $dwFullDefaultStyle],
				
				True,
					AppendTo[$dwP, If[MemberQ[{Image, Text, "Expression", "Text3D"}, head], {{0,0}}, {}]];
					AppendTo[$dwHead, If[head === Automatic, $dwHead[[$dwSelected[[1]]]], head]];
					AppendTo[$dwStyle, Switch[head, Image, $dwDefaultImageStyle, Text, $dwDefaultTextStyle, "Expression", $dwDefaultExpressionStyle, _, $dwStyle[[$dwSelected[[1]]]]]]
			];
			$dwSelected = {Length@$dwP};
			$dwBoundingBoxes = 
				Switch[form,
					"text", Join[$dwBoundingBoxes, {dwTextBox[$dwSelected[[1]]]}],
					"image", Join[$dwBoundingBoxes, {dwImageBox[$dwSelected[[1]]]}],
					"expression", Join[$dwBoundingBoxes, {dwExpressionBox[$dwSelected[[1]]]}],
					_, Join[$dwBoundingBoxes, {{}}]
				];
			dwUpdateBoundingBox[{$dwSelected[[1]]}];
			$dwObjectGradients = Join[$dwObjectGradients, {{}}];
			$dwLineGradients = Join[$dwLineGradients, {$dwDefaultGradient}];
			$dwAnimate = Join[$dwAnimate, {{$dwP[[$dwSelected[[1]]]], $dwStyle[[-1]], 0, {1,1}, {1,30,30,30}, 1, 1, 30, 30, 240, False, {0,0}, {{0,0}}}}];
			$dwObjectQuantity = Length[$dwP];
			$dwPointQuantity = Length[Flatten[$dwP, 1]],
			
			(* first layer *)
			Switch[form,
				"text",
					$dwHead = {Text};
					$dwStyle = {$dwDefaultTextStyle};
					$dwP = {{{0,0}}},
				"image",
					$dwHead = {Image};
					$dwStyle = {$dwDefaultImageStyle};
					$dwP = {{{0,0}}},
				"expression",
					$dwHead = {"Expression"};
					$dwStyle = {$dwDefaultExpressionStyle};
					$dwP = {{{0,0}}},
				_,
					$dwHead = If[head === Automatic, {$dwDefaultHead}, {head}];
					$dwStyle = {$dwFullDefaultStyle};
					$dwP = If[MemberQ[{Image, Text}, head], {{{0,0}}}, {{}}]
			];
			$dwSelected = {1};
			$dwBoundingBoxes = 
				Switch[form, 
					"text", {dwTextBox[$dwSelected[[1]]]}, 
					"image", {dwImageBox[$dwSelected[[1]]]}, 
					"expression", {dwExpressionBox[$dwSelected[[1]]]},
					_, {{}}
				];
			dwUpdateBoundingBox[$dwSelected];
			$dwObjectGradients = {{}};
			$dwLineGradients = {$dwDefaultGradient};
			$dwAnimate = {{$dwP[[$dwSelected[[1]]]], $dwStyle[[-1]], 0, {1,1}, {1,30,30,30}, 1, 1, 30, 30, 240, False, {0,0}, {{0,0}}}};
			$dwObjectQuantity = Length[$dwP]
	   ]
	]

Options[dwDeleteLayer] = {"SetUndo"->True};
dwDeleteLayer[OptionsPattern[]] :=
	Module[{selectionFiltered = {}, groupStart, compoundStart},
		If[OptionValue["SetUndo"], dwSetUndo[], Nothing];
		Switch[$dwMode,
			"preview"|"wireframe"|"draw",
				$dwP = Delete[$dwP, List/@$dwSelected];
				$dwStyle = Delete[$dwStyle, List/@$dwSelected];
				$dwHead = Delete[$dwHead, List/@$dwSelected];
				$dwBoundingBoxes = Delete[$dwBoundingBoxes, List/@$dwSelected];
				$dwObjectGradients = Delete[$dwObjectGradients, List/@$dwSelected];
				$dwLineGradients = Delete[$dwLineGradients, List/@$dwSelected];
				$dwAnimate = If[Length[$dwAnimate] == Length[$dwP] + Length[$dwSelected], Delete[$dwAnimate, List/@$dwSelected], {}];
				(* update groups *)
				$dwGroupLayers=$dwGroupLayers/.(#->Nothing&/@$dwSelected);
				$dwGroupLayers=If[Length[#]<2,Nothing,#]&/@$dwGroupLayers;
				groupStart=$dwGroupLayers;
				Do[Do[Do[
					If[groupStart[[gln,gln2]]>sl,$dwGroupLayers[[gln,gln2]]=$dwGroupLayers[[gln,gln2]]-1,Nothing],
						{gln2,Length[$dwGroupLayers[[gln]]]}],
					{gln,Length[$dwGroupLayers]}],
				{sl,$dwSelected}];
				(* update compoundpaths *)
				$dwCompoundPathLayers=$dwCompoundPathLayers/.(#->Nothing&/@$dwSelected);
				$dwCompoundPathLayers=If[Length[#]<2,Nothing,#]&/@$dwCompoundPathLayers;
				compoundStart=$dwCompoundPathLayers;
				Do[Do[Do[
					If[compoundStart[[gln,gln2]]>sl,$dwCompoundPathLayers[[gln,gln2]]=$dwCompoundPathLayers[[gln,gln2]]-1,Nothing],
						{gln2,Length[$dwCompoundPathLayers[[gln]]]}],
					{gln,Length[$dwCompoundPathLayers]}],
				{sl,$dwSelected}];
				(* update hidden layers *)
				Do[dwUpdateHideLayersAfterDelete[n], {n, Reverse[Sort[$dwSelected]]}];
				(* update image filters relying on other object *)
				Do[
					Do[
						Do[
							If[$dwHead[[pn]] === Image && Flatten[Position[$dwStyle[[pn]],{filter,__},Infinity]] =!= {},
								Which[
									$dwStyle[[Sequence@@Join[{pn},Position[$dwStyle[[pn]],{filter,__},Infinity][[1]],{2}]]] > sn,
										$dwStyle[[Sequence@@Join[{pn},Position[$dwStyle[[pn]],{filter,__},Infinity][[1]],{2}]]] -= 1,
									$dwStyle[[Sequence@@Join[{pn},Position[$dwStyle[[pn]],{filter,__},Infinity][[1]],{2}]]] == sn,
										$dwStyle[[Sequence@@Join[{pn},Position[$dwStyle[[pn]],{filter,__},Infinity][[1]],{2}]]] = pn,
									True,
										Nothing
								]
							],
						{filter, {"Mask"}}], 
					{pn, Length[$dwP]}], 
				{sn, $dwSelected}];
				(* update gradient filters relying on other object *)
				Do[
					Do[
						Do[
							If[Flatten[Position[$dwStyle[[pn]],{gradient,___},Infinity]] =!= {},
								Which[
									$dwStyle[[Sequence@@Join[{pn},Position[$dwStyle[[pn]],{gradient,___},Infinity][[1]],{2}]]] > sn,
										$dwStyle[[Sequence@@Join[{pn},Position[$dwStyle[[pn]],{gradient,___},Infinity][[1]],{2}]]] -= 1,
									$dwStyle[[Sequence@@Join[{pn},Position[$dwStyle[[pn]],{gradient,___},Infinity][[1]],{2}]]] == sn,
										$dwStyle[[Sequence@@Join[{pn},Position[$dwStyle[[pn]],{gradient,___},Infinity][[1]],{2}]]] = pn,
									True,
										Nothing
								]
							],
						{gradient, {"BlendGradient"}}], 
					{pn, Length[$dwP]}], 
				{sn, $dwSelected}];
				(* update text on curve *)
				Do[dwUpdateTextOnCurveAfterDelete[n], {n, Reverse[Sort[$dwSelected]]}];
				(* update selection *)
				(*$dwSelected=If[Flatten[Position[$dwGroupLayers,1]]==={},
					{1},
					Join[{1},$dwGroupLayers[[Flatten[Position[$dwGroupLayers,1]][[1]]]]]
				];*)
				$dwSelected = {},
			"point",
				Do[
					If[n[[2]] != 0,
						If[$dwHead[[n[[1]]]] === BezierCurve,
							(* add main point and its handles to selection for deletion *)
							If[Mod[n[[2]] + 2, 3] == 0,
								If[n[[2]] == 1,
									AppendTo[selectionFiltered, n];
									AppendTo[selectionFiltered, n + {0, +1}];
									(* remove first handle so path starts with a main point *)
									If[Complement[Range[1, Length[$dwP[[n[[1]]]]], 3], #[[2]]&/@Cases[$dwSelected, {n[[1]],_}]] =!= {},
										AppendTo[selectionFiltered, n + {0, Complement[Range[1, Length[$dwP[[n[[1]]]]], 3], #[[2]]&/@Cases[$dwSelected, {n[[1]],_}]][[1]] - 2}],
										Nothing
									],
									
									AppendTo[selectionFiltered, n + {0, -1}];
									AppendTo[selectionFiltered, n];
									AppendTo[selectionFiltered, n + {0, 1}]
								],
								Nothing
							], 
							AppendTo[selectionFiltered, n]
						],
					Nothing],
				{n, $dwSelected}];
				selectionFiltered = Sort[DeleteDuplicates[selectionFiltered]];
				(* do not remove object here if all of its points are selected; instead call dwDeleteEmptyLayers[] after dwDeleteLayer[] call  *)
				$dwP = Delete[$dwP, selectionFiltered](*/. {} -> Sequence[]*);
				$dwSelected = DeleteDuplicates[{#[[1]],0}&/@$dwSelected];
				dwUpdateBoundingBox[#[[1]]&/@$dwSelected],
			_,
				$dwSelected = {}
		];
		$dwObjectQuantity = Length[$dwP];
		$dwPointQuantity = Length[Flatten[$dwP, 1]];
		If[$dwMode === "draw", $dwMode = $dwCurrentPreviewMode, Nothing];
	]

Options[dwMoveLayers] = {"SetUndo"->True};
dwMoveLayers[layerListToMove_, moveToLayer_, OptionsPattern[]]:=
	Block[{newLayerOrder, replacementList},
		If[OptionValue["SetUndo"], dwSetUndo[], Nothing];
		dwConvertPointSelectionToLayerSelection[];
		newLayerOrder = dwUpdateOrderOfLayersAfterMove[Range[Length[$dwP]], layerListToMove, moveToLayer, "ReturnUpdatedList"->False];
		(* move layers *)
		$dwP = $dwP[[newLayerOrder]];
		$dwHead = $dwHead[[newLayerOrder]];
		$dwStyle = $dwStyle[[newLayerOrder]];
		$dwObjectGradients = $dwObjectGradients[[newLayerOrder]];
		$dwLineGradients = $dwLineGradients[[newLayerOrder]];
		$dwAnimate = If[Length[$dwAnimate] != Length[newLayerOrder], {}, $dwAnimate[[newLayerOrder]]];
		$dwBoundingBoxes = $dwBoundingBoxes[[newLayerOrder]];
		
		(* updates *)
		$dwGroupLayers = dwUpdateOrderOfLayersAfterMove[$dwGroupLayers, layerListToMove, moveToLayer];
		$dwCompoundPathLayers = dwUpdateOrderOfLayersAfterMove[$dwCompoundPathLayers, layerListToMove, moveToLayer];
		$dwHideLayers = dwUpdateOrderOfLayersAfterMove[$dwHideLayers, layerListToMove, moveToLayer];
		(* update image filters and gradients relying on other object - object number must be second item of list *)
		Do[
			$dwStyle = ReplacePart[$dwStyle,ReplacePart[pos,-1->2]->dwUpdateOrderOfLayersAfterMove[{Extract[$dwStyle,Most[pos]][[2]]},layerListToMove,moveToLayer][[1]]], 
			{pos, Flatten[Position[$dwStyle,#,Infinity]&/@{"Mask", "BlendGradient"}, 1]}
		];
		(* update text on curve *)
		Do[
			If[$dwStyle[[pos,12]] =!= None, 
				$dwStyle = ReplacePart[$dwStyle,{{pos,12}}->dwUpdateOrderOfLayersAfterMove[{$dwStyle[[pos, 12]]},layerListToMove,moveToLayer][[1]]],
				Nothing
			], {pos, Flatten[Position[$dwHead, Text, Infinity]]}];
		(* update selection *)
		replacementList = Flatten[Position[newLayerOrder, #] & /@ layerListToMove];
		$dwSelected = replacementList;
	]

Options[dwDuplicateLayer] = {"SetUndo"->True, "ChangeMode"->True};
dwDuplicateLayer[OptionsPattern[]]:=
	Module[{startLength = Length[$dwP], maskobjnum},
		If[OptionValue["SetUndo"], dwSetUndo[], Nothing];
		dwConvertPointSelectionToLayerSelection["ChangeMode"->OptionValue["ChangeMode"]];
		
		(* duplicate objects *)
		$dwSelected = Sort@$dwSelected;
		If[$dwSelected =!= {},
			Do[
				(*length = Length[$dwP];*)
				AppendTo[$dwP, 
					If[$dwConstrainHAngle < 0,
						Total[{$dwDupeOffset[[1]]*((1/$dwGridStep)*$dwGridSize[[2]]),$dwDupeOffset[[2]]*((1/$dwGridStep)*$dwGridSize[[1]])}],
						Total[{$dwDupeOffset[[1]]*((1/$dwGridStep)*$dwGridSize[[1]]),$dwDupeOffset[[2]]*((1/$dwGridStep)*$dwGridSize[[2]])}]
					]+#&/@$dwP[[s]]];
				AppendTo[$dwHead,$dwHead[[s]]];
				AppendTo[$dwStyle,$dwStyle[[s]]];
				AppendTo[$dwBoundingBoxes,$dwBoundingBoxes[[s]]];
				AppendTo[$dwObjectGradients, $dwObjectGradients[[s]]];
				AppendTo[$dwLineGradients, $dwLineGradients[[s]]];
				AppendTo[$dwAnimate, If[Length[$dwAnimate] < s, {{}}, $dwAnimate[[s]]]];
				$dwAnimate[[-1,-1]] = {{0,0}},
			{s, Sort@$dwSelected}];(* sorting did not work before because of unsorted compound paths and groups which are now sorted; keeping note here in case issues occur *)
			(* update compound paths *)
			$dwCompoundPathLayers = Sort[Sort[#]&/@$dwCompoundPathLayers];
			$dwCompoundPathLayers = Join[$dwCompoundPathLayers,
					Table[
						If[SubsetQ[$dwSelected, $dwCompoundPathLayers[[n]]],
							Table[startLength + Flatten[Position[$dwSelected, $dwCompoundPathLayers[[n,cpn]]]][[1]], {cpn, Length[$dwCompoundPathLayers[[n]]]}],
							Nothing
						],
					{n, Length[$dwCompoundPathLayers]}]
				];
			(* update groups *)
			$dwGroupLayers = Sort[Sort[#]&/@$dwGroupLayers];
			$dwGroupLayers = Join[$dwGroupLayers,
					Table[
						If[SubsetQ[$dwSelected, $dwGroupLayers[[n]]],
							Table[startLength + Flatten[Position[$dwSelected, $dwGroupLayers[[n,gn]]]][[1]], {gn, Length[$dwGroupLayers[[n]]]}],
							Nothing
						],
					{n, Length[$dwGroupLayers]}]
				];
			(* update image filters relying on other object *)
			Do[
				If[pos[[1]] > startLength, (* not original mask *)
					maskobjnum = $dwStyle[[Sequence@@ReplacePart[pos,-1->2]]];
					If[MemberQ[$dwSelected, maskobjnum], (* mask object was duplicated *)
						(* new mask object number *)
						$dwStyle = ReplacePart[$dwStyle, ReplacePart[pos,-1->2]->startLength + Flatten[Position[$dwSelected, maskobjnum]][[1]]],
						(* mask number since mask object not duplicated *)
						$dwStyle = ReplacePart[$dwStyle, ReplacePart[pos,-1->2]->pos[[1]]]
					],
					Nothing
				], 
				{pos, Flatten[Position[$dwStyle,#,Infinity]&/@{"Mask", "BlendGradient"}, 1]}
			];
		];
		
		(* updates *)
		$dwSelected = Range[startLength + 1, Length@$dwP];
		$dwObjectQuantity = Length[$dwP];
		$dwPointQuantity = Length[Flatten[$dwP, 1]];
		dwUpdateBoundingBox[$dwSelected]
	]

dwDeleteEmptyLayers[]:=
	Block[{sel = $dwSelected},
		$dwMode = $dwCurrentPreviewMode;
		$dwSelected = {};
		Do[
			If[$dwP[[n]] === {}, 
				AppendTo[$dwSelected, n],
				Nothing
			], 
		{n, Length@$dwP}];
		If[$dwSelected =!= {}, dwDeleteLayer["SetUndo"->False], $dwSelected = sel]
	]
	
End[] (* End Private Context *)

EndPackage[]