(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwObjects[]:=
	Block[{},
		{
			
			(* shapes *)
			Tooltip[
				dwObjectPresets[],
			"Choose object preset from popup menu...", TooltipDelay->$dwTooltipDelay],
	
			(* patterns *)
			Tooltip[
				dwObjectPatterns[],
			"Choose pattern preset from popup menu...", TooltipDelay->$dwTooltipDelay],
	
			(* libraries *)
			Tooltip[
				dwObjectLibrary[],
			"Choose library preset from popup menu...", TooltipDelay->$dwTooltipDelay],
	
			(* map *)
			Tooltip[
				dwObjectMaps[],
			"Choose map preset from popup menu...", TooltipDelay->$dwTooltipDelay]
				
		}
	]
	
dwObjectPresets[]:=
	Block[{},
		dwSetUndo[];
		ActionMenu[Pane[Show[$dwIconSpiral/.{Gray->$dwToolColor}, ImageSize->{$dwToolWidth/2.75, $dwToolWidth/2.75}]],
			{
				$dwIconBox:>(dwConvertPointSelectionToLayerSelection[]; dwShapeBox[]; $dwStyleMode = "fill"),
				$dwIconTextBox:>(dwConvertPointSelectionToLayerSelection[]; dwShapeTextBox[]; $dwStyleMode = "fill"),
				$dwIconDisk:>(dwConvertPointSelectionToLayerSelection[]; dwShapeDisk[]; $dwStyleMode = "fill"),
				Delimiter,
				$dwIconRoundedBox:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapeRoundedBox[]; $dwStyleMode = "fill"),
				$dwIconCapsule:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapeCapsule[]; $dwStyleMode = "fill"),
				$dwIconTorn:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapeTorn[]; $dwStyleMode = "fill"),
				$dwIconNSidedPolygon:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapeNSidedPolygon[]; $dwStyleMode = "fill"),
				$dwIconShapeCloud:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeCloud]; $dwStyleMode = "image"),
				$dwIconShapeFire:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeFire]; $dwStyleMode = "image"),
				Delimiter,
				$dwIconSpiral:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapeSpiral[]; $dwStyleMode = "stroke"),
				$dwIconSineWave:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapeLineWave[]; $dwStyleMode = "stroke"),
				Delimiter,
				$dwIconPolygonArrow:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapePolygonArrow[]; $dwStyleMode = "fill"),
				$dwIconPolygonMultiArrow:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapePolygonSplitArrow[]; $dwStyleMode = "fill"),
				$dwIconPolygonBentArrow:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapePolygonBentArrow[]; $dwStyleMode = "fill"),
				$dwIconPolygonSwoopArrow:>(dwConvertPointSelectionToLayerSelection[]; $dwSelected = {}; dwShapePolygonSwoopArrow[]; $dwStyleMode = "fill"),
				Delimiter,
				$dwIconDimensionArrow:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwDimensionArrow]; $dwStyleMode = "stroke"),
				$dwIconAngleArrow:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwAngleArrow]; $dwStyleMode = "stroke"),
				$dwIconAxes:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeAxes]; $dwStyleMode = "stroke")
			}, ImageSize->{$dwToolWidth/2, $dwToolWidth/2}, $dwButtonStyle]
	]
	
dwObjectPatterns[]:=
	Block[{},
		dwSetUndo[]; 
		ActionMenu[Pane[Show[$dwIconShapeArray/.{Gray->$dwToolColor}, ImageSize->{$dwToolWidth/2.75, $dwToolWidth/2.75}]],
			{
				$dwIconShapeArray:>(dwConvertPointSelectionToLayerSelection[]; dwShapeArray[]; $dwStyleMode = "fill"),
				$dwIconShapeHilbertCurve:>(dwConvertPointSelectionToLayerSelection[]; dwShapeHilbertCurve[]; $dwStyleMode = "stroke"),
				$dwIconShapePeanoCurve:>(dwConvertPointSelectionToLayerSelection[]; dwShapePeanoCurve[]; $dwStyleMode = "stroke"),
				$dwIconShapeSierpinskiCurve:>(dwConvertPointSelectionToLayerSelection[]; dwShapeSierpinskiCurve[]; $dwStyleMode = "stroke"),
				$dwIconShapeKochCurve:>(dwConvertPointSelectionToLayerSelection[]; dwShapeKochCurve[]; $dwStyleMode = "stroke"),
				ImageAdd[ImageMultiply[$dwIconShapeBarCode, GrayLevel[.2]], .65]:>(dwConvertPointSelectionToLayerSelection[]; dwShapeBarCode[]; $dwStyleMode = "image"),
				$dwIconShapeGuilloche:>(dwConvertPointSelectionToLayerSelection[]; dwGuillocheDialog[]; $dwStyleMode = "stroke"),
				$dwIconShapeTextDots:>(dwConvertPointSelectionToLayerSelection[]; textDots[]; $dwStyleMode = "point"),
				$dwIconShapeConcentricCircles:>(dwConvertPointSelectionToLayerSelection[]; dwShapeConcentricCircles[]; $dwStyleMode = "stroke"),
				$dwIconTicks:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeTicks]; $dwStyleMode = "stroke"),
				$dwIconShapeTreeCurve:>(dwConvertPointSelectionToLayerSelection[]; dwShapeTreeCurve[]; $dwStyleMode = "stroke"),
				$dwIconShapeTree:>(dwConvertPointSelectionToLayerSelection[]; dwShapeTreeDialog[]; $dwStyleMode = "stroke"),
				$dwIconShapeGraph:>(dwConvertPointSelectionToLayerSelection[]; dwShapeGraph[]; $dwStyleMode = "stroke"),
				$dwIconShapeRandomPoints:>(dwConvertPointSelectionToLayerSelection[]; dwShapeRandomPoints[]; $dwStyleMode = "point"),
				$dwIconShapeRoughenSelection:>(dwConvertPointSelectionToLayerSelection[]; dwShapeRoughenSelection[]; $dwStyleMode = "stroke")
			}, ImageSize->{$dwToolWidth/2, $dwToolWidth/2}, $dwButtonStyle]
	]
	
dwObjectLibrary[]:=
	Block[{},
		dwSetUndo[];
		ActionMenu[Pane[Show[$dwIconCoilSpring, ImageSize->{$dwToolWidth/2.75, $dwToolWidth/2.75}]],
			{
				$dwIconCoilSpring:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeCoilSpring]; $dwStyleMode = "stroke"),
				$dwIconGear:>(dwConvertPointSelectionToLayerSelection[]; dwShapeGear[]; $dwStyleMode = "fill"),
				$dwIconShapePivot:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapePivot]; $dwStyleMode = "fill"),
				$dwIconShapeResistor:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeResistor]; $dwStyleMode = "stroke"),
				$dwIconShapeRheostat:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeRheostat]; $dwStyleMode = "stroke"),
				$dwIconShapeCapacitor:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeCapacitor]; $dwStyleMode = "stroke"),
				$dwIconShapeInductor:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeInductor]; $dwStyleMode = "stroke"),
				$dwIconShapeInductorCoil:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeInductorCoil]; $dwStyleMode = "stroke"),
				$dwIconShapeCurrentSource:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeCurrentSource]; $dwStyleMode = "stroke"),
				$dwIconShapeVoltageSource:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeVoltageSource]; $dwStyleMode = "stroke"),
				$dwIconShapeACVoltageSource:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeACVoltageSource]; $dwStyleMode = "stroke"),
				$dwIconShapeGround:>(dwConvertPointSelectionToLayerSelection[]; dwShapeStyleDialog[dwShapeGround]; $dwStyleMode = "stroke"),
				$dwIconShapeNode:>(dwConvertPointSelectionToLayerSelection[]; dwShapeNode[]; $dwStyleMode = "fill"),
				$dwIconShapeOpenNode:>(dwConvertPointSelectionToLayerSelection[]; dwShapeOpenNode[]; $dwStyleMode = "fill")
			}, ImageSize->{$dwToolWidth/2, $dwToolWidth/2}, $dwButtonStyle]
	]
	
dwObjectMaps[]:=
	Block[{},
		dwSetUndo[];
		ActionMenu[Pane[Show[$dwIconMap/.{Gray->$dwToolColor}, ImageSize->{$dwToolWidth/2.75, $dwToolWidth/2.75}]],
			{
				$dwIconMap:>(dwConvertPointSelectionToLayerSelection[]; dwShapeMap[]; $dwStyleMode = "fill"),
				$dwIconShapeCityBuilder:>(dwConvertPointSelectionToLayerSelection[]; dwShapeCityBuilder[]; $dwStyleMode = "fill")
			}, ImageSize->{$dwToolWidth/2, $dwToolWidth/2}, $dwButtonStyle]
	]

End[] (* End Private Context *)

EndPackage[]