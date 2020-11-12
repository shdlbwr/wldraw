(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwDraw[]:=
	Module[{buttonImageSize = {$dwToolWidth/2, $dwToolWidth/2}, iconImageSize = {$dwToolWidth/2.75, $dwToolWidth/2.75}},
		{
			
			(* draw *)
			Tooltip[(*Dynamic[*)Button[Show[$dwIconAddPoint,ImageSize->.8iconImageSize],
				dwConvertPointSelectionToLayerSelection[];
				$dwPointModeSelections = $dwSelected;
				$dwMode = "draw";
				$dwStyleMode = "stroke";
				$dwDrawNewObject = True;
				dwNewEmptyLayer[],
				Background->Dynamic@If[$dwMode==="draw", $dwButtonHighlight, $dwButtonBackgroundColor], ImageSize->buttonImageSize, $dwButtonStyle](*]*), 
			Dynamic@If[
				MemberQ[{BezierCurve}, 
					Which[
						$dwSelected === {}, {},
						Length[$dwSelected[[1]]] == 2, $dwHead[[$dwSelected[[1, 1]]]],
						True, $dwHead[[$dwSelected[[1]]]]
					]
				],
				If[$dwP =!= {} && $dwP[[-1]] === {},
					(* no path *)
					Row[{"Draw path\n-----\nDrag canvas for each point",
						If[$dwPointModeSelections =!= {} && $dwPointModeSelections =!= $dwSelected, Row[{"\nDrag end point to continue existing path"}], ""],
						If[$dwPointModeSelections === $dwSelected, "\n"<>$dwOptionKey<>" drag for corner point\nClick Draw button to start new path", ""]
					}],
					(* path *)
					Row[{"Draw path\n-----\nDrag canvas for each point",
						If[MemberQ[{"draw", "point", "pointwireframe"}, $dwMode] && $dwSelected =!= {}, "\n"<>$dwOptionKey<>" drag for corner point\nClick Draw button to start new path", ""]
					}]
				],
				If[$dwP =!= {} && $dwP[[-1]] === {},
					(* no path *)
					Row[{"Draw path\nClick canvas for each point",
						If[$dwPointModeSelections =!= {} && $dwPointModeSelections =!= $dwSelected, Row[{"\nClick end point to continue existing path"}], ""],
						If[$dwPointModeSelections === $dwSelected, "\nClick Draw button to start new path", ""]
					}],
					(* path *)
					Row[{"Draw path\n-----\nClick canvas for each point",
						If[$dwMode === "draw" && $dwSelected =!= {}, "\nClick Draw button to start new path", ""]
					}]
				]
			], TooltipDelay->$dwTooltipDelay],
			
			(* text *)
			Tooltip[Button[Show[$dwIconText/.{(FontSize->_)->(FontSize->36)},ImageSize->iconImageSize],
				$dwMode = $dwCurrentPreviewMode;
				$dwStyleMode = "text";
				dwNewEmptyLayer["Form"->"text"],
			ImageSize->buttonImageSize, $dwButtonStyle],
			"Insert text", TooltipDelay->$dwTooltipDelay],
	
			(* 3D *)
			Tooltip[
				Button[Show[$dwIcon3D,ImageSize->iconImageSize],
					dwSetUndo[];
					dwConvertPointSelectionToLayerSelection[];
					$dwAxoAxisRotation = {0,0,0};
					$dwAxoExtrude = False;
					$dwAxoExtrudeAmount = -.2;
					$dwAxoExtrudeOrder = 1;
					If[$dwSelected =!= {},
						If[$dwHead[[$dwSelected[[1]]]] === "Text3D",
							{$dwTilt, $dwTurn, $dwAxoProjection, $dwAxoRotateOrder, $dwAxoAxisRotation} = $dwStyle[[$dwSelected[[1]],1]],
							Nothing
						],
						Nothing
					];
					dwAxoDialog[]
					,
				ImageSize->buttonImageSize, $dwButtonStyle],
			"3D\n-----\nDistort selected 2D objects to 3D...\n-----\nSet 3D view by clicking this tool with nothing selected...\nView change will not update existing objects", TooltipDelay->$dwTooltipDelay],
	
			(* image *)
			Tooltip[Button[Show[$dwIconImage,ImageSize->iconImageSize],
				$dwMode = $dwCurrentPreviewMode;
				$dwStyleMode = "image";
				dwNewEmptyLayer["Form"->"image"];
				dwUpdateBoundingBox[{-1}], ImageSize->buttonImageSize, $dwButtonStyle],
			"Insert bitmap image or graphics", TooltipDelay->$dwTooltipDelay]
				
		}
	]

End[] (* End Private Context *)

EndPackage[]