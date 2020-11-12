(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwStyleMode[sel_, opts_]:=
	Panel[Row[{
		dwHead[], dwStylePresets[],

		Switch[$dwStyleMode,
			"image",
				If[MemberQ[{Image}, $dwHead[[sel]]],
					Row[{
						Button[Style["CAMERA IMAGE", 8], 
							dwSetUndo[]; $dwStyle[[sel,2]] = ImageResize[CurrentImage[], {{$dwImageCameraMaxSize},{$dwImageCameraMaxSize}}]; $dwStyle[[sel,4]] = $dwImageFilterValuesDefault; dwUpdateBoundingBox[{sel}],
						Appearance->"Palette", ImageSize->{100, 26}],
						
						Button[Style["CANVAS IMAGE", 8],
							Block[{offset = $dwOriginOffset, sHeight = $dwStyleHeight},
								$dwShowSelectionBox = False;
								$dwOriginOffset = $dwOriginOffset*If[$dwZoom <= 1, 0, .4 + 0.07384 ($dwZoom - 1.5)];
								$dwHideLayers = Join[$dwHideLayers, {sel}];
								dwSetUndo[]; 
								$dwStyle[[sel,2]] = ImageResize[Rasterize[dwRender[], ImageResolution->$dwImageResolution], {{$dwImageCanvasMaxSize},{$dwImageCanvasMaxSize}}];
								$dwStyle[[sel,4]] = {};
								dwUpdateBoundingBox[{sel}];
								$dwHideLayers = Delete[$dwHideLayers, -1];
								$dwOriginOffset = offset;
								$dwStyleHeight = sHeight;
								$dwShowSelectionBox = True;
							],
						Appearance->"Palette", ImageSize->{100, 26}]
					}],
					Nothing],
			_,
				Nothing
		],
		
		Spacer[10],
		
		Pane[Row[{
			Tooltip[Button[$dwIconStyleFill, $dwStyleMode = "fill", opts], "Show fill styles", TooltipDelay->1],
			Tooltip[Button[$dwIconStyleStroke, $dwStyleMode = "stroke", opts], "Show stroke styles", TooltipDelay->1],
			Tooltip[Button[$dwIconStyleArrow, $dwStyleMode = "arrow", opts], "Show arrow styles", TooltipDelay->1],
			Tooltip[Button[$dwIconStylePoint, $dwStyleMode = "point", opts], "Show point styles", TooltipDelay->1],
			Tooltip[Button[$dwIconStyleText, $dwStyleMode = "text", opts], "Show text styles", TooltipDelay->1],
			Tooltip[Button[$dwIconStyleImage, $dwStyleMode = "image", opts], "Show image styles", TooltipDelay->1]
		}], Alignment->Center, BaselinePosition->Scaled[.35], ImageSize->{114,23}]
	}],Background->GrayLevel[.5], FrameMargins->0, ImageSize->Full]

End[] (* End Private Context *)

EndPackage[]