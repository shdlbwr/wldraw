(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 



(* ---------- examples ---------- *)
dwExampleImageGradientFilter[]:=
{"Image - GradientFilter","",
dwExampleImageGradientFilterPreview[],"",
Row[{"1. Click ", Button[$dwIconImage, Null, ImageSize->{$dwShapeMenuIconSize,$dwShapeMenuIconSize}, $dwButtonStyle]," from objects."}],
"2. Change image filter name 'None' to 'GradientFilter'.
3. Click '+' to add a new image filter.
4. Change 'None' to 'StandardDeviationFilter' then set to 2.
5. Click '+' to add a new image filter.
6. Change 'None' to 'Sharpen'.",
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwExampleImageGradientFilter2Preview[],.5],"GradientFilter2"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageGradientFilter2[]}, Appearance->None],
	Button[Style[Column[{Magnify[dwExampleImageRidgeFilterPreview[],.5],"RidgeFilter"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageRidgeFilter[]}, Appearance->None],
	Button[Style[Column[{Magnify[dwExampleImageRidgeFilter2Preview[],.5],"RidgeFilter2"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageRidgeFilter2[]}, Appearance->None]
}}, Spacings->{2,0}]
}

dwExampleImageGradientFilter2[]:=
{"Image - GradientFilter 2","",
dwExampleImageGradientFilter2Preview[],"",
Row[{"1. Click ", Button[$dwIconImage, Null, ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth}, $dwButtonStyle]," from objects."}],
"2. Change image filter name 'None' to 'GradientOrientationFilter'.
3. Click '+' to add a new image filter.
4. Change 'None' to 'StandardDeviationFilter' then set to 1.9.
5. Click '+' to add a new image filter.
6. Change 'None' to 'Grayscale' then set to 0.7.",
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwExampleImageGradientFilterPreview[],.5],"GradientFilter"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageGradientFilter[]}, Appearance->None],
	Button[Style[Column[{Magnify[dwExampleImageRidgeFilterPreview[],.5],"RidgeFilter"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageRidgeFilter[]}, Appearance->None],
	Button[Style[Column[{Magnify[dwExampleImageRidgeFilter2Preview[],.5],"RidgeFilter2"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageRidgeFilter2[]}, Appearance->None]
}}, Spacings->{2,0}]
} 

dwExampleImageRidgeFilter[]:=
{"Image - RidgeFilter","",
dwExampleImageRidgeFilterPreview[],"",
Row[{"1. Click ", Button[$dwIconImage, Null, ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth}, $dwButtonStyle]," from objects."}],
"2. Change image filter name 'None' to 'RidgeFilter'.
3. Click '+' to add a new image filter.
4. Change 'None' to 'Grayscale' then set to 0.7.
5. Click '+' to add a new image filter.
6. Change 'None' to 'Sharpen'.",
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwExampleImageGradientFilterPreview[],.5],"GradientFilter"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageGradientFilter[]}, Appearance->None],
	Button[Style[Column[{Magnify[dwExampleImageGradientFilter2Preview[],.5],"GradientFilter2"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageGradientFilter2[]}, Appearance->None],
	Button[Style[Column[{Magnify[dwExampleImageRidgeFilter2Preview[],.5],"RidgeFilter2"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageRidgeFilter2[]}, Appearance->None]
}}, Spacings->{2,0}]
}

dwExampleImageRidgeFilter2[]:=
{"Image - RidgeFilter 2","",
dwExampleImageRidgeFilter2Preview[],"",
Row[{"1. Click ", Button[$dwIconImage, Null, ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth}, $dwButtonStyle]," from objects."}],
"2. Change image filter name 'None' to 'LaplacianGaussianFilter'.
3. Click '+' to add a new image filter.
4. Change 'None' to 'BalancedBinarize'.
5. Click '+' to add a new image filter.
6. Change 'None' to 'RidgeFilter'.",
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwExampleImageGradientFilterPreview[],.5],"GradientFilter"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageGradientFilter[]}, Appearance->None],
	Button[Style[Column[{Magnify[dwExampleImageGradientFilter2Preview[],.5],"GradientFilter2"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageGradientFilter2[]}, Appearance->None],
	Button[Style[Column[{Magnify[dwExampleImageRidgeFilterPreview[],.5],"RidgeFilter"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageRidgeFilter[]}, Appearance->None]
}}, Spacings->{2,0}]
}

dwExampleImageObjectMultiply[]:=
{"Image - Blend","",
dwExampleImageObjectMultiplyPreview[],"",
Row[{"1. Click ", Button[$dwIconImage, Null, ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth}, $dwButtonStyle]," from draw section."}],
Row[{"2. Click ", Button[$dwIconObjectDupe, Null, $dwButtonStyle]," to duplicate at an offset."}],
"3. Change image filter name 'None' to 'Blend'.",
Row[{"4. Click ", Button[$dwIconClipboard, Null, ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth}, $dwButtonStyle]," then paste into notebook."}],
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Blends are only displayed in pasted graphics, not on the canvas.
- The Blend filter of one image does not pass to the Blend filter of another image.
- The Blend and Mask filters cannot be used with the same image.",
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwExampleImageObjectMaskPreview[],.5],"Mask"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageObjectMask[]}, Appearance->None]
}}, Spacings->{2,0}]
}

dwExampleImageObjectMask[]:=
{"Image - Mask","",
dwExampleImageObjectMaskPreview[],"",
Row[{"1. Choose ", Graphics[{Gray,Disk[]},ImageSize->24]," from the object preset popup menu ", dwObjectPresets[]/.{(ImageSize->_)->(ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth})}, "."}],
Row[{"2. Click ", Button[$dwIconImage, Null, ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth}, $dwButtonStyle]," from draw section."}],
"3. Change image filter name 'None' to 'Mask' and set to 1.",
"",
"-------------------------------------------------------------------
The default object number of the popup menu is the object using the filter so nothing 
is applied. To locate the ID number of an object just select it and the ID is displayed in 
the message bar located at the bottom of the window. Any image, shape or text can be 
used to mask. Image and Text used as a mask are automatically hidden but can become 
visible by choosing 'Show all' of selection menu. 'Wireframe' mode displays mask shapes.
-------------------------------------------------------------------",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- The Blend and Mask filters cannot be used with the same image.",
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwExampleImageObjectMultiplyPreview[],.5],"Blend"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageObjectMultiply[]}, Appearance->None]
}}, Spacings->{2,0}]
}



(* ---------- previews ---------- *)
dwExampleImageGradientFilterPreview[]:= ImageResize[Sharpen[ImageAdjust[StandardDeviationFilter[RemoveAlphaChannel[ImageAdjust[GradientFilter[$dwImageFilterImageDefault,8]]],2]],4],100]
dwExampleImageGradientFilter2Preview[]:= ImageResize[ImageAdjust[ColorConvert[ImageAdjust[StandardDeviationFilter[RemoveAlphaChannel[ImageAdjust[GradientOrientationFilter[$dwImageFilterImageDefault,8]]],1]],"Grayscale"],{0,0,.6}],100]
dwExampleImageRidgeFilterPreview[]:= ImageResize[Sharpen[ImageAdjust[ImageAdjust[ColorConvert[ImageAdjust[RidgeFilter[$dwImageFilterImageDefault,4]],"Grayscale"]],{0,0,.6}],4],100]
dwExampleImageRidgeFilter2Preview[]:= ImageResize[ImageAdjust[RidgeFilter[LocalAdaptiveBinarize[ImageAdjust[LaplacianGaussianFilter[$dwImageFilterImageDefault,8]],8],4]],100]
dwExampleImageObjectMultiplyPreview[]:= ColorReplace[ImageResize[ImageCrop[ImageMultiply[Graphics[Inset[$dwImageFilterImageDefault],ImageSize->100],Graphics[Inset[$dwImageFilterImageDefault,{.05,.05}],PlotRange->.2,ImageSize->100],1]],100],White->$dwHelpBackgroundColor,.1]
dwExampleImageObjectMaskPreview[]:= ColorReplace[ImageResize[SetAlphaChannel[Graphics[Inset[$dwImageFilterImageDefault,{0,0},ImageScaled[{.5,.5}],.4],PlotRange->0.2],Graphics[{White,Disk[{0,0},.2]},Background->Black,ImageSize->180,PlotRange->0.2]],100],White->$dwHelpBackgroundColor,.1]


End[] (* End Private Context *)

EndPackage[]