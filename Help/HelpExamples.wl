(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
		
dwHelpExamples[]:=
{"Examples", "","",
Grid[{
	{Style["Assorted", Sequence@@$dwHelpSubheadStyle], SpanFromLeft},
	{
		Button[Style[Column[{Magnify[dwExamplePointReductionPreview[],.5],"Point reduction"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExamplePointReduction[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwExampleHandDrawnArrowPreview[],.5],"Hand drawn"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleHandDrawnArrow[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwStarArrayPreview[],.5],"Fifty stars"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwStarArray[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwSpiralArrayPreview[],.5],"Spiral array"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwSpiralArray[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwSpiralAnimationPreview[],.5],"Animated spiral"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwSpiralAnimation[]}, Appearance->None]
	},
	{},
	{Style["Auto position", Sequence@@$dwHelpSubheadStyle], SpanFromLeft},
	{
		Button[Style[Column[{Magnify[dwExampleAngleArrowPreview[],2/3],"Rotation angle"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleAngleArrow[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwExampleSpringPreview[],2/3],"Spring"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleSpring[]}, Appearance->None]
	},
	{},
	{Style["Gradients", Sequence@@$dwHelpSubheadStyle], SpanFromLeft},
	{
		Button[Style[Column[{Magnify[dwExampleGradientArrowPreview[],.5],"Arrow"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleGradientArrow[]}, Appearance->None]
	},
	{},
	{Style["Image", Sequence@@$dwHelpSubheadStyle], SpanFromLeft},
	{
		Button[Style[Column[{Magnify[dwExampleImageGradientFilterPreview[],.5],"GradientFilter"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageGradientFilter[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwExampleImageGradientFilter2Preview[],.5],"GradientFilter2"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageGradientFilter2[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwExampleImageRidgeFilterPreview[],.5],"RidgeFilter"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageRidgeFilter[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwExampleImageRidgeFilter2Preview[],.5],"RidgeFilter2"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageRidgeFilter2[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwExampleImageObjectMultiplyPreview[],.5],"Blend"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageObjectMultiply[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwExampleImageObjectMaskPreview[],.5],"Mask"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageObjectMask[]}, Appearance->None]
	},
	{},
	{Style["3D", Sequence@@$dwHelpSubheadStyle], SpanFromLeft},
	{
		Button[Style[Column[{Magnify[dwAxoBoxPreview[],.5],"Projection"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwAxoBoxProjection[]}, Appearance->None],
		Button[Style[Column[{Magnify[dwAxoBoxExtrudePreview[],.5],"Extrusion"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwAxoBoxExtrude[]}, Appearance->None]
	}
}, Alignment->{Center, Bottom, Table[{n,1}->Left, {n, 1,13,3}]}, Spacings->{1,.5}]
}

End[] (* End Private Context *)

EndPackage[]