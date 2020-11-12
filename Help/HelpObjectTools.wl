(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
		
dwHelpObjectTools[]:=
	{
		{$dwIconObjectDupe, dwHelpObjectDupe[]},
		{$dwIconMoveToFront, dwHelpMoveToFront[]},
		{$dwIconGroupShape, dwHelpGroupShape[]},
		{$dwIconCombineShapes, dwHelpCombineShapes[]},
		{$dwIconIntersectShapes, dwHelpIntersectShapes[]},
		{$dwIconBlendShapes, dwHelpBlendShapes[]},
		{$dwIconShearShape, dwHelpShearShape[]},
		{$dwIconReflectH, dwHelpReflectH[]},
		{$dwIconMirrorH, dwHelpMirrorH[]},
		{$dwIconCompoundPath, dwHelpCompoundPath[]},
		{$dwIconRoundCorners, dwHelpRoundCorners[]},
		{$dwIconDistortShape, dwHelpDistortShape[]},
		{$dwIconObjectShadow, dwHelpObjectShadow[]},
		{$dwIconOffsetLines, dwHelpOffsetLines[]},
		{$dwIconConnector, dwHelpConnector[]},
		{$dwIconAlign, dwHelpAlign[]},
		{$dwIconCenterObject, dwHelpCenterObject[]},
		{$dwIconSnapPts, dwHelpSnapPts[]}
	}
	
dwHelpConnector[]:= {"Arrow connector", "",
"Insert arrow from first selected object to second selected object.
Choose the arrow shape from the popup menu.",
"",
"There are three arrow shapes:",
Grid[{{
	Labeled[Graphics[{Arrowheads[Small],CapForm["Butt"],Arrow[{{0,0},{.5,.5}}],Rectangle[{1,1},{.5,.5}],Hue[0,1,.8],Rectangle[{-.25,-.25},{.25,.25}],White,Text["1",{0,0}],Text["2",{.75,.75}]},ImageSize->{Automatic,48}], "straight"],
	Labeled[Graphics[{Arrowheads[Small],CapForm["Butt"],Arrow[{{0,0},{0,.75},{.5,.75}}],Rectangle[{1,1},{.5,.5}],Hue[0,1,.8],Rectangle[{-.25,-.25},{.25,.25}],White,Text["1",{0,0}],Text["2",{.75,.75}]},ImageSize->{Automatic,48}],"corner"],
	Labeled[Graphics[{Arrowheads[Small],CapForm["Butt"],Arrow[{{0,0},{.625,0},{.625,.75},{1,.75}}],Rectangle[{1.5,1},{1,.5}],Hue[0,1,.8],Rectangle[{-.25,-.25},{.25,.25}],White,Text["1",{0,0}],Text["2",{1.25,.75}]},ImageSize->{Automatic,48}],"zigzag"]
}},Spacings->3],
"",
"The gray lines represent arrow possibilities dependent on object position.
",
Row[{Graphics[{Gray,Rectangle[{-1,-1},{-.5,-.5}],Rectangle[{1,-1},{.5,-.5}],Rectangle[{-1,1},{-.5,.5}],Line[{{-.75,-.75},{.75,.75}}],Line[{{-.75,.75},{.75,-.75}}],Black,CapForm["Butt"],Line[{{0,0},{.5,.5}}],Rectangle[{1,1},{.5,.5}],Hue[0,1,.8],Rectangle[{-.25,-.25},{.25,.25}]},ImageSize->30],
	"= straight"}],
"",
Row[{Graphics[{Gray,Rectangle[{-1,-1},{-.5,-.5}],Rectangle[{1,-1},{.5,-.5}],Rectangle[{-1,1},{-.5,.5}],Line[{{0,0},{0,.75}}],Line[{{-.75,.75},{.75,.75}}],Line[{{0,0},{0,-.75}}],Line[{{-.75,-.75},{.75,-.75}}],Black,CapForm["Butt"],Line[{{0,0},{0,.75},{.5,.75}}],Rectangle[{1,1},{.5,.5}],Hue[0,1,.8],Rectangle[{-.25,-.25},{.25,.25}]},ImageSize->30],
	"= corner vertical"}],
"",
Row[{Graphics[{Gray,Rectangle[{-1,-1},{-.5,-.5}],Rectangle[{1,-1},{.5,-.5}],Rectangle[{-1,1},{-.5,.5}],Line[{{0,0},{.75,0}}],Line[{{.75,-.75},{.75,.75}}],Line[{{0,0},{-.75,0}}],Line[{{-.75,-.75},{-.75,.75}}],Black,CapForm["Butt"],Line[{{0,0},{.75,0},{.75,.5}}],Rectangle[{1,1},{.5,.5}],Hue[0,1,.8],Rectangle[{-.25,-.25},{.25,.25}]},ImageSize->30],
	"= corner horizontal"}],
"",
Row[{Graphics[{Gray,Rectangle[{-1,-1.5},{-.5,-1}],Rectangle[{.5,-1.5},{1,-1}],Rectangle[{-1,1.5},{-.5,1}],Line[{{0,-.625},{0,0.625}}],Line[{{-.75,1},{-.75,.625},{.75,.625},{.75,1}}],Line[{{-.75,-1},{-.75,-.625},{.75,-.625},{.75,-1}}],Black,CapForm["Butt"],Line[{{0,0},{0,.625},{.75,.625},{.75,1}}],Rectangle[{1,1.5},{.5,1}],Hue[0,1,.8],Rectangle[{-.25,-.25},{.25,.25}]},AspectRatio->1,ImageSize->30],
	"= zigzag vertical"}],
"",
Row[{Graphics[{Gray,Rectangle[{-1.5,-1},{-1,-.5}],Rectangle[{1,-1},{1.5,-.5}],Rectangle[{-1.5,1},{-1,.5}],Line[{{-.625,0},{.625,0}}],Line[{{1,.75},{.625,.75},{.625,-.75},{1,-.75}}],Line[{{-1,.75},{-.625,.75},{-.625,-.75},{-1,-.75}}],Black,CapForm["Butt"],Line[{{0,0},{.625,0},{.625,.75},{1,.75}}],Rectangle[{1.5,1},{1,.5}],Hue[0,1,.8],Rectangle[{-.25,-.25},{.25,.25}]},AspectRatio->1,ImageSize->30],
	"= zigzag horizontal"}],
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Straight arrows occur if object centers align vertically or horizontally.
- Straight arrows occur if space between objects is too small for requested arrow.",
Row[{"- Select arrow after creating and click ", Button[$dwIconReverseDirection, Null, $dwButtonStyle], " to change direction."}]
}
	
dwHelpGroupShape[]:= {"Group objects", "",
"Group selected objects.
"<>StringTake[$dwCommandKey, {1,-4}]<>"-click to remove grouping.

Click an object to select its entire group.
Drag around or "<>StringTake[$dwOptionKey, {1,-4}]<>"-click an object to select individual objects within group.",
"",
Graphics[{GrayLevel[.8], EdgeForm[{Black}], Polygon[{{-0.5, 0.2}, {-0.1, 0.2}, {-0.1, -0.2}, {-0.5, -0.2}}], FilledCurve[BezierCurve[{{0.2, 0.2}, {0.31, 0.2}, {0.4, 0.11}, {0.4, 0.}, {0.4, -0.11}, {0.31, -0.2}, {0.2, -0.2}, {0.09, -0.2}, {0., -0.11}, {0., 0.}, {0., 0.11}, {0.09, 0.2}, {0.2, 0.2}}]], Opacity[0], EdgeForm[{Hue[0,1,.8]}], Polygon[{{-0.525, 0.225}, {0.425, 0.225}, {0.425, -0.225}, {-0.525, -0.225}}], Hue[0,1,.8,1], Polygon[{{-0.525, 0.225}, {-0.505, 0.225}, {-0.505, 0.205}, {-0.525, 0.205}}], Polygon[{{0.405, 0.225}, {0.425, 0.225}, {0.425, 0.205}, {0.405, 0.205}}], Polygon[{{-0.525, -0.205}, {-0.505, -0.205}, {-0.505, -0.225}, {-0.525, -0.225}}], Polygon[{{0.405, -0.205}, {0.425, -0.205}, {0.425, -0.225}, {0.405, -0.225}}]}, ImageSize->200]
}
	
dwHelpCompoundPath[]:= {"Compound object", "",
"Select two or more 'Line' objects then click the compound object tool.
'Arrow', 'Point' and 'Polygon' objects will be converted to 'Line' objects.",
StringTake[$dwCommandKey, {1,-4}]<>"-click the compound path tool to release selected compound objects.",
Grid[{
{Graphics[{Black, Line[{{0.11755705045849463,-0.1618033988749895},{0.1902113032590307, 0.06180339887498949},{0.,0.2},{-0.1902113032590307,0.06180339887498949},{-0.11755705045849463,-0.1618033988749895},{0.11755705045849463,-0.1618033988749895}}],Line[{{0.058778525229247314,-0.07135254915624213},{0.09510565162951536,0.040450849718747385},{0.,0.10954915028125264},{-0.09510565162951536,0.040450849718747385},{-0.058778525229247314,-0.07135254915624213},{0.058778525229247314,-0.07135254915624213}}]}, ImageSize->100],
	Spacer[30],
Graphics[{EdgeForm[Black], Gray, FilledCurve[{Line[{{0.11755705045849463,-0.1618033988749895},{0.1902113032590307, 0.06180339887498949},{0.,0.2},{-0.1902113032590307,0.06180339887498949},{-0.11755705045849463,-0.1618033988749895},{0.11755705045849463,-0.1618033988749895}}],Line[{{0.058778525229247314,-0.07135254915624213},{0.09510565162951536,0.040450849718747385},{0.,0.10954915028125264},{-0.09510565162951536,0.040450849718747385},{-0.058778525229247314,-0.07135254915624213},{0.058778525229247314,-0.07135254915624213}}]}]}, ImageSize->100]},
{Style["before",Italic],"",Style["after",Italic]}
}],
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
Row[{"- Compound objects are only visible in 'Preview'. ", Button[$dwIconPreview, Null, $dwButtonStyle], " toggles 'Wireframe' | 'Preview'."}],
"- Remove line connecting compound objects by setting the stroke opacity to 0 or 
   choosing 'hide edge' from the PRESETS popup menu.
- Set a compound object's fill to a gradient such as 'Solid' to remove connecting lines.
- BezierCurve may be used but may not show holes if gradient is used.
- BSplineCurve may be used but ignores closed shapes.
",
Grid[{
{Graphics[{Line[{{-0.2,-0.2},{0.2,-0.2},{0.2,0.2},{-0.2,0.2}}],Line[{{-0.1,-0.1},{0.3,-0.1},{0.3,0.3},{-0.1,0.3}}]},ImageSize->100],
	Spacer[30],
Graphics[{EdgeForm[Black],Gray,FilledCurve[{Line[{{-0.2,-0.2},{0.2,-0.2},{0.2,0.2},{-0.2,0.2}}],Line[{{-0.1,-0.1},{0.3,-0.1},{0.3,0.3},{-0.1,0.3}}]}]},ImageSize->100],
	Spacer[30],
Graphics[{EdgeForm[Black],Texture[Graphics[{Gray,Thickness[1],Line[{{0,0.5},{1,0.5}}]},ImagePadding->0,PlotRangePadding->0]],FilledCurve[{{Line[{{-0.2,-0.2},{0.2,-0.2},{0.2,0.2},{-0.2,0.2}}]},{Line[{{-0.1,-0.1},{0.3,-0.1},{0.3,0.3},{-0.1,0.3}}]}},VertexTextureCoordinates->{{{0.,0.},{0.8,0.},{0.8,0.8},{0.,0.8}},{{0.2,0.2},{1.,0.2},{1.,1.},{0.2,1.}}}]},ImageSize->100]},
{Style["original lines",Italic],"",Style["standard fill",Italic],"",Style["'Solid' gradient",Italic]}
}]
}
	
dwHelpMoveToFront[]:= {"Move to front", "",
"Move selected objects to front.",
StringTake[$dwCommandKey, {1,-4}]<>"-click to move to back.",
StringTake[$dwOptionKey, {1,-4}]<>"-click to move all selected to front selection.",
StringTake[$dwShiftKey, {1,-4}]<>"-click to move all selected to back selection.",
Grid[{
{Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{-0.3,-0.1},{0.3,-0.1},{0.3,0.1},{-0.3,0.1}}], Polygon[{{0.2,0.},{0.,0.2},{-0.2,0.},{0.,-0.2}}]}, ImageSize->{Automatic,100}],
	Spacer[30],
Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{0.2,0.},{0.,0.2},{-0.2,0.},{0.,-0.2}}], Polygon[{{-0.3,-0.1},{0.3,-0.1},{0.3,0.1},{-0.3,0.1}}]}, ImageSize->{Automatic,100}]},
{Style["before",Italic],"",Style["after",Italic]}
}]
}
	
dwHelpObjectDupe[]:= {"Duplicate", "",
"Duplicate selected objects.",
StringTake[$dwCommandKey, {1,-4}]<>"-click to set distance, direction and quantity.",
Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8],
	Polygon[{{-0.5,0.1},{-0.4,0.},{-0.5,-0.1},{-0.6,0.}}],
	Polygon[{.25,0}+#&/@{{-0.5,0.1},{-0.4,0.},{-0.5,-0.1},{-0.6,0.}}],
	Polygon[{.5,0}+#&/@{{-0.5,0.1},{-0.4,0.},{-0.5,-0.1},{-0.6,0.}}],
	Polygon[{.75,0}+#&/@{{-0.5,0.1},{-0.4,0.},{-0.5,-0.1},{-0.6,0.}}],
	Polygon[{1,0}+#&/@{{-0.5,0.1},{-0.4,0.},{-0.5,-0.1},{-0.6,0.}}]},
ImageSize->400]
}
	
dwHelpCombineShapes[]:= {"Combine", "",
"Combine selected objects.
A dialog will open to set the spline resolution if a BezierCurve or BSplineCurve is used.
Text, images and compound objects are ignored.",
Grid[{
{Graphics[{EdgeForm[{GrayLevel[0]}], Opacity[0], Polygon[{{-0.3,-0.1},{0.3,-0.1},{0.3,0.1},{-0.3,0.1}}],Polygon[{{0.2,0.},{0.,0.2},{-0.2,0.},{0.,-0.2}}]}, ImageSize->100],
	Spacer[30],
Graphics[{EdgeForm[{GrayLevel[0]}], Opacity[0], Polygon[{{-0.1,0.1},{-0.3,0.1},{-0.3,-0.1},{-0.1,-0.1},{0.,-0.2},{0.1,-0.1},{0.3,-0.1},{0.3,0.1},{0.1,0.1},{0.,0.2}}]}, ImageSize->100]},
{Style["before",Italic],"",Style["after",Italic]}
}]
}
	
dwHelpIntersectShapes[]:= {"Intersect", "",
"Intersect selected objects.
A dialog will open to set the spline resolution if a BezierCurve or BSplineCurve is used.
Text, images and compound objects are ignored.",
Grid[{
{Graphics[{EdgeForm[{GrayLevel[0]}], Opacity[0], Polygon[{{-0.3,-0.1},{0.3,-0.1},{0.3,0.1},{-0.3,0.1}}],Polygon[{{0.2,0.},{0.,0.2},{-0.2,0.},{0.,-0.2}}]}, ImageSize->100],
	Spacer[30],
Graphics[{EdgeForm[{GrayLevel[0]}],Opacity[0],Polygon[{{-0.1,0.1},{-0.2,0},{-0.1,-0.1},{0.1,-0.1},{.2,0},{0.1,0.1}}]},ImageSize->100, PlotRange->{{-.3,.3},{-.2,.2}}, PlotRangePadding->.02]},
{Style["before",Italic],"",Style["after",Italic]}
}]
}
	
dwHelpBlendShapes[]:= {"Blend", "",
"Blend shape and style between a selected source object and a target object.",
Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8],
	FilledCurve[BezierCurve[{{-0.5,0.1},{-0.5,0.1},{-0.4,0.},{-0.4,0.},{-0.4,0.},{-0.5,-0.1},{-0.5,-0.1},{-0.5,-0.1},{-0.6,0.},{-0.6,0.},{-0.6,0.},{-0.5,0.1},{-0.5,0.1}}]],
	FilledCurve[BezierCurve[{{-0.225,0.1},{-0.21125,0.1},{-0.125,0.01375},{-0.125,0.},{-0.125,-0.01375},{-0.21125,-0.1},{-0.225,-0.1},{-0.23875,-0.1},{-0.325,-0.01375},{-0.325,0.},{-0.325,0.01375},{-0.23875,0.1},{-0.225,0.1}}]],
	FilledCurve[BezierCurve[{{0.05,0.1},{0.0775,0.1},{0.15,0.0275},{0.15,0.},{0.15,-0.0275},{0.0775,-0.1},{0.05,-0.1},{0.0225,-0.1},{-0.05,-0.0275},{-0.05,0.},{-0.05,0.0275},{0.0225,0.1},{0.05,0.1}}]],
	FilledCurve[BezierCurve[{{0.325,0.1},{0.36625,0.1},{0.425,0.04125},{0.425,0.},{0.425,-0.04125},{0.36625,-0.1},{0.325,-0.1},{0.28375,-0.1},{0.225,-0.04125},{0.225,0.},{0.225,0.04125},{0.28375,0.1},{0.325,0.1}}]],
	FilledCurve[BezierCurve[{{0.6,0.1},{0.655,0.1},{0.7,0.055},{0.7,0.},{0.7,-0.055},{0.655,-0.1},{0.6,-0.1},{0.545,-0.1},{0.5,-0.055},{0.5,0.},{0.5,0.055},{0.545,0.1},{0.6,0.1}}]]},
ImageSize->400],
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Blends are visible in preview mode only.
- Shapes and styles update when source or target are altered.
- Blend settings are found by clicking the Blend tool with the source object selected.
- Blend settings are also found in the 'BlendGradient' fill of the source object.
- Blends are not included in object and point counts.
- Blend resolution is global for all blends.
- Exposed blends are standalone objects released from their source."
}
	
dwHelpReflectH[]:= {"Reflect", "",
"Reflect selected objects horizontally. 
Command-click to reflect vertically.
Set the transform center in Preferences.",
Grid[{
{Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{0.1, 0.}, {0., -0.1}, {0., -0.05}, {-0.1, -0.05}, {-0.1, 0.05}, {0., 0.05}, {0., 0.1}}]}, ImageSize->100],
	Spacer[30],
Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{-0.1, 0.}, {0., -0.1}, {0., -0.05}, {0.1, -0.05}, {0.1, 0.05}, {0., 0.05}, {0., 0.1}}]}, ImageSize->100]},
{Style["before",Italic],"",Style["after",Italic]}
}]
}
	
dwHelpMirrorH[]:= {"Mirror", "",
"Mirror selected objects horizontally. 
Command-click to mirror vertically.
Mirrored points are added to the original points.
Points are always mirrored around the center of all selected objects.",
Grid[{
{Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{0.2, -0.1}, {-0.2, 0.2}, {-0.2, -0.1}}]}, ImageSize->{Automatic, 80}],
	Spacer[30],
Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{-0.2, -0.1}, {-0.2, 0.2},{0.2, -0.1}, {-1/3, -0.1}, {0.0667, 0.2},{0.0667, -0.1}}]}, ImageSize->{Automatic, 80}]},
{Style["before",Italic],"",Style["after",Italic]}
}]
}
	
dwHelpRoundCorners[]:= {"Round corners", "",
"Round corners of selected objects.
Set size of corner radius.",
Grid[{
{Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{-0.2, -0.1}, {0.2, -0.1}, {0.2, 0.1}, {-0.2, 0.1}}]}, ImageSize->100],
	Spacer[30],
Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], FilledCurve[BezierCurve[{{-0.2,-0.05},{-0.2,-0.075},{-0.175,-0.1},{-0.15,-0.1},{-0.125,-0.1},{0.125,-0.1},{0.15,-0.1},{0.175,-0.1},{0.2,-0.075},{0.2,-0.05},{0.2,-0.025},{0.2,0.025},{0.2,0.05},{0.2,0.075},{0.175,0.1},{0.15,0.1},{0.125,0.1},{-0.125,0.1},{-0.15,0.1},{-0.175,0.1},{-0.2,0.075},{-0.2,0.05}}]]}, 
	ImageSize->100]},
{Style["before",Italic],"",Style["after",Italic]}
}]
}
	
dwHelpOffsetLines[]:= {"Offset boundary", "",
"Offset boundary of selected objects.
Set distance, resolution and quantity.",
Grid[{
{Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{0.11755705045849463,-0.1618033988749895},{0.1902113032590307,0.06180339887498949},{0.,0.2},{-0.1902113032590307,0.06180339887498949},{-0.11755705045849463,-0.1618033988749895}}]}, ImageSize->80],
	Spacer[30],
Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{0.2225129094868334,0.09996890994328228},{0.19864728915170934,0.11747769523329403},{0.03580503562941504,0.2357895177734727},{0.02200066357176176,0.24489956349901593},{0.011231179236688822,0.24872228045723396},{-0.011231180719314292,0.2487222801154678},{-0.021968591033702002,0.24491526475480196},{-0.03579488066365642,0.23579689578801616},{-0.2188527449460851,0.10279757230094838},{-0.22859433488593564,0.09384622776271084},{-0.23397033324088606,0.08599321687858517},{-0.2401941691094013,0.06311226150485405},{-0.23846282572110153,0.04869650831725995},{-0.23145104359572216,0.02692286991161425},{-0.1675789227871056,-0.1696553047855306},{-0.16207764376313022,-0.18456118373650965},{-0.15619968551383534,-0.19353267173113534},{-0.14292682457109127,-0.20488906425775713},{-0.12022583336840822,-0.2117321240517828},{-0.09188520854572035,-0.21180339887491587},{0.09188520756025226,-0.21180339887497696},{0.12022583190425767,-0.2117321241300444},{0.14292682304958979,-0.20488906515365019},{0.15619968486070998,-0.1935326725265675},{0.16208272951126954,-0.1845512318462701},{0.167579411213475,-0.16965380156373444},{0.23145111422325898,0.0269230872807907},{0.23844957388473123,0.04864782032284355},{0.24019416910941518,0.06311226150432409},{0.23397033323341476,0.08599321689210068}}],Polygon[{{0.11755705045849463,-0.1618033988749895},{0.1902113032590307,0.06180339887498949},{0.,0.2},{-0.1902113032590307,0.06180339887498949},{-0.11755705045849463,-0.1618033988749895}}]}, 
	ImageSize->100]},
{Style["before",Italic],"",Style["after",Italic]}
}]
}
	
dwHelpShearShape[]:= {"Shear", "",
"Shear selected objects.
Set angle, direction and transform center.",
Grid[{
{Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{-0.2, -0.1}, {0.2, -0.1}, {0.2, 0.1}, {-0.2, 0.1}}]}, ImageSize->{Automatic, 50}],
	Spacer[30],
Graphics[{EdgeForm[{GrayLevel[0]}], GrayLevel[0.8], Polygon[{{-0.2577350269189626, -0.1}, {0.14226497308103742, -0.1},  {0.2577350269189626, 0.1}, {-0.14226497308103742, 0.1}}]}, 
	ImageSize->{Automatic, 50}]},
{Style["before",Italic],"",Style["after",Italic]}
}]
}
	
dwHelpDistortShape[]:= {"Distort", "",
"Distort points or position of selected objects.
Increase number of object points for smoother point distortion.
Drag red points of 'mesh' or use -1 to 1 values for a method below.",
"",
Grid[{{
	
	Column[{
		Style["Spherize", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.19,-0.19},{0.16,0.},{0.19,0.19},{0.,0.16},{-0.19,0.19},{-0.16,0.},{-0.19,-0.19},{0.,-0.16},{0.19,-0.19}}],Line[{{0.05,-0.05},{0.04,0.},{0.05,0.05},{0.,0.04},{-0.05,0.05},{-0.04,0.},{-0.05,-0.05},{0.,-0.04},{0.05,-0.05}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.28,-0.28},{0.32,0.},{0.28,0.28},{0.,0.32},{-0.28,0.28},{-0.32,0.},{-0.28,-0.28},{0.,-0.32},{0.28,-0.28}}],Line[{{0.19,-0.19},{0.2,0.},{0.19,0.19},{0.,0.2},{-0.19,0.19},{-0.2,0.},{-0.19,-0.19},{0.,-0.2},{0.19,-0.19}}]}}, ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}],
		"",
		Style["Horizontal", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], StrokeForm[Black], {Polygon[{{0.21, -0.35}, {0.21, 0.}, {0.21, 0.35}, {0., 0.35}, {-0.21, 0.35}, {-0.21, 0.}, {-0.21, -0.35}, {0., -0.35}, {0.21, -0.35}}],Line[{{0.1893, -0.24}, {0.1893, 0.}, {0.1893, 0.24}, {0., 0.24}, {-0.1893, 0.24}, {-0.1893, 0.}, {-0.1893, -0.24}, {0., -0.24}, {0.1893, -0.24}}],Line[{{0.1193, -0.12}, {0.1193, 0.}, {0.1193, 0.12}, {0., 0.12}, {-0.1193, 0.12}, {-0.1193, 0.}, {-0.1193, -0.12}, {0., -0.12}, {0.1193, -0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], StrokeForm[Black], {Polygon[{{0.49, -0.35}, {0.49, 0.}, {0.49, 0.35}, {0., 0.35}, {-0.49, 0.35}, {-0.49, 0.}, {-0.49, -0.35}, {0., -0.35}, {0.49, -0.35}}],Line[{{0.2907, -0.24}, {0.2907, 0.}, {0.2907, 0.24}, {0., 0.24}, {-0.2907, 0.24}, {-0.2907, 0.}, {-0.2907, -0.24}, {0., -0.24}, {0.2907, -0.24}}],Line[{{0.1207, -0.12}, {0.1207, 0.}, {0.1207, 0.12}, {0., 0.12}, {-0.1207, 0.12}, {-0.1207, 0.}, {-0.1207, -0.12}, {0., -0.12}, {0.1207, -0.12}}]}}, ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}],
		"",
		Style["Left", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{0.35,-0.35},{0.35,0},{0.35,0.35},{0.153125,0.35},{0.35,0.35},{0.35,0},{0.35,-0.35},{0.153125,-0.35},{0.35,-0.35}}],Polygon[{{0.257,-0.24},{0.257,0},{0.257,0.24},{0.153125,0.24},{0.23438214285714282,0.24},{0.2343821428571428,0},{0.2343821428571428,-0.24},{0.153125,-0.24},{0.25701071428571426,-0.24}}],Polygon[{{0.188,-0.12},{0.188,0},{0.188,0.12},{0.153125,0.12},{0.16444,0.12},{0.16443928571428568,0},{0.16444,-0.12},{0.153125,-0.12},{0.188,-0.12}}]},ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{0.35,-0.35},{0.35,0},{0.35,0.35},{-0.153125,0.35},{-1.05,0.35},{-1.05,0},{-1.05,-0.35},{-0.153125,-0.35},{0.35,-0.35}}],Polygon[{{0.223,-0.24},{0.223,0},{0.22298928571428567,0.24},{-0.153125,0.24},{-0.7143821428571427,0.24},{-0.7143821428571426,0},{-0.7143821428571427,-0.24},{-0.153125,-0.24},{0.223,-0.24}}],Polygon[{{0.0519,-0.12},{0.0519,0.},{0.0519,0.12},{-0.1531,0.12},{-0.4044,0.12},{-0.4044,0.},{-0.4044,-0.12},{-0.1531,-0.12},{0.0519,-0.12}}]},ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}],
		"",
		Style["Bottom", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{0.35,0.35},{0.35,0.1531},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.1531},{-0.35,0.35},{0.,0.35},{0.35,0.35}}], Polygon[{{0.24,0.2344},{0.24,0.1531},{0.24,0.257},{0.,0.257},{-0.24,0.257},{-0.24,0.1531},{-0.24,0.2344},{0.,0.2344},{0.24,0.2344}}], Polygon[{{0.12,0.1644},{0.12,0.1531},{0.12,0.1881},{0.,0.1881},{-0.12,0.1881},{-0.12,0.1531},{-0.12,0.1644},{0.,0.1644},{0.12,0.1644}}]},ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{0.35,-1.05},{0.35,-0.1531},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,-0.1531},{-0.35,-1.05},{0.,-1.05},{0.35,-1.05}}],Polygon[{{0.24,-0.7144},{0.24,-0.1531},{0.24,0.223},{0.,0.223},{-0.24,0.223},{-0.24,-0.1531},{-0.24,-0.7144},{0.,-0.7144},{0.24,-0.7144}}],Polygon[{{0.12,-0.4044},{0.12,-0.1531},{0.12,0.0519},{0.,0.0519},{-0.12,0.0519},{-0.12,-0.1531},{-0.12,-0.4044},{0.,-0.4044},{0.12,-0.4044}}]},ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}],
		"",
		Style["InnerTwist", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Polygon[{{0.1523,-0.2657},{0.1815,-0.1071},{0.2657,0.1523},{0.1071,0.1815},{-0.1523,0.2657},{-0.1815,0.1071},{-0.2657,-0.1523},{-0.1071,-0.1815},{0.1523,-0.2657}}],Polygon[{{-0.0328,-0.1648},{0.0588,-0.112},{0.1648,-0.0328},{0.112,0.0588},{0.0328,0.1648},{-0.0588,0.112},{-0.1648,0.0328},{-0.112,-0.0588},{-0.0328,-0.1648}}]},ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Polygon[{{0.2657,-0.1523},{0.1815,0.1071},{0.1523,0.2657},{-0.1071,0.1815},{-0.2657,0.1523},{-0.1815,-0.1071},{-0.1523,-0.2657},{0.1071,-0.1815},{0.2657,-0.1523}}],Polygon[{{0.1648,0.0328},{0.0588,0.112},{-0.0328,0.1648},{-0.112,0.0588},{-0.1648,-0.0328},{-0.0588,-0.112},{0.0328,-0.1648},{0.112,-0.0588},{0.1648,0.0328}}]},ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}]
	}],
	
	Spacer[30],
	
	Column[{
		Style["Corners", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], StrokeForm[Black], {Polygon[{{0., 0.}, {0.3937, 0.}, {0., 0.}, {0., 0.3937}, {0., 0.}, {-0.3937, 0.}, {0., 0.}, {0., -0.3937}, {0., 0.}}],Line[{{0.1888, -0.1888}, {0.2559, 0.}, {0.1888, 0.1888}, {0., 0.2559}, {-0.1888, 0.1888}, {-0.2559, 0.}, {-0.1888, -0.1888}, {0., -0.2559}, {0.1888, -0.1888}}],Line[{{0.12, -0.12}, {0.1202, 0.}, {0.12, 0.12}, {0., 0.1202}, {-0.12, 0.12}, {-0.1202, 0.}, {-0.12, -0.12}, {0., -0.1202}, {0.12, -0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], StrokeForm[Black], {Polygon[{{0.7, -0.7}, {0.3062, 0.}, {0.7, 0.7}, {0., 0.3062}, {-0.7, 0.7}, {-0.3062, 0.}, {-0.7, -0.7}, {0., -0.3062}, {0.7, -0.7}}],Line[{{0.2912, -0.2912}, {0.2241, 0.}, {0.2912, 0.2912}, {0., 0.2241}, {-0.2912, 0.2912}, {-0.2241, 0.}, {-0.2912, -0.2912}, {0., -0.2241}, {0.2912, -0.2912}}],Line[{{0.12, -0.12}, {0.1198, 0.}, {0.12, 0.12}, {0., 0.1198}, {-0.12, 0.12}, {-0.1198, 0.}, {-0.12, -0.12}, {0., -0.1198}, {0.12, -0.12}}]}}, ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}],
		"",
		Style["Vertical", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], StrokeForm[Black], {Polygon[{{0.35, -0.21}, {0.35, 0.}, {0.35, 0.21}, {0., 0.21}, {-0.35, 0.21}, {-0.35, 0.}, {-0.35, -0.21}, {0., -0.21}, {0.35, -0.21}}],Line[{{0.24, -0.1893}, {0.24, 0.}, {0.24, 0.1893}, {0., 0.1893}, {-0.24, 0.1893}, {-0.24, 0.}, {-0.24, -0.1893}, {0., -0.1893}, {0.24, -0.1893}}],Line[{{0.12, -0.1193}, {0.12, 0.}, {0.12, 0.1193}, {0., 0.1193}, {-0.12, 0.1193}, {-0.12, 0.}, {-0.12, -0.1193}, {0., -0.1193}, {0.12, -0.1193}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], StrokeForm[Black], {Polygon[{{0.35, -0.49}, {0.35, 0.}, {0.35, 0.49}, {0., 0.49}, {-0.35, 0.49}, {-0.35, 0.}, {-0.35, -0.49}, {0., -0.49}, {0.35, -0.49}}],Line[{{0.24, -0.2907}, {0.24, 0.}, {0.24, 0.2907}, {0., 0.2907}, {-0.24, 0.2907}, {-0.24, 0.}, {-0.24, -0.2907}, {0., -0.2907}, {0.24, -0.2907}}],Line[{{0.12, -0.1207}, {0.12, 0.}, {0.12, 0.1207}, {0., 0.1207}, {-0.12, 0.1207}, {-0.12, 0.}, {-0.12, -0.1207}, {0., -0.1207}, {0.12, -0.1207}}]}}, ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}],
		"",
		Style["Right", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{-0.35,-0.35},{-0.35,0.},{-0.35,0.35},{-0.1531,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{-0.1531,-0.35},{-0.35,-0.35}}],Polygon[{{-0.2344,-0.24},{-0.2344,0.},{-0.2344,0.24},{-0.1531,0.24},{-0.257,0.24},{-0.257,0.},{-0.257,-0.24},{-0.1531,-0.24},{-0.2344,-0.24}}],Polygon[{{-0.1644,-0.12},{-0.1644,0.},{-0.1644,0.12},{-0.1531,0.12},{-0.1881,0.12},{-0.1881,0.},{-0.1881,-0.12},{-0.1531,-0.12},{-0.1644,-0.12}}]},ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{1.05,-0.35},{1.05,0.},{1.05,0.35},{0.1531,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.1531,-0.35},{1.05,-0.35}}], Polygon[{{0.7144,-0.24},{0.7144,0.},{0.7144,0.24},{0.1531,0.24},{-0.223,0.24},{-0.223,0.},{-0.223,-0.24},{0.1531,-0.24},{0.7144,-0.24}}], Polygon[{{0.4044,-0.12},{0.4044,0.},{0.4044,0.12},{0.1531,0.12},{-0.0519,0.12},{-0.0519,0.},{-0.0519,-0.12},{0.1531,-0.12},{0.4044,-0.12}}]},ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}],
		"",
		Style["Top", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{0.35,-0.35},{0.35,-0.1531},{0.35,-0.35},{0.,-0.35},{-0.35,-0.35},{-0.35,-0.1531},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Polygon[{{0.24,-0.257},{0.24,-0.1531},{0.24,-0.2344},{0.,-0.2344},{-0.24,-0.2344},{-0.24,-0.1531},{-0.24,-0.257},{0.,-0.257},{0.24,-0.257}}],Polygon[{{0.12,-0.1881},{0.12,-0.1531},{0.12,-0.1644},{0.,-0.1644},{-0.12,-0.1644},{-0.12,-0.1531},{-0.12,-0.1881},{0.,-0.1881},{0.12,-0.1881}}]},ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{0.35,-0.35},{0.35,0.1531},{0.35,1.05},{0.,1.05},{-0.35,1.05},{-0.35,0.1531},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Polygon[{{0.24,-0.223},{0.24,0.1531},{0.24,0.7144},{0.,0.7144},{-0.24,0.7144},{-0.24,0.1531},{-0.24,-0.223},{0.,-0.223},{0.24,-0.223}}],Polygon[{{0.12,-0.0519},{0.12,0.1531},{0.12,0.4044},{0.,0.4044},{-0.12,0.4044},{-0.12,0.1531},{-0.12,-0.0519},{0.,-0.0519},{0.12,-0.0519}}]},ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}],
		"",
		Style["OuterTwist", Sequence@@$dwHelpSubheadStyle],
		Grid[{
			{
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{-0.1054,-0.4836},{0.1891,-0.2945},{0.4836,-0.1054},{0.2945,0.1891},{0.1054,0.4836},{-0.1891,0.2945},{-0.4836,0.1054},{-0.2945,-0.1891},{-0.1054,-0.4836}}],Polygon[{{0.0155,-0.3059},{0.1882,-0.0948},{0.3059,0.0155},{0.0948,0.1882},{-0.0155,0.3059},{-0.1882,0.0948},{-0.3059,-0.0155},{-0.0948,-0.1882},{0.0155,-0.3059}}],Polygon[{{0.1167,-0.121},{0.126,0.011},{0.121,0.1167},{-0.011,0.126},{-0.1167,0.121},{-0.126,-0.011},{-0.121,-0.1167},{0.011,-0.126},{0.1167,-0.121}}]},ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.35,-0.35},{0.35,0.},{0.35,0.35},{0.,0.35},{-0.35,0.35},{-0.35,0.},{-0.35,-0.35},{0.,-0.35},{0.35,-0.35}}],Black,Line[{{0.24,-0.24},{0.24,0.},{0.24,0.24},{0.,0.24},{-0.24,0.24},{-0.24,0.},{-0.24,-0.24},{0.,-0.24},{0.24,-0.24}}],Line[{{0.12,-0.12},{0.12,0.},{0.12,0.12},{0.,0.12},{-0.12,0.12},{-0.12,0.},{-0.12,-0.12},{0.,-0.12},{0.12,-0.12}}]}}, ImageSize->{50,50}],
				Graphics[{EdgeForm[{Black}],GrayLevel[0.8],Polygon[{{0.4836,0.1054},{0.1891,0.2945},{-0.1054,0.4836},{-0.2945,0.1891},{-0.4836,-0.1054},{-0.1891,-0.2945},{0.1054,-0.4836},{0.2945,-0.1891},{0.4836,0.1054}}],Polygon[{{0.3059,-0.0155},{0.1882,0.0948},{0.0155,0.3059},{-0.0948,0.1882},{-0.3059,0.0155},{-0.1882,-0.0948},{-0.0155,-0.3059},{0.0948,-0.1882},{0.3059,-0.0155}}],Polygon[{{0.121,-0.1167},{0.126,-0.011},{0.1167,0.121},{0.011,0.126},{-0.121,0.1167},{-0.126,0.011},{-0.1167,-0.121},{-0.011,-0.126},{0.121,-0.1167}}]},ImageSize->{50,50}]
			},
			{Style["-1",Italic],Style["0",Italic],Style["1",Italic]}
		}]
	}]
	
}}]
}
	
dwHelpAlign[]:= {"Align", "",
"Align selected objects. 
Choose alignment from popup menu.",
"",
Grid[{{
	
	Column[{
		Style["Center", Sequence@@$dwHelpSubheadStyle],
		Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{-0.2, 0.2}, {0.2, 0.2}, {0.2, -0.2}, {-0.2, -0.2}}], Polygon[{{-0.132, 0.132}, {0.132, 0.132}, {0.132, -0.132}, {-0.132, -0.132}}], Polygon[{{-0.066, 0.066}, {0.066, 0.066}, {0.066, -0.066}, {-0.066, -0.066}}]}}, ImageSize->{Automatic,60}],
		"",
		Style["Left", Sequence@@$dwHelpSubheadStyle],
		Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{-0.2, 0.2}, {0.2, 0.2}, {0.2, -0.2}, {-0.2, -0.2}}], Polygon[{{-0.2, 0.132}, {0.064, 0.132}, {0.064, -0.132}, {-0.2, -0.132}}], Polygon[{{-0.2, 0.066}, {-0.068, 0.066}, {-0.068, -0.066}, {-0.2, -0.066}}]}}, ImageSize->{Automatic,60}],
		"",
		Style["Right", Sequence@@$dwHelpSubheadStyle],
		Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{-0.468, 0.2}, {-0.068, 0.2}, {-0.068, -0.2}, {-0.468, -0.2}}], Polygon[{{-0.332, 0.132}, {-0.068, 0.132}, {-0.068, -0.132}, {-0.332, -0.132}}], Polygon[{{-0.2, 0.066}, {-0.068, 0.066}, {-0.068, -0.066}, {-0.2, -0.066}}]}}, ImageSize->{Automatic,60}],
		"",
		Style["Bottom", Sequence@@$dwHelpSubheadStyle],
		Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{-0.401, 0.133}, {-0.001, 0.133}, {-0.001, -0.267}, {-0.401, -0.267}}], Polygon[{{-0.333, -0.003}, {-0.069, -0.003}, {-0.069, -0.267}, {-0.333, -0.267}}], Polygon[{{-0.267, -0.135}, {-0.135, -0.135}, {-0.135, -0.267}, {-0.267, -0.267}}]}}, ImageSize->{Automatic,60}],
		"",
		Style["Top", Sequence@@$dwHelpSubheadStyle],
		Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{-0.401, 0.133}, {-0.001, 0.133}, {-0.001, -0.267}, {-0.401, -0.267}}], Polygon[{{-0.333, 0.133}, {-0.069, 0.133}, {-0.069, -0.131}, {-0.333, -0.131}}], Polygon[{{-0.267, 0.133}, {-0.135, 0.133}, {-0.135, 0.001}, {-0.267, 0.001}}]}}, ImageSize->{Automatic,60}]
	}],
	
	Spacer[30],
	
	Column[{
		Style["Horizontal center", Sequence@@$dwHelpSubheadStyle],
		Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{0.225, 0.175}, {0.225, -0.225}, {-0.175, -0.225}, {-0.175, 0.175}}], Polygon[{{0.025, 0.125}, {0.025, -0.175}, {-0.275, -0.175}, {-0.275, 0.125}}], Polygon[{{0.275, 0.075}, {0.275, -0.125}, {0.075, -0.125}, {0.075, 0.075}}]}}, ImageSize->{Automatic,60}],
		"",
		Style["Vertical center", Sequence@@$dwHelpSubheadStyle],
		Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{-0.4, 0.2}, {0.4, 0.2}, {0.4, -0.2}, {-0.4, -0.2}}], Polygon[{{-0.3, 0.}, {0.3, 0.}, {0.3, -0.3}, {-0.3, -0.3}}], Polygon[{{-0.2, 0.25}, {0.2, 0.25}, {0.2, 0.05}, {-0.2, 0.05}}]}}, ImageSize->{Automatic,60}],
		"",
		Style["Balance horizontal space", Sequence@@$dwHelpSubheadStyle],
		Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{-0.2, 0.2}, {0., 0.2}, {0., -0.2}, {-0.2, -0.2}}], Polygon[{{0.1, 0.15}, {0.3, 0.15}, {0.3, -0.15}, {0.1, -0.15}}], Polygon[{{0.4, 0.1}, {0.6, 0.1}, {0.6, -0.1}, {0.4, -0.1}}]}}, ImageSize->{Automatic,60}],
		"",
		Style["Balance vertical space", Sequence@@$dwHelpSubheadStyle],
		Graphics[{EdgeForm[{Black}], GrayLevel[0.8], {Polygon[{{-0.4, 0.1}, {0.4, 0.1}, {0.4, 0.}, {-0.4, 0.}}], Polygon[{{-0.3, 0.25}, {0.3, 0.25}, {0.3, 0.15}, {-0.3, 0.15}}], Polygon[{{-0.2, 0.4}, {0.2, 0.4}, {0.2, 0.3}, {-0.2, 0.3}}]}}, ImageSize->{Automatic,60}],
		"",
		Style["Align grid...", Sequence@@$dwHelpSubheadStyle],
		Grid[{
	      {Button[
	          Rotate[Graphics[{Gray, 
	             Polygon[{{.25, .2}, {.75, .2}, {.75, .7}, {.25, .7}}], 
	             Black, Line[{{0, .1}, {1, .1}}]
	             }, ImageSize -> 18], -Pi/2], 
	          Null],
	         Button[
	          Rotate[Graphics[{Gray, 
	             Polygon[{{.25, .2}, {.75, .2}, {.75, .7}, {.25, .7}}], 
	             Black, Line[{{0, .1}, {1, .1}}]
	             }, ImageSize -> 18], Pi/2], 
	          Null],
	         Button[
	          Rotate[Graphics[{Gray, 
	             Polygon[{{.25, .2}, {.75, .2}, {.75, .7}, {.25, .7}}], 
	             Black, Line[{{0, .1}, {1, .1}}]
	             }, ImageSize -> 18], Pi], Null],
	         Button[
	          Graphics[{Gray, 
	            Polygon[{{.25, .2}, {.75, .2}, {.75, .7}, {.25, .7}}], 
	            Black, Line[{{0, .1}, {1, .1}}]
	            }, ImageSize -> 18], Null]
	         },
	      {Button[
	          Rotate[Graphics[{Gray, 
	             Polygon[{{.4, .1}, {.9, .1}, {.9, .6}, {.4, .6}}], Black,
	              Line[{{0, 0}, {1, 0}, {1, 1}}]
	             }, ImageSize -> 18], Pi], 
	          Null],
	         Button[
	          Rotate[Graphics[{Gray, 
	             Polygon[{{.4, .1}, {.9, .1}, {.9, .6}, {.4, .6}}], Black,
	              Line[{{0, 0}, {1, 0}, {1, 1}}]
	             }, ImageSize -> 18], Pi/2], 
	          Null],
	         Button[
	          Rotate[Graphics[{Gray, 
	             Polygon[{{.4, .1}, {.9, .1}, {.9, .6}, {.4, .6}}], Black,
	              Line[{{0, 0}, {1, 0}, {1, 1}}]
	             }, ImageSize -> 18], -Pi/2], 
	          Null], 
	         Button[Graphics[{Gray, 
	            Polygon[{{.4, .1}, {.9, .1}, {.9, .6}, {.4, .6}}], Black, 
	            Line[{{0, 0}, {1, 0}, {1, 1}}]
	            }, ImageSize -> 18], Null]
	         }
	     }, Spacings -> {0, 0}]
	}]
	
}}, Alignment->Top]
}
	
dwHelpSnapPts[]:= {"Snap objects or points to grid", "",
"Snap selected objects to grid.
Snap selected points to grid when using point selection tool.

Alternating gridlines are displayed to reduce screen clutter so points and objects 
will snap to grid line intersections and half way between each grid line.",
Graphics[{LightGray, Line[{{-0.4, 0.3}, {-0.4, -0.3}}], 
     Line[{{-0.2, 0.3}, {-0.2, -0.3}}], 
     Line[{{0., 0.3}, {0., -0.3}}], 
     Line[{{0.2, 0.3}, {0.2, -0.3}}], 
     Line[{{-0.4, -0.3}, {0.2, -0.3}}], 
     Line[{{-0.4, -0.1}, {0.2, -0.1}}], 
     Line[{{-0.4, 0.1}, {0.2, 0.1}}], 
     Line[{{-0.4, 0.3}, {0.2, 0.3}}], 
    Hue[.6], 
  Line[{{-0.4, 0.1}, {-0.2, 0.1}, {-0.1, 0.}, {0., 0.}, {0.1, 0.1}}], 
  PointSize[Medium], 
  Point[{{-0.4, 0.1}, {-0.2, 0.1}, {-0.1, 0.}, {0., 0.}, {0.1, 0.1}}]}, ImageSize -> 100],
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Mouse clicks snap to grid when drawing.
- Mouse dragging moves objects or points a grid unit.
- Grid settings are in the '2D/3D grid' popup menu:
	Choose a grid size or turn it off.
	Each size is twice the distance of the previous size. 
	(i.e, 'Dense' is twice the size of 'Tiny')"
}
	
dwHelpCenterObject[]:= {"Center on canvas", "",
"Center selected objects on canvas.

If transform 'each object' is checked in Preferences, each object 
will center on canvas even if mulitple objects are selected. 
Otherwise all selected objects will be centered as a whole."
}
	
dwHelpObjectShadow[]:= {"Object shadow", "",
"Create a shadow for each selected object.

All objects generate Image shadows.
BezierCurve, BSplineCurve and Polygon objects can also generate Polygon shadows.
Multiple object selections will generate either all Polygon or all Image shadows.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Polygon shadows may not be possible for some complex shapes.
- Polygon shadows may require manual adjustment."
}

End[] (* End Private Context *)

EndPackage[]