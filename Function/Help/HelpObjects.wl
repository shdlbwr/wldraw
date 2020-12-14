(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
		
dwHelpObjects[]:=
	{
		{$dwIconAddPoint, dwHelpDraw[]},
		{$dwIconText, dwHelpText[]},
		{$dwIcon3D, dwHelp3D[]},
		{$dwIconImage, dwHelpImage[]},
		{$dwIconSpiral/.{(ImageSize->_)->(ImageSize->24), Gray->$dwToolColor}, dwHelpPresetObjects[]},
		{$dwIconShapeArray/.{(ImageSize->_)->(ImageSize->24)}, dwHelpPatterns[]},
		{$dwIconCoilSpring, dwHelpPresetLibrary[]},
		{$dwIconMap, dwHelpPresetMaps[]}
	}
	
dwHelpPatterns[]:= {"Preset patterns", "",
Row[{Spacer[{0,12}], "Choose a preset pattern from the popup menu."}],
"",
Style["Selected object array", Sequence@@$dwHelpSubheadStyle],
Row[{$dwIconShapeArray/.{GrayLevel[.7]->GrayLevel[.3]},
	Grid[{
		{" Select an object before choosing array preset or use a default shape."},
		{" Text and Images are ignored."},
		{Row[{" Examples:",Spacer[5],
			Button[Style["Fifty star array", 10, $dwHelpExampleColor], $dwHelpTopic = {$dwIconHelpExample, dwStarArray[]}, Appearance->None],
			Spacer[5],Button[Style["Spiral array", 10, $dwHelpExampleColor], $dwHelpTopic = {$dwIconHelpExample, dwSpiralArray[]}, Appearance->None]
		}]}
	}, Alignment->Left]
}],
"",
Style["Patterns", Sequence@@$dwHelpSubheadStyle],
Multicolumn[Row[#]&/@Partition[
{
	$dwIconShapeHilbertCurve/.{$dwToolColor -> GrayLevel[.3]}, " Hilbert curve",
	$dwIconShapePeanoCurve/.{$dwToolColor -> GrayLevel[.3]}, " Peano curve",
	$dwIconShapeSierpinskiCurve/.{$dwToolColor -> GrayLevel[.3]}, " Sierpinski curve",
	$dwIconShapeKochCurve/.{$dwToolColor -> GrayLevel[.3]}, " Koch curve",
	ImageAdd[ImageMultiply[$dwIconShapeBarCode, GrayLevel[.2]], .5], " Barcode",
	$dwIconShapeGuilloche/.{GrayLevel[.6] -> GrayLevel[.3]}, " Guilloche pattern",
	$dwIconShapeTextDots/.{GrayLevel[.6] -> GrayLevel[.5]}, "Dot text",
	$dwIconShapeConcentricCircles, " Concentric circles",
	$dwIconTicks, " Ticks"
}(*/.{GrayLevel[.7]->Gray, GrayLevel[1,1]->GrayLevel[.8]}*), 2,2,1,{}], 3, Spacings->{2,1}, Alignment->Left],
"",
Style["Fractal Trees", Sequence@@$dwHelpSubheadStyle],
Grid[{
	{$dwIconShapeTreeCurve/.{GrayLevel[.6] -> GrayLevel[.3]}, " Create a geometric tree."},
	{$dwIconShapeTree, " Create an organic tree."}
}, Alignment->Left],
"",
Style["Graph", Sequence@@$dwHelpSubheadStyle],
Grid[{
	{$dwIconShapeGraph/.{GrayLevel[.6] -> GrayLevel[.4]}, " Adjust the vertex quantity and layout to create a variety of generic graphs."}
}],
Style["Random", Sequence@@$dwHelpSubheadStyle],
Grid[{
	{$dwIconShapeRandomPoints/.{GrayLevel[.6] -> GrayLevel[.4]}, " Fill selected shape with random points."}
}],
Style["Roughen", Sequence@@$dwHelpSubheadStyle],
Grid[{
	{$dwIconShapeRoughenSelection/.{Gray -> GrayLevel[.3]}, " Roughen selected object."}
}],
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Progress of generating large patterns is displayed in message bar at bottom of canvas."
}
	
dwHelpPresetObjects[]:= {"Preset objects", "",
Row[{Spacer[{0,12}], "Choose a preset object from the popup menu."}],
"",
Style["Objects", Sequence@@$dwHelpSubheadStyle],
Multicolumn[Row[#]&/@Partition[
{
	$dwIconBox," Square",
	$dwIconTextBox," Rectangle grouped with text",
	$dwIconDisk," BezierCurve disk"
}/.{GrayLevel[.7]->Gray}, 2,2,1,{}], 3, Spacings->{2,0}, Alignment->Left],
" ",
Style["Objects with dialog settings", Sequence@@$dwHelpSubheadStyle],
Multicolumn[Row[#]&/@Partition[
{
	$dwIconRoundedBox, " Rounded rectangle",
	$dwIconCapsule, " Capsule",
	$dwIconTorn, " Torn",
	$dwIconNSidedPolygon, " N-sided polygon",
	$dwIconShapeCloud, " Cloud",
	$dwIconPolygonArrow, " Straight arrow",
	$dwIconPolygonMultiArrow, " Split arrow",
	$dwIconPolygonBentArrow, " Bent arrow",
	$dwIconPolygonSwoopArrow, " Swoop arrow",
	$dwIconAngleArrow, " Rotation arrow",
	$dwIconDimensionArrow, " Dimension arrow",
	$dwIconSpiral, " Spiral",
	$dwIconSineWave, " Line wave",
	Style[Show[$dwIconAxes/.{Arrow->Line,Gray->GrayLevel[.4],LightGray->GrayLevel[.65]},ImageSize->{36,28}],Antialiasing->False], " Labeled axes",
	Row[{$dwIconShapeFire, Spacer[4]}], " Fire"
	}/.{GrayLevel[.7]->Gray, GrayLevel[1,1]->GrayLevel[.8]}, 2,2,1,{}], 3, Spacings->{2,0}, Alignment->Left],
" ",
Style["Objects that position by mouse clicks", Sequence@@$dwHelpSubheadStyle],
"Choose 'Show position markers' from grid menu.",
"Click 2 canvas positions before choosing preset:  click1-start  click2-end",
Row[{
	Spacer[{20,20}],
	Multicolumn[Row[#]&/@Partition[
	{
		$dwIconPolygonArrow, " Straight arrow",
		$dwIconPolygonSwoopArrow, " Swoop arrow",
		$dwIconDimensionArrow, " Dimension arrow",
		Row[{"Example: ",Button[Style["Spring", 10, $dwHelpExampleColor], $dwHelpTopic = {$dwIconHelpExample, dwExampleSpring[]}, Appearance->None]}]
	}/.{Gray->GrayLevel[.4], White->GrayLevel[.7], GrayLevel[.7]->Gray, GrayLevel[1,1]->GrayLevel[.8]}, 2,2,1,{}], 3, Spacings->{2,0}, Alignment->Left]
}],
Row[{Spacer[{0,20}],"Click 3 canvas positions before choosing preset:  click1-origin  click2-start  click3-end"}],
Row[{
	Spacer[{20,20}],
	Multicolumn[Row[#]&/@Partition[
	{
		Row[{$dwIconAngleArrow," Rotation arrow        Example: ",Button[Style["Rotation arrow", 10, $dwHelpExampleColor], $dwHelpTopic = {$dwIconHelpExample, dwExampleAngleArrow[]}, Appearance->None]}]
	}/.{Gray->GrayLevel[.4], White->GrayLevel[.7]}, 2,2,1,{}], 3, Spacings->{2,0}, Alignment->Left]
}],
"Markers disappear when object is created. Objects are not selectable with visible markers.
Unlimited marker clicks are possible but only the last three clicks are marked and used.
Red marker is only used for objects supporting three markers, such as the rotation arrow."
}
	
dwHelpPresetLibrary[]:= {"Preset library", "",
Row[{Spacer[{0,12}], "Choose a preset library object from the popup menu."}],
"",
Style["Objects with dialog settings", Sequence@@$dwHelpSubheadStyle],
Multicolumn[Row[#]&/@Partition[
{
	$dwIconCoilSpring," Spring",
	$dwIconGear, " Gear",
	$dwIconShapeResistor, " Resistor",
	$dwIconShapeRheostat, " Rheostat",
	$dwIconShapeGround, " Ground",
	$dwIconShapeInductor, " Inductor",
	$dwIconShapeInductorCoil, " Inductor coil",
	$dwIconShapeCurrentSource, " Current source",
	$dwIconShapeVoltageSource, " Voltage source",
	$dwIconShapeACVoltageSource, " AC voltage source",
	$dwIconShapePivot, " Pivot",
	$dwIconShapeCapacitor, " Capacitor",
	$dwIconShapeNode, " Node",
	$dwIconShapeOpenNode, " Open node"
	}/.{Gray->GrayLevel[.3], GrayLevel[1,1]->GrayLevel[.8]}, 2,2,1,{}], 3, Spacings->{2,0}, Alignment->Left],
" ",
Style["Objects that position by mouse clicks", Sequence@@$dwHelpSubheadStyle],
"Choose 'Show position markers' from grid menu.",
"Click 2 canvas positions before choosing preset:  click1-start  click2-end",
Row[{
	Spacer[{20,20}],
	Multicolumn[Row[#]&/@Partition[
	{
		Row[{$dwIconCoilSpring/.{Gray->GrayLevel[.3]}, " Spring                  Example: ",Button[Style["Spring", 10, $dwHelpExampleColor], $dwHelpTopic = {$dwIconHelpExample, dwExampleSpring[]}, Appearance->None]}]
	}/.{GrayLevel[.7]->Gray, GrayLevel[1,1]->GrayLevel[.8]}, 2,2,1,{}], 3, Spacings->{2,0}, Alignment->Left]
}],
"Markers disappear when object is created. Objects are not selectable with visible markers.
Unlimited marker clicks are possible but only the last three clicks are marked and used.
Red marker is only used for objects supporting three markers, such as the rotation arrow."
}
	
dwHelpCityBuilder[]:= {"City builder", "",
"Build random cities with houses, high-rises, streets and trees.
Export settings to keep a library of various cities.
Set the view with 'City View' popup menu.",
"",
Style["Start building...", Sequence@@$dwHelpSubheadStyle],
"Layout and subdivide the buildings. Set the minimum and maximum heights.
Adjust the yard size, trees and border. Create dramatic shading by adjusting contrast. 
Uncheck boxes to remove objects from scene. Object totals are provided for comparison.",
"",
Style["Layout footprint", Sequence@@$dwHelpSubheadStyle],
"Set the overall layout and building footprint. Uniform and non-uniform presets are 
provided for cubic blocks. Isolate the city by increasing the border or pad settings.",
"",
Style["Buildings", Sequence@@$dwHelpSubheadStyle],
"Choose a building pattern from the popup menu for overall height pattern.
Set the footprint size in either direction and space between divided buildings. 
Randomize shapes by X, Y or height. Sloped roofs can be set from 0 for no roofs 
to 1 for roofs on all buildings.",
"",
Style["Streets", Sequence@@$dwHelpSubheadStyle],
"Set the street style and width. Move the removal value left to retain streets.",
"",
Style["Environment", Sequence@@$dwHelpSubheadStyle],
"Color the ground or set soil depth. Remove item by setting its value to 0.",
"",
Style["City presets", Sequence@@$dwHelpSubheadStyle],
"City presets can affect all elements of the scene. This includes the style and layout
of buildings and the environment.",
"",
Style["Building color", Sequence@@$dwHelpSubheadStyle],
"Style all buildings with 1 color or up to 5 colors. 'Color buildings by roof' uses first color 
for flat roof buildings and the current number of colors for sloped roof buildings or the inverse.
'5 Scene Colors' offers a dramatic effect that styles all elements of the scene with 5 colors.
Move the position of the palette colors to the left or right by clicking the arrow on either side.
Change a color of the palette by clicking the color setter.",
"",
Style["Expand cities", Sequence@@$dwHelpSubheadStyle],
"Create large cities by choosing the amount of city blocks from the 'Expand' popup menu.
The dialog remains open after the city appears to allow setting experiments."
}
	
dwHelpText[]:= {"Text", "",
"Add text to canvas.

Double click text to open dialog to change text.

CONVERT TEXT TO GRAPHICS button replaces text with compound shapes.
NOTE: Convert words not paragraphs.",
"",
Style["Text on a BSplineCurve", Sequence@@$dwHelpSubheadStyle],
"- Create a BSplineCurve with a minimum of 4 points.
- Click text tool then double-click the new text to open dialog.
- Select the curve from popup menu.
- Adjust text spacing and curve starting point then close dialog.
- Text alignment Top, Bottom and Center control placement relative to BSplineCurve.",
Row[{Spacer[5], dwTextOnCurveExampleArt[]}],
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Text should be a \"string\" to avoid conflicts with variable names.
- Use \\ with special characters. ( \\n for new line,  \\\" for quotes, etc. )
- Font size does not scale with graphics.
- STYLESHEET FORMAT
   Stylesheet formats are not diplayed within WLDraw since they vary by notebook.
   Stylesheet formats will retain font size, color, opacity, slant, tracking and weight.
   Mathematica documentation stylesheet formats:
      'NoStyle' - no style wrapper
      'TI' - variable name
      'Notes' - standard text of Details section
      'TableText' - table text
      'ExampleText' - example text
      'InlineFormula' - function name"
}
	
dwHelpDraw[]:= {"Draw path", "",
"Start a new object by clicking the Draw path button then click positions on the canvas.

The style settings will appear above the canvas when the Draw button is clicked.

Six object types are available and may be changed at any time by choosing 
a new type from the popup menu in the style section at top.",
"",
PopupMenu["BezierCurve", 
		{"BezierCurve","BSplineCurve","Polygon","Line","Arrow","Point"}, Appearance->"Palette", Alignment->Center, ImageSize->{142, 24}],
"",
"BezierCurve is provided for smooth curves which are created by dragging on the canvas. 
Switching back and forth from BezierCurve to another type may not automatically produce 
the expected shape since the curved line between points is based on multiple points.

The figure below displays the six object types, each created with the same five points.",
Graphics[{StrokeForm[Black],FaceForm[GrayLevel[.8]],AbsolutePointSize[4],Arrowheads[Medium],
Point[{{-1.1324429495415054, -0.1618033988749895}, {-1.0597886967409693, 0.06180339887498949}, {-1.25, 0.2}, 
     {-1.4402113032590307, 0.06180339887498949}, {-1.3675570504584946, -0.1618033988749895}}], 
   Arrow[{{-0.6324429495415054, -0.1618033988749895}, {-0.5597886967409693, 0.06180339887498949}, {-0.75, 0.2}, 
     {-0.9402113032590307, 0.06180339887498949}, {-0.8675570504584946, -0.1618033988749895}}], 
   Line[{{-0.13244294954150543, -0.1618033988749895}, {-0.05978869674096937, 0.06180339887498949}, {-0.25, 0.2}, 
     {-0.44021130325903074, 0.06180339887498949}, {-0.3675570504584947, -0.1618033988749895}}], 
   Polygon[{{0.36755705045849457, -0.1618033988749895}, {0.44021130325903063, 0.06180339887498949}, {0.25, 0.2}, 
     {0.05978869674096926, 0.06180339887498949}, {0.13244294954150532, -0.1618033988749895}}], 
   FilledCurve[BSplineCurve[{{0.8675570504584946, -0.1618033988749895}, {0.9402113032590307, 0.06180339887498949}, {0.75, 0.2}, 
      {0.5597886967409693, 0.06180339887498949}, {0.6324429495415054, -0.1618033988749895}}, SplineClosed -> True, SplineDegree -> 3]], 
   FilledCurve[BezierCurve[{{1.3675570504584946, -0.1618033988749895}, {1.4402113032590307, 0.06180339887498949}, {1.25, 0.2}, 
      {1.0597886967409693, 0.06180339887498949}}]], 
   Inset[Rotate[Style["Point", FontFamily -> "Source Sans Pro", FontSize -> 10], 0.], {-1.25, -0.3}, {0, 0}], 
   Inset[Rotate[Style["Arrow", FontFamily -> "Source Sans Pro", FontSize -> 10], 0.], {-0.75, -0.3}, {0, 0}], 
   Inset[Rotate[Style["Line", FontFamily -> "Source Sans Pro", FontSize -> 10], 0.], {-0.25, -0.3}, {0, 0}], 
   Inset[Rotate[Style["Polygon", FontFamily -> "Source Sans Pro", FontSize -> 10], 0.], {0.25, -0.3}, {0, 0}], 
   Inset[Rotate[Style["BSplineCurve", FontFamily -> "Source Sans Pro", FontSize -> 10], 0.], {0.75, -0.3}, {0, 0}], 
   Inset[Rotate[Style["BezierCurve", FontFamily -> "Source Sans Pro", FontSize -> 10], 0.], {1.25, -0.3}, {0, 0}]},ImageSize->400],
   "",
   Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Click the Draw tool to start a new object.
- Click canvas to add points for all object types except BezierCurve which requires dragging.
- Press ShiftKey while dragging/clicking to constrain movement.
- Press "<>StringTake[$dwOptionKey, {1,-4}]<>" while dragging BezierCurve handle for corner point.
- Use wireframe display for more responsive interface when creating complex illustrations.",
Row[{"- Toggle between preview and wireframe display by clicking ", Button[$dwIconPreview, Null, $dwButtonStyle, FrameMargins->0],"."}]
}
	
dwHelp3D[]:= {"3D", "",
"A dialog will appear to set the 3D view if no object is selected.
If an object is selected a dialog appears to set the object's axis projection and rotation.
NOTE: Objects are distorted to appear 3D but always remain 2D objects.
",
Style["Setting 3D view", Sequence@@$dwHelpSubheadStyle],
"- Set the 3D view by clicking the 3D tool with no objects selected. The default is isometric.
- Set the 3D view before beginning an illustration because changing the view will not 
   convert any existing objects to the new view.
",
Style["Draw in 3D", Sequence@@$dwHelpSubheadStyle],
"- Choose '3D grid' from the grid popup menu.
- Click the Draw path button then click positions on the canvas.
- Object points will snap to the 3D grid when drawn.
- Objects cannot be rotated in 3D space.
- Scale and rotation of object selection box behave the same as 2D.
",
Style["Convert 2D object to 3D", Sequence@@$dwHelpSubheadStyle],
"- Select a 2D object then click 3D tool to open dialog.
- Choose an axis projection by clicking x, y or z.
- Set the object rotation by using the x, y, and z sliders. Click 'OK'.
- The object cannot be rotated in 3D space after conversion.
",
Style["Moving objects along 3D grid", Sequence@@$dwHelpSubheadStyle],
"- Choose '3D grid' from the grid popup menu.
- Drag object to snap its location to the 3D grid.
",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Create a scene combining objects using the same 3D view since no perspective is used. 
- Use the nudge selection tool for moving objects by grid units in a single axis direction.
- The back side of a texture cannot display when rotating in 3D space so pinking will occur.
",
Style["EXAMPLES", Sequence@@$dwHelpSubheadStyle],
Row[{
	Button[Style["3D box by projection", $dwHelpExampleColor], $dwHelpTopic = {$dwIconHelpExample, dwAxoBoxProjection[]}, Appearance->None],
	Spacer[20],
	Button[Style["3D box by extrusion", $dwHelpExampleColor], $dwHelpTopic = {$dwIconHelpExample, dwAxoBoxExtrude[]}, Appearance->None]
}]
}
	
dwHelpImage[]:= {"Image", "",
"Click the Image tool to add the default image to the canvas.
 
Replace default by double clicking image to open dialog then pasting a new image. 
Crop by using the 'left', 'right', 'bottom' or 'top' sliders.
Resize by clicking one of the percentage buttons.

Click CAMERA IMAGE to insert a camera image if a camera is available.
Click CANVAS IMAGE to insert a canvas screenshot.

Rotate and scale an image by dragging its selection box, same as graphics. 
However images scale the width and height equally unlike graphics which 
supports individual scaling.

Convert selected graphics to an image by clicking 'CREATE IMAGE' in 
image part of style section.
",
Style["Image filters", Sequence@@$dwHelpSubheadStyle],
Row[{"Apply up to ", ToString[$dwImageMultipleFilterMax], " filters to an image. 'None' is added by default. Click 'None' then choose"}],
"a new filter from the popup menu. Add a new filter by clicking the large '+' button or 
remove with the 'x' button. Choose 'Flatten filters' to apply and remove all filters.",
Show[Import[$dwFileDirectory<>"Help/imagefilters.png"], ImageSize->450],
Style["Row of image filters", Italic],
"",
"A few examples of the available filters applied to 'Original image':",
Style[Grid[
	Riffle[
	Partition[{
		Magnify[Graphics[Inset[$dwImageFilterImageDefault, Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[Binarize[$dwImageFilterImageDefault,.5], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[ImageEffect[$dwImageFilterImageDefault,{"Comics",{0.255, 0.745}}], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[ImageEffect[$dwImageFilterImageDefault,{"Embossing",5}], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[ImageAdjust[GradientFilter[$dwImageFilterImageDefault,4]], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[ImageEffect[$dwImageFilterImageDefault,{"Jitter",2}], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[EdgeDetect[$dwImageFilterImageDefault,4], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[ImageEffect[$dwImageFilterImageDefault,{"EdgeStylization",16}, RandomSeeding->1], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[ImageAdjust[RidgeFilter[$dwImageFilterImageDefault,4]], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[ImageEffect[$dwImageFilterImageDefault,{"TornFrame",Scaled[.2],{Bottom},.2},RandomSeeding->12], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[ImageEffect[$dwImageFilterImageDefault,{"Vignette",45,Hue[.25,.7,.7]}], Automatic, Automatic, 2],ImageSize->{200,200}],1/3],
		Magnify[Graphics[Inset[ImageEffect[$dwImageFilterImageDefault,{"GaussianNoise",.5}, RandomSeeding->1], Automatic, Automatic, 2],ImageSize->{200,200}],1/3]
	}, 6, 6, 1, {}], 
	Partition[{
	"Original image","Binarize","Comics","Embossing","GradientFilter","Jitter","EdgeDetect","EdgeStylization","RidgeFilter","FrameTorn","FrameVignette","GaussianNoise"
	}, 6, 6, 1, {}]]
], 9](*,
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style["GradientFilter", $dwHelpExampleTextSize, $dwHelpExampleColor], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageGradientFilter[]}, Appearance->None],
	Button[Style["RidgeFilter", $dwHelpExampleTextSize, $dwHelpExampleColor], $dwHelpTopic = {$dwIconHelpExample, dwExampleImageRidgeFilter[]}, Appearance->None]
	}}, Spacings->{2,0}]*)
}
	
dwHelpPresetMaps[]:= {"Preset maps", "",
Row[{Spacer[{0,12}], "Choose a preset map object from the popup menu."}],
"",
Style["World, continent, country or region", Sequence@@$dwHelpSubheadStyle],
"Generate shapes for the world, continent, country or any region.",
Button[Style["Click for more info on world maps", 10, $dwHelpExampleColor], $dwHelpTopic = {Null, dwHelpMap[]}, Appearance->None],
"",
Style["City builder", Sequence@@$dwHelpSubheadStyle],
"Build random cities with houses, high-rises, streets and trees.",
Button[Style["Click for more info on city builder", 10, $dwHelpExampleColor], $dwHelpTopic = {Null, dwHelpCityBuilder[]}, Appearance->None]
}
	
dwHelpMap[]:= {"World shapes", "",
Style["World, continent, country or region", Sequence@@$dwHelpSubheadStyle],
"Display the world by choosing 'World' from the 'continent' popup menu.

Display a continent, country or region by:
     Choosing the continent from the 'continent' popup menu
     Choosing the country from the 'country' popup menu
     Choosing the region from the 'region' popup menu

'All' displays an outline of the country.
'All regions' displays an outline of all regions of the country.

If preview is not visible, click the large button to generate polygons which fit the -1 to 1 space.
Please be patient after changing settings since large amounts of data are transferred.",
"",
Style["Resolution", Sequence@@$dwHelpSubheadStyle],
"Select a 'resolution'. If 'Detailed' is chosen a 'point removal' setting may be selected. 
The total number of points is displayed for comparison of point removal settings.
Example below displays the decrease in number of points for each point removal setting.",
dwDecreasePointsExample[],
"",
Style["Projection", Sequence@@$dwHelpSubheadStyle],
"Select a 'projection'. The projection center is automatically set but can be changed if desired.",
Grid[{
	{dwIconLambertAzimuthal[],dwIconOrthographic[],dwIconSinusoidal[],dwIconMollweide[],dwIconRobinson[],dwIconEquirectangular[],dwIconMillerCylindrical[],dwIconMercator[]},
	{" Lambert\nAzimuthal"," Ortho-\ngraphic","Sinusoidal","Mollweide","Robinson","     Equi-\nrectangular","    Miller\nCylindrical","Mercator"}
},Spacings->{1,Automatic}]
}

dwTextOnCurveExampleArt[]:=
	Grid[{
		With[{align = #},
			Graphics[{
				Gray,BSplineCurve[{{-0.5,0},{-0.25,0.25},{0.25,-0.25},{0.5,0}}],
				White,
				Style[{
					GeometricTransformation[Text["t",{-0.400319162332563,0.060114210079361526},{-1,align}],{{{0.9561680983028004,-0.29281831873707326},{0.29281831873707326,0.9561680983028004}},{0.00005579175677966619,0.11985570421925798}}],
					GeometricTransformation[Text["e",{-0.374706033923181,0.06647790893357505},{-1,align}],{{{0.980842799025244,-0.19480093326348535},{0.19480093326348535,0.980842799025244}},{0.0057716399033452825,0.07426661576951689}}],
					GeometricTransformation[Text["x",{-0.33664287679430765,0.07138812724019747},{-1,align}],{{{0.9976481155102754,-0.06854369131288604},{0.06854369131288604,0.9976481155102754}},{0.004101460596448336,0.023242642058877654}}],
					GeometricTransformation[Text["t",{-0.2982587951188491,0.07190406727087967},{-1,align}],{{{0.9994033979239542,0.03453763480689606},{-0.03453763480689606,0.9994033979239542}},{-0.0026613382328989355,-0.010258255227949695}}],
					GeometricTransformation[Text["",{-0.2719162887294996,0.07017175005972022},{-1,align}],{{{0.9957158037626606,0.09246641626709007},{-0.09246641626709007,0.9957158037626606}},{-0.007653472992258492,-0.024842495195890782}}],
					GeometricTransformation[Text["o",{-0.23383924216887655,0.0652245699746984},{-1,align}],{{{0.9871045452122805,0.16007690908203118},{-0.16007690908203118,0.9871045452122805}},{-0.013456410932737722,-0.03659116261532115}}],
					GeometricTransformation[Text["n",{-0.1961234796197213,0.05800727456538226},{-1,align}],{{{0.9773283434889117,0.2117293296008369},{-0.2117293296008369,0.9773283434889117}},{-0.016728285519397812,-0.04020997185477627}}],
					GeometricTransformation[Text["",{-0.15877515138727155,0.04907342110953474},{-1,align}],{{{0.9681498935889893,0.25037129137269043},{-0.25037129137269043,0.9681498935889893}},{-0.017343581282379006,-0.03818974600643458}}],
					GeometricTransformation[Text["a",{-0.12175309247294802,0.03886721451246539},{-1,align}],{{{0.960385627301009,0.278674446036316},{-0.278674446036316,0.960385627301009}},{-0.01565447185571403,-0.03238977527663871}}],
					GeometricTransformation[Text["",{-0.08499309643635893,0.027751648465161517},{-1,align}],{{{0.9544808794510535,0.2982721085893608},{-0.2982721085893608,0.9544808794510535}},{-0.012146353707049279,-0.024087839457693994}}],
					GeometricTransformation[Text["B",{-0.048420306873806405,0.016033483200531485},{-1,align}],{{{0.9505963815121812,0.31042957245718034},{-0.31042957245718034,0.9505963815121812}},{-0.0073694057027970175,-0.014238983074011283}}],
					GeometricTransformation[Text["S",{-0.007400709484407705,0.0024665150429477654},{-1,align}],{{{0.9487336169976497,0.3160767691219285},{-0.3160767691219285,0.9487336169976497}},{-0.001159015712682336,-0.0022127430381687825}}],
					GeometricTransformation[Text["p",{0.03359795506340437,-0.011163731783523149},{-1,align}],{{{0.9495585798438287,0.3135897055787563},{-0.3135897055787563,0.9495585798438287}},{0.005195559930896567,0.009972858350977769}}],
					GeometricTransformation[Text["l",{0.07011726811449562,-0.02304784325459704},{-1,align}],{{{0.9525246532117856,0.3044614672233051},{-0.3044614672233051,0.9525246532117856}},{0.010346021793205255,0.020253801976596802}}],
					GeometricTransformation[Text["i",{0.09301568038821385,-0.030244543092755857},{-1,align}],{{{0.9554424653199232,0.29517739659971826},{-0.29517739659971826,0.9554424653199232}},{0.01307205489615644,0.026108504102205887}}],
					GeometricTransformation[Text["n",{0.11599391462580631,-0.037181916852748104},{-1,align}],{{{0.9591404426140631,0.28293040017308013},{-0.28293040017308013,0.9591404426140631}},{0.015259354625422814,0.03129896801735747}}],
					GeometricTransformation[Text["e",{0.1529703625163063,-0.04755264967232075},{-1,align}],{{{0.9665479182355686,0.25648610440818925},{-0.25648610440818925,0.9665479182355686}},{0.017313770943171103,0.037644037246762053}}],
					GeometricTransformation[Text["C",{0.1902642252091943,-0.05671213314713676},{-1,align}],{{{0.9755298727459905,0.21986693107466557},{-0.21986693107466557,0.9755298727459905}},{0.0171249124725133,0.0404450581750878}}],
					GeometricTransformation[Text["u",{0.23502388287749162,-0.06541682411026072},{-1,align}],{{{0.9870919045219063,0.16015483766441732},{-0.16015483766441732,0.9870919045219063}},{0.013510531565715522,0.0367958051980168}}],
					GeometricTransformation[Text["r",{0.2731112875631059,-0.0702827135555978},{-1,align}],{{{0.9957189416649571,0.09243261983530349},{-0.09243261983530349,0.9957189416649571}},{0.007665620697094422,0.024943507419374192}}],
					GeometricTransformation[Text["v",{0.3042569524721189,-0.07207681632162775},{-1,align}],{{{0.9997365592233184,0.02295238881512106},{-0.02295238881512106,0.9997365592233184}},{0.0017344888006400883,0.00696443590037138}}],
					GeometricTransformation[Text["e",{0.34262625814121533,-0.0709336920420635},{-1,align}],{{{0.9965486271524989,-0.08301104577385944},{0.08301104577385944,0.9965486271524989}},{-0.004705748992823111,-0.028686582616573593}}]
				},FontFamily->"SourceSansPro",FontSize->14]
			}, ImageSize->200, PlotRange->{{-.5,.5},{-.1,.15}}]
		]&/@{0, -2/3}
	}]


End[] (* End Private Context *)

EndPackage[]