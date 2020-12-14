(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
		
dwHelpCanvasTools[]:=
	{
		{Graphics[{Text[Style["P",18,White]]},Background->Hue[.25,.7,.7],$dwButtonStyle], dwHelpPreferences[]},
		{Graphics[{Table[{If[{x,y}==={1,1},GrayLevel[1],Gray],Rectangle[{x,y},{x+.8,y+.8}]},{x,0,2},{y,0,2}]},ImageSize->27], dwHelpTransformOrigin[]},
		{Magnify[Graphics[{Gray,EdgeForm[Gray],Disk[{-.05,.05}],
			Dynamic@Table[With[{b=buttonParts},
				Inset[Button[Graphics[{White,b[[1]]},ImageSize->14],Null,FrameMargins->0,ImageSize->14,Appearance->"Frameless"],buttonParts[[3]]]],
					{buttonParts,
						{{Polygon[{{0.,0.5},{0.5,0.},{0.25,0.},{0.25,-0.5},{-0.25,-0.5},{-0.25,0.},{-0.5,0.}}],{0,1},{0,.55}},
						{Polygon[{{-0.5,0.},{0.,0.5},{0.,0.25},{0.5,0.25},{0.5,-0.25},{0.,-0.25},{0.,-0.5}}],{-1,0},{-.55,0}},
						{Polygon[{{1/2,0},{0.,-1/2},{0.,-0.25},{-1/2,-0.25},{-1/2,0.25},{0.,0.25},{0.,1/2}}],{1,0},{.55,0}},
						{Polygon[{{0.,-0.5},{-0.5,0.},{-0.25,0.},{-0.25,0.5},{0.25,0.5},{0.25,0.},{0.5,0.}}],{0,-1},{0,-.55}},
						{Polygon[{{0,1},{1,0},{0,-1},{-1,0}}], {0,0}, {0,0}}}}
			]},ImageSize->48],2/3], dwHelpNudge[]},
		{$dwIconPreview, dwHelpPreview[]},
		{Style["UNDO", 8, White], dwHelpUndo[]},
		{Style["REDO", 8, White], dwHelpRedo[]},
		{$dwIconObjectDelete, dwHelpDelete[]},
		{$dwIconObjectSelect, dwHelpObjectSelection[]},
		{$dwIconPointSelect, dwHelpPointSelection[]},
		{$dwIconSelectionMenu, dwHelpSelectionMenu[]},
		{$dwIconZoom, dwHelpZoom[]},
		{$dwIconMoveCanvas, dwHelpMoveCanvas[]},
		{$dwIconPlotRange, dwHelpBoundary[]},
		{$dwIconTemplate, dwHelpTemplate[]},
		{$dwIconAutoDraw, dwHelpAutoDraw[]},
		{$dwIconInsertExpression, dwHelpInsertExpression[]},
		{$dwIconSaveFile, dwHelpSave[]},
		{$dwIconOpenFile, dwHelpOpen[]},
		{$dwIconClipboard, dwHelpCopy[]},
		{$dwIconImportFile, dwHelpImport[]},
		{$dwIconModelicaExport, dwHelpWSMicon[]},
		{$dwIconAnimate, dwHelpAnimate[]}
	}
	
dwHelpAnimate[]:= {"Animate", "",
Row[{Spacer[{0,12}], "Animate transitions from each object to its destination object."}],
"",
"Start by clicking the 'Animate' button to open the dialog then:
     1. Click 'Reset all objects' to initialize parameters if starting new animation.
     2. Click an object in the dialog preview to select.
     3. Change the object transition settings or add a motion path.
     4. Drag the PREVIEW 'frame' slider to preview the animation.
     5. Click 'Save settings' to return to canvas or 'Export' to create animation.",
"",
Style["Motion path", Sequence@@$dwHelpSubheadStyle],
"'Add bezier motion path to selected object' button adds an adjustable bezier path to control motion.",
"",
Style["Transition", Sequence@@$dwHelpSubheadStyle],
"- Set the selected object's final appearance when fully transitioned.",
"- Check 'cycle arrow' to constrain arrow speed settings for smooth repetition.",
"- Check 'show object points' to animate object points. Object point positions are relative 
   to original object position. Click point to select then drag to move.",
"- Set the frame to 'begin' the transition. Also set the 'peak', 'peak end' and 'end'.
   The peak displays the full transition. The transition will reverse for any frames 
   remaining between the 'peak end' and 'end' frame.",
"",
Style["Export animation", Sequence@@$dwHelpSubheadStyle],
"Click the 'Export' button to create an animated PNG file.
The file is located in your documents directory and named 'animation-1234.png'.
Open file in a web browser or evaluate Import[<file path>] in a Wolfram Language notebook.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Work in wireframe mode for speedy interface but set to preview mode before exporting.
- Set 'frame width' to large size for composing then set proper size before exporting.
- The aspect ratio of the exported animation is derived from the boundary which is set 
   on the main canvas. It cannot be changed in the animate dialog.
- If an object is not animating as expected click one of the three RESET buttons or 
   'Reset all objects'. Altering objects outside of animate dialog may require a reset.
- Click 'Set group' to pass transforms of selected object to all objects in its group.
- Choose an object ID from the 'Origin' popup menu to change an objects transform origin.
- An object's motion path is not visible if 'show object points' is checked.
- Check 'remove last frame' for smooth transition of looped animation.
- Gradients do not rotate with objects. Gradient colors cannot be animated.
- Arrowheads set to a quantity of 3 or more will automatically move along path."
}
	
dwHelpAutoDraw[]:= {"Auto draw", "",
"Paste an Image or Graphics into the dialog box to automatically convert to vector shapes.
",
Grid[{
{Show[Import[$dwFileDirectory<>"Help/bullseye.png"],ImageSize->100],
	Spacer[30],
Graphics[{EdgeForm[Black], GrayLevel[.8], Disk[{0,0},1], Disk[{0,0},2/3], Disk[{0,0},1/3]}, ImageSize->100]},
{Style["before",Italic],"",Style["after",Italic]}
}]
}
	
dwHelpPreferences[]:= {"Preferences", 
"",
Grid[{{dwPreferences[],
	Pane[Style[Row[{Style["CONSTRAIN ANGLE",Bold]," is used when ShiftKey is pressed while drawing or moving an object or point.

Set ",Style["ROTATE",Bold]," and\n",Style["SCALE",Bold]," step to\ncontrol units\nwhen transforming\nwith selection box.



Set small and large object size used by the selection menu."
	}],
		LineIndent->0], ImageSize->115]
}}, Alignment->Bottom]
}
	
dwHelpTransformOrigin[]:= {"Transform origin", "",
"Set the origin for scale and rotation.
Click one of the nine boxes to set the origin position relative to all selected objects.",
"",
Grid[Partition[
	If[# === Null, 
		Null,
		Column[{
			Graphics[{EdgeForm[LightGray], FaceForm[Opacity[0]], Rectangle[], EdgeForm[Gray],
				Rotate[Rectangle[{.25,.25},{.75,.75}], If[#[[2]] === Null,0,20\[Degree]],#[[1]]],
				If[#[[2]] === Null,
					Nothing,
					{AbsolutePointSize[6],Hue[.58],Point[#[[1]]],Arrowheads[Small],Arrow[Table[#[[1]]+.15{Cos[x],Sin[x]},{x,0,1.5Pi,Pi/20}]]}
				]},ImageSize->72],
			If[#[[2]] === Null,
				"original\nposition",
				Graphics[Table[{If[{x,y}===#[[2]],GrayLevel[1],GrayLevel[.5]],Rectangle[{x,y},{x+.8,y+.8}]},{x,0,2},{y,0,2}],ImageSize->33]
			]
		},Alignment->Center]
	]&/@{
			{{.5,.5},Null},{{.5,.5},{1,1}},Null,Null,
			{{.25,.25},{0,0}},{{.75,.25},{2,0}},{{.75,.75},{2,2}},{{.25,.75},{0,2}},
			{{.5,.25},{1,0}},{{.75,.5},{2,1}},{{.5,.75},{1,2}},{{.25,.5},{0,1}}
		},
4], Alignment->Top, Spacings->{1,1}],
"",
"Set the origin position relative to each selected object by checking 'each object' in 
the TRANSFORM section of Preferences dialog."
}
	
dwHelpNudge[]:= {"Nudge selection", "",
"Click a direction arrow to move selected objects or points 1 grid unit.
"<>StringTake[$dwCommandKey, {1,-4}]<>"-click direction arrow to move multiple grid units.
Click center diamond to center object.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"Set 'nudge multiplier' for multiple grid units in the Preferences dialog."
}
	
dwHelpDelete[]:= {"Delete selection", "",
"Delete selected objects or points."
}
	
dwHelpUndo[]:= {"Undo", "",
"Multiple undo."
}
	
dwHelpRedo[]:= {"Redo", "",
"Multiple redo."
}
	
dwHelpObjectSelection[]:= {"Object selection tool", "",
"Click an object or drag around multiple objects to select.
ShiftKey-click or ShiftKey-drag to toggle selected on/off.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Click an object to select its entire group.
- "<>StringTake[$dwOptionKey, {1,-4}]<>"-click an object to select it within a group.
- Drag to select individual objects of a group.
- Compound paths are selected as a single object."
}
	
dwHelpPointSelection[]:= {"Point selection tool", "",
"Click object to display its points.
Click or drag around points to select.
ShiftKey-click to toggle selection on/off.

MOVE POINTS
Drag point to move all selected points."
}
	
dwHelpSelectionMenu[]:= {"Selection menu", "",
"Select, hide or delete multiple objects by choosing from the popup menu.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Settings for 'Select small objects' and 'Select large objects' are in the Preferences dialog."
}
	
dwHelpPreview[]:= {"Preview / Wireframe toggle", "",
"Click to toggle between preview and wireframe display.

Preview displays styles for all objects.
Wireframe displays objects as lines and boxes.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"Use wireframe display for a more responsive interface with complex illustrations."
}
	
dwHelpZoom[]:= {"Zoom tool", "",
"Click the canvas to zoom in. 
Drag on the canvas to zoom into a specific area. 
"<>StringTake[$dwOptionKey, {1,-4}]<>"-click canvas to zoom out. 
"<>StringTake[$dwCommandKey, {1,-4}]<>"-click zoom tool for actual size.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Zoom anytime without using the zoom tool by pressing the "<>StringTake[$dwCommandKey, {1,-4}]<>"
   and "<>StringTake[$dwOptionKey, {1,-4}]<>" while dragging the canvas vertically.
- Current zoom percentage displayed in message bar at bottom of window."
}
	
dwHelpMoveCanvas[]:= {"Move canvas tool", "",
"Drag to move canvas.
"<>StringTake[$dwCommandKey, {1,-4}]<>"-click move canvas tool to center canvas.

Objects drawn as wireframe.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Move canvas anytime without using the move canvas tool by pressing the 
   "<>StringTake[$dwCommandKey, {1,-4}]<>" while dragging the canvas."
}
	
dwHelpBoundary[]:= {"Resize boundary", "",
"The boundary hides graphics outside of its perimeter.

Drag a corner dot of the yellow box to resize.
"<>StringTake[$dwCommandKey, {1,-4}]<>"-click tool to fit boundary to all objects.
ShiftKey-click tool to fit boundary to all objects without padding.
"<>StringTake[$dwOptionKey, {1,-4}]<>"-click tool for dialog to enter an image size.", 
"",
Grid[{
{Graphics[{FaceForm[White],StrokeForm[Hue[.125]],Polygon[{{-1., 1.}, {1., 1.}, {1., -1.}, {-1., -1.},{-1., 1.}}],Hue[.125],Disk[{-1,-1},Offset[4]],Disk[{1,1},Offset[4]],StrokeForm[Black],FaceForm[Opacity[0]],Polygon[{{0.29389262614623657, -0.4045084971874737},{0.47552825814757677, 0.15450849718747373},{0., 0.5},{-0.47552825814757677,0.15450849718747373}, {-0.29389262614623657,-0.4045084971874737}}]},ImagePadding->5,ImageSize->120],
	Spacer[30],
Graphics[{FaceForm[White],StrokeForm[Hue[.125]],Polygon[{{-0.5, 0.525}, {0.5, 0.525},{0.5, -0.45}, {-0.5, -0.45},{-0.5, 0.525}}],Hue[.125],Disk[{-0.5, -0.45},Offset[4]],Disk[{0.5, 0.525},Offset[4]],StrokeForm[Black],FaceForm[Opacity[0]],Polygon[{{0.29389262614623657, -0.4045084971874737},{0.47552825814757677, 0.15450849718747373}, {0., 0.5},{-0.47552825814757677,0.15450849718747373}, {-0.29389262614623657,-0.4045084971874737}}]},ImagePadding->5,ImageSize->60]},
{Style["before",Italic],"",Style["after",Italic]}
}],
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- The trimmed area of the canvas is light gray."
}
	
dwHelpTemplate[]:= {"Canvas template", "",
"Opens a dialog box to paste image or graphics for tracing on the canvas. 
Set size, rotation, location and fade.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Uncheck 'show template' or leave field empty to hide template.
- Click 'Use camera image' to take a picture with the built-in or connected camera."
}
	
dwHelpInsertExpression[]:= {"Insert expression", "",
"Insert Wolfram Language expression.
Double-click object to paste new expression.

Expressions can be moved or scaled.
Style before pasting expression.

Possible expressions include:
- Graphics
- Graphics3D
- Graphs
- Regions
- Polyhedrons
- Plots and charts"
}
	
dwHelpSave[]:= {"Save WLDraw file", "",
"Saves WLDraw file as 'myfile.wldraw'

NOTE: Using 'Save' from the menu will save WLDraw window as an unusable notebook."
}
	
dwHelpOpen[]:= {"Open WLDraw file", "",
"Opens WLDraw file such as 'myfile.wldraw'

NOTE: Using 'Open' from the menu will open a notebook without any relevance to WLDraw.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Opening large files may display 'Loading objects...' in the message bar 
    located below the canvas. Please wait until the file is completely loaded 
    before preceding or errors might occur.
- The boundary of the file is used if no objects exist prior to opening the file."
}
	
dwHelpCopy[]:= {"Copy graphics to clipboard", "",
"Copy styled graphics to the clipboard.

ShiftKey-click to copy styled graphics without boundary or size.
"<>StringTake[$dwCommandKey, {1,-4}]<>"-click to copy unstyled graphics without boundary or size."
}
	
dwHelpExport[]:= {"Export graphics to notebook", "",
"Export graphics to a notebook.

Exported graphics that are imported into WLDraw will contain the following changes:
- Gradients are converted to an Image and will not be editable."
}
	
dwHelpImport[]:= {"Import notebook graphics", "",
"Open a notebook to load its graphics.
"<>StringTake[$dwCommandKey, {1,-4}]<>"-click to load unstyled and unordered graphics.

Only the first graphic is loaded if multiple are found.
The progress of opening the graphics is displayed in message bar at bottom of window.
All objects are grouped together.

It is possible to open graphics not created by WL-Draw but adjustments such as color
and line thickness might be needed. If the aspect ratio is wrong or the scale is too large, 
click the checkbox 'fix imported file' in Preferences to preserve the original aspect ratio 
and scale to fit in -1 to 1 coordinates. Large sizes will automatically be fixed.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Import Wolfram Language graphics with 'Insert expression' tool for best result.,
- Graphics created with Plot, ListPlot and similar visualizations will only include the 
    data graphics. Axes, frames, legends, epilogs, prolog, etc. will be ignored.
- BarChart, PieChart, etc. do not have compatible graphics so nothing will appear. 
- Only BezierCurve, BSplineCurve, Polygon, Line, Arrow and Point are supported so 
    primitives such as Disk and Circle will be ignored.
- Offset, Scaled and ImageScaled coordinates are not supported.
- Dynamics is not supported.
- 2D Graph can be used by wrapping with Show (i.e., Show[Graph[...]]). 
         - Graph labels are not supported. 
         - Graph vertices will not appear unless Point is used.
- 2D Mesh can be used by wrapping with Show (i.e., Show[Mesh[...]])."
}
	
dwHelpWSMicon[]:= {"Save SystemModeler component icon", 
"",
"Save Arrows, Lines, Polygons and BSplineCurves as an icon of a SystemModeler file.

BezierCurve and Point are not supported.",
"",
Style["Tips", Sequence@@$dwHelpSubheadStyle],
"- Reload open SystemModeler models to see changes.
- A filled BSplineCurve is always closed in SystemModeler.
- The curve of a BSplineCurve is tighter in SystemModeler.
- Some SystemModeler styling, such as gradients, will be necessary."
}

End[] (* End Private Context *)

EndPackage[]