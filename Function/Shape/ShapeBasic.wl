(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *)  

dwShapeBox[]:=
	Block[{},
		dwNewEmptyLayer["Head"->Polygon];
		$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
		$dwP[[$dwSelected[[1]]]] = {{-.2,.2},{.2,.2},{.2,-.2},{-.2,-.2}};
		$dwPointQuantity = Length[Flatten[$dwP, 1]];
		dwUpdateBoundingBox[$dwSelected[[{1}]]]
	]

dwShapeTextBox[]:=
	Block[{},
		dwNewEmptyLayer["Head"->Polygon];
		$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
		$dwP[[$dwSelected[[1]]]] = {{-.2,-.1},{.2,-.1},{.2,.1},{-.2,.1}};
		dwNewEmptyLayer["Form"->"text", "Head"->Text];
		$dwStyle[[$dwSelected[[1]]]] = $dwDefaultTextStyle;
		$dwP[[$dwSelected[[1]]]] = {{0,0}};
		$dwAnimate[[$dwSelected[[1]], 2]] = $dwDefaultTextStyle;
		$dwGroupLayers = Join[$dwGroupLayers, {{Length[$dwP] - 1, Length[$dwP]}}];
		$dwPointQuantity = Length[Flatten[$dwP, 1]];
		$dwSelected = {Length[$dwP] - 1, Length[$dwP]};
		dwUpdateBoundingBox[$dwSelected]
	]

dwShapeDisk[]:=
	Block[{},
		dwNewEmptyLayer["Head"->BezierCurve];
		$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
		$dwP[[$dwSelected[[1]]]] = .2#&/@{{0,1},{.55,1},{1,.55},{1,0},{1,-.55},{.55,-1},{0,-1},{-.55,-1},{-1,-.55},{-1,0},{-1,.55},{-0.55,1},{0,1},{.55,1}};
		$dwPointQuantity = Length[Flatten[$dwP, 1]];
		dwUpdateBoundingBox[$dwSelected[[{1}]]]
	]

dwShapeNSidedPolygon[]:=
	CreateDialog[
		DynamicModule[{sides = 5, radius = .2, quantity = 1, head = Polygon, offset = {0,0}, offsetBend = 1, midOffset = .2, iterate = 1,
			c1 = GrayLevel[.7], c2 = GrayLevel[.5], c3 = GrayLevel[.95], temp},
				Pane[
					Grid[{
						{Row[{Column[{
								"center offset",
								Slider2D[Dynamic@offset, {{-1,-1},{1,1},{.05,.05}}, ImageSize->Medium],
								Button["reset",offset={0,0}]
							}, Alignment->Center],Spacer[10],
							Dynamic@Show[
								
								Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]],
								
								Graphics[{
									(* axes and frame *)
									{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
									(* n-gon *)
									Sequence@@$dwFullDefaultStyle[[$dwStyleStart;;-1]], 
									Table[{EdgeForm[If[quantity == 1, {Black}, {}]], If[head === Line, Black, If[quantity == 1, GrayLevel[.8], 
										If[iterate == 1, Blend[{{0,c1},{midOffset,c2},{1,c3}},(n/quantity)], Blend[Flatten[Table[{c1, c3}, iterate]],(n/quantity)]]]], 
										If[head === BSplineCurve, FilledCurve, Sequence][head[Join[(((n-1)/quantity)^offsetBend)*radius*offset+#&/@(((quantity-(n-1))/quantity)*radius*CirclePoints[sides]),
										If[head === Line, 
											(((n-1)/quantity)^offsetBend)*radius*offset+#&/@(((quantity-(n-1))/quantity)*radius*CirclePoints[sides][[{1}]]), 
											{}
										]],If[head === BSplineCurve, SplineClosed->True, {}]]/.{{}->Sequence[]}]}, 
									{n, quantity}],
									If[head === Line, {}, {Black, 
										Switch[head,
											Polygon,
												Line[Join[radius*CirclePoints[sides], radius*CirclePoints[sides][[{1}]]]],
											_,
												BSplineCurve[radius*CirclePoints[sides], SplineClosed->True]
										]
									}]
								}], 
								
							Background->White, ImageSize->{200,200}, PlotRange->1.1]}], SpanFromLeft, SpanFromLeft},
						
						{Dynamic@Row[{
							PopupMenu[Dynamic@head,{Line,Polygon,BSplineCurve}],Spacer[5],
								ColorSetter[Dynamic@c1],If[iterate == 1, 
								ColorSetter[Dynamic@c2], Nothing], 
								ColorSetter[Dynamic@c3]
							}], SpanFromLeft, SpanFromLeft},
						{"    scale", Slider[Dynamic@radius,{0, 1, $dwGridStep}], Pane[Dynamic@radius,ImageSize->60]},
						{"    sides", Slider[Dynamic@sides,{3, 12, 1}], Pane[Dynamic@sides,ImageSize->60]},
						{" quantity", Slider[Dynamic@quantity,{1, 30, 1}], Pane[Dynamic@quantity,ImageSize->60]},
						{"      rim", Slider[Dynamic@midOffset,{0, 1, .05}], Pane[Dynamic@midOffset,ImageSize->60]},
						{"rim shift", Slider[Dynamic@offsetBend,{.1, 2, .05}], Pane[Dynamic@offsetBend,ImageSize->60]},
						{"  iterate", Slider[Dynamic@iterate,{1, 10, 1}], Pane[Dynamic@iterate,ImageSize->60]},
						{Grid[{{
							Button[Dynamic@dwShapeNSidedPolygonPreview[],
								head = Polygon; (*sides = 5; *)quantity = 1; offsetBend = 1; midOffset = .2; iterate = 1; offset = {0,0}; c1 = GrayLevel[.7]; c2 = GrayLevel[.5]; c3 = GrayLevel[.95], $dwPresetButtonStyle],
							Button[Dynamic@dwShapeNSidedPolygonPreview[Line, 5, 1, 3, 1, .2, 1, {0,0}, GrayLevel[.7], GrayLevel[.5], GrayLevel[.95]],
								head = Line; (*sides = 5; *)quantity = 3; offsetBend = 1; midOffset = .2; iterate = 1; offset = {0,0}; c1 = GrayLevel[.7]; c2 = GrayLevel[.5]; c3 = GrayLevel[.95], $dwPresetButtonStyle],
							Button[dwShapeNSidedPolygonPreview[BSplineCurve, 5, 1, 5, 1, .2, 3, {0,0}, Red, Red, White],
								head = BSplineCurve; sides = 5; quantity = 5; offsetBend = 1; midOffset = .2; iterate = 3; offset = {0,0}; c1 = Red; c2 = Red; c3 = White, $dwPresetButtonStyle],
							Button[dwShapeNSidedPolygonPreview[BSplineCurve, 5, 1, 5, 1, .2, 3, {0,0}, White, Red, Red],
								head = BSplineCurve; sides = 5; quantity = 5; offsetBend = 1; midOffset = .2; iterate = 3; offset = {0,0}; c1 = White; c2 = Red; c3 = Red, $dwPresetButtonStyle],
							Button[dwShapeNSidedPolygonPreview[BSplineCurve, 5, 1, 20, .75, .15, 1, {1/4,1/4}, GrayLevel[.7], GrayLevel[.5], GrayLevel[.95]],
								head = BSplineCurve; sides = 5; quantity = 20; offsetBend = .75; midOffset = .15; iterate = 1; offset = {1/4,1/4}; c1 = GrayLevel[.7]; c2 = GrayLevel[.5]; c3 = GrayLevel[.95], $dwPresetButtonStyle],
							Button[dwShapeNSidedPolygonPreview[BSplineCurve, 5, 1, 20, .75, .15, 1, {1/4,1/4}, RGBColor[1, 0.5, 0], RGBColor[1, .3, 0], RGBColor[1, 0.85, 0]],
								head = BSplineCurve; sides = 5; quantity = 20; offsetBend = .75; midOffset = .15; iterate = 1; offset = {1/4,1/4}; c1 = RGBColor[1, 0.75, 0]; c2 = RGBColor[1, .3, 0]; c3 = RGBColor[1, 0.85, 0], $dwPresetButtonStyle],
							Button[dwShapeNSidedPolygonPreview[BSplineCurve, 5, 1, 20, .75, .15, 1, {1/4,1/4}, Hue[.58], Hue[.58,1,.7], Hue[.5]],
								head = BSplineCurve; sides = 5; quantity = 20; offsetBend = .75; midOffset = .15; iterate = 1; offset = {1/4,1/4}; c1 = Hue[.58]; c2 = Hue[.58,1,.7]; c3 = Hue[.5], $dwPresetButtonStyle],
							Button[dwShapeNSidedPolygonPreview[BSplineCurve, 5, 1, 20, .75, .15, 1, {1/4,1/4}, Hue[.25, 1, .9], Hue[.3, 1, .7], Hue[.2]],
								head = BSplineCurve; sides = 5; quantity = 20; offsetBend = .75; midOffset = .15; iterate = 1; offset = {1/4,1/4}; c1 = Hue[.25, 1, .9]; c2 = Hue[.25,1,.7]; c3 = Hue[.2], $dwPresetButtonStyle]
						}},Spacings->{0,0}], SpanFromLeft, SpanFromLeft},
						
						{Row[{CancelButton[DialogReturn[]],
							DefaultButton[
								(* add to canvas *)	
								dwSetUndo[];
								temp = Length[$dwP];
								If[iterate == 1 && (head =!= Line && quantity > 1),
									
									(* ---------- begin blend by blend gradient ---------- *)
									(* first blend *)
									dwNewEmptyLayer["SetUndo"->False, "Head"->head];
									$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
									$dwP[[$dwSelected[[1]]]] = radius*CirclePoints[sides];
									$dwStyle[[$dwSelected[[1]],8,1]] = {"BlendGradient", $dwSelected[[1]]+1, 
										If[IntegerPart[(midOffset*quantity)] > 0,
											Min[IntegerPart[(midOffset*quantity)], quantity-1],
											quantity-2
										], 1};
									$dwStyle[[$dwSelected[[1]],10]] = If[head === BSplineCurve, True, $dwDefaultSplineClosed];
									$dwStyle[[$dwSelected[[1]]]][[Flatten[Position[$dwStyle[[$dwSelected[[1]]]], FaceForm[_]]][[1]], 1, 1]] = c1;
									$dwStyle[[$dwSelected[[1]]]][[Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]], 1, 2]] = Opacity[0];
									dwUpdateBoundingBox[$dwSelected[[{1}]]];
									(* mid blend *)
									If[IntegerPart[(midOffset*quantity)] > 0,
										dwNewEmptyLayer["SetUndo"->False, "Head"->head];
										$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
										$dwP[[$dwSelected[[1]]]] = ((((midOffset*quantity))/quantity)^offsetBend)*radius*offset+#&/@(((quantity-(midOffset*quantity))/quantity)*radius*CirclePoints[sides]);
										$dwStyle[[$dwSelected[[1]],8,1]] = {"BlendGradient", $dwSelected[[1]]+1, quantity-IntegerPart[(midOffset*quantity)]-3, 1};
										$dwStyle[[$dwSelected[[1]],10]] = If[head === BSplineCurve, True, $dwDefaultSplineClosed];
										$dwStyle[[$dwSelected[[1]]]][[Flatten[Position[$dwStyle[[$dwSelected[[1]]]], FaceForm[_]]][[1]], 1, 1]] = c2;
										$dwStyle[[$dwSelected[[1]]]][[Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]], 1, 2]] = Opacity[0];
										dwUpdateBoundingBox[$dwSelected[[{1}]]],
										Nothing
									];
									(* end blend target *)
									dwNewEmptyLayer["SetUndo"->False, "Head"->head];
									$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
									$dwP[[$dwSelected[[1]]]] = (((quantity-1)/quantity)^offsetBend)*radius*offset+#&/@(((quantity-(quantity-1))/quantity)*radius*CirclePoints[sides]);
									$dwStyle[[$dwSelected[[1]],10]] = If[head === BSplineCurve, True, $dwDefaultSplineClosed];
									$dwStyle[[$dwSelected[[1]]]][[Flatten[Position[$dwStyle[[$dwSelected[[1]]]], FaceForm[_]]][[1]], 1, 1]] = c3;
									$dwStyle[[$dwSelected[[1]]]][[Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]], 1, 2]] = Opacity[0];
									dwUpdateBoundingBox[$dwSelected[[{1}]]],
								
									
									(* ---------- blend by shapes ---------- *)
									Do[
										dwNewEmptyLayer["SetUndo"->False, "Head"->head];
										$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
										$dwP[[$dwSelected[[1]]]] = Join[(((n-1)/quantity)^offsetBend)*radius*offset+#&/@(((quantity-(n-1))/quantity)*radius*CirclePoints[sides]),
											If[head === Line, (((n-1)/quantity)^offsetBend)*radius*offset+#&/@(((quantity-(n-1))/quantity)*radius*CirclePoints[sides][[{1}]]), {}]];
										(* adjustments *)
										$dwStyle[[$dwSelected[[1]],10]] = If[head === BSplineCurve, True, $dwDefaultSplineClosed];
										$dwStyle[[$dwSelected[[1]]]][[Flatten[Position[$dwStyle[[$dwSelected[[1]]]], FaceForm[_]]][[1]], 1, 1]] = If[quantity == 1, GrayLevel[.8], If[iterate == 1, Blend[{{0,c1},{midOffset,c2},{1,c3}},(n/quantity)], Blend[Flatten[Table[{c1, c3}, iterate]],(n/quantity)]]];
										$dwStyle[[$dwSelected[[1]]]][[Flatten[Position[$dwStyle[[$dwSelected[[1]]]], StrokeForm[_]]][[1]], 1, 2]] = If[head === Line || quantity == 1, Opacity[1], Opacity[0]];
										(* update bounding box *)
										dwUpdateBoundingBox[$dwSelected[[{1}]]],
									{n, quantity}]
								
								];
								(* add borderline *)
								If[head =!= Line && quantity != 1,
									dwNewEmptyLayer["SetUndo"->False, "Head"->If[head === BSplineCurve, BSplineCurve, Line]];
									$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
									$dwP[[$dwSelected[[1]]]] = Join[radius*CirclePoints[sides], If[head === BSplineCurve, {}, radius*CirclePoints[sides][[{1}]]]];
									(* adjustments *)
									$dwStyle[[$dwSelected[[1]],10]] = If[head === BSplineCurve, True, $dwDefaultSplineClosed];
									$dwStyle[[$dwSelected[[1]],1]] = False;
									(* update bounding box *)
									dwUpdateBoundingBox[$dwSelected[[{1}]]]
								];
								(* update groups and selection *)
								If[quantity > 1, $dwGroupLayers = Join[$dwGroupLayers, {Range[temp+1, Length[$dwP]]}]];
								If[quantity > 1, $dwSelected = Range[temp+1, Length[$dwP]]];
								$dwPointQuantity = Length[Flatten[$dwP, 1]];
								DialogReturn[]
							]}],SpanFromLeft,SpanFromLeft}
						}, Alignment->Center],
				ImageSize->320]
		], Background->LightGray, WindowTitle->"N-Sided Polygon"
	]
	
dwShapeNSidedPolygonPreview[head_:Polygon, sides_:5, radius_:1, quantity_:1, offsetBend_:1, midOffset_:.1, iterate_:1, offset_:{0,0}, c1_:GrayLevel[.7], c2_:GrayLevel[.5], c3_:GrayLevel[.95]]:=
	Graphics[{
		Table[{AbsoluteThickness[1], EdgeForm[If[quantity == 1, {White}, {}]], If[head === Line, Black, If[quantity == 1, GrayLevel[.8], If[iterate == 1, Blend[{{0,c1},{midOffset,c2},{1,c3}},(n/quantity)], Blend[Flatten[Table[{c1, c3}, iterate]],(n/quantity)]]]], 
			If[head === BSplineCurve, FilledCurve, Sequence][head[Join[(((n-1)/quantity)^offsetBend)*radius*offset+#&/@(((quantity-(n-1))/quantity)*radius*CirclePoints[sides]),
			If[head === Line, 
				(((n-1)/quantity)^offsetBend)*radius*offset+#&/@(((quantity-(n-1))/quantity)*radius*CirclePoints[sides][[{1}]]), 
				{}
			]],If[head === BSplineCurve, SplineClosed->True, {}]]/.{{}->Sequence[]}]}, 
		{n, quantity}],
		If[head === Line, {}, {Black, 
			Switch[head,
				Polygon,
					Line[Join[radius*CirclePoints[sides], radius*CirclePoints[sides][[{1}]]]],
				_,
					BSplineCurve[radius*CirclePoints[sides], SplineClosed->True]
			]
		}]}, ImageSize->27]

dwShapeCapsule[]:=
	CreateDialog[
		DynamicModule[{size = 1, width = .4, height = .2, fullradius = 1, degree = .55, radius},
				Pane[
					Grid[{
						{Dynamic@Show[
							
							Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]],
							
							Graphics[{
								(* axes and frame *)
								{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
								(* box *)
								radius = fullradius/1Min[width, height];
								Sequence@@$dwFullDefaultStyle[[$dwStyleStart;;-1]], 
								BezierCurve[Most[size{
									{(1-radius)+(width-1), height}, {((1-radius)+degree*radius)+(width-1), height},
									{width, ((1-radius)+degree*radius)+(height-1)}, {width, 0}, {width, -((1-radius)+degree*radius)-(height-1)},
									{((1-radius)+degree*radius)+(width-1), -height}, {(1-radius)+(width-1), -height}, {(1-radius)+(width-1), -height},
									{-(1-radius)-(width-1), -height}, {-(1-radius)-(width-1), -height}, {-((1-radius)+degree*radius)-(width-1),-height},
									{-width, -((1-radius)+degree*radius)-(height-1)}, {-width, 0}, {-width, ((1-radius)+degree*radius)+(height-1)},
									{-((1-radius)+degree*radius)-(width-1), height}, {-(1-radius)-(width-1), height}, {-(1-radius)-(width-1), height},
									{(1-radius)+(width-1), height}, {(1-radius)+(width-1), height}, {((1-radius)+degree*radius)+(width-1), height}
								}]]
								}], 
								
							Background->White, ImageSize->{200,200}, PlotRange->1.1], SpanFromLeft, SpanFromLeft},
							
						{"scale ", Slider[Dynamic@size,{.1, 2, .01}], Pane[Dynamic@size,ImageSize->30]},
						{"width ", Slider[Dynamic@width,{.2, 2, $dwGridStep}], Pane[Dynamic@width,ImageSize->30]},
						{"radius ", Slider[Dynamic@fullradius,{0, 1, $dwGridStep}], Pane[Dynamic@fullradius,ImageSize->30]},
						
						{Row[{CancelButton[DialogReturn[]],
							DefaultButton[
								(* add to canvas *)	
								dwNewEmptyLayer["Head"->BezierCurve];
								$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
								$dwP[[$dwSelected[[1]]]] = size{
									{(1-radius)+(width-1), height}, {((1-radius)+degree*radius)+(width-1), height},
									{width, ((1-radius)+degree*radius)+(height-1)}, {width, 0}, {width, -((1-radius)+degree*radius)-(height-1)},
									{((1-radius)+degree*radius)+(width-1), -height}, {(1-radius)+(width-1), -height}, {(1-radius)+(width-1), -height},
									{-(1-radius)-(width-1), -height}, {-(1-radius)-(width-1), -height}, {-((1-radius)+degree*radius)-(width-1),-height},
									{-width, -((1-radius)+degree*radius)-(height-1)}, {-width, 0}, {-width, ((1-radius)+degree*radius)+(height-1)},
									{-((1-radius)+degree*radius)-(width-1), height}, {-(1-radius)-(width-1), height}, {-(1-radius)-(width-1), height},
									{(1-radius)+(width-1), height}, {(1-radius)+(width-1), height}, {((1-radius)+degree*radius)+(width-1), height}
								};
								dwUpdateBoundingBox[$dwSelected[[{1}]]];
								$dwPointQuantity = Length[Flatten[$dwP, 1]];
								DialogReturn[]
							]}],SpanFromLeft,SpanFromLeft}
						}, Alignment->Center],
				ImageSize->280]
		], Background->LightGray, WindowTitle->"Capsule"
	]

dwShapeRoundedBox[]:=
	CreateDialog[
		DynamicModule[{size = 1, width = .4, height = .2, fullradius = .5, degree = .55, radius},
				Pane[
					Grid[{
						{Dynamic@Show[
							
							Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]],
							
							Graphics[{
								(* axes and frame *)
								{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
								(* box *)
								radius = fullradius/1Min[width, height];
								Sequence@@$dwFullDefaultStyle[[$dwStyleStart;;-1]], 
								BezierCurve[Most[size{
									{(1-radius)+(width-1), height}, {((1-radius)+degree*radius)+(width-1), height},
									{width, ((1-radius)+degree*radius)+(height-1)}, {width, (1-radius)+(height-1)},{width, Max[((1-radius)-degree*radius)+(height-1),((1-radius)+(height-1))/2]},
									{width, Min[-((1-radius)-degree*radius)-(height-1),(-(1-radius)-(height-1))/2]}, {width, -(1-radius)-(height-1)}, {width, -((1-radius)+degree*radius)-(height-1)},
									{((1-radius)+degree*radius)+(width-1), -height}, {(1-radius)+(width-1), -height}, {Max[((1-radius)-degree*radius)+(width-1),((1-radius)+(width-1))/2], -height},
									{Min[-((1-radius)-degree*radius)-(width-1),(-(1-radius)-(width-1))/2], -height}, {-(1-radius)-(width-1), -height}, {-((1-radius)+degree*radius)-(width-1), -height},
									{-width, -((1-radius)+degree*radius)-(height-1)}, {-width, -(1-radius)-(height-1)}, {-width, Min[-((1-radius)-degree*radius)-(height-1),(-(1-radius)-(height-1))/2]},
									{-width, Max[((1-radius)-degree*radius)+(height-1),((1-radius)+(height-1))/2]}, {-width, (1-radius)+(height-1)}, {-width, ((1-radius)+degree*radius)+(height-1)},
									{-((1-radius)+degree*radius)-(width-1), height}, {-(1-radius)-(width-1), height}, {Min[-((1-radius)-degree*radius)-(width-1),(-(1-radius)-(width-1))/2], height},
									{Max[((1-radius)-degree*radius)+(width-1),((1-radius)+(width-1))/2], height}, {(1-radius)+(width-1), height}, {((1-radius)+degree*radius)+(width-1), height}
									}]]
								}], 
								
							Background->White, ImageSize->{200,200}, PlotRange->1.1], SpanFromLeft, SpanFromLeft},
							
						{"width ", Slider[Dynamic@width, {.1, 2, $dwGridStep}], Pane[Dynamic@width,ImageSize->30]},
						{"height ", Slider[Dynamic@height, {.1, 2, $dwGridStep}], Pane[Dynamic@height,ImageSize->30]},
						{"radius ", Slider[Dynamic@fullradius, {0, 1, $dwGridStep}], Pane[Dynamic@fullradius,ImageSize->30]},
						
						{Row[{CancelButton[DialogReturn[]],
							DefaultButton[
								(* add to canvas *)	
								dwNewEmptyLayer["Head"->BezierCurve];
								$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
								$dwP[[$dwSelected[[1]]]] = 
								If[fullradius == 1 && width == height, (* circle *)
									size{
										{(1-radius)+(width-1),height},{((1-radius)+degree*radius)+(width-1),height},
										{width,((1-radius)+degree*radius)+(height-1)},{width,(1-radius)+(height-1)},{width,((1-radius)-degree*radius)+(height-1)},
										{((1-radius)+degree*radius)+(width-1),-height},{(1-radius)+(width-1),-height},{((1-radius)-degree*radius)+(width-1),-height},
										{-width,-((1-radius)+degree*radius)-(height-1)},{-width,-(1-radius)-(height-1)},{-width,-((1-radius)-degree*radius)-(height-1)},
										{-((1-radius)+degree*radius)-(width-1),height},{-(1-radius)-(width-1),height},{-((1-radius)-degree*radius)-(width-1),height}
										},
									size{
										{(1-radius)+(width-1), height}, {((1-radius)+degree*radius)+(width-1), height},
										{width, ((1-radius)+degree*radius)+(height-1)}, {width, (1-radius)+(height-1)}, {width, Max[((1-radius)-degree*radius)+(height-1),((1-radius)+(height-1))/2]},
										{width, Min[-((1-radius)-degree*radius)-(height-1),(-(1-radius)-(height-1))/2]}, {width, -(1-radius)-(height-1)}, {width, -((1-radius)+degree*radius)-(height-1)},
										{((1-radius)+degree*radius)+(width-1), -height}, {(1-radius)+(width-1), -height}, {Max[((1-radius)-degree*radius)+(width-1),((1-radius)+(width-1))/2], -height},
										{Min[-((1-radius)-degree*radius)-(width-1),(-(1-radius)-(width-1))/2], -height}, {-(1-radius)-(width-1), -height}, {-((1-radius)+degree*radius)-(width-1), -height},
										{-width, -((1-radius)+degree*radius)-(height-1)}, {-width, -(1-radius)-(height-1)}, {-width, Min[-((1-radius)-degree*radius)-(height-1),(-(1-radius)-(height-1))/2]},
										{-width, Max[((1-radius)-degree*radius)+(height-1),((1-radius)+(height-1))/2]}, {-width, (1-radius)+(height-1)}, {-width, ((1-radius)+degree*radius)+(height-1)},
										{-((1-radius)+degree*radius)-(width-1), height}, {-(1-radius)-(width-1), height}, {Min[-((1-radius)-degree*radius)-(width-1),(-(1-radius)-(width-1))/2], height},
										{Max[((1-radius)-degree*radius)+(width-1),((1-radius)+(width-1))/2], height}, {(1-radius)+(width-1), height}, {((1-radius)+degree*radius)+(width-1), height}
									}
								];
								dwUpdateBoundingBox[$dwSelected[[{1}]]];
								$dwPointQuantity = Length[Flatten[$dwP, 1]];
								DialogReturn[]
							]}],SpanFromLeft,SpanFromLeft}
						}, Alignment->Center],
				ImageSize->280]
		], Background->LightGray, WindowTitle->"Rounded Rectangle"
	]

dwShapeTorn[]:=
	CreateDialog[
		DynamicModule[{seed = 1, quantity = 50, scale = 1, height = 1, tear},
				Pane[
					Grid[{
						{Dynamic@Show[
							
							Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]],
							
							Graphics[{
								(* axes and frame *)
								{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
								(* box *)
								{
									$dwDefaultFaceStyle, $dwDefaultStrokeStyle,
									tear = (SeedRandom[seed]; RandomReal[{-1, 1}, quantity]);
									Polygon[Join[{{scale,scale}, {-scale,scale}}, scale*(2./(quantity - 1))*{1, height}# + {-scale, 0}&/@Table[{(n - 1), tear[[n]]}, {n, quantity}]]]
								}
								}], 
								
							Background->White, ImageSize->{200,200}, PlotRange->1.1], SpanFromLeft, SpanFromLeft},
							
						{"scale ", Slider[Dynamic@scale, {.1, 1, .01}], Pane[Dynamic@scale,ImageSize->30]},
						{"quantity ", Slider[Dynamic@quantity, {10, 100, 1}], Pane[Dynamic@quantity,ImageSize->30]},
						{"random ", Slider[Dynamic@seed, {1, 20, 1}], Pane[Dynamic@seed,ImageSize->30]},
						{"height ", Slider[Dynamic@height, {.1, 1, .1}], Pane[Dynamic@height,ImageSize->30]},
						
						{Row[{CancelButton[DialogReturn[]],
							DefaultButton[
								(* add to canvas *)	
								dwNewEmptyLayer["Head"->Polygon];
								$dwP[[-1]] = Join[{{scale,scale}, {-scale,scale}}, scale*(2./(quantity - 1))*{1, height}# + {-scale, 0}&/@Table[{(n - 1), tear[[n]]}, {n, quantity}]];
								dwUpdateBoundingBox[$dwSelected[[{1}]]];
								$dwPointQuantity = Length[Flatten[$dwP, 1]];
								DialogReturn[]
							]}],SpanFromLeft,SpanFromLeft}
						}, Alignment->Center],
				ImageSize->280]
		], Background->LightGray, WindowTitle->"Torn"
	]

End[] (* End Private Context *)

EndPackage[]