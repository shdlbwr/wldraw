(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
		
(* 
	Saving graphics changes the following which requires a Graphics parser rather than reading in styles:
	1. StrokeForm duplicated and head replaced with EdgeForm
	2. Remaining StrokeForm retains color and opacity then places rest in lowest level
	3. Text head replaced with Inset
	
	Function steps:
	1. Collect objects with positions
	2. Collect styles with positions
	3. Match style to objects by creating style array with length equal to object array
	4. Insert objects and styles
	
	Note: DEBUG variables located after each step; uncomment DEBUG variables; comment everything after
	
	This function is used for importing graphics from a notebook and shapes using dwShapeStyleDialog.
*)

(* "ExtraScaling" added for scaling text when it is converted to graphics *)
Options[dwGraphicsToWLDraw] = {"ExtraScaling"->1, "Group"->True, "SetUndo"->True, "ReturnObjectValues"->False, "Style"->{}};

dwGraphicsToWLDraw[graphics_, OptionsPattern[]]:=
	Block[{g, length = Length[$dwP], gcolors, gtype, minobjectposlength, firstobjectlist, gradients = {}, finalStrokeform, finalFaceform,
		counter, temp, pos, wrapperObjects, aspectratio, inset, texture, newobject = {}, 
		absolutedashing, absolutepointsize, absolutethickness, arrowheads, capform, color, dashing, faceform, object, imagesize, 
		joinform, opacity, plotrange, pointsize, strokeform, thickness, gcpts, object1, gradients1, texture1,
		strayabsolutedashing, strayabsolutethickness, straycapform, straycolor, straydashing, strayjoinform, strayopacity, straythickness, 
		extraScaling = OptionValue["ExtraScaling"], group = OptionValue["Group"], passStyle = OptionValue["Style"]},
		
		$dwMode = $dwCurrentPreviewMode;
		$dwMessageBarText = "Gathering graphics...";
		
		If[CurrentValue[$dwCommandKey],
		
			(* ---------- collect raw unstyled and unlayered primitives - removes textures, vertex colors and compound paths (FilledCurve) ---------- *)
			g = InputForm[graphics]/.{Texture[___] -> Sequence[], (VertexColors -> {___}) -> Sequence[], (VertexTextureCoordinates -> {___}) -> Sequence[]};
			g = Cases[g, _GraphicsComplex, Infinity];
			g = {1, Join[g, Cases[InputForm[graphics]/.{GraphicsComplex[___]->Sequence[]}, _Arrow | _BezierCurve | _BSplineCurve | _Line | _Point | _Polygon, Infinity]]},
	
			(* ---------- collect styled graphics ----------  *)
			(* 	1. collect graphics with position
				2. choose shortest sorted position to exclude graphics used to create textures *)
			pos = Flatten[Position[InputForm[graphics], _Graphics, Infinity]];
			g = If[pos =!= {},
						Sort[Append[#, Extract[InputForm[graphics], #]]&/@Position[InputForm[graphics], _Graphics, Infinity]][[1]],
					{}
				];
			(* remove GeometricTransforms *)
			g = g/.{GeometricTransformation[_]->Sequence[], GeometricTransformationBox[_]->Sequence[]}
		];
		
		If[Flatten[Position[g, _Dynamic, Infinity]] === {},
	
			If[g === {},
				
				MessageDialog["No 2D graphics found in file."],
				
				g = g[[-1]];
				
				aspectratio = Quiet[Flatten[{Extract[g, #]&/@Position[g, AspectRatio->_, Infinity]}]]/.{Automatic->1}; (* quiet for case where AspectRatio comes wrapped in Pane *)
				aspectratio = If[aspectratio === {}, 1, aspectratio[[1,-1]]];
				imagesize = (Quiet@Flatten[{Extract[g, #]&/@Position[g, ImageSize->_, Infinity]}])/.{Small->{180}, Medium->{360}, Large->{720}}; (* quiet for case where ImageSize comes wrapped in Pane *)
				plotrange = Quiet@Flatten[{Extract[g, #]&/@Position[g, PlotRange->_, Infinity]}][[1,-1]]; (* quiet for case where PlotRange comes wrapped in Pane *)
				
				(*<<<<<<<<<<<<<<<<<<<<<<< COLLECT OBJECTS >>>>>>>>>>>>>>>>>>>>>>*)
				If[Flatten[{Cases[g, _GraphicsComplex, Infinity]}] =!= {} && Flatten[{Cases[g, _Inset, Infinity]}] === {},(* commented for raw primitives case - is it needed? *)
					
					(* GraphicsComplex *)
					g = Flatten[{Cases[g, _GraphicsComplex, Infinity]}];
					gcpts = g[[1,1]];
					g = g[[1,2]];
					If[g =!= {},
						{object, gradients, texture} = dwCollectObjects[gc];
						If[object =!= {},
							(* flatten point list *)
							Do[object[[n,-1,1]] = Flatten[{object[[n,-1,1]]}], {n, Length[object]}];
							(* add point values to object *)
							object = Table[ReplacePart[object[[n]],gcpts[[object[[n,-1,1]]]], {-1, 1}], {n, Length[object]}];
							object = If[Head[#[[-1,1]]] =!= List, Nothing, #]&/@object
						],
						object = gradients = texture = {}
					],
					object = gradients = texture = {}
				];
				
				(* graphics without GraphicsComplex *)
				{object1, gradients1, texture1} = dwCollectObjects[g/.{GraphicsComplex[___]->Sequence[]}];
				object = Join[object, object1];
				gradients = Join[gradients, gradients1];
				texture = Join[texture, texture1];
				
				(* get objects *)
				If[g =!= {} && object =!= {},
				
					(* remove any objects using Offset, Scaled, ImageScaled or Entity (GeoGraphics) for coordinates *)
					object = Table[If[Head[o[[-1]]] === Inset || Flatten[Position[o[[-1]], _Offset | _Scaled | ImageScaled | Entity, Infinity]] === {}, o, Nothing], {o, object}];
				
					(* remove any objects using multiple sets of coordinates (i.e., Polygon[{list, list, ...}]) *)
					object = Table[If[Length[Dimensions[o[[-1]]]] > 2, Nothing, o], {o, object}];
					
					(* DEBUG: check collected objects, gradients and textures *)
					(*Print@Column[{OpenerView[{"g: ",g}],OpenerView[{"object: ",Column[object]}],OpenerView[{"gradients: ",Column[gradients]}],OpenerView[{"texture",Column[texture]}]
						}]*)
					
					(*<<<<<<<<<<<<<<<<<<<<<<< COLLECT STYLES >>>>>>>>>>>>>>>>>>>>>>*)
					(* remove epilog, prolog, inset, texture and gradients from g *)
					g = ReplacePart[g, #->{}&/@Join[Position[g, Epilog->_, Infinity], Position[g, Prolog->_, Infinity]]];
					g = ReplacePart[g, #->{}&/@Join[Position[g, _Inset, Infinity], Position[g, _Texture, Infinity], (#[[;; -2]] & /@ Position[g, VertexColors->_, Infinity])]];
					
					If[passStyle =!= {},
						
						(* default styles to pass through *)
						absolutedashing = Table[{passStyle[[1]]}, Length[object]];
						absolutethickness = Table[{passStyle[[2]]}, Length[object]];
						absolutepointsize = Table[{passStyle[[3]]}, Length[object]];
						arrowheads = Table[{passStyle[[4]]}, Length[object]];
						capform = Table[{passStyle[[5]]}, Length[object]];
						color = Table[{passStyle[[6]]}, Length[object]];
						dashing = Table[{passStyle[[7]]}, Length[object]];
						strokeform = Table[{passStyle[[8]]}, Length[object]];
						faceform = Table[{passStyle[[9]]}, Length[object]];
						joinform = Table[{passStyle[[10]]}, Length[object]];
						opacity = Table[{passStyle[[11]]}, Length[object]];
						pointsize = Table[{passStyle[[12]]}, Length[object]];
						thickness = Table[{passStyle[[13]]}, Length[object]];
						texture = Table[{passStyle[[14]]}, Length[object]],
						
						(* collect styles *)
						absolutedashing = dwCollectObjectStyles[g, _AbsoluteDashing];
						absolutethickness = dwCollectObjectStyles[g, _AbsoluteThickness];
						absolutepointsize = dwCollectObjectStyles[g, _AbsolutePointSize];
						arrowheads = dwCollectObjectStyles[g, _Arrowheads];
						capform = dwCollectObjectStyles[g, _CapForm];
						color = dwCollectObjectStyles[g, _GrayLevel | _RGBColor | _Hue | _LABColor | _LCHColor | _LUVColor | _XYZColor];
						dashing = dwCollectObjectStyles[g, _Dashing];
						strokeform = dwCollectObjectStyles[g, _EdgeForm(*|_StrokeForm*)]/.{EdgeForm->StrokeForm}; (* StrokeForm in Graphics is not needed since its head is replaced with EdgeForm and a duplicate StrokeForm contains color only *)
						faceform = dwCollectObjectStyles[g, _FaceForm];
						joinform = dwCollectObjectStyles[g, _JoinForm];
						opacity = dwCollectObjectStyles[g, _Opacity];
						pointsize = dwCollectObjectStyles[g, _PointSize];
						thickness = dwCollectObjectStyles[g, _Thickness];
						
						(* remove duplicates *)
						absolutedashing = dwRemoveDuplicateStyles[absolutedashing, Join[faceform, strokeform]];
						absolutethickness = dwRemoveDuplicateStyles[absolutethickness, Join[faceform, strokeform]];
						capform = dwRemoveDuplicateStyles[capform, Join[faceform, strokeform]];
						color = dwRemoveDuplicateStyles[color, Join[faceform, strokeform]];
						dashing = dwRemoveDuplicateStyles[dashing, Join[faceform, strokeform]];
						joinform = dwRemoveDuplicateStyles[joinform, Join[faceform, strokeform]];
						opacity = dwRemoveDuplicateStyles[opacity, Join[faceform, strokeform]];
						thickness = dwRemoveDuplicateStyles[thickness, Join[faceform, strokeform]];
						(*texture = dwRemoveDuplicateStyles[texture, gradients]; (* remove textures which are included in gradients *)*)
						(*object = dwRemoveDuplicateStyles[object, object];*) (* remove objects included in other objects *)(* this works if needed but commented out since it can slow import for complex illustrations *)
						
						(* remove any objects using Small, Medium, Large - Automatic is probably not needed but using as placeholder *)
						absolutedashing = Table[If[Flatten[Cases[n, Small|Medium|Large, Infinity]] === {}, n, Nothing], {n, absolutedashing}];
						absolutethickness = Table[If[Flatten[Cases[n, Small|Medium|Large, Infinity]] === {}, n, Nothing], {n, absolutethickness}];
						absolutepointsize = Table[If[Flatten[Cases[n, Small|Medium|Large, Infinity]] === {}, n, Nothing], {n, absolutepointsize}];
						arrowheads = Table[If[Flatten[Cases[n, Automatic, Infinity]] === {}, n, Nothing], {n, arrowheads}];
						capform = Table[If[Flatten[Cases[n, Automatic, Infinity]] === {}, n, Nothing], {n, capform}];
						color = Table[If[Flatten[Cases[n, Automatic, Infinity]] === {}, n, Nothing], {n, color}];
						dashing = Table[If[Flatten[Cases[n, Small|Medium|Large, Infinity]] === {}, n, Nothing], {n, dashing}];
						strokeform = Table[If[Flatten[Cases[n, Small|Medium|Large, Infinity]] === {}, n, Nothing], {n, strokeform}];
						faceform = Table[If[Flatten[Cases[n, Small|Medium|Large, Infinity]] === {}, n, Nothing], {n, faceform}];
						joinform = Table[If[Flatten[Cases[n, Automatic, Infinity]] === {}, n, Nothing], {n, joinform}];
						opacity = Table[If[Flatten[Cases[n, Automatic, Infinity]] === {}, n, Nothing], {n, opacity}];
						pointsize = Table[If[Flatten[Cases[n, Small|Medium|Large, Infinity]] === {}, n, Nothing], {n, pointsize}];
						thickness = Table[If[Flatten[Cases[n, Small|Medium|Large, Infinity]] === {}, n, Nothing], {n, thickness}];
						
						(* convert dashing and thickness to absolute values; join to include absolutedashing if no dashing; last item of list used *)
						imagesize = If[imagesize === {}, {1,1}, imagesize[[1,-1]]];
						absolutedashing = If[dashing =!= {}, Join[absolutedashing, If[#[[-1,1]]=!={},Flatten[{#[[;;-2]],AbsoluteDashing[imagesize[[1]]*(#[[-1,1]])]}],Nothing]&/@dashing], absolutedashing];
						absolutethickness = If[thickness =!= {}, Join[absolutethickness, Flatten[{#[[;;-2]], If[#[[-1,1]] =!= 1, AbsoluteThickness[imagesize[[1]]*(#[[-1,1]])], AbsoluteThickness[1]]}]&/@thickness], absolutethickness];
						
						(* DEBUG: check objects and collected styles *)
						(*Print@Column[{OpenerView[{"g",g}],OpenerView[{"object",Column[object]}],OpenerView[{"absolutedashing",Column[absolutedashing]}],OpenerView[{"absolutepointsize",Column[absolutepointsize]}],OpenerView[{"absolutethickness",Column[absolutethickness]}],OpenerView[{"arrowheads",Column[arrowheads]}],OpenerView[{"capform",Column[capform]}],OpenerView[{"color",Column[color]}],OpenerView[{"dashing",Column[dashing]}],OpenerView[{"faceform",Column[faceform]}],OpenerView[{"gradients",Column[gradients]}],OpenerView[{"joinform",Column[joinform]}],OpenerView[{"opacity",Column[opacity]}],OpenerView[{"pointsize",Column[pointsize]}],OpenerView[{"strokeform",Column[strokeform]}],OpenerView[{"thickness",Column[thickness]}],OpenerView[{"texture",Column[texture]}]
							}]*)
							
						(*<<<<<<<<<<<<<<<<<<<<<<< MATCH STYLES TO OBJECTS >>>>>>>>>>>>>>>>>>>>>>*)
						absolutedashing = dwMatchStylesToObjects[object, absolutedashing, AbsoluteDashing[{}]];
						absolutethickness = dwMatchStylesToObjects[object, absolutethickness, AbsoluteThickness[1]];
						absolutepointsize = dwMatchStylesToObjects[object, absolutepointsize, AbsolutePointSize[$dwPointSize]];
						arrowheads = dwMatchStylesToObjects[object, arrowheads, $dwDefaultArrowStyle];
						capform = dwMatchStylesToObjects[object, capform, CapForm["Round"]];
						color = dwMatchStylesToObjects[object, color, Black];
						faceform = dwMatchStylesToObjects[object, faceform, $dwDefaultFaceStyle, color];
						gradients = dwMatchStylesToObjects[object, gradients, 0];
						joinform = dwMatchStylesToObjects[object, joinform, JoinForm["Round"]];
						opacity = dwMatchStylesToObjects[object, opacity, Opacity[1]];
						strokeform = dwMatchStylesToObjects[object, strokeform, $dwDefaultStrokeStyle, color];
						texture = dwMatchStylesToObjects[object, texture, 0];
						
						(* add missing faceform parameters *)
						faceform = 
							If[Length[Flatten[{#[[-1,1]]}]] == 1,
								If[Head[Flatten[{#[[-1,1]]}][[1]]] === Opacity,
									{FaceForm[{Black, Flatten[{#[[-1,1]]}][[1]]}]},
									{FaceForm[{Flatten[{#[[-1,1]]}][[1]], Opacity[1]}]}
								],
								#
							]&/@faceform;
						
						(* organize strokeform parameters and add missing parameters *)
						(*strokeform = strokeform/.{Dashing->AbsoluteDashing, Thickness->AbsoluteThickness};*)
						strokeform = Join[#[[;;-2]], {StrokeForm[Flatten[{#[[-1,1]]}]]}]&/@(strokeform/.{Dashing->AbsoluteDashing, Thickness->AbsoluteThickness});
						Do[
							strokeform[[n,-1]] = 
								StrokeForm[Flatten[{
									If[Flatten[Position[strokeform[[n, -1, 1]], GrayLevel|RGBColor|Hue|LABColor|LCHColor|LUVColor|XYZColor, Infinity]] === {}, RGBColor[0,0,0], strokeform[[n, -1, 1, Position[strokeform[[n, -1, 1]], GrayLevel|RGBColor|Hue|LABColor|LCHColor|LUVColor|XYZColor, Infinity][[1,1]]]]],
									If[Flatten[Position[strokeform[[n, -1, 1]], Opacity, Infinity]] === {}, Opacity[1], strokeform[[n, -1, 1, Position[strokeform[[n, -1, 1]], Opacity, Infinity][[1,1]]]]],
									If[Flatten[Position[strokeform[[n, -1, 1]], AbsoluteThickness, Infinity]] === {}, AbsoluteThickness[1], strokeform[[n, -1, 1, Position[strokeform[[n, -1, 1]], AbsoluteThickness, Infinity][[1,1]]]]],
									If[Flatten[Position[strokeform[[n, -1, 1]], AbsoluteDashing, Infinity]] === {}, AbsoluteDashing[{}], strokeform[[n, -1, 1, Position[strokeform[[n, -1, 1]], AbsoluteDashing, Infinity][[1,1]]]]],
									If[Flatten[Position[strokeform[[n, -1, 1]], CapForm, Infinity]] === {}, CapForm["Round"], strokeform[[n, -1, 1, Position[strokeform[[n, -1, 1]], CapForm, Infinity][[1,1]]]]],
									If[Flatten[Position[strokeform[[n, -1, 1]], JoinForm, Infinity]] === {}, JoinForm["Round"], strokeform[[n, -1, 1, Position[strokeform[[n, -1, 1]], JoinForm, Infinity][[1,1]]]]]
								}]],
						{n, Length[strokeform]}];
						
						(* replace missing strokeform objects *)
						strokeform = strokeform/.({1})->$dwDefaultStrokeStyle
					];
					
					(* DEBUG: check objects and matched styles *)
					(*Print@Column[{OpenerView[{"g",g}],OpenerView[{"object",Column[object]}],OpenerView[{"absolutedashing",Column[absolutedashing]}],OpenerView[{"absolutepointsize",Column[absolutepointsize]}],OpenerView[{"absolutethickness",Column[absolutethickness]}],OpenerView[{"arrowheads",Column[arrowheads]}],OpenerView[{"capform",Column[capform]}],OpenerView[{"color",Column[color]}],OpenerView[{"faceform",Column[faceform]}],OpenerView[{"gradients",Column[gradients]}],OpenerView[{"joinform",Column[joinform]}],OpenerView[{"opacity",Column[opacity]}],OpenerView[{"strokeform",Column[strokeform]}],OpenerView[{"texture",Column[texture]}]
						}]*)
					
					(*<<<<<<<<<<<<<<<<<<<<<<< INSERT OBJECTS AND STYLES >>>>>>>>>>>>>>>>>>>>>>*)
					If[OptionValue["SetUndo"], dwSetUndo[], Nothing];
					$dwMessageBarText = "Inserting graphics...";
					If[OptionValue["ReturnObjectValues"],
						
						Return[{object, aspectratio, absolutedashing, absolutepointsize, absolutethickness, arrowheads, 
							capform, color, faceform, gradients, joinform, opacity, plotrange, strokeform, texture, extraScaling, group}],
							
						dwInsertObjectsAndStyles[object, aspectratio, absolutedashing, absolutepointsize, absolutethickness, arrowheads, 
							capform, color, faceform, gradients, joinform, opacity, plotrange, strokeform, texture, extraScaling, group]
					],
					
					MessageDialog["No useable 2D graphics found in file."]
				]
			];
			$dwMessageBarText = "",
			
			MessageDialog["Dynamic objects are not supported"]
		]
		
	]
	
dwInsertObjectsAndStyles[object_, aspectratio_, absolutedashing_, absolutepointsize_, absolutethickness_, arrowheads_, capform_, color_, faceform_, gradients_, joinform_, opacity_, plotrange_, strokeform_, texture_, extraScaling_, group_]:=
	Block[{test={}, newPts, finalFaceform, finalStrokeform, temp, ratio, coordBounds, length = Length[$dwP], gradienttextures = {}, gtype, rescalePts, rescalePtsFinal, reduction},
		(* do not change the current plot range; converting text to graphics causes problems *)
		(*$dwPlotRange = 	If[FreeQ[plotrange, All | Automatic | Full],
							If[Head[plotrange] === List,
								{{plotrange[[1,1]],plotrange[[2,1]]},{plotrange[[1,2]],plotrange[[2,2]]}},
								{{-plotrange,-plotrange},{plotrange,plotrange}}
							],
							{{-1,-1},{1,1}}
						];*)
		newPts = Table[ 
				Which[
					Head[object[[n,-1]]] === Text,
						{object[[n,-1,2]]},
					Head[object[[n,-1]]] === Inset,
						{object[[n,-1,2]]},
					Head[object[[n,-1]]] === Arrow,
						If[MemberQ[{BezierCurve, BSplineCurve, Line}, Head[object[[n,-1,1]]]],
							object[[n,-1,1,1]],
							object[[n,-1,1]]
						],
					Head[object[[n,-1]]] === FilledCurve,
						If[MemberQ[{List}, Head[object[[n,-1,1]]]],
							If[Head[object[[n,-1,1,1,1]]] =!= List,
								object[[n,-1,1,1,1,1]],
								object[[n,-1,1,1,1]]
							],
							object[[n,-1,1,1]]
						],
					True,
						object[[n,-1,1]]
				],
			{n, Length[object]}];

		(* replace Offset, Scaled and ImageScaled coordinates *)
		newPts = newPts/.{Offset[__] | Scaled[__] | ImageScaled[__]->{0,0}};
		
		If[newPts =!= {},
			(* remove extra list nesting *)
			newPts = If[ArrayDepth[#] > 1, Flatten[#, ArrayDepth[#[[1]]] - 1], {#}]&/@(newPts/.{{}->Sequence[]});
			(* fix scale and aspect ratio *)
			If[Max[Flatten[newPts]] > 2(*$dwFixScaleAspectRatioOpenedFile*),
				coordBounds = CoordinateBounds[newPts];
				(*If[FreeQ[Total[Abs[#]]&/@coordBounds, 0.|0],*)
				If[FreeQ[Subtract[Sequence@@#]&/@coordBounds, 0.|0],
					temp = RescalingTransform[coordBounds,{{-1,1},aspectratio{-1,1}}];
					newPts = temp[#]&/@newPts,
					Nothing
				],
				Nothing
			];
			reduction = extraScaling;
			If[extraScaling == 1,
				If[Total[Abs[#]] > 10 && 1/Total[Abs[#]] < reduction, reduction = 1/Total[Abs[#]], Nothing]&/@coordBounds,
				Nothing
			];
			$dwP = Join[$dwP, reduction*newPts];
			$dwHead = Join[$dwHead, 
				Table[ 
					Which[
						Head[object[[n,-1]]] === Arrow,
							If[MemberQ[{BezierCurve, BSplineCurve, Line}, Head[object[[n,-1,1]]]],
								Head[object[[n,-1,1]]],
								Head[object[[n,-1]]]
							],
						Head[object[[n,-1]]] === FilledCurve,
							If[Head[object[[n,-1,1]]] === List,
								If[gradients[[n]] === {0} && texture[[n]] =!= {0},
									If[Length[Flatten[{object[[n,-1,1]]}]] > 1,
										Head[Flatten[{object[[n,-1,1]]}][[1]]],
										Head[object[[n,-1,1,1]]]
									],
									Head[object[[n,-1,1,1]]]
								],
								Head[object[[n,-1,1]]]
							],
						Head[object[[n,-1]]] === Inset,
							Image,
						True,
							Head[object[[n,-1]]]
					],
				{n, Length[object]}]];
			(* add last point of BezierCurve because it was removed when saved by WL-Draw or it never existed if not saved by WL-Draw *)
			Do[
				If[$dwHead[[n]] === BezierCurve,
					$dwP[[n]] = Join[$dwP[[n]], 
						If[$dwP[[n,1]] === $dwP[[n,-1]],
							{$dwP[[n,2]]},
							{$dwP[[n,-1]] + ($dwP[[n,-1]] - $dwP[[n,-2]])}
						]],
					Nothing
				],
			{n, length+1, Length[$dwP]}];
			$dwStyle = Join[$dwStyle,
					Table[
						Switch[Head[object[[n,-1]]],
							Text,
								{
									If[Length[object[[n,-1]]] < 3 || object[[n,-1,1]] === Style, 
										0,
										If[NumericQ[object[[n,-1,1,-1]]],
											object[[n,-1,1,-1]],
											0
										]
									],
									Which[
										Head[object[[n,-1,1]]] === Style, 
											object[[n,-1,1,1]], 
										Length[object[[n,-1]]] < 3, 
											object[[n,-1,1]], 
										True,
											object[[n,-1,1,1,1]]
									],
									If[Length[object[[n,-1]]] < 3, {0,0}, object[[n,-1,3]]],
									temp = Extract[InputForm[object[[n]]], Position[InputForm[object[[n]]], FontFamily->_, Infinity]];
										If[temp === {}, FontFamily->"Source Sans Pro", temp[[1]]],
									temp = Extract[InputForm[object[[n]]], Position[InputForm[object[[n]]], FontSize->_, Infinity]];
										If[temp === {}, FontSize->10, temp[[1]]],
									temp = Extract[InputForm[object[[n]]], Position[InputForm[object[[n]]], FontColor->_, Infinity]];
										If[temp === {}, FontColor->Black, temp[[1]]],
									temp = Extract[InputForm[object[[n]]], Position[InputForm[object[[n]]], FontOpacity->_, Infinity]];
										If[temp === {}, FontOpacity->1, temp[[1]]],
									temp = Extract[InputForm[object[[n]]], Position[InputForm[object[[n]]], FontSlant->_, Infinity]];
										If[temp === {}, FontSlant->Plain, temp[[1]]],
									temp = Extract[InputForm[object[[n]]], Position[InputForm[object[[n]]], FontWeight->_, Infinity]];
										If[temp === {}, FontWeight->Plain, temp[[1]]],
									temp = Extract[InputForm[object[[n]]], Position[InputForm[object[[n]]], FontTracking->_, Infinity]];
										If[temp === {}, FontTracking->"Plain", temp[[1]]],
									temp = Extract[InputForm[object[[n]]], Position[InputForm[object[[n]]], LineSpacing->_, Infinity]];
										If[temp === {}, LineSpacing->{0,12,10}, temp[[1]]],
									None, 1, 0, TextAlignment->Center
								},
							Inset,
								Which[
									Length[object[[n,-1,1]]] == 0 || MemberQ[{Graphics,Graphics3D}, Head[object[[n,-1,1]]]],
									{
										0,
										object[[n,-1,1]],
										1,
										{{
											$dwImageFilterHead,
											$dwImageFilterValue,
											$dwImageFilterValueMin,
											$dwImageFilterValueMax,
											$dwImageFilterColor,
											$dwImageFilterExtraValue
										}},
										False
									},
									Head[object[[n,-1,1,1]]] === Magnify,
									{
										If[MemberQ[{Integer, Real}, Head[object[[n,-1,1,-1]]]], object[[n,-1,1,-1]], 0],
										object[[n,-1,1,1,1]],
										object[[n,-1,1,1,-1]],
										{{
											$dwImageFilterHead,
											$dwImageFilterValue,
											$dwImageFilterValueMin,
											$dwImageFilterValueMax,
											$dwImageFilterColor,
											$dwImageFilterExtraValue
										}},
										False
									},
									True,
									{
										0(*If[MemberQ[{Integer, Real}, Head[object[[n,-1,1,-1]]]], object[[n,-1,1,-1]], 0]*),
										object[[n,-1,1,1]],
										1,
										{{
											$dwImageFilterHead,
											$dwImageFilterValue,
											$dwImageFilterValueMin,
											$dwImageFilterValueMax,
											$dwImageFilterColor,
											$dwImageFilterExtraValue
										}},
										False
									}
								],
							_,
								{
									(*$dwFilledCurve*)  
									If[MemberQ[{FilledCurve, Polygon}, Head[object[[n,-1]]]], True, False],
									(* $dwShowArrowheads *)
									If[Head[object[[n,-1]]] === Arrow,
										If[MemberQ[{BezierCurve, BSplineCurve, Line}, Head[object[[n,-1,1]]]], True, False],
										False
									],
									(* $dwArrowheadSize *)
									If[Head[Flatten[{arrowheads[[n,-1,1]]}][[1]]]===Times,
										ToExpression[StringJoin[Rest[Characters[ToString[Flatten[{arrowheads[[n,-1,1]]}][[1]]]]]]],
										Flatten[{arrowheads[[n,-1,1]]}][[1]]
									],
									(* $dwArrowheadStartPosition *)
									Switch[arrowheads[[n,-1,1]],
										{{_,_},_} | {{_,_}},
											arrowheads[[n,-1,1,1,2]],
										_,
											0
									],
									(* $dwArrowheadEndPosition *)
									Switch[arrowheads[[n,-1,1]],
										{{_,_}},
											arrowheads[[n,-1,1,1,2]],
										{_,{_,_}},
											arrowheads[[n,-1,1,2,2]],
										_,
											1
									],
									(* $dwArrowheadStyle *)
									Switch[arrowheads[[n,-1,1]],
										{{_,_,_},_},
											arrowheads[[n,-1,1,1,3]],
										{_,{_,_,_}},
											arrowheads[[n,-1,1,2,3]],
										_,
											"Default"
									],
									(* $dwPointSize *)
									absolutepointsize[[n,-1,1]],
									(* $dwDefaultGradient *)
									If[MemberQ[{0, {0}}, texture[[n]]],
										$dwDefaultGradient,
										$dwDefaultGradient/.{None->Texture}
									],
									$dwDefaultGradientParameters,
									Which[
										Head[object[[n,-1]]] === BSplineCurve,
											pos = Flatten[Position[object[[n,-1]], SplineClosed->_, Infinity]];
											If[pos =!= {},
												Extract[object[[n,-1]], pos][[-1]],
												$dwDefaultSplineClosed
											],
										Head[object[[n,-1,1]]] === BSplineCurve,
											pos = Flatten[Position[object[[n,-1,1]], SplineClosed->_, Infinity]];
											If[pos =!= {},
												Extract[object[[n,-1,1]], pos][[-1]],
												$dwDefaultSplineClosed
											],
										True,
											$dwDefaultSplineClosed
									],
									Which[
										Head[object[[n,-1]]] === BSplineCurve,
											pos = Flatten[Position[object[[n,-1]], SplineDegree->_, Infinity]];
											If[pos =!= {},
												Extract[object[[n,-1]], pos][[-1]],
												$dwDefaultSplineDegree
											],
										Head[object[[n,-1,1]]] === BSplineCurve,
											pos = Flatten[Position[object[[n,-1,1]], SplineDegree->_, Infinity]];
											If[pos =!= {},
												Extract[object[[n,-1,1]], pos][[-1]],
												$dwDefaultSplineDegree
											],
										True,
											$dwDefaultSplineDegree
									],
									(* arrowhead quantity *)
									$dwArrowheadQuantity,
									(* $dwStrayColor *)
									If[color =!= {}, color[[n,-1]], $dwStrayColor],
									(* $dwStrayOpacity *)
									$dwStrayOpacity(*If[opacity =!= {}, opacity[[n,-1]], $dwStrayOpacity]*),
									faceform[[n,-1]],
									strokeform[[n,-1]],
									arrowheads[[n,-1]],
									absolutepointsize[[n,-1]]
								}
						], 
					{n, Length[object]}]
				];
				
			(* textures *)
			$dwObjectGradients = Join[$dwObjectGradients, If[# === {0}, {}, #[[-1]]]&/@texture];
			
			(* UPDATE line gradients *)
			$dwLineGradients = Join[$dwLineGradients, Table[$dwDefaultGradient, {n, Length[object]}]];
			
			(* UPDATE animation *)
			$dwAnimate = Join[$dwAnimate, Table[{$dwP[[n-(Length[object]+1)]], $dwStyle[[n-(Length[object]+1)]], 0, {1,1}, {1,30,30,30}, 1, 1, 30, 30, 240, False, {{0,0}}}, {n, Length[object]}]];
			
			(* add extra compound path objects - must be done here since object split while collecting will be deleted as a duplicate *)
			temp = Position[object, _FilledCurve, Infinity];
			If[temp =!= {},
				Do[If[Head[object[[Sequence@@n,1]]] === List,
					If[Length[object[[Sequence@@n,1]]] > 1,
						Do[
							If[$dwFixScaleAspectRatioOpenedFile,
								If[FreeQ[Total[Abs[#]]&/@coordBounds, 0.|0],
									rescalePts = RescalingTransform[coordBounds,{{-1,1},aspectratio{-1,1}}];
									obj[[1]] = rescalePts[#]&/@obj[[1]]
								]
							];
							$dwP = Join[$dwP, {obj[[1]]}];
							$dwHead = Join[$dwHead, {Head[obj]}];
							$dwStyle = Join[$dwStyle, If[Cases[texture, {n[[1]], ___}, 2] =!= {}, {$dwFullDefaultStyle/.{None->Texture}}, {$dwFullDefaultStyle}]], 
						{obj, Flatten[{object[[Sequence@@n,1]][[2;;-1]]}]}];
						$dwCompoundPathLayers = Join[$dwCompoundPathLayers, {Flatten[{length+n[[1]], Range[Length[$dwP]-(Length[object[[Sequence@@n,1]]]-2), Length[$dwP]]}]}];
						$dwObjectGradients = Join[$dwObjectGradients, {If[Cases[texture, {n[[1]], ___}, 2] =!= {}, Cases[texture, {n[[1]], ___}, 2][[1, -1]], {}]}],
						Nothing
					]
				],{n, temp}]
			];
				
			(* UPDATE bounding boxes *)
			$dwBoundingBoxes = Join[$dwBoundingBoxes, Table[{}, {n, Length[$dwP] - length(*Length[object]+Length[temp]*)}]];
			dwUpdateBoundingBox[Range[length + 1, Length[$dwP]]];
			
			(* UPDATE selection *)
			$dwSelected = Range[length + 1, Length[$dwP]];
			
			(* GROUP new layers *)
			If[Length[$dwSelected] > 1 && group,
				$dwGroupLayers = Join[$dwGroupLayers, {$dwSelected}]
			];
			
			(* UPDATE *)
			$dwStyleMode = 
				If[$dwSelected =!= {},
					Switch[$dwHead[[$dwSelected[[1]]]],
						Image, "image",
						Text, "text",
						Line, "stroke",
						Arrow, "arrow",
						Point, "point",
						_, "fill"
					]
				];
			$dwPStart = $dwP;
			$dwObjectQuantity = Length[$dwP],
			
			MessageDialog["No usable coordinates found."]
		]
	]

(* 	returns a style for each object; starts with default style, checks first list, checks object list *)
dwMatchStylesToObjects[obj_, style_, defaultstyle_, color_:{}]:=
	Block[{finalcolor = {}, straystyle, temp, temp2, temp3},
	(* collect stray color *)
	If[color =!= {},
		Do[
			Do[
				If[Length[color[[cn]]] === 1,
					AppendTo[finalcolor, color[[cn,1]]],
					If[
						color[[cn]][[;;Min[Length[color[[cn]][[;;-2]]], Length[obj[[objn]][[;;-2]]]]]][[-1]] <
							obj[[objn]][[;;Min[Length[color[[cn]][[;; -2]]], Length[obj[[objn]][[;;-2]]]]]][[-1]],
								AppendTo[finalcolor, color[[cn,-1]]]; Break[],
								Nothing
					]
				], 
			{cn, Length[color], 1, -1}],
		{objn, Length[obj]}]
	];
	
	(* if color missing use default style *)
	straystyle = 
		Table[
			If[style =!= {} && (Length[style[[1]]] < 4 || (Length[style[[1]]] < 7 && Flatten[Position[style[[1]], VertexTextureCoordinates, Infinity]] =!= {})), 
				style[[1]], 
				If[color =!= {} && Length[finalcolor] >= n,
					{defaultstyle/.{GrayLevel[0]->finalcolor[[n]]}},(* GrayLevel[0] for StrokeForm *)
					{defaultstyle}
				]
			],
		{n, Length[obj]}];
	
	(* return style for each object *)
	Table[
		temp = Extract[style, Position[style, Join[obj[[n]][[;;-3]],{___,_[___]}], Infinity]];
		If[temp =!= {} && Length[obj[[n]]] > If[Head[temp[[-1,-1]]] === Line, 2, 3],(* Line is for WL-Draw gradient *)
			(* find closest style not within object *)
			temp2 = 
				Table[
					If[
						If[defaultstyle === 0 || Head[obj[[n,-1]]] === FilledCurve, 
							True, 
							Length[obj[[n]]] >= Length[temp[[n2]]]
						] && obj[[n,-2]] > temp[[n2,Length[obj[[n]][[;; -2]]]]],
						
		  				temp[[n2]],
		  				Nothing
		  			],
		 		{n2, Length[temp], 1, -1}];
		 		
		 	If[temp2 === {}, 
		 		straystyle[[n]], 
		 		temp2[[1]]
		 	],
		 	
		 	(* use stray style or default style *)
			straystyle[[n]]
		],
	{n, Length[obj]}]
]
	
dwCollectObjectStyles[g_, style_]:=
	Block[{},
		Append[#, Extract[g, #]]&/@Position[g, style, Infinity]
	]

(* removes duplicates by looking at position numbers; if both inputs are identical items included within other items are removed *)
dwRemoveDuplicateStyles[item_, duplicates_]:=
	Block[{finalitems = {}},
		If[item === duplicates,
			
			Do[
				Do[
					If[i === d,
						Nothing,
						If[i[[;;Min[Length[d[[;;-2]]],Length[i[[;;-2]]]]]] === d[[;;Min[Length[d[[;;-2]]],Length[i[[;;-2]]]]]], AppendTo[finalitems, If[Length[i] < Length[d], d, i]]; Break[], Nothing]
					],
				{d, duplicates}],
			{i, item}];
			Complement[item, finalitems],
			
			Do[
				Do[
					If[i[[;;Min[Length[d[[;;-2]]],Length[i[[;;-2]]]]]] === d[[;;Min[Length[d[[;;-2]]],Length[i[[;;-2]]]]]], AppendTo[finalitems, i]; Break[], Nothing],
				{d, duplicates}],
			{i, item}];
			Complement[item, finalitems]
		]
	]
	
(*	input: Graphics
	output: {object, gradients, texture} *)
dwCollectObjects[g_]:=
	Block[{newg, object, newobject = {}, gradients = {}, texture, minobjectposlength, inset, filledcurve, primitives, pts},
		(* remove Annotations used in WL visualizations such as Plot *)
		newg = ReplacePart[g, Table[n->g[[Sequence@@Append[n, 1]]], {n, Position[g, _Annotation, Infinity]}]];
		newg = newg/.{LineBox->Line, ArrowBox->Arrow, PolygonBox->Polygon, PointBox->Point, BezierCurveBox->BezierCurve, BSplineCurveBox->BSplineCurve, FilledCurveBox->FilledCurve, TextureBox->Texture};
		
		(* check for single primitive without list *)
		newg = If[Head[newg[[1]]] =!= List, ReplacePart[newg, 1->List[newg[[1]]]], newg];
		
		(*<<<<<<<<<<<<<<<<<<<<<<< COLLECT OBJECTS >>>>>>>>>>>>>>>>>>>>>>*)
		
		(* collect textures *)
		texture = Append[#, Extract[newg, #]]&/@Position[newg, _Texture, Infinity];
		newg = ReplacePart[newg, #->{}&/@Position[newg, Texture[_], Infinity]];
		
		(* collect insets;  1) replace Head with Text or Image;  2) remove Inset from g *)
		inset = 
			Which[
				MemberQ[{String, Symbol, Style}, Head[#[[-1,1]]]], (* text *)
					#/.Inset->Text,
				MemberQ[{Graphics, Graphics3D, GraphicsComplex, Image}, Head[#[[-1,1]]]], (* image or graphics *)
					#,
				Head[#[[-1,1]]] === Rotate,
					Which[
						MemberQ[{String, Symbol}, Head[#[[-1,1,1]]]], (* text without Style wrapper *)
						#/.Inset->Text, 
						MemberQ[{Graphics, Graphics3D, GraphicsComplex, Image}, Head[#[[-1,1,1]]]], (* image or graphics *)
						#, 
						MemberQ[{String, Symbol}, Head[#[[-1,1,1,1]]]], (* text with Style wrapper *)
						#/.Inset->Text, 
						True, #
					],
				True, #
			]&/@Table[Append[i, Extract[newg, i]], {i, Position[newg, _Inset, Infinity]}];

		newg = ReplacePart[newg, #->{}&/@Position[newg, Epilog->_, Infinity]];
		newg = ReplacePart[newg, #->{}&/@Position[newg, Prolog->_, Infinity]];
		newg = ReplacePart[newg, #->{}&/@Position[newg, _Inset, Infinity]];
		
		(* remove FilledCurve objects using coordinate list instead of Line, BezierCurve or BSplineCurve *)
		(*newg = newg/.{FilledCurve[{{_},_},_]->{}};*)
		
		(* collect FilledCurve *)
		filledcurve = Append[#, Extract[newg, #]]&/@Position[newg, _FilledCurve, Infinity];
		
		filledcurve = 
			If[Length[#[[-1,1,1,1]]] == 3,
				(* FilledCurve objects using FilledCurve[{{{object type (i.e. Line, BezierCurve, etc.), number of points, spline degree}, ...},...}},{{coordinate list}, ...}] *)
				primitives=GeometricFunctions`DecodeFilledCurve[#[[-1]]];
				Table[
					pts = 
						Flatten[
							Table[
								Switch[Head[prim],
									Line,
										If[Length[prim[[1]]] > 1,
											Sequence@@Table[{nPrim,nPrim}, {nPrim, prim[[1]]}],
											Table[prim[[1,1]], 3]
										],
									_,
										prim[[1]]
								],
							{prim, primitive}],
						1];
					ReplacePart[#,-1->FilledCurve[BezierCurve[
						Join[pts, pts[[{1,-1}]]][[;;3(IntegerPart[Length[pts]/3])+1]]
					]]],
				{primitive,primitives[[1]]}],
				(* FilledCurve objects using FilledCurve[{Line,BezierCurve or BSplineCurve}] *)
				If[Flatten[Position[#, _Line | _BezierCurve | _BSplineCurve, Infinity]] === {}, Nothing, #]
			]&/@filledcurve;
			
		filledcurve = If[filledcurve =!= {}, If[ArrayDepth[#] > 1, Sequence@@#, #]&/@(filledcurve/.{{}->Sequence[]}), filledcurve];
		
		(* remove FilledCurve objects *)
		newg = ReplacePart[newg, #->{}&/@Position[newg, FilledCurve[___], Infinity]];
		
		(* collect remaining objects *)
		object = Append[#, Extract[newg, #]]&/@Position[newg, _Line | _Arrow | _Polygon | _Point | _BezierCurve | _BSplineCurve, Infinity];
		
		(* join all objects *)
		object = Join[filledcurve, object, inset];
		
		If[object =!= {},
			object = Sort[object, 0];(* use 0 so ties treated identical *)
			(* collect gradients
				1. textures must be present in object
				2. previous sort puts gradient before object
				3. locate objects with same position
				4. if VertexColors found save as gradient
			*)
			Do[
				If[object[[n]][[;;Min[Length[object[[n]]],Length[object[[n+1]]]]-1]] === object[[n+1]][[;;Min[Length[object[[n]]],Length[object[[n+1]]]]-1]],
					If[Position[object[[n]], VertexColors, Infinity] =!= {},
						AppendTo[gradients, object[[n]]],
						AppendTo[newobject, object[[n]]]
					],
					AppendTo[newobject, object[[n]]]
				],
			{n, Length[object]-1}];
			
			newobject = Join[newobject, {object[[-1]]}];
			newobject = Delete[newobject, #[[;;-2]]&/@Position[(If[Head[#[[-1]]] =!= Inset, #, Nothing]&/@newobject), _Texture, Infinity]];
			
			Return[{newobject, gradients, texture}],
			
			Return[{{}, {}, {}}]
		]
	]

End[] (* End Private Context *)

EndPackage[]