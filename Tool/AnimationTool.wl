(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(* start and $dwAnimate: {pts, style, rotation, scale, {begin, peak, peak range, end}, ramp, arrowSpeed, frameTotal, framePerSecond, frameWidth, removeLastFrame, origin (* always second to last *), motion function points (always last)} *)
dwAnimate[]:=
	CreateDialog[
		DynamicModule[{plotrange, multiplier, imagesize, start = Table[{$dwP[[n]], $dwStyle[[n]], 0, {1,1}, {1,30,30,30}, 1, 1, 30, 30, 240, False, {0,0}, {{0,0}}}, {n, Length[$dwP]}], temp, hwTemp, prTemp, framePerSecond = 30, originSide = "Center", 
			removeLastFrame = False, peak = {1,30,20,30}, ramp = 1, arrowSpeed = 1, repeat = Infinity, continuousArrows = True, filename, objPointNum = Null, pointLoc, startPointLoc,
			defaultPath = {{0,0},{.2,0},{.2,.2},{0,.2}}, frameWidth = 450, ctr, obj = 1, cpList, rotateFunction, scaleFunction, rotate = 0, scale = {1,1}, frame = 1, frameTotal = 30, 
			matchOffset = {0.,0.}, findOffset, objectID = False, mouseAnnotation},
			dwConvertPointSelectionToLayerSelection[];
			$dwSelected = {};
			If[Head[$dwAnimate] =!= List || Length[$dwAnimate] =!= Length[$dwP],
				$dwAnimate = start,
				Do[If[Length[$dwAnimate[[n,1]]] =!= Length[$dwP[[n]]], $dwAnimate[[n,1]] = $dwP[[n]], Nothing], {n, Length[$dwP]}]
			];
			(* image,expression and text transforms *)
			Do[
				Switch[$dwHead[[n]],
					Image|"Expression",
						start[[n,3]] = $dwStyle[[n,1]];
						start[[n,4]] = {$dwStyle[[n,3]], $dwStyle[[n,3]]},
					Text,
						start[[n,3]] = $dwStyle[[n,1]],
					_,
						Nothing
				],
			{n, Length[$dwP]}];
			pointLoc = startPointLoc = #[[1]]&/@$dwAnimate;
			{rotate, scale, peak, ramp, arrowSpeed, frameTotal, framePerSecond, frameWidth, removeLastFrame} = $dwAnimate[[obj,{3, 4, 5, 6, 7, 8, 9, 10, 11}]];
			
			Pane[
				Grid[{
					{
						Grid[{{
							Column[{
								Style["TRANSITION", 18, Bold],
								Grid[{
									{Row[{Dynamic@dwAnimateStyleSettings[obj]}](* must be dynamic or styles not updated *)
									},{
									Grid[{
											{
												"rotate",
												EventHandler[
													Slider[Dynamic[rotate], {-360, 360, 1}],{
													"MouseUp":>(
														rotateFunction = RotationTransform[rotate*Pi/180];
														$dwAnimate[[obj,1]] = rotateFunction[$dwAnimate[[obj,1]]];
														$dwAnimate[[obj,3]] = rotate;
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
														)
													}, PassEventsDown->True],
												Pane[Dynamic@rotate, ImageSize->30] (* set right pane size here *)
											},
											{
												"scale horizontal",
												EventHandler[
													Slider[Dynamic[scale[[1]]], {0, 2, .01}],{
													"MouseUp":>(
														scaleFunction = ScalingTransform[scale];
														$dwAnimate[[obj,1]] = scaleFunction[$dwAnimate[[obj,1]]];
														$dwAnimate[[obj,4]] = scale;
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
														)
													}, PassEventsDown->True],
												Pane[Dynamic[scale[[1]]]]
											},
											{
												"scale verticle",
												EventHandler[
													Slider[Dynamic[scale[[2]]], {0, 2, .01}],{
													"MouseUp":>(
														scaleFunction = ScalingTransform[scale];
														$dwAnimate[[obj,1]] = scaleFunction[$dwAnimate[[obj,1]]];
														$dwAnimate[[obj,4]] = scale;
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
														)
													}, PassEventsDown->True],
												Pane[Dynamic[scale[[2]]]]
											},
											{
												"arrow speed",
												EventHandler[
													Slider[Dynamic[arrowSpeed], {0, 2,
														If[continuousArrows && MemberQ[$dwShapeSymbols,$dwHead[[obj]]],
															temp = Length[start[[obj,2,Flatten[Position[start[[obj,2]], Arrowheads[_]]][[1]],1]]];
															If[temp > 2, 1/temp, .01],
															.01
														]
														}],{
													"MouseUp":>(
														$dwAnimate[[obj,7]] = arrowSpeed;
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
														)
												}, PassEventsDown->True],
												Pane[Dynamic@arrowSpeed]
											},
											{Column[{"accelerate", Null}, Spacings->{0,0}],
												Column[{
													EventHandler[
														Slider[Dynamic[ramp], {.1, 2, .05}],{
														"MouseUp":>(
															$dwAnimate[[obj,6]] = ramp;
															dwUpdateFrameData[start, frame, frameTotal, pointLoc]
															)
													}, PassEventsDown->True],
													Row[{Style["rapid start         constant             rapid end", Italic]}]
												}, Spacings->{0,0}],
												Column[{Pane[Dynamic@ramp], Null}, Spacings->{0,0}]
											},
											{""},
											{Row[{Checkbox[Dynamic@continuousArrows], " cycle arrow", 
												Spacer[10], Checkbox[Dynamic@$dwAnimatePoints], " show object points",
												Spacer[10], Checkbox[Dynamic@objectID], " show object ID", Spacer[20]}], SpanFromLeft, SpanFromLeft},
											{"FRAME",Pane[Grid[{{ 
												Dynamic@ActionMenu[Dynamic@peak[[1]], 
													With[{n = #}, n:>(
														peak[[1]] = n; 
														$dwAnimate[[obj,5,1]] = n; 
														If[peak[[2]] < peak[[1]], peak[[2]] = peak[[1]]; $dwAnimate[[obj,5,2]] = $dwAnimate[[obj,5,1]]];
														If[peak[[4]] < peak[[1]], peak[[4]] = peak[[1]]; $dwAnimate[[obj,5,4]] = $dwAnimate[[obj,5,1]]];
														If[peak[[3]] < peak[[2]], peak[[3]] = peak[[2]]; $dwAnimate[[obj,5,3]] = $dwAnimate[[obj,5,2]]];
														If[peak[[3]] > peak[[4]], peak[[3]] = peak[[4]]; $dwAnimate[[obj,5,3]] = $dwAnimate[[obj,5,4]]];
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
													)]&/@Range[frameTotal], Appearance->"PopupMenu"], 
												Dynamic@ActionMenu[Dynamic@peak[[2]], 
													With[{n = #}, n:>(
														peak[[2]] = n; 
														$dwAnimate[[obj,5,2]] = n; 
														If[peak[[3]] < peak[[2]], peak[[3]] = peak[[2]]; $dwAnimate[[obj,5,3]] = $dwAnimate[[obj,5,2]]];
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
													)]&/@Range[peak[[1]], peak[[4]]], Appearance->"PopupMenu"], 
												Dynamic@ActionMenu[Dynamic@peak[[3]], 
													With[{n = #}, n:>(
														peak[[3]] = n; 
														$dwAnimate[[obj,5,3]] = n; 
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
													)]&/@Range[peak[[2]],peak[[4]]], Appearance->"PopupMenu"],
												Dynamic@ActionMenu[Dynamic@peak[[4]], 
													With[{n = #}, n:>(
														peak[[4]] = n; 
														$dwAnimate[[obj,5,4]] = n;
														If[peak[[2]] > peak[[4]], peak[[2]] = peak[[4]]; $dwAnimate[[obj,5,2]] = $dwAnimate[[obj,5,4]]];
														If[peak[[3]] < peak[[2]], peak[[3]] = peak[[2]]; $dwAnimate[[obj,5,3]] = $dwAnimate[[obj,5,2]]];
														If[peak[[3]] > peak[[4]], peak[[3]] = peak[[4]]; $dwAnimate[[obj,5,3]] = $dwAnimate[[obj,5,4]]];
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
													)]&/@Range[peak[[1]], frameTotal], Appearance->"PopupMenu"]
												},{"begin","peak","peak end","end"}},Spacings->{0,Automatic}], Alignment->Left, ImageSize->245], SpanFromLeft},
												
											{"ORIGIN", Pane[Row[{
												Dynamic@ActionMenu[Round[$dwAnimate[[obj,-2]], .001], Flatten[{
													"Choose object ID":>(Nothing),
													{0, 0}:>($dwAnimate[[obj,-2]] = {0,0}; dwUpdateFrameData[start, frame, frameTotal, pointLoc]),
													obj:>($dwAnimate[[obj,-2]] = dwFindOrigin[obj, originSide, start]; dwUpdateFrameData[start, frame, frameTotal, pointLoc]),
													Delimiter,
													With[{n = #}, n:>(
														$dwAnimate[[obj,-2]] = dwFindOrigin[n, originSide, start];
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
													)]&/@Complement[Range[Length[$dwP]], {obj}]
												}],Appearance->"PopupMenu"], 
												Dynamic@ActionMenu[Dynamic@originSide, 
													With[{side = #}, side:>(
														originSide = side; 
														$dwAnimate[[obj,-2]] = dwFindOrigin[obj, side, start]; 
														dwUpdateFrameData[start, frame, frameTotal, pointLoc]
													)]&/@{"Center","LeftTop","Top","RightTop","Left","Right","LeftBottom","Bottom","RightBottom"},
												Appearance->"PopupMenu"]
											}], Alignment->Left, ImageSize->245], SpanFromLeft}
										}, Alignment->{{Right, Center, Left}}]
								}}(*, Background->LightGray, Frame->True, FrameStyle -> {AbsoluteThickness[5], LightGray}*)],
								"",
								Row[{Style["PREVIEW ", 18, Bold],
									" Drag 'frame' slider to preview. "
								}],
								Grid[{{
										dwPreviewButton[], 
										Row[{"  frame  ",
											EventHandler[
												Slider[Dynamic[frame], {1, Dynamic@frameTotal, 1}, ImageSize->240],
												{"MouseUp":>(dwUpdateFrameData[start, frame, frameTotal, pointLoc])},
											PassEventsDown->True], 
											Button["<", frame = If[frame > 1, --frame, frame]; dwUpdateFrameData[start, frame, frameTotal, pointLoc], Appearance->"Palette"],
											Button[">", frame = If[frame < frameTotal, ++frame, frameTotal]; dwUpdateFrameData[start, frame, frameTotal, pointLoc], Appearance->"Palette"],
											"  ", Pane[Dynamic@frame, ImageSize->30]
										}]
									},{
										SpanFromAbove,
										Row[{"  select  ",
											EventHandler[
												Slider[Dynamic@obj, {1, Length[$dwP], 1}, ContinuousAction->False, ImageSize->240],
												{
													"MouseUp":>(
														{rotate, scale, peak, ramp, arrowSpeed} = $dwAnimate[[obj, {3,4,5,6,7}]]
													)
											}, PassEventsDown->True],
											Button["<", obj = If[obj > 1, --obj, obj]; {rotate, scale, peak, ramp, arrowSpeed} = $dwAnimate[[obj, {3,4,5,6,7}]], Appearance->"Palette"],
											Button[">", obj = If[obj < Length[$dwP], ++obj, obj]; {rotate, scale, peak, ramp, arrowSpeed} = $dwAnimate[[obj, {3,4,5,6,7}]], Appearance->"Palette"],
											"  ", Pane[Dynamic@obj, ImageSize->30]
										}]
								}}, Alignment->{{Left},{Center}}, Spacings->{0,0}]
							}],
							
							Column[{
								Style["MOTION PATH", 18, Bold],
								Grid[{
									{	Dynamic@Button[If[Length[$dwAnimate[[obj,-1]]] != 4, "ADD bezier motion path to selected object.","REMOVE bezier motion path from selected object"],
											If[Length[$dwAnimate[[obj,-1]]] != 4,
												cpList = If[FreeQ[$dwCompoundPathLayers, obj], {obj}, $dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, obj]][[1]]]]];
												ctr = dwFindCenter[Join[Sequence@@Table[start[[each,1]], {each, cpList}]]];
												$dwAnimate[[obj,-1]] = ctr+#&/@defaultPath;
												$dwAnimatePoints = False,
												$dwAnimate[[obj,-1]] = {{0,0}}
											], ImageSize->350]
									},{
										If[Length[$dwP] > 1,
											
											Grid[{{
													"Add this motion path to:",
													Dynamic@ActionMenu["choose unselected object", Table[
														With[{n=n},n:>(
																findOffset = dwFindCenter[start[[n,1]]] - dwFindCenter[start[[obj,1]]];
																$dwAnimate[[n,-1]] = #+matchOffset+findOffset&/@$dwAnimate[[obj,-1]]
															)
														], {n, Complement[Range[Length[$dwP]], {obj}]}], Appearance->"PopupMenu"]
												},{
													"with offset:",
													Row[{
														"  X ",
														Dynamic@PopupMenu[Dynamic[matchOffset[[1]]], Table[n, {n, -.1,.1,.025}]],
														"     Y ",
														Dynamic@PopupMenu[Dynamic[matchOffset[[2]]], Table[n, {n, -.1,.1,.025}]]
													}]
											}}, Alignment->{{Right, Left}}],
											
											Grid[{{" "},{" "}}]
										]
									}
								}, Alignment->Left(*, Background->LightGray, Frame->True, FrameStyle -> {AbsoluteThickness[8], Gray}*)],
								"",
								Grid[{
									{Style["FRAMES", 18, Bold], Row[{PopupMenu[Dynamic@repeat, Join[{Infinity},Range[10]]], " repeat", Spacer[20], Checkbox[Dynamic@removeLastFrame], " remove last frame"}], SpanFromLeft},
									{"total frames ", 
										EventHandler[
											SetterBar[Dynamic@frameTotal, {2,30,60,90,120,150,180,210}],{
											"MouseUp":>(
												(* maintain ratio of parameters out of range *)
												Do[
													Do[$dwAnimate[[n,5,n2]] = 
														If[$dwAnimate[[n,5,n2]] == 1, 1,IntegerPart[$dwAnimate[[n,5,n2]]*(frameTotal/$dwAnimate[[obj,8]])]], 
													{n2, Length[$dwAnimate[[n,5]]]}], 
												{n, Length[$dwAnimate]}];
												peak = Table[
														If[peak[[n]] == 1, 1, IntegerPart[peak[[n]]*(frameTotal/$dwAnimate[[obj,8]])]], 
													{n, Length[peak]}];
												(* update parameters *)
												$dwAnimate[[obj,8]] = frameTotal;
												If[frame > frameTotal, frame = frameTotal, Nothing];
												dwUpdateFrameData[start, frame, frameTotal, pointLoc]
												)
											}, PassEventsDown->True], Null},
									{"frames per second ", 
										EventHandler[
											SetterBar[Dynamic@framePerSecond, {1,5,10,15,24,25,30,45,60}],{
											"MouseUp":>(
												$dwAnimate[[obj,9]] = framePerSecond;
												dwUpdateFrameData[start, frame, frameTotal, pointLoc]
												)
											}, PassEventsDown->True], Null},
									{"frame width ", 
										EventHandler[
											SetterBar[Dynamic@frameWidth, {180,240,300,360,720,1920}],{
											"MouseUp":>(
												$dwAnimate[[obj,10]] = frameWidth;
												dwUpdateFrameData[start, frame, frameTotal, pointLoc]
												)
											}, PassEventsDown->True], Null},
									{"total frames ", 
										EventHandler[
											Slider[Dynamic@frameTotal, {2,210,1}],{
											"MouseUp":>(
												(* maintain ratio of parameters out of range *)
												Do[
													Do[$dwAnimate[[n,5,n2]] = 
														If[$dwAnimate[[n,5,n2]] == 1, 1,IntegerPart[$dwAnimate[[n,5,n2]]*(frameTotal/$dwAnimate[[obj,8]])]], 
													{n2, Length[$dwAnimate[[n,5]]]}], 
												{n, Length[$dwAnimate]}];
												peak = Table[
														If[peak[[n]] == 1, 1, IntegerPart[peak[[n]]*(frameTotal/$dwAnimate[[obj,8]])]], 
													{n, Length[peak]}];
												(* update parameters *)
												$dwAnimate[[obj,8]] = frameTotal;
												If[frame > frameTotal, frame = frameTotal, Nothing];
												dwUpdateFrameData[start, frame, frameTotal, pointLoc]
												)
											}, PassEventsDown->True], 
										Pane[Dynamic@frameTotal]},
									{"frames per second ", 
										EventHandler[
											Slider[Dynamic@framePerSecond, {1,60,1}],{
											"MouseUp":>(
												$dwAnimate[[obj,9]] = framePerSecond;
												dwUpdateFrameData[start, frame, frameTotal, pointLoc]
												)
											}, PassEventsDown->True], 
										Pane[Dynamic@framePerSecond]},
									{"frame width ", 
										EventHandler[
											Slider[Dynamic@frameWidth, {50,720,1}, ContinuousAction->False],{
											"MouseUp":>(
												$dwAnimate[[obj,10]] = frameWidth;
												dwUpdateFrameData[start, frame, frameTotal, pointLoc]
												)
											}, PassEventsDown->True], 
										Pane[Dynamic@frameWidth]}
								}, Alignment->{{Right, Left, Left}}(*, Background->LightGray, Frame->True, FrameStyle -> {AbsoluteThickness[5], LightGray}*)],
								"",
								Row[{"  RESET SELECTED OBJ  ", 
									Button["Points", 
										Do[$dwP[[n]] = $dwAnimate[[n,1]] = pointLoc[[n]] = startPointLoc[[n]] = start[[n,1]], {n, If[FreeQ[$dwCompoundPathLayers, obj], {obj}, $dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, obj]][[1]]]]]}];
										dwUpdateFrameData[start, frame, frameTotal, pointLoc]
									],
									Button["Style", 
										$dwAnimate[[obj,1;;2]] = start[[obj,1;;2]]; 
										$dwStyle[[obj]] = start[[obj,2]];
										$dwAnimateHandle = Null;
										dwUpdateFrameData[start, frame, frameTotal, pointLoc]
									],
									Button["Transform", 
										{rotate, scale, peak, ramp, arrowSpeed} = $dwAnimate[[obj,{3,4,5,6,7}]] = start[[obj,{3,4,5,6,7}]];
										$dwAnimateHandle = Null;
										dwUpdateFrameData[start, frame, frameTotal, pointLoc]
									],
									Spacer[{0,22}]
								}],
								Row[{
									Column[{
										Button[Style["Reset all objects",10], 
											$dwAnimate = start;
											(* compound paths *)
											Do[
												cpList = If[FreeQ[$dwCompoundPathLayers, n], {n}, $dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]]];
												$dwAnimate[[n,-2]] = dwFindCenter[Join[Sequence@@Table[start[[each,1]], {each, cpList}]]] + defaultPath[[1]],
											{n, Length[$dwP]}];
											(* update variables *)
											pointLoc = startPointLoc = #[[1]]&/@$dwAnimate;
											{rotate, scale, peak, ramp, arrowSpeed} = $dwAnimate[[obj,{3,4,5,6,7}]];
											dwUpdateFrameData[start, frame, frameTotal, pointLoc],
										ImageSize->{115,25}],
										Tooltip[
											Button[Style["Set group",10], 
												cpList = If[FreeQ[$dwGroupLayers, obj], {obj}, $dwGroupLayers[[Flatten[Position[$dwGroupLayers, obj]][[1]]]]];
												Do[$dwAnimate[[n, #]] = $dwAnimate[[obj, #]], {n, Complement[cpList, {obj}]}]&/@{3, 4, 5, 6, 7};(* rotate, scale, peak, ramp, arrowSpeed *)
												temp = dwFindCenter[start[[obj,1]]];
												Do[$dwAnimate[[n, -2]] = temp, {n, cpList}];
												Do[$dwAnimate[[n, -1]] = (dwFindCenter[start[[n,1]]] - temp) + #&/@$dwAnimate[[obj, -1]], {n, cpList}];
												dwUpdateFrameData[start, frame, frameTotal, pointLoc],
											ImageSize->{115,25}],
											"Pass transforms of selected object to all objects in its group"]
									}, Spacings->0],
									Button["Export",
										DialogReturn[];
										$dwMessageBarText = "Rendering animation...";
										MessageDialog[Dynamic@$dwMessageBarText, Enabled->Dynamic@If[$dwMessageBarText === "Rendering animation...", False, True]];
										filename = "/animation-"<>ToString[AbsoluteTime[DateString[]]]<>".png";
										(* calculate plot range if preset size (1920x1080) *)
										If[frameWidth == 1920,
											prTemp = Partition[Flatten[$dwPlotRange][[{1, 3, 2, 4}]], 2];
											hwTemp = {Subtract[Sequence @@ prTemp[[2]]], Subtract[Sequence @@ prTemp[[1]]]};
											multiplier = (1 + ((Abs[Divide[Sequence@@hwTemp]] - (1080/1920))/hwTemp[[1]]));
											plotrange = {prTemp[[1]], multiplier*prTemp[[2]]};
											imagesize = Abs[(($dwPlotRange[[2, 1]] - $dwPlotRange[[1, 1]])/4*$dwOverallSize)];
											imagesize = {imagesize, (1080/1920)*imagesize},
											plotrange = Partition[Flatten[$dwPlotRange][[{1, 3, 2, 4}]], 2];
											imagesize = Abs[(($dwPlotRange[[2, 1]] - $dwPlotRange[[1, 1]])/4*$dwOverallSize)]
										];
										(* create animation *)
										Export[$UserDocumentsDirectory<>"/"<>filename,
											Table[
												Graphics[{ 
													(* 'f' used in place of 'frame' *)
													dwUpdateFrameData[start, f, frameTotal, pointLoc];
													
													Inset[Magnify[Graphics[{dwRenderLayers[]}, ImageSize->imagesize, PlotRange->plotrange], Abs[frameWidth/(($dwPlotRange[[2,1]]-$dwPlotRange[[1,1]])/4*$dwOverallSize)]]]
													
													},
														Background->White, ImageSize->If[frameWidth == 1920, {1920,1080}, frameWidth], 
														PlotRange->plotrange
												], 
											{f, If[removeLastFrame, frameTotal-1, frameTotal]}],
											AnimationRepetitions->repeat, "DisplayDurations" -> Table[1/framePerSecond, If[removeLastFrame, frameTotal-1, frameTotal]]
										];
										(* return original state *)
										$dwP = #[[1]]&/@start;
										$dwStyle = #[[2]]&/@start;
										(* save point changes *)
										Do[$dwAnimate[[n,1]] = pointLoc[[n]], {n, Length[$dwAnimate]}];
										Do[$dwAnimate[[n,8]] = frameTotal, {n, Length[$dwAnimate]}];
										Do[$dwAnimate[[n,9]] = framePerSecond, {n, Length[$dwAnimate]}];
										Do[$dwAnimate[[n,10]] = frameWidth, {n, Length[$dwAnimate]}];
										Do[$dwAnimate[[n,11]] = removeLastFrame, {n, Length[$dwAnimate]}];
										$dwMessageBarText = "The file '"<>StringDrop[filename, 1]<>"' is ready to view in your documents directory. Open in a web browser to view.",
									Method->"Queued", ImageSize->{115,54}],
									DefaultButton["Save settings",
										$dwP = #[[1]]&/@start;
										$dwStyle = #[[2]]&/@start;
										Do[$dwAnimate[[n,1]] = pointLoc[[n]], {n, Length[$dwAnimate]}];
										Do[$dwAnimate[[n,8]] = frameTotal, {n, Length[$dwAnimate]}];
										Do[$dwAnimate[[n,9]] = framePerSecond, {n, Length[$dwAnimate]}];
										Do[$dwAnimate[[n,10]] = frameWidth, {n, Length[$dwAnimate]}];
										Do[$dwAnimate[[n,11]] = removeLastFrame, {n, Length[$dwAnimate]}];
										$dwSelected = {};
										DialogReturn[],
									ImageSize->{115,54}]
								}]
							}]
							
						}}, Alignment->Top]
						
					}, {	
					
						(* ---------- PREVIEW ---------- *)
							
						Pane[EventHandler[

							(* render *)
							Dynamic@Graphics[{
								
								(* use Magnify and only dwRenderLayers[] for proper arrowheads size, line weights, etc. throughout all sizes however images remain same size throughout also *)
								Inset[Magnify[Graphics[dwRenderLayers[], ImageSize->Abs[(($dwPlotRange[[2, 1]] - $dwPlotRange[[1, 1]])/4*$dwOverallSize)], PlotRange->Dynamic@Partition[Flatten[$dwPlotRange][[{1,3,2,4}]], 2]], Abs[frameWidth/(($dwPlotRange[[2,1]]-$dwPlotRange[[1,1]])/4*$dwOverallSize)]]],
								
								(* highlight object *)
								{$dwSelectionColor, Switch[$dwHead[[obj]],
									Image|"Expression",
										Line[$dwBoundingBoxes[[obj]]],
									BezierCurve,
										BezierCurve[Most@$dwP[[obj]]],
									BSplineCurve,
										BSplineCurve[$dwP[[obj]], SplineClosed->$dwStyle[[obj,10]], SplineDegree->$dwStyle[[obj,11]]],
									Polygon,
										Line[Join[$dwP[[obj]], $dwP[[obj,{1}]]]],
									Text,
										Line[Join[$dwBoundingBoxes[[obj]], {$dwBoundingBoxes[[obj, 1]]}]],
									_,
										Line[$dwP[[obj]]]
									]},
								
								(* object ID numbers for reference *)
								If[objectID, Table[Text[n, $dwP[[n,1]], Background->GrayLevel[1,.5]],{n, Length[$dwP]}], {}],
									
								(* render path with control handles *)
								If[!$dwAnimatePoints && Length[$dwAnimate[[obj,-1]]] == 4,
									{
										{Black, CapForm["Butt"], AbsoluteDashing[{3,3}], BezierCurve[$dwAnimate[[obj,-1]]]},
										{Black, Sequence@@Table[Line[$dwAnimate[[obj,-1]][[{n, n+1}]]], {n, {1,3}}]},
										{AbsolutePointSize[6], Sequence@@Table[Annotation[{If[n === $dwAnimateHandle, Red, Black], Point[$dwAnimate[[obj,-1,n]]]}, ToString[n], "Mouse"], {n, 2, 4, 1}]}
									},
									{}
								],
								
								(* render selected object points *)
								If[$dwAnimatePoints && MemberQ[$dwShapeSymbols, $dwHead[[obj]]],
									{
										{GrayLevel[.5,.5], Line[Join[pointLoc[[obj]], pointLoc[[obj, {1}]]]]}, 
										{AbsolutePointSize[6], Sequence@@Table[Annotation[{If[n === objPointNum, Red, Black], Point[pointLoc[[obj, n]]]}, ToString[n+4], "Mouse"], {n, Length[pointLoc[[obj]]]}]}
									},
									{}
								]
							},
							Background->White, Frame->True, FrameTicks->False, FrameStyle->$dwPlotRangeColor, ImageSize->frameWidth,
							PlotRange->Partition[Flatten[$dwPlotRange][[{1,3,2,4}]], 2]]/.{Indeterminate->0},
							
							{
								"MouseDown":>(
									$dwAnimateClickPt = MousePosition["Graphics"];
									$dwStartAnimate = $dwAnimate;
									startPointLoc = pointLoc;
									mouseAnnotation = MouseAnnotation[];
									If[mouseAnnotation =!= Null,
										If[StringQ[mouseAnnotation],
											If[ToExpression[mouseAnnotation] > 4,
												
												(* select object point *)
												objPointNum = ToExpression[mouseAnnotation] - 4;
												$dwAnimateHandle = Null,
												
												(* select motion point *)
												objPointNum = Null;
												$dwAnimateHandle = ToExpression[mouseAnnotation]
											],
											
											(* select object *)
											$dwAnimateHandle = Null;
											obj = mouseAnnotation;
											{rotate, scale, peak, ramp, arrowSpeed} = $dwAnimate[[obj, {3,4,5,6,7}]];
											Block[{a, b, c, d, round = .001},
												{a,b,c,d} = $dwBoundingBoxes[[obj]];
												originSide = 
													Which[
														Round[$dwAnimate[[obj, -1, 1]], round] == Round[a, round],
															"LeftBottom",
														Round[$dwAnimate[[obj, -1, 1]], round] == Round[{b[[1]] + (a[[1]] - b[[1]])/2, b[[2]]}, round],
															"Bottom",
														Round[$dwAnimate[[obj, -1, 1]], round] == Round[b, round],
															"RightBottom",
														Round[$dwAnimate[[obj, -1, 1]], round] == Round[{b[[1]], b[[2]] + (c[[2]] - b[[2]])/2}, round],
															"Right",
														Round[$dwAnimate[[obj, -1, 1]], round] == Round[c, round],
															"RightTop",
														Round[$dwAnimate[[obj, -1, 1]], round] == Round[{c[[1]] + (d[[1]] - c[[1]])/2, c[[2]]}, round],
															"Top",
														Round[$dwAnimate[[obj, -1, 1]], round] == Round[d, round],
															"LeftTop",
														Round[$dwAnimate[[obj, -1, 1]], round] == Round[{a[[1]], a[[2]] + (d[[2]] - a[[2]])/2}, round],
															"Left",
														True,
															"Center"
													]
											];
										]
									]
									),
								"MouseDragged":>(
									If[Head[MousePosition["Graphics"]] === List,
										If[objPointNum =!= Null,
											If[$dwHead[[obj]] === BezierCurve,
												If[Mod[objPointNum + 2, 3] == 0,
													
													(* main point *)
													pointLoc[[obj, objPointNum]] = (MousePosition["Graphics"] - $dwAnimateClickPt) + startPointLoc[[obj,objPointNum]];
													(* handle before *)
													If[objPointNum > 1, 
														pointLoc[[obj, objPointNum-1]] = (MousePosition["Graphics"] - $dwAnimateClickPt) + startPointLoc[[obj,objPointNum-1]]
													];
													(* handle after *)
													If[objPointNum < Length[$dwStartAnimate[[obj,1]]], 
														pointLoc[[obj, objPointNum+1]] = (MousePosition["Graphics"] - $dwAnimateClickPt) + startPointLoc[[obj,objPointNum+1]]
													];
													(* closed curve first point is same as second to last point and second point is same as last point *)
													If[Length[$dwP[[obj]]] > 5 && $dwStartAnimate[[obj,1,objPointNum]] == $dwStartAnimate[[obj,1,1]],
														pointLoc[[obj, 1]] = pointLoc[[obj, objPointNum]];
														pointLoc[[obj, 2]] = pointLoc[[obj, -1]];
													],
													
													(* handle - move separately since not sure if smooth or corner point *)
													pointLoc[[obj, objPointNum]] = (MousePosition["Graphics"] - $dwAnimateClickPt) + startPointLoc[[obj,objPointNum]]
												],
												
												pointLoc[[obj, objPointNum]] = (MousePosition["Graphics"] - $dwAnimateClickPt) + startPointLoc[[obj,objPointNum]]
											];
											dwUpdateFrameData[start, frame, frameTotal, pointLoc],
											
											If[$dwAnimateHandle =!= Null, 
												$dwAnimate[[obj,-1,$dwAnimateHandle]] = (MousePosition["Graphics"] - $dwAnimateClickPt) + $dwStartAnimate[[obj,-1,$dwAnimateHandle]];
												dwUpdateFrameData[start, frame, frameTotal, pointLoc]
											]
										]
									]
									)
							}
						], Alignment->Center, ImageSize->Dynamic@If[frameWidth==1920, {1920,1080}, {720, frameWidth*(($dwPlotRange[[2, 2]] - $dwPlotRange[[1, 2]])/($dwPlotRange[[2, 1]] - $dwPlotRange[[1, 1]]))}]]
						
					}(*,{
						Column[{
							Row[{"Object ID: ", Dynamic@obj}],
							Row[{"Select",
								EventHandler[
									Slider[Dynamic@obj, {1, Length[$dwP], 1}, ContinuousAction->False],
									{
										"MouseUp":>(
											{rotate, scale, peak, ramp, arrowSpeed} = $dwAnimate[[obj, {3,4,5,6,7}]]
										)
								}, PassEventsDown->True],
								Button["<", obj = If[obj > 1, --obj, obj]; {rotate, scale, peak, ramp, arrowSpeed} = $dwAnimate[[obj, {3,4,5,6,7}]], Appearance->"Palette"],
								Button[">", obj = If[obj < Length[$dwP], ++obj, obj]; {rotate, scale, peak, ramp, arrowSpeed} = $dwAnimate[[obj, {3,4,5,6,7}]], Appearance->"Palette"],
								Dynamic@obj
							}],
							"Click object or drag slider to select",
							""
						}, Alignment->Center]
					}*)
					
				}, Alignment->{Center, Top, {{2,1}->{Center, Top}}}, Spacings->{1, Automatic}]
			]
		],
	Background->LightGray, WindowTitle->"Animate", Modal->True]
	
dwUpdateFrameData[start_, frame_, frameTotal_, pointLoc_]:=
	DynamicModule[{cpList, cpn, rampResult, rotateFunction, scaleFunction, moveFunction, temp, faceform1, faceform2, strokeform1, strokeform2},
		Do[
			(* include compound paths *)
			cpList = If[FreeQ[$dwCompoundPathLayers, n], {n}, $dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, n]][[1]]]]];
			cpn = cpList[[1]];
			
			(* transform *)
			rampResult = If[frame == 1 && $dwAnimate[[cpn,5,1]] == frameTotal,
					1,
					Piecewise[{
						{
							0,
							frame < $dwAnimate[[cpn,5,1]]
						},{
							((((frame-1)/(frameTotal-1))-(($dwAnimate[[cpn,5,1]]-1)/(frameTotal-1)))/(((($dwAnimate[[cpn,5,2]]-1)/(frameTotal-1))+$MachineEpsilon)-(($dwAnimate[[cpn,5,1]]-1)/(frameTotal-1))))^If[$dwAnimate[[cpn,6]]>1,(Pi/2)*$dwAnimate[[cpn,6]],$dwAnimate[[cpn,6]]],
							frame < $dwAnimate[[cpn,5,2]]
						},{
							1,
							$dwAnimate[[cpn,5,2]] <= frame <= $dwAnimate[[cpn,5,3]]
						},{
							(((($dwAnimate[[cpn,5,4]]-1)/(frameTotal-1))-((frame-1)/(frameTotal-1)))/((($dwAnimate[[cpn,5,4]]-1)/(frameTotal-1))-(($dwAnimate[[cpn,5,3]]-1)/(frameTotal-1))+$MachineEpsilon))^If[$dwAnimate[[cpn,6]]>1,(Pi/2)*$dwAnimate[[cpn,6]],$dwAnimate[[cpn,6]]],
							$dwAnimate[[cpn,5,3]] < frame < $dwAnimate[[cpn,5,4]]
						},{
							If[$dwAnimate[[cpn,5,2]] == $dwAnimate[[cpn,5,4]], 1, 0],
							frame >= $dwAnimate[[cpn,5,4]]
						}}]
				];
			rotateFunction = RotationTransform[(rampResult)*$dwAnimate[[cpn,3]]*Pi/180, $dwAnimate[[cpn,-2]]];
			scaleFunction = ScalingTransform[1+(rampResult*($dwAnimate[[cpn,4]]-1)), $dwAnimate[[cpn,-2]]];
			moveFunction = If[Length[$dwAnimate[[cpn,-1]]] == 4, BezierFunction[-dwFindCenter[Join[Sequence@@Table[start[[each,1]], {each, cpList}]]]+#&/@$dwAnimate[[cpn,-1]]], Null];
			$dwP[[n]] = If[moveFunction === Null,
						rotateFunction[scaleFunction[Table[start[[n,1,n2]]+rampResult(pointLoc[[n,n2]]-start[[n,1,n2]]), {n2, Length[$dwP[[n]]]}]]],
						moveFunction[rampResult]+#&/@rotateFunction[scaleFunction[Table[start[[n,1,n2]]+rampResult(pointLoc[[n,n2]]-start[[n,1,n2]]), {n2, Length[$dwP[[n]]]}]]]
				];
				
			(* style (size & rotation for text and image) *)
			Switch[$dwHead[[n]],
				Text,
					temp = Flatten[Position[start[[n,2]], FontColor]][[1]];
					$dwStyle[[n,temp,2]] = Blend[ColorConvert[{start[[n,2,temp,2]], $dwAnimate[[n,2,temp,2]]}, "LAB"],rampResult];
					temp = Flatten[Position[start[[n,2]], FontSize]][[1]];
					$dwStyle[[n,temp,2]] = (start[[n,2,temp,2]] + ($dwAnimate[[n,4,1]]*($dwAnimate[[n,2,temp,2]] - start[[n,2,temp,2]]))rampResult);
					temp = Flatten[Position[start[[n,2]], FontOpacity]][[1]];
					$dwStyle[[n,temp,2]] = (start[[n,2,temp,2]] + ($dwAnimate[[n,4,1]]*($dwAnimate[[n,2,temp,2]] - start[[n,2,temp,2]]))rampResult);
					temp = Flatten[Position[start[[n,2]], LineSpacing]][[1]];
					$dwStyle[[n,temp,2,2]] = (start[[n,2,temp,2,2]] - (start[[n,2,temp,2,2]] - $dwAnimate[[n,2,temp,2,2]])rampResult);
					(* rotation *)
					$dwStyle[[n,1]] = (start[[n,2,1]] + ($dwAnimate[[n,3]] Degree)*rampResult),
				Point,(* color, opacity, size *)
					$dwStyle[[n,$dwStyleStart]] = Blend[ColorConvert[{start[[n,2,13]], $dwAnimate[[n,2,13]]}, "LAB"], rampResult];
					$dwStyle[[n,$dwStyleStart+1,1]] = (start[[n,2,$dwStyleStart+1,1]] - (start[[n,2,$dwStyleStart+1,1]] - $dwAnimate[[n,2,$dwStyleStart+1,1]])rampResult);
					temp = Flatten[Position[start[[n,2]], AbsolutePointSize[_]]][[1]];
					$dwStyle[[n,temp,1]] = (start[[n,2,temp,1]] - $dwAnimate[[n,7]]*(start[[n,2,temp,1]] - $dwAnimate[[n,2,temp,1]])rampResult),
				Image|"Expression",
					(* size *)
					$dwStyle[[n,3]] = (start[[n,2,3]] - $dwAnimate[[n,4]]*(start[[n,2,3]] - $dwAnimate[[n,4]])rampResult)[[1]];
					(* rotation *)
					$dwStyle[[n,1]] = (start[[n,2,1]] + ($dwAnimate[[n,3]] Degree)*rampResult),
				_,
					temp = Flatten[Position[start[[n,2]], FaceForm]][[1]];
					faceform1 = Cases[start[[n,2]], _FaceForm, Infinity];
					faceform2 = Cases[$dwAnimate[[n,2]], _FaceForm, Infinity];
					$dwStyle[[n,temp]] = FaceForm[{Blend[ColorConvert[{faceform1[[1, 1, 1]], faceform2[[1, 1, 1]]}, "LAB"], rampResult], 
						Opacity[(faceform1[[1, 1, 2, 1]] - (faceform1[[1, 1, 2, 1]] - faceform2[[1, 1, 2, 1]])rampResult)]
					}];
					temp = Flatten[Position[start[[n,2]], StrokeForm]][[1]];
					strokeform1 = Cases[start[[n,2]], _StrokeForm, Infinity];
					strokeform2 = Cases[$dwAnimate[[n,2]], _StrokeForm, Infinity];
					$dwStyle[[n,temp]] = StrokeForm[{Blend[ColorConvert[{strokeform1[[1, 1, 1]], strokeform2[[1, 1, 1]]}, "LAB"], rampResult], 
						Opacity[(strokeform1[[1, 1, 2, 1]] - (strokeform1[[1, 1, 2, 1]] - strokeform2[[1, 1, 2, 1]])rampResult)],
						AbsoluteThickness[strokeform1[[1, 1, 3, 1]] - (strokeform1[[1, 1, 3, 1]] - strokeform2[[1, 1, 3, 1]])rampResult],
						Sequence@@strokeform1[[1, 1, 4;;6]]
					}];
					temp = Flatten[Position[start[[n,2]], Arrowheads[_]]][[1]];
					If[Length[$dwStyle[[n,temp,1]]] > 2,
						Do[$dwStyle[[n,temp,1,i,2]] = Chop[FractionalPart[start[[n,2,temp,1,i,2]] + $dwAnimate[[n,7]]*(frame/frameTotal)]]/.{0->1}, {i, Length[$dwStyle[[n,temp,1]]]}],
						Nothing
					]
			],
		{n, Length[$dwP]}]
	]
	
dwFindOrigin[obj_, originSide_, start_]:=
	dwFindSide[
		Join[Sequence@@Table[start[[each,1]], {each, If[FreeQ[$dwCompoundPathLayers, obj], {obj}, $dwCompoundPathLayers[[Flatten[Position[$dwCompoundPathLayers, obj]][[1]]]]]}]], 
		Switch[originSide,
			"LeftTop", {Left,Top},
			"Top", {Center,Top},
			"RightTop", {Right,Top},
			"Left", {Left,Center},
			"Right", {Right,Center},
			"LeftBottom", {Left,Bottom},
			"Bottom", {Center,Bottom},
			"RightBottom", {Right,Bottom},
			_,
				{Center,Center}
		]
	]
	
dwAnimateStyleSettings[sel_]:=
	Switch[$dwHead[[sel]],
		Text,
			Grid[{
			{
				ColorSetter[Dynamic[$dwAnimate[[sel, 2, Flatten[Position[$dwAnimate[[sel,2]], FontColor]][[1]]]][[2]]], ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight}],
				InputField[Dynamic[$dwAnimate[[sel, 2, Flatten[Position[$dwAnimate[[sel,2]], FontOpacity]][[1]]]][[2]]], Number, ImageSize->{50,$dwStyleButtonHeight-2}],
				InputField[Dynamic[$dwAnimate[[sel, 2, Flatten[Position[$dwAnimate[[sel,2]], FontSize]][[1]]]][[2]]], Number, ImageSize->{50,$dwStyleButtonHeight-2}],
				InputField[Dynamic[$dwAnimate[[sel, 2, Flatten[Position[$dwAnimate[[sel,2]], LineSpacing]][[1]]]][[2]][[2]]], Number, ImageSize->{50,$dwStyleButtonHeight-2}]
			},
				{"COLOR","OPACITY","SIZE","LEADING"}
			}, Alignment->Top, Spacings->{0, .5}],
		Image|"Expression",
			Spacer[{0,$dwStyleButtonHeight+18}],
		Point,
			Grid[{
			{
				ColorSetter[Dynamic[$dwAnimate[[sel, 2, $dwStyleStart]]], ImageSize->{48, 25}],
				InputField[Dynamic[$dwAnimate[[sel, 2, $dwStyleStart+1, 1]]], Number, ImageSize->{50,$dwStyleButtonHeight-1}],
				InputField[Dynamic[$dwAnimate[[sel, 2, Flatten[Position[$dwAnimate[[sel,2]], AbsolutePointSize[_]]][[1]], 1]]], Number, ImageSize->{50,$dwStyleButtonHeight-1}]
			},
				{"COLOR","OPACITY","SIZE"}
			}, Alignment->Top, Spacings->{0, .5}],
		_,
			Grid[{
			{
				EventHandler[ColorSetter[Dynamic[$dwAnimate[[sel, 2, Flatten[Position[$dwAnimate[[sel,2]], FaceForm[_]]][[1]], 1,1]]], ImageSize->{48, 25}], {"MouseDown":>(dwUpdateSelected[sel, "StylesToUpdate"->"animateFillColor"])}, PassEventsDown->True],
				InputField[Dynamic[$dwAnimate[[sel, 2, Flatten[Position[$dwAnimate[[sel,2]], FaceForm[_]]][[1]], 1,2,1]]], Number, ImageSize->{50,$dwStyleButtonHeight-1}],
				EventHandler[ColorSetter[Dynamic[$dwAnimate[[sel, 2, Flatten[Position[$dwAnimate[[sel,2]], StrokeForm[_]]][[1]], 1,1]]], ImageSize->{2$dwStyleButtonHeight,$dwStyleButtonHeight+1}], {"MouseDown":>(dwUpdateSelected[sel, "StylesToUpdate"->"animateStrokeColor"])}, PassEventsDown->True],
				InputField[Dynamic[$dwAnimate[[sel, 2,Flatten[Position[$dwAnimate[[sel,2]], StrokeForm[_]]][[1]], 1,2,1]]], Number, ImageSize->{50,$dwStyleButtonHeight-1}],
				InputField[Dynamic[$dwAnimate[[sel, 2,Flatten[Position[$dwAnimate[[sel,2]], StrokeForm[_]]][[1]], 1,3,1]]], Number, ImageSize->{50,$dwStyleButtonHeight-1}]
			},
				{"FILL","OPACITY","STROKE","OPACITY","THICK"}
			}, Alignment->Top, Spacings->{0, .5}]
	]

End[] (* End Private Context *)

EndPackage[]