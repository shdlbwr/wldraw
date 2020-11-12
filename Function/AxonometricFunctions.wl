(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwObjectAxonometric[selList_]:= $dwP = dwAxo[$dwP, selList, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}]

(* "ReturnValuesWithoutUpdating"->True returns {angleH, angleV}; False updates $dwConstrainHAngle and $dwConstrainVAngle *)
Options[dwUpdateAxonometricAngles]:={"ReturnValuesWithoutUpdating"->False};
dwUpdateAxonometricAngles[tilt_:$dwTilt, turn_:$dwTurn, OptionsPattern[]]:= 
	If[OptionValue["ReturnValuesWithoutUpdating"],
		{dwAxoAngles[{}, {}, "Tilt"->tilt, "Turn"->turn][[1]], dwAxoAngles[{}, {}, "Tilt"->tilt, "Turn"->turn][[2]]-90},
		$dwConstrainHAngle = dwAxoAngles[{}, {}, "Tilt"->tilt, "Turn"->turn][[1]];
		$dwConstrainVAngle = dwAxoAngles[{}, {}, "Tilt"->tilt, "Turn"->turn][[2]]-90;
	]
	
dwAxoDialog[]:=
	CreateDialog[
		DynamicModule[{temp, angleH = $dwConstrainHAngle, angleV = $dwConstrainVAngle, tilt = $dwTilt, turn = $dwTurn, extObj, extOrder, extLength, face, showBackface=False, showBackfaceInFront=False, extPts1, extPts2, exportedPoints, startLength, firstSelection},
			{angleH, angleV} = dwUpdateAxonometricAngles[tilt, turn, "ReturnValuesWithoutUpdating"->True];
			Pane[
				Grid[{
					{Column[{
						Dynamic@Graphics[{
						(* axes, frame, constrain *)
						{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], If[angleH < 0, Nothing, InfiniteLine[{{-1, 0}, {1, 0}}]], 
							If[angleH < 0,
								Line[dwAxo[{{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}}, 1, "Tilt"->tilt, "Turn"->turn, "Direction"->"Top"][[1]]], 
								Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]
							],
							InfiniteLine[{{0, 0}, {Cos[angleH/180*Pi], Sin[angleH/180*Pi]}}], InfiniteLine[{{0, 0}, {Cos[(angleV+90)/180*Pi], Sin[(angleV+90)/180*Pi]}}]
						},
						{Black, 
							Text["x", {Cos[angleH/180*Pi], Sin[angleH/180*Pi]}],
							Text["y", {Cos[(angleV+90)/180*Pi], Sin[(angleV+90)/180*Pi]}],
							If[angleH =!= angleV, Text["z", {0, 1}], Nothing]
						},
						If[$dwSelected === {},
							{},
							
							(* objects *)
							Sequence@@Flatten[{
								Table[
									Switch[$dwHead[[n]],
										"Expression",
											{},
										Text|"Text3D",
											dwAxoText[$dwStyle[[n,2]], $dwStyle[[n]][[Join[Range[4,11], {15}]]]/.{(FontSize->fs_):>(FontSize->.6fs),(LineSpacing->ls_):>(LineSpacing->.6ls)}, 
												"Center"->dwAxo[$dwP[[n]], {1}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}][[1]], 
												"Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}
											],
										Image,
											Inset[
												dwAxoImage[{Rasterize[Rotate[$dwStyle[[n, 2]], $dwStyle[[n, 1]], ImageResolution->$dwImageResolution], Background->None]}, {1}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}][[1]],
												dwAxo[$dwP[[n]], {1}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}][[1]],
												Automatic, 
												(.001075ImageDimensions[dwAxoImage[{Rasterize[Graphics[Rectangle[]], ImageResolution->$dwImageResolution]}, {1}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}][[1]]][[1]])*$dwStyle[[n, 3]]
											],
										_,
											If[Length[$dwP[[n]]] > 1,
												If[$dwSelected[[1]] == n && $dwAxoExtrude,
													(* first object extrusion *)
													extObj = dwAxoExtrude[{If[Length[$dwP[[n]]] == 2, {$dwP[[n,1]], Mean[$dwP[[n]]], $dwP[[n,2]]}, $dwP[[n]]]}, "Extrude"->If[$dwAxoExtrudeAmount == 0, .01, -$dwAxoExtrudeAmount]];
													face = If[showBackface || showBackfaceInFront, extObj[[-2]], extObj[[-1]]];
													extObj = extObj[[;;-3]];
													extLength = Length[extObj];
													extOrder = $dwAxoExtrudeOrder + 2 extLength;
													extObj = Join[extObj, extObj, extObj, extObj];
													extPts1 = extObj[[extOrder-IntegerPart[extLength/2]-1;;extOrder-1]];
													extPts2 = Reverse[extObj[[extOrder+1;;extOrder+(extLength-IntegerPart[extLength/2])-2]]];
													{
														(*Sequence@@$dwStyle[[n]][[$dwStyleStart;;-1]]*)
														StrokeForm[{Black}], FaceForm[{GrayLevel[.8]}],
														If[showBackface && !showBackfaceInFront, Polygon[face], Nothing],
														Polygon[extPts1],
														Polygon[extPts2],
														{Red,Polygon[extObj[[extOrder]]]},
														StrokeForm[{Black}], FaceForm[{GrayLevel[.8]}],
														If[!showBackface || showBackfaceInFront, Polygon[face], Nothing]
													},
													(* no extrusion *)
													temp = dwAxo[{$dwP[[n]]}, {1}, "Tilt"->tilt, "Turn"->turn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}][[1]];
													{
														StrokeForm[{Black}],
														Switch[$dwHead[[n]],
															Arrow,
																Line[temp],
															Polygon,
																Line[Join[temp, {temp[[1]]}]],
															BezierCurve,
																BezierCurve[If[Length[$dwP[[n]]] > 2, Most[temp], temp]], 
															BSplineCurve, 
																If[Length[$dwP[[n]]] > 1,
																	BSplineCurve[temp, SplineClosed->$dwStyle[[n,10]], SplineDegree->$dwStyle[[n,11]]],
																	Nothing
																], 
															_,
																$dwHead[[n]][temp]
														]
													}
												],
												Nothing
											]
									],
								{n, $dwSelected}]
							}]]
						}, Background->White, ImageSize->If[$dwSelected === {}, {400, 174}, {400, 370}], PlotRange->If[$dwSelected === {}, {{-4.1, 4.1},{-1.6, 1.6}}, {{-1.6, 1.6},{-1.6, 1.6}}]],
						If[$dwSelected === {}, Nothing, Style["Preview uses wireframe style for objects.",Gray]]
					}],
						If[$dwSelected === {},
							Grid[{
								{Panel[Column[{
									Style["3D VIEW", 15],
									Row[{
										
										Button["favor right side", tilt = 30(*28.3077*); turn = 30(*29.4705*); {angleH, angleV} = dwUpdateAxonometricAngles[tilt, turn, "ReturnValuesWithoutUpdating"->True]],
										Button["isometric", tilt = 35.2672; turn = 45; {angleH, angleV} = dwUpdateAxonometricAngles[tilt, turn, "ReturnValuesWithoutUpdating"->True]],
										Button["favor left side", tilt = 30(*28.3079*); turn = 60(*60.529*); {angleH, angleV} = dwUpdateAxonometricAngles[tilt, turn, "ReturnValuesWithoutUpdating"->True]]
									}],
									Grid[{{
										Pane["tilt", Alignment->Right, ImageSize->60], 
										EventHandler[
											Slider[Dynamic@tilt, {15, 75, 5}, ImageSize->200], 
											{"MouseUp":>({angleH, angleV} = dwUpdateAxonometricAngles[tilt, turn, "ReturnValuesWithoutUpdating"->True])
										}, PassEventsDown->True],
										Pane[Dynamic@tilt,ImageSize->60]}
									}],
									Grid[{{
										Pane["turn", Alignment->Right, ImageSize->60], 
										EventHandler[
											Slider[Dynamic@turn, {15, 75, 5}, ImageSize->200], 
											{"MouseUp":>({angleH, angleV} = dwUpdateAxonometricAngles[tilt, turn, "ReturnValuesWithoutUpdating"->True])
										}, PassEventsDown->True],
										Pane[Dynamic@turn,ImageSize->60]}}],
									"",
									Style["Set the 3D view before beginning an illustration.", Italic],
									Style["A view change will not reorient existing objects.", Italic],
									"",
									Style["Shapes drawn or moved on the 3D floor will snap to the grid.", Italic]
								}, Alignment->Center], Alignment->Center, ImageSize->320]}
							}],
							Grid[{
								{Panel[Grid[{
									{Style["AXIS PROJECTION", 15]},
									{Style[SetterBar[Dynamic@$dwAxoProjection, {"Right"->"x","Left"->"y","Top"->"z"}, Alignment -> Center, ImageSize -> {40, 40}], 18]}
								}, Alignment->Center], Alignment->Center, ImageSize->320]},
								{Panel[Grid[{
									{Style["3D ROTATION", 15], SpanFromLeft, SpanFromLeft},
									{Row[{"order ", SetterBar[Dynamic@$dwAxoRotateOrder, {"zxy"->"xy", "zyx"->"yx"}]}], SpanFromLeft, SpanFromLeft},
									{Pane["x", Alignment->Right, ImageSize->30], Slider[Dynamic@$dwAxoAxisRotation[[1]] ,{-90, 90, 1}], Pane[Dynamic@$dwAxoAxisRotation[[1]],ImageSize->30]},
									{Pane["y", Alignment->Right, ImageSize->30], Slider[Dynamic@$dwAxoAxisRotation[[2]] ,{-90, 90, 1}], Pane[Dynamic@$dwAxoAxisRotation[[2]],ImageSize->30]},
									{Pane["z", Alignment->Right, ImageSize->30], Slider[Dynamic@$dwAxoAxisRotation[[3]] ,{-90, 90, 1}], Pane[Dynamic@$dwAxoAxisRotation[[3]],ImageSize->30]}, 
									{Button["reset all rotations", $dwAxoAxisRotation = {0,0,0}], SpanFromLeft, SpanFromLeft}
								}, Alignment->Center], Alignment->Center, ImageSize->320]},
								
								{Panel[Grid[{
									{Style["EXTRUDE", 15], SpanFromLeft, SpanFromLeft}, 
									{Row[{Checkbox[Dynamic@$dwAxoExtrude], " extrude first object (splines not discretized)"}], SpanFromLeft, SpanFromLeft},
									{Row[{Checkbox[Dynamic@showBackface], " backface     ", Checkbox[Dynamic@showBackfaceInFront], " backface in front"}], SpanFromLeft, SpanFromLeft},
									{Pane["depth", Alignment->Right, ImageSize->40], Slider[Dynamic@$dwAxoExtrudeAmount, {-1, 1, .01}], Pane[Dynamic@$dwAxoExtrudeAmount,ImageSize->40]},
									{Pane["order", Alignment->Right, ImageSize->40], Slider[Dynamic@$dwAxoExtrudeOrder, {1, Length[$dwP[[$dwSelected[[1]]]]], 1}], Pane[Dynamic@$dwAxoExtrudeOrder,ImageSize->40]},
									{Style["adjust order until foremost extruded polygon is red", Italic], SpanFromLeft, SpanFromLeft}
								}, Alignment->Center], Alignment->Center, ImageSize->320]}
							}]
						]},
							
						{Row[{
							CancelButton[dwSetGridSize[];DialogReturn[]], 
							DefaultButton[
								dwSetUndo[];
								$dwTilt = tilt; $dwTurn = turn;
								$dwConstrainHAngle = angleH; $dwConstrainVAngle = angleV;
								dwSetGridSize[];
								If[$dwAxoExtrude && FreeQ[{Image, Text, "Expression"}, $dwHead[[$dwSelected[[1]]]]],
									
									(* extrusion *)
									firstSelection = $dwSelected[[1]];
									startLength = Length[$dwP];
									dwObjectAxonometric[];(* in case of multiple objects since only first is extruded *)
									exportedPoints = Join[extPts1, extPts2, {extObj[[extOrder]]}];
									If[showBackface && !showBackfaceInFront, PrependTo[exportedPoints, face], AppendTo[exportedPoints, face]];
									Do[
										dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
										$dwP[[-1]] = exportedPoints[[n]];
										$dwStyle[[-1]] = $dwStyle[[firstSelection]];
										dwUpdateBoundingBox[{-1}],
									{n, Length[exportedPoints]}];
									$dwSelected = {firstSelection};
									dwDeleteLayer[];
									$dwGroupLayers = Join[$dwGroupLayers, {Range[startLength, Length[$dwP]]}];
									$dwSelected = $dwGroupLayers[[-1]],
									
									Do[
										Switch[$dwHead[[n]],
											"Expression",
												Nothing,
											Text|"Text3D",
												$dwP[[n]] = dwAxo[$dwP[[n]], {1}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}];
												$dwStyle[[n, 1]] = {$dwTilt, $dwTurn, $dwAxoProjection, $dwAxoRotateOrder, $dwAxoAxisRotation},
											Image,
												$dwStyle[[n, 2]] = dwAxoImage[{Rasterize[Rotate[$dwStyle[[n, 2]], $dwStyle[[n, 1]], ImageResolution->$dwImageResolution], Background->None]}, {1}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}][[1]];
												$dwP[[n]] = dwAxo[$dwP[[n]], {1}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->$dwAxoProjection, "RotateOrder"->$dwAxoRotateOrder, "AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}];
												$dwStyle[[n, 1]] = 0,
											_,
												(* no extrusion *)
												dwObjectAxonometric[{n}]
										],
									{n, $dwSelected}];
									(* head change cannot occur in previous Do statement *)
									Do[If[$dwHead[[n]] === Text, $dwHead[[n]] = "Text3D", Nothing], {n, $dwSelected}];
									dwUpdateBoundingBox[$dwSelected]
								];
								$dwPointQuantity = Length[Flatten[$dwP, 1]];
								dwSetGridSize[];
								(*$dwStyleMode = "fill";*)
								DialogReturn[]
							]}], SpanFromLeft, SpanFromLeft}
					}, Alignment->Center],
			ImageSize->730]], WindowTitle->"3D"
	]

(* extrude - must be polygon; if no extrusion Reverse@ points *)
Options[dwAxoExtrude]:={"Center"->{0,0},"Extrude"->.5,"BuildPolygons"->1,"Taper"->1};
dwAxoExtrude[g_,OptionsPattern[]]:=
	Module[{frontface,backface,extrudedfaces,doubleG,values,valuesZero,temp,center,extrude,buildPolys,taper
		},
		{center,extrude,buildPolys,taper}=OptionValue[{"Center","Extrude","BuildPolygons","Taper"}];
		values = pro7[allTiltTurn[$dwTilt+$MachineEpsilon, $dwTurn+$MachineEpsilon, {{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}}]][[1,-1]];
		valuesZero = pro7[allTiltTurn[$dwTilt+$MachineEpsilon, $dwTurn+$MachineEpsilon, {{0, $dwAxoProjection}}]][[1,1]];
		
		frontface = dwAxo[g,{1},"Tilt"->$dwTilt+$MachineEpsilon,"Turn"->$dwTurn+$MachineEpsilon,"AxisRotation"->{{$dwAxoAxisRotation[[3]],"Top"},{$dwAxoAxisRotation[[1]],"Right"},{$dwAxoAxisRotation[[2]],"Left"}},"Center"->{0,0},"Direction"->$dwAxoProjection,"RotateOrder"->$dwAxoRotateOrder];
		
		(* extruded polygons *)
		Switch[$dwAxoProjection,
			"Left",temp={12,15},
			"Right",temp={4,13},
			"Top",temp={8,14}
		];
		extrudedfaces={doubleG=Flatten[{#,#},1]; (* points doubled for ability to start at any point *)
			Table[
				{doubleG[[buildPolys+n]],doubleG[[buildPolys+n+1]],
				doubleG[[buildPolys+n+1]]-values[[temp[[2]]]] extrude{Cos[values[[temp[[1]]]] \[Degree]],Sin[values[[temp[[1]]]] \[Degree]]},
				doubleG[[buildPolys+n]]-values[[temp[[2]]]] extrude{Cos[values[[temp[[1]]]] \[Degree]],Sin[values[[temp[[1]]]] \[Degree]]}},
			{n, Length[#]}]
		}&/@frontface;
		
		(* remove flat quads *)
		extrudedfaces = If[Length@DeleteDuplicates[Round[#,.001]]<4,Nothing,#]&/@extrudedfaces[[1,1]];
		
		(* back face *)
		backface = {#-values[[temp[[2]]]] extrude{Cos[values[[temp[[1]]]] \[Degree]],Sin[values[[temp[[1]]]] \[Degree]]}&/@frontface[[1]]};
		
		(* add face *)
		Join[extrudedfaces, backface, frontface]
	]

(* Copyright 2017, Tim Shedelbower *)
(* returns base and offaxis tilt turn values, offAxis is a list {{30,"Left"},{45,"Top"}} *)
allTiltTurn[tilt_,turn_,offAxis_]:=
	Module[{tt={}, temp},
		{
			tt=tiltTurn[tilt,turn];
			(Switch[#[[2]],
				"Left",
					temp=pro2[tt[[2]],tt[[3]],tt[[1]],tt[[9]],tt[[6]],tt[[5]]];
					tt[[6]]-=temp[[1]];tt[[4]]+=90;tt[[5]]-=temp[[2]];tt[[9]]+=#[[1]];
					tt[[9]]=pro5[tt[[9]]];
					temp=pro4[tt[[3]],tt[[9]]];
					tt[[1]]=temp[[1]];tt[[2]]=temp[[2]];tt[[8]]=pro3[tt[[3]],tt[[1]],tt[[8]]];tt[[7]]=pro3[tt[[2]],tt[[3]],tt[[7]]];
					temp=pro2[tt[[2]],tt[[3]],tt[[1]],tt[[9]],tt[[6]],tt[[5]]];
					tt[[6]]+=temp[[1]];tt[[4]]-=90;tt[[5]]+=temp[[2]],
				"Right",
					temp=pro2[tt[[3]],tt[[1]],tt[[2]],tt[[7]],tt[[5]],tt[[4]]];
					tt[[6]]+=90;tt[[4]]-=temp[[2]];tt[[5]]-=temp[[1]];
					tt[[7]]+=#[[1]];
					tt[[7]]=pro5[tt[[7]]];
					temp=pro4[tt[[1]],tt[[7]]];
					tt[[2]]=temp[[1]];tt[[3]]=temp[[2]];tt[[8]]=pro3[tt[[3]],tt[[1]],tt[[8]]];tt[[9]]=pro3[tt[[1]],tt[[2]],tt[[9]]];
					temp=pro2[tt[[3]],tt[[1]],tt[[2]],tt[[7]],tt[[5]],tt[[4]]];
					tt[[6]]-=90;tt[[4]]+=temp[[2]];tt[[5]]+=temp[[1]],
				"Top",
					temp=pro2[tt[[1]],tt[[2]],tt[[3]],tt[[8]],tt[[4]],tt[[6]]];
					tt[[6]]-=temp[[2]];tt[[4]]-=temp[[1]];tt[[5]]+=90;tt[[8]]+=#[[1]];
					tt[[8]]=pro5[tt[[8]]];
					temp=pro4[tt[[2]],tt[[8]]];
					tt[[3]]=temp[[1]];tt[[1]]=temp[[2]];tt[[9]]=pro3[tt[[1]],tt[[2]],tt[[9]]];tt[[7]]=pro3[tt[[2]],tt[[3]],tt[[7]]];
					temp=pro2[tt[[1]],tt[[2]],tt[[3]],tt[[8]],tt[[4]],tt[[6]]];
					tt[[6]]+=temp[[2]];tt[[4]]+=temp[[1]];tt[[5]]-=90
			];tt)&/@offAxis
		}
	]

(* returns tilt, angle and rotation values *)
tiltTurn[tilt_,turn_]:=
	Module[{},
		{
			ArcTan[(Cos[tilt Pi/180] Cos[turn Pi/180])/Sqrt[1-Cos[tilt Pi/180]^2 Cos[turn Pi/180]^2]]180/Pi,
			tilt,
			ArcTan[(Cos[tilt Pi/180] Sin[turn Pi/180])/Sqrt[1-Cos[tilt Pi/180]^2 Sin[turn Pi/180]^2]]180/Pi,
			ArcTan[Sin[tilt Pi/180]Tan[turn Pi/180]]180/Pi,
			-90,
			180-ArcTan[Sin[tilt Pi/180]/Tan[turn Pi/180]]180/Pi,
			ArcTan[Tan[tilt Pi/180]/Sin[turn Pi/180]]180/Pi,
			turn,
			ArcTan[Cos[turn Pi/180]/Tan[tilt Pi/180]]180/Pi
		}
	]

(* returns remaining turn values *)
pro2[tilt1_,tilt2_,tilt3_,turn_,constrain1_,constrain2_]:=
	Module[{temp1,temp2},
		Which[tilt1>0,temp1=ArcTan[Sin[tilt2 Pi/180]Tan[turn Pi/180]]180/Pi,
			tilt1<0,temp1=180+ArcTan[Sin[tilt2 Pi/180]Tan[turn Pi/180]]180/Pi,
			tilt1==0,temp2=turn+90;Which[tilt2>0,temp1=turn,tilt2<0,temp1=-turn,True,temp1=constrain1]
		];
		Which[tilt3>0&&tilt1!=0,temp2=180+ArcTan[-Sin[tilt2 Pi/180]/Tan[turn Pi/180]]180/Pi,
			tilt3<0&&tilt1!=0,temp2=ArcTan[-Sin[tilt2 Pi/180]/Tan[turn Pi/180]]180/Pi,
			tilt3==0,Which[tilt2>0,temp2=90-turn,tilt2<0,temp2=turn-90,True,temp2=constrain2]
		];
		{temp1,temp2}
	]

(* returns turn value *)
pro3[tilt1_,tilt2_,turn_]:=
	Module[{finalTurn},
		finalTurn=turn;
		Which[tilt2>0,finalTurn=ArcTan[Sin[tilt1 Pi/180]/Sin[tilt2 Pi/180]]180/Pi,
			tilt2<0,finalTurn=180+ArcTan[Sin[tilt1 Pi/180]/Sin[tilt2 Pi/180]]180/Pi,
			tilt2==0,Which[tilt1>0,finalTurn=90,tilt1<0,finalTurn=-90]
		];
		finalTurn
	]

(* returns remaining tilt values *)
pro4[tilt_,turn_]:=
	Module[{tilt1,tilt2,ctst,ctct},
		ctst=Cos[tilt Pi/180]Sin[turn Pi/180];
		ctct=Cos[tilt Pi/180]Cos[turn Pi/180];
		Which[
			ctst==1,tilt1=90,
			ctst==-1,tilt1=-90,
			True,tilt1=ArcTan[ctst/Sqrt[1-ctst^2]]180/Pi
		];
		Which[
			ctct==1,tilt2=90,
			ctct==-1,tilt2=-90,
			True,tilt2=ArcTan[ctct/Sqrt[1-ctct^2]]180/Pi
		];
		{tilt1,tilt2}
	]

(* returns adjusted turn value *)
pro5[turn_]:=
	Module[{ct,st,at,finalTurn},
		finalTurn=turn;
		ct=Cos[turn Pi/180];
		st=Sin[turn Pi/180];
		at=ArcTan[Tan[turn Pi/180]]180/Pi;
		Which[
			ct>0,finalTurn=at,
			ct<0,finalTurn=If[st>=0,180+at,at-180],
			ct==0,finalTurn=Which[st>0,90,st<0,-90]
		];
		finalTurn
	]

(* returns inclination angle value *)
pro6[angle_]:=
	Which[
		Cos[angle Pi/180]==0,0,
		Cos[angle Pi/180]<0,ArcTan[Tan[angle Pi/180]]180/Pi,
		True,180+ArcTan[Tan[angle Pi/180]]180/Pi
	]

(* returns:
{right rotation 1, right horizontal scale, right rotation 2, right inclination,
top rotation 1, top horizontal scale, top rotation 2, top inclination,
left rotation 1, left horizontal scale, left rotation 2, left inclination,
right inclination scale, top inclination scale, left inclination scale} *)
pro7[tt_]:=
	{
		N@{pro5[#[[7]]+180],Sin[#[[1]]Pi/180],pro5[#[[6]]],pro6[#[[6]]],(* right *)
		pro5[#[[8]]+90],Sin[#[[2]]Pi/180],pro5[#[[5]]],pro6[#[[5]]],(* top *)
		pro5[#[[9]]-90],Sin[#[[3]]Pi/180],pro5[#[[4]]],pro6[#[[4]]],(* left *)
		Cos[#[[1]]Pi/180],(* right scale *)
		Cos[#[[2]]Pi/180],(* top scale *)
		Cos[#[[3]]Pi/180](* left scale *)
		}&/@tt[[1]]
	}

(* final angle output *)
Options[dwAxoAngles]:={"Tilt"->30,"Turn"->30,"AxisRotation"->{{0,"Top"},{0,"Right"},{0,"Left"}},"Center"->{0,0},"Direction"->"Right","RotateOrder"->"zyx"};
dwAxoAngles[pts_:{}, selected_:{}, OptionsPattern[]]:=
	Block[{g,ctr=1,scaleF,rotateF,rotate2F,values,loc,tilt,turn,offAxis,center,direction,rotateOrder,imageDimensions},
		{tilt,turn,offAxis,center,direction,rotateOrder} = OptionValue[{"Tilt","Turn","AxisRotation","Center","Direction","RotateOrder"}];
		offAxis=Join[{{0,direction}},If[rotateOrder==="zyx",offAxis,{offAxis[[1]],offAxis[[3]],offAxis[[2]]}]]/.{0->.001,90->89.999,180->179.99,-90->-89.999,-180->-179.99};
		values = pro7[allTiltTurn[tilt+$MachineEpsilon, turn+$MachineEpsilon, offAxis]][[1,-1]];
		{values[[4]], values[[12]], values[[8]]}
	]

(* final text output *)
Options[dwAxoText]:={"Tilt"->30,"Turn"->30,"AxisRotation"->{{0,"Top"},{0,"Right"},{0,"Left"}},"Center"->{0,0},"Direction"->"Right","RotateOrder"->"zyx"};
dwAxoText[string_:"string", style_:{}, OptionsPattern[]]:=
	Block[{g,ctr=1,scaleF,rotateF,rotate2F,values,loc,tilt,turn,offAxis,center,direction,rotateOrder,imageDimensions},
		{tilt,turn,offAxis,center,direction,rotateOrder} = OptionValue[{"Tilt","Turn","AxisRotation","Center","Direction","RotateOrder"}];
		offAxis=Join[{{0,direction}},If[rotateOrder==="zyx",offAxis,{offAxis[[1]],offAxis[[3]],offAxis[[2]]}]]/.{0->.001,90->89.999,180->179.99,-90->-89.999,-180->-179.99};
		values = pro7[allTiltTurn[tilt+$MachineEpsilon, turn+$MachineEpsilon, offAxis]][[1,-1]];
		g = string;
		Switch[offAxis[[ctr++, 2]],
			"Right",
				g = GeometricTransformation[GeometricTransformation[GeometricTransformation[Inset[Style[g, Sequence@@style], center], RotationTransform[#[[1]]Degree, center]], ScalingTransform[{#[[2]], 1}, center]], RotationTransform[#[[3]]Degree, center]],
			"Top",
				g = GeometricTransformation[GeometricTransformation[GeometricTransformation[Inset[Style[g, Sequence@@style], center], RotationTransform[#[[5]]Degree, center]], ScalingTransform[{#[[6]], 1}, center]], RotationTransform[#[[7]]Degree, center]],
			"Left",
				g = GeometricTransformation[GeometricTransformation[GeometricTransformation[Inset[Style[g, Sequence@@style], center], RotationTransform[#[[9]]Degree, center]], ScalingTransform[{#[[10]], 1}, center]], RotationTransform[#[[11]]Degree, center]]
		]&/@{values};
		g
	]

(* final image output *)
Options[dwAxoImage]:={"Tilt"->30,"Turn"->30,"AxisRotation"->{{0,"Top"},{0,"Right"},{0,"Left"}},"Center"->{0,0},"Direction"->"Right","RotateOrder"->"zyx"};
dwAxoImage[image_:{}, selected_:{}, OptionsPattern[]]:=
	Block[{g,ctr=1,scaleF,rotateF,rotate2F,values,loc,tilt,turn,offAxis,center,direction,rotateOrder,imageDimensions},
		{tilt,turn,offAxis,center,direction,rotateOrder} = OptionValue[{"Tilt","Turn","AxisRotation","Center","Direction","RotateOrder"}];
		offAxis=Join[{{0,direction}},If[rotateOrder==="zyx",offAxis,{offAxis[[1]],offAxis[[3]],offAxis[[2]]}]]/.{0->.001,90->89.999,180->179.99,-90->-89.999,-180->-179.99};
		values = pro7[allTiltTurn[tilt+$MachineEpsilon, turn+$MachineEpsilon, offAxis]][[1,-1]];
		g = image;
		Do[
	 		imageDimensions = ImageDimensions[g[[n]]];
			Switch[offAxis[[ctr++, 2]],
				"Right",
					g[[n]] = ImageCrop[Rasterize[Rotate[ImageResize[Rasterize[Rotate[g[[n]], #[[1]]Degree, center], Background->None], {Scaled[#[[2]]], All}], #[[3]]Degree, center], Background->None]],
				"Top",
					g[[n]] = ImageCrop[Rasterize[Rotate[ImageResize[Rasterize[Rotate[g[[n]], #[[5]]Degree, center], Background->None], {Scaled[#[[6]]], All}], #[[7]]Degree, center], Background->None]],
				"Left",
					g[[n]] = ImageCrop[Rasterize[Rotate[ImageResize[Rasterize[Rotate[g[[n]], #[[9]]Degree, center], Background->None], {Scaled[#[[10]]], All}], #[[11]]Degree, center], Background->None]]
			]&/@{values},
		{n, selected}];
		g
	]

(* final output *)
Options[dwAxo]:={"Tilt"->30,"Turn"->30,"AxisRotation"->{{0,"Top"},{0,"Right"},{0,"Left"}},"Center"->{0,0},"Direction"->"Right","RotateOrder"->"zyx"};
dwAxo[pts_:{}, selected_:{}, OptionsPattern[]]:=
	Block[{g,ctr=1,scaleF,rotateF,rotate2F,values,loc,tilt,turn,offAxis,center,direction,rotateOrder,imageDimensions},
		{tilt,turn,offAxis,center,direction,rotateOrder} = OptionValue[{"Tilt","Turn","AxisRotation","Center","Direction","RotateOrder"}];
		offAxis=Join[{{0,direction}},If[rotateOrder==="zyx",offAxis,{offAxis[[1]],offAxis[[3]],offAxis[[2]]}]]/.{0->.001,90->89.999,180->179.99,-90->-89.999,-180->-179.99};
		values = pro7[allTiltTurn[tilt+$MachineEpsilon, turn+$MachineEpsilon, offAxis]][[1,-1]];
		g = pts;
		Do[
			If[Length[g[[n]]] > 1,
				ctr = 1;
				Switch[offAxis[[ctr++, 2]],
					"Right",
						rotateF = RotationTransform[#[[1]]Degree, center];
						scaleF = ScalingTransform[#[[2]],{1,0},center];
						rotate2F = RotationTransform[#[[3]]Degree, center];
						g[[n]] = rotate2F[scaleF[rotateF[g[[n]]]]],
					"Top",
						rotateF = RotationTransform[#[[5]]Degree, center];
						scaleF = ScalingTransform[#[[6]],{1,0},center];
						rotate2F = RotationTransform[#[[7]]Degree, center];
						g[[n]] = rotate2F[scaleF[rotateF[g[[n]]]]],
					"Left",
						rotateF = RotationTransform[#[[9]]Degree, center];
						scaleF = ScalingTransform[#[[10]],{1,0},center];
						rotate2F = RotationTransform[#[[11]]Degree, center];
						g[[n]] = rotate2F[scaleF[rotateF[g[[n]]]]]
				]&/@{values},
				g
			],
		{n, selected}];
		g
	]

End[] (* End Private Context *)

EndPackage[]