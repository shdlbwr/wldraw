(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwNudgeTool[]:=
	DynamicModule[{pts},
		Dynamic@Tooltip[
			If[$dwConstrainHAngle >= 0,
				
				(* 2D - using an object with texture causes entire button to turn white *)
				EventHandler[Graphics[{Gray, EdgeForm[{AbsoluteThickness[1], Gray}], 
					Disk[],
					Table[With[{b=buttonParts},
						Inset[
							EventHandler[
								Graphics[{White, b[[1]]}, ImageSize->14],
								{
									"MouseDown" :> (Null),
									"MouseDragged" :> (Null),
									"MouseUp" :> (Null),
									"MouseClicked":>(
										dwSetUndo[];
										pts = $dwP;
										Do[
											If[pts[[If[Head[s] === List,s[[1]],s]]]=!={},
												If[Head[s] === List,
													(* point *)
													Which[
														s[[2]] == 0,
															Nothing,
														b[[2]] === {0,0},
															Nothing,
														$dwHead[[s[[1]]]] === BezierCurve && Mod[s[[2]] + 2, 3] != 0, (* remove all handles *)
															Nothing,
														b[[2,2]] == 0, (* horizontal movement *)
															If[$dwHead[[s[[1]]]] === BezierCurve,
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2]]]*$dwGridSize[[1]]+pts[[Sequence@@samePos]],
																{samePos, DeleteDuplicates[Join[{s[[1]], #[[1]]}&/@Position[pts[[s[[1]]]], pts[[Sequence@@s]]], If[s[[2]] == 1, {s + {0,1}}, {s + {0,-1}, s + {0,1}}]]]}];
																If[$dwP[[s[[1]],1]] === $dwP[[s[[1]],-2]], $dwP[[s[[1]],2]] = $dwP[[s[[1]],-1]], Nothing],
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2]]]*$dwGridSize[[1]]+pts[[Sequence@@samePos]],
																{samePos, {s[[1]],Flatten[#][[1]]}&/@Position[pts[[s[[1]]]],pts[[Sequence@@s]]]}]
															],
														True, (* vertical movement *)
															If[$dwHead[[s[[1]]]] === BezierCurve,
																
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2]]]*$dwGridSize[[2]]+pts[[Sequence@@samePos]],
																{samePos, DeleteDuplicates[Join[{s[[1]], #[[1]]}&/@Position[pts[[s[[1]]]], pts[[Sequence@@s]]], If[s[[2]] == 1, {s + {0,1}}, {s + {0,-1}, s + {0,1}}]]]}];
																If[$dwP[[s[[1]],1]] === $dwP[[s[[1]],-2]], $dwP[[s[[1]],2]] = $dwP[[s[[1]],-1]], Nothing],
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2]]]*$dwGridSize[[2]]+pts[[Sequence@@samePos]],
																{samePos, {s[[1]],Flatten[#][[1]]}&/@Position[pts[[s[[1]]]],pts[[Sequence@@s]]]}]
															]
													],
													(* object *)
													Which[
														b[[2]] === {0,0},
															dwCenterObject[s],
														b[[2,2]] == 0, (* horizontal movement *)
															$dwP[[s]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2]]]*$dwGridSize[[1]]+#&/@pts[[s]],
														True, (* vertical movement *)
															$dwP[[s]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2]]]*$dwGridSize[[2]]+#&/@pts[[s]]
													]
												]
											],
										{s, DeleteDuplicates[Complement[$dwSelected, If[pts[[#[[1]], 1]] === pts[[#[[1]], If[$dwHead[[#[[1]]]] === BezierCurve, -2, -1]]], #, Nothing]&/@Cases[$dwSelected, {_, 1}]]]}];
										dwUpdateBoundingBox[DeleteDuplicates[Complement[$dwSelected, If[pts[[#[[1]], 1]] === pts[[#[[1]], If[$dwHead[[#[[1]]]] === BezierCurve, -2, -1]]], #, Nothing]&/@Cases[$dwSelected, {_, 1}]]]]
									)
								},
								PassEventsUp->False
							],buttonParts[[3]]
						]],
					{buttonParts,
						{
							{Polygon[{{0.,0.5},{0.5,0.},{0.25,0.},{0.25,-0.5},{-0.25,-0.5},{-0.25,0.},{-0.5,0.}}], {0,1}, {0,.6}},
							{Polygon[{{-0.5,0.},{0.,0.5},{0.,0.25},{0.5,0.25},{0.5,-0.25},{0.,-0.25},{0.,-0.5}}], {-1,0}, {-.6,0}},
							{Polygon[{{1/2,0},{0.,-1/2},{0.,-0.25},{-1/2,-0.25},{-1/2,0.25},{0.,0.25},{0.,1/2}}], {1,0}, {.6,0}},
							{Polygon[{{0.,-0.5},{-0.5,0.},{-0.25,0.},{-0.25,0.5},{0.25,0.5},{0.25,0.},{0.5,0.}}], {0,-1}, {0,-.6}},
							{Polygon[{{0,1},{1,0},{0,-1},{-1,0}}], {0,0}, {0,0}}
						}
					}]},ImagePadding->{{0,0},{0,0}}, ImageSize->48],
					{
						"MouseDown" :> (Null),
						"MouseDragged" :> (Null),
						"MouseUp" :> (Null),
						"MouseClicked":>(Null)
					}, PassEventsUp->False
				],
					
				(* 3D *)
				Graphics[{Gray, EdgeForm[{AbsoluteThickness[1], Gray}], Disk[],
					Dynamic@Table[With[{b=buttonParts},
						Inset[
							EventHandler[
								Graphics[{White,b[[1]]},ImageSize->(If[b[[3,1]] == 0, 14, 12])],
								{
									"MouseDown" :> (Null),
									"MouseDragged" :> (Null),
									"MouseUp" :> (Null),
									"MouseClicked":>(
										dwSetUndo;
										pts = $dwP;
										Do[
											If[pts[[If[Head[s] === List,s[[1]],s]]]=!={},
												If[Head[s] === List,
													(* point *)
													Which[
														s[[2]] == 0,
															Nothing,
														b[[2]] === {0,0,0},
															Nothing,
														b[[2,3]] =!= 0, (* x axis *)
															If[$dwHead[[s[[1]]]] === BezierCurve,
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*b[[2,3]]*$dwGridSize[[3]]+pts[[Sequence@@samePos]],
																{samePos, DeleteDuplicates[Join[{s[[1]], #[[1]]}&/@Position[pts[[s[[1]]]], pts[[Sequence@@s]]], If[s[[2]] == 1, {s + {0,1}}, {s + {0,-1}, s + {0,1}}]]]}];
																If[$dwP[[s[[1]],1]] === $dwP[[s[[1]],-2]], $dwP[[s[[1]],2]] = $dwP[[s[[1]],-1]], Nothing],
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*b[[2,3]]*$dwGridSize[[3]]+pts[[Sequence@@samePos]],
																{samePos, {s[[1]],Flatten[#][[1]]}&/@Position[pts[[s[[1]]]],pts[[Sequence@@s]]]}]
															],
														b[[2,2]] == 0, (* y axis *)
															If[$dwHead[[s[[1]]]] === BezierCurve,
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2,{1,2}]]]*$dwGridSize[[1]]+pts[[Sequence@@samePos]],
																{samePos, DeleteDuplicates[Join[{s[[1]], #[[1]]}&/@Position[pts[[s[[1]]]], pts[[Sequence@@s]]], If[s[[2]] == 1, {s + {0,1}}, {s + {0,-1}, s + {0,1}}]]]}];
																If[$dwP[[s[[1]],1]] === $dwP[[s[[1]],-2]], $dwP[[s[[1]],2]] = $dwP[[s[[1]],-1]], Nothing],
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2,{1,2}]]]*$dwGridSize[[1]]+pts[[Sequence@@samePos]],
																{samePos, {s[[1]],Flatten[#][[1]]}&/@Position[pts[[s[[1]]]],pts[[Sequence@@s]]]}]
															],
														True,  (* z axis *)
															If[$dwHead[[s[[1]]]] === BezierCurve,
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2,{1,2}]]]*$dwGridSize[[2]]+pts[[Sequence@@samePos]],
																{samePos, DeleteDuplicates[Join[{s[[1]], #[[1]]}&/@Position[pts[[s[[1]]]], pts[[Sequence@@s]]], If[s[[2]] == 1, {s + {0,1}}, {s + {0,-1}, s + {0,1}}]]]}];
																If[$dwP[[s[[1]],1]] === $dwP[[s[[1]],-2]], $dwP[[s[[1]],2]] = $dwP[[s[[1]],-1]], Nothing],
																Do[
																	$dwP[[Sequence@@samePos]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2,{1,2}]]]*$dwGridSize[[2]]+pts[[Sequence@@samePos]],
																{samePos, {s[[1]],Flatten[#][[1]]}&/@Position[pts[[s[[1]]]],pts[[Sequence@@s]]]}]
															]
													],
													(* object *)
													Which[
														b[[2]] === {0,0,0},
															dwCenterObject[s],
														b[[2,3]] =!= 0, (* x axis *)
															$dwP[[s]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*b[[2,3]]*$dwGridSize[[3]]+#&/@pts[[s]],
														b[[2,2]] == 0, (* y axis *)
															$dwP[[s]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2,{1,2}]]]*$dwGridSize[[1]]+#&/@pts[[s]],
														True,  (* z axis *)
															$dwP[[s]] = If[CurrentValue[$dwCommandKey], $dwNudgeMultiplier, 1]*Total[b[[2,{1,2}]]]*$dwGridSize[[2]]+#&/@pts[[s]]
													]
												]
											],
										{s,Complement[$dwSelected, If[pts[[#[[1]], 1]] === pts[[#[[1]], If[$dwHead[[#[[1]]]] === BezierCurve, -2, -1]]], #, Nothing]&/@Cases[$dwSelected, {_, 1}]]}];
										dwUpdateBoundingBox[Complement[$dwSelected, If[pts[[#[[1]], 1]] === pts[[#[[1]], If[$dwHead[[#[[1]]]] === BezierCurve, -2, -1]]], #, Nothing]&/@Cases[$dwSelected, {_, 1}]]]
									)
								},
								PassEventsUp->False
							],buttonParts[[3]]
						]],
					{buttonParts,
						{
							{Polygon[{{0.,0.5},{0.5,0.},{0.25,0.},{0.25,-0.5},{-0.25,-0.5},{-0.25,0.},{-0.5,0.}}], {0,0,1}, {0,.6}},
							{Polygon[{{0.4040610178208843,0.40406101782088427},{0.4040610178208843,-0.30304576336566313},{0.2272843225242478,-0.12626906806902632},{-0.12626906806902605,-0.4798224586623},{-0.4798224586622999,-0.1262690680690262},{-0.12626906806902605,0.2272843225242474},{-0.303045763365663,0.40406101782088427}}], {0,1,0}, {.45,.3}},
							{Polygon[{{0.4040610178208843,-0.4040610178208842},{0.4040610178208843,0.3030457633656632},{0.22728432252424735,0.12626906806902632},{-0.12626906806902627,0.47982245866230006},{-0.4798224586623001,0.12626906806902632},{-0.12626906806902627,-0.22728432252424724},{-0.30304576336566313,-0.4040610178208842}}], {-1,0,0}, {.45,-.3}},
							{Polygon[{{0.,-0.5},{0.5,0.},{0.25,0.},{0.25,0.5},{-0.25,0.5},{-0.25,0.},{-0.5,0.}}],{0,0,-1},{0,-.6}},
							{Polygon[{{-0.4040610178208843,-0.4040610178208842},{-0.4040610178208843,0.30304576336566313},{-0.22728432252424735,0.12626906806902632},{0.12626906806902627,0.4798224586623},{0.47982245866230033,0.12626906806902632},{0.12626906806902627,-0.22728432252424724},{0.30304576336566325,-0.4040610178208842}}], {0,-1,0}, {-.45,-.3}},
							{Polygon[{{-0.4040610178208843,0.40406101782088427},{-0.4040610178208843,-0.30304576336566313},{-0.2272843225242478,-0.12626906806902632},{0.12626906806902605,-0.47982245866229994},{0.4798224586623001,-0.1262690680690262},{0.12626906806902605,0.2272843225242474},{0.303045763365663,0.40406101782088427}}], {1,0,0}, {-.45,.3}},
							{Polygon[{{0,1},{1,0},{0,-1},{-1,0}}], {0,0,0}, {0,0}}
						}
						
					}]},ImagePadding->{{0,0},{0,0}}, ImageSize->48]
			],
		Row[{"Nudge selection\nClick one of the direction arrows to move 1 grid unit\n"<>$dwCommandKey<>"-click to move ",$dwNudgeMultiplier," grid units\nClick center diamond to center object"}], TooltipDelay->$dwTooltipDelay]
	]
	
End[] (* End Private Context *)

EndPackage[]