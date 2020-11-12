(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeMap[]:=
	If[!$NetworkConnected || FailureQ[Entity["GeographicRegion", $dwShapeMapContinent]],
		
		MessageDialog["Unable to access world shapes."],
		
		CreateDialog[
			DynamicModule[{temp, temp2, originalLength, coordBounds, pts={}, pts2={}, res="Polygon", center, a, b, c, d, popUpMenuSize=150},
				center =	Which[
								$dwShapeMapContinent === "World",
									"Africa",
								$dwShapeMapCountry === All,
									Switch[$dwShapeMapContinent,"NorthAmerica","UnitedStates","SouthAmerica","Paraguay","Australia","Australia","Antarctica","Antarctica","Africa","CentralAfricanRepublic",_,"UnitedStates"],
								$dwShapeMapRegion === All,
									$dwShapeMapCountry,
								True,
									If[MemberQ[{"AllRegions", "MainlandRegions"}, $dwShapeMapRegion], $dwShapeMapCountry, $dwShapeMapRegion]
							];
				
				Pane[
					Grid[{
						{Dynamic[If[$dwShapeMapDrawPreview,
							Graphics[{
								
								(* axes and frame *)
								{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
								
								(* map *)
								(* non-rectangular world boundaries - must be included before country points for proper size *)
								pts = If[$dwShapeMapContinent === "World",
										Switch[$dwShapeMapProjection,
											"LambertAzimuthal",
												{Table[2{Sin[x],Cos[x]}, {x, 0, 2Pi, Pi/40}]},
											"Orthographic",
												{Table[{Sin[x],Cos[x]}, {x, 0, 2Pi, Pi/40}]},
											"Equirectangular"|"MillerCylindrical"|"Mercator",
												{},
											_,
												#[[1]]&/@(Cases[GeoGraphics[GeoRange->"World",GeoBackground->GeoStyling[{}],GeoCenter->Entity["GeographicRegion",center],GeoProjection->$dwShapeMapProjection],_Polygon,Infinity])/.{{}->Sequence[]}
										],
										{}
									];
								pts2 = If[$dwShapeMapContinent === "World",
											(* world uses Polygon and Line from FilledCurve *)
											Join[
												Flatten[#[[1]]&/@((Cases[GeoGraphics["World", GeoBackground->"Coastlines", GeoCenter->Entity["GeographicRegion",center], GeoProjection->$dwShapeMapProjection], _Polygon, Infinity])[[2 ;; -1]]), 1],
												#[[1]]&/@(Cases[GeoGraphics["World",GeoBackground->"Coastlines",GeoCenter->Entity["GeographicRegion",center],GeoProjection->$dwShapeMapProjection],_Line,Infinity])
											]/.{{}->Sequence[]},
											
											(* continents *)
											(* NOTE: WL version 12 added projection box with country so keep list of depth 5 only *)
											If[$dwShapeMapCountry === All, 
												Switch[$dwShapeMapContinent,
													"Europe", (* delete Russia; must use GeoListPlot instead of GeoGraphics to remove Netherlands extras *)
														If[Depth[#] == 5, #[[1,1]], Nothing]&/@Cases[GeoListPlot[Entity["Country",#]["SchematicPolygon"]&/@((#[[2]]&/@Entity["GeographicRegion",$dwShapeMapContinent]["Countries"])/.{"Russia"->Nothing}),GeoCenter->Entity["Country",center], GeoBackground->None, GeoProjection->$dwShapeMapProjection],_Polygon,Infinity]/.{GeoPosition[_]->Sequence[]},
													"Asia", (* add Russia *)
														If[Depth[#] == 5, #[[1,1]], Nothing]&/@Cases[GeoGraphics[Entity["Country",#][{"SchematicPolygon", $dwShapeMapProjection}]&/@Join[(#[[2]]&/@Entity["GeographicRegion",$dwShapeMapContinent]["Countries"]),{"Russia"}],GeoCenter->Entity["Country",center], GeoBackground->None, GeoProjection->$dwShapeMapProjection],_Polygon,Infinity]/.{GeoPosition[_]->Sequence[]},
													"NorthAmerica",(* add Alaska *)
														Join[If[Depth[#]==5,#[[1,1]],Nothing]&/@Cases[GeoGraphics[Entity["Country",#][{"SchematicPolygon",$dwShapeMapProjection}]&/@(#[[2]]&/@Entity["GeographicRegion",$dwShapeMapContinent]["Countries"]),GeoCenter->Entity["Country","UnitedStates"],GeoBackground->None,GeoProjection->$dwShapeMapProjection],_Polygon,Infinity],(If[Depth[#]==5,#[[1]],Nothing]&/@Cases[GeoGraphics[Entity["AdministrativeDivision", {"Alaska", "UnitedStates"}]["Polygon"],GeoCenter->Entity["Country","UnitedStates"],GeoProjection->$dwShapeMapProjection],_Polygon,Infinity])[[-1]]],
													_,
														If[Depth[#] == 5, #[[1,1]], Nothing]&/@Cases[GeoGraphics[Entity["Country",#][{"SchematicPolygon", $dwShapeMapProjection}]&/@(#[[2]]&/@Entity["GeographicRegion",$dwShapeMapContinent]["Countries"]),GeoCenter->Entity["Country",center], GeoBackground->None, GeoProjection->$dwShapeMapProjection],_Polygon,Infinity]/.{GeoPosition[_]->Sequence[]}
												],
												
												If[$dwShapeMapRegion === All, 
													(* countries *)
													Join[Sequence@@(If[Depth[#] == 5, #[[1]], Nothing]&/@Cases[GeoGraphics[Entity["Country", $dwShapeMapCountry][res], GeoCenter->Entity["Country", center], GeoBackground->None, GeoProjection->$dwShapeMapProjection][[1, 1]], _Polygon, Infinity])],
													(* regions *)
													Switch[$dwShapeMapRegion,
														"AllRegions",
															center = $dwShapeMapCountry;
															If[MissingQ[Entity["Country", $dwShapeMapCountry][EntityProperty["Country", "AdministrativeDivisions"]]],
																{},
																Join[Sequence@@Table[
																	If[MissingQ[Entity["AdministrativeDivision", {r[[-1,1]], $dwShapeMapCountry}][res]],
																		{},
																		Join[Sequence@@(If[Depth[#] == 5, #[[1]], Nothing]&/@Cases[GeoGraphics[Entity["AdministrativeDivision", {r[[-1,1]], $dwShapeMapCountry}][res], GeoCenter->Entity["Country", center], GeoBackground->None, GeoProjection->$dwShapeMapProjection][[1, 1]], _Polygon, Infinity])]
																	],
																{r, Entity["Country", $dwShapeMapCountry][EntityProperty["Country", "AdministrativeDivisions"]]}]]
															],
														"MainlandRegions",
															center = $dwShapeMapCountry;
															Join[Sequence@@Table[
																If[MissingQ[Entity["AdministrativeDivision", {r[[-1,1]], $dwShapeMapCountry}][res]],
																	{},
																	Join[Sequence@@(If[Depth[#] == 5, #[[1]], Nothing]&/@Cases[GeoGraphics[Entity["AdministrativeDivision", {r[[-1,1]], $dwShapeMapCountry}][res], GeoCenter->Entity["Country", center], GeoBackground->None, GeoProjection->$dwShapeMapProjection][[1, 1]], _Polygon, Infinity])]
																],
															{r, ReplaceAll[Entity["Country", $dwShapeMapCountry][EntityProperty["Country", "AdministrativeDivisions"]], Table[Entity[_, {s, _}] -> Nothing, {s, {"Alaska", "Hawaii"}}]]}]],
														
														_,
															(* single region *)
															If[MissingQ[Entity["AdministrativeDivision", {$dwShapeMapRegion, $dwShapeMapCountry}][res]],
																{},
																Join[Sequence@@(If[Depth[#] == 5, #[[1]], Nothing]&/@Cases[GeoGraphics[Entity["AdministrativeDivision", {$dwShapeMapRegion, $dwShapeMapCountry}][res], GeoCenter->Entity["AdministrativeDivision", {center, $dwShapeMapCountry}], GeoBackground->None, GeoProjection->$dwShapeMapProjection][[1, 1]], _Polygon, Infinity])]
															]
													]
												]
												
											]
										];
										
								pts = If[Flatten[pts2] === {}, pts, Join[pts, pts2]];
								If[pts =!= {},
									
									(* point reduction *)
									If[res === "Polygon" && ($dwShapeMapCountry =!= All && $dwShapeMapContinent =!= "World"), pts = dwSimplifyPts[#, $dwPointDecreaseRate]&/@pts];
									(* point scaling *)
									If[$dwShapeMapContinent === "World" && $dwShapeMapProjection === "Orthographic",
										Nothing,
										coordBounds = CoordinateBounds[pts];
										If[FreeQ[Total[Abs[#]]&/@coordBounds, 0.|0],
											temp = Reverse[#[[1]] - #[[2]]&/@coordBounds];
											If[temp[[1]] > temp[[2]],
												temp2 = RescalingTransform[coordBounds,{{-1,1},Divide[Sequence@@temp]{-1,1}}],
												temp2 = RescalingTransform[coordBounds,{Divide[Sequence@@Reverse[temp]]{-1,1},{-1,1}}]
											];
											pts = temp2[#]&/@pts,
											Nothing
										]; 
									];
									(* rectangular world boundaries - must be after country points to calculate size *)
									{{a, b}, {c, d}} = CoordinateBounds[pts];
									pts = Join[
										If[$dwShapeMapContinent === "World",
											Switch[$dwShapeMapProjection,
												"Equirectangular"|"MillerCylindrical"|"Mercator",
													{{{a, c}, {b, c}, {b, d}, {a, d}}},
												_,
													{}
											],
											{}
										], pts
									];,
									
									Text[Style["No shape\navailable", 18, Gray], {0,0}]
									
								],
								
								Sequence@@$dwFullDefaultStyle[[$dwStyleStart;;-1]],
								Polygon[#]&/@pts
								
							}, Background->White, ImageSize->{300,300}, PlotRange->1.1],
						Pane[Button[Pane[Style["Click here to generate points. Please be patient while loading.",LineIndent->0,TextAlignment->Center],ImageSize->140,Alignment->Center],$dwShapeMapDrawPreview = True,ImageSize->{150,100}], Alignment->Center, ImageSize->{300,300}]]
						]},
						
						{Row[{"point total: ",Dynamic[If[pts === {}, 0, IntegerPart[Length[Flatten[pts]]/2]]]}]},
						{""},
						{Dynamic[Grid[{
							{"continent", ActionMenu[Dynamic@$dwShapeMapContinent, Join[{"World":>($dwShapeMapContinent = "World"; center="Africa"), Delimiter}, With[{m = #}, m:>($dwShapeMapContinent = m; center = $dwShapeMapCountry = Entity["GeographicRegion",$dwShapeMapContinent]["Countries"][[1,2]]; $dwShapeMapRegion = All)]&/@{"Africa", "Antarctica", "Asia", "Australia", "Europe", "NorthAmerica", "SouthAmerica"}], Appearance->"PopupMenu", ImageSize->popUpMenuSize]},
							{Sequence@@If[$dwShapeMapContinent =!= "World", {"country", Dynamic@ActionMenu[Dynamic@$dwShapeMapCountry, 
								Switch[$dwShapeMapContinent,
									"World",
										{},
									"Europe",
										Join[{All:>(center = "Poland"; $dwShapeMapCountry = All), Delimiter}, With[{m = #}, m:>(center = $dwShapeMapCountry = m; $dwShapeMapRegion = All)]&/@((#[[2]]&/@Entity["GeographicRegion",$dwShapeMapContinent]["Countries"])/.{"Russia"->Nothing})],
									"Asia",
										Join[{All:>(center = "Mongolia"; $dwShapeMapCountry = All), Delimiter}, With[{m = #}, m:>(center = $dwShapeMapCountry = m; $dwShapeMapRegion = All)]&/@(Sort[Join[{"Russia"}, #[[2]]&/@Entity["GeographicRegion",$dwShapeMapContinent]["Countries"]]])],
									_,
										Join[{All:>(center = Switch[$dwShapeMapContinent,"NorthAmerica","UnitedStates","SouthAmerica","Paraguay","Australia","Australia","Antarctica","Antarctica","Africa","CentralAfricanRepublic",_,"UnitedStates"]; $dwShapeMapCountry = All), Delimiter}, With[{m = #}, m:>(center = $dwShapeMapCountry = m; $dwShapeMapRegion = All)]&/@(#[[2]]&/@Entity["GeographicRegion", $dwShapeMapContinent]["Countries"])]
								], Appearance->"PopupMenu", ImageSize->popUpMenuSize]}, Null]},
							{Sequence@@If[$dwShapeMapContinent =!= "World" && $dwShapeMapCountry =!= All, {"region", Dynamic@ActionMenu[Dynamic@$dwShapeMapRegion, 
								Join[{All:>(center = $dwShapeMapCountry; $dwShapeMapRegion = All), 
									Delimiter, 
									"All regions":>($dwShapeMapRegion = "AllRegions"; center = $dwShapeMapCountry; $dwPointDecreaseRate = 100; res = "Polygon"), 
									If[$dwShapeMapCountry === "UnitedStates",
										"Mainland regions":>($dwShapeMapRegion = "MainlandRegions"; center = $dwShapeMapCountry; $dwPointDecreaseRate = 100; res = "Polygon"),
										Null], 
									Delimiter}/.{Null->Sequence[]},
									Flatten[{
										With[{m = #[[1]]},
										m:>(center = $dwShapeMapRegion = m; $dwPointDecreaseRate = 100; res = "Polygon")]&/@If[Entity["Country", $dwShapeMapCountry][EntityProperty["Country","AdministrativeDivisions"]] =!= Missing["NotAvailable"], #[[2]]&/@Entity["Country",$dwShapeMapCountry][EntityProperty["Country","AdministrativeDivisions"]],{}]
									}]
								], Appearance->"PopupMenu", ImageSize->popUpMenuSize]}, Null]},
							{Sequence@@If[$dwShapeMapCountry === All || $dwShapeMapContinent === "World" || $dwShapeMapRegion =!= All, Null, {"resolution", PopupMenu[Dynamic@res, {"SchematicPolygon"->"Simple","Polygon"->"Detailed"}, Alignment->Center, ImageSize->popUpMenuSize]}]},
							{Sequence@@If[res === "Polygon" && ($dwShapeMapCountry =!= All && $dwShapeMapContinent =!= "World"), {"point removal", PopupMenu[Dynamic@$dwPointDecreaseRate, {100->"None", 1->"Very Low", .7->"Low", .5->"Medium", .3->"High", .1->"Very High"}, Alignment->Center, ImageSize->popUpMenuSize]},Null]},
							{"projection", PopupMenu[Dynamic@$dwShapeMapProjection, dwGeoProjections[], Alignment->Center, ImageSize->popUpMenuSize]},
							{"projection center", Dynamic@PopupMenu[Dynamic@center, 
									Which[
										$dwShapeMapContinent === "World",
											{"Africa", "Antarctica", "Asia", "Australia", "Europe", "NorthAmerica", "SouthAmerica"},
										$dwShapeMapRegion === All,
											Switch[$dwShapeMapContinent,
												"Europe",
													((#[[2]]&/@Entity["GeographicRegion",$dwShapeMapContinent]["Countries"])/.{"Russia"->Nothing}),
												"Asia",
													Sort@Join[{"Russia"}, #[[2]]&/@Entity["GeographicRegion",$dwShapeMapContinent]["Countries"]],
												_,
													#[[2]]&/@Entity["GeographicRegion", $dwShapeMapContinent]["Countries"]
											],
										True,
											If[MemberQ[{"AllRegions", "MainlandRegions"}, $dwShapeMapRegion], {$dwShapeMapCountry}, {$dwShapeMapRegion}]
									], Alignment->Center, ImageSize->popUpMenuSize
								]}
						}, Alignment->{{Right,Left},Center}]/.{{Null}->Sequence[]}]},
						{""},
						{Row[{CancelButton[$dwShapeMapDrawPreview = False; $dwMessageBarText = ""; DialogReturn[]],
							DefaultButton[
								(* add to canvas *)
								originalLength = Length[$dwP];	
								Do[
									dwNewEmptyLayer["Head"->Polygon];
									$dwStyle[[$dwSelected[[1]]]] = $dwFullDefaultStyle;
									$dwP[[$dwSelected[[1]]]] = pts[[n]];
									(* update bounding box *)
									dwUpdateBoundingBox[$dwSelected[[{1}]]],
								{n, Length[pts]}];
								(* update groups and selection *)
								If[Length[pts] > 1, $dwGroupLayers = Join[$dwGroupLayers, {Range[originalLength+1, Length[$dwP]]}]];
								If[Length[pts] > 1, $dwSelected = Range[originalLength+1, Length[$dwP]]];
								$dwPointQuantity = Length[Flatten[$dwP, 1]];
								$dwShapeMapDrawPreview = False;
								$dwMessageBarText = "";
								DialogReturn[]
							]}]}
						}, Alignment->Center],
				ImageSize->300]
			], Background->LightGray, WindowTitle->"World shapes"
		]
	]
	
dwGeoProjections[]:= {"LambertAzimuthal"->dwIconLambertAzimuthal[], "Orthographic"->dwIconOrthographic[], "Sinusoidal"->dwIconSinusoidal[], "Mollweide"->dwIconMollweide[], "Robinson"->dwIconRobinson[], "Equirectangular"->dwIconEquirectangular[], "MillerCylindrical"->dwIconMillerCylindrical[], "Mercator"->dwIconMercator[]}

dwIconLambertAzimuthal[]:=Graphics[{BezierCurve[{{0.,1.},{0.55,1.},{1.,0.55},{1.,0.},{1.,-0.55},{0.55,-1.},{0.,-1.},{-0.55,-1.},{-1.,-0.55},{-1.,0.},{-1.,0.55},{-0.55,1.},{0.,1.}}],Line[{{-1.,0.},{1.,0.}}],Line[{{0.,-1.},{0.,1.}}],BezierCurve[{{0.,0.85},{0.2,0.85},{0.35,0.8},{0.35,0.65},{0.35,0.5},{0.167,0.475},{0.,0.475},{-0.167,0.475},{-0.35,0.5},{-0.35,0.65},{-0.35,0.8},{-0.2,0.85},{0.,0.85}}],BezierCurve[{{0.,0.925},{0.45,0.925},{0.7,0.6},{0.7,0.5},{0.7,0.4},{0.55,0.25},{0.,0.25},{-0.55,0.25},{-0.7,0.4},{-0.7,0.5},{-0.7,0.6},{-0.45,0.925},{0.,0.925}}],BezierCurve[{{0,-0.85},{0.2,-0.85},{0.35,-0.8},{0.35,-0.65},{0.35,-0.5},{0.167,-0.475},{0,-0.475},{-0.167,-0.475},{-0.35,-0.5},{-0.35,-0.65},{-0.35,-0.8},{-0.2,-0.85},{0,-0.85}}],BezierCurve[{{0,-0.925},{0.45,-0.925},{0.7,-0.6},{0.7,-0.5},{0.7,-0.4},{0.55,-0.25},{0,-0.25},{-0.55,-0.25},{-0.7,-0.4},{-0.7,-0.5},{-0.7,-0.6},{-0.45,-0.925},{0,-0.925}}],BezierCurve[{{0.,0.7},{0.4,0.7},{0.705,0.4},{0.7,0.},{0.7,-0.4},{0.4,-0.7},{0.,-0.7},{-0.4,-0.7},{-0.7,-0.4},{-0.7,0.},{-0.7,0.4},{-0.4,0.705},{0.,0.7}}],BezierCurve[{{0.,-0.7},{0.,-0.7},{-0.4,-0.5},{-0.4,0.},{-0.4,0.5},{0.,0.7},{0.,0.7}}],BezierCurve[{{0,-0.7},{0,-0.7},{0.35,-0.5},{0.4,0},{0.4,0.5},{0,0.7},{0,0.7}}]},ImageSize->40]

dwIconOrthographic[]:=Graphics[{BezierCurve[{{0.,1.},{0.55,1.},{1.,0.55},{1.,0.},{1.,-0.55},{0.55,-1.},{0.,-1.},{-0.55,-1.},{-1.,-0.55},{-1.,0.},{-1.,0.55},{-0.55,1.},{0.,1.}}],Line[{{-1.,0.},{1.,0.}}],Line[{{0.,1.},{0.,-1.}}],Line[{{-0.9,0.4},{0.9,0.4}}],Line[{{-0.65,0.75},{0.65,0.75}}],Line[{{-0.65,-0.75},{0.65,-0.75}}],Line[{{-0.9,-0.4},{0.9,-0.4}}],BezierCurve[{{0.,1.},{0.17325,1.},{0.55,0.7},{0.55,0.},{0.55,-0.7},{0.17325,-1.},{0.,-1.},{-0.17325,-1.},{-0.55,-0.7},{-0.55,0.},{-0.55,0.7},{-0.17325,1.},{0.,1.}}],BezierCurve[{{0.,1.},{0.3575,1.},{0.85,0.7},{0.85,0.},{0.85,-0.7},{0.3575,-1.},{0.,-1.},{-0.3575,-1.},{-0.85,-0.7},{-0.85,0.},{-0.85,0.7},{-0.3575,1.},{0.,1.}}]},ImageSize->40]

dwIconSinusoidal[]:=Graphics[{BezierCurve[{{0.,1.},{0.,1.},{-1.,0.45},{-1.,0.},{-1.,-0.45},{0.,-1.},{0.,-1.}}],BezierCurve[{{0.,1.},{0.,1.},{-0.35,0.45},{-0.35,0.},{-0.35,-0.45},{0.,-1.},{0.,-1.}}],BezierCurve[{{0.,1.},{0.,1.},{-0.7,0.45},{-0.7,0.},{-0.7,-0.45},{0.,-1.},{0.,-1.}}],BezierCurve[{{0.,1.},{0.,1.},{1.,0.45},{1.,0.},{1.,-0.45},{0.,-1.},{0.,-1.}}],BezierCurve[{{0.,1.},{0.,1.},{0.35,0.45},{0.35,0.},{0.35,-0.45},{0.,-1.},{0.,-1.}}],BezierCurve[{{0.,1.},{0.,1.},{0.7,0.45},{0.7,0.},{0.7,-0.45},{0.,-1.},{0.,-1.}}],Line[{{-1.,0.},{1.,0.}}],Line[{{-0.85,0.35},{0.85,0.35}}],Line[{{-0.45,0.7},{0.45,0.7}}],Line[{{-0.45,-0.7},{0.45,-0.7}}],Line[{{-0.85,-0.35},{0.85,-0.35}}],Line[{{0.,-1.},{0.,1.}}]},AspectRatio->.75,ImageSize->40]

dwIconMollweide[]:=Graphics[{BezierCurve[{{0.,1.},{0.55,1.},{1.,0.55},{1.,0.},{1.,-0.55},{0.55,-1.},{0.,-1.},{-0.55,-1.},{-1.,-0.55},{-1.,0.},{-1.,0.55},{-0.55,1.},{0.,1.}}],BezierCurve[{{0.,1.},{0.3575,1.},{0.65,0.55},{0.65,0.},{0.65,-0.55},{0.3575,-1.},{0.,-1.},{-0.3575,-1.},{-0.65,-0.55},{-0.65,0.},{-0.65,0.55},{-0.3575,1.},{0.,1.}}],BezierCurve[{{0.,1.},{0.17325,1.},{0.315,0.55},{0.315,0.},{0.315,-0.55},{0.17325,-1.},{0.,-1.},{-0.17325,-1.},{-0.315,-0.55},{-0.315,0.},{-0.315,0.55},{-0.17325,1.},{0.,1.}}],Line[{{-1.,0.},{1.,0.}}],Line[{{0.,1.},{0.,-1.}}],Line[{{-0.9,0.4},{0.9,0.4}}],Line[{{-0.65,0.75},{0.65,0.75}}],Line[{{-0.65,-0.75},{0.65,-0.75}}],Line[{{-0.9,-0.4},{0.9,-0.4}}]},AspectRatio->.75,ImageSize->40]

dwIconRobinson[]:=Graphics[{BezierCurve[{{-0.55,1.},{-0.55,1.},{-1.,0.7},{-1.,0.},{-1.,-0.7},{-0.55,-1.},{-0.55,-1.}}],BezierCurve[{{-0.3425,1.},{-0.3425,1.},{-0.6575,0.7},{-0.6575,0.},{-0.6575,-0.7},{-0.3425,-1.},{-0.3425,-1.}}],BezierCurve[{{-0.1825,1.},{-0.1825,1.},{-0.3215,0.7},{-0.3215,0.},{-0.3215,-0.7},{-0.1825,-1.},{-0.1825,-1.}}],BezierCurve[{{0.55,1.},{0.55,1.},{1.,0.7},{1.,0.},{1.,-0.7},{0.55,-1.},{0.55,-1.}}],BezierCurve[{{0.35,1.},{0.35,1.},{0.65,0.7},{0.65,0.},{0.65,-0.7},{0.35,-1.},{0.35,-1.}}],BezierCurve[{{0.175,1.},{0.175,1.},{0.325,0.7},{0.325,0.},{0.325,-0.7},{0.175,-1.},{0.175,-1.}}],Line[{{-1.,0.},{1.,0.}}],Line[{{0.,1.},{0.,-1.}}],Line[{{-0.55,1.},{0.55,1.}}],Line[{{-0.55,-1.},{0.55,-1.}}],Line[{{-0.95,0.35},{0.95,0.35}}],Line[{{-0.8,0.7},{0.8,0.7}}],BezierCurve[{{-0.95,-0.35},{-0.95,-0.35},{0.95,-0.35},{0.95,-0.35}}],BezierCurve[{{-0.8,-0.7},{-0.8,-0.7},{0.8,-0.7},{0.8,-0.7}}]},AspectRatio->.75,ImageSize->40]

dwIconEquirectangular[]:=Graphics[{Line[{{-1.,1.},{1.,1.},{1.,-1.},{-1.,-1.},{-1.,1.}}],Line[{{-1.,0.},{1.,0.}}],Line[{{-1.,0.33},{1.,0.33}}],Line[{{-1.,0.66},{1.,0.66}}],Line[{{-1.,-0.66},{1.,-0.66}}],Line[{{-1.,-0.33},{1.,-0.33}}],Line[{{-0.75,1.},{-0.75,-1.}}],Line[{{-0.5,1.},{-0.5,-1.}}],Line[{{-0.25,1.},{-0.25,-1.}}],Line[{{0.,1.},{0.,-1.}}],Line[{{0.25,1.},{0.25,-1.}}],Line[{{0.5,1.},{0.5,-1.}}],Line[{{0.75,1.},{0.75,-1.}}]},AspectRatio->.5,ImageSize->40]

dwIconMillerCylindrical[]:=Graphics[{Line[{{-1.,1.},{1.,1.},{1.,-1.},{-1.,-1.},{-1.,1.}}],Line[{{-1.,0.},{1.,0.}}],Line[{{-1.,0.2},{1.,0.2}}],Line[{{-1.,0.55},{1.,0.55}}],Line[{{-1.,-0.55},{1.,-0.55}}],Line[{{-1.,-0.2},{1.,-0.2}}],Line[{{-0.75,1.},{-0.75,-1.}}],Line[{{-0.5,1.},{-0.5,-1.}}],Line[{{-0.25,1.},{-0.25,-1.}}],Line[{{0.,1.},{0.,-1.}}],Line[{{0.25,1.},{0.25,-1.}}],Line[{{0.5,1.},{0.5,-1.}}],Line[{{0.75,1.},{0.75,-1.}}]},AspectRatio->.7,ImageSize->40]

dwIconMercator[]:=Graphics[{Line[{{-1.,1.},{1.,1.},{1.,-1.},{-1.,-1.},{-1.,1.}}],Line[{{-1.,0.},{1.,0.}}],Line[{{-1.,0.2},{1.,0.2}}],Line[{{-1.,0.4},{1.,0.4}}],Line[{{-1.,-0.4},{1.,-0.4}}],Line[{{-1.,-0.2},{1.,-0.2}}],Line[{{-0.75,1.},{-0.75,-1.}}],Line[{{-0.5,1.},{-0.5,-1.}}],Line[{{-0.25,1.},{-0.25,-1.}}],Line[{{0.,1.},{0.,-1.}}],Line[{{0.25,1.},{0.25,-1.}}],Line[{{0.5,1.},{0.5,-1.}}],Line[{{0.75,1.},{0.75,-1.}}]},ImageSize->40]

End[] (* End Private Context *)

EndPackage[]