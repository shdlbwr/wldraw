(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeCityBuilder[]:=
	CreateDialog[								
		DynamicModule[{buildingColorQuantityOutput, filename, finalScale = 1, temp, temp1, temp2, temp3, topview1, topview2, 
			chimneyWidth = .125, carsize, extrude, maxRowsAndColumns = 4, contrast = .6, currentBuildingPattern = "Default",
			shapePts = {{-0.2, 0.2}, {0.2, 0.2}, {0.2, -0.2}, {-0.2, -0.2}}, scaleByHeightFunction, scaleByHeightPts, (*cityTreeIndexList = {},*) cityObjects = .5, cityObjCtr = 0, riverTurns = 4,
			columns = 3, rows = 3, defaultScaleEach = {.5,.6}, scaleEach, defaultScaleHeight = .325, scaleHeight, scaleByHeight = .5, yardSize, streetThickness = .025, subdivideScale = .85, step = .05, groundStep = .025, subdivideDistanceOrigin = {}, reverseHeight = False,
			randomScale, randomHeight = .75, defaultSeed = 22, seed, streetPad = 0, ruralArea = .05, roofHeight = .5, roofHipped = .5, roofQuantity = .65, windowSize = .5, windowStripes = True, flipHeight = 0, 
			removeBlockedStreets = True, showBuildings = True, showEnvironment = True, showTrees = True, showStroke = True, showStreets = True, showWindows = True, subdivideSize, defaultBuildingRemoval = .2, buildingRemoval, exitRoadPattern = "None", exitRoads = {},
			soilLayer = .025, subsoilLayer = 0, rockLayer = 0, bedrockLayer = 0, concreteSize = {0, 0}, curbThickness = .5, minBuildingHeight, maxBuildingHeight = 10, (* 4 is actual max achieved but giving it yardSize *)
			style, extObj, startLength, roofCenter, extraRoofHeight, windowPts, buildingColorQuantity = 5, finalHeight, finalHeightScaled,
			buildingOpacityFill = 1, buildingOpacityStroke = 1, buildingColor, windowColor, roofColor = GrayLevel[.5], waterColor = Hue[.6, .5, 1], colorByHeight = True, invertHeightColor = False, currentBuildingColorQuantity, landEdgeRoughness = 1, 
			streetColor = GrayLevel[.4], streetCurbColor = Black, streetCenterlineDash = {3, 3}, streetCenterlineColor = LightGray, streetCenterlineOpacity = 1, sidewalkWidth = .4, sidewalks, landEdgeLimit = 2,
			backgroundColor = White, barkColor = Hue[.1, 1, .7], concreteColor = GrayLevel[.8], grassColor = Hue[.25, .7, .7], soilColor = Hue[.1, 1, .7], subsoilColor = Hue[.1, .6, .85], rockColor = Hue[.1, .3, .75], bedrockColor = Hue[.1, .2, .65], 
			angleH = $dwConstrainHAngle, angleV = $dwConstrainVAngle, previewButtonSize = 48, extraLineSpace, (*randomTreeCtr,*) randomReal, 
			doorLimit = .12, doorAwningDepth = 42, doorWidth = 24, doorHeight = 6, birdList, birdColor = GrayLevel[.3], birdSize = 1, birdCount, birdVariations, birdShape = .03{{-0.35, 0.05}, {-0.2, 0.1}, {0.2, 0.5}, {0.05, 0.1}, {0.6, 0.}, {0.05, -0.1}, {0.2, -0.5}, {-0.2, -0.1}, {-0.35, -0.05}, {-0.35, 0.05}}, 
			traffic = 1, treeLoc, treeSize = .05, ruralTreeCount = 7500, ruralTreeBalance = .1, treeRandomSize = {.75,1.25}, overallTreeSize = 1, edgeTrees = 1, timingDivider = 3, (* lower timingDivider to increase estimated time *)
			treeTrunk = {{-0.1,0.5},{-0.1,0.3},{-0.15,0.},{-0.1,-0.05},{0.,-0.1},{0.1,-0.05},{0.15,0.},{0.1,0.3},{0.1,0.5}},
			treeTrunkDark = {{-0.1,0.5},{0.,0.4},{0.05,0.05},{-0.05,-0.05},{-0.15,0.},{-0.1,-0.05},{0.,-0.1},{0.1,-0.05},{0.15,0.},{0.1,0.3},{0.1,0.5}}, 
			tree = {{-0.3,0.4},{-0.45,0.45},{-0.55,0.55},{-0.7,0.6},{-0.85,0.75},{-0.8,0.95},{-0.7,1.1},{-0.75,1.25},{-0.65,1.35},{-0.6,1.45},{-0.6,1.6},{-0.4,1.7},{-0.3,1.8},{-0.05,1.9},{0.2,1.9},{0.35,1.8},{0.5,1.75},{0.6,1.6},{0.7,1.5},{0.7,1.3},{0.8,1.1},{0.8,0.95},{0.7,0.8},{0.75,0.7},{0.75,0.6},{0.6,0.45},{0.4,0.4},{0.3,0.45},{0.2,0.5},{0.15,0.5},{0.1,0.5},{0.,0.5},{-0.1,0.5},{-0.2,0.45},{-0.3,0.4},{-0.3,0.4}},
			treeLight = {{-0.6,1.6},{-0.4,1.7},{-0.3,1.8},{-0.05,1.9},{0.2,1.9},{0.15,1.85},{0.,1.8},{0.05,1.75},{0.1,1.7},{-0.05,1.7},{-0.2,1.6},{-0.35,1.55},{-0.25,1.45},{-0.1,1.4},{0.05,1.45},{0.2,1.3},{0.1,1.1},{0.05,1.2},{-0.05,1.15},{-0.2,1.2},{-0.35,1.15},{-0.4,1.},{-0.55,1.05},{-0.65,0.95},{-0.65,0.75},{-0.75,0.9},{-0.7,1.1},{-0.75,1.25},{-0.65,1.35},{-0.6,1.45}},
			treeDark = {{-0.65,0.75},{-0.45,0.7},{-0.3,0.6},{-0.15,0.65},{0.,0.75},{0.15,0.7},{0.3,0.7},{0.4,0.8},{0.4,0.9},{0.5,1},{0.6,1.},{0.65,1.05},{0.6,1.1},{0.55,1.2},{0.45,1.25},{0.35,1.35},{0.5,1.4},{0.55,1.5},{0.45,1.65},{0.25,1.8},{0.2,1.9},{0.35,1.8},{0.5,1.75},{0.6,1.6},{0.7,1.5},{0.7,1.3},{0.8,1.1},{0.8,0.95},{0.7,0.8},{0.75,0.7},{0.75,0.6},{0.6,0.45},{0.4,0.4},{0.3,0.45},{0.2,0.5},{0.05,0.5},{-0.1,0.5},{-0.2,0.45},{-0.3,0.4},{-0.45,0.45},{-0.55,0.55},{-0.7,0.6},{-0.85,0.75},{-0.8,0.95},{-0.7,1.1},{-0.75,0.9}},
			randomTreeStart = 0, (* zero or larger; controls tree placement and size; set so default looks good *)
			defaultMinBuildingHeight = .15, defaultRandomScale = {.25,.25}, defaultSubdivideSize = .5, defaultYardSize = {.3, .3}, defaultWindowColor = Hue[.125,.5,1],
			defaultBuildingColors = {Hue[.12, .2, .7], Hue[.1, 1, .8], Hue[.06, 1, .8], Hue[1/12, 1, 1], Hue[.12, 1, 1]}
			},
			
			$dwAxoAxisRotation = {0,0,0};
			$dwAxoProjection = "Top";
			{angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True];
			buildingColor = defaultBuildingColors;
			buildingRemoval = defaultBuildingRemoval;
			windowColor = defaultWindowColor;
			yardSize = defaultYardSize;
			scaleEach = defaultScaleEach;
			subdivideSize = defaultSubdivideSize;
			randomScale = defaultRandomScale;
			scaleHeight = defaultScaleHeight;
			minBuildingHeight = defaultMinBuildingHeight;
			seed = defaultSeed;
			exitRoads = dwExitRoads[exitRoadPattern, rows, columns];
			$dwCBheightPattern = dwCityHeightPatternFunction["Default", rows, columns, 2];
			
			Pane[
				Grid[{
					{Column[{
						
						(* preview *)
						Dynamic[Show[
							dwCityBuilderExpand[
								"Preview"->True, "Rows"->rows, "Columns"->columns, "ImageSize"->Max[rows, columns]*100, "FinalScale"->1, 
								"PreviewRows"->rows, "PreviewColumns"->columns, "CurrentBuildingPattern"->currentBuildingPattern, "BuildingColorQuantity"->buildingColorQuantity, 
								"AngleH"->angleH, "AngleV"->angleV, "BackgroundColor"->backgroundColor, "BarkColor"->barkColor, "BedrockColor"->bedrockColor, "BedrockLayer"->bedrockLayer, "BirdColor"->birdColor, 
								"BirdShape"->birdShape, "BirdSize"->birdSize, "BuildingColor"->buildingColor, "BuildingOpacityFill"->buildingOpacityFill, "BuildingOpacityStroke"->buildingOpacityStroke,  "BuildingRemoval"->buildingRemoval, 
								"CityObjects"->cityObjects, "ColorByHeight"->colorByHeight, "ConcreteColor"->concreteColor, "ConcreteSize"->concreteSize, "Contrast"->contrast, "CurbThickness"->curbThickness, 
								"DoorAwningDepth"->doorAwningDepth, "DoorHeight"->doorHeight, "DoorLimit"->doorLimit, "DoorWidth"->doorWidth, "EdgeTrees"->edgeTrees, 
								"ExitRoads"->dwExtremeExitRoads[rows, columns, rows, columns, exitRoads], "FlipHeight"->flipHeight, "GrassColor"->grassColor, 
								"InvertHeightColor"->invertHeightColor, "MaxBuildingHeight"->maxBuildingHeight, "MinBuildingHeight"->minBuildingHeight, 
								"LandEdgeRoughness"->landEdgeRoughness, "LandEdgeLimit"->landEdgeLimit, "OverallTreeSize"->overallTreeSize, 
								"RandomHeight"->randomHeight, "RandomScale"->randomScale, "RandomTreeStart"->randomTreeStart, "RemoveBlockedStreets"->removeBlockedStreets, 
								"ReverseHeight"->reverseHeight, "RiverTurns"->riverTurns, "RockColor"->rockColor, "RockLayer"->rockLayer, 
								"RoofColor"->roofColor, "RoofHeight"->roofHeight, "RoofHipped"->roofHipped, "RoofQuantity"->roofQuantity, 
								"RuralArea"->ruralArea, "RuralTreeBalance"->ruralTreeBalance, "RuralTreeCount"->ruralTreeCount, "ScaleByHeight"->scaleByHeight, 
								"ScaleEach"->scaleEach, "ScaleHeight"->scaleHeight, "Seed"->seed, "ShapePts"->shapePts, 
								"ShowBuildings"->showBuildings, "ShowEnvironment"->showEnvironment, "ShowStreets"->showStreets, "ShowTrees"->showTrees, "ShowWindows"->showWindows, 
								"SidewalkWidth"->sidewalkWidth, "WaterColor"->waterColor, "SoilColor"->soilColor, "SoilLayer"->soilLayer, 
								"StreetCenterlineColor"->streetCenterlineColor, "StreetCenterlineDash"->streetCenterlineDash, "StreetCenterlineOpacity"->streetCenterlineOpacity, 
								"StreetColor"->streetColor, "StreetCurbColor"->streetCurbColor, "StreetPad"->streetPad, "StreetThickness"->streetThickness, 
								"SubdivideDistanceOrigin"->subdivideDistanceOrigin, "SubdivideSize"->subdivideSize, "SubdivideScale"->subdivideScale, "SubsoilColor"->subsoilColor, 
								"SubsoilLayer"->subsoilLayer, "Traffic"->traffic, "Tree"->tree, "TreeDark"->treeDark, "TreeLight"->treeLight, "TreeRandomSize"->treeRandomSize, "TreeTrunk"->treeTrunk, 
								"TreeTrunkDark"->treeTrunkDark, "WindowColor"->windowColor, "WindowSize"->windowSize, "WindowStripes"->windowStripes, "YardSize"->yardSize
							], ImageSize->{320, 245}, PlotRange->All],
							
							(* DO NOT include 'buildings', 'streets', 'land', 'buildingColorQuantity' , $xxxx, or any counts below because of dynamic issues *)
							TrackedSymbols :> {angleH, angleV, backgroundColor, barkColor, bedrockColor, bedrockLayer, birdSize, buildingColor, buildingOpacityFill, buildingOpacityStroke, buildingRemoval, cityObjects, colorByHeight, columns, concreteColor, concreteSize, 
							contrast, currentBuildingPattern, edgeTrees, exitRoads, grassColor, invertHeightColor, landEdgeRoughness, minBuildingHeight, overallTreeSize, randomHeight, randomScale, 
							removeBlockedStreets, reverseHeight, flipHeight, riverTurns, rockColor, rockLayer, roofColor, roofHeight, roofHipped, roofQuantity, rows, ruralArea, scaleEach, scaleHeight, 
							scaleByHeight, seed, showBuildings, showEnvironment, showStreets, showTrees, showWindows, waterColor, soilColor, soilLayer, yardSize, streetCenterlineColor, 
							streetCenterlineDash, streetColor, streetCurbColor, streetPad, streetThickness, subdivideDistanceOrigin, subdivideSize, subdivideScale, 
							subsoilColor, subsoilLayer, traffic, windowColor, windowSize, windowStripes}

							],
							
						(* scene info *)
						Dynamic@Row[{
							If[showWindows, $dwCBwindowCount, 0], " windows  ",
							If[showBuildings && $dwCBbuildings =!= {}, $dwCBbuildingCount(*Length[Flatten[$dwCBbuildings,1]/.{{Null,___}->Sequence[]}]*), 0], " buildings  ", 
							If[showStreets && $dwCBstreets =!= {}, Length[($dwCBstreets/.{Null->Sequence[]})], 0], " streets  ", 
							If[showTrees, $dwCBtreeCount, 0], " trees  ",
							If[showTrees && birdSize != 0, $dwCBoverallBirdCount, 0], " birds"
						}],
						
						Dynamic@Row[{
							Style[Row[{
								"TOTAL OBJECTS: ", Total[{
									If[showBuildings && $dwCBbuildings =!= {}, $dwCBbuildingCount(* 3*Length[Flatten[$dwCBbuildings,1]/.{{Null,___}->Sequence[]}]*), 0], 
									4*Length[($dwCBstreets/.{Null->Sequence[]})], 
									If[showEnvironment, 
										Total[{
											If[MemberQ[concreteSize, 0], 1, 2], 
											If[Total[{soilLayer, subsoilLayer, rockLayer, bedrockLayer}] == 0, 0, 1], (* ground white highlight line *)
											If[soilLayer != 0, Length[$dwCBfinalLandPts], 0], 
											If[subsoilLayer != 0, Length[$dwCBfinalLandPts], 0], 
											If[rockLayer != 0, Length[$dwCBfinalLandPts], 0], 
											If[bedrockLayer != 0, Length[$dwCBfinalLandPts], 0]
										}], 0], 
									If[showWindows, 2*$dwCBwindowCount, 0], 
									4*$dwCBroofCount, 
									3*$dwCBflatRoof1Count,
									4*$dwCBflatRoof2Count,
									7*$dwCBflatRoof3Count, 
									$dwCBextraRoofCount,
									$dwCBtreeObjCount,
									2*$dwCBdoorCount,
									If[showTrees && birdSize != 0, $dwCBoverallBirdCount, 0],
									$dwCBlineCount
								}]
							}]]
						}],
							
						(* visibility *)
						Row[{
							Checkbox[Dynamic@showWindows], "windows   ",
							Checkbox[Dynamic@showBuildings], "buildings   ",
							Checkbox[Dynamic@showStreets], "streets   ",
							Checkbox[Dynamic@showTrees], "trees   ",
							Checkbox[Dynamic@showEnvironment], "land   "
							}],
							
						Row[{
							Style["CITY BLOCKS:", 12],
							Spacer[5], "X ", PopupMenu[Dynamic@rows, Range[maxRowsAndColumns]], 
							Spacer[5], "Y ", PopupMenu[Dynamic@columns, Range[maxRowsAndColumns]],
							
							Spacer[12], 
							(* plotRange = same ratio as image size *)
							Tooltip[Dynamic@ActionMenu["Expand",
								
								Table[
									With[{n = n},
										Row[{n*rows, "x", n*columns, " blocks   ", Max[3, IntegerPart[((n^2)*rows*columns)/timingDivider]], " seconds"}]:>(
											MessageDialog[Row[{"Building ", n*rows, "x", n*columns, " city in a new notebook which will take around ", Max[3, IntegerPart[((n^2)*rows*columns)/timingDivider]], " seconds..."}]];
											dwCityBuilderExpand[
												"Preview"->False, "Rows"->n*rows, "Columns"->n*columns, "ImageSize"->Max[rows, columns]*(n*100), "FinalScale"->1, 
												"PreviewRows"->rows, "PreviewColumns"->columns, "CurrentBuildingPattern"->currentBuildingPattern, "BuildingColorQuantity"->buildingColorQuantity, 
												"AngleH"->angleH, "AngleV"->angleV, "BackgroundColor"->backgroundColor, "BarkColor"->barkColor, "BedrockColor"->bedrockColor, "BedrockLayer"->bedrockLayer, "BirdColor"->birdColor, 
												"BirdShape"->birdShape, "BirdSize"->birdSize, "BuildingColor"->buildingColor, "BuildingOpacityFill"->buildingOpacityFill, "BuildingOpacityStroke"->buildingOpacityStroke,  "BuildingRemoval"->buildingRemoval, "CityObjects"->cityObjects, 
												"ColorByHeight"->colorByHeight, "ConcreteColor"->concreteColor, "ConcreteSize"->n*concreteSize, "Contrast"->contrast, "CurbThickness"->curbThickness, 
												"DoorAwningDepth"->doorAwningDepth, "DoorHeight"->doorHeight, "DoorLimit"->doorLimit, "DoorWidth"->doorWidth, "EdgeTrees"->edgeTrees, 
												"ExitRoads"->dwExtremeExitRoads[rows, columns, n*rows, n*columns, exitRoads], "FlipHeight"->flipHeight, "GrassColor"->grassColor, 
												"InvertHeightColor"->invertHeightColor, "MaxBuildingHeight"->maxBuildingHeight, "MinBuildingHeight"->(1/n)*minBuildingHeight, 
												"LandEdgeRoughness"->landEdgeRoughness, "LandEdgeLimit"->landEdgeLimit, "OverallTreeSize"->overallTreeSize, 
												"RandomHeight"->randomHeight, "RandomScale"->randomScale, "RandomTreeStart"->randomTreeStart, "RemoveBlockedStreets"->removeBlockedStreets, 
												"ReverseHeight"->reverseHeight, "RiverTurns"->riverTurns, "RockColor"->rockColor, "RockLayer"->rockLayer, 
												"RoofColor"->roofColor, "RoofHeight"->roofHeight, "RoofHipped"->roofHipped, "RoofQuantity"->roofQuantity, 
												"RuralArea"->ruralArea, "RuralTreeBalance"->ruralTreeBalance, "RuralTreeCount"->ruralTreeCount, "ScaleByHeight"->scaleByHeight, 
												"ScaleEach"->scaleEach, "ScaleHeight"->scaleHeight, "Seed"->seed, "ShapePts"->shapePts, 
												"ShowBuildings"->showBuildings, "ShowEnvironment"->showEnvironment, "ShowStreets"->showStreets, "ShowTrees"->showTrees, "ShowWindows"->showWindows, 
												"SidewalkWidth"->sidewalkWidth, "WaterColor"->waterColor, "SoilColor"->soilColor, "SoilLayer"->soilLayer, 
												"StreetCenterlineColor"->streetCenterlineColor, "StreetCenterlineDash"->streetCenterlineDash, "StreetCenterlineOpacity"->streetCenterlineOpacity, 
												"StreetColor"->streetColor, "StreetCurbColor"->streetCurbColor, "StreetPad"->streetPad, "StreetThickness"->streetThickness, 
												"SubdivideDistanceOrigin"->subdivideDistanceOrigin, "SubdivideSize"->subdivideSize, "SubdivideScale"->subdivideScale, "SubsoilColor"->subsoilColor, 
												"SubsoilLayer"->subsoilLayer, "Traffic"->traffic, "Tree"->tree, "TreeDark"->treeDark, "TreeLight"->treeLight, "TreeRandomSize"->treeRandomSize, "TreeTrunk"->treeTrunk, 
												"TreeTrunkDark"->treeTrunkDark, "WindowColor"->windowColor, "WindowSize"->windowSize, "WindowStripes"->windowStripes, "YardSize"->yardSize
											]
										)
									], {n,6}], 
								Appearance->"PopupMenu",  Method->"Queued", ImageSize->100], 
								
								"Create an expanded city in a new notebook.", TooltipDelay->$dwTooltipDelay
							]
						}],
						
						Row[{		
						(* Note: grassColor and concreteColor must be different than all other colors to restrict hidden items to grass and concrete only *)
							ActionMenu["CITY PRESETS",
								{
								
									"Default city" :> (
										currentBuildingPattern = "Default"; birdSize = 1; buildingRemoval = defaultBuildingRemoval; cityObjects = .5; concreteSize = {0, 0}; edgeTrees = 1; flipHeight = 0; groundStep = .025; 
										$dwCBheightPattern = dwCityHeightPatternFunction["Default", rows, columns, 2]; buildingOpacityFill = 1; buildingOpacityStroke = 1; 
										landEdgeRoughness = 1; minBuildingHeight = defaultMinBuildingHeight; overallTreeSize = 1; randomHeight = .75; randomScale = defaultRandomScale; riverTurns = 4;
										roofHeight = .5; roofHipped = .5; roofQuantity = .65; ruralArea = .05; scaleEach = defaultScaleEach; scaleByHeight = .5; scaleHeight = defaultScaleHeight; seed = defaultSeed; step = .05; 
										streetPad = 0; streetThickness = .025; subdivideDistanceOrigin = {}; subdivideScale = .85; subdivideSize = defaultSubdivideSize; windowSize = .5; yardSize = defaultYardSize; 
										removeBlockedStreets = True; reverseHeight = False; showBuildings = True; showEnvironment = True; showStreets = True; showTrees = True; showWindows = True; windowStripes = True; 
										colorByHeight = True; invertHeightColor = False;
										soilLayer = .025; subsoilLayer = 0; rockLayer = 0; bedrockLayer = 0;
										exitRoadPattern = "None"; traffic = 1;
										(* colors *)
										contrast = .6; 
										buildingColorQuantity = 5; buildingColor = defaultBuildingColors;
										roofColor = GrayLevel[.5]; windowColor = defaultWindowColor; backgroundColor = White;
										barkColor = Hue[.1, 1, .7]; concreteColor = GrayLevel[.8]; grassColor = Hue[.25, .7, .7]; waterColor = Hue[.6, .5, 1]; 
										streetCenterlineColor = LightGray; streetColor = GrayLevel[.4]; streetCurbColor = Black; 
										soilColor = Hue[.1, 1, .7]; subsoilColor = Hue[.1, .6, .85]; rockColor = Hue[.1, .3, .75]; bedrockColor = Hue[.1, .2, .65];
									),
										
									"Pattern preview" :> (
										buildingRemoval = 0; columns = 3; flipHeight = 0; randomHeight = 1; randomScale = {0,0}; riverTurns = 0; rows = 3; scaleByHeight = 0; subdivideSize = 0; yardSize = defaultYardSize;
										showBuildings = True; showStreets = False; showTrees = False; 
										(* colors *)
										contrast = .6; buildingOpacityFill = 1; buildingOpacityStroke = 1;
										buildingColorQuantity = 1; buildingColor = defaultBuildingColors; 
										grassColor = Hue[.25, .7, .7]; windowColor = defaultWindowColor
									),
									
									Delimiter,
									"COLOR" :> (Null),
								
									"Day" :> (
										contrast = .6; buildingOpacityFill = 1; buildingOpacityStroke = 1;
										buildingColorQuantity = 5; buildingColor = defaultBuildingColors;
										roofColor = GrayLevel[.5]; windowColor = defaultWindowColor;  backgroundColor = White;
										barkColor = Hue[.1, 1, .7]; concreteColor = GrayLevel[.8]; grassColor = Hue[.25, .7, .7]; waterColor = Hue[.6, .5, 1]; 
										streetCenterlineColor = LightGray; streetColor = GrayLevel[.4]; streetCurbColor = Black; 
										soilColor = Hue[.1, 1, .7]; subsoilColor = Hue[.1, .6, .85]; rockColor = Hue[.1, .3, .75]; bedrockColor = Hue[.1, .2, .65];
									),
								
									"Night" :> (
										contrast = .6; buildingOpacityFill = 1; buildingOpacityStroke = 1;
										buildingColorQuantity = 5;
										buildingColor = {GrayLevel[.25], GrayLevel[.3], GrayLevel[.2], GrayLevel[.35], GrayLevel[.4]}; 
										roofColor = GrayLevel[.4]; windowColor = Hue[.125,1,1]; backgroundColor = GrayLevel[.25];
										barkColor = GrayLevel[.25]; concreteColor = GrayLevel[.61]; grassColor = GrayLevel[.41]; waterColor = GrayLevel[.6]; 
										streetCenterlineColor = LightGray; streetColor = GrayLevel[.4]; streetCurbColor = Black; 
										soilColor = GrayLevel[.25]; subsoilColor = GrayLevel[.4]; rockColor = GrayLevel[.3]; bedrockColor = GrayLevel[.5]; 
									),
								
									"Night glow" :> (
										contrast = .6; buildingOpacityFill = 1; buildingOpacityStroke = 1;
										buildingColorQuantity = 5;
										buildingColor = defaultBuildingColors; 
										roofColor = GrayLevel[.4]; windowColor = Hue[.125,1,1]; backgroundColor = GrayLevel[.25];
										barkColor = GrayLevel[.25]; concreteColor = GrayLevel[.61]; grassColor = GrayLevel[.41]; waterColor = GrayLevel[.6]; 
										streetCenterlineColor = LightGray; streetColor = GrayLevel[.4]; streetCurbColor = Black; 
										soilColor = GrayLevel[.25]; subsoilColor = GrayLevel[.4]; rockColor = GrayLevel[.3]; bedrockColor = GrayLevel[.5]; 
									),
								
									"Gray" :> (
										contrast = .6; buildingOpacityFill = 1; buildingOpacityStroke = 1;
										buildingColorQuantity = 5;
										buildingColor = Table[GrayLevel[1], 5]; 
										roofColor = GrayLevel[.4]; windowColor = GrayLevel[.3]; backgroundColor = White;
										barkColor = GrayLevel[.4]; concreteColor = GrayLevel[.91]; grassColor = GrayLevel[.56]; waterColor = GrayLevel[.85]; 
										streetCenterlineColor = White; streetColor = GrayLevel[.4]; streetCurbColor = GrayLevel[.2]; 
										soilColor = GrayLevel[.4]; subsoilColor = GrayLevel[.7]; rockColor = GrayLevel[.6]; bedrockColor = GrayLevel[.8]; 
									),
								
									"One scene color" :> (
										buildingColorQuantity = "5SC"; buildingOpacityFill = 1; buildingOpacityStroke = 1;  backgroundColor = White;
										buildingColor = {Orange, White, Black, Orange, Black}; 
									),
								
									"Black and white" :> (
										buildingColorQuantity = "5SC"; buildingOpacityFill = 1; buildingOpacityStroke = 1;  backgroundColor = White;
										buildingColor = {White, Black, Black, Gray, Black}; 
									),
								
									"Xray" :> (
										buildingColorQuantity = "5SC"; buildingOpacityFill = .5; buildingOpacityStroke = 1;  backgroundColor = White;
										buildingColor = {White, Gray, Black, Gray, Black}; 
									),
								
									"Five scene colors" :> (
										buildingColorQuantity = "5SC"; buildingOpacityFill = 1; buildingOpacityStroke = 1;  backgroundColor = White;
										buildingColor = ColorData[106, "ColorList"][[{2,3,1,4,5}]]; 
									),
									
									Delimiter,
									"LAYOUT" :> (Null),
								
									"Compact" :> (
										buildingRemoval = 0; edgeTrees = 1.5;  
										landEdgeRoughness = 1; minBuildingHeight = defaultMinBuildingHeight; randomHeight = .75; randomScale = defaultRandomScale; riverTurns = 0;
										ruralArea = .05; scaleEach = defaultScaleEach; scaleByHeight = .5; scaleHeight = defaultScaleHeight;
										subdivideSize = defaultSubdivideSize; yardSize = {.2, .2}; showBuildings = True; showStreets = False; overallTreeSize = 1.25
									),
								
									"Balanced" :> (
										buildingRemoval = defaultBuildingRemoval; edgeTrees = 1;  
										landEdgeRoughness = 1; minBuildingHeight = defaultMinBuildingHeight; randomHeight = .75; randomScale = defaultRandomScale; riverTurns = 4;
										ruralArea = .05; scaleEach = defaultScaleEach; scaleByHeight = .5; scaleHeight = defaultScaleHeight;
										subdivideSize = defaultSubdivideSize; yardSize = defaultYardSize; showBuildings = True; showStreets = True; overallTreeSize = 1
									),
								
									"Sparse" :> (
										buildingRemoval = 0; edgeTrees = .5;  
										landEdgeRoughness = 1; minBuildingHeight = defaultMinBuildingHeight; randomHeight = .75; randomScale = defaultRandomScale; riverTurns = 4;
										ruralArea = .05; scaleEach = defaultScaleEach; scaleByHeight = .5; scaleHeight = defaultScaleHeight;
										subdivideSize = 0; yardSize = {.4, .4}; showBuildings = True; showStreets = True; overallTreeSize = .75
									)
									
								}, Appearance->"PopupMenu", BaselinePosition->Scaled[.25], ImageSize->220],
							
							ActionMenu["VIEW",
							Flatten[{
								"Isometric" :> ($dwTilt = 35.2672; $dwTurn = 45; {angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True]),
								"Isometric left" :> ($dwTilt = 30; $dwTurn = 60; {angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True]),
								"Isometric right" :> ($dwTilt = 30; $dwTurn = 30; {angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True]),
								Delimiter,
								"Birds eye" :> ($dwTilt = 60; $dwTurn = 45; {angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True]),
								"Birds eye left" :> ($dwTilt = 60; $dwTurn = 60; {angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True]),
								"Birds eye right" :> ($dwTilt = 60; $dwTurn = 30; {angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True]),
								Delimiter,
								"Ground" :> ($dwTilt = 15; $dwTurn = 45; {angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True]),
								"Ground left" :> ($dwTilt = 15; $dwTurn = 60; {angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True]),
								"Ground right" :> ($dwTilt = 15; $dwTurn = 30; {angleH, angleV} = dwUpdateAxonometricAngles[$dwTilt, $dwTurn, "ReturnValuesWithoutUpdating"->True])
								
							}], Appearance->"PopupMenu", BaselinePosition->Scaled[.25], ImageSize->100]
						}],
						
						(* city blocks *)
						Panel[Column[{
							Row[{Pane["layout ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@seed,{1,40,1}, ContinuousAction -> False, ImageSize->180],
								Button["<",seed=Max[seed-=1,1],Appearance->"Palette"],
								Button[">",seed=Min[seed+=1,40],Appearance->"Palette"],
								" ",Pane[Dynamic@seed, ImageSize->30]}],
							Row[{Pane["river ", ImageSize->60, Alignment->Right],
								Dynamic@Slider[Dynamic@riverTurns,{0, 9, 1}, ContinuousAction -> False, ImageSize->180],
								Button["<",riverTurns=Max[riverTurns-=1,0],Appearance->"Palette"],
								Button[">",riverTurns=Min[riverTurns+=1,9],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[riverTurns,1], ImageSize->30]}],
							Row[{Pane["city parks ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@buildingRemoval,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",buildingRemoval=Max[buildingRemoval-=step,0],Appearance->"Palette"],
								Button[">",buildingRemoval=Min[buildingRemoval+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[buildingRemoval,step], ImageSize->30]}],
							Row[{Pane["park swap ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@cityObjects,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",cityObjects=Max[cityObjects-=step,0],Appearance->"Palette"],
								Button[">",cityObjects=Min[cityObjects+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[cityObjects,step], ImageSize->30]}],
							Row[{Pane["rural trees ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@edgeTrees,{0,3,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",edgeTrees=Max[edgeTrees-=step,0],Appearance->"Palette"],
								Button[">",edgeTrees=Min[edgeTrees+=step,3],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[edgeTrees,step], ImageSize->30]}],
							Row[{Pane["windows ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@windowSize,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",windowSize=Max[windowSize-=step,0],Appearance->"Palette"],
								Button[">",windowSize=Min[windowSize+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[windowSize,step], ImageSize->30]}],
							Row[{Pane["traffic ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@traffic,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",traffic=Max[traffic-=step,0],Appearance->"Palette"],
								Button[">",traffic=Min[traffic+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[traffic,step], ImageSize->30]}],
							Row[{Pane["contrast ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@contrast,{.5,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",contrast=Max[contrast-=step,.5],Appearance->"Palette"],
								Button[">",contrast=Min[contrast+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@contrast, ImageSize->30]}],
							Row[{Pane["fill opacity ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@buildingOpacityFill,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",buildingOpacityFill=Max[buildingOpacityFill-=step,0],Appearance->"Palette"],
								Button[">",buildingOpacityFill=Min[buildingOpacityFill+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@buildingOpacityFill, ImageSize->30]}],
							Row[{Pane["line opacity ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@buildingOpacityStroke,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",buildingOpacityStroke=Max[buildingOpacityStroke-=step,0],Appearance->"Palette"],
								Button[">",buildingOpacityStroke=Min[buildingOpacityStroke+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@buildingOpacityStroke, ImageSize->30]}],
							Style["DIMENSIONS", 12],
							Row[{Pane["subdivide ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@subdivideSize,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",subdivideSize=Max[subdivideSize-=step,0],Appearance->"Palette"],
								Button[">",subdivideSize=Min[subdivideSize+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[subdivideSize,step], ImageSize->30]}],
							Row[{Pane["tree size ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@overallTreeSize,{.25,2,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",overallTreeSize=Max[overallTreeSize-=step,.25],Appearance->"Palette"],
								Button[">",overallTreeSize=Min[overallTreeSize+=step,2],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[overallTreeSize,step], ImageSize->30]}],
							Row[{Pane["yard size ", ImageSize->60, Alignment->Right],
								EventHandler[
									Row[{
										Slider[Dynamic@yardSize[[2]],{.2,.5,.5step}, ContinuousAction -> False, ImageSize->180],
										Button["<",yardSize[[2]]=Max[yardSize[[2]]-=.5step,.2],Appearance->"Palette"],
										Button[">",yardSize[[2]]=Min[yardSize[[2]]+=.5step,.5],Appearance->"Palette"]
									}],
									{
										"MouseClicked":>(yardSize[[1]] = yardSize[[2]]),
										"MouseUp":>(yardSize[[1]] = yardSize[[2]])
								}, PassEventsDown->True],
								" ",Pane[Dynamic@yardSize[[2]], ImageSize->30]}],
							Row[{Pane["bird size ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@birdSize,{0,2,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",birdSize=Max[birdSize-=step,0],Appearance->"Palette"],
								Button[">",birdSize=Min[birdSize+=step,2],Appearance->"Palette"],
								" ",Pane[Dynamic@birdSize, ImageSize->30]}]
							
						}, Alignment->Center], Alignment->Center, ImageSize->320]
						
					}, Alignment->Center],
					
					Grid[{{
						Grid[{{
							Panel[Column[{
								Style["LAYOUT FOOTPRINT", 15, Bold],
								Grid[{{
									Column[{
										EventHandler[
											Dynamic[
												Graphics[{
													
													{Gray, Line[{{0,0}, {1.1, -1.1} + {-.5, .5}ruralArea}], Line[{{0,0}, {-1.1, -1.1} + {.5, .5}ruralArea}]},
													
													Rotate[{
														If[buildingColorQuantity === "5SC", buildingColor[[3]], grassColor], Polygon[$dwCBfinalLandPts],
														If[buildingColorQuantity =!= "5SC", {concreteColor, Polygon[$dwCBconcretePts]}, {}]
														}, Pi/4, {0, 0}],
														
													Rotate[{
														If[showStreets, {LightGray, Table[If[$dwCBstreets[[n]] =!= Null, Annotation[Polygon[$dwCBstreets[[n]]], n, "Mouse"], Nothing], {n, Length[$dwCBstreets]}]}, {}],
														If[showBuildings && $dwCBbuildings =!= {},
															{
																StrokeForm[{Black}], Gray, 
																Table[
																	Table[
																		If[$dwCBbuildings[[n, bn, 1]] =!= Null && FreeQ[$dwCBriverFlow, $dwCBriverToBuildingArray[[n]]],
																			
																			extrude = (SeedRandom[n+seed];RandomReal[{randomHeight,1}]*$dwCBscaleAll*scaleHeight*$dwCBheightSize[[Mod[n, Length[$dwCBheightSize], 1]]]);
																			finalHeight = Min[Max[(SeedRandom[n+bn+seed];RandomReal[{randomHeight, 1}]*extrude), Min[minBuildingHeight, maxBuildingHeight]], maxBuildingHeight];
																			finalHeightScaled = Rescale[{finalHeight}, $dwCBminMaxHeight, {.01, 1}][[1]];
																			scaleByHeightFunction = ScalingTransform[({1, 1}-.5(scaleByHeight*finalHeightScaled)), dwFindCenter[$dwCBbuildings[[n, bn]]]];
																			scaleByHeightPts = scaleByHeightFunction[$dwCBbuildings[[n, bn]]];
																			Polygon[scaleByHeightPts],
																			
																			{}
																		],
																	{bn, Length[$dwCBbuildings[[n]]]}],
																{n, Length[$dwCBbuildings]}]
															}, 
															{}
														]
													}, Pi/4, {0, 0}],
													
													GrayLevel[.5],
													Text[Style["x", 15, Bold], {1.1, -1.1} + {-.5, .5}ruralArea, {-1, 1}],
													Text[Style["y", 15, Bold], {-1.1, -1.1} + {.5, .5}ruralArea, {1, 1}]
												}, 
												Background->None, ImageSize->{180, 180}, PlotRange->1.5-ruralArea, PlotRangePadding->0], 
												
												TrackedSymbols :> {$dwCBbuildings, $dwCBconcretePts, $dwCBfinalLandPts, $dwCBstreets, $dwCBriverFlow, 
													buildingColor, buildingColorQuantity, concreteSize, concreteColor, flipHeight, 
													grassColor, riverTurns, scaleByHeight, showBuildings, showEnvironment, showStreets}],
											{
												"MouseClicked":>(
													If[MouseAnnotation[] =!= Null,
														If[MemberQ[exitRoads, MouseAnnotation[]], 
															exitRoads = Complement[exitRoads, {MouseAnnotation[]}],
															exitRoads = Union[exitRoads, {MouseAnnotation[]}]
														]
													];
												)
											}, PassEventsDown->True
										],
										
										"BUILDING HEIGHT MAP PREVIEW",
										Dynamic[Grid[{
											{"3 x 3", "9 x 9", "18 x 18"},
											{
												Sequence@@If[$dwCBheightPattern === {},
													Switch[currentBuildingPattern,
														 "Corner left",
														 	(temp = Table[(r/# + c/#)/2, {r, 0, #-1}, {c, 0, #-1}];
														 	Rotate[ArrayPlot[Rescale[temp, MinMax[temp], {1, 0}], ImageSize -> 48, PlotRangePadding -> None], Pi])&/@{3, 9, 18},
														 "Corner right",
														 	(temp = Table[(r/# + c/#)/2, {r, 0, #-1}, {c, 0, #-1}];
														 	ArrayPlot[Rescale[temp, MinMax[temp], {1, 0}], ImageSize -> 48, PlotRangePadding -> None])&/@{3, 9, 18},
														 "Corner back",
														 	(temp = Table[(r/# + c/#)/2, {r, 0, #-1}, {c, 0, #-1}];
														 	Rotate[ArrayPlot[Rescale[temp, MinMax[temp], {1, 0}], ImageSize -> 48, PlotRangePadding -> None], Pi/2])&/@{3, 9, 18},
														 "Corner perimeter",
														 	(temp = Table[2 - (Sin[r Pi/((# - 1) + $MachineEpsilon)] + Sin[c Pi/((# - 1) + $MachineEpsilon)]), {r, 0, # - 1}, {c, 0, # - 1}];
														 	ArrayPlot[Rescale[temp, MinMax[temp], {1, 0}], ImageSize -> 48, PlotRangePadding -> None])&/@{3, 9, 18},
														_,
															{Graphics[Text[Style["Preview not available", Gray]], ImageSize->{Automatic, 48}]}
													],
													
													dwCityHeightPatternFunction[currentBuildingPattern, #, #, seed, "Preview"->True]&/@{3, 9, 18}
												]
											}}, Spacings->{1.25,0}],
										TrackedSymbols :> {buildingRemoval, concreteSize, concreteColor, currentBuildingPattern, grassColor, scaleByHeight, streetColor}]
									}, Alignment->Center(*, Spacings->0*)],
									
									Graphics[{Line[{{0,0},{0,300}}]}, ImageSize->{15,250}],
										
									Pane[
										Dynamic[Grid[Partition[{
											Style["BUILDING\nLAYOUT PRESETS", LineSpacing->{0, 12}, TextAlignment->Center], SpanFromLeft,
											
											Button[dwCityPreview01[LightGray, If[buildingColorQuantity === "5SC", buildingColor[[3]], grassColor], concreteColor, finalScale, .05, $dwCBheightPattern, subdivideDistanceOrigin, reverseHeight, dwExitRoads["None", 1, 1], 0, seed, flipHeight], 
												rows = 1; columns = 1; scaleEach = defaultScaleEach; yardSize = defaultYardSize; randomScale = defaultRandomScale; concreteSize = {0, 0}; scaleByHeight = 0; subdivideScale = .85; streetThickness = .025; streetPad = 0; buildingRemoval = 0; exitRoads = dwExitRoads[exitRoadPattern, 1, 1];
												subdivideSize = 0; removeBlockedStreets = True, Appearance->None, ImageSize -> {previewButtonSize, previewButtonSize}],
											Button[dwCityPreview02[LightGray, If[buildingColorQuantity === "5SC", buildingColor[[3]], grassColor], concreteColor, finalScale, .05, $dwCBheightPattern, subdivideDistanceOrigin, reverseHeight, dwExitRoads["None", 1, 1], defaultSubdivideSize, seed, flipHeight], 
												rows = 1; columns = 1; scaleEach = defaultScaleEach; yardSize = defaultYardSize; randomScale = defaultRandomScale; concreteSize = {0, 0}; scaleByHeight = .5; subdivideScale = .85; streetThickness = .025; streetPad = 0; buildingRemoval = 0; exitRoads = dwExitRoads[exitRoadPattern, 1, 1];
												subdivideSize = defaultSubdivideSize; removeBlockedStreets = True, Appearance->None, ImageSize -> {previewButtonSize, previewButtonSize}],
											Button[dwCityPreview03[LightGray, If[buildingColorQuantity === "5SC", buildingColor[[3]], grassColor], concreteColor, finalScale, .05, dwCityHeightPatternFunction[currentBuildingPattern, 2, 2, seed], subdivideDistanceOrigin, reverseHeight, dwExitRoads["None", 2, 2], 0, seed, flipHeight], 
												rows = 2; columns = 2; scaleEach = defaultScaleEach; yardSize = defaultYardSize; randomScale = defaultRandomScale; concreteSize = {0, 0}; scaleByHeight = 0; subdivideScale = .85; streetThickness = .025; streetPad = 0; exitRoads = dwExitRoads[exitRoadPattern, 2, 2];
												subdivideSize = 0; removeBlockedStreets = True, Appearance->None, ImageSize -> {previewButtonSize, previewButtonSize}],
											Button[dwCityPreview04[LightGray, If[buildingColorQuantity === "5SC", buildingColor[[3]], grassColor], concreteColor, finalScale, .05, dwCityHeightPatternFunction[currentBuildingPattern, 2, 2, seed], subdivideDistanceOrigin, reverseHeight, dwExitRoads["None", 2, 2], defaultSubdivideSize, seed, flipHeight], 
												rows = 2; columns = 2; scaleEach = defaultScaleEach; yardSize = defaultYardSize; randomScale = defaultRandomScale; concreteSize = {0, 0}; scaleByHeight = .5; subdivideScale = .85; streetThickness = .025; streetPad = 0; exitRoads = dwExitRoads[exitRoadPattern, 2, 2];
												subdivideSize = defaultSubdivideSize; removeBlockedStreets = True, Appearance->None, ImageSize -> {previewButtonSize, previewButtonSize}],
											Button[dwCityPreview05[LightGray, If[buildingColorQuantity === "5SC", buildingColor[[3]], grassColor], concreteColor, finalScale, .05, dwCityHeightPatternFunction[currentBuildingPattern, 3, 3, seed], subdivideDistanceOrigin, reverseHeight, dwExitRoads["None", 3, 3], 0, seed, flipHeight], 
												rows = 3; columns = 3; scaleEach = defaultScaleEach; yardSize = defaultYardSize; randomScale = defaultRandomScale; concreteSize = {0, 0}; scaleByHeight = 0; subdivideScale = .85; streetThickness = .025; streetPad = 0; exitRoads = dwExitRoads[exitRoadPattern, 3, 3];
												subdivideSize = 0; removeBlockedStreets = True, Appearance->None, ImageSize -> {previewButtonSize, previewButtonSize}],
											Button[dwCityPreview06[LightGray, If[buildingColorQuantity === "5SC", buildingColor[[3]], grassColor], concreteColor, finalScale, .05, dwCityHeightPatternFunction[currentBuildingPattern, 3, 3, seed], subdivideDistanceOrigin, reverseHeight, dwExitRoads["None", 3, 3], defaultSubdivideSize, seed, flipHeight], 
												rows = 3; columns = 3; scaleEach = defaultScaleEach; yardSize = defaultYardSize; randomScale = defaultRandomScale; concreteSize = {0, 0}; scaleByHeight = .5; subdivideScale = .85; streetThickness = .025; streetPad = 0; exitRoads = dwExitRoads[exitRoadPattern, 3, 3];
												subdivideSize = defaultSubdivideSize; removeBlockedStreets = True, Appearance->None, ImageSize -> {previewButtonSize, previewButtonSize}],
											Button[dwCityPreview07[LightGray, If[buildingColorQuantity === "5SC", buildingColor[[3]], grassColor], concreteColor, finalScale, .05, dwCityHeightPatternFunction[currentBuildingPattern, 4, 4, seed], subdivideDistanceOrigin, reverseHeight, dwExitRoads["None", 4, 4], 0, seed, flipHeight], 
												rows = 4; columns = 4; scaleEach = defaultScaleEach; yardSize = defaultYardSize; randomScale = defaultRandomScale; concreteSize = {0, 0}; scaleByHeight = 0; subdivideScale = .85; streetThickness = .025; streetPad = 0; exitRoads = dwExitRoads[exitRoadPattern, 4, 4];
												subdivideSize = 0; removeBlockedStreets = True, Appearance->None, ImageSize -> {previewButtonSize, previewButtonSize}],
											Button[dwCityPreview08[LightGray, If[buildingColorQuantity === "5SC", buildingColor[[3]], grassColor], concreteColor, finalScale, .05, dwCityHeightPatternFunction[currentBuildingPattern, 4, 4, seed], subdivideDistanceOrigin, reverseHeight, dwExitRoads["None", 4, 4], defaultSubdivideSize, seed, flipHeight], 
												rows = 4; columns = 4; scaleEach = defaultScaleEach; yardSize = defaultYardSize; randomScale = defaultRandomScale; concreteSize = {0, 0}; scaleByHeight = .5; subdivideScale = .85; streetThickness = .025; streetPad = 0; exitRoads = dwExitRoads[exitRoadPattern, 4, 4];
												subdivideSize = defaultSubdivideSize; removeBlockedStreets = True, Appearance->None, ImageSize -> {previewButtonSize, previewButtonSize}]
												
											},2,2,1,{}], Spacings->.9{1, 1}],
										TrackedSymbols :> {buildingRemoval, concreteSize, concreteColor, $dwCBheightPattern, grassColor, scaleByHeight, streetColor, flipHeight}], 
									ImageSize->130]
									
								}}, Alignment->Bottom, Spacings->0],
								
								Pane[
									Row[{
										ActionMenu["Choose exit streets", 
											With[{er = #},
												er :> (exitRoadPattern = er; exitRoads = dwExitRoads[er, rows, columns])
											]&/@{"None", "All", "Middle", "Corners", "Random"},
										Appearance->"PopupMenu", ImageSize->170],
										
										Spacer[5],
										
										Style[" or click end of street.", 10]
											
									}],
								Alignment->Left, ImageSize->320],
								
								Pane[
									Column[{
										
										Row[{Spacer[{0,20}], Style["BUILDING FOOTPRINT SIZE", 12]}],
										
										Row[{Pane["X ", ImageSize->60, Alignment->Right],
											Slider[Dynamic@scaleEach[[2]],{.2,1,step}, ContinuousAction -> False, ImageSize->180],
											Button["<",scaleEach[[2]]=Max[scaleEach[[2]]-=step,.2],Appearance->"Palette"],
											Button[">",scaleEach[[2]]=Min[scaleEach[[2]]+=step,1],Appearance->"Palette"],
											" ",Pane[Dynamic@scaleEach[[2]], ImageSize->30]}],
										Row[{Pane["Y ", ImageSize->60, Alignment->Right],
											Slider[Dynamic@scaleEach[[1]],{.2,1,step}, ContinuousAction -> False, ImageSize->180],
											Button["<",scaleEach[[1]]=Max[scaleEach[[1]]-=step,.2],Appearance->"Palette"],
											Button[">",scaleEach[[1]]=Min[scaleEach[[1]]+=step,1],Appearance->"Palette"],
											" ",Pane[Dynamic@scaleEach[[1]], ImageSize->30]}],
										Row[{Pane["trim width ", ImageSize->60, Alignment->Right],
											Slider[Dynamic@scaleByHeight,{0,1,step}, ContinuousAction -> False, ImageSize->180],
											Button["<",scaleByHeight=Max[scaleByHeight-=step,0],Appearance->"Palette"],
											Button[">",scaleByHeight=Min[scaleByHeight+=step,1],Appearance->"Palette"],
											" ",Pane[Dynamic@Round[scaleByHeight,step], ImageSize->30]}],
										Row[{Pane["split size ", ImageSize->60, Alignment->Right],
											Slider[Dynamic@subdivideScale,{.2,1,step}, ContinuousAction -> False, ImageSize->180],
											Button["<",subdivideScale=Max[subdivideScale-=step,.2],Appearance->"Palette"],
											Button[">",subdivideScale=Min[subdivideScale+=step,1],Appearance->"Palette"],
											" ",Pane[Dynamic@subdivideScale, ImageSize->30]}],
											
										Style["RANDOMIZE FOOTPRINT", 12],
										
										Row[{Pane["X ", ImageSize->60, Alignment->Right],
											Slider[Dynamic@randomScale[[2]],{0,1,step}, ContinuousAction -> False, ImageSize->180],
											Button["<",randomScale[[2]]=Max[randomScale[[2]]-=step,0],Appearance->"Palette"],
											Button[">",randomScale[[2]]=Min[randomScale[[2]]+=step,1],Appearance->"Palette"],
											" ",Pane[Dynamic@Round[randomScale[[2]],step], ImageSize->30]}],
										Row[{Pane["Y ", ImageSize->60, Alignment->Right],
											Slider[Dynamic@randomScale[[1]],{0,1,step}, ContinuousAction -> False, ImageSize->180],
											Button["<",randomScale[[1]]=Max[randomScale[[1]]-=step,0],Appearance->"Palette"],
											Button[">",randomScale[[1]]=Min[randomScale[[1]]+=step,1],Appearance->"Palette"],
											" ",Pane[Dynamic@Round[randomScale[[1]],step], ImageSize->30]}],
										Row[{Pane["height ", ImageSize->60, Alignment->Right],
											Slider[Dynamic@randomHeight,{0,1,step}, ContinuousAction -> False, ImageSize->180],
											Button["<",randomHeight=Max[randomHeight-=step,0],Appearance->"Palette"],
											Button[">",randomHeight=Min[randomHeight+=step,1],Appearance->"Palette"],
											" ",Pane[Dynamic@Round[randomHeight,step], ImageSize->30]}]
									
									}, Alignment->Center, Spacings->.3], 
									
								ImageSize->320]
								
							}, Alignment->Center], ImageSize->320]
						},{
							Panel[Column[{
								
								Style["LAND SURFACE", 15, Bold],
									
								Row[{Spacer[{0,15}], Style["CONCRETE CENTER", 12]}],
								
								Row[{Pane["concrete X ", ImageSize->60, Alignment->Right],
									Dynamic@Slider[Dynamic@concreteSize[[2]],{0,rows,1}, ContinuousAction -> False, ImageSize->180],
									Button["<",concreteSize[[2]]=Max[concreteSize[[2]]-=1,0],Appearance->"Palette"],
									Button[">",concreteSize[[2]]=Min[concreteSize[[2]]+=1,rows],Appearance->"Palette"],
									" ",Pane[Dynamic@concreteSize[[2]], ImageSize->30]}],
								Row[{Pane["concrete Y ", ImageSize->60, Alignment->Right],
									Dynamic@Slider[Dynamic@concreteSize[[1]],{0,columns,1}, ContinuousAction -> False, ImageSize->180],
									Button["<",concreteSize[[1]]=Max[concreteSize[[1]]-=1,0],Appearance->"Palette"],
									Button[">",concreteSize[[1]]=Min[concreteSize[[1]]+=1,columns],Appearance->"Palette"],
									" ",Pane[Dynamic@concreteSize[[1]], ImageSize->30]}],
									
								Style["LAND BORDER", 12],
								
								Row[{Pane["rough ", ImageSize->60, Alignment->Right],
									Slider[Dynamic@landEdgeRoughness,{0,1,step}, ContinuousAction -> False, ImageSize->180],
									Button["<",landEdgeRoughness=Max[landEdgeRoughness-=step,0],Appearance->"Palette"],
									Button[">",landEdgeRoughness=Min[landEdgeRoughness+=step,1],Appearance->"Palette"],
									" ",Pane[Dynamic@Round[landEdgeRoughness,step], ImageSize->30]}],
								Row[{Pane["pad ", ImageSize->60, Alignment->Right],
									Slider[Dynamic@ruralArea,{0,.25,.0125}, ContinuousAction -> False, ImageSize->180],
									Button["<",ruralArea=Max[ruralArea-=.0125,0],Appearance->"Palette"],
									Button[">",ruralArea=Min[ruralArea+=.0125,.25],Appearance->"Palette"],
									" ",Pane[Dynamic@Round[4ruralArea,.0125], ImageSize->30]}]
									
							}, Alignment->Center], ImageSize->{320, Automatic}]
							
						}
					}(*, Spacings->{Automatic, .4}*)],
						
					Column[{
					
						(* 	building pattern *)	
						ActionMenu[Style["BUILDING PATTERN", 15, Bold],{
							
								"House":>(currentBuildingPattern = "House"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = 1; scaleHeight = .15; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								"High-rise":>(currentBuildingPattern = "High-rise"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = 0; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								"--- TALL SIDE ---":>(Null),
								
								"Left":>(currentBuildingPattern = "Back left";  
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .3; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								"Right":>(currentBuildingPattern = "Back right";  
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .3; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								"Middle":>(currentBuildingPattern = "Middle";  
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .3; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								"Middle step":>(currentBuildingPattern = "Middle step";  
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .6; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = True; randomHeight = 1),
								
								"Perimeter":>(currentBuildingPattern = "Perimeter"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .3; scaleHeight = .2; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								"-- TALL CORNER --":>(Null),
								
								"Left":>(currentBuildingPattern = "Corner left"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .3; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {1, -1}; reverseHeight = False; randomHeight = 1),
								
								"Right":>(currentBuildingPattern = "Corner right"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .3; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {-1, 1}; reverseHeight = False; randomHeight = 1),
								
								"Back":>(currentBuildingPattern = "Corner back"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .3; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {-1, -1}; reverseHeight = False; randomHeight = 1),
								
								"Four":>(currentBuildingPattern = "Corners"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .3; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								"Perimeter":>(currentBuildingPattern = "Corner perimeter"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .3; scaleHeight = .2; subdivideDistanceOrigin = {0, 0}; reverseHeight = False; randomHeight = 1),
								
								"-- HEIGHT MAPS --":>(Null),
								
								dwCityHeightPatternFunction["HM 1", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 1"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								dwCityHeightPatternFunction["HM 2", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 2"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								dwCityHeightPatternFunction["HM 3", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 3"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								dwCityHeightPatternFunction["HM 4", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 4"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								dwCityHeightPatternFunction["HM 5", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 5"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								dwCityHeightPatternFunction["HM 6", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 6"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								dwCityHeightPatternFunction["HM 7", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 7"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								dwCityHeightPatternFunction["HM 8", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 8"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								dwCityHeightPatternFunction["HM 9", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 9"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1),
								
								dwCityHeightPatternFunction["HM 10", 9, 9, seed, "Preview"->True, ImageSize->24]:>(currentBuildingPattern = "HM 10"; 
									flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; roofQuantity = .65; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = 1)
								
								}, Appearance->"PopupMenu", BaselinePosition->Scaled[.25], ImageSize->{318, $dwStyleButtonHeight}],
						Row[{
							Button[Style["Default", 10], currentBuildingPattern = "Default"; $dwCBheightPattern = dwCityHeightPatternFunction["Default", rows, columns, seed]; flipHeight = 0; minBuildingHeight = defaultMinBuildingHeight; 
								roofQuantity = .65; scaleByHeight = .5; scaleHeight = defaultScaleHeight; subdivideDistanceOrigin = {}; reverseHeight = False; randomHeight = .75; randomScale = defaultRandomScale; scaleEach = {.5, .5}, 
								Appearance->"Palette", ImageSize->{79, $dwStyleButtonHeight}],
							Button[Style["Uniform", 10], randomScale = {0, 0}; randomHeight = 1; scaleByHeight = 0; scaleEach = {.5, .5}, 
								Appearance->"Palette", ImageSize->{79, $dwStyleButtonHeight}],
							Button[Style["Non-uniform", 10], flipHeight = 0; randomScale = defaultRandomScale; randomHeight = .75; scaleByHeight = .5; scaleEach = {.5, .5}, 
								Appearance->"Palette", ImageSize->{79, $dwStyleButtonHeight}],
							Button[Style["Same height", 10], flipHeight = .5, 
								Appearance->"Palette", ImageSize->{79, $dwStyleButtonHeight}]
						}],
						
						Panel[Column[{
							Grid[{{Row[{
								Tooltip[
										ActionMenu[Dynamic@buildingColorQuantity, 
											Flatten[{
												
												"Default colors" :> (colorByHeight = True; contrast = .6; invertHeightColor = False; windowColor = defaultWindowColor;
													buildingColor = defaultBuildingColors;
													roofColor = GrayLevel[.5];
													If[buildingColorQuantity === "5SC", buildingColorQuantity = 5]),
													
												Delimiter,
												
												Dynamic[If[colorByHeight, "Do not color buildings by roof", "Color buildings by roof"]] :> (colorByHeight = If[colorByHeight, False, True]),
												Dynamic[If[invertHeightColor, "Same color for flat roofed buildings", "Same color for sloped roofed buildings"]] :> (colorByHeight = True; invertHeightColor = If[invertHeightColor, False, True]),
													
												Delimiter,
												
												"BUILDING COLORS",
													
												"1 Color" :> (buildingColorQuantity = 1; ++rows; --rows), (* rows is used to update dynamics since buildingColorQuantity causes dynamic issues in TrackedSymbols *)
												"2 Colors" :> (buildingColorQuantity = 2; ++rows; --rows),
												"3 Colors" :> (buildingColorQuantity = 3; ++rows; --rows),
												"4 Colors" :> (buildingColorQuantity = 4; ++rows; --rows),
												"5 Colors" :> (buildingColorQuantity = 5; ++rows; --rows),
													
												Delimiter,
												
												"FIVE SCENE COLORS",
												
												Table[
													With[{color = data},
														Row[color]:>(
															buildingColorQuantity = "5SC"; 
															buildingColor = color
														)],
													{data, {
														ColorData[106, "ColorList"][[{2,3,1,4,5}]],
														{Orange, White, Black, Orange, Black},
														{White, Black, Orange, Black, Black},
														{White, Black, Black, Orange, Black},
														{Orange, White, Black, Gray, Black}
													}
												}]
												
											}], 
											Alignment->Center, Appearance->"Palette", BaselinePosition->Scaled[.4], ImageSize->{62, 31}
										],
										"Color palette choices..."
									],
									
									Tooltip[
										Button["<", buildingColor = RotateLeft[buildingColor], Appearance->"Palette", BaselinePosition->Scaled[.4], ImageSize->{Automatic, 31}],
										"Move palette colors left"
									],
									Sequence@@Table[With[{n = bcn}, ColorSetter[Dynamic@buildingColor[[n]], ImageSize->{24, 32}]], {bcn, 5}],
									
									Tooltip[
										Button[">", buildingColor = RotateRight[buildingColor], Appearance->"Palette", BaselinePosition->Scaled[.4], ImageSize->{Automatic, 31}],
										"Move palette colors right"
									]
									
								}],
									ColorSetter[Dynamic@windowColor],
									ColorSetter[Dynamic@roofColor]
								},
								{"buildings", "windows", "roofs"}
							}],
							"",
							Style["BUILDING HEIGHT", 12],
							Row[{Pane["min height ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@minBuildingHeight,{.025,1,.5step}, ContinuousAction -> False, ImageSize->180],
								Button["<",minBuildingHeight=Max[minBuildingHeight-=.5step,.025],Appearance->"Palette"],
								Button[">",scaleHeminBuildingHeightight=Min[minBuildingHeight+=.5step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[minBuildingHeight,.5step], ImageSize->30]}],
							Row[{Pane["max height ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@scaleHeight,{.1,1,.5step}, ContinuousAction -> False, ImageSize->180],(* minimum must be 0.1; 0.05 causes error *)
								Button["<",scaleHeight=Max[scaleHeight-=.5step,.1],Appearance->"Palette"],
								Button[">",scaleHeight=Min[scaleHeight+=.5step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[scaleHeight,.5step], ImageSize->30]}],
							Row[{Pane["flip height ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@flipHeight,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",flipHeight=Max[flipHeight-=step,0],Appearance->"Palette"],
								Button[">",flipHeight=Min[flipHeight+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[flipHeight,step], ImageSize->30]}],
							Style["SLOPED ROOFS", 12],
							Row[{Pane["height ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@roofHeight,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",roofHeight=Max[roofHeight-=step,0],Appearance->"Palette"],
								Button[">",roofHeight=Min[roofHeight+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[roofHeight,step], ImageSize->30]}],
							Row[{Pane["hipped ", ImageSize->60, Alignment->Right],
								Dynamic@Slider[Dynamic@roofHipped,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",roofHipped=Max[roofHipped-=step,0],Appearance->"Palette"],
								Button[">",roofHipped=Min[roofHipped+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@roofHipped, ImageSize->30]}],
							Row[{Pane["quantity ", ImageSize->60, Alignment->Right],
								Dynamic@Slider[Dynamic@roofQuantity,{0,1,step}, ContinuousAction -> False, ImageSize->180],
								Button["<",roofQuantity=Max[roofQuantity-=step,0],Appearance->"Palette"],
								Button[">",roofQuantity=Min[roofQuantity+=step,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[roofQuantity,.01], ImageSize->30]}],
							""
						}, Alignment->Center], Alignment->Center, ImageSize->320],
					
						Panel[Column[{
							Style["STREETS", 15, Bold],
							Grid[{{ColorSetter[Dynamic@streetColor], 
								Row[{ColorSetter[Dynamic@streetCenterlineColor], 
									PopupMenu[Dynamic@streetCenterlineDash, 
										{
											{3,3} -> Graphics[{AbsoluteDashing[{3,3}],Line[{{0,.1},{1,.1}}]}, ImageSize->50],
											{4,4} -> Graphics[{AbsoluteDashing[{4,4}],Line[{{0,.1},{1,.1}}]}, ImageSize->50],
											{5,5} -> Graphics[{AbsoluteDashing[{5,5}],Line[{{0,.1},{1,.1}}]}, ImageSize->50]
										}, Alignment->Center, Appearance->"Palette", BaselinePosition->Scaled[.4], ImageSize->{3 $dwStyleButtonHeight, 31}]
									}], 
								ColorSetter[Dynamic@streetCurbColor], 
								ColorSetter[Dynamic@concreteColor]},
								{"street", "lane divider", "curb", "concrete"}}],
							Row[{Pane["size ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@streetThickness,{.02,.04,.0025}, ContinuousAction -> False, ImageSize->180],
								Button["<",streetThickness=Max[streetThickness-=.0025,.02],Appearance->"Palette"],
								Button[">",streetThickness=Min[streetThickness+=.0025,.04],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[10streetThickness,.01], ImageSize->30]}],
							Row[{Pane["removal ", ImageSize->60, Alignment->Right],
								Dynamic@Slider[Dynamic@streetPad,{-2streetThickness,2streetThickness,.01}, ContinuousAction -> False, ImageSize->180],
								Button["<",streetPad=Max[streetPad-=.01,-2streetThickness],Appearance->"Palette"],
								Button[">",streetPad=Min[streetPad+=.01,2streetThickness],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[10streetPad,.1], ImageSize->30]}],
							""
						}, Alignment->Center], Alignment->Center, ImageSize->320],
						
						Panel[Column[{
							Style["ENVIRONMENT", 15, Bold],
							Row[{
								Button["No layers", soilLayer = subsoilLayer = rockLayer = bedrockLayer = 0, 
									Appearance->"Palette", ImageSize->{4$dwStyleButtonHeight, $dwStyleButtonHeight}],
								Button["One layer", soilLayer = 0.025; subsoilLayer = rockLayer = bedrockLayer = 0, 
									Appearance->"Palette", ImageSize->{4$dwStyleButtonHeight, $dwStyleButtonHeight}],
								Button["All layers", soilLayer = 0.025; subsoilLayer = .025; rockLayer = .05; bedrockLayer = .075, 
									Appearance->"Palette", ImageSize->{4$dwStyleButtonHeight, $dwStyleButtonHeight}]
							}],
							Grid[{
								{
									ColorSetter[Dynamic@backgroundColor], ColorSetter[Dynamic@waterColor], 
									ColorSetter[Dynamic@barkColor], ColorSetter[Dynamic@grassColor], 
									ColorSetter[Dynamic@soilColor], ColorSetter[Dynamic@subsoilColor], 
									ColorSetter[Dynamic@rockColor], ColorSetter[Dynamic@bedrockColor]
								},
								{"bkgnd","water","bark","foliage","soil","subsoil","rock","bedrock"}
							}, Spacings->{.25, Automatic}],
							Row[{Pane["soil ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@soilLayer,{0,1,groundStep}, ContinuousAction -> False, ImageSize->180],
								Button["<",soilLayer=Max[soilLayer-=groundStep,0],Appearance->"Palette"],
								Button[">",soilLayer=Min[soilLayer+=groundStep,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[soilLayer,groundStep], ImageSize->30]}],
							Row[{Pane["subsoil ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@subsoilLayer,{0,1,groundStep}, ContinuousAction -> False, ImageSize->180],
								Button["<",subsoilLayer=Max[subsoilLayer-=groundStep,0],Appearance->"Palette"],
								Button[">",subsoilLayer=Min[subsoilLayer+=groundStep,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[subsoilLayer,groundStep], ImageSize->30]}],
							Row[{Pane["rock ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@rockLayer,{0,1,groundStep}, ContinuousAction -> False, ImageSize->180],
								Button["<",rockLayer=Max[rockLayer-=groundStep,0],Appearance->"Palette"],
								Button[">",rockLayer=Min[rockLayer+=groundStep,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[rockLayer,groundStep], ImageSize->30]}],
							Row[{Pane["bedrock ", ImageSize->60, Alignment->Right],
								Slider[Dynamic@bedrockLayer,{0,1,groundStep}, ContinuousAction -> False, ImageSize->180],
								Button["<",bedrockLayer=Max[bedrockLayer-=groundStep,0],Appearance->"Palette"],
								Button[">",bedrockLayer=Min[bedrockLayer+=groundStep,1],Appearance->"Palette"],
								" ",Pane[Dynamic@Round[bedrockLayer,groundStep], ImageSize->30]}]
						}, Alignment->Center, Spacings->.5], Alignment->Center, ImageSize->{320, Automatic}]
					}(*, Spacings->0*)]
					},
						
					{	
						
						(* BOTTOM ROW BUTTONS *)
						Grid[{{
							
							Tooltip[
								Button["Import settings...",
				
					  				filename=SystemDialogInput["FileOpen", CurrentValue["NotebookBrowseDirectory"], WindowTitle->"Select a city builder file (xxxx.ctybldr)"];
										If[filename=!=$Canceled,
											
											Quiet@If[StringMatchQ[filename, {"*.ctybldr"}],
												 
												temp = Uncompress[Import[filename, "Text"]];
												If[ToString[temp[[1]]] =!= "CityBuilder",
													
													MessageDialog["Not a city builder file."],
													
													{
														backgroundColor, barkColor, bedrockColor, birdColor, birdShape, birdSize, buildingColor, buildingColorQuantity, buildingRemoval, cityObjects, colorByHeight, columns, concreteColor, contrast, edgeTrees, exitRoads, grassColor, groundStep, 
														$dwCBheightPattern, invertHeightColor, minBuildingHeight, randomHeight, randomScale, removeBlockedStreets, reverseHeight, flipHeight, overallTreeSize, riverTurns, rockColor, roofColor, roofHeight, roofHipped, 
														roofQuantity, rows, ruralArea, scaleEach, scaleByHeight, scaleHeight, seed, showBuildings, showEnvironment, showStreets, showWindows, waterColor, 
														soilColor, soilLayer, step, streetCenterlineColor, streetColor, streetCurbColor, streetPad, streetThickness, subdivideDistanceOrigin, 
														subdivideScale, subdivideSize, subsoilColor, subsoilLayer, traffic, windowColor, windowSize, windowStripes, yardSize, landEdgeRoughness
													} = temp[[2;;-1]]
												],
												
												MessageDialog["Not a city builder file."]
											]
										], Method->"Queued"
								], "Import a city builder file (xxxx.ctyblder)"
							],
							
							Tooltip[
								Button["Export settings...",
									
					  				filename = SystemDialogInput["FileSave", "untitled.ctybldr", WindowTitle->"Save city builder settings to file"];
									If[filename =!= $Canceled,
										filename = If[!StringMatchQ[filename,"*.ctybldr"], filename<>".ctybldr", filename];
										Export[Evaluate@filename, Compress[{"CityBuilder",  
											backgroundColor, barkColor, bedrockColor, birdColor, birdShape, birdSize, buildingColor, buildingColorQuantity, buildingRemoval, cityObjects, colorByHeight, columns, concreteColor, contrast, edgeTrees, exitRoads, grassColor, groundStep, 
											$dwCBheightPattern, invertHeightColor, minBuildingHeight, randomHeight, randomScale, removeBlockedStreets, reverseHeight, flipHeight, overallTreeSize, riverTurns, rockColor, roofColor, roofHeight, roofHipped, 
											roofQuantity, rows, ruralArea, scaleEach, scaleByHeight, scaleHeight, seed, showBuildings, showEnvironment, showStreets, showWindows, waterColor, 
											soilColor, soilLayer, step, streetCenterlineColor, streetColor, streetCurbColor, streetPad, streetThickness, subdivideDistanceOrigin, 
											subdivideScale, subdivideSize, subsoilColor, subsoilLayer, traffic, windowColor, windowSize, windowStripes, yardSize, landEdgeRoughness}], "Text"]
									], Method->"Queued"
								], "Save city builder settings to file"
							],
							
							CancelButton[DialogReturn[]],
							
							Dynamic@DefaultButton["Create "<> ToString[Row[{rows,"x",columns}]] <>" city for canvas",
								dwSetUndo[];
								$dwSynchronousUpdating = False; (* improve speed for large quantity of graphics *)
								$dwConstrainHAngle = angleH; $dwConstrainVAngle = angleV;
								startLength = Length[$dwP];
								$dwCBrandomTreeCtr = randomTreeStart;
								treeSize = overallTreeSize*(.5/(Max[rows,columns]^2)^.5);
								cityObjCtr = 0;
								
								(* reset city if currently expanded *)
								dwCityBuilderExpand[
									"Preview"->True, "Rows"->rows, "Columns"->columns, "ImageSize"->Max[rows, columns]*100, "FinalScale"->1, 
									"PreviewRows"->rows, "PreviewColumns"->columns, "CurrentBuildingPattern"->currentBuildingPattern, "BuildingColorQuantity"->buildingColorQuantity, 
									"AngleH"->angleH, "AngleV"->angleV, "BackgroundColor"->backgroundColor, "BarkColor"->barkColor, "BedrockColor"->bedrockColor, "BedrockLayer"->bedrockLayer, "BirdColor"->birdColor, 
									"BirdShape"->birdShape, "BirdSize"->birdSize, "BuildingColor"->buildingColor, "BuildingOpacityFill"->buildingOpacityFill, "BuildingOpacityStroke"->buildingOpacityStroke,  "BuildingRemoval"->buildingRemoval, "CityObjects"->cityObjects, 
									"ColorByHeight"->colorByHeight, "ConcreteColor"->concreteColor, "ConcreteSize"->concreteSize, "Contrast"->contrast, "CurbThickness"->curbThickness, 
									"DoorAwningDepth"->doorAwningDepth, "DoorHeight"->doorHeight, "DoorLimit"->doorLimit, "DoorWidth"->doorWidth, "EdgeTrees"->edgeTrees, 
									"ExitRoads"->dwExtremeExitRoads[rows, columns, rows, columns, exitRoads], "FlipHeight"->flipHeight, "GrassColor"->grassColor, 
									"InvertHeightColor"->invertHeightColor, "MaxBuildingHeight"->maxBuildingHeight, "MinBuildingHeight"->minBuildingHeight, 
									"LandEdgeRoughness"->landEdgeRoughness, "LandEdgeLimit"->landEdgeLimit, "OverallTreeSize"->overallTreeSize, 
									"RandomHeight"->randomHeight, "RandomScale"->randomScale, "RandomTreeStart"->randomTreeStart, "RemoveBlockedStreets"->removeBlockedStreets, 
									"ReverseHeight"->reverseHeight, "RiverTurns"->riverTurns, "RockColor"->rockColor, "RockLayer"->rockLayer, 
									"RoofColor"->roofColor, "RoofHeight"->roofHeight, "RoofHipped"->roofHipped, "RoofQuantity"->roofQuantity, 
									"RuralArea"->ruralArea, "RuralTreeBalance"->ruralTreeBalance, "RuralTreeCount"->ruralTreeCount, "ScaleByHeight"->scaleByHeight, 
									"ScaleEach"->scaleEach, "ScaleHeight"->scaleHeight, "Seed"->seed, "ShapePts"->shapePts, 
									"ShowBuildings"->showBuildings, "ShowEnvironment"->showEnvironment, "ShowStreets"->showStreets, "ShowTrees"->showTrees, "ShowWindows"->showWindows, 
									"SidewalkWidth"->sidewalkWidth, "WaterColor"->waterColor, "SoilColor"->soilColor, "SoilLayer"->soilLayer, 
									"StreetCenterlineColor"->streetCenterlineColor, "StreetCenterlineDash"->streetCenterlineDash, "StreetCenterlineOpacity"->streetCenterlineOpacity, 
									"StreetColor"->streetColor, "StreetCurbColor"->streetCurbColor, "StreetPad"->streetPad, "StreetThickness"->streetThickness, 
									"SubdivideDistanceOrigin"->subdivideDistanceOrigin, "SubdivideSize"->subdivideSize, "SubdivideScale"->subdivideScale, "SubsoilColor"->subsoilColor, 
									"SubsoilLayer"->subsoilLayer, "Traffic"->traffic, "Tree"->tree, "TreeDark"->treeDark, "TreeLight"->treeLight, "TreeRandomSize"->treeRandomSize, "TreeTrunk"->treeTrunk, 
									"TreeTrunkDark"->treeTrunkDark, "WindowColor"->windowColor, "WindowSize"->windowSize, "WindowStripes"->windowStripes, "YardSize"->yardSize
								];
			
								(* environment *)
								If[showEnvironment,
									Do[
										If[temp[[1, -1]] != 0,
											temp1 = -$dwCBscaleAll*Total[temp[[1]][[;;-2]]];
											temp2 = -$dwCBscaleAll*Total[temp[[1]]];
											Do[
												dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
												$dwP[[-1]] = 
													If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
														{{0, temp2}, {0, temp2}, {0, temp1}, {0, temp1}} + $dwCBlandPts[[{2,1,1,2}]],
														Join[Table[{0, temp2}, 2] + soilPts, Table[{0, temp1}, 2] + Reverse@soilPts]
													];
												style = ReplacePart[$dwFullDefaultStyle, temp[[2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
												style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style,
											{soilPts, Partition[Join[$dwCBlandPts[[IntegerPart[Length[$dwCBlandPts]/2];;-1]], $dwCBlandPts[[{1}]]], 2, 1]}];
											Do[
												dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
												$dwP[[-1]] = 
													If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
														{{0, temp2}, {0, temp2}, {0, temp1}, {0, temp1}} + $dwCBlandPts[[{1,4,4,1}]],
														Join[Table[{0, temp2}, 2] + soilPts, Table[{0, temp1}, 2] + Reverse@soilPts]
													];
												style = ReplacePart[$dwFullDefaultStyle, Darker[temp[[2]], .5 contrast], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
												style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style,
											{soilPts, Partition[$dwCBlandPts[[;;IntegerPart[Length[$dwCBlandPts]/2]]], 2, 1]}],
											Nothing
										], {temp,
												{
													{{0, soilLayer, subsoilLayer, rockLayer, bedrockLayer},
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[4]],
															_, bedrockColor
														]},
													{{0, soilLayer, subsoilLayer, rockLayer},
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[4]],
															_, rockColor
														]},
													{{0, soilLayer, subsoilLayer},
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[4]],
															_, subsoilColor
														]},
													{{0, soilLayer}, 
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[4]],
															_, soilColor
														]}
												}
											}
									];
										
									(* grass *)
									dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
									$dwP[[-1]] = $dwCBlandPts;
									style = ReplacePart[$dwFullDefaultStyle,
										Switch[buildingColorQuantity,
											"5SC", buildingColor[[3]],
											_, grassColor
										], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
									style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
									$dwStyle[[-1]] = style;
									If[FreeQ[concreteSize, 0],
										(* concrete *)
										dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
										$dwP[[-1]] = $dwCBconcretePts;
										style = ReplacePart[$dwFullDefaultStyle,
											Switch[buildingColorQuantity,
												"5SC", buildingColor[[3]],
												_, concreteColor
											], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
										style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
										$dwStyle[[-1]] = style,
										Nothing
									],
									Nothing
								];
			
								(* river *)
								If[riverTurns > 0 && (rows > 2 && columns > 2),
									
									dwNewEmptyLayer["Head"->BSplineCurve, "SetUndo"->False];
									$dwP[[-1]] = $dwCBriverFinalPts;
									style = ReplacePart[$dwFullDefaultStyle,
										Switch[buildingColorQuantity,
											"5SC", buildingColor[[1]],
											_, waterColor
										], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
									style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
									style = ReplacePart[style, True, 1];
									style = ReplacePart[style, 2, 11];
									$dwStyle[[-1]] = style;
									
									temp3 = Flatten[Position[$dwCBriverFinalPts, #, 1]&/@First/@Select[Gather[$dwCBriverFinalPts], Length[#] > 1 &]];
									
									dwNewEmptyLayer["Head"->BSplineCurve, "SetUndo"->False];
									$dwP[[-1]] = $dwCBriverFinalPts[[1;;temp3[[1]]]];
									style = ReplacePart[$dwFullDefaultStyle,
										Switch[buildingColorQuantity,
											"5SC", buildingColor[[5]],
											_, Darker[waterColor,.2]
										], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
									style = ReplacePart[style, CapForm["Butt"], Position[style, CapForm[_]]];
									style = ReplacePart[style, False, 1];
									style = ReplacePart[style, 2, 11];
									$dwStyle[[-1]] = style;
									
									dwNewEmptyLayer["Head"->BSplineCurve, "SetUndo"->False];
									$dwP[[-1]] = $dwCBriverFinalPts[[temp3[[-1]];;-1]];
									style = ReplacePart[$dwFullDefaultStyle,
										Switch[buildingColorQuantity,
											"5SC", buildingColor[[5]],
											_, Darker[waterColor,.2]
										], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
									style = ReplacePart[style, CapForm["Butt"], Position[style, CapForm[_]]];
									style = ReplacePart[style, False, 1];
									style = ReplacePart[style, 2, 11];
									$dwStyle[[-1]] = style,
									
									Nothing
								];
								
								(* sidewalks *)
								sidewalks = dwAxo[{#}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]&/@Table[{sw[[1]] + sidewalkWidth(sw[[1]] - sw[[4]]), sw[[2]] + sidewalkWidth(sw[[2]] - sw[[3]]), sw[[3]] + sidewalkWidth(sw[[3]] - sw[[2]]), sw[[4]] + sidewalkWidth(sw[[4]] - sw[[1]])}, {sw, $dwCBstreets/.{Null->Sequence[]}}];
								Do[
									dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
									$dwP[[-1]] = p;
									style = ReplacePart[$dwFullDefaultStyle,
										Switch[buildingColorQuantity,
											"5SC", buildingColor[[3]],
											_, concreteColor
										], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
									style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
									$dwStyle[[-1]] = style,
								{p, sidewalks}];
								
								(* street curbs *)
								Do[
									dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
									$dwP[[-1]] = p;
									style = ReplacePart[$dwFullDefaultStyle, 
										Switch[buildingColorQuantity,
											"5SC", buildingColor[[5]],
											_, streetCurbColor
										], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
									style = ReplacePart[style, 1, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
									style = ReplacePart[style, 2curbThickness, Join[Flatten[Position[style, StrokeForm[_]]], {1,3,1}]];
									style = ReplacePart[style, "Butt", Join[Flatten[Position[style, StrokeForm[_]]], {1,5,1}]];
									$dwStyle[[-1]] = style,
								{p, Flatten[Table[dwAxo[{p}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"], {p, #[[{1, 2}]]&/@($dwCBstreets/.{Null->Sequence[]})}], 1]}];
								
								(* streets and center lines (center lines do not cross intersections *)
								Do[
									If[$dwCBstreets[[n]] =!= Null,
										(* street *)
										dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
										$dwP[[-1]] = dwAxo[{$dwCBstreets[[n]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										style = ReplacePart[$dwFullDefaultStyle,
											Switch[buildingColorQuantity,
												"5SC", White,
												_, streetColor
											], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
										style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
										$dwStyle[[-1]] = style;
										(* line *)
										dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
										$dwP[[-1]] = dwAxo[{
												If[MemberQ[exitRoads, n],
													{Mean[$dwCBstreets[[n]][[{1, 4}]]], Mean[$dwCBstreets[[n]][[{2, 3}]]]},
													{($dwCBstreets[[n]][[1]] + .5($dwCBstreets[[n]][[4]] - $dwCBstreets[[n]][[1]])) + .225($dwCBstreets[[n]][[2]] - $dwCBstreets[[n]][[1]]), ($dwCBstreets[[n]][[2]] + .5($dwCBstreets[[n]][[3]] - $dwCBstreets[[n]][[2]])) + .3($dwCBstreets[[n]][[1]] - $dwCBstreets[[n]][[2]])}
												]
											}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										style = ReplacePart[$dwFullDefaultStyle,
											Switch[buildingColorQuantity,
												"5SC", buildingColor[[3]],
												_, streetCenterlineColor
											], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
										style = ReplacePart[style, streetCenterlineOpacity, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
										style = ReplacePart[style, streetCenterlineDash, Join[Flatten[Position[style, StrokeForm[_]]], {1,4,1}]];
										$dwStyle[[-1]] = style
									],
								{n, Length[$dwCBstreets]}];
								
								(* street crosswalks *)
								Do[
									If[$dwCBstreets[[n]] =!= Null,
										If[FreeQ[exitRoads, n],
											Do[
												dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
												$dwP[[-1]] = dwAxo[{{($dwCBstreets[[n]][[1]] + lineloc($dwCBstreets[[n]][[4]] - $dwCBstreets[[n]][[1]])) + .225($dwCBstreets[[n]][[2]] - $dwCBstreets[[n]][[1]]), ($dwCBstreets[[n]][[1]] + lineloc($dwCBstreets[[n]][[4]] - $dwCBstreets[[n]][[1]])) + .3($dwCBstreets[[n]][[2]] - $dwCBstreets[[n]][[1]])}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
												style = ReplacePart[$dwFullDefaultStyle,
													Switch[buildingColorQuantity,
														"5SC", buildingColor[[3]],
														_, streetCenterlineColor
													], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
												style = ReplacePart[style, streetCenterlineOpacity, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style,
											{lineloc, {1/6, 1/3, 1/2, 2/3, 5/6}}];
											Do[
												dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
												$dwP[[-1]] = dwAxo[{{($dwCBstreets[[n]][[2]] + lineloc($dwCBstreets[[n]][[3]] - $dwCBstreets[[n]][[2]])) + .3($dwCBstreets[[n]][[1]] - $dwCBstreets[[n]][[2]]), ($dwCBstreets[[n]][[2]] + lineloc($dwCBstreets[[n]][[3]] - $dwCBstreets[[n]][[2]])) + .225($dwCBstreets[[n]][[1]] - $dwCBstreets[[n]][[2]])}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
												style = ReplacePart[$dwFullDefaultStyle,
													Switch[buildingColorQuantity,
														"5SC", buildingColor[[3]],
														_, streetCenterlineColor
													], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
												style = ReplacePart[style, streetCenterlineOpacity, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style,
											{lineloc, {1/6, 1/3, 1/2, 2/3, 5/6}}]
										]
									],
								{n, Length[$dwCBstreets]}];
			
								(* vehicles *)
								temp = 0;
								temp3 = Length[($dwCBstreets/.{Null->Sequence[]})];
								carsize = 2*streetThickness;
								(
									SeedRandom[seed + temp++];
									temp1 = RandomReal[{.25, .75}];
									SeedRandom[seed + temp + 1];
									temp2 = RandomReal[{.25, .75}];
									
									(* vehicle set 1 *)
									If[#[[1, 1]] - #[[2, 1]] > #[[1, 2]] - #[[2, 2]],
										
										(* X roads - away from viewer *)
										topview1 = dwAxo[{carsize{{0.2, -0.4}, {0.2, 0.4}, {-0.2, 0.4}, {-0.2, -0.4}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										topview2 = dwAxo[{carsize{{0.2, -0.4}, {0.2, 0}, {-0.2, 0}, {-0.2, -0.4}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										Do[
											dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
											style = ReplacePart[$dwFullDefaultStyle,
												Switch[carPart[[1]],
													1,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[1]],
															_, Lighter[buildingColor[[Mod[temp+1, buildingColorQuantity, 1]]], .5]
														],
													2,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[5]],
															_, Darker[buildingColor[[Mod[temp+1, buildingColorQuantity, 1]]], .75contrast]
														],
													_,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[5]],
															_, buildingColor[[Mod[temp+1, buildingColorQuantity, 1]]]
														]
												], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
											style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
											$dwStyle[[-1]] = style;
											$dwP[[-1]] = Table[dwAxo[{(#[[4]]+3(#[[1]]-#[[4]])/4) + temp1*(#[[2]]-#[[1]])}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]] + $dwCBscaleAll*carPts, {carPts, carPart[[2]]}],
												
											{carPart, {
												(* side *)
												{-1, {topview1[[4]], topview2[[4]] + {0, .25carsize}, topview2[[3]] + {0, .25carsize}, topview1[[3]]}},
												(* hood *) 
												{1, Join[topview2[[{2,3}]] + Table[{0, .25carsize}, 2], topview1[[{3,2}]]]},
												(* top *)
												{1, topview2 + Table[{0, .25carsize}, 4]},
												(* back *) 
												{2, Join[topview2[[{1,4}]] + Table[{0, .25carsize}, 2], topview1[[{4,1}]]]}
											}}
										],
										
										(* Y roads - towards viewer *)
										topview1 = dwAxo[{carsize{{0.4, 0.2}, {-0.4, 0.2}, {-0.4, -0.2}, {0.4, -0.2}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										topview2 = dwAxo[{carsize{{0.4, 0.2}, {0, 0.2}, {0, -0.2}, {0.4, -0.2}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										Do[
											dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
											style = ReplacePart[$dwFullDefaultStyle,
												Switch[carPart[[1]],
													1,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[1]],
															_, Lighter[buildingColor[[Mod[temp, buildingColorQuantity, 1]]], .5]
														],
													2,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[5]],
															_, Darker[buildingColor[[Mod[temp, buildingColorQuantity, 1]]], .75contrast]
														],
													_,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[5]],
															_, buildingColor[[Mod[temp, buildingColorQuantity, 1]]]
														]
												], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
											style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
											$dwStyle[[-1]] = style;
											$dwP[[-1]] = Table[dwAxo[{(#[[4]]+3(#[[1]]-#[[4]])/4) + temp2*(#[[2]]-#[[1]])}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]] + $dwCBscaleAll*carPts, {carPts, carPart[[2]]}],
											
											{carPart, {
												(* side *)
												{2, {topview1[[4]], topview2[[4]] + {0, .25carsize}, topview2[[3]] + {0, .25carsize}, topview1[[3]]}},
												(* hood *) 
												{-1, Join[topview2[[{2,3}]] + Table[{0, .25carsize}, 2], topview1[[{3,2}]]]},
												(* top *)
												{1, topview2 + Table[{0, .25carsize}, 4]}
											}}
										]
									];
									
									(*  vehicle set 2 - rotate set 1 points 180 degrees *)
									If[#[[1, 1]] - #[[2, 1]] > #[[1, 2]] - #[[2, 2]],
										
										(* X roads - towards viewer *)
										topview1 = dwAxo[{carsize{{-0.2, 0.4}, {-0.2, -0.4}, {0.2, -0.4}, {0.2, 0.4}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										topview2 = dwAxo[{carsize{{-0.2, 0.4}, {-0.2, 0}, {0.2, 0}, {0.2, 0.4}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										Do[
											dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
											style = ReplacePart[$dwFullDefaultStyle,
												Switch[carPart[[1]],
													1,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[1]],
															_, Lighter[buildingColor[[Mod[temp, buildingColorQuantity, 1]]], .5]
														],
													2,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[5]],
															_, Darker[buildingColor[[Mod[temp, buildingColorQuantity, 1]]], .75contrast]
														],
													_,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[5]],
															_, buildingColor[[Mod[temp, buildingColorQuantity, 1]]]
														]
												], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
											style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
											$dwStyle[[-1]] = style;
											$dwP[[-1]] = Table[dwAxo[{(#[[4]]+(#[[1]]-#[[4]])/4) + temp2*(#[[2]]-#[[1]])}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]] + $dwCBscaleAll*carPts, {carPts, carPart[[2]]}],
											
											{carPart, {
												(* side *)
												{-1, {topview1[[1]], topview2[[1]] + {0, .25carsize}, topview2[[2]] + {0, .25carsize}, topview1[[2]]}},
												(* hood *) 
												{2, Join[topview2[[{2,3}]] + Table[{0, .25carsize}, 2], topview1[[{3,2}]]]},
												(* top *)
												{1, topview2 + Table[{0, .25carsize}, 4]}
											}}
										],
										
										(* Y roads - away from viewer *)
										topview1 = dwAxo[{carsize{{-0.4, -0.2}, {0.4, -0.2}, {0.4, 0.2}, {-0.4, 0.2}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										topview2 = dwAxo[{carsize{{-0.4, -0.2}, {0, -0.2}, {0, 0.2}, {-0.4, 0.2}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
										Do[
											dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
											style = ReplacePart[$dwFullDefaultStyle,
												Switch[carPart[[1]],
													1,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[1]],
															_, Lighter[buildingColor[[Mod[temp+1, buildingColorQuantity, 1]]], .5]
														],
													2,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[5]],
															_, Darker[buildingColor[[Mod[temp+1, buildingColorQuantity, 1]]], .75contrast]
														],
													_,
														Switch[buildingColorQuantity,
															"5SC", buildingColor[[5]],
															_, buildingColor[[Mod[temp+1, buildingColorQuantity, 1]]]
														]
												], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
											style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
											$dwStyle[[-1]] = style;
											$dwP[[-1]] = Table[dwAxo[{(#[[4]]+(#[[1]]-#[[4]])/4) + temp1*(#[[2]]-#[[1]])}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]] + $dwCBscaleAll*carPts, {carPts, carPart[[2]]}],
												
											{carPart, {
												(* side *)
												{2, {topview1[[1]], topview2[[1]] + {0, .25carsize}, topview2[[2]] + {0, .25carsize}, topview1[[2]]}},
												(* hood *) 
												{1, Join[topview2[[{2,3}]] + Table[{0, .25carsize}, 2], topview1[[{3,2}]]]},
												(* top *)
												{1, topview2 + Table[{0, .25carsize}, 4]},
												(* back *) 
												{-1, Join[topview2[[{1,4}]] + Table[{0, .25carsize}, 2], topview1[[{4,1}]]]}
											}}
										]
									]
								)&/@(($dwCBstreets/.{Null->Sequence[]})[[Delete[(SeedRandom[seed]; RandomSample[Range[temp3]]), List/@Range[temp3 (1 - traffic)]]]]);
								
								(* environment highlight line - after streets *)
								If[showEnvironment && Total[{soilLayer, subsoilLayer, rockLayer, bedrockLayer}] != 0,
									dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
									$dwP[[-1]] = 
										If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
											$dwCBlandPts[[{2, 1, 4}]],
											Join[
												$dwCBlandPts[[Position[$dwCBlandPts, Sort[$dwCBlandPts, #[[1]] < #2[[1]] &][[1]]][[1, 1]];;-1]],
												$dwCBlandPts[[1;;Position[$dwCBlandPts, Sort[$dwCBlandPts, #[[1]] > #2[[1]] &][[1]]][[1, 1]]]]
											]
										];
									style = ReplacePart[$dwFullDefaultStyle, White, Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
									style = ReplacePart[style, 1, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
									$dwStyle[[-1]] = style,
									Nothing
								];
											
								(* plant trees in back for rural and rough edge areas *)
								If[showTrees && edgeTrees != 0,
									If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
										{},
										
										If[riverTurns == 0 && (exitRoads === {} || (!showStreets || DeleteDuplicates[$dwCBstreets] === {Null})),
							
											temp = Sort[SeedRandom[seed];RandomPoint[Polygon[$dwCBlandPtsNoRural[[;;2columns + 2]]], IntegerPart[columns*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&];
											temp1 = Sort[SeedRandom[seed];RandomPoint[Polygon[Join[$dwCBlandPtsNoRural[[4columns + 2rows + 2;;-1]], $dwCBlandPtsNoRural[[{1}]]]], IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&],
											
											temp = Quiet[BooleanRegion[
														And[#1, Not[#2]] &, 
														{
															Polygon[$dwCBlandPtsNoRural[[2columns + 1;;2columns + 2rows + 2]]],
															RegionUnion[Sequence@@Table[Polygon[sw], {sw, sidewalks}], Polygon[$dwCBriverFinalPts]] 
														}
													]];
											temp = If[Head[temp] === BooleanRegion,
												Region[Polygon[$dwCBlandPtsNoRural[[2columns + 1;;2columns + 2rows + 2]]]],
												temp
											];
											temp = Sort[SeedRandom[seed];RandomPoint[temp, IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&];
												
											temp1 = Quiet[BooleanRegion[
														And[#1, Not[#2]] &, 
														{
															Polygon[$dwCBlandPtsNoRural[[2columns + 2rows + 2;;4columns + 2rows + 2]]],
															RegionUnion[Sequence@@Table[Polygon[sw], {sw, sidewalks}], Polygon[$dwCBriverFinalPts]] 
														}
													]];
											temp1 = If[Head[temp1] === BooleanRegion,
												Region[Polygon[$dwCBlandPtsNoRural[[2columns + 2rows + 2;;4columns + 2rows + 2]]]],
												temp1
											];
											temp1 = Sort[SeedRandom[seed];RandomPoint[temp1, IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&]
										];
										Do[
											Do[
												SeedRandom[seed+$dwCBrandomTreeCtr++];
												randomReal = RandomReal[treeRandomSize];
												{
													(* tree trunk *)
													dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
													$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeTrunk;
													style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[barkColor, .1randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
													style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,.5randomReal]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
													style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
													$dwStyle[[-1]] = style,
													
													(* tree trunk dark *)
													dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
													$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeTrunkDark;
													style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,contrast*randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
													style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
													$dwStyle[[-1]] = style,
													
													(* tree leaves *)
													dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
													$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@tree;
													style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
													style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
													$dwStyle[[-1]] = style,
													
													dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
													$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeDark;
													style = ReplacePart[$dwFullDefaultStyle, Black, Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
													style = ReplacePart[style, If[buildingColorQuantity === "5SC", .25, .2+.33contrast^2], Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
													style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
													$dwStyle[[-1]] = style,
													
													dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
													$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeLight;
													style = ReplacePart[$dwFullDefaultStyle, White, Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
													style = ReplacePart[style, .35, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
													style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
													$dwStyle[[-1]] = style,
													
													dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
													$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@tree;
													style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
													style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor, contrast+.05]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
													style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
													$dwStyle[[-1]] = style
												},
											{treeLoc, tr}],
										{tr, {temp, temp1}}]
									],
									{}
								];
								
								(* buildings *)
								buildingColorQuantityOutput = currentBuildingColorQuantity = buildingColorQuantity;
								If[$dwCBbuildings =!= {},
									
									Do[
										
										extrude = (SeedRandom[n+seed];RandomReal[{randomHeight,1}])*$dwCBscaleAll*scaleHeight*($dwCBheightSize[[Mod[n, Length[$dwCBheightSize], 1]]]);
										
										Do[
											If[$dwCBbuildings[[n, bn, 1]] =!= Null && FreeQ[Flatten[$dwCBriverFlow], $dwCBriverToBuildingArray[[n]]],
												
												finalHeight = Min[Max[(SeedRandom[n+bn+seed];RandomReal[{randomHeight, 1}]*extrude), Min[minBuildingHeight, maxBuildingHeight]], Max[minBuildingHeight, maxBuildingHeight]];
												
												(* scale footprint by height *)
												finalHeightScaled = Rescale[{finalHeight}, $dwCBminMaxHeight, {.01, 1}][[1]];
												scaleByHeightFunction = ScalingTransform[({1, 1}-.5(scaleByHeight*finalHeightScaled)), dwFindCenter[$dwCBbuildings[[n, bn]]]];
												scaleByHeightPts = scaleByHeightFunction[$dwCBbuildings[[n, bn]]];
													
												extObj = dwAxoExtrude[{scaleByHeightPts}, "Extrude"->-finalHeight][[{2,3,5}]];
												
												buildingColorQuantityOutput = 
													Which[
														buildingColorQuantityOutput === "5SC",
															"5SC",
														!showBuildings,
															buildingColorQuantity,
														colorByHeight && invertHeightColor,
															If[finalHeightScaled <= roofQuantity, 1, currentBuildingColorQuantity],
														colorByHeight,
															If[finalHeightScaled > roofQuantity, 1, currentBuildingColorQuantity],
														True,
															currentBuildingColorQuantity
													];
												
												(* building *)
												If[showBuildings,
													
													Do[
														dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
														$dwP[[-1]] = extObj[[pn]];
														Switch[buildingColorQuantityOutput,
															"5SC",
																style = ReplacePart[$dwFullDefaultStyle, 
																	Switch[pn, 
																		1, buildingColor[[5]], 
																		2, buildingColor[[1]],
																		_, buildingColor[[2]]
																	], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
															_,
																style = ReplacePart[$dwFullDefaultStyle, 
																	Switch[pn, 
																		1, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], 
																		2, buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]],
																		_, Lighter[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast]
																	], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
														];
														Switch[buildingColorQuantityOutput,
															"5SC",
																style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
															_,
																style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
														];
														style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
														style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
														$dwStyle[[-1]] = style,
													{pn, Length[extObj]}];
												
													(* side line below top *)
													extraLineSpace = $dwCBscaleAll*.02(1/Length[$dwCBbuildings]^.1);
													If[finalHeight >= $dwCBscaleAll*.05, 
														dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
														$dwP[[-1]] = ({0, -extraLineSpace} + #)&/@extObj[[1, {3, 4}]];
														Switch[buildingColorQuantityOutput,
															"5SC",
																style = ReplacePart[$dwFullDefaultStyle, buildingColor[[1]], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]],
															_,
																style = ReplacePart[$dwFullDefaultStyle, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], 1/3contrast], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]]
														];
														style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
														style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
														$dwStyle[[-1]] = style, 
														Nothing
													];
													
													(* side line above bottom *)
													If[finalHeight >= $dwCBscaleAll*.1,  
														dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
														$dwP[[-1]] = ({0, extraLineSpace} + #)&/@extObj[[1, {1, 2}]];
														Switch[buildingColorQuantityOutput,
															"5SC",
																style = ReplacePart[$dwFullDefaultStyle, buildingColor[[1]], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]],
															_,
																style = ReplacePart[$dwFullDefaultStyle, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], 1/3contrast], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]]
														];
														style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
														style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
														$dwStyle[[-1]] = style, 
														Nothing
													];
												
													(* front line below top *)
													extraLineSpace = $dwCBscaleAll*.02(1/Length[$dwCBbuildings]^.1);
													If[finalHeight >= $dwCBscaleAll*.05, 
														dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
														$dwP[[-1]] = ({0, -extraLineSpace} + #)&/@extObj[[3, {1, 4}]];
														Switch[buildingColorQuantityOutput,
															"5SC",
																style = ReplacePart[$dwFullDefaultStyle, buildingColor[[5]], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]],
															_,
																style = ReplacePart[$dwFullDefaultStyle, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]]
														];
														style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
														style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
														$dwStyle[[-1]] = style, 
														Nothing
													];
													
													(* front line above bottom *)
													If[finalHeight >= $dwCBscaleAll*.1,  
														dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
														$dwP[[-1]] = ({0, extraLineSpace} + #)&/@extObj[[2, {1, 2}]];
														Switch[buildingColorQuantityOutput,
															"5SC",
																style = ReplacePart[$dwFullDefaultStyle, buildingColor[[5]], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]],
															_,
																style = ReplacePart[$dwFullDefaultStyle, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]]
														];
														style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
														style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
														$dwStyle[[-1]] = style, 
														Nothing
													];
														
													(* windows *)
													If[showWindows && windowSize != 0,
														(* right windows *)
														windowPts = dwCityWindows[extObj[[1]], windowSize, 
															Which[
																finalHeightScaled > roofQuantity + 2((1 - roofQuantity)/3), 
																	False,
																finalHeightScaled > roofQuantity + ((1 - roofQuantity)/3), 
																	True,
																True, 
																	False
															], rows*columns];
														
														Do[
															(* window *)
															dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
															$dwP[[-1]] = windowPts[[wn]];
															style = ReplacePart[$dwFullDefaultStyle, 
																Switch[buildingColorQuantityOutput,
																	"5SC", buildingColor[[1]],
																	_,
																		Darker[windowColor, .6 contrast]
																], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
															style = ReplacePart[style, 1, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
															style = ReplacePart[style, 
																Switch[buildingColorQuantityOutput,
																	"5SC", 
																		Darker[buildingColor[[1]], .15],
																	_,
																		Darker[windowColor, contrast]
																], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
															style = ReplacePart[style, 1, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															style = ReplacePart[style, .5, Join[Flatten[Position[style, StrokeForm[_]]], {1,3,1}]];
															style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
															style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															$dwStyle[[-1]] = style;
															(* extra highlight *)
															dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
															$dwP[[-1]] = windowPts[[wn]][[{2,3,4}]];
															style = ReplacePart[$dwFullDefaultStyle, 
																Switch[buildingColorQuantityOutput,
																	"5SC", 
																		Lighter[buildingColor[[1]], .25],
																	_,
																		windowColor
																], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
															style = ReplacePart[style, 1, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															style = ReplacePart[style, .5, Join[Flatten[Position[style, StrokeForm[_]]], {1,3,1}]];
															style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
															style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															$dwStyle[[-1]] = style,
														{wn, Length[windowPts]}];
														
														(* left windows *)
														windowPts = dwCityWindows[extObj[[2]], windowSize, 
															If[finalHeightScaled > roofQuantity + 2((1 - roofQuantity)/3), 
																	True,
																	False
															], rows*columns];
														Do[
															(* window *)
															dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
															$dwP[[-1]] = windowPts[[wn]];
															style = ReplacePart[$dwFullDefaultStyle, 
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		buildingColor[[5]],
																	_, Lighter[windowColor, contrast]
																], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
															style = ReplacePart[style, 1, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
															style = ReplacePart[style, 1, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															style = ReplacePart[style, 
																Switch[buildingColorQuantityOutput,
																	"5SC", 
																		Darker[buildingColor[[5]], .15],
																	_,
																		windowColor
																], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
															style = ReplacePart[style, 1, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															style = ReplacePart[style, .5, Join[Flatten[Position[style, StrokeForm[_]]], {1,3,1}]];
															style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
															style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															$dwStyle[[-1]] = style;
															(* extra highlight *)
															dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
															$dwP[[-1]] = windowPts[[wn]][[{2,3,4}]];
															style = ReplacePart[$dwFullDefaultStyle, 
																Switch[buildingColorQuantityOutput,
																	"5SC", 
																		Lighter[buildingColor[[5]], .25],
																	_,
																		Lighter[windowColor, 1.5contrast]
																], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
															style = ReplacePart[style, 1, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															style = ReplacePart[style, .5, Join[Flatten[Position[style, StrokeForm[_]]], {1,3,1}]];
															style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
															style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															$dwStyle[[-1]] = style,
														{wn, Length[windowPts]}],
														Nothing
													];
													
													(* door *)
													If[finalHeight >= $dwCBscaleAll*doorLimit,
														temp = ScalingTransform[doorWidth*extraLineSpace*{1,1}/$dwCBscaleAll, Mean[extObj[[2, {1, 2}]]]];
														temp1 = ScalingTransform[-doorWidth*extraLineSpace*{1,1}/$dwCBscaleAll, extObj[[1, 2]]];
														temp2 = ScalingTransform[doorAwningDepth*extraLineSpace*{1,1}/$dwCBscaleAll, extObj[[1, 2]]];
														dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
														$dwP[[-1]] = temp[Join[
																extObj[[2, {2, 1}]],
																({0, doorHeight*extraLineSpace} + #)&/@extObj[[2, {1, 2}]]
															]];
														style = ReplacePart[$dwFullDefaultStyle, 
															Switch[buildingColorQuantityOutput,
																"5SC", 
																	buildingColor[[1]],
																_,
																	Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast]
															], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
														Switch[buildingColorQuantityOutput,
															"5SC",
																style = ReplacePart[style, buildingColor[[5]], Join[Flatten[Position[style, FaceForm[_]]], {1,1}]],
															_,
																style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], Join[Flatten[Position[style, FaceForm[_]]], {1,1}]]
														];
														style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
														style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
														$dwStyle[[-1]] = style;
														dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
														$dwP[[-1]] = 
															(# + (temp[{0, doorHeight*extraLineSpace} + extObj[[2, 1]]] - extObj[[1, 2]]))&/@
															temp1[
																Join[
																	temp2[extObj[[1, {2, 1}]]],
																	(Subtract[Sequence@@(({0, doorHeight*extraLineSpace} + #)&/@extObj[[2, {1, 2}]])] + #)&/@temp2[extObj[[1, {1, 2}]]]
																]
															];
														style = ReplacePart[$dwFullDefaultStyle, 
															Switch[buildingColorQuantityOutput,
																"5SC", 
																	buildingColor[[1]],
																_,
																	Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast]
															], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
															
														Switch[buildingColorQuantityOutput,
															"5SC",
																style = ReplacePart[style, buildingColor[[2]], Join[Flatten[Position[style, FaceForm[_]]], {1,1}]],
															_,
																style = ReplacePart[style, Lighter[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], Join[Flatten[Position[style, FaceForm[_]]], {1,1}]]
														];
														style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
														style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
														$dwStyle[[-1]] = style
													];
													
													(* roof *)
													If[finalHeightScaled <= roofQuantity,
														
														roofCenter = Mean[extObj[[3, {1,3}]]];
														temp = ScalingTransform[{1/2, 1/2}, Mean[extObj[[3, {1,4}]]]];
														temp1 = ScalingTransform[{2/3, 2/3}, Mean[extObj[[3, {1,4}]]]];
														temp2 = ScalingTransform[{1/4, 1/4}, Mean[extObj[[3, {3,4}]]]];
														temp3 = ScalingTransform[{1/4, 1/4}, Mean[extObj[[3, {2,3}]]]];
														Do[
															dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
															$dwP[[-1]] = pn[[2]];
															style = ReplacePart[$dwFullDefaultStyle, 
																Switch[pn[[1]],
																	1|5, 
																		Switch[buildingColorQuantityOutput,
																			"5SC", buildingColor[[2]],
																			_, Darker[roofColor, 1/3 contrast]
																		], 
																	2, 
																		Switch[buildingColorQuantityOutput,
																			"5SC", buildingColor[[2]],
																			_, Darker[roofColor, 2/3 contrast]
																		], 
																	3|6, 
																		Switch[buildingColorQuantityOutput,
																			"5SC", buildingColor[[2]],
																			_, Darker[roofColor, contrast]
																		], 
																	7, 
																		Switch[buildingColorQuantityOutput,
																			"5SC", buildingColor[[1]],
																			_, buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]]
																		], 
																	8, 
																		Switch[buildingColorQuantity,
																			"5SC", buildingColor[[5]],
																			_, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast]
																		], 
																	_, 
																		Switch[buildingColorQuantityOutput,
																			"5SC", buildingColor[[2]],
																			_, roofColor
																]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
															If[showStroke, 
																style = ReplacePart[style, 
																	Switch[buildingColorQuantityOutput,
																		"5SC", buildingColor[[1]],
																		_, Darker[roofColor, contrast]
																	], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]]
															];
															style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
															style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
															$dwStyle[[-1]] = style,
															
															{pn, 
																If[EuclideanDistance[Sequence@@extObj[[3, {1, 2}]]] < EuclideanDistance[Sequence@@extObj[[3, {2, 3}]]],
																	{
																		Sequence@@If[finalHeight < $dwCBscaleAll*.1,
																			(* chimney *)
																			{
																				{7, Join[-(extObj[[3, 2]] - extObj[[3, 1]])*chimneyWidth + #&/@Reverse[temp3[extObj[[3, {2,3}]]]], 
																						-(extObj[[3, 2]] - extObj[[3, 1]])*chimneyWidth + #&/@(temp3[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {2,3}]]])
																					]},
																				{7, Join[Reverse[temp3[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {2,3}]]]],
																						-(extObj[[3, 2]] - extObj[[3, 1]])*chimneyWidth + #&/@(temp3[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {2,3}]]])
																					]},
																				{8, Join[temp3[extObj[[3, {2,3}]]][[{2}]],
																						temp3[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {2,3}]]][[{2}]],
																						(-(extObj[[3, 2]] - extObj[[3, 1]])*chimneyWidth + #&/@(temp3[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {2,3}]]]))[[{2}]],
																						(-(extObj[[3, 2]] - extObj[[3, 1]])*chimneyWidth + #&/@temp3[extObj[[3, {2,3}]]])[[{2}]]
																					]}
																			},
																			{}
																		],
																		
																		(* roof - direction 1 *)
																		{1, Join[extObj[[3, {1,2}]], {{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {1, 2}]]] - roofCenter)}]},
																		{2, Join[extObj[[3, {3,2}]], 
																			{{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {1, 2}]]] - roofCenter)},
																			{{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {3, 4}]]] - roofCenter)}]},
																		{3, Join[extObj[[3, {3,4}]], {{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {3, 4}]]] - roofCenter)}]},
																		{4, Join[extObj[[3, {4,1}]], 
																			{{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {1, 2}]]] - roofCenter)},
																			{{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {3, 4}]]] - roofCenter)}]},
																		Sequence@@If[finalHeight >= $dwCBscaleAll*.1,
																			{
																				(* front peak *)
																				{5, Join[{temp[extObj[[3, {4,1}]]][[2]]}, 
																						temp1[{{Mean[extObj[[3, {4,1}]]][[1]], EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2 + roofCenter[[2]] + (Mean[extObj[[3, {1, 4}]]] - roofCenter)[[2]]}}],
																						temp1[{{roofCenter[[1]], EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2 + roofCenter[[2]]}}]
																					]},
																				{6, Join[{temp[extObj[[3, {4,1}]]][[1]]}, 
																						temp1[{{Mean[extObj[[3, {4,1}]]][[1]], EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2 + roofCenter[[2]] + (Mean[extObj[[3, {1, 4}]]] - roofCenter)[[2]]}}],
																						temp1[{{roofCenter[[1]], EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2 + roofCenter[[2]]}}]
																					]},
																				{7, Join[temp[extObj[[3, {4,1}]]], 
																						temp1[{{Mean[extObj[[3, {4,1}]]][[1]], EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2 + roofCenter[[2]] + (Mean[extObj[[3, {1, 4}]]] - roofCenter)[[2]]}}]
																					]},
																					
																				(* chimney *)
																				{8, Join[temp2[extObj[[3, {3,4}]]], 
																						temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {4,3}]]]
																					]},
																				{7, Join[Reverse[temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {4,3}]]]],
																						-(extObj[[3, 4]] - extObj[[3, 1]])*chimneyWidth + #&/@(temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {4,3}]]])
																					]},
																				{7, Join[temp2[extObj[[3, {3,4}]]][[{2}]],
																						temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {4,3}]]][[{1}]],
																						(-(extObj[[3, 4]] - extObj[[3, 1]])*chimneyWidth + #&/@(temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {4,3}]]]))[[{1}]]
																					]}
																			},
																			{}
																		]
																	},
																	
																	{
																		(* roof - direction 2 *)
																		{2, Join[extObj[[3, {2,3}]], {{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {2, 3}]]] - roofCenter)}]},
																		{1, Join[extObj[[3, {2,1}]], 
																			{{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {4, 1}]]] - roofCenter)},
																			{{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {2, 3}]]] - roofCenter)}]},
																		{3, Join[extObj[[3, {4,3}]], 
																			{{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {2, 3}]]] - roofCenter)},
																			{{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {4, 1}]]] - roofCenter)}]},
																		{4, Join[extObj[[3, {1,4}]], {{0, EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*roofHeight/2} + roofCenter + (1 - roofHipped)*(Mean[extObj[[3, {1, 4}]]] - roofCenter)}]},
																		
																		(* chimney *)
																		{8, Join[temp2[extObj[[3, {3,4}]]], 
																				temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {4,3}]]]
																			]},
																		{7, Join[temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {3,4}]]],
																				-(extObj[[3, 4]] - extObj[[3, 1]])*chimneyWidth + #&/@(temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {4,3}]]])
																			]},
																		{7, Join[temp2[extObj[[3, {3,4}]]][[{2}]],
																				temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {4,3}]]][[{1}]],
																				(-(extObj[[3, 4]] - extObj[[3, 1]])*chimneyWidth + #&/@(temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {4,3}]]]))[[{1}]]
																			]}
																	}
																]
														}],
														
														(* flat roof decoration *)
														extraRoofHeight = .2EuclideanDistance[Sequence@@extObj[[3, {1, 4}]]];
														temp = ScalingTransform[(.75EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]/EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]){1,1}, Mean[extObj[[3, {1,3}]]]];
														temp1 = ScalingTransform[(.33EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]/EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]){1,1}, Mean[extObj[[3, {1,3}]]] + {0, extraRoofHeight}];
														temp2 = ScalingTransform[(.66EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]/EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]){1,1}, Mean[extObj[[3, {1,3}]]]];
														temp3 = ScalingTransform[(.5EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]/EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]){1,1}, Mean[extObj[[3, {1,3}]]]];
														
														Which[
															
															finalHeightScaled <= roofQuantity + ((1 - roofQuantity)/3), (* (.../3) for 3 equal zones *)
															
																If[EuclideanDistance[Sequence@@extObj[[3, {1, 2}]]] > 2*extraRoofHeight, (* prevent RegionIntersection error *)
																	
																	(* roof top 1 *)
																	dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																	$dwP[[-1]] = temp[Join[extObj[[3, {2, 3}]], {RegionIntersection[Line[extObj[[3, {4, 3}]]], Line[# - {0, extraRoofHeight}&/@extObj[[3, {2, 3}]]]][[1,1]], extObj[[3, 2]] - {0, extraRoofHeight}}]];
																	Switch[buildingColorQuantityOutput,
																		"5SC",
																			style = ReplacePart[$dwFullDefaultStyle, buildingColor[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																		_,
																			style = ReplacePart[$dwFullDefaultStyle, buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																	];
																	style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																	$dwStyle[[-1]] = style;
																	
																	dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																	$dwP[[-1]] = temp[Join[extObj[[3, {1, 2}]], {extObj[[3, 2]] - {0, extraRoofHeight}}, {RegionIntersection[Line[extObj[[3, {1, 4}]]], Line[extObj[[3, {1, 2}]] - Table[{0, extraRoofHeight}, 2]]][[1,1]]}]];
																	Switch[buildingColorQuantityOutput,
																		"5SC",
																			style = ReplacePart[$dwFullDefaultStyle, buildingColor[[5]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																		_,
																			style = ReplacePart[$dwFullDefaultStyle, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																	];
																	style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																	style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																	style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																	$dwStyle[[-1]] = style;
																	
																	dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																	$dwP[[-1]] = temp[{
																			RegionIntersection[Line[extObj[[3, {1, 4}]]], Line[extObj[[3, {1, 2}]] - Table[{0, extraRoofHeight}, 2]]][[1,1]], 
																			extObj[[3, 2]] - {0, extraRoofHeight},
																			RegionIntersection[Line[extObj[[3, {4, 3}]]], Line[extObj[[3, {2, 3}]] - Table[{0, extraRoofHeight}, 2]]][[1,1]],
																			extObj[[3, 4]]
																			}];
																	Switch[buildingColorQuantityOutput,
																		"5SC",
																			style = ReplacePart[$dwFullDefaultStyle, buildingColor[[2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																		_,
																			style = ReplacePart[$dwFullDefaultStyle, roofColor, Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																	];
																	style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																	style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																	style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																	$dwStyle[[-1]] = style,
																	
																	Nothing
																],
																
															finalHeightScaled <= roofQuantity + 2((1 - roofQuantity)/3),
															
																(* roof top 2 *)
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp[Join[extObj[[3, {1, 4}]], Reverse[extObj[[3, {1, 4}]] + Table[{0, extraRoofHeight}, 2]]]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style;
																
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp[Join[extObj[[3, {4, 3}]], Reverse[extObj[[3, {4, 3}]] + Table[{0, extraRoofHeight}, 2]]]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[5]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style;
																
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp[extObj[[3]] + Table[{0, extraRoofHeight}, 4]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, Lighter[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style;
																
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp3[extObj[[3]] + Table[{0, 1.5extraRoofHeight}, 4]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style,
																
															True,
															
																(* roof top 3 - layer 1 *)
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp2[Join[extObj[[3, {1, 4}]], Reverse[extObj[[3, {1, 4}]] + Table[{0, 2extraRoofHeight}, 2]]]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style;
																
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp2[Join[extObj[[3, {4, 3}]], Reverse[extObj[[3, {4, 3}]] + Table[{0, 2extraRoofHeight}, 2]]]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[5]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style;
																
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp2[extObj[[3]] + Table[{0, 2extraRoofHeight}, 4]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, Lighter[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style;
															
																(* roof top 3 - layer 2 *)
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp1[Join[extObj[[3, {1, 4}]] + Table[{0, 2extraRoofHeight}, 2], Reverse[extObj[[3, {1, 4}]] + Table[{0, 5extraRoofHeight}, 2]]]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style;
																
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp1[Join[extObj[[3, {4, 3}]] + Table[{0, 2extraRoofHeight}, 2], Reverse[extObj[[3, {4, 3}]] + Table[{0, 5extraRoofHeight}, 2]]]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[5]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style;
																
																dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
																$dwP[[-1]] = temp1[extObj[[3]] + Table[{0, 5extraRoofHeight}, 4]];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[$dwFullDefaultStyle, buildingColor[[2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[$dwFullDefaultStyle, Lighter[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]]
																];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[1]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], .75contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style;
															
																(* roof top 3 - antenna *)
																dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
																$dwP[[-1]] = temp1[(Mean[extObj[[3, {1, 3}]]] + {0, #*extraRoofHeight})&/@{5, 10}];
																Switch[buildingColorQuantityOutput,
																	"5SC",
																		style = ReplacePart[style, buildingColor[[5]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
																		style = ReplacePart[style, CapForm["Round"], Position[style, CapForm[_]]];
																		style = ReplacePart[style, AbsoluteThickness[2], Position[style, AbsoluteThickness[_]]],
																	_,
																		style = ReplacePart[style, Darker[buildingColor[[Mod[n+bn, buildingColorQuantityOutput, 1]]], contrast], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
																		style = ReplacePart[style, CapForm["Round"], Position[style, CapForm[_]]];
																		style = ReplacePart[style, AbsoluteThickness[2], Position[style, AbsoluteThickness[_]]]
																];
																style = ReplacePart[style, buildingOpacityFill, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
																style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
																$dwStyle[[-1]] = style
														]
													
													];
													
													(* reset buildingColorQuantity *)
													buildingColorQuantityOutput = buildingColorQuantity = currentBuildingColorQuantity;
											],
										
											(* plant city trees *)
											If[Length[$dwCBbuildings[[n]]] > 1,
												
												(* object at center of each missing subdivided building *)
												dwAddTreesToCanvas[showTrees, If[MemberQ[Flatten[$dwCBriverFlow], $dwCBriverToBuildingArray[[n]]], True, False], seed, treeRandomSize, If[$dwCBbuildings[[n, bn, 1]] === Null, $dwCBbuildings[[n, bn, 2]], $dwCBbuildings[[n, bn]]], buildingColorQuantity, waterColor, concreteColor, streetColor, streetCenterlineColor, 
													buildingColor, barkColor, grassColor, soilColor, contrast, buildingOpacityFill, buildingOpacityStroke, treeSize, treeTrunk, treeTrunkDark, tree, treeDark, treeLight, n, bn, 
													(.5/(Max[rows,columns]^2)^.47), $dwCBcityTreeIndexList, If[MemberQ[$dwCBcityTreeIndexList, n], cityObjCtr++, cityObjCtr], "Subdivided"->True],
												
												(* object at center of each missing non-subdivided building *)
												dwAddTreesToCanvas[showTrees, If[MemberQ[Flatten[$dwCBriverFlow], $dwCBriverToBuildingArray[[n]]], True, False], seed, treeRandomSize, If[$dwCBbuildings[[n, bn, 1]] === Null, $dwCBbuildings[[n, bn, 2]], $dwCBbuildings[[n, bn]]], buildingColorQuantity, waterColor, concreteColor, streetColor, streetCenterlineColor, 
													buildingColor, barkColor, grassColor, soilColor, contrast, buildingOpacityFill, buildingOpacityStroke, treeSize, treeTrunk, treeTrunkDark, tree, treeDark, treeLight, n, bn, 
													(.5/(Max[rows,columns]^2)^.47), $dwCBcityTreeIndexList, If[MemberQ[$dwCBcityTreeIndexList, n], cityObjCtr++, cityObjCtr], "Subdivided"->False]
											]
										],
									{bn, Length[$dwCBbuildings[[n]]]}],
								{n, Length[$dwCBbuildings]}],
								Nothing
							];
							
									
							(* plant trees in front for rough edge areas *)
							If[showTrees && edgeTrees != 0,
								If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
									{},
									
									If[riverTurns == 0 && (exitRoads === {} || (!showStreets || DeleteDuplicates[$dwCBstreets] === {Null})),
							
										temp = Sort[SeedRandom[seed];RandomPoint[Polygon[$dwCBlandPtsNoRural[[;;2columns + 2]]], IntegerPart[columns*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&];
										temp1 = Sort[SeedRandom[seed];RandomPoint[Polygon[Join[$dwCBlandPtsNoRural[[4columns + 2rows + 2;;-1]], $dwCBlandPtsNoRural[[{1}]]]], IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&],
										
										temp = Quiet[BooleanRegion[
													And[#1, Not[#2]] &, 
													{
														Polygon[$dwCBlandPtsNoRural[[;;2columns + 2]]],
														RegionUnion[Sequence@@Table[Polygon[sw], {sw, sidewalks}], Polygon[$dwCBriverFinalPts]] 
													}
												]];
										temp = If[Head[temp] === BooleanRegion,
											Region[Polygon[$dwCBlandPtsNoRural[[;;2columns + 2]]]],
											temp
										];
										temp = Sort[SeedRandom[seed];RandomPoint[temp, IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&];
											
										temp1 = Quiet[BooleanRegion[
													And[#1, Not[#2]] &, 
													{
														Polygon[Join[$dwCBlandPtsNoRural[[4columns + 2rows + 2;;-1]], $dwCBlandPtsNoRural[[{1}]]]],
														RegionUnion[Sequence@@Table[Polygon[sw], {sw, sidewalks}], Polygon[$dwCBriverFinalPts]] 
													}
												]];
										temp1 = If[Head[temp1] === BooleanRegion,
											Region[Polygon[Join[$dwCBlandPtsNoRural[[4columns + 2rows + 2;;-1]], $dwCBlandPtsNoRural[[{1}]]]]],
											temp1
										];
										temp1 = Sort[SeedRandom[seed];RandomPoint[temp1, IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&]
									];
									Do[
										Do[
											SeedRandom[seed+$dwCBrandomTreeCtr++];
											randomReal = RandomReal[treeRandomSize];
											{
												(* tree trunk *)
												dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
												$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeTrunk;
												style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[barkColor, .1randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
												style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,.5randomReal]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
												style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style,
												
												(* tree trunk dark *)
												dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
												$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeTrunkDark;
												style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,contrast*randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
												style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style,
												
												(* tree leaves *)
												dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
												$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@tree;
												style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
												style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style,
												
												dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
												$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeDark;
												style = ReplacePart[$dwFullDefaultStyle, Black, Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
												style = ReplacePart[style, If[buildingColorQuantity === "5SC", .25, .2+.33contrast^2], Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
												style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style,
												
												dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
												$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeLight;
												style = ReplacePart[$dwFullDefaultStyle, White, Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
												style = ReplacePart[style, .35, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
												style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style,
												
												dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
												$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@tree;
												style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
												style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor, contrast+.05]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
												style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
												$dwStyle[[-1]] = style
											}, 
										{treeLoc, tr}],
									{tr, {temp, temp1}}]
								],
								{}
							];
							
							(* birds *)
							birdCount = 2*$dwCBtreeCount;
							SeedRandom[seed];
							If[MemberQ[{birdSize, birdCount}, 0|0.],
								Nothing,
								temp = {
									dwAxo[.75$dwCBscaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{25,"Top"}}],
									dwAxo[.75$dwCBscaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{75,"Top"}}],
									dwAxo[.75$dwCBscaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{115,"Top"}}],
									dwAxo[$dwCBscaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{25,"Top"}}],
									dwAxo[$dwCBscaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{75,"Top"}}],
									dwAxo[$dwCBscaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{115,"Top"}}]
								};
								birdVariations = Length[temp];
								birdList = If[birdCount >= birdVariations^2,
									Partition[Riffle[Range[birdVariations], Partition[RandomPoint[Polygon[({0, Max[({1,.66}*$dwCBminMaxHeight)[[RandomInteger[{1,2}]]], .1]} + #)&/@$dwCBlandPtsNoRural], birdCount], IntegerPart[birdCount/birdVariations]]],2],
									{}
								];
								If[Length[Flatten[#[[2]]&/@birdList]] >= birdVariations,
									Do[
										Do[
											dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
											$dwP[[-1]] = (p1 + #)&/@temp[[p2[[1]]]];
											style = ReplacePart[$dwFullDefaultStyle, 
												Switch[buildingColorQuantity,
													"5SC", buildingColor[[5]],
													_, birdColor
												], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
											style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
											$dwStyle[[-1]] = style, 
										{p1, p2[[2]]}],
									{p2, birdList}]
								]
							];
							
							dwUpdateBoundingBox[Range[startLength+1, Length[$dwP]]];
							$dwPointQuantity = Length[Flatten[$dwP, 1]];
							$dwGroupLayers = Join[$dwGroupLayers, {Range[startLength+1, Length[$dwP]]}];
							$dwSelected = $dwGroupLayers[[-1]];
							$dwStyleMode = "fill";
							dwSetGridSize[];
							$dwSynchronousUpdating = Automatic; (* return state *)
							DialogReturn[],
							Method -> "Queued" (* to avoid timeout with large graphics *)
						]
				}}], SpanFromLeft}}, Alignment->Top]}
			}, Alignment->Top],
		ImageSize->973]],
	Background->LightGray, WindowTitle->"City builder", Modal->False]

End[] (* End Private Context *)

EndPackage[]