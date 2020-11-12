(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(* plotRange must be {{-1,1},{-1,1}} for locating character but not final *)
(* colors for grassColor and concreteColor must not not used by any other objects - trees use altered version of grassColor which is okay *)
dwCityBuilderHiddenItemMap[map_, rows_, columns_, imageSize_, showBuildings_, grassColor_, concreteColor_, backgroundColor_, yardSize_, buildingColor_, buildingColorQuantity_]:=
	Block[{p, scaleAll = $dwCBscaleAll, itemSize = yardSize/5, itemList, hiddenAreaPad = .5, legendboxsize, legendboxspace, legenditemspace, imagePositions},

		p = If[!showBuildings,
			
			{},
			
			imagePositions = ImageAdjust[Rasterize[
				Show[
					map/.{
						If[buildingColorQuantity === "5SC" && FreeQ[buildingColor[[{1,2,4,5}]], buildingColor[[3]]], buildingColor[[3]], grassColor]->Black, 
						If[buildingColorQuantity === "5SC" && FreeQ[buildingColor[[{1,2,4,5}]], buildingColor[[3]]], buildingColor[[3]], concreteColor]->Black,
						GrayLevel[_]->White, Hue[___]->White, RGBColor[___]->White, LABColor[___]->White, 
						LCHColor[___]->White, LUVColor[___]->White, CMYKColor[___]->White, XYZColor[___]->White
					},
					Graphics[{White, 
						Polygon[Join[
							yardSize[[1]]*scaleAll*dwAxo[{{-columns/2+hiddenAreaPad, -rows/2+hiddenAreaPad}, {columns/2-hiddenAreaPad, -rows/2+hiddenAreaPad}, {columns/2-hiddenAreaPad, rows/2-hiddenAreaPad}, {-columns/2+hiddenAreaPad, rows/2-hiddenAreaPad}, {-columns/2+hiddenAreaPad, -rows/2+hiddenAreaPad}}, 5, "Tilt" -> $dwTilt, "Turn" -> $dwTurn, "Direction" -> "Top"],
							{{0, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}, {0, -1}}
						]]
					}]
				], ImageSize->imageSize, ImageResolution->$dwImageResolution], {0,1}];
			(* increase dilation to remove small islands and center in space but it reduces possible locations *)
			{-1,1} + {-1,1}(2/imageSize)*Reverse[#]&/@(
				-Position[
					ImageData[Dilation[imagePositions, .15Sqrt[imageSize]]], 
					{0.,0.,0.}, Infinity
			])
		];
		
			
		If[p === {} || !showBuildings,
			
			Graphics[map[[1]], Background->backgroundColor, ImageSize->imageSize, PlotRange->All],
			
			(* add hidden item *)
			itemList = Cases[Import[$dwFileDirectory<>"Media/cityBuilderHiddenItem.nb"], _GraphicsBox, Infinity]/.{(ImageSize->___)->Sequence[], (PlotRange->___)->Sequence[]};
			itemList = Flatten[Table[Table[itemList[[n]], Max[IntegerPart[Sqrt[rows*columns]/Min[Length[p], Length[itemList]]], 1]], {n, Min[Length[itemList], IntegerPart[Sqrt[rows*columns]]]}]];
			If[Length[p] > 1,
				p = #[[RandomInteger[{1, Length[#]}]]]&/@FindClusters[p, Length[itemList], PerformanceGoal->"Speed"];
				p = If[Length[p] > Length[itemList], p[[;;Length[itemList]]], p]
			];
			Show[
				map,
				
				Graphics[
					(Table[
						Inset[Graphics@@itemList[[n]], p[[n]], ImageScaled[{.5, 0}], scaleAll*itemSize],
					{n, Length[p]}])
				], 
				
				legenditemspace = 20;
				legendboxsize = {Max[(Length[p] + 1)*legenditemspace, 90], 60};
				legendboxspace = 5;
				Graphics[GraphicsGroup[{
					EdgeForm[{GrayLevel[0,.5]}], GrayLevel[1,.5],
					Rectangle[Offset[{-legendboxsize[[1]] - legendboxspace, legendboxspace}, ImageScaled[{1, 0}]], Offset[{-legendboxspace, legendboxsize[[2]] + legendboxspace}, ImageScaled[{1, 0}]], RoundingRadius -> Offset[6]],
					GrayLevel[0,1], Text["HIDDEN ITEMS", Offset[{-(legendboxsize[[1]]/2) - legendboxspace, legendboxsize[[2]] + legendboxspace - 5}, ImageScaled[{1, 0}]], {0,1}],
					Table[Inset[Graphics@@itemList[[n]], Offset[{-(legendboxsize[[1]] + legenditemspace*Length[p])/2 + legenditemspace*(n-1) + .5legenditemspace - legendboxspace, (legendboxsize[[2]]/2) + legendboxspace - 5}, ImageScaled[{1, 0}]], ImageScaled[{.5, .5}], Offset[1.5legenditemspace]], {n, Length[p]}]
				}]],
					
				ImageSize->imageSize, PlotRange->All
			]
		]
	]
	
Options[dwCityBuilderExpand]={"Preview"->False, 
	"Rows"->3, "Columns"->3, "ImageSize"->300, "FinalScale"->1, 
	"PreviewRows"->3, "PreviewColumns"->3, "CurrentBuildingPattern"->"Default", "BuildingColorQuantity"->5, "AngleH"->0, "AngleV"->0, 
	"BackgroundColor"->White, "BarkColor"->Hue[.1, 1, .7], "BedrockColor"->Hue[.1, .2, .65], "BedrockLayer"->0, "BirdColor"->GrayLevel[.3], 
	"BirdShape"->.03{{-0.35, 0.05}, {-0.2, 0.1}, {0.2, 0.5}, {0.05, 0.1}, {0.6, 0.}, {0.05, -0.1}, {0.2, -0.5}, {-0.2, -0.1}, {-0.35, -0.05}, {-0.35, 0.05}}, 
	"BirdSize"->1, 
	"BuildingColor"->{Hue[.12, .2, .7], Hue[.1, 1, .8], Hue[.06, 1, .8], Hue[1/12, 1, 1], Hue[.12, 1, 1]},
	"BuildingOpacityFill"->1, "BuildingOpacityStroke"->1,  
	"BuildingRemoval"->.4, "CityObjects"->.5, "ColorByHeight"->True, "ConcreteColor"->GrayLevel[.8], "ConcreteSize"->{0, 0}, 
	"Contrast"->.6, "CurbThickness"->.5, "DoorAwningDepth"->42, "DoorHeight"->6, "DoorLimit"->.12, "DoorWidth"->24, 
	"EdgeTrees"->1, "ExitRoads"->{}, "FlipHeight"->0, "GrassColor"->Hue[.25, .7, .7], "InvertHeightColor"->False, "MaxBuildingHeight"->10, "MinBuildingHeight"->.15, 
	"LandEdgeRoughness"->1, "LandEdgeLimit"->2, "OverallTreeSize"->1, "RandomHeight"->.25, "RandomScale"->{.25,.25}, "RandomTreeStart"->0, 
	"RemoveBlockedStreets"->True, "ReverseHeight"->False, "RiverTurns"->4, 
	"RockColor"->Hue[.1, .3, .75], "RockLayer"->0, "RoofColor"->GrayLevel[.5], "RoofHeight"->.5, "RoofHipped"->.5, "RoofQuantity"->.65, 
	"RuralArea"->.05, "RuralTreeBalance"->.1, "RuralTreeCount"->7500, "ScaleByHeight"->.5, "ScaleEach"->{.5,.6}, "ScaleHeight"->.325, 
	"Seed"->22, 
	"ShapePts"->{{-0.2, 0.2}, {0.2, 0.2}, {0.2, -0.2}, {-0.2, -0.2}}, "ShowBuildings"->True, "ShowEnvironment"->True, "ShowStreets"->True, "ShowTrees"->True, "ShowWindows"->True, 
	"SidewalkWidth"->.4, "WaterColor"->Hue[.6, .5, 1], "SoilColor"->Hue[.1, 1, .7], "SoilLayer"->.025, 
	"StreetCenterlineColor"->LightGray, "StreetCenterlineDash"->{3, 3}, "StreetCenterlineOpacity"->1, "StreetColor"->GrayLevel[.4], "StreetCurbColor"->Black, "StreetPad"->0, "StreetThickness"->.025, 
	"SubdivideDistanceOrigin"->{}, "SubdivideSize"->.5, "SubdivideScale"->.85, "SubsoilColor"->Hue[.1, .6, .85], "SubsoilLayer"->0, "Traffic"->1,
	"Tree"->{{-0.3,0.4},{-0.45,0.45},{-0.55,0.55},{-0.7,0.6},{-0.85,0.75},{-0.8,0.95},{-0.7,1.1},{-0.75,1.25},{-0.65,1.35},{-0.6,1.45},{-0.6,1.6},{-0.4,1.7},{-0.3,1.8},{-0.05,1.9},{0.2,1.9},{0.35,1.8},{0.5,1.75},{0.6,1.6},{0.7,1.5},{0.7,1.3},{0.8,1.1},{0.8,0.95},{0.7,0.8},{0.75,0.7},{0.75,0.6},{0.6,0.45},{0.4,0.4},{0.3,0.45},{0.2,0.5},{0.15,0.5},{0.1,0.5},{0.,0.5},{-0.1,0.5},{-0.2,0.45},{-0.3,0.4},{-0.3,0.4}}, 
	"TreeDark"->{{-0.65,0.75},{-0.45,0.7},{-0.3,0.6},{-0.15,0.65},{0.,0.75},{0.15,0.7},{0.3,0.7},{0.4,0.8},{0.4,0.9},{0.5,1},{0.6,1.},{0.65,1.05},{0.6,1.1},{0.55,1.2},{0.45,1.25},{0.35,1.35},{0.5,1.4},{0.55,1.5},{0.45,1.65},{0.25,1.8},{0.2,1.9},{0.35,1.8},{0.5,1.75},{0.6,1.6},{0.7,1.5},{0.7,1.3},{0.8,1.1},{0.8,0.95},{0.7,0.8},{0.75,0.7},{0.75,0.6},{0.6,0.45},{0.4,0.4},{0.3,0.45},{0.2,0.5},{0.05,0.5},{-0.1,0.5},{-0.2,0.45},{-0.3,0.4},{-0.45,0.45},{-0.55,0.55},{-0.7,0.6},{-0.85,0.75},{-0.8,0.95},{-0.7,1.1},{-0.75,0.9}}, 
	"TreeLight"->{{-0.6,1.6},{-0.4,1.7},{-0.3,1.8},{-0.05,1.9},{0.2,1.9},{0.15,1.85},{0.,1.8},{0.05,1.75},{0.1,1.7},{-0.05,1.7},{-0.2,1.6},{-0.35,1.55},{-0.25,1.45},{-0.1,1.4},{0.05,1.45},{0.2,1.3},{0.1,1.1},{0.05,1.2},{-0.05,1.15},{-0.2,1.2},{-0.35,1.15},{-0.4,1.},{-0.55,1.05},{-0.65,0.95},{-0.65,0.75},{-0.75,0.9},{-0.7,1.1},{-0.75,1.25},{-0.65,1.35},{-0.6,1.45}}, 
	"TreeRandomSize"->{.75,1.25}, 
	"TreeTrunk"->{{-0.1,0.5},{-0.1,0.3},{-0.15,0.},{-0.1,-0.05},{0.,-0.1},{0.1,-0.05},{0.15,0.},{0.1,0.3},{0.1,0.5}}, 
	"TreeTrunkDark"->{{-0.1,0.5},{0.,0.4},{0.05,0.05},{-0.05,-0.05},{-0.15,0.},{-0.1,-0.05},{0.,-0.1},{0.1,-0.05},{0.15,0.},{0.1,0.3},{0.1,0.5}}, 
	"WindowColor"->Hue[.125,.5,1], "WindowSize"->.5, "WindowStripes"->True, "YardSize"->{.3, .3}
	
}

dwCityBuilderExpand[OptionsPattern[]]:=
	Block[{
			map, buildingColorQuantity2, scaleByHeightFunction, scaleByHeightPts, 
			land, temp, temp1, temp2, temp3, extrude, buildingsAndHeight, extObj, finalLandPtsNoRural, landPtsNoRural, landNoRural,
			landPts, roofCenter, extraRoofHeight, concretePts, finalHeight, finalHeightScaled, currentBuildingColorQuantity, sidewalks, 
			extraLineSpace, windows, randomTreeCtr, randomReal, birdList, birdCount, birdVariations, treeLoc, cityTreeIndexList, 
			treeSize = .05, cityObjCtr, minmax, chimneyWidth = .125, carsize, topview1, topview2, 
			riverFinalPts, riverToBuildingArray, riverTurns, riverTurnPos, riverPts, startRiver, startRiverPtNum, riverFlow = {}, startRiverEdgePt, endRiverEdgePt, 
			
			minMaxHeight, streets, buildings, finalLandPts, heightSize, heightPattern, scaleAll, buildingCount, windowCount, flatRoof1Count, flatRoof2Count, flatRoof3Count, roofCount, lineCount, doorCount, treeCount, treeObjCount, overallBirdCount, extraRoofCount,
			
			preview, rows, columns, imageSize, finalScale, previewRows, previewColumns, currentBuildingPattern, buildingColorQuantity, angleH, angleV, 
			backgroundColor, barkColor, bedrockColor, bedrockLayer, birdColor, birdShape, birdSize, buildingColor, buildingOpacityFill, buildingOpacityStroke, buildingRemoval, cityObjects, colorByHeight, concreteColor, concreteSize, 
			contrast, curbThickness, doorAwningDepth, doorHeight, doorLimit, doorWidth, edgeTrees, exitRoads, grassColor, invertHeightColor, maxBuildingHeight, minBuildingHeight, 
			landEdgeRoughness, landEdgeLimit, overallTreeSize, randomHeight, randomScale, randomTreeStart, removeBlockedStreets, reverseHeight, 
			flipHeight, rockColor, rockLayer, roofColor, roofHeight, roofHipped, roofQuantity, ruralArea, ruralTreeBalance, ruralTreeCount, scaleByHeight, scaleEach, scaleHeight, 
			seed, shapePts, showBuildings, showEnvironment, showStreets, showTrees, showWindows, sidewalkWidth, waterColor, soilColor, soilLayer, streetCenterlineColor, 
			streetCenterlineDash, streetCenterlineOpacity, streetColor, streetCurbColor, streetPad, streetThickness, subdivideDistanceOrigin, subdivideSize, subdivideScale, 
			subsoilColor, subsoilLayer, traffic, tree, treeDark, treeLight, treeRandomSize, treeTrunk, treeTrunkDark, windowColor, windowSize, windowStripes, yardSize
		},
		
		{
			preview, rows, columns, imageSize, finalScale, previewRows, previewColumns, currentBuildingPattern, buildingColorQuantity, angleH, angleV, 
			backgroundColor, barkColor, bedrockColor, bedrockLayer, birdColor, birdShape, birdSize, buildingColor, buildingOpacityFill, buildingOpacityStroke, buildingRemoval, cityObjects, colorByHeight, concreteColor, concreteSize, 
			contrast, curbThickness, doorAwningDepth, doorHeight, doorLimit, doorWidth, edgeTrees, exitRoads, flipHeight, grassColor, invertHeightColor, 
			maxBuildingHeight, minBuildingHeight, landEdgeRoughness, landEdgeLimit, overallTreeSize, randomHeight, randomScale, randomTreeStart, removeBlockedStreets, reverseHeight, 
			riverTurns, rockColor, rockLayer, roofColor, roofHeight, roofHipped, roofQuantity, ruralArea, ruralTreeBalance, ruralTreeCount, scaleByHeight, scaleEach, scaleHeight, 
			seed, shapePts, showBuildings, showEnvironment, showStreets, showTrees, showWindows, sidewalkWidth, waterColor, soilColor, soilLayer, streetCenterlineColor, 
			streetCenterlineDash, streetCenterlineOpacity, streetColor, streetCurbColor, streetPad, streetThickness, subdivideDistanceOrigin, subdivideSize, subdivideScale, 
			subsoilColor, subsoilLayer, traffic, tree, treeDark, treeLight, treeRandomSize, treeTrunk, treeTrunkDark, windowColor, windowSize, windowStripes, yardSize
		} = OptionValue[{"Preview", "Rows", "Columns", "ImageSize", "FinalScale", "PreviewRows", "PreviewColumns", "CurrentBuildingPattern", "BuildingColorQuantity", 
				"AngleH", "AngleV", "BackgroundColor", "BarkColor", "BedrockColor", "BedrockLayer", "BirdColor", "BirdShape", "BirdSize", "BuildingColor", "BuildingOpacityFill", "BuildingOpacityStroke", "BuildingRemoval", "CityObjects", 
				"ColorByHeight", "ConcreteColor", "ConcreteSize", "Contrast", "CurbThickness", "DoorAwningDepth", "DoorHeight", "DoorLimit", "DoorWidth", "EdgeTrees", "ExitRoads", 
				"FlipHeight", "GrassColor", "InvertHeightColor", "MaxBuildingHeight", "MinBuildingHeight", "LandEdgeRoughness", "LandEdgeLimit", "OverallTreeSize", 
				"RandomHeight", "RandomScale", "RandomTreeStart", "RemoveBlockedStreets", "ReverseHeight", "RiverTurns", "RockColor", "RockLayer", 
				"RoofColor", "RoofHeight", "RoofHipped", "RoofQuantity", "RuralArea", "RuralTreeBalance", "RuralTreeCount", "ScaleByHeight", "ScaleEach", "ScaleHeight", 
				"Seed", "ShapePts", "ShowBuildings", "ShowEnvironment", "ShowStreets", "ShowTrees", "ShowWindows", "SidewalkWidth", "WaterColor", 
				"SoilColor", "SoilLayer", "StreetCenterlineColor", "StreetCenterlineDash", "StreetCenterlineOpacity", "StreetColor", "StreetCurbColor", 
				"StreetPad", "StreetThickness", "SubdivideDistanceOrigin", "SubdivideSize", "SubdivideScale", "SubsoilColor", "SubsoilLayer", 
				"Traffic", "Tree", "TreeDark", "TreeLight", "TreeRandomSize", "TreeTrunk", "TreeTrunkDark", "WindowColor", "WindowSize", "WindowStripes", "YardSize"
			}];
			
		{
			minMaxHeight, streets, buildings, finalLandPts, heightSize, heightPattern, scaleAll, riverFlow, riverFinalPts, riverToBuildingArray, buildingCount, windowCount, flatRoof1Count, 
			flatRoof2Count, flatRoof3Count, roofCount, lineCount, doorCount, treeCount, treeObjCount, overallBirdCount, extraRoofCount
		} = 
		{
			$dwCBminMaxHeight, $dwCBstreets, $dwCBbuildings, $dwCBfinalLandPts, $dwCBheightSize, $dwCBheightPattern, $dwCBscaleAll, $dwCBriverFlow, $dwCBriverFinalPts, $dwCBriverToBuildingArray, 
			$dwCBbuildingCount, $dwCBwindowCount, $dwCBflatRoof1Count, $dwCBflatRoof2Count, $dwCBflatRoof3Count, $dwCBroofCount, $dwCBlineCount, $dwCBdoorCount, $dwCBtreeCount, $dwCBtreeObjCount, $dwCBoverallBirdCount, $dwCBextraRoofCount
		};
		
		heightSize = {1};
		scaleAll = 2;
		buildingColorQuantity2 = buildingColorQuantity;
		heightPattern = dwCityHeightPatternFunction[currentBuildingPattern, rows, columns, seed];
		
		map = Graphics[{
				
			(* update values *)
			randomTreeCtr = randomTreeStart;
			cityObjCtr = 0;
			buildingCount = windowCount = flatRoof1Count = flatRoof2Count = flatRoof3Count = roofCount = lineCount = doorCount = treeCount = treeObjCount = overallBirdCount = extraRoofCount = 0;
			scaleAll = finalScale/(.5 Max[Reverse[yardSize]*{rows, columns}] + ruralArea);
			land = If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
				dwCityLand[scaleAll, yardSize, rows, columns, ruralArea],
				dwCityLandRough[scaleAll, yardSize, rows, columns, landEdgeRoughness, seed, ruralArea]
			];
			landNoRural = If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
				dwCityLand[scaleAll, yardSize, rows, columns, 0],
				dwCityLandRough[scaleAll, yardSize, rows, columns, landEdgeRoughness, seed, 0]
			];
			finalLandPtsNoRural = 
				If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
					landNoRural,
					Flatten[
						Table[
							Which[
								lp == 1,
									{landNoRural[[lp]], landNoRural[[lp]]},
								lp == columns + 1,
									{landNoRural[[lp]], landNoRural[[lp]]},
								lp == columns + rows + 1,
									{landNoRural[[lp]], landNoRural[[lp]]},
								lp == 2columns + rows + 1,
									{landNoRural[[lp]], landNoRural[[lp]]},
								landNoRural[[lp, 1]] >= (yardSize[[1]]*scaleAll*columns)/2,
									{landNoRural[[lp]] - {0, scaleAll*4.5*sidewalkWidth*streetThickness}, landNoRural[[lp]] + {0, scaleAll*4.5*sidewalkWidth*streetThickness}}, 
								landNoRural[[lp, 1]] <= -(yardSize[[1]]*scaleAll*columns)/2,
									{landNoRural[[lp]] + {0, scaleAll*4.5*sidewalkWidth*streetThickness}, landNoRural[[lp]] - {0, scaleAll*4.5*sidewalkWidth*streetThickness}},
								landNoRural[[lp, 2]] >= (yardSize[[2]]*scaleAll*rows)/2,
									{landNoRural[[lp]] + {scaleAll*4.5*sidewalkWidth*streetThickness, 0}, landNoRural[[lp]] - {scaleAll*4.5*sidewalkWidth*streetThickness, 0}},
								landNoRural[[lp, 2]] <= -(yardSize[[2]]*scaleAll*rows)/2,
									{landNoRural[[lp]] - {scaleAll*4.5*sidewalkWidth*streetThickness, 0}, landNoRural[[lp]] + {scaleAll*4.5*sidewalkWidth*streetThickness, 0}},
								True,
									{landNoRural[[lp]], landNoRural[[lp]]}
							], 
						{lp, Length[landNoRural]}],
					1]
				];
			finalLandPts = 
				If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
					land,
					Flatten[
						Table[
							Which[
								lp == 1,
									{land[[lp]], land[[lp]]} - .5{scaleAll*ruralArea, scaleAll*ruralArea},
								lp == columns + 1,
									{land[[lp]] + .5{scaleAll*ruralArea, -scaleAll*ruralArea}, land[[lp]] + .5{scaleAll*ruralArea, -scaleAll*ruralArea}} ,
								lp == columns + rows + 1,
									{land[[lp]], land[[lp]]} + .5{scaleAll*ruralArea, scaleAll*ruralArea},
								lp == 2columns + rows + 1,
									{land[[lp]] + .5{-scaleAll*ruralArea, scaleAll*ruralArea}, land[[lp]] + .5{-scaleAll*ruralArea, scaleAll*ruralArea}},
								land[[lp, 1]] >= (yardSize[[1]]*scaleAll*columns)/2,
									{land[[lp]] - {0, scaleAll*4.5*sidewalkWidth*streetThickness} + {scaleAll*ruralArea, 0}, land[[lp]] + {0, scaleAll*4.5*sidewalkWidth*streetThickness} + {scaleAll*ruralArea, 0}}, 
								land[[lp, 1]] <= -(yardSize[[1]]*scaleAll*columns)/2,
									{land[[lp]] + {0, scaleAll*4.5*sidewalkWidth*streetThickness} - {scaleAll*ruralArea, 0}, land[[lp]] - {0, scaleAll*4.5*sidewalkWidth*streetThickness} - {scaleAll*ruralArea, 0}},
								land[[lp, 2]] >= (yardSize[[2]]*scaleAll*rows)/2,
									{land[[lp]] + {scaleAll*4.5*sidewalkWidth*streetThickness, 0} + {0, scaleAll*ruralArea}, land[[lp]] - {scaleAll*4.5*sidewalkWidth*streetThickness, 0} + {0, scaleAll*ruralArea}},
								land[[lp, 2]] <= -(yardSize[[2]]*scaleAll*rows)/2,
									{land[[lp]] - {scaleAll*4.5*sidewalkWidth*streetThickness, 0} - {0, scaleAll*ruralArea}, land[[lp]] + {scaleAll*4.5*sidewalkWidth*streetThickness, 0} - {0, scaleAll*ruralArea}},
								True,
									{land[[lp]], land[[lp]]}
							], 
						{lp, Length[land]}],
					1]
				];
			buildingsAndHeight = dwCityBuildings[scaleAll, subdivideSize, shapePts, scaleEach, yardSize, seed, randomScale, rows, columns, subdivideScale, subdivideDistanceOrigin, reverseHeight, heightPattern, flipHeight];
			buildings = dwRemoveBuildings[buildingsAndHeight[[1]], buildingRemoval, seed];
			cityTreeIndexList = Complement[#[[1]]&/@Cases[Position[buildings, Null, Infinity], {_, 1, 1}], #[[1]]&/@Position[Length[#]&/@buildings, 4]];
			cityTreeIndexList = RandomSample[cityTreeIndexList, Length[cityTreeIndexList]][[;;IntegerPart[cityObjects*Length[cityTreeIndexList]]]];
			heightSize = buildingsAndHeight[[2]];
			minMaxHeight = 
				(	
					MinMax[Flatten[
						Table[
							extrude = (SeedRandom[n+seed];RandomReal[{randomHeight, 1}]*scaleAll*scaleHeight*heightSize[[Mod[n, Length[heightSize], 1]]]);
							Table[
								Min[Max[(SeedRandom[n+bn+seed];RandomReal[{randomHeight, 1}]*extrude), minBuildingHeight], maxBuildingHeight],
							{bn, Length[buildings[[n]]]}],
						{n, Length[buildings]}]
					]]
				);
			streets = dwCityStreets[scaleAll, showStreets, streetThickness, yardSize, rows, columns, ruralArea, exitRoads, land, landEdgeRoughness, landEdgeLimit];
			streets = If[showBuildings && removeBlockedStreets,
					dwRemoveStreets[buildings, streets, shapePts, scaleEach, yardSize, seed, randomScale, rows, columns, streetPad, buildingRemoval],
					streets
				];
			landPts = dwAxo[{finalLandPts}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
			landPtsNoRural = dwAxo[{finalLandPtsNoRural}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
			concretePts = {(1.25*scaleAll*{Min[concreteSize[[1]],columns],Min[concreteSize[[2]],rows]}*(2*yardSize)*#)&/@(shapePts)};
			treeSize = overallTreeSize*(.5/(Max[rows,columns]^2)^.5);
			
			(* environment - below axes *)
			If[showEnvironment,
				GraphicsGroup[{
					If[#[[1, -1]] > 0,
						If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
							{	temp1 = -scaleAll*Total[#[[1]][[;;-2]]];
								temp2 = -scaleAll*Total[#[[1]]];
								#[[2]], Polygon[{{0, temp2}, {0, temp2}, {0, temp1}, {0, temp1}} + landPts[[{2,1,1,2}]]],
								Darker[#[[2]], .5 contrast], Polygon[{{0, temp2}, {0, temp2}, {0, temp1}, {0, temp1}} + landPts[[{1,4,4,1}]]]
							},
							{	temp1 = -scaleAll*Total[#[[1]][[;;-2]]];
								temp2 = -scaleAll*Total[#[[1]]];
								#[[2]], 
								Sequence@@Table[
									Polygon[Join[Table[{0, temp2}, 2] + soilPts, Table[{0, temp1}, 2] + Reverse@soilPts]],
									{soilPts, Partition[Join[landPts[[IntegerPart[Length[landPts]/2];;-1]], landPts[[{1}]]], 2, 1]}],
								Darker[#[[2]], .5 contrast], 
								Sequence@@Table[
									Polygon[Join[Table[{0, temp2}, 2] + soilPts, Table[{0, temp1}, 2] + Reverse@soilPts]],
									{soilPts, Partition[landPts[[;;IntegerPart[Length[landPts]/2]]], 2, 1]}]
							}
						],
						{}
					]&/@{
							{{0, soilLayer, subsoilLayer, rockLayer, bedrockLayer},
								Switch[buildingColorQuantity2,
									"5SC", buildingColor[[4]],
									_, bedrockColor
								]},
							{{0, soilLayer, subsoilLayer, rockLayer},
								Switch[buildingColorQuantity2,
									"5SC", buildingColor[[4]],
									_, rockColor
								]},
							{{0, soilLayer, subsoilLayer},
								Switch[buildingColorQuantity2,
									"5SC", buildingColor[[4]],
									_, subsoilColor
								]},
							{{0, soilLayer}, 
								Switch[buildingColorQuantity2,
									"5SC", buildingColor[[4]],
									_, soilColor
								]}
						}
				}],
				{}
			],
			
			(* axes - displays below grass but above ground layers *)
			If[preview,
				{
					{White, Thick, Line[1.5finalScale{Cos[angleH/180*Pi], Sin[angleH/180*Pi]}#&/@{{.5, .5}, .95{1, 1}}], Line[1.5finalScale{Cos[(angleV+90)/180*Pi], Sin[(angleV+90)/180*Pi]}#&/@{{.5, .5}, .95{1, 1}}]},
					Gray, Line[1.5finalScale{Cos[angleH/180*Pi], Sin[angleH/180*Pi]}#&/@{{.5, .5}, .95{1, 1}}], Line[1.5finalScale{Cos[(angleV+90)/180*Pi], Sin[(angleV+90)/180*Pi]}#&/@{{.5, .5}, .95{1, 1}}],
					GrayLevel[.6], Text[Style["x", 18, Bold], 1.5finalScale{Cos[angleH/180*Pi], Sin[angleH/180*Pi]}],
					Text[Style["y", 18, Bold], 1.5finalScale{Cos[(angleV+90)/180*Pi], Sin[(angleV+90)/180*Pi]}]
				},
				{}
			],
			
			(* environment - above axes *)
			If[showEnvironment,
				{
					Switch[buildingColorQuantity2,
							"5SC", buildingColor[[3]],
							_, grassColor
					],
					Polygon[landPts],
					
					If[FreeQ[concreteSize, 0],
						{
							Switch[buildingColorQuantity2,
								"5SC", buildingColor[[3]],
								_, concreteColor
							], 
							Polygon[dwAxo[concretePts, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						},
						{}
					]
				},
				{}
			],
			
			(* river *)
			If[showEnvironment,
				If[riverTurns > 0 && (rows > 2 && columns > 2),
					
					lineCount += 3;
					riverTurnPos = 0;
					riverPts = Reverse[Sort[Flatten[Table[scaleAll*yardSize{x, y}, {x, -columns/2, columns/2}, {y, -rows/2, rows/2}], 1], #1[[2]] > #2[[2]]&]];
					(* find midpoints *)
					riverPts = Table[If[Mod[n, columns + 1] != 0, riverPts[[n]] + (riverPts[[n + columns + 2]] - riverPts[[n]])/2, Nothing], {n, Length[riverPts] - (columns + 2)}];
					(* create river *)
					riverPts = dwAxo[{riverPts},1,"Tilt"->$dwTilt,"Turn"->$dwTurn,"Direction"->"Top"][[1]];
					startRiver = (SeedRandom[seed];RandomChoice[Range[IntegerPart[(rows/2)+1]]]);
					startRiverPtNum = Table[n(columns) + 1 , {n, 0, rows + 1}][[startRiver]];
					temp = 0;
					riverFlow = Table[startRiverPtNum + (n - 1), {n, columns}];
					riverFlow = 
						Flatten[Table[
							If[Mod[n, IntegerPart[(columns + 1)/Min[riverTurns, columns - 1]]] == 0, (* THIS LINE NEEDS FIXED *)
								Which[
									(*n == columns, (* removes last turn if at end *)
										riverFlow[[n]] + riverTurnPos*columns,*)
									temp < 2 && riverFlow[[n]] + (riverTurnPos+1)*columns <= Length[riverPts],
										temp++;
										riverTurnPos++;
										{riverFlow[[n]] + (riverTurnPos-1)*columns, riverFlow[[n]] + riverTurnPos*columns},
									riverFlow[[n]] - (riverTurnPos-1)*columns > 1,
										temp = 0;
										riverTurnPos--;
										{riverFlow[[n]] + (riverTurnPos+1)*columns, riverFlow[[n]] + riverTurnPos*columns},
									True,
										riverFlow[[n]] + riverTurnPos*columns
								],
								riverFlow[[n]] + riverTurnPos*columns
							], 
						{n, Length[riverFlow]}]];
					If[Length[landPts] == 4,
							
						startRiverEdgePt = Partition[Table[landPts[[2]] + n(landPts[[1]] - landPts[[2]])/rows, {n, 0, rows}], 2, 1];
						startRiverEdgePt = {
								#[[1]] + (#[[2]] - #[[1]])/2.5, 
								#[[2]] + (#[[1]] - #[[2]])/2.5
							}&/@startRiverEdgePt;
						endRiverEdgePt = Partition[Table[landPts[[4]] + n(landPts[[3]] - landPts[[4]])/rows, {n, 0, rows}], 2, 1];
						endRiverEdgePt = {
								#[[1]] + (#[[2]] - #[[1]])/2.5, 
								#[[2]] + (#[[1]] - #[[2]])/2.5
							}&/@endRiverEdgePt,
						
						startRiverEdgePt = Join[Partition[landPts[[-(2rows + 1);;-2]], 2], {landPts[[{-1, 1}]]}];
						startRiverEdgePt = {
								#[[1]] + (#[[2]] - #[[1]])/3, 
								#[[2]] + (#[[1]] - #[[2]])/3
							}&/@startRiverEdgePt;
						endRiverEdgePt = Partition[landPts[[2(columns);;-1]], 2];
						endRiverEdgePt = {
								#[[1]] + (#[[2]] - #[[1]])/3, 
								#[[2]] + (#[[1]] - #[[2]])/3
							}&/@endRiverEdgePt
					];
					temp = If[Length[landPts] == 4,
						Join[{Mean[startRiverEdgePt[[-startRiver]]]}, riverPts[[riverFlow]], {Mean[endRiverEdgePt[[startRiver + riverTurnPos]]]}],
						Join[{Mean[startRiverEdgePt[[-startRiver]]]}, riverPts[[riverFlow]], {Mean[endRiverEdgePt[[startRiver + 1 + riverTurnPos]]]}]
					];
					temp = Riffle[temp, Table[(Mean[temp[[n;;n+1]]][[{1, 2}]]), {n, Length[temp]-1}]]; (* increase point quantity *)
					temp = Riffle[temp, Table[(Mean[temp[[n;;n+1]]][[{1, 2}]]), {n, Length[temp]-1}]]; (* increase point quantity *)
					temp = Join[temp[[{1}]], Table[(SeedRandom[seed+n]; scaleAll*RandomReal[{0, .02}, 2] + temp[[n]]), {n, 2, Length[temp] - 1}], temp[[{-1}]]]; (* randomize *)
					(* add width *)
					temp1 = Riffle[temp, Table[(Mean[temp[[n ;; n + 1]]] + Cross[Append[temp[[n + 1]] - Mean[temp[[n ;; n + 1]]], 0], {0, 0, -.5}][[{1, 2}]]), {n, Length[temp] - 1}]][[2;;-3;;2]];
					temp2 = Reverse[Riffle[temp, Table[(Mean[temp[[n ;; n + 1]]] + Cross[Append[temp[[n + 1]] - Mean[temp[[n ;; n + 1]]], 0], {0, 0, .5}][[{1, 2}]]), {n, Length[temp] - 1}]][[2;;-3;;2]]];
					temp1 = Table[If[EuclideanDistance[temp1[[n]], temp1[[n + 1]]] > scaleAll.02, temp1[[n]], Nothing], {n, Length[temp1] - 1}]; (* remove close points *)
					temp2 = Table[If[EuclideanDistance[temp2[[n]], temp2[[n + 1]]] > scaleAll.02, temp2[[n]], Nothing], {n, Length[temp2] - 1}]; (* remove close points *)
					riverFinalPts = If[Length[landPts] == 4,
						Join[
							{startRiverEdgePt[[-startRiver, 1]]},
							temp1,
							endRiverEdgePt[[startRiver + riverTurnPos]][[{2,2,1,1}]], 
							temp2,
							{startRiverEdgePt[[-startRiver, 2]]}
						],
						Join[
							{startRiverEdgePt[[-startRiver, 1]]},
							temp1,
							endRiverEdgePt[[startRiver + 1 + riverTurnPos]][[{2,2,1,1}]], 
							temp2,
							{startRiverEdgePt[[-startRiver, 2]]}
						]
					];
					{
						CapForm["Butt"],
						Switch[buildingColorQuantity2,
								"5SC", buildingColor[[1]],
								_, waterColor
							], 
						FilledCurve[BSplineCurve[riverFinalPts, SplineDegree->2]],
						Switch[buildingColorQuantity2,
								"5SC", buildingColor[[5]],
								_, Darker[waterColor,.2]
							], 
						temp3 = Flatten[Position[riverFinalPts, #, 1]&/@First/@Select[Gather[riverFinalPts], Length[#] > 1 &]]; (* return duplicate points *)
						BSplineCurve[riverFinalPts[[1;;temp3[[1]]]], SplineDegree->2],
						BSplineCurve[riverFinalPts[[temp3[[-1]];;-1]], SplineDegree->2]
					},
					
					riverFlow = {}
				],
					
				riverFlow = {}
			],
			
			(* sidewalks *)
			sidewalks = dwAxo[{#}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]&/@Table[{sw[[1]] + sidewalkWidth(sw[[1]] - sw[[4]]), sw[[2]] + sidewalkWidth(sw[[2]] - sw[[3]]), sw[[3]] + sidewalkWidth(sw[[3]] - sw[[2]]), sw[[4]] + sidewalkWidth(sw[[4]] - sw[[1]])}, {sw, streets/.{Null->Sequence[]}}];
			{
				Switch[buildingColorQuantity2,
						"5SC", buildingColor[[3]],
						_, concreteColor
					], 
				Polygon[#]&/@sidewalks
			},
			
			(* street curbs *)
			{
				AbsoluteThickness[2curbThickness], CapForm["Butt"], 
				Switch[buildingColorQuantity2,
						"5SC", buildingColor[[5]],
						_, streetCurbColor
					], 
				Line[#]&/@Flatten[Table[dwAxo[{p}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"], {p, #[[{1, 2}]]&/@(streets/.{Null->Sequence[]})}], 1]
			},
			
			(* streets and center lines (center lines do not cross intersections *)
			Table[
				If[streets[[n]] === Null,
					
					{},
					
					{
						Switch[buildingColorQuantity2,
								"5SC", White,
								_, streetColor
							], 
						Polygon[dwAxo[{streets[[n]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]], 
						Switch[buildingColorQuantity2,
								"5SC", buildingColor[[3]],
								_, streetCenterlineColor
							], 
						Opacity[streetCenterlineOpacity], AbsoluteDashing[streetCenterlineDash], 
						Line[dwAxo[{
							If[MemberQ[exitRoads, n],
								{Mean[streets[[n]][[{1, 4}]]], Mean[streets[[n]][[{2, 3}]]]},
								{(streets[[n]][[1]] + .5(streets[[n]][[4]] - streets[[n]][[1]])) + .225(streets[[n]][[2]] - streets[[n]][[1]]), (streets[[n]][[2]] + .5(streets[[n]][[3]] - streets[[n]][[2]])) + .3(streets[[n]][[1]] - streets[[n]][[2]])}
							]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]
						]
					}
				],
			{n, Length[streets]}],
			
			(* street crosswalks *)
			Table[
				If[streets[[n]] === Null,
					
					{},
					
					{
						Switch[buildingColorQuantity2,
								"5SC", buildingColor[[3]],
								_, streetCenterlineColor
							], 
						Opacity[streetCenterlineOpacity],  
						If[MemberQ[exitRoads, n],
							{},
							lineCount += 10;
							{
								GraphicsGroup[Table[
									Line[dwAxo[{{(streets[[n]][[1]] + lineloc(streets[[n]][[4]] - streets[[n]][[1]])) + .225(streets[[n]][[2]] - streets[[n]][[1]]), (streets[[n]][[1]] + lineloc(streets[[n]][[4]] - streets[[n]][[1]])) + .3(streets[[n]][[2]] - streets[[n]][[1]])}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
								{lineloc, {1/6, 1/3, 1/2, 2/3, 5/6}}]],
								GraphicsGroup[Table[
									Line[dwAxo[{{(streets[[n]][[2]] + lineloc(streets[[n]][[3]] - streets[[n]][[2]])) + .3(streets[[n]][[1]] - streets[[n]][[2]]), (streets[[n]][[2]] + lineloc(streets[[n]][[3]] - streets[[n]][[2]])) + .225(streets[[n]][[1]] - streets[[n]][[2]])}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
								{lineloc, {1/6, 1/3, 1/2, 2/3, 5/6}}]]
							}
						]
					}
				],
			{n, Length[streets]}],
			
			(* vehicles *)
			temp = 0;
			temp3 = Length[(streets/.{Null->Sequence[]})];
			carsize = 2*streetThickness;
			{ 	
				SeedRandom[seed + temp++];
				temp1 = RandomReal[{.25, .75}];
				SeedRandom[seed + temp + 1];
				temp2 = RandomReal[{.25, .75}];
				{
					(* vehicle set 1 *)
					If[#[[1, 1]] - #[[2, 1]] > #[[1, 2]] - #[[2, 2]],
						
						(* X roads - away from viewer *)
						lineCount += 4;
						topview1 = dwAxo[{carsize{{0.2, -0.4}, {0.2, 0.4}, {-0.2, 0.4}, {-0.2, -0.4}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						topview2 = dwAxo[{carsize{{0.2, -0.4}, {0.2, 0}, {-0.2, 0}, {-0.2, -0.4}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						GraphicsGroup[Table[{
								Switch[carPart[[1]],
									1,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[1]],
											_, Lighter[buildingColor[[Mod[temp+1, buildingColorQuantity2, 1]]], .5]
										],
									2,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[5]],
											_, Darker[buildingColor[[Mod[temp+1, buildingColorQuantity2, 1]]], .75contrast]
										],
									_,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[5]],
											_, buildingColor[[Mod[temp+1, buildingColorQuantity2, 1]]]
										]
								],
								Polygon[Table[dwAxo[{(#[[4]]+3(#[[1]]-#[[4]])/4) + temp1*(#[[2]]-#[[1]])}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]] + scaleAll*carPts, {carPts, carPart[[2]]}]]
							},
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
						]],
						
						(* Y roads - towards viewer *)
						lineCount += 3;
						topview1 = dwAxo[{carsize{{0.4, 0.2}, {-0.4, 0.2}, {-0.4, -0.2}, {0.4, -0.2}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						topview2 = dwAxo[{carsize{{0.4, 0.2}, {0, 0.2}, {0, -0.2}, {0.4, -0.2}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						GraphicsGroup[Table[{
								Switch[carPart[[1]],
									1,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[1]],
											_, Lighter[buildingColor[[Mod[temp, buildingColorQuantity2, 1]]], .5]
										],
									2,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[5]],
											_, Darker[buildingColor[[Mod[temp, buildingColorQuantity2, 1]]], .75contrast]
										],
									_,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[5]],
											_, buildingColor[[Mod[temp, buildingColorQuantity2, 1]]]
										]
								],
								Polygon[Table[dwAxo[{(#[[4]]+3(#[[1]]-#[[4]])/4) + temp2*(#[[2]]-#[[1]])}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]] + scaleAll*carPts, {carPts, carPart[[2]]}]]
							},
							{carPart, {
								(* side *)
								{2, {topview1[[4]], topview2[[4]] + {0, .25carsize}, topview2[[3]] + {0, .25carsize}, topview1[[3]]}},
								(* hood *) 
								{-1, Join[topview2[[{2,3}]] + Table[{0, .25carsize}, 2], topview1[[{3,2}]]]},
								(* top *)
								{1, topview2 + Table[{0, .25carsize}, 4]}
							}}
						]]
					],
					
					(*  vehicle set 2 - rotate set 1 points 180 degrees *)
					If[#[[1, 1]] - #[[2, 1]] > #[[1, 2]] - #[[2, 2]],
						
						(* X roads - towards viewer *)
						lineCount += 3;
						topview1 = dwAxo[{carsize{{-0.2, 0.4}, {-0.2, -0.4}, {0.2, -0.4}, {0.2, 0.4}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						topview2 = dwAxo[{carsize{{-0.2, 0.4}, {-0.2, 0}, {0.2, 0}, {0.2, 0.4}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						GraphicsGroup[Table[{
								Switch[carPart[[1]],
									1,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[1]],
											_, Lighter[buildingColor[[Mod[temp, buildingColorQuantity2, 1]]], .5]
										],
									2,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[5]],
											_, Darker[buildingColor[[Mod[temp, buildingColorQuantity2, 1]]], .75contrast]
										],
									_,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[5]],
											_, buildingColor[[Mod[temp, buildingColorQuantity2, 1]]]
										]
								],
								Polygon[Table[dwAxo[{(#[[4]]+(#[[1]]-#[[4]])/4) + temp2*(#[[2]]-#[[1]])}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]] + scaleAll*carPts, {carPts, carPart[[2]]}]]
							},
							{carPart, {
								(* side *)
								{-1, {topview1[[1]], topview2[[1]] + {0, .25carsize}, topview2[[2]] + {0, .25carsize}, topview1[[2]]}},
								(* hood *) 
								{2, Join[topview2[[{2,3}]] + Table[{0, .25carsize}, 2], topview1[[{3,2}]]]},
								(* top *)
								{1, topview2 + Table[{0, .25carsize}, 4]}
							}}
						]],
						
						(* Y roads - away from viewer *)
						lineCount += 4;
						topview1 = dwAxo[{carsize{{-0.4, -0.2}, {0.4, -0.2}, {0.4, 0.2}, {-0.4, 0.2}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						topview2 = dwAxo[{carsize{{-0.4, -0.2}, {0, -0.2}, {0, 0.2}, {-0.4, 0.2}}}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						GraphicsGroup[Table[{
								Switch[carPart[[1]],
									1,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[1]],
											_, Lighter[buildingColor[[Mod[temp+1, buildingColorQuantity2, 1]]], .5]
										],
									2,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[5]],
											_, Darker[buildingColor[[Mod[temp+1, buildingColorQuantity2, 1]]], .75contrast]
										],
									_,
										Switch[buildingColorQuantity2,
											"5SC", buildingColor[[5]],
											_, buildingColor[[Mod[temp+1, buildingColorQuantity2, 1]]]
										]
								],
								Polygon[Table[dwAxo[{(#[[4]]+(#[[1]]-#[[4]])/4) + temp1*(#[[2]]-#[[1]])}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]] + scaleAll*carPts, {carPts, carPart[[2]]}]]
							},
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
						]]
					]
				}
			}&/@((streets/.{Null->Sequence[]})[[Delete[(SeedRandom[seed]; RandomSample[Range[temp3]]), List/@Range[temp3 (1 - traffic)]]]]),
			
			(* environment highlight line - after streets *)
			If[showEnvironment && Total[{soilLayer, subsoilLayer, rockLayer, bedrockLayer}] != 0,
				{AbsoluteThickness[1], White, Line[{
					If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
						landPts[[{2, 1, 4}]],
						Join[
							landPts[[Position[landPts, Sort[landPts, #[[1]] < #2[[1]] &][[1]]][[1, 1]];;-1]],
							landPts[[1;;Position[landPts, Sort[landPts, #[[1]] > #2[[1]] &][[1]]][[1, 1]]]]
						]
					]
					}]},
				{}
			],
								
			(* plant trees in back for rural and rough edge areas *)
			If[showTrees && edgeTrees != 0,
				If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
					{},
					(*{Red, Opacity[.5], Polygon[landPtsNoRural[[2columns + 1;;2columns + 2rows + 2]]], Polygon[landPtsNoRural[[2columns+2rows + 2;;4columns + 2rows + 2]]]}*)
					If[riverTurns == 0 && (exitRoads === {} || (!showStreets || DeleteDuplicates[streets] === {Null})),
							
						temp = Sort[SeedRandom[seed];RandomPoint[Polygon[landPtsNoRural[[2columns + 1;;2columns + 2rows + 2]]], IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&];
						temp1 = Sort[SeedRandom[seed];RandomPoint[Polygon[landPtsNoRural[[2columns + 2rows + 2;;4columns + 2rows + 2]]], IntegerPart[columns*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&],
						
						temp = Quiet[BooleanRegion[
									And[#1, Not[#2]] &, 
									{
										Polygon[landPtsNoRural[[2columns + 1;;2columns + 2rows + 2]]],
										RegionUnion[Sequence@@Table[Polygon[sw], {sw, sidewalks}], Polygon[riverFinalPts]] 
									}
								]];
						temp = If[Head[temp] === BooleanRegion,
							Region[Polygon[landPtsNoRural[[2columns + 1;;2columns + 2rows + 2]]]],
							temp
						];
						temp = Sort[SeedRandom[seed];RandomPoint[temp, IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&];
						
						temp1 = Quiet[BooleanRegion[
									And[#1, Not[#2]] &, 
									{
										Polygon[landPtsNoRural[[2columns + 2rows + 2;;4columns + 2rows + 2]]],
										RegionUnion[Sequence@@Table[Polygon[sw], {sw, sidewalks}], Polygon[riverFinalPts]] 
									}
								]];
						temp1 = If[Head[temp1] === BooleanRegion,
							Region[Polygon[landPtsNoRural[[2columns + 2rows + 2;;4columns + 2rows + 2]]]],
							temp1
						];
						temp1 = Sort[SeedRandom[seed];RandomPoint[temp1, IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&]
					];
					
					Table[
						Table[
							treeCount += 1;
							treeObjCount += 6;
							SeedRandom[seed+randomTreeCtr++];
							randomReal = RandomReal[treeRandomSize];
							GraphicsGroup[{EdgeForm[Opacity[buildingOpacityStroke]],
								EdgeForm[Switch[buildingColorQuantity2, "5SC", buildingColor[[5]], _, Darker[barkColor,.5randomReal]]], Switch[buildingColorQuantity2, "5SC", buildingColor[[4]], _, Darker[barkColor, .1randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeTrunk],
								EdgeForm[{}], Switch[buildingColorQuantity2, "5SC", buildingColor[[5]], _, Darker[barkColor,contrast*randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeTrunkDark],
								EdgeForm[{}], Switch[buildingColorQuantity2, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@tree],
								EdgeForm[{}], GrayLevel[0, If[buildingColorQuantity2 === "5SC", .25, .2+.33contrast^2]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeDark],
								EdgeForm[{}], GrayLevel[1,.35], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeLight],
								Opacity[buildingOpacityStroke], Switch[buildingColorQuantity2, "5SC", buildingColor[[5]], _, Darker[grassColor, contrast+.05]], Line[treeLoc+(randomReal*treeSize)*#&/@tree]
							}],
						{treeLoc, tr}],
					{tr, {temp, temp1}}]
				],
				{}
			],

			(* buildings *)
			currentBuildingColorQuantity = buildingColorQuantity2;
			riverToBuildingArray = Join[Reverse[Flatten[Table[columns*(r - 1) + c, {c, columns}, {r, rows}]]]];
			If[buildings =!= {},
				Table[
					
					extrude = (SeedRandom[n+seed];RandomReal[{randomHeight, 1}])*scaleAll*scaleHeight*(heightSize[[Mod[n, Length[heightSize], 1]]]);
					
					Table[
						If[buildings[[n, bn, 1]] =!= Null && FreeQ[Flatten[riverFlow], riverToBuildingArray[[n]]],
							
							finalHeight = Min[Max[(SeedRandom[n+bn+seed];RandomReal[{randomHeight, 1}]*extrude), Min[minBuildingHeight, maxBuildingHeight]], Max[minBuildingHeight, maxBuildingHeight]];
							
							(* scale footprint by height *)
							finalHeightScaled = Rescale[{finalHeight}, minMaxHeight, {.01, 1}][[1]];
							scaleByHeightFunction = ScalingTransform[({1, 1}-.5(scaleByHeight*finalHeightScaled)), dwFindCenter[buildings[[n, bn]]]];
							scaleByHeightPts = scaleByHeightFunction[buildings[[n, bn]]];
							
							extObj = dwAxoExtrude[{scaleByHeightPts}, "Extrude"->-finalHeight][[{2,3,5}]];
							
							buildingColorQuantity2 = 
								Which[
									buildingColorQuantity2 === "5SC",
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
							
							If[showBuildings,
								buildingCount += 3;
								GraphicsGroup[Flatten[{
									FaceForm[Opacity[buildingOpacityFill]],
									StrokeForm[Opacity[buildingOpacityStroke]],
									(* building *)
									Table[{
										Switch[buildingColorQuantity2,
											"5SC",
												StrokeForm[{buildingColor[[1]]}],
											_,
												StrokeForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
										],
										Switch[buildingColorQuantity2,
											"5SC",
												FaceForm[{
													Switch[en, 
														1, buildingColor[[5]], 
														2, buildingColor[[1]],
														_, buildingColor[[2]]
													]
												}],
											_,
												FaceForm[{
													Switch[en, 
														1, Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast], 
														2, buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]],
														_, Lighter[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]
													]
												}]
										],
										Polygon[extObj[[en]]]
										
									}, {en, Length[extObj]}], 
									
									(* extra side lines below top and above bottom *)
									Switch[buildingColorQuantity2,
										"5SC",
											StrokeForm[{buildingColor[[1]]}],
										_,
											StrokeForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], 1/3contrast]}]
									],
									(* line below top *)
									extraLineSpace = scaleAll*.02(1/30^.1);
									If[finalHeight >= scaleAll*.05, 
										lineCount += 1;
										Line[({0, -extraLineSpace} + #)&/@extObj[[1, {3, 4}]]], 
										{}
									],
									(* line above bottom *)
									If[finalHeight >= scaleAll*.1, 
										lineCount += 1;
										Line[({0, extraLineSpace} + #)&/@extObj[[1, {1, 2}]]], 
										{}
									],
									
									(* extra front lines below top and above bottom *)
									Switch[buildingColorQuantity2,
										"5SC",
											StrokeForm[{buildingColor[[5]]}],
										_,
											StrokeForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
									],
									(* line below top *)
									extraLineSpace = scaleAll*.02(1/30^.1);
									If[finalHeight >= scaleAll*.05, 
										lineCount += 1;
										Line[({0, -extraLineSpace} + #)&/@extObj[[3, {1, 4}]]], 
										{}
									],
									(* line above bottom *)
									If[finalHeight >= scaleAll*.1, 
										lineCount += 1;
										Line[({0, extraLineSpace} + #)&/@extObj[[2, {1, 2}]]], 
										{}
									],
									
									(* windows *)
									If[showWindows && windowSize != 0,
										windows = Table[dwCityWindows[extObj[[wn]], windowSize, 
											Which[
												finalHeightScaled > roofQuantity + 2((1 - roofQuantity)/3), 
													If[wn == 2, True, False],
												finalHeightScaled > roofQuantity + ((1 - roofQuantity)/3), 
													If[wn == 1, True, False],
												True, 
													False
											], rows*columns], {wn, 2}];
										windowCount += Length[Flatten[windows, 1]];
										{	(* must be list or opacity affects roofs *)
											{
												EdgeForm[{AbsoluteThickness[.5],
													Switch[buildingColorQuantity2,
														"5SC", 
															Darker[buildingColor[[1]], .15],
														_,
															Darker[windowColor, contrast]
													]
												}], 
												Switch[buildingColorQuantity2,
													"5SC", 
														buildingColor[[1]],
													_,
														Darker[windowColor, .6 contrast]
												], 
												Polygon[#]&/@windows[[1]],
												AbsoluteThickness[.5],
												Switch[buildingColorQuantity2,
													"5SC", 
														Lighter[buildingColor[[1]], .25],
													_,
														windowColor
												],
												Line[#[[{2,3,4}]]]&/@windows[[1]]
											}, 
											{
												EdgeForm[{AbsoluteThickness[.5],
													Switch[buildingColorQuantity2,
														"5SC", 
															Darker[buildingColor[[5]], .15],
														_,
															windowColor
													]
												}], 
												Switch[buildingColorQuantity2,
													"5SC",
														buildingColor[[5]],
													_, 
														Lighter[windowColor, contrast]
												], 
												Polygon[#]&/@windows[[2]],
												AbsoluteThickness[.5],
												Switch[buildingColorQuantity2,
													"5SC", 
														Lighter[buildingColor[[5]], .25],
													_,
														Lighter[windowColor, 1.5contrast]
												],
												Line[#[[{2,3,4}]]]&/@windows[[2]]
											}
										},
										{}
									],
										
									(* door *)
									If[finalHeight >= scaleAll*doorLimit,
										temp = ScalingTransform[doorWidth*extraLineSpace*{1,1}/scaleAll, Mean[extObj[[2, {1, 2}]]]];
										temp1 = ScalingTransform[-doorWidth*extraLineSpace*{1,1}/scaleAll, extObj[[1, 2]]];
										temp2 = ScalingTransform[doorAwningDepth*extraLineSpace*{1,1}/scaleAll, extObj[[1, 2]]];
										doorCount += 1;
										{
											Switch[buildingColorQuantity2,
												"5SC",
													StrokeForm[{buildingColor[[1]]}],
												_,
													StrokeForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
											],
											Switch[buildingColorQuantity2,
												"5SC",
													FaceForm[{buildingColor[[5]]}],
												_,
													FaceForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
											],
											Polygon[temp[Join[
												extObj[[2, {2, 1}]],
												({0, doorHeight*extraLineSpace} + #)&/@extObj[[2, {1, 2}]]
											]]],
											Switch[buildingColorQuantity2,
													"5SC", FaceForm[{buildingColor[[2]]}],
												_,
													FaceForm[{Lighter[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
											],
											Polygon[
												(# + (temp[{0, doorHeight*extraLineSpace} + extObj[[2, 1]]] - extObj[[1, 2]]))&/@
												temp1[
													Join[
														temp2[extObj[[1, {2, 1}]]],
														(Subtract[Sequence@@(({0, doorHeight*extraLineSpace} + #)&/@extObj[[2, {1, 2}]])] + #)&/@temp2[extObj[[1, {1, 2}]]]
													]
												]
											]
										}, 
										{}
									],
									
									(* roof *)
									Flatten[{
										If[finalHeightScaled <= roofQuantity,
											roofCount += 1;
											roofCenter = Mean[extObj[[3, {1,3}]]];
											temp = ScalingTransform[{1/2, 1/2}, Mean[extObj[[3, {1,4}]]]];
											temp1 = ScalingTransform[{2/3, 2/3}, Mean[extObj[[3, {1,4}]]]];
											temp2 = ScalingTransform[{1/4, 1/4}, Mean[extObj[[3, {3,4}]]]];
											temp3 = ScalingTransform[{1/4, 1/4}, Mean[extObj[[3, {2,3}]]]];
											Table[
												{
													StrokeForm[{
														Switch[buildingColorQuantity2,
															"5SC", buildingColor[[1]],
															_, Darker[roofColor, contrast]
														]
													}],
													FaceForm[{
														Switch[pn[[1]],
															1|5, 
																Switch[buildingColorQuantity2,
																	"5SC", buildingColor[[2]],
																	_, Darker[roofColor, 1/3 contrast]
																], 
															2, 
																Switch[buildingColorQuantity2,
																	"5SC", buildingColor[[2]],
																	_, Darker[roofColor, 2/3 contrast]
																], 
															3|6, 
																Switch[buildingColorQuantity2,
																	"5SC", buildingColor[[2]],
																	_, Darker[roofColor, contrast]
																], 
															7, 
																Switch[buildingColorQuantity2,
																	"5SC", buildingColor[[1]],
																	_, buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]]
																], 
															8, 
																Switch[buildingColorQuantity2,
																	"5SC", buildingColor[[5]],
																	_, Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]
																], 
															_, 
																Switch[buildingColorQuantity2,
																	"5SC", buildingColor[[2]],
																	_, roofColor
																]]
													}], 
													Polygon[pn[[2]]]}, 
												{pn, 
													If[EuclideanDistance[Sequence@@extObj[[3, {1, 2}]]] < EuclideanDistance[Sequence@@extObj[[3, {2, 3}]]],
														{
															Sequence@@If[finalHeight < scaleAll*.1,
																{
																	extraRoofCount += 3;
																	(* chimney *)
																	{7, Join[-(extObj[[3, 2]] - extObj[[3, 1]])*chimneyWidth + #&/@temp3[extObj[[3, {3,2}]]], 
																			-(extObj[[3, 2]] - extObj[[3, 1]])*chimneyWidth + #&/@(temp3[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {2,3}]]])
																		]},
																	{7, Join[temp3[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {3,2}]]],
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
																
															Sequence@@If[finalHeight >= scaleAll*.1,
																{
																	extraRoofCount += 6;
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
																	{7, Join[temp2[{0,EuclideanDistance[Sequence@@extObj[[3, {1, 3}]]]*2roofHeight}+#&/@extObj[[3, {3,4}]]],
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
															extraRoofCount += 3;
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
												
													(* roof top 1 *)
													If[EuclideanDistance[Sequence@@extObj[[3, {1, 2}]]] > 2*extraRoofHeight, (* prevent RegionIntersection error *)
														flatRoof1Count += 1;
														{
															StrokeForm[{Opacity[0]}],
															Switch[buildingColorQuantity2,
																	"5SC", FaceForm[{buildingColor[[1]]}],
																_,
																	FaceForm[{buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]]}]
															],
															Polygon[temp[Join[extObj[[3, {2, 3}]], 
																{RegionIntersection[Line[extObj[[3, {4, 3}]]], Line[# - {0, extraRoofHeight}&/@extObj[[3, {2, 3}]]]][[1,1]], extObj[[3, 2]] - {0, extraRoofHeight}}]]],
															Switch[buildingColorQuantity2,
																	"5SC", FaceForm[{buildingColor[[5]]}],
																_,
																	FaceForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], .75contrast]}]
															],
															Polygon[temp[Join[extObj[[3, {1, 2}]], {extObj[[3, 2]] - {0, extraRoofHeight}},
																{RegionIntersection[Line[extObj[[3, {1, 4}]]], Line[extObj[[3, {1, 2}]] - Table[{0, extraRoofHeight}, 2]]][[1,1]]}]]],
															Switch[buildingColorQuantity2,
																	"5SC", FaceForm[{buildingColor[[2]]}],
																_,
																	FaceForm[{roofColor(*Lighter[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], .75contrast]*)}]
															],
															Polygon[temp[{
																RegionIntersection[Line[extObj[[3, {1, 4}]]], Line[extObj[[3, {1, 2}]] - Table[{0, extraRoofHeight}, 2]]][[1,1]], 
																extObj[[3, 2]] - {0, extraRoofHeight},
																RegionIntersection[Line[extObj[[3, {4, 3}]]], Line[extObj[[3, {2, 3}]] - Table[{0, extraRoofHeight}, 2]]][[1,1]],
																extObj[[3, 4]]
																}]
															]
														},
														{}
													],
													
												finalHeightScaled <= roofQuantity + 2((1 - roofQuantity)/3), (* 2(.../3) for 3 equal zones *)
												
													(* roof top 2 *)
													flatRoof2Count += 1;
													{
														Switch[buildingColorQuantity2,
															"5SC",
																StrokeForm[{buildingColor[[1]]}],
															_,
																StrokeForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
														],
														Switch[buildingColorQuantity2,
																"5SC", FaceForm[{buildingColor[[1]]}],
															_,
																FaceForm[{buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]]}]
														],
														Polygon[temp[Join[extObj[[3, {1, 4}]], Reverse[extObj[[3, {1, 4}]] + Table[{0, extraRoofHeight}, 2]]]]],
														Switch[buildingColorQuantity2,
																"5SC", FaceForm[{buildingColor[[5]]}],
															_,
																FaceForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], .75contrast]}]
														],
														Polygon[temp[Join[extObj[[3, {4, 3}]], Reverse[extObj[[3, {4, 3}]] + Table[{0, extraRoofHeight}, 2]]]]],
														Switch[buildingColorQuantity2,
																"5SC", FaceForm[{buildingColor[[2]]}],
															_,
																FaceForm[{Lighter[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
														],
														Polygon[temp[extObj[[3]] + Table[{0, extraRoofHeight}, 4]]],
														Switch[buildingColorQuantity2,
																"5SC", {EdgeForm[], FaceForm[{buildingColor[[2]]}]},
															_,
																{EdgeForm[], FaceForm[{buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]]}]}
														],
														Polygon[temp3[extObj[[3]] + Table[{0, 1.5extraRoofHeight}, 4]]]
													},
													
												True,
												
													(* roof top 3 *)
													flatRoof3Count += 1;
													{
														(* layer 1 *)
														Switch[buildingColorQuantity2,
															"5SC",
																StrokeForm[{buildingColor[[1]]}],
															_,
																StrokeForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
														],
														Switch[buildingColorQuantity2,
																"5SC", FaceForm[{buildingColor[[1]]}],
															_,
																FaceForm[{buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]]}]
														],
														Polygon[temp2[Join[extObj[[3, {1, 4}]], Reverse[extObj[[3, {1, 4}]] + Table[{0, 2extraRoofHeight}, 2]]]]],
														Switch[buildingColorQuantity2,
																"5SC", FaceForm[{buildingColor[[5]]}],
															_,
																FaceForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], .75contrast]}]
														],
														Polygon[temp2[Join[extObj[[3, {4, 3}]], Reverse[extObj[[3, {4, 3}]] + Table[{0, 2extraRoofHeight}, 2]]]]],
														Switch[buildingColorQuantity2,
																"5SC", FaceForm[{buildingColor[[2]]}],
															_,
																FaceForm[{Lighter[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
														],
														Polygon[temp2[extObj[[3]] + Table[{0, 2extraRoofHeight}, 4]]],
														
														(* layer 2 *)
														Switch[buildingColorQuantity2,
																"5SC", FaceForm[{buildingColor[[1]]}],
															_,
																FaceForm[{buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]]}]
														],
														Polygon[temp1[Join[extObj[[3, {1, 4}]] + Table[{0, 2extraRoofHeight}, 2], Reverse[extObj[[3, {1, 4}]] + Table[{0, 5extraRoofHeight}, 2]]]]],
														Switch[buildingColorQuantity2,
																"5SC", FaceForm[{buildingColor[[5]]}],
															_,
																FaceForm[{Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], .75contrast]}]
														],
														Polygon[temp1[Join[extObj[[3, {4, 3}]] + Table[{0, 2extraRoofHeight}, 2], Reverse[extObj[[3, {4, 3}]] + Table[{0, 5extraRoofHeight}, 2]]]]],
														Switch[buildingColorQuantity2,
																"5SC", FaceForm[{buildingColor[[2]]}],
															_,
																FaceForm[{Lighter[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
														],
														Polygon[temp1[extObj[[3]] + Table[{0, 5extraRoofHeight}, 4]]],
														
														(* antenna *)
														Switch[buildingColorQuantity2,
															"5SC",
																StrokeForm[{CapForm["Round"], AbsoluteThickness[2], buildingColor[[5]]}],
															_,
																StrokeForm[{CapForm["Round"], AbsoluteThickness[2], Darker[buildingColor[[Mod[n+bn, buildingColorQuantity2, 1]]], contrast]}]
														],
														Line[temp1[(Mean[extObj[[3, {1, 3}]]] + {0, #*extraRoofHeight})&/@{5, 10}]]
													}
											]
											
										]
									}],
					
									(* reset buildingColorQuantity *)
									buildingColorQuantity2 = currentBuildingColorQuantity;
										
								}, 2]]
							],
								
							(* plant trees *)
							If[Length[buildings[[n]]] > 1,
								
								(* object at center of each missing subdivided building *)
								{randomTreeCtr, temp, treeCount, treeObjCount} = dwAddTrees[showTrees, If[MemberQ[Flatten[riverFlow], riverToBuildingArray[[n]]], True, False], seed, randomTreeCtr, treeRandomSize, If[buildings[[n, bn, 1]] === Null, buildings[[n, bn, 2]], buildings[[n, bn]]], buildingColorQuantity, waterColor, concreteColor, streetColor, streetCenterlineColor, 
									buildingColor, barkColor, grassColor, soilColor, contrast, buildingOpacityFill, buildingOpacityStroke, treeCount, treeObjCount, treeSize, treeTrunk, treeTrunkDark, tree, treeDark, treeLight, n, bn, cityTreeIndexList, 
									(.5/(Max[rows,columns]^2)^.47), cityObjCtr, "Subdivided"->True];
								temp,
								
								(* object at center of each missing non-subdivided building *)
								{randomTreeCtr, temp, treeCount, treeObjCount} = dwAddTrees[showTrees, If[MemberQ[Flatten[riverFlow], riverToBuildingArray[[n]]], True, False], seed, randomTreeCtr, treeRandomSize, If[buildings[[n, bn, 1]] === Null, buildings[[n, bn, 2]], buildings[[n, bn]]], buildingColorQuantity, waterColor, concreteColor, streetColor, streetCenterlineColor, 
									buildingColor, barkColor, grassColor, soilColor, contrast, buildingOpacityFill, buildingOpacityStroke, treeCount, treeObjCount, treeSize, treeTrunk, treeTrunkDark, tree, treeDark, treeLight, n, bn, cityTreeIndexList, 
									(.5/(Max[rows,columns]^2)^.47), If[MemberQ[cityTreeIndexList, n], cityObjCtr++, cityObjCtr], "Subdivided"->False];
								temp
							]
							
						],
					{bn, Length[buildings[[n]]]}],
				{n, Length[buildings]}],
			{}],
								
			(* plant trees in front for rural and rough edge areas *)
			If[showTrees && edgeTrees != 0,
				If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
					{},
					(*{Red, Opacity[.5], Polygon[landPtsNoRural[[;;2columns + 2]]], Polygon[Join[landPtsNoRural[[4columns + 2rows + 2;;-1]], landPtsNoRural[[{1}]]]]}*)
					
					If[riverTurns == 0 && (exitRoads === {} || (!showStreets || DeleteDuplicates[streets] === {Null})),
							
						temp = Sort[SeedRandom[seed];RandomPoint[Polygon[landPtsNoRural[[;;2columns + 2]]], IntegerPart[columns*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&];
						temp1 = Sort[SeedRandom[seed];RandomPoint[Polygon[Join[landPtsNoRural[[4columns + 2rows + 2;;-1]], landPtsNoRural[[{1}]]]], IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&],
						
						temp = Quiet[BooleanRegion[
									And[#1, Not[#2]] &, 
									{
										Polygon[landPtsNoRural[[;;2columns + 2]]],
										RegionUnion[Sequence@@Table[Polygon[sw], {sw, sidewalks}], Polygon[riverFinalPts]] 
									}
								]];
						temp = If[Head[temp] === BooleanRegion,
							Region[Polygon[landPtsNoRural[[;;2columns + 2]]]],
							temp
						];
						temp = Sort[SeedRandom[seed];RandomPoint[temp, IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&];
							
						temp1 = Quiet[BooleanRegion[
									And[#1, Not[#2]] &, 
									{
										Polygon[Join[landPtsNoRural[[4columns + 2rows + 2;;-1]], landPtsNoRural[[{1}]]]],
										RegionUnion[Sequence@@Table[Polygon[sw], {sw, sidewalks}], Polygon[riverFinalPts]] 
									}
								]];
						temp1 = If[Head[temp1] === BooleanRegion,
							Region[Polygon[Join[landPtsNoRural[[4columns + 2rows + 2;;-1]], landPtsNoRural[[{1}]]]]],
							temp1
						];
						temp1 = Sort[SeedRandom[seed];RandomPoint[temp1, IntegerPart[rows*edgeTrees(ruralTreeCount*landEdgeRoughness)^ruralTreeBalance]], #1[[2]] > #2[[2]]&]
					];
					Table[
						Table[
							treeCount += 1;
							treeObjCount += 6;
							SeedRandom[seed+randomTreeCtr++];
							randomReal = RandomReal[treeRandomSize];
							GraphicsGroup[{EdgeForm[Opacity[buildingOpacityStroke]],
								EdgeForm[Switch[buildingColorQuantity2, "5SC", buildingColor[[5]], _, Darker[barkColor,.5randomReal]]], Switch[buildingColorQuantity2, "5SC", buildingColor[[4]], _, Darker[barkColor, .1randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeTrunk],
								EdgeForm[{}], Switch[buildingColorQuantity2, "5SC", buildingColor[[5]], _, Darker[barkColor,contrast*randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeTrunkDark],
								EdgeForm[{}], Switch[buildingColorQuantity2, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@tree],
								EdgeForm[{}], GrayLevel[0, If[buildingColorQuantity2 === "5SC", .25, .2+.33contrast^2]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeDark],
								EdgeForm[{}], GrayLevel[1,.35], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeLight],
								Opacity[buildingOpacityStroke], Switch[buildingColorQuantity2, "5SC", buildingColor[[5]], _, Darker[grassColor, contrast+.05]], Line[treeLoc+(randomReal*treeSize)*#&/@tree]
							}],
						{treeLoc, tr}],
					{tr, {temp, temp1}}]
				],
				{}
				],
				
			(* birds *)
			birdCount = 2*treeCount;
			SeedRandom[seed];
			If[MemberQ[{birdSize, birdCount}, 0],
				{},
				temp = {
					dwAxo[.75scaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{25,"Top"}}],
					dwAxo[.75scaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{75,"Top"}}],
					dwAxo[.75scaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{115,"Top"}}],
					dwAxo[scaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{25,"Top"}}],
					dwAxo[scaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{75,"Top"}}],
					dwAxo[scaleAll*birdSize*birdShape, Length[birdShape], "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top", "AxisRotation"->{{115,"Top"}}]
				};
				birdVariations = Length[temp];
				birdList = If[birdCount >= birdVariations^2,
					Partition[Riffle[Range[birdVariations], Partition[RandomPoint[Polygon[({0, Max[({1,.66}*minMaxHeight)[[RandomInteger[{1,2}]]], .1]} + #)&/@landPtsNoRural], birdCount], IntegerPart[birdCount/birdVariations]]],2],
					{}
				];
				If[Length[Flatten[#[[2]]&/@birdList]] >= birdVariations,
					GraphicsGroup[Flatten[{
						Switch[buildingColorQuantity2,
								"5SC", buildingColor[[5]],
								_, birdColor
							],
						Table[
							Table[
								overallBirdCount++;
								Polygon[(p1 + #)&/@temp[[p2[[1]]]]], 
							{p1, p2[[2]]}],
						{p2, birdList}]
					}]],
					{}
				]
			]
			
		}, Background->backgroundColor, ImageSize->imageSize, PlotRange->{{-1,1},{-1,1}}];
		
		{
			$dwCBminMaxHeight, $dwCBstreets, $dwCBbuildings, $dwCBlandPts, $dwCBlandPtsNoRural, $dwCBfinalLandPts, $dwCBconcretePts, $dwCBheightSize, $dwCBheightPattern, $dwCBscaleAll, $dwCBriverFlow, $dwCBriverFinalPts, $dwCBriverToBuildingArray, 
			$dwCBbuildingCount, $dwCBwindowCount, $dwCBflatRoof1Count, $dwCBflatRoof2Count, $dwCBflatRoof3Count, $dwCBroofCount, $dwCBlineCount, $dwCBdoorCount, $dwCBtreeCount, $dwCBtreeObjCount, $dwCBoverallBirdCount, $dwCBextraRoofCount, $dwCBcityTreeIndexList
		} = 
		{
			minMaxHeight, streets, buildings, landPts, landPtsNoRural, finalLandPts, concretePts, heightSize, heightPattern, scaleAll, riverFlow, riverFinalPts, riverToBuildingArray, buildingCount, windowCount, flatRoof1Count, 
			flatRoof2Count, flatRoof3Count, roofCount, lineCount, doorCount, treeCount, treeObjCount, overallBirdCount, extraRoofCount, cityTreeIndexList
		};
		
		If[preview,
			
			map,
				
			CreateDocument[
				ExpressionCell[
					
					(* add hidden item *)
					dwCityBuilderHiddenItemMap[map, rows, columns, imageSize, showBuildings, grassColor, concreteColor, backgroundColor, yardSize, buildingColor, buildingColorQuantity],
					
					"Output", Evaluatable->True
				],
			WindowTitle->ToString[rows]<>" x "<>ToString[columns]<>" City"]
		]
	]

End[] (* End Private Context *)

EndPackage[]