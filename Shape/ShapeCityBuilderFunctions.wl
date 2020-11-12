(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(* add city trees and objects *)
Options[dwAddTrees] = {"Subdivided"->True};
dwAddTrees[showTrees_, riverTree_, seed_, randomTreeCtr_, treeRandomSize_, pts_, buildingColorQuantity_, waterColor_, concreteColor_, streetColor_, streetCenterlineColor_, buildingColor_, barkColor_, grassColor_, soilColor_, contrast_, buildingOpacityFill_, buildingOpacityStroke_, treeCount_, treeObjCount_, treeSize_, treeTrunk_, treeTrunkDark_, tree_, treeDark_, treeLight_, num_, bn_, cityTreeIndexList_, scaleAdj_, cityObjCtr_, OptionsPattern[]]:= 
	Block[{randomReal, treeCount2 = treeCount, treeObjCount2 = treeObjCount, treeLoc, treeLocF, buildingPts, rtCtr = randomTreeCtr, objCount = 0},
		
		If[!riverTree && MemberQ[cityTreeIndexList, num],

			treeLoc = dwFindCenter[dwAxo[{pts}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]];
			Return[{
				randomTreeCtr,
				
				(* ADD NEW SPORT COURTS HERE - change Mod[cityObjCtr, <number of items>] *)
				Switch[Mod[cityObjCtr, 20],
					19,
						(* golf course *)
						objCount += 9;
						GraphicsGroup[Join[
							Table[
								{cp[[1]], Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
							{cp, Partition[Riffle[{
								Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
								White, 
								White, 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
								White, 
								White
								}, dwGolfCoursePts[]], 2]
							}],
							{
								Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
								Polygon[(treeLoc + scaleAdj{.6, .15}) + scaleAdj#&/@{{0 ,.25}, {.35, .375}, {0, .5}}],
								Line[(treeLoc + scaleAdj{.6, .15}) + scaleAdj#&/@{{0, 0}, {0, .5}}]
							}
						]],
					18,
						(* basketball court *)
						objCount += 9;
						GraphicsGroup[Join[
							Table[
								{cp[[1]], Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
							{cp, Partition[Riffle[{
								Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[concreteColor,.4]],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]]
								}, dwBasketballCourt2Pts[][[;;7]]], 2]
							}],
							{
								Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
								Polygon[scaleAdj dwAxo[{dwBasketballCourt2Pts[][[9, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1, 1]] + treeLoc + 1.1scaleAdj#&/@dwAxo[{dwBasketballCourtPts[][[8, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]]],
								Polygon[-scaleAdj dwAxo[{dwBasketballCourt2Pts[][[9, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1, 1]] + treeLoc + 1.1scaleAdj#&/@dwAxo[{dwBasketballCourtPts[][[8, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]]]
							}
						]],
					17,
						(* pond 8 *)
						objCount += 2;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond08Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond08Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}],
					16,
						(* soccer court 2 *)
						objCount += 7;
						GraphicsGroup[Table[
							{cp[[1]], Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
						{cp, Partition[Riffle[{
							Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[grassColor,.6]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor]
							}, dwSoccerField2Pts[]], 2]
						}]],
					15,
						(* pond 7 *)
						objCount += 2;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond07Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond07Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}],
					14,
						(* baseball field 2 *)
						objCount += 7;
						GraphicsGroup[Table[
							{cp[[1]], Polygon[treeLoc + 3scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
						{cp, Partition[Riffle[{
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							White, White, White, White, 
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor]
							}, dwBaseballField2Pts[]], 2]
						}]],
					13,
						(* pond 6 *)
						objCount += 2;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond06Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond06Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}],
					12,
						(* tennis court 2 *)
						objCount += 6;
						GraphicsGroup[Join[
							Table[
								{cp[[1]], Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
							{cp, Partition[Riffle[{
								Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[grassColor,.6]],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor]
								}, dwTennisCourt2Pts[][[;;-2]]], 2]
							}],
							{
								Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
								Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{dwTennisCourt2Pts[][[-1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Right"][[1]]]
							}
						]],
					11,
						(* parking lot 2 *)
						objCount += 3;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", White, _, streetColor], 
							Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLot2Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, streetCenterlineColor], 
							Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLot2Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, streetCenterlineColor], 
							Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLot2Pts[][[3, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}],
					10,
						(* golf course *)
						objCount += 9;
						GraphicsGroup[Join[
							Table[
								{cp[[1]], Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
							{cp, Partition[Riffle[{
								Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .3]],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
								White, 
								White, 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
								White, 
								White
								}, dwGolfCourse2Pts[]], 2]
							}],
							{
								Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
								Polygon[(treeLoc + scaleAdj{-.6, .15}) + scaleAdj#&/@{{0 ,.25}, {.35, .375}, {0, .5}}],
								Line[(treeLoc + scaleAdj{-.6, .15}) + scaleAdj#&/@{{0, 0}, {0, .5}}]
							}
						]],
					9,
						(* pond 5 *)
						objCount += 2;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond05Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond05Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}],
					8,
						(* basketball court *)
						objCount += 9;
						GraphicsGroup[Join[
							Table[
								{cp[[1]], Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
							{cp, Partition[Riffle[{
								Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[concreteColor,.4]],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]]
								}, dwBasketballCourtPts[][[;;7]]], 2]
							}],
							{
								Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
								Polygon[scaleAdj dwAxo[{dwBasketballCourtPts[][[9, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1, 1]] + treeLoc + 1.1scaleAdj#&/@dwAxo[{dwBasketballCourtPts[][[8, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Right"][[1]]],
								Polygon[-scaleAdj dwAxo[{dwBasketballCourtPts[][[9, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1, 1]] + treeLoc + 1.1scaleAdj#&/@dwAxo[{dwBasketballCourtPts[][[8, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Right"][[1]]]
							}
						]],
					7,
						(* pond 4 *)
						objCount += 2;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond04Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond04Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}],
					6,
						(* soccer court *)
						objCount += 7;
						GraphicsGroup[Table[
							{cp[[1]], Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
						{cp, Partition[Riffle[{
							Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[grassColor,.6]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor]
							}, dwSoccerFieldPts[]], 2]
						}]],
					5,
						(* pond 3 *)
						objCount += 2;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond03Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond03Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}],
					4,
						(* baseball field *)
						objCount += 7;
						GraphicsGroup[Table[
							{cp[[1]], Polygon[treeLoc + 3scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
						{cp, Partition[Riffle[{
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							White, White, White, White, 
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor]
							}, dwBaseballFieldPts[]], 2]
						}]],
					3,
						(* pond 2 *)
						objCount += 2;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond02Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond02Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}],
					2,
						(* tennis court *)
						objCount += 6;
						GraphicsGroup[Join[
							Table[
								{cp[[1]], Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]}, 
							{cp, Partition[Riffle[{
								Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[grassColor,.6]],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
								Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor]
								}, dwTennisCourtPts[][[;;-2]]], 2]
							}],
							{
								Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
								Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{dwTennisCourtPts[][[-1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]]]
							}
						]],
					1,
						(* parking lot *)
						objCount += 3;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", White, _, streetColor], 
							Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLotPts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, streetCenterlineColor], 
							Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLotPts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, streetCenterlineColor], 
							Polygon[treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLotPts[][[3, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}],
					_,
						(* pond 1 *)
						objCount += 2;
						GraphicsGroup[{
							Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond01Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], 
							Polygon[treeLoc + scaleAdj#&/@dwAxo[{dwPond01Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]
						}]
				],
						
				treeCount2,
				treeObjCount2 += objCount
			}],
			
			If[showTrees,
				If[OptionValue["Subdivided"],
					
					(* tree at center of each missing subdivided building *)
					SeedRandom[seed+rtCtr++]; 
					randomReal = If[riverTree, .9, 1]*RandomReal[treeRandomSize];
					treeLoc = dwFindCenter[dwAxo[{pts}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]];
					treeCount2 += 1;
					Return[{
						randomTreeCtr + 1,
					
						If[MemberQ[seedRandom[num];RandomSample[{2,3,4}, 2], bn],
							
							(* evergreen tree *)
							objCount += 14;
							GraphicsGroup[{
								EdgeForm[{AbsoluteThickness[1], AbsoluteDashing[{1,2,1,3}], Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor,.35+.5contrast]]}],
								Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,.3+.4contrast]],
								Polygon[treeLoc + .9(randomReal*treeSize)*#&/@dwAxo[{dwEvergreenTreePts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]],
								Table[
									Polygon[treeLoc + 1.1(randomReal*treeSize)*#&/@dwAxo[{dwEvergreenTreePts[][[2, 1]]}, 1, "AxisRotation"->{{deg,"Top"},{0,"Right"},{0,"Left"}}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]]],
								{deg, Range[105,-5,-30]}],
								Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,.3]],
								Table[
									Polygon[treeLoc + 1.1(randomReal*treeSize)*#&/@dwAxo[{dwEvergreenTreePts[][[2, 1]]}, 1, "AxisRotation"->{{deg,"Top"},{0,"Right"},{0,"Left"}}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]]],
								{deg, Range[135,345,30]}],
								AbsoluteThickness[1], Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,.1+.4contrast]],
								Line[treeLoc + 1.1(randomReal*treeSize)*#&/@dwAxo[{dwEvergreenTreePts[][[3, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]]]
							}],
							
							(* tree *)
							objCount += 6;
							GraphicsGroup[{EdgeForm[Opacity[buildingOpacityStroke]],
								EdgeForm[Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,.5randomReal]]], Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[barkColor, .1randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeTrunk],
								EdgeForm[{}], Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,contrast*randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeTrunkDark],
								EdgeForm[{}], Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@tree],
								EdgeForm[{}], GrayLevel[0, If[buildingColorQuantity === "5SC", .25, .2+.33contrast^2]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeDark],
								EdgeForm[{}], GrayLevel[1,.35], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeLight],
								Opacity[buildingOpacityStroke], Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor, contrast+.05]], Line[treeLoc+(randomReal*treeSize)*#&/@tree]
							}]
						],
						
						treeCount2,
						treeObjCount2 += objCount
					}],
				
					(* tree at corner of each missing non-subdivided building *)
					objCount += 24;
					treeCount2 += 4;
					treeObjCount2 += objCount;
					Return[{
						randomTreeCtr + 4,
						
						(* four trees *)
						Table[
							SeedRandom[seed+bn+rtCtr++];
							randomReal = RandomReal[treeRandomSize];
							treeLocF = ScalingTransform[{2/3,2/3}, dwFindCenter[dwAxo[{pts}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]];
							treeLoc = treeLocF[dwAxo[{buildingPts}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]];
							GraphicsGroup[{EdgeForm[Opacity[buildingOpacityStroke]],
								EdgeForm[Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,.5randomReal]]], Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[barkColor, .1randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeTrunk],
								EdgeForm[{}], Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,contrast*randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeTrunkDark],
								EdgeForm[{}], Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Polygon[treeLoc+(randomReal*treeSize)*#&/@tree],
								EdgeForm[{}], GrayLevel[0, If[buildingColorQuantity === "5SC", .25, .2+.33contrast^2]], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeDark],
								EdgeForm[{}], GrayLevel[1,.35], Polygon[treeLoc+(randomReal*treeSize)*#&/@treeLight],
								Opacity[buildingOpacityStroke], Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor, contrast+.05]], Line[treeLoc+(randomReal*treeSize)*#&/@tree]
							}], 
						{buildingPts, pts[[{2,3,1,4}]]}],
						
						treeCount2,
						treeObjCount2
					}]
				],
				Return[{
					randomTreeCtr, 
					{},
					treeCount2,
					treeObjCount2
				}]
			]
		]
			
	]

(* add city trees and objects to canvas *)
Options[dwAddTreesToCanvas] = {"Subdivided"->True};
dwAddTreesToCanvas[showTrees_, riverTree_, seed_, treeRandomSize_, pts_, buildingColorQuantity_, waterColor_, concreteColor_, streetColor_, streetCenterlineColor_, buildingColor_, barkColor_, grassColor_, soilColor_, contrast_, buildingOpacityFill_, buildingOpacityStroke_, treeSize_, treeTrunk_, treeTrunkDark_, tree_, treeDark_, treeLight_, num_, bn_, scaleAdj_, cityTreeIndexList_, cityObjCtr_, OptionsPattern[]]:= 
	Block[{randomReal, treeLoc, treeLocF, style},
		If[!riverTree && MemberQ[cityTreeIndexList, num],

			treeLoc = dwFindCenter[dwAxo[{pts}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]];
			
			(* ADD NEW SPORT COURTS HERE - change Mod[cityObjCtr, <number of items>] *)
			Switch[Mod[cityObjCtr, 20],
					19,
						(* golf course *)
						Do[
							dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
							$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
							style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
							style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
							$dwStyle[[-1]] = style, 
						{cp, Partition[Riffle[{
							Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .3]],
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
							White, 
							White, 
							Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
							White, 
							White
							}, dwGolfCoursePts[]], 2]
						}];
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = (treeLoc + scaleAdj{.6, .15}) + scaleAdj#&/@{{0 ,.25}, {.35, .375}, {0, .5}};
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
						$dwP[[-1]] = (treeLoc + scaleAdj{.6, .15}) + scaleAdj#&/@{{0, 0}, {0, .5}};
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
						$dwStyle[[-1]] = style,
				18,
					(* basketball court 2 *)
					Do[
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
					{cp, Partition[Riffle[{
						Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[concreteColor,.4]],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]]
						}, dwBasketballCourt2Pts[][[;;7]]], 2]
					}];
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = scaleAdj dwAxo[{dwBasketballCourt2Pts[][[9, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1, 1]] + treeLoc + 1.1scaleAdj#&/@dwAxo[{dwBasketballCourtPts[][[8, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = -scaleAdj dwAxo[{dwBasketballCourt2Pts[][[9, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1, 1]] + treeLoc + 1.1scaleAdj#&/@dwAxo[{dwBasketballCourtPts[][[8, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				17,
					(* pond 8 *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond08Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond08Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				16,
					(* soccer field 2 *)
					Do[
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
					{cp, Partition[Riffle[{
						Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[grassColor,.6]],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor]
						}, dwSoccerField2Pts[]], 2]
					}],
				15,
					(* pond 7 *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond07Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond07Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				14,
					(* baseball field 2 *)
					Do[
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 3scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
					{cp, Partition[Riffle[{
						Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						White, White, White, White, 
						Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor]
						}, dwBaseballField2Pts[]], 2]
					}],
				13,
					(* pond 6 *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond06Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond06Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				12,
					(* tennis court 2 *)
					Do[
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
					{cp, Partition[Riffle[{
						Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[grassColor,.6]],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor]
						}, dwTennisCourt2Pts[][[;;-2]]], 2]
					}];
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{dwTennisCourt2Pts[][[-1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Right"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				11,
					(* parking lot 2 *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLot2Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", White, _, streetColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLot2Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, streetCenterlineColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLot2Pts[][[3, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, streetCenterlineColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				10,
					(* golf course *)
					Do[
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
					{cp, Partition[Riffle[{
						Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .3]],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
						White, 
						White, 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						White, 
						White
						}, dwGolfCourse2Pts[]], 2]
					}];
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = (treeLoc + scaleAdj{-.6, .15}) + scaleAdj#&/@{{0 ,.25}, {.35, .375}, {0, .5}};
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
					$dwP[[-1]] = (treeLoc + scaleAdj{-.6, .15}) + scaleAdj#&/@{{0, 0}, {0, .5}};
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, StrokeForm[_]]], {1,1}]];
					$dwStyle[[-1]] = style,
				9,
					(* pond 5 *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond05Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond05Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				8,
					(* basketball court *)
					Do[
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
					{cp, Partition[Riffle[{
						Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[concreteColor,.4]],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, Darker[concreteColor,.25]]
						}, dwBasketballCourtPts[][[;;7]]], 2]
					}];
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = scaleAdj dwAxo[{dwBasketballCourtPts[][[9, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1, 1]] + treeLoc + 1.1scaleAdj#&/@dwAxo[{dwBasketballCourtPts[][[8, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Right"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = -scaleAdj dwAxo[{dwBasketballCourtPts[][[9, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1, 1]] + treeLoc + 1.1scaleAdj#&/@dwAxo[{dwBasketballCourtPts[][[8, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Right"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				7,
					(* pond 4 *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond04Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond04Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				6,
					(* soccer field *)
					Do[
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
					{cp, Partition[Riffle[{
						Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[grassColor,.6]],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor]
						}, dwSoccerFieldPts[]], 2]
					}],
				5,
					(* pond 3 *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond03Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond03Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				4,
					(* baseball field *)
					Do[
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 3scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
					{cp, Partition[Riffle[{
						Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						White, White, White, White, 
						Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor]
						}, dwBaseballFieldPts[]], 2]
					}],
				3,
					(* pond 2 *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond02Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond02Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				2,
					(* tennis court *)
					Do[
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{cp[[2,1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, cp[[1]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
					{cp, Partition[Riffle[{
						Switch[buildingColorQuantity, "5SC", buildingColor[[2]], _, Lighter[grassColor,.6]],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor],
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor], 
						Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, grassColor]
						}, dwTennisCourtPts[][[;;-2]]], 2]
					}];
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{dwTennisCourtPts[][[-1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, soilColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				1,
					(* parking lot *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLotPts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", White, _, streetColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLotPts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, streetCenterlineColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + 1.1scaleAdj#&/@dwAxo[{dwParkingLotPts[][[3, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[3]], _, streetCenterlineColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style,
				_,
					(* pond 1 *)
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond01Pts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", Lighter[buildingColor[[3]],.3], _, Darker[grassColor, .2]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style;
					dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
					$dwP[[-1]] = treeLoc + scaleAdj#&/@dwAxo[{dwPond01Pts[][[2, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
					style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, waterColor], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
					style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
					$dwStyle[[-1]] = style
			],
			
			If[showTrees,
				
				If[OptionValue["Subdivided"],
					
					SeedRandom[seed+$dwCBrandomTreeCtr++]; 
					randomReal = RandomReal[treeRandomSize];
					treeLoc = dwFindCenter[dwAxo[{pts}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]];
					
					If[MemberQ[seedRandom[num];RandomSample[{2,3,4}, 2], bn],
								
						(* SUBDIVIDED EVERGREEN TREE - plant an evergreen tree at center of each missing subdivided building *)
						(* base *)
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + .9(randomReal*treeSize)*#&/@dwAxo[{dwEvergreenTreePts[][[1, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]];
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,.3+.4contrast]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor,.35+.5contrast]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
						style = ReplacePart[style, AbsoluteDashing[{1,2,1,3}], Join[Flatten[Position[style, StrokeForm[_]]], {1,4}]];
						$dwStyle[[-1]] = style;
						
						(* needle dark *)
						Table[
							dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
							$dwP[[-1]] = treeLoc + 1.1(randomReal*treeSize)*#&/@dwAxo[{dwEvergreenTreePts[][[2, 1]]}, 1, "AxisRotation"->{{deg,"Top"},{0,"Right"},{0,"Left"}}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]];
							style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,.3+.4contrast]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
							style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor,.35+.5contrast]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
							style = ReplacePart[style, AbsoluteDashing[{1,2,1,3}], Join[Flatten[Position[style, StrokeForm[_]]], {1,4}]];
							$dwStyle[[-1]] = style,
						{deg, Range[105,-5,-30]}];
							
						(* needle *)
						Table[
							dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
							$dwP[[-1]] = treeLoc + 1.1(randomReal*treeSize)*#&/@dwAxo[{dwEvergreenTreePts[][[2, 1]]}, 1, "AxisRotation"->{{deg,"Top"},{0,"Right"},{0,"Left"}}, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]];
							style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,.3]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
							style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor,.35+.5contrast]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
							style = ReplacePart[style, AbsoluteDashing[{1,2,1,3}], Join[Flatten[Position[style, StrokeForm[_]]], {1,4}]];
							$dwStyle[[-1]] = style,
						{deg, Range[135,345,30]}];
						
						(* middle line *)
						dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
						$dwP[[-1]] = treeLoc + 1.1(randomReal*treeSize)*#&/@dwAxo[{dwEvergreenTreePts[][[3, 1]]}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Left"][[1]];
						style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,.1+.4contrast]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
						$dwStyle[[-1]] = style,
					
						(* SUBDIVIDED TREE - plant a tree at center of each missing subdivided building *)
						(* tree trunk *)
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeTrunk;
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[barkColor, .1randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,.5randomReal]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
						style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						(* tree trunk dark *)
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeTrunkDark;
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,contrast*randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						(* tree leaves *)
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@tree;
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeDark;
						style = ReplacePart[$dwFullDefaultStyle, Black, Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, If[buildingColorQuantity === "5SC", .25, .2+.33contrast^2], Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeLight;
						style = ReplacePart[$dwFullDefaultStyle, White, Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, .35, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@tree;
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor, contrast+.05]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
						style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style
					],
					
					(* NON-SUBDIVIDED TREE - plant a tree at each corner of each missing non-subdivided building *)
					Do[
						SeedRandom[seed+bn+$dwCBrandomTreeCtr++]; 
						randomReal = RandomReal[treeRandomSize];
						treeLocF = ScalingTransform[{2/3,2/3}, dwFindCenter[dwAxo[{pts}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]]];
						treeLoc = treeLocF[dwAxo[{buildingPts}, 1, "Tilt"->$dwTilt, "Turn"->$dwTurn, "Direction"->"Top"][[1]]];
						
						(* tree trunk *)
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeTrunk;
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[barkColor, .1randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,.5randomReal]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
						style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						(* tree trunk dark *)
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeTrunkDark;
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[barkColor,contrast*randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						(* tree leaves *)
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@tree;
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeDark;
						style = ReplacePart[$dwFullDefaultStyle, Black, Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, If[buildingColorQuantity === "5SC", .25, .2+.33contrast^2], Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						dwNewEmptyLayer["Head"->Polygon, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@treeLight;
						style = ReplacePart[$dwFullDefaultStyle, White, Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, .35, Join[Flatten[Position[style, FaceForm[_]]], {1,2,1}]];
						style = ReplacePart[style, 0, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style;
						
						dwNewEmptyLayer["Head"->Line, "SetUndo"->False];
						$dwP[[-1]] = treeLoc+(randomReal*treeSize)*#&/@tree;
						style = ReplacePart[$dwFullDefaultStyle, Switch[buildingColorQuantity, "5SC", buildingColor[[4]], _, Darker[grassColor,-.1+.4randomReal]], Join[Flatten[Position[$dwFullDefaultStyle, FaceForm[_]]], {1,1}]];
						style = ReplacePart[style, Switch[buildingColorQuantity, "5SC", buildingColor[[5]], _, Darker[grassColor, contrast+.05]], Join[Flatten[Position[style, StrokeForm[_]]], {1,1}]];
						style = ReplacePart[style, buildingOpacityStroke, Join[Flatten[Position[style, StrokeForm[_]]], {1,2,1}]];
						$dwStyle[[-1]] = style, 
						
					{buildingPts, pts[[{2,3,1,4}]]}]
				
				],
				
				Nothing
			]
		]
	]

(* returns 5 colors; sorts colors light to dark unless "RemoveGreenUnsorted" is True *)
Options[dwCityColorPalette]:= {"RemoveGreenUnsorted"->False};
dwCityColorPalette[colorDataN_:100, OptionsPattern[]]:= 
	If[OptionValue["RemoveGreenUnsorted"],
		Sort[ColorData[colorDataN, #]&/@(If[.166 < InputForm[ColorConvert[ColorData[colorDataN, #], "HSB"]][[1, 1]] < .4, Nothing, #]&/@Range[10])[[;; 5]], ColorConvert[#2, "Grayscale"][[1]] < ColorConvert[#1, "Grayscale"][[1]] &],
		Sort[ColorData[color, #]&/@Range[5], ColorConvert[#2, "Grayscale"][[1]] < ColorConvert[#1, "Grayscale"][[1]] &]
	]

(* expands edge by roughness without rural *)	
dwCityLandRough[scaleAll_, yardSize_, rows_, columns_, offset_, seed_, ruralArea_]:=
	Block[{n = seed, pts, temp, offsetAdj = 1.5},
		pts = (yardSize*scaleAll*{columns,rows}*(#/2))&/@
			Join[
				Table[{r, -1 - (SeedRandom[n++]; .1 + RandomReal[{0,offsetAdj*offset}])/rows}(* + {0, -ruralArea}*), {r, -1, 1, 1/(columns/2)}],
				Table[{1 + (SeedRandom[n++]; .1 + RandomReal[{0,offsetAdj*offset}])/columns, c}(* + {ruralArea, 0}*), {c, -1 + (1/(rows/2)), 1 - (1/(rows/2)), 1/(rows/2)}],
				Reverse@Table[{r, 1 + (SeedRandom[n++]; .1 + RandomReal[{0,offsetAdj*offset}])/rows}(* + {0, ruralArea}*), {r, -1, 1, 1/(columns/2)}],
				Reverse@Table[{-1 - (SeedRandom[n++]; .1 + RandomReal[{0,offsetAdj*offset}])/columns, c}(* + {-ruralArea, 0}*), {c, -1 + (1/(rows/2)), 1 - (1/(rows/2)), 1/(rows/2)}]
			];
		ReplacePart[pts,
			{
				(* do not add distance to corner points because it changes tree planting area *)
				temp = (yardSize*scaleAll*{columns,rows})/2;
				{1}->{Min[Mean[{pts[[2, 1]], pts[[-1, 1]]}], -temp[[1]]], Min[Mean[{pts[[2, 2]], pts[[-1, 2]]}], -temp[[2]]]}, 
				{columns + 1}->{Max[Mean[{pts[[columns, 1]], pts[[columns + 2, 1]]}], temp[[1]]], Min[Mean[{pts[[columns, 2]], pts[[columns + 2, 2]]}], -temp[[2]]]}, 
				{columns + rows + 1}->{Max[Mean[{pts[[columns + rows, 1]], pts[[columns + rows + 2, 1]]}], temp[[1]]], Max[Mean[{pts[[columns + rows, 2]], pts[[columns + rows + 2, 2]]}], temp[[2]]]},
				{2columns + rows + 1}->{Min[Mean[{pts[[2columns + rows, 1]], pts[[2columns + rows + 2, 1]]}], -temp[[1]]], Max[Mean[{pts[[2columns + rows, 2]], pts[[2columns + rows + 2, 2]]}], temp[[2]]]}
			}
		]
	]

(* expands edge by ruralArea *)	
dwCityLand[scaleAll_, yardSize_, rows_, columns_, ruralArea_]:=
	scaleAll{-ruralArea,{-ruralArea,ruralArea},ruralArea,{ruralArea,-ruralArea}}+
		(yardSize*scaleAll*(#/2)&/@{{-columns,-rows},{-columns,rows},{columns,rows},{columns,-rows}})

(* returns {subdivided building footprints, building heights} *)
dwCityBuildings[scaleAll_, subdivideSize_, shapePts_, scaleEach_, yardSize_, seed_, randomScale_, rows_, columns_, subdivideScale_, subdivideDistanceOrigin_, reverseHeight_, heightPattern_, flipHeight_]:=
	Block[{buildings, heightSize, minMaxHeight, ctr = 0, b, distances = {}, plus = 1.25, minus = .75},
							
		(* building footprints *)
		buildings = Flatten[Reverse[Partition[Flatten[
			Table[scaleAll ({(x - 1) yardSize[[1]], -(y - 1) yardSize[[2]]} + 
				{
					SeedRandom[x+y+seed];RandomReal[{1-.95randomScale[[1]],1}],
					SeedRandom[x+x+y+seed];RandomReal[{1-.95randomScale[[2]],1}]
				}*# - {yardSize[[1]]*(columns - 1), - yardSize[[2]]*(rows - 1)}/2)&/@((scaleEach*#)&/@shapePts), 
			{x, columns}, {y, rows}], 1], rows]], 1];
			
		(* find building distance from provided origin *)
		heightSize = 
			If[subdivideDistanceOrigin =!= {},
				If[rows*columns == 1,
					{1},
					
					distances = EuclideanDistance[subdivideDistanceOrigin, dwFindCenter[#]]&/@buildings;
					distances = 
						If[reverseHeight,
							Max[distances] - distances,
							distances
						];
					distances = Rescale[distances, MinMax[distances], {.01, 1}] (* {0, 1} will always subdivide smallest building height *)
				],
				dwCityHeightPattern[subdivideDistanceOrigin, reverseHeight, rows, columns, buildings, heightPattern]
			];
		minMaxHeight = MinMax[heightSize];
		heightSize = 
			If[Length[buildings] == 1, 
				
				{flipHeight},
				
				Table[
					heightSize[[n]] + flipHeight(Abs[heightSize[[n]] - minMaxHeight[[2]]] - heightSize[[n]]), 
				{n, Length[heightSize]}]
			];	
		
		(* subdivided building footprints if below specific height or distance *)
		b = buildings;
		Do[
			If[++ctr > Length[heightSize], ctr=1];
			buildings[[n]] = 
				If[
					Rescale[{heightSize[[ctr]]}, minMaxHeight+{0,$MachineEpsilon}, {.01, 1}][[1]] <= subdivideSize && subdivideSize != 0,
					{
						{	b[[n,2]] + {plus, 1}subdivideScale ((b[[n,1]] - b[[n,2]])/2), 
							b[[n,2]], (* upper right *)
							b[[n,2]] + {1, minus}subdivideScale ((b[[n,3]] - b[[n,2]])/2), 
							b[[n,2]] + {plus, minus}subdivideScale ((b[[n,4]] - b[[n,2]])/2)},
						{	b[[n,1]], (* upper left *)
							b[[n,1]] + {minus, 1}subdivideScale ((b[[n,2]] - b[[n,1]])/2), 
							b[[n,1]] + {minus, plus}subdivideScale ((b[[n,3]] - b[[n,1]])/2), 
							b[[n,1]] + {1, plus}subdivideScale ((b[[n,4]] - b[[n,1]])/2)},
						{	b[[n,3]] + {minus, plus}subdivideScale ((b[[n,1]] - b[[n,3]])/2), 
							b[[n,3]] + {1, plus}subdivideScale ((b[[n,2]] - b[[n,3]])/2),
							b[[n,3]], (* lower right *)
							b[[n,3]] + {minus, 1}subdivideScale ((b[[n,4]] - b[[n,3]])/2)},
						{	b[[n,4]] + {1, minus}subdivideScale ((b[[n,1]] - b[[n,4]])/2), 
							b[[n,4]] + {plus, minus}subdivideScale ((b[[n,2]] - b[[n,4]])/2), 
							b[[n,4]] + {plus, 1}subdivideScale ((b[[n,3]] - b[[n,4]])/2),
							b[[n,4]]} (* lower left *)
					},
					{buildings[[n]]}
				], 
		{n, Length[buildings]}];
		
		Return[{buildings, heightSize}]
	]

(* returns values for heightPattern *)
Options[dwCityHeightPatternFunction]:= {ImageSize->48, "Preview"->False};
dwCityHeightPatternFunction[currentBuildingPattern_, rows_, columns_, seed_, OptionsPattern[]]:=
	Block[{temp, pattern, size = OptionValue[ImageSize]},
		pattern = 
			Switch[currentBuildingPattern,
				"Default",
					(SeedRandom[seed];RandomReal[{0, 1}, rows*columns]),
				"House",
					(SeedRandom[seed];RandomReal[{0, 1}, rows*columns]),
				"High-rise",
					(SeedRandom[seed];RandomReal[{0, 1}, rows*columns]),
				"Back left",
					Reverse[Flatten[Table[r, {c, columns}, {r, rows}]]],
				"Back right",
					Reverse[Flatten[Table[c, {c, columns}, {r, rows}]]],
				"Middle",
					Flatten[Table[If[(r > rows/3 && r <= rows - rows/3) && (c > columns/3 && c <= columns - columns/3), 1, 0], {c, columns}, {r, rows}]],
				"Middle step",
					Flatten[Table[Round[EuclideanDistance[{(rows + 1)/2, (columns + 1)/2}, {r, c}]^.75, .75], {c, columns}, {r, rows}]],
				"Perimeter",
					If[Min[rows, columns] < 4,
						
						Flatten[Table[If[MemberQ[{1, rows}, r] || MemberQ[{1, columns}, c], 1, 0], {c, columns}, {r, rows}]],
						
						temp = 6; (* corner is 1/temp length of side; minimum is 1 building *) 
						Flatten[Table[
							If[
								r <= Max[Min[rows, columns], temp]/temp || 
								c <= Max[Min[rows, columns], temp]/temp ||
								c >= (rows + 1) - (Max[Min[rows, columns], temp]/temp) ||
								r >= (columns + 1) - (Max[Min[rows, columns], temp]/temp),
									1, 
									0
							], {c, columns}, {r, rows}]
						]
					],
				"Corners",
				 	temp = 3; (* corner is 1/temp length of side; minimum is 1 building *) 
					Flatten[Table[
						If[
							(r <= Max[Min[rows, columns], temp]/temp && c <= Max[Min[rows, columns], temp]/temp) ||
							(r <= Max[Min[rows, columns], temp]/temp && c >= (rows + 1) - (Max[Min[rows, columns], temp]/temp)) || 
							(r >= (columns + 1) - (Max[Min[rows, columns], temp]/temp) && c >= (rows + 1) - (Max[Min[rows, columns], temp]/temp)) ||
							(r >= (columns + 1) - (Max[Min[rows, columns], temp]/temp)) && c <= Max[Min[rows, columns], temp]/temp,
								1, 
								0
						], {c, columns}, {r, rows}]
					],
				"HM 1",
					If[rows > 1 || columns > 1,
						Flatten[Table[Cos[r/2] + Cos[c/2], {r, -3 Pi, 3 Pi, 6 Pi/((rows - 1)+$MachineEpsilon)}, {c, -3 Pi, 3 Pi, 6 Pi/(columns - 1)}]],
						{}
					],
				"HM 2",
					If[rows > 1 || columns > 1,
						Flatten[Table[Cos[(r c)/2], {r, -3 Pi, 3 Pi, 6 Pi/((rows - 1)+$MachineEpsilon)}, {c, -3 Pi, 3 Pi, 6 Pi/(columns - 1)}]],
						{}
					],
				"HM 3",
					If[rows > 1 || columns > 1,
						Flatten[Table[Sin[r c]^2, {r, -3 Pi, 3 Pi, 6 Pi/((rows - 1)+$MachineEpsilon)}, {c, -3 Pi, 3 Pi, 6 Pi/(columns - 1)}]],
						{}
					],
				"HM 4",
					If[rows > 1 || columns > 1,
						Flatten[Table[Sin[r c], {r, -3 Pi, 3 Pi, 6 Pi/((rows - 1)+$MachineEpsilon)}, {c, -3 Pi, 3 Pi, 6 Pi/(columns - 1)}]],
						{}
					],
				"HM 5",
					If[rows > 1 || columns > 1,
						Flatten[Table[Cos[r^2] Sin[c^2], {r, -3 Pi, 3 Pi, 6 Pi/((rows - 1)+$MachineEpsilon)}, {c, -3 Pi, 3 Pi, 6 Pi/(columns - 1)}]],
						{}
					],
				"HM 6",
					If[rows > 1 || columns > 1,
						Flatten[Table[Cos[r^2] Sin[c^4], {r, -3 Pi, 3 Pi, 6 Pi/((rows - 1)+$MachineEpsilon)}, {c, -3 Pi, 3 Pi, 6 Pi/(columns - 1)}]],
						{}
					],
				"HM 7",
					If[rows > 1 || columns > 1,
						Flatten[Table[Sin[r c]^4*Cos[r c]^4,{r,-Pi,Pi, 2Pi/((rows-1)+$MachineEpsilon)},{c,-Pi,Pi, 2Pi/(columns-1)}]],
						{}
					],
				"HM 8",
					If[rows > 1 || columns > 1,
						Flatten[Table[Sin[r]*Cos[c],{r,-Pi,Pi, 2Pi/((rows-1)+$MachineEpsilon)},{c,-Pi,Pi,2Pi/(columns-1)}]],
						{}
					],
				"HM 9",
					If[rows > 1 || columns > 1,
						Flatten[Table[-Sin[r c]*Cos[r c],{r,-3 Pi,3 Pi,6 Pi/((rows-1)+$MachineEpsilon)},{c,-3 Pi,3 Pi,6 Pi/(columns-1)}]],
						{}
					],
				"HM 10",
					If[rows > 1 || columns > 1,
						Flatten[Table[-Sin[r]*Cos[c],{r,-3 Pi,3 Pi,6 Pi/((rows-1)+$MachineEpsilon)},{c,-3 Pi,3 Pi,6 Pi/(columns-1)}]],
						{}
					],
				_,
					{}
			];
			
			(* return values or preview *)
			If[OptionValue["Preview"],
				If[MemberQ[{"Middle step"}, currentBuildingPattern],
					
					Rotate[ArrayPlot[Partition[Rescale[pattern, MinMax[pattern]], rows], ImageSize->size, PlotRangePadding->None], -Pi/2],
					
					(* reverse height map tones to match conventional height maps *)
					Rotate[ArrayPlot[Partition[Rescale[pattern, Reverse@MinMax[pattern]], rows], ImageSize->size, PlotRangePadding->None], -Pi/2]
				],
				pattern
			]
	]
	 
(*	- need all buildings for input (no nulls)
	- heightPattern is an unlimited list of reals
	- subdivideDistanceOrigin is a coordinate
	- one number list for heightPattern will either have all or no roofs and subdivision
	- list with fewer items for heightPattern will create a consistent pattern for roofs and subdivision depending on the number of rows and columns  *)
dwCityHeightPattern[subdivideDistanceOrigin_:{0, 0}, reverseHeight_:False, rows_, columns_, buildings_, heightPattern_:{}]:=
	Block[{distances},
		distances = 
			If[heightPattern === {},
			
				If[subdivideDistanceOrigin === {},
					{1},
					EuclideanDistance[subdivideDistanceOrigin, dwFindCenter[Flatten[#, 1]]]&/@buildings
				],
			
				heightPattern
			];
			
		distances = 
			If[reverseHeight,
				Max[distances] - distances,
				distances
			];
				
		(* {0, n} causes error; n affects overall height but needs to be 1 since roofs cover to 1 *)
		Rescale[distances, MinMax[distances], {.1, 1}]
	]

dwRemoveBuildings[buildings_, buildingRemoval_, seed_]:=
	Block[{ctr = 0, ranges, removedBuildings, removedBuildingsPos},
		If[buildingRemoval != 0,
			
			ranges = Length[#]&/@buildings;
			removedBuildings = (SeedRandom[seed]; RandomSample[Range[Length[Flatten[buildings,1]]], Length[Flatten[buildings,1]]]);
			removedBuildings = Table[removedBuildings[[++ctr]]&/@Range[n], {n, ranges}];
			removedBuildingsPos = Table[Flatten[Position[removedBuildings, n, Infinity], 1], {n, Range[Length[Flatten[buildings, 1]]]}];
			ReplacePart[buildings, #->{Null, buildings[[Sequence@@#]]}&/@removedBuildingsPos[[Range[Max[Round[buildingRemoval*Length[Flatten[buildings, 1]]], 1]]]]],
			
			buildings
		]
	]
	
dwExitRoads[exitRoadPattern_, rows_, columns_]:=
	Block[{allexits},
		allexits = 
			Flatten[{
				Table[(n - 1)*rows + 1, {n, columns - 1}],
				Table[(columns - 1)*rows + n*columns, {n, rows - 1}],
				Reverse@Table[n*rows, {n, columns - 1}],
				Reverse@Table[(columns - 1)*(rows - 1) + n*columns, {n, rows - 1}]
			}];
		
		If[allexits === {},
			
			{},
			
			Switch[exitRoadPattern,
				"All",
					allexits, 
				"Middle",
					allexits[[{
						IntegerPart[(columns - 1)/2] + 1, 
						(columns - 1) + IntegerPart[(rows - 1)/2] + 1, 
						(columns - 1) + (rows - 1) + IntegerPart[(columns - 1)/2] + 1, 
						2 (columns - 1) + (rows - 1) + IntegerPart[(rows - 1)/2] + 1
					}]], 
				"Corners",
					allexits[[{
						1, (columns - 1), 
						columns, (columns - 1) + (rows - 1), 
						(columns - 1) + rows, 2*(columns - 1) + (rows - 1),
						2*(columns - 1) + rows, -1
					}]], 
				"Random",
					RandomSample[allexits, IntegerPart[Length[allexits]/2]],
				_,(* none *)
					{}
			]
		]
	]
	
dwExtremeExitRoads[rows_, columns_, newRows_, newColumns_, exitRoads_] :=
	Block[{allExitRoads, allNewExitRoads, roadPos},
		allExitRoads = {
				Table[(n - 1)*rows + 1, {n, columns - 1}],
				Table[(columns - 1)*rows + n*columns, {n, rows - 1}],
				Reverse@Table[n*rows, {n, columns - 1}],
				Reverse@Table[(columns - 1)*(rows - 1) + n*columns, {n, rows - 1}]
			};
		allNewExitRoads = {
				Table[(n - 1)*newRows + 1, {n, newColumns - 1}],
				Table[(newColumns - 1)*newRows + n*newColumns, {n, newRows - 1}],
				Reverse@Table[n*newRows, {n, newColumns - 1}],
				Reverse@Table[(newColumns - 1)*(newRows - 1) + n*newColumns, {n, newRows - 1}]
			};
		roadPos = Position[allExitRoads, #] & /@ exitRoads;
		Flatten[{
			If[columns != 1 && Length[Position[roadPos, {1, _}]] == columns - 1,
				allNewExitRoads[[1]],
				Table[allNewExitRoads[[1, IntegerPart[newColumns (pos/columns)]]], {pos, #[[2]] & /@ Extract[roadPos, Position[roadPos, {1, _}]]}]
			],
			If[rows != 1 && Length[Position[roadPos, {2, _}]] == rows - 1,
				allNewExitRoads[[2]],
				Table[allNewExitRoads[[2, IntegerPart[newRows (pos/rows)]]], {pos, #[[2]] & /@ Extract[roadPos, Position[roadPos, {2, _}]]}]
			],
			If[columns != 1 && Length[Position[roadPos, {3, _}]] == columns - 1,
				allNewExitRoads[[3]],
				Table[allNewExitRoads[[3, IntegerPart[newColumns (pos/columns)]]], {pos, #[[2]] & /@ Extract[roadPos, Position[roadPos, {3, _}]]}]
			],
			If[rows != 1 && Length[Position[roadPos, {4, _}]] == rows - 1,
				allNewExitRoads[[4]],
				Table[allNewExitRoads[[4, IntegerPart[newRows (pos/rows)]]], {pos, #[[2]] & /@ Extract[roadPos, Position[roadPos, {4, _}]]}]
			]
		}]
	]
	
dwCityStreets[scaleAll_, showStreets_, streetThickness_, yardSize_, rows_, columns_, ruralArea_, exitRoads_, land_, landEdgeRoughness_, landEdgeLimit_]:=
	Block[{st1, st2, ctr = 0,
		(* list that pairs street position with land pts *)
		landorder = 
			If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
				{},
				Partition[
					Riffle[
						Range[2 rows + 2 columns],
						Insert[
							Flatten[{
								Table[(n - 1)*rows + 1, {n, columns - 1}],
								Table[(columns - 1)*rows + n*columns, {n, rows - 1}],
								Reverse@Table[n*rows, {n, columns - 1}],
								Reverse@Table[(columns - 1)*(rows - 1) + n*columns, {n, rows - 1}]
							}], 
							0, 
							{{1}, {columns}, {columns + rows - 1}, {2 columns + rows - 2}}
						]
					], 
				2]
			]
		},
		(* position points - st1 and st2 remove street thickness from outer edge of street *)
		If[showStreets, 
			Flatten[{
				Table[
					Table[
						++ctr;
						st1 = If[y == -(rows)/2, If[MemberQ[exitRoads, ctr], 
							If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
								ruralArea,
								ManhattanDistance[{(Mean[{x,x+1}] yardSize[[1]]), (yardSize[[2]] y)}, land[[(Position[landorder, ctr]/.{{_, 1}->Sequence[]})[[1, 1]]]]/scaleAll] + ruralArea
							], 0], streetThickness];
						st2 = If[y == (rows-2)/2, If[MemberQ[exitRoads, ctr], 
							If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
								ruralArea,
								ManhattanDistance[{(Mean[{x,x+1}] yardSize[[1]]), (yardSize[[2]] y) + yardSize[[2]]}, land[[(Position[landorder, ctr]/.{{_, 1}->Sequence[]})[[1, 1]]]]/scaleAll] + ruralArea
							], 0], streetThickness];
						scaleAll{
						{(Mean[{x,x+1}] yardSize[[1]]) + streetThickness, (yardSize[[2]] y) - st1}, 
						{(Mean[{x,x+1}] yardSize[[1]]) + streetThickness, (yardSize[[2]] y) + st2 + yardSize[[2]]}, 
						{(Mean[{x,x+1}] yardSize[[1]]) - streetThickness, (yardSize[[2]] y) + st2 + yardSize[[2]]}, 
						{(Mean[{x,x+1}] yardSize[[1]]) - streetThickness, (yardSize[[2]] y) - st1}}, 
					{y, -(rows)/2, (rows-1)/2}], 
				{x, -(columns-1)/2, (columns-2)/2}],
				Table[
					Table[
						++ctr;
						st1 = If[x == -(columns)/2, If[MemberQ[exitRoads, ctr], 
							If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
								ruralArea,
								ManhattanDistance[{(yardSize[[1]] x), (Mean[{y,y+1}] yardSize[[2]])}, land[[(Position[landorder, ctr]/.{{_, 1}->Sequence[]})[[1, 1]]]]/scaleAll] + ruralArea
							], 0], streetThickness];
						st2 = If[x == (columns-2)/2, If[MemberQ[exitRoads, ctr], 
							If[landEdgeRoughness == 0 || (rows < landEdgeLimit || columns < landEdgeLimit),
								ruralArea,
								ManhattanDistance[{(yardSize[[1]] x) + yardSize[[1]], (Mean[{y,y+1}] yardSize[[2]])}, land[[(Position[landorder, ctr]/.{{_, 1}->Sequence[]})[[1, 1]]]]/scaleAll] + ruralArea
							], 0], streetThickness];
						scaleAll{
						{(yardSize[[1]] x) - st1, (Mean[{y,y+1}] yardSize[[2]]) + streetThickness}, 
						{(yardSize[[1]] x) + st2 + yardSize[[1]], (Mean[{y,y+1}] yardSize[[2]]) + streetThickness}, 
						{(yardSize[[1]] x) + st2 + yardSize[[1]], (Mean[{y,y+1}] yardSize[[2]]) - streetThickness},
						{(yardSize[[1]] x) - st1, (Mean[{y,y+1}] yardSize[[2]]) - streetThickness}}, 
					{x, -(columns)/2, (columns-1)/2}], 
				{y, -(rows-1)/2, (rows-2)/2}]
			}, 2], 
		{}]
	]
	
dwRemoveStreets[buildings_, streets_, shapePts_, scaleEach_, yardSize_, seed_, randomScale_, rows_, columns_, streetPad_, buildingRemoval_]:=
	Block[{buildingsRemoved, buildingCenters, streetCenters, closestBuildings, closestBuildingsID},
			
		If[buildingRemoval == 1,
			
			streets,
			
			(* find closest building to street segment *)
			buildingsRemoved = Flatten[buildings, 1]/.{{Null,___}->Sequence[]};
			buildingCenters = Table[dwFindCenter[b], {b, buildingsRemoved}]/.{{}->Sequence[]};
			streetCenters = Table[{n,dwFindCenter[streets[[n]]]}, {n, Length[streets]}];
			closestBuildings = Table[Nearest[buildingCenters, streetCenters[[n,2]], 2], {n, Length[streetCenters]}];
			closestBuildingsID = Table[Flatten[Position[buildingCenters,#]&/@cb], {cb, closestBuildings}];
						
			(* check if building plus pad intersects building side of street *)
			Table[
				If[(RegionDimension[RegionIntersection[MeshRegion[{{-streetPad, streetPad}, {streetPad, streetPad}, {streetPad, -streetPad}, {-streetPad, -streetPad}}+buildingsRemoved[[closestBuildingsID[[n,#]]]], Polygon[{1,2,3,4}]], MeshRegion[streets[[n]], Polygon[{1,2,3,4}]]]]&/@If[Length[closestBuildingsID[[n]]] == 2, {1, 2}, {1, 1}]) === {-Infinity, -Infinity},
					streets[[n]],
					Null
				],
			{n, Length[streets]}]
		]
	]

(* all windows are similar in size so quantity of windows is based on building size *)
dwCityWindows[fourPts_, windowSize_, windowStripes_, buildingQuantity_]:=
	Block[{grid, windows, w, div, h, quantity, gridSize = 25(buildingQuantity^.5) (* controls number of windows *), ratio = 2/3},
		If[windowSize == 0,
			{},
			quantity = Round[windowSize*gridSize];
			(* div is {number windows in row, number of rows}; 1 creates a stripe; 0 produces no windows *)
			div = {If[windowStripes, 1, Round[IntegerPart[((quantity*EuclideanDistance[fourPts[[1]], fourPts[[2]]]))], 4] + 1], IntegerPart[ratio*quantity*EuclideanDistance[fourPts[[2]], fourPts[[3]]]]};
			div = If[FreeQ[div, If[windowStripes, 0, 0|1]], (* 0|1 for no stripes *)
				w = Table[fourPts[[2]] + n (fourPts[[1]] - fourPts[[2]]), {n, 0, 1, 1/div[[1]]}];
				h = Table[fourPts[[3]] + n (fourPts[[2]] - fourPts[[3]]), {n, 0, 1, 1/div[[2]]}];
				grid = Flatten[Table[{0, hn[[2]] - w[[1, 2]]} + wn, {hn, h}, {wn, w}], 1];
				windows = Table[{n, n + 1, n + Length[w] + 1, n + Length[w]}, {n, Length[w] + 3, Length[grid] - 2 Length[w] - 1, 4}];
				grid[[#]]&/@windows,
				{}
			]
		]
	 ]

(* returns a simple footprint of the city *)
dwCityPreview[rows_, columns_, scaleEach_, yardSize_, randomScale_, concreteSize_, seed_, subdivideScale_, streetThickness_, streetPad_, subdivideSize_, removeBlockedStreets_, finalScale_, streetColor_, grassColor_, concreteColor_, ruralArea_, subdivideDistanceOrigin_, reverseHeight_, exitRoads_, flipHeight_, heightPattern_]:=
	Block[{land, streets, scaleAll, concretePts, shapePts = {{-0.2, 0.2}, {0.2, 0.2}, {0.2, -0.2}, {-0.2, -0.2}}},
		scaleAll = finalScale/(.5 Max[Reverse[yardSize]*{rows, columns}] + ruralArea);
		land = dwCityLand[scaleAll, yardSize, rows, columns, ruralArea];
		streets = dwCityStreets[scaleAll, True, streetThickness, yardSize, rows, columns, ruralArea, exitRoads, {}, 0, 0];
		concretePts = {(1.25*scaleAll*concreteSize*(2*yardSize)*#)&/@(shapePts)};
		Graphics[{
			{EdgeForm[{}], grassColor, Polygon[land]},
			{EdgeForm[{}], concreteColor, Polygon[concretePts]},
			{EdgeForm[{}], streetColor, Polygon[#]&/@(streets/.{Null->Sequence[]})},
			{EdgeForm[{}], Black, Polygon[#]&/@dwCityBuildings[scaleAll, subdivideSize, shapePts, scaleEach, yardSize, seed, randomScale, rows, columns, subdivideScale, subdivideDistanceOrigin, reverseHeight, heightPattern, flipHeight][[1]]}
		}, ImageSize->{52, 52}] (* ImageSize should match previewButtonSize *)
	]

(* rows, columns, scaleEach, yardSize, randomScale, concreteSize, seed, subdivideScale, streetThickness, streetPad, subdivideSize, removeBlockedStreets *)
dwCityPreview01[streetColor_, grassColor_, concreteColor_, finalScale_, ruralArea_, heightPattern_, subdivideDistanceOrigin_, reverseHeight_, exitRoads_, defaultSubdivideSize_, seed_, flipHeight_]:= dwCityPreview[1, 1, {.5,.6}, {.3, .3}, {.25, .25}, {0, 0}, seed, .8, .03, 0, 0, True, finalScale, streetColor, grassColor, concreteColor, ruralArea, subdivideDistanceOrigin, reverseHeight, exitRoads, flipHeight, heightPattern]
dwCityPreview02[streetColor_, grassColor_, concreteColor_, finalScale_, ruralArea_, heightPattern_, subdivideDistanceOrigin_, reverseHeight_, exitRoads_, defaultSubdivideSize_, seed_, flipHeight_]:= dwCityPreview[1, 1, {.5,.6}, {.3, .3}, {.25, .25}, {0, 0}, seed, .8, .03, 0, defaultSubdivideSize, True, finalScale, streetColor, grassColor, concreteColor, ruralArea, subdivideDistanceOrigin, reverseHeight, exitRoads, flipHeight, heightPattern]
dwCityPreview03[streetColor_, grassColor_, concreteColor_, finalScale_, ruralArea_, heightPattern_, subdivideDistanceOrigin_, reverseHeight_, exitRoads_, defaultSubdivideSize_, seed_, flipHeight_]:= dwCityPreview[2, 2, {.5,.6}, {.3, .3}, {.25, .25}, {0, 0}, seed, .8, .03, 0, 0, True, finalScale, streetColor, grassColor, concreteColor, ruralArea, subdivideDistanceOrigin, reverseHeight, exitRoads, flipHeight, heightPattern]
dwCityPreview04[streetColor_, grassColor_, concreteColor_, finalScale_, ruralArea_, heightPattern_, subdivideDistanceOrigin_, reverseHeight_, exitRoads_, defaultSubdivideSize_, seed_, flipHeight_]:= dwCityPreview[2, 2, {.5,.6}, {.3, .3}, {.25, .25}, {0, 0}, seed, .8, .03, 0, defaultSubdivideSize, True, finalScale, streetColor, grassColor, concreteColor, ruralArea, subdivideDistanceOrigin, reverseHeight, exitRoads, flipHeight, heightPattern]
dwCityPreview05[streetColor_, grassColor_, concreteColor_, finalScale_, ruralArea_, heightPattern_, subdivideDistanceOrigin_, reverseHeight_, exitRoads_, defaultSubdivideSize_, seed_, flipHeight_]:= dwCityPreview[3, 3, {.5,.6}, {.3, .3}, {.25, .25}, {0, 0}, seed, .6, .03, 0, 0, True, finalScale, streetColor, grassColor, concreteColor, ruralArea, subdivideDistanceOrigin, reverseHeight, exitRoads, flipHeight, heightPattern]
dwCityPreview06[streetColor_, grassColor_, concreteColor_, finalScale_, ruralArea_, heightPattern_, subdivideDistanceOrigin_, reverseHeight_, exitRoads_, defaultSubdivideSize_, seed_, flipHeight_]:= dwCityPreview[3, 3, {.5,.6}, {.3, .3}, {.25, .25}, {0, 0}, seed, .6, .03, 0, defaultSubdivideSize, True, finalScale, streetColor, grassColor, concreteColor, ruralArea, subdivideDistanceOrigin, reverseHeight, exitRoads, flipHeight, heightPattern]
dwCityPreview07[streetColor_, grassColor_, concreteColor_, finalScale_, ruralArea_, heightPattern_, subdivideDistanceOrigin_, reverseHeight_, exitRoads_, defaultSubdivideSize_, seed_, flipHeight_]:= dwCityPreview[4, 4, {.5,.6}, {.3, .3}, {.25, .25}, {0, 0}, seed, .6, .03, 0, 0, True, finalScale, streetColor, grassColor, concreteColor, ruralArea, subdivideDistanceOrigin, reverseHeight, exitRoads, flipHeight, heightPattern]
dwCityPreview08[streetColor_, grassColor_, concreteColor_, finalScale_, ruralArea_, heightPattern_, subdivideDistanceOrigin_, reverseHeight_, exitRoads_, defaultSubdivideSize_, seed_, flipHeight_]:= dwCityPreview[4, 4, {.5,.6}, {.3, .3}, {.25, .25}, {0, 0}, seed, .6, .03, 0, defaultSubdivideSize, True, finalScale, streetColor, grassColor, concreteColor, ruralArea, subdivideDistanceOrigin, reverseHeight, exitRoads, flipHeight, heightPattern]

dwEvergreenTreePts[]:= {Polygon[{{0., 0.4}, {0.1553, 0.3684}, {0.2825, 0.2825}, {0.3684, 0.1553}, {0.4, 0.}, {0.3684, -0.1553}, {0.2825, -0.2825}, {0.1553, -0.3684}, {0., -0.4}, {-0.1553, -0.3684}, {-0.2825, -0.2825}, {-0.3684, -0.1553}, {-0.4, 0.}, {-0.3684, 0.1553}, {-0.2825, 0.2825}, {-0.1553, 0.3684}, {0., 0.4}}],Polygon[{{0., 0.}, {0.4, 0.}, {0.55, 0.1}, {0.3, 0.4}, {0.4, 0.45}, {0.15, 0.75}, {0.25, 0.8}, {0.05, 1.05}, {0.15, 1.1}, {0., 1.3}}],Polygon[{{0., 0.2}, {0., 1.}}]}
dwBaseballFieldPts[]:= {Polygon[{{-0.2835, 0.1492}, {-0.2835, 0.149}, {-0.2835, -0.314}, {-0.1995, -0.33}, {-0.1205, -0.334}, {-0.0445, -0.328}, {0.0265, -0.311}, {0.0915, -0.284}, {0.1505, -0.248}, {0.2025, -0.204}, {0.2465, -0.152}, {0.2825, -0.093}, {0.3095, -0.027}, {0.3265, 0.044}, {0.3335, 0.12}, {0.3305, 0.2}, {0.3165, 0.284}, {-0.1487, 0.284}, {-0.1625, 0.305}, {-0.1945, 0.326}, {-0.2335, 0.334}, {-0.2725, 0.326}, {-0.3045, 0.305}, {-0.3255, 0.273}, {-0.3335, 0.234}, {-0.3255, 0.195}, {-0.3045, 0.163}}],Polygon[{{-0.1335, 0.234}, {-0.1415, 0.195}, {-0.1625, 0.163}, {-0.1945, 0.142}, {-0.2335, 0.134}, {-0.2335, 0.098}, {-0.2335, 0.061}, {-0.2335, 0.024}, {-0.2335, -0.016}, {-0.1945, -0.024}, {-0.1625, -0.045}, {-0.1415, -0.077}, {-0.1335, -0.116}, {-0.0965, -0.116}, {-0.0585, -0.116}, {-0.0205, -0.116}, {0.0165, -0.116}, {0.0245, -0.077}, {0.0455, -0.045}, {0.0775, -0.024}, {0.1165, -0.016}, {0.1165, 0.024}, {0.1165, 0.062}, {0.1165, 0.099}, {0.1165, 0.134}, {0.0775, 0.142}, {0.0455, 0.163}, {0.0245, 0.195}, {0.0165, 0.234}, {-0.0285, 0.234}, {-0.0775, 0.234}, {-0.1175, 0.234}, {-0.1335, 0.234}}],Polygon[{{0.1465, 0.264}, {0.1465, 0.204}, {0.0865, 0.204}, {0.0865, 0.264}}],Polygon[{{0.1465, -0.086}, {0.1465, -0.146}, {0.0865, -0.146}, {0.0865, -0.086}}],Polygon[{{-0.2035, -0.086}, {-0.2035, -0.146}, {-0.2635, -0.146}, {-0.2635, -0.086}}],Polygon[{{-0.1853, 0.2383}, {-0.2378, 0.1858}, {-0.2648, 0.2113}, {-0.2648, 0.2653}, {-0.2108, 0.2653}}],Polygon[{{0.0165, 0.059}, {0.0106, 0.0299}, {-0.0055, 0.006}, {-0.0294, -0.0101}, {-0.0585, -0.016}, {-0.0876, -0.0101}, {-0.1115, 0.006}, {-0.1276, 0.0299}, {-0.1335, 0.059}, {-0.1276, 0.0881}, {-0.1115, 0.112}, {-0.0876, 0.1281}, {-0.0585, 0.134}, {-0.0294, 0.1281}, {-0.0055, 0.112}, {0.0106, 0.0881}, {0.0165, 0.059}}]}
dwBaseballField2Pts[]:= {Polygon[{{0.2835, -0.1492}, {0.2835, -0.149}, {0.2835, 0.314}, {0.1995, 0.33}, {0.1205, 0.334}, {0.0445, 0.328}, {-0.0265, 0.311}, {-0.0915, 0.284}, {-0.1505, 0.248}, {-0.2025, 0.204}, {-0.2465, 0.152}, {-0.2825, 0.093}, {-0.3095, 0.027}, {-0.3265, -0.044}, {-0.3335, -0.12}, {-0.3305, -0.2}, {-0.3165, -0.284}, {0.1487, -0.284}, {0.1625, -0.305}, {0.1945, -0.326}, {0.2335, -0.334}, {0.2725, -0.326}, {0.3045, -0.305}, {0.3255, -0.273}, {0.3335, -0.234}, {0.3255, -0.195}, {0.3045, -0.163}}], Polygon[{{0.1335, -0.234}, {0.1415, -0.195}, {0.1625, -0.163}, {0.1945, -0.142}, {0.2335, -0.134}, {0.2335, -0.098}, {0.2335, -0.061}, {0.2335, -0.024}, {0.2335, 0.016}, {0.1945, 0.024}, {0.1625, 0.045}, {0.1415, 0.077}, {0.1335, 0.116}, {0.0965, 0.116}, {0.0585, 0.116}, {0.0205, 0.116}, {-0.0165, 0.116}, {-0.0245, 0.077}, {-0.0455, 0.045}, {-0.0775, 0.024}, {-0.1165, 0.016}, {-0.1165, -0.024}, {-0.1165, -0.062}, {-0.1165, -0.099}, {-0.1165, -0.134}, {-0.0775, -0.142}, {-0.0455, -0.163}, {-0.0245, -0.195}, {-0.0165, -0.234}, {0.0285, -0.234}, {0.0775, -0.234}, {0.1175, -0.234}, {0.1335, -0.234}}], Polygon[{{-0.1465, -0.264}, {-0.1465, -0.204}, {-0.0865, -0.204}, {-0.0865, -0.264}}], Polygon[{{-0.1465, 0.086}, {-0.1465, 0.146}, {-0.0865, 0.146}, {-0.0865, 0.086}}], Polygon[{{0.2035, 0.086}, {0.2035, 0.146}, {0.2635, 0.146}, {0.2635, 0.086}}], Polygon[{{0.1853, -0.2383}, {0.2378, -0.1858}, {0.2648, -0.2113}, {0.2648, -0.2653}, {0.2108, -0.2653}}], Polygon[{{-0.0165, -0.059}, {-0.0106, -0.0299}, {0.0055, -0.006}, {0.0294, 0.0101}, {0.0585, 0.016}, {0.0876, 0.0101}, {0.1115, -0.006}, {0.1276, -0.0299}, {0.1335, -0.059}, {0.1276, -0.0881}, {0.1115, -0.112}, {0.0876, -0.1281}, {0.0585, -0.134}, {0.0294, -0.1281}, {0.0055, -0.112}, {-0.0106, -0.0881}, {-0.0165, -0.059}}]}
dwTennisCourtPts[]:= {Polygon[{{-0.9,0.6},{0.9,0.6},{0.9,-0.6},{-0.9,-0.6}}],Polygon[{{-0.8,0.5},{-0.45,0.5},{-0.45,-0.5},{-0.8,-0.5}}],Polygon[{{-0.35,0.5},{0.35,0.5},{0.35,0.05},{-0.35,0.05}}],Polygon[{{-0.35,-0.05},{0.35,-0.05},{0.35,-0.5},{-0.35,-0.5}}],Polygon[{{0.45,0.5},{0.8,0.5},{0.8,-0.5},{0.45,-0.5}}],Polygon[{{-0.6,0.},{-0.6,0.3},{0.6,0.3},{0.6,0.}}]}
dwTennisCourt2Pts[]:= {Polygon[{{-0.6, -0.9}, {-0.6, 0.9}, {0.6, 0.9}, {0.6, -0.9}}], Polygon[{{-0.5, -0.8}, {-0.5, -0.45}, {0.5, -0.45}, {0.5, -0.8}}], Polygon[{{-0.5, -0.35}, {-0.5, 0.35}, {-0.05, 0.35}, {-0.05, -0.35}}], Polygon[{{0.05, -0.35}, {0.05, 0.35}, {0.5, 0.35}, {0.5, -0.35}}], Polygon[{{-0.5, 0.45}, {-0.5, 0.8}, {0.5, 0.8}, {0.5, 0.45}}], Polygon[{{-0.6, 0.}, {-0.6, 0.3}, {0.6, 0.3}, {0.6, 0.}}]}
dwBasketballCourtPts[]:= {Polygon[{{-0.6, -0.9}, {-0.6, 0.9}, {0.6, 0.9}, {0.6, -0.9}}], Polygon[{{-0.1842, 0.0777}, {-0.1657, 0.1116}, {-0.1412, 0.1412}, {-0.1116, 0.1657}, {-0.0777, 0.1842}, {-0.0402, 0.1959}, {0., 0.2}, {0., 0.2}, {0.0402, 0.1959}, {0.0777, 0.1842}, {0.1116, 0.1657}, {0.1412, 0.1412}, {0.1657, 0.1116}, {0.1842, 0.0777}, {0.1938, 0.05}, {-0.1938, 0.05}}], Polygon[{{0.3, 0.7}, {0.2977, 0.6625}, {0.2908, 0.6264}, {0.2797, 0.5919}, {0.2647, 0.5593}, {0.2461, 0.5289}, {0.2241, 0.501}, {0.199, 0.4759}, {0.1711, 0.4539}, {0.1407, 0.4353}, {0.1081, 0.4203}, {0.0736, 0.4092}, {0.0375, 0.4023}, {0., 0.4}, {-0.0375, 0.4023}, {-0.0736, 0.4092}, {-0.1081, 0.4203}, {-0.1407, 0.4353}, {-0.1711, 0.4539}, {-0.199, 0.4759}, {-0.2241, 0.501}, {-0.2461, 0.5289}, {-0.2647, 0.5593}, {-0.2797, 0.5919}, {-0.2908, 0.6264}, {-0.2977, 0.6625}, {-0.3, 0.8}, {-0.5, 0.8}, {-0.5, 0.05}, {-0.3, 0.05}, {-0.2908, 0.0736}, {-0.2797, 0.1081}, {-0.2647, 0.1407}, {-0.2461, 0.1711}, {-0.2241, 0.199}, {-0.199, 0.2241}, {-0.1711, 0.2461}, {-0.1407, 0.2647}, {-0.1081, 0.2797}, {-0.0736, 0.2908}, {-0.0375, 0.2977}, {0., 0.3}, {0.0375, 0.2977}, {0.0736, 0.2908}, {0.1081, 0.2797}, {0.1407, 0.2647}, {0.1711, 0.2461}, {0.199, 0.2241}, {0.2241, 0.199}, {0.2461, 0.1711}, {0.2647, 0.1407}, {0.2797, 0.1081}, {0.2908, 0.0736}, {0.3, 0.05}, {0.5, 0.05}, {0.5, 0.8}, {0.3, 0.8}, {0.3, 0.7}}], Polygon[{{-0.2, 0.7}, {0.2, 0.7}, {0.1959, 0.6598}, {0.1842, 0.6223}, {0.1657, 0.5884}, {0.1412, 0.5588}, {0.1116, 0.5343}, {0.0777, 0.5158}, {0.0402, 0.5041}, {0., 0.5}, {-0.0402, 0.5041}, {-0.0777, 0.5158}, {-0.1116, 0.5343}, {-0.1412, 0.5588}, {-0.1657, 0.5884}, {-0.1842, 0.6223}, {-0.1959, 0.6598}}], Polygon[{{-0.1842, -0.0777}, {-0.1657, -0.1116}, {-0.1412, -0.1412}, {-0.1116, -0.1657}, {-0.0777, -0.1842}, {-0.0402, -0.1959}, {0., -0.2}, {0., -0.2}, {0.0402, -0.1959}, {0.0777, -0.1842}, {0.1116, -0.1657}, {0.1413, -0.1412}, {0.1657, -0.1116}, {0.1842, -0.0777}, {0.1938, -0.05}, {-0.1938, -0.05}}], Polygon[{{0.3, -0.7}, {0.2977, -0.6625}, {0.2908, -0.6264}, {0.2797, -0.5919}, {0.2647, -0.5593}, {0.2461, -0.5289}, {0.2241, -0.501}, {0.199, -0.4759}, {0.1711, -0.4539}, {0.1407, -0.4353}, {0.1081, -0.4203}, {0.0736, -0.4092}, {0.0375, -0.4023}, {0., -0.4}, {-0.0375, -0.4023}, {-0.0736, -0.4092}, {-0.1081, -0.4203}, {-0.1407, -0.4353}, {-0.1711, -0.4539}, {-0.199, -0.4759}, {-0.2241, -0.501}, {-0.2461, -0.5289}, {-0.2647, -0.5593}, {-0.2797, -0.5919}, {-0.2908, -0.6264}, {-0.2977, -0.6625}, {-0.3, -0.8}, {-0.5, -0.8}, {-0.5, -0.05}, {-0.3, -0.05}, {-0.2908, -0.0736}, {-0.2797, -0.1081}, {-0.2647, -0.1407}, {-0.2461, -0.1711}, {-0.2241, -0.199}, {-0.199, -0.2241}, {-0.1711, -0.2461}, {-0.1407, -0.2647}, {-0.1081, -0.2797}, {-0.0736, -0.2908}, {-0.0375, -0.2977}, {0., -0.3}, {0.0375, -0.2977}, {0.0736, -0.2908}, {0.1081, -0.2797}, {0.1407, -0.2647}, {0.1711, -0.2461}, {0.199, -0.2241}, {0.2241, -0.199}, {0.2461, -0.1711}, {0.2647, -0.1407}, {0.2797, -0.1081}, {0.2908, -0.0736}, {0.3, -0.05}, {0.5, -0.05}, {0.5, -0.8}, {0.3, -0.8}, {0.3, -0.7}}], Polygon[{{-0.2, -0.7}, {0.2, -0.7}, {0.1959, -0.6598}, {0.1842, -0.6223}, {0.1657, -0.5884}, {0.1413, -0.5588}, {0.1116, -0.5343}, {0.0777, -0.5158}, {0.0402, -0.5041}, {0., -0.5}, {-0.0402, -0.5041}, {-0.0777, -0.5158}, {-0.1116, -0.5343}, {-0.1412, -0.5588}, {-0.1657, -0.5884}, {-0.1842, -0.6223}, {-0.1959, -0.6598}}], Polygon[{{0.075, 0.3}, {0.075, 0.}, {-0.075, 0.}, {-0.075, 0.3}, {-0.25, 0.3}, {-0.25, 0.475}, {-0.225, 0.525}, {-0.175, 0.575}, {-0.1, 0.6}, {0.1, 0.6}, {0.175, 0.575}, {0.225, 0.525}, {0.25, 0.475}, {0.25, 0.3}}], Polygon[{{0., 0.9}, {0., 0.}}]}
dwBasketballCourt2Pts[]:= {Polygon[{{0.9, -0.6}, {-0.9, -0.6}, {-0.9, 0.6}, {0.9, 0.6}}], Polygon[{{-0.0777, -0.1842}, {-0.1116, -0.1657}, {-0.1412, -0.1412}, {-0.1657, -0.1116}, {-0.1842, -0.0777}, {-0.1959, -0.0402}, {-0.2, 0.}, {-0.2, 0.}, {-0.1959, 0.0402}, {-0.1842, 0.0777}, {-0.1657, 0.1116}, {-0.1412, 0.1412}, {-0.1116, 0.1657}, {-0.0777, 0.1842}, {-0.05, 0.1938}, {-0.05, -0.1938}}], Polygon[{{-0.7, 0.3}, {-0.6625, 0.2977}, {-0.6264, 0.2908}, {-0.5919, 0.2797}, {-0.5593, 0.2647}, {-0.5289, 0.2461}, {-0.501, 0.2241}, {-0.4759, 0.199}, {-0.4539, 0.1711}, {-0.4353, 0.1407}, {-0.4203, 0.1081}, {-0.4092, 0.0736}, {-0.4023, 0.0375}, {-0.4, 0.}, {-0.4023, -0.0375}, {-0.4092, -0.0736}, {-0.4203, -0.1081}, {-0.4353, -0.1407}, {-0.4539, -0.1711}, {-0.4759, -0.199}, {-0.501, -0.2241}, {-0.5289, -0.2461}, {-0.5593, -0.2647}, {-0.5919, -0.2797}, {-0.6264, -0.2908}, {-0.6625, -0.2977}, {-0.8, -0.3}, {-0.8, -0.5}, {-0.05, -0.5}, {-0.05, -0.3}, {-0.0736, -0.2908}, {-0.1081, -0.2797}, {-0.1407, -0.2647}, {-0.1711, -0.2461}, {-0.199, -0.2241}, {-0.2241, -0.199}, {-0.2461, -0.1711}, {-0.2647, -0.1407}, {-0.2797, -0.1081}, {-0.2908, -0.0736}, {-0.2977, -0.0375}, {-0.3, 0.}, {-0.2977, 0.0375}, {-0.2908, 0.0736}, {-0.2797, 0.1081}, {-0.2647, 0.1407}, {-0.2461, 0.1711}, {-0.2241, 0.199}, {-0.199, 0.2241}, {-0.1711, 0.2461}, {-0.1407, 0.2647}, {-0.1081, 0.2797}, {-0.0736, 0.2908}, {-0.05, 0.3}, {-0.05, 0.5}, {-0.8, 0.5}, {-0.8, 0.3}, {-0.7, 0.3}}], Polygon[{{-0.7, -0.2}, {-0.7, 0.2}, {-0.6598, 0.1959}, {-0.6223, 0.1842}, {-0.5884, 0.1657}, {-0.5588, 0.1412}, {-0.5343, 0.1116}, {-0.5158, 0.0777}, {-0.5041, 0.0402}, {-0.5, 0.}, {-0.5041, -0.0402}, {-0.5158, -0.0777}, {-0.5343, -0.1116}, {-0.5588, -0.1412}, {-0.5884, -0.1657}, {-0.6223, -0.1842}, {-0.6598, -0.1959}}], Polygon[{{0.0777, -0.1842}, {0.1116, -0.1657}, {0.1412, -0.1412}, {0.1657, -0.1116}, {0.1842, -0.0777}, {0.1959, -0.0402}, {0.2, 0.}, {0.2, 0.}, {0.1959, 0.0402}, {0.1842, 0.0777}, {0.1657, 0.1116}, {0.1412, 0.1413}, {0.1116, 0.1657}, {0.0777, 0.1842}, {0.05, 0.1938}, {0.05, -0.1938}}], Polygon[{{0.7, 0.3}, {0.6625, 0.2977}, {0.6264, 0.2908}, {0.5919, 0.2797}, {0.5593, 0.2647}, {0.5289, 0.2461}, {0.501, 0.2241}, {0.4759, 0.199}, {0.4539, 0.1711}, {0.4353, 0.1407}, {0.4203, 0.1081}, {0.4092, 0.0736}, {0.4023, 0.0375}, {0.4, 0.}, {0.4023, -0.0375}, {0.4092, -0.0736}, {0.4203, -0.1081}, {0.4353, -0.1407}, {0.4539, -0.1711}, {0.4759, -0.199}, {0.501, -0.2241}, {0.5289, -0.2461}, {0.5593, -0.2647}, {0.5919, -0.2797}, {0.6264, -0.2908}, {0.6625, -0.2977}, {0.8, -0.3}, {0.8, -0.5}, {0.05, -0.5}, {0.05, -0.3}, {0.0736, -0.2908}, {0.1081, -0.2797}, {0.1407, -0.2647}, {0.1711, -0.2461}, {0.199, -0.2241}, {0.2241, -0.199}, {0.2461, -0.1711}, {0.2647, -0.1407}, {0.2797, -0.1081}, {0.2908, -0.0736}, {0.2977, -0.0375}, {0.3, 0.}, {0.2977, 0.0375}, {0.2908, 0.0736}, {0.2797, 0.1081}, {0.2647, 0.1407}, {0.2461, 0.1711}, {0.2241, 0.199}, {0.199, 0.2241}, {0.1711, 0.2461}, {0.1407, 0.2647}, {0.1081, 0.2797}, {0.0736, 0.2908}, {0.05, 0.3}, {0.05, 0.5}, {0.8, 0.5}, {0.8, 0.3}, {0.7, 0.3}}], Polygon[{{0.7, -0.2}, {0.7, 0.2}, {0.6598, 0.1959}, {0.6223, 0.1842}, {0.5884, 0.1657}, {0.5588, 0.1413}, {0.5343, 0.1116}, {0.5158, 0.0777}, {0.5041, 0.0402}, {0.5, 0.}, {0.5041, -0.0402}, {0.5158, -0.0777}, {0.5343, -0.1116}, {0.5588, -0.1412}, {0.5884, -0.1657}, {0.6223, -0.1842}, {0.6598, -0.1959}}], Polygon[{{-0.3, 0.075}, {0., 0.075}, {0., -0.075}, {-0.3, -0.075}, {-0.3, -0.25}, {-0.475, -0.25}, {-0.525, -0.225}, {-0.575, -0.175}, {-0.6, -0.1}, {-0.6, 0.1}, {-0.575, 0.175}, {-0.525, 0.225}, {-0.475, 0.25}, {-0.3, 0.25}}], Polygon[{{-0.9, 0.}, {0., 0.}}]}
dwSoccerFieldPts[]:= {Polygon[{{-0.6, -0.9}, {-0.6, 0.9}, {0.6, 0.9}, {0.6, -0.9}}],Polygon[{{-0.1842, 0.0777}, {-0.1657, 0.1116}, {-0.1412, 0.1412}, {-0.1116, 0.1657}, {-0.0777, 0.1842}, {-0.0402, 0.1959}, {0., 0.2}, {0., 0.2}, {0.0402, 0.1959}, {0.0777, 0.1842}, {0.1116, 0.1657}, {0.1412, 0.1412}, {0.1657, 0.1116}, {0.1842, 0.0777}, {0.1938, 0.05}, {-0.1938, 0.05}}],Polygon[{{-0.3, 0.5}, {-0.3, 0.8}, {-0.5, 0.8}, {-0.5, 0.05}, {-0.3, 0.05}, {-0.2908, 0.0736}, {-0.2797, 0.1081}, {-0.2647, 0.1407}, {-0.2461, 0.1711}, {-0.2241, 0.199}, {-0.199, 0.2241}, {-0.1711, 0.2461}, {-0.1407, 0.2647}, {-0.1081, 0.2797}, {-0.0736, 0.2908}, {-0.0375, 0.2977}, {0., 0.3}, {0.0375, 0.2977}, {0.0736, 0.2908}, {0.1081, 0.2797}, {0.1407, 0.2647}, {0.1711, 0.2461}, {0.199, 0.2241}, {0.2241, 0.199}, {0.2461, 0.1711}, {0.2647, 0.1407}, {0.2797, 0.1081}, {0.2908, 0.0736}, {0.3, 0.05}, {0.5, 0.05}, {0.5, 0.8}, {0.3, 0.8}, {0.3, 0.5}}],Polygon[{{-0.1842, -0.0777}, {-0.1657, -0.1116}, {-0.1412, -0.1412}, {-0.1116, -0.1657}, {-0.0777, -0.1842}, {-0.0402, -0.1959}, {0., -0.2}, {0., -0.2}, {0.0402, -0.1959}, {0.0777, -0.1842}, {0.1116, -0.1657}, {0.1413, -0.1412}, {0.1657, -0.1116}, {0.1842, -0.0777}, {0.1938, -0.05}, {-0.1938, -0.05}}],Polygon[{{-0.2, 0.8}, {-0.1, 0.8}, {-0.1, 0.7}, {0.1, 0.7}, {0.1, 0.8}, {0.2, 0.8}, {0.2, 0.6}, {-0.2, 0.6}}],Polygon[{{-0.3, -0.5}, {-0.3, -0.8}, {-0.5, -0.8}, {-0.5, -0.05}, {-0.3, -0.05}, {-0.2908, -0.0736}, {-0.2797, -0.1081}, {-0.2647, -0.1407}, {-0.2461, -0.1711}, {-0.2241, -0.199}, {-0.199, -0.2241}, {-0.1711, -0.2461}, {-0.1407, -0.2647}, {-0.1081, -0.2797}, {-0.0736, -0.2908}, {-0.0375, -0.2977}, {0., -0.3}, {0.0375, -0.2977}, {0.0736, -0.2908}, {0.1081, -0.2797}, {0.1407, -0.2647}, {0.1711, -0.2461}, {0.199, -0.2241}, {0.2241, -0.199}, {0.2461, -0.1711}, {0.2647, -0.1407}, {0.2797, -0.1081}, {0.2908, -0.0736}, {0.3, -0.05}, {0.5, -0.05}, {0.5, -0.8}, {0.3, -0.8}, {0.3, -0.5}}],Polygon[{{-0.2, -0.8}, {-0.1, -0.8}, {-0.1, -0.7}, {0.1, -0.7}, {0.1, -0.8}, {0.2, -0.8}, {0.2, -0.6}, {-0.2, -0.6}}]}
dwSoccerField2Pts[]:= {Polygon[{{-0.9, 0.6}, {0.9, 0.6}, {0.9, -0.6}, {-0.9, -0.6}}], Polygon[{{0.0777, 0.1842}, {0.1116, 0.1657}, {0.1412, 0.1412}, {0.1657, 0.1116}, {0.1842, 0.0777}, {0.1959, 0.0402}, {0.2, 0.}, {0.2, 0.}, {0.1959, -0.0402}, {0.1842, -0.0777}, {0.1657, -0.1116}, {0.1412, -0.1412}, {0.1116, -0.1657}, {0.0777, -0.1842}, {0.05, -0.1938}, {0.05, 0.1938}}], Polygon[{{0.5, 0.3}, {0.8, 0.3}, {0.8, 0.5}, {0.05, 0.5}, {0.05, 0.3}, {0.0736, 0.2908}, {0.1081, 0.2797}, {0.1407, 0.2647}, {0.1711, 0.2461}, {0.199, 0.2241}, {0.2241, 0.199}, {0.2461, 0.1711}, {0.2647, 0.1407}, {0.2797, 0.1081}, {0.2908, 0.0736}, {0.2977, 0.0375}, {0.3, 0.}, {0.2977, -0.0375}, {0.2908, -0.0736}, {0.2797, -0.1081}, {0.2647, -0.1407}, {0.2461, -0.1711}, {0.2241, -0.199}, {0.199, -0.2241}, {0.1711, -0.2461}, {0.1407, -0.2647}, {0.1081, -0.2797}, {0.0736, -0.2908}, {0.05, -0.3}, {0.05, -0.5}, {0.8, -0.5}, {0.8, -0.3}, {0.5, -0.3}}], Polygon[{{-0.0777, 0.1842}, {-0.1116, 0.1657}, {-0.1412, 0.1412}, {-0.1657, 0.1116}, {-0.1842, 0.0777}, {-0.1959, 0.0402}, {-0.2, 0.}, {-0.2, 0.}, {-0.1959, -0.0402}, {-0.1842, -0.0777}, {-0.1657, -0.1116}, {-0.1412, -0.1413}, {-0.1116, -0.1657}, {-0.0777, -0.1842}, {-0.05, -0.1938}, {-0.05, 0.1938}}], Polygon[{{0.8, 0.2}, {0.8, 0.1}, {0.7, 0.1}, {0.7, -0.1}, {0.8, -0.1}, {0.8, -0.2}, {0.6, -0.2}, {0.6, 0.2}}], Polygon[{{-0.5, 0.3}, {-0.8, 0.3}, {-0.8, 0.5}, {-0.05, 0.5}, {-0.05, 0.3}, {-0.0736, 0.2908}, {-0.1081, 0.2797}, {-0.1407, 0.2647}, {-0.1711, 0.2461}, {-0.199, 0.2241}, {-0.2241, 0.199}, {-0.2461, 0.1711}, {-0.2647, 0.1407}, {-0.2797, 0.1081}, {-0.2908, 0.0736}, {-0.2977, 0.0375}, {-0.3, 0.}, {-0.2977, -0.0375}, {-0.2908, -0.0736}, {-0.2797, -0.1081}, {-0.2647, -0.1407}, {-0.2461, -0.1711}, {-0.2241, -0.199}, {-0.199, -0.2241}, {-0.1711, -0.2461}, {-0.1407, -0.2647}, {-0.1081, -0.2797}, {-0.0736, -0.2908}, {-0.05, -0.3}, {-0.05, -0.5}, {-0.8, -0.5}, {-0.8, -0.3}, {-0.5, -0.3}}], Polygon[{{-0.8, 0.2}, {-0.8, 0.1}, {-0.7, 0.1}, {-0.7, -0.1}, {-0.8, -0.1}, {-0.8, -0.2}, {-0.6, -0.2}, {-0.6, 0.2}}]}
dwPond01Pts[]:= {Polygon[{{0., 1.}, {0.216, 0.9908}, {0.4044, 0.964}, {0.5659, 0.9211}, {0.7009, 0.8633}, {0.8101, 0.7921}, {0.894, 0.7088}, {0.9533, 0.6148}, {0.9884, 0.5114}, {1., 0.4}, {0.9769, 0.2338}, {0.9107, 0.1023}, {0.806, 0.004}, {0.6675, -0.0629}, {0.5, -0.1}, {0.3158, -0.145}, {0.181, -0.223}, {0.0883, -0.3285}, {0.0304, -0.456}, {0., -0.6}, {-0.0342, -0.7682}, {-0.1006, -0.894}, {-0.2167, -0.9727}, {-0.4, -1.}, {-0.4987, -0.9879}, {-0.5954, -0.9515}, {-0.6875, -0.8906}, {-0.7723, -0.8049}, {-0.8475, -0.6944}, {-0.9104, -0.5589}, {-0.9585, -0.3981}, {-0.9892, -0.2118}, {-1., 0.}, {-0.9895, 0.1579}, {-0.9589, 0.3052}, {-0.9096, 0.441}, {-0.8429, 0.5644}, {-0.7601, 0.6743}, {-0.6625, 0.7699}, {-0.5515, 0.8502}, {-0.4284, 0.9143}, {-0.2946, 0.9613}, {-0.1513, 0.9902}, {0., 1.}}], Polygon[{{0., 0.75}, {0.2352, 0.742}, {0.4244, 0.7167}, {0.569, 0.672}, {0.6705, 0.6057}, {0.7304, 0.5157}, {0.75, 0.4}, {0.7029, 0.2618}, {0.5726, 0.1722}, {0.375, 0.125}, {0.2212, 0.087}, {0.0886, 0.0176}, {-0.0213, -0.077}, {-0.1071, -0.1905}, {-0.1671, -0.3169}, {-0.2, -0.45}, {-0.2476, -0.6066}, {-0.3443, -0.6825}, {-0.4625, -0.675}, {-0.5665, -0.6012}, {-0.6336, -0.5162}, {-0.6924, -0.3906}, {-0.7341, -0.22}, {-0.75, 0.}, {-0.7353, 0.1608}, {-0.6927, 0.3061}, {-0.625, 0.4341}, {-0.5348, 0.543}, {-0.4246, 0.6308}, {-0.2969, 0.6958}, {-0.1546, 0.7361}, {0., 0.75}}]}
dwPond02Pts[]:= {Polygon[{{-0.7375, 0.5125}, {-0.7704, 0.6266}, {-0.7469, 0.732}, {-0.6756, 0.825}, {-0.5654, 0.9018}, {-0.425, 0.9587}, {-0.2633, 0.992}, {-0.0889, 0.9978}, {0.0893, 0.9726}, {0.2625, 0.9125}, {0.4001, 0.8399}, {0.5247, 0.7566}, {0.6356, 0.6619}, {0.7318, 0.555}, {0.8125, 0.4352}, {0.8768, 0.3018}, {0.9238, 0.154}, {0.9527, -0.0089}, {0.9625, -0.1875}, {0.947, -0.3296}, {0.9024, -0.4598}, {0.8318, -0.5774}, {0.7383, -0.6818}, {0.6247, -0.7721}, {0.4941, -0.8477}, {0.3496, -0.9078}, {0.1942, -0.9516}, {0.0308, -0.9784}, {-0.1375, -0.9875}, {-0.3666, -0.9705}, {-0.5569, -0.9228}, {-0.7092, -0.8498}, {-0.8246, -0.7567}, {-0.9042, -0.6486}, {-0.9488, -0.531}, {-0.9596, -0.4088}, {-0.9375, -0.2875}, {-0.8572, -0.1694}, {-0.7228, -0.0809}, {-0.5629, -0.0106}, {-0.4058, 0.0527}, {-0.2803, 0.1201}, {-0.2147, 0.203}, {-0.2375, 0.3125}, {-0.34, 0.3811}, {-0.4803, 0.3929}, {-0.6242, 0.4145}, {-0.7375, 0.5125}}], Polygon[{{-0.5028, 0.6358}, {-0.4607, 0.7201}, {-0.3589, 0.786}, {-0.2081, 0.8198}, {-0.0192, 0.8077}, {0.1972, 0.7358}, {0.3281, 0.6641}, {0.4427, 0.5778}, {0.5396, 0.4751}, {0.6174, 0.3539}, {0.6747, 0.2124}, {0.7101, 0.0486}, {0.7222, -0.1392}, {0.7042, -0.271}, {0.653, -0.3887}, {0.5731, -0.4913}, {0.4688, -0.5777}, {0.3447, -0.6467}, {0.2051, -0.6974}, {0.0545, -0.7285}, {-0.1028, -0.7392}, {-0.3555, -0.7117}, {-0.5225, -0.6398}, {-0.6131, -0.5391}, {-0.6368, -0.4254}, {-0.6028, -0.3142}, {-0.5184, -0.2331}, {-0.389, -0.1681}, {-0.2406, -0.1056}, {-0.0993, -0.0323}, {0.009, 0.0651}, {0.0582, 0.2001}, {0.0222, 0.3858}, {-0.0993, 0.4911}, {-0.2798, 0.5248}, {-0.4406, 0.5515}, {-0.5028, 0.6358}}]}
dwPond03Pts[]:= {Polygon[{{-1., 0.}, {-0.9908, 0.216}, {-0.964, 0.4044}, {-0.9211, 0.5659}, {-0.8633, 0.7009}, {-0.7921, 0.8101}, {-0.7088, 0.894}, {-0.6148, 0.9533}, {-0.5114, 0.9884}, {-0.4, 1.}, {-0.2338, 0.9769}, {-0.1023, 0.9107}, {-0.004, 0.806}, {0.0629, 0.6675}, {0.1, 0.5}, {0.145, 0.3158}, {0.223, 0.181}, {0.3285, 0.0883}, {0.456, 0.0304}, {0.6, 0.}, {0.7682, -0.0342}, {0.894, -0.1006}, {0.9727, -0.2167}, {1., -0.4}, {0.9879, -0.4987}, {0.9515, -0.5954}, {0.8906, -0.6875}, {0.8049, -0.7723}, {0.6944, -0.8475}, {0.5589, -0.9104}, {0.3981, -0.9585}, {0.2118, -0.9892}, {0., -1.}, {-0.1579, -0.9895}, {-0.3052, -0.9589}, {-0.441, -0.9096}, {-0.5644, -0.8429}, {-0.6743, -0.7601}, {-0.7699, -0.6625}, {-0.8502, -0.5515}, {-0.9143, -0.4284}, {-0.9613, -0.2946}, {-0.9902, -0.1513}, {-1., 0.}}], Polygon[{{-0.75, 0.}, {-0.742, 0.2352}, {-0.7167, 0.4244}, {-0.672, 0.569}, {-0.6057, 0.6705}, {-0.5157, 0.7304}, {-0.4, 0.75}, {-0.2618, 0.7029}, {-0.1722, 0.5726}, {-0.125, 0.375}, {-0.087, 0.2212}, {-0.0176, 0.0886}, {0.077, -0.0213}, {0.1905, -0.1071}, {0.3169, -0.1671}, {0.45, -0.2}, {0.6066, -0.2476}, {0.6825, -0.3443}, {0.675, -0.4625}, {0.6012, -0.5665}, {0.5162, -0.6336}, {0.3906, -0.6924}, {0.22, -0.7341}, {0., -0.75}, {-0.1608, -0.7353}, {-0.3061, -0.6927}, {-0.4341, -0.625}, {-0.543, -0.5348}, {-0.6308, -0.4246}, {-0.6958, -0.2969}, {-0.7361, -0.1546}, {-0.75, 0.}}]}
dwPond04Pts[]:= {Polygon[{{-0.5059, -0.7338}, {-0.62, -0.7667}, {-0.7254, -0.7432}, {-0.8184, -0.6719}, {-0.8952, -0.5617}, {-0.9521, -0.4213}, {-0.9853, -0.2595}, {-0.9912, -0.0852}, {-0.966, 0.093}, {-0.9059, 0.2662}, {-0.8333, 0.4038}, {-0.75, 0.5284}, {-0.6553, 0.6393}, {-0.5484, 0.7355}, {-0.4286, 0.8162}, {-0.2951, 0.8805}, {-0.1474, 0.9275}, {0.0155, 0.9564}, {0.1941, 0.9662}, {0.3362, 0.9507}, {0.4664, 0.9061}, {0.5841, 0.8356}, {0.6884, 0.742}, {0.7788, 0.6284}, {0.8543, 0.4979}, {0.9144, 0.3533}, {0.9582, 0.1979}, {0.985, 0.0345}, {0.9941, -0.1338}, {0.9771, -0.3629}, {0.9294, -0.5532}, {0.8564, -0.7055}, {0.7633, -0.8209}, {0.6553, -0.9005}, {0.5376, -0.9451}, {0.4154, -0.9559}, {0.2941, -0.9338}, {0.176, -0.8535}, {0.0875, -0.7191}, {0.0172, -0.5591}, {-0.046, -0.4021}, {-0.1135, -0.2766}, {-0.1964, -0.2109}, {-0.3059, -0.2338}, {-0.3745, -0.3363}, {-0.3863, -0.4765}, {-0.4079, -0.6205}, {-0.5059, -0.7338}}], Polygon[{{-0.6292, -0.4991}, {-0.7135, -0.457}, {-0.7794, -0.3552}, {-0.8132, -0.2044}, {-0.8011, -0.0155}, {-0.7292, 0.2009}, {-0.6575, 0.3318}, {-0.5712, 0.4464}, {-0.4684, 0.5433}, {-0.3472, 0.6211}, {-0.2057, 0.6784}, {-0.042, 0.7138}, {0.1458, 0.7259}, {0.2776, 0.7079}, {0.3953, 0.6567}, {0.4979, 0.5768}, {0.5843, 0.4725}, {0.6533, 0.3484}, {0.704, 0.2088}, {0.7351, 0.0582}, {0.7458, -0.0991}, {0.7183, -0.3518}, {0.6464, -0.5188}, {0.5457, -0.6094}, {0.432, -0.6331}, {0.3208, -0.5991}, {0.2398, -0.5147}, {0.1747, -0.3853}, {0.1122, -0.2369}, {0.0389, -0.0956}, {-0.0585, 0.0127}, {-0.1934, 0.0619}, {-0.3792, 0.0259}, {-0.4845, -0.0956}, {-0.5181, -0.2761}, {-0.5448, -0.4368}, {-0.6292, -0.4991}}]}
dwPond05Pts[]:= {Polygon[{{0., -1.}, {-0.216, -0.9908}, {-0.4044, -0.964}, {-0.5659, -0.9211}, {-0.7009, -0.8633}, {-0.8101, -0.7921}, {-0.894, -0.7088}, {-0.9533, -0.6148}, {-0.9884, -0.5114}, {-1., -0.4}, {-0.9769, -0.2338}, {-0.9107, -0.1023}, {-0.806, -0.004}, {-0.6675, 0.0629}, {-0.5, 0.1}, {-0.3158, 0.145}, {-0.181, 0.223}, {-0.0883, 0.3285}, {-0.0304, 0.456}, {0., 0.6}, {0.0342, 0.7682}, {0.1006, 0.894}, {0.2167, 0.9727}, {0.4, 1.}, {0.4987, 0.9879}, {0.5954, 0.9515}, {0.6875, 0.8906}, {0.7723, 0.8049}, {0.8475, 0.6944}, {0.9104, 0.5589}, {0.9585, 0.3981}, {0.9892, 0.2118}, {1., 0.}, {0.9895, -0.1579}, {0.9589, -0.3052}, {0.9096, -0.441}, {0.8429, -0.5644}, {0.7601, -0.6743}, {0.6625, -0.7699}, {0.5515, -0.8502}, {0.4284, -0.9143}, {0.2946, -0.9613}, {0.1513, -0.9902}, {0., -1.}}], Polygon[{{0., -0.75}, {-0.2352, -0.742}, {-0.4244, -0.7167}, {-0.569, -0.672}, {-0.6705, -0.6057}, {-0.7304, -0.5157}, {-0.75, -0.4}, {-0.7029, -0.2618}, {-0.5726, -0.1722}, {-0.375, -0.125}, {-0.2212, -0.087}, {-0.0886, -0.0176}, {0.0213, 0.077}, {0.1071, 0.1905}, {0.1671, 0.3169}, {0.2, 0.45}, {0.2476, 0.6066}, {0.3443, 0.6825}, {0.4625, 0.675}, {0.5665, 0.6012}, {0.6336, 0.5162}, {0.6924, 0.3906}, {0.7341, 0.22}, {0.75, 0.}, {0.7353, -0.1608}, {0.6927, -0.3061}, {0.625, -0.4341}, {0.5348, -0.543}, {0.4246, -0.6308}, {0.2969, -0.6958}, {0.1546, -0.7361}, {0., -0.75}}]}
dwPond06Pts[]:= {Polygon[{{0.7404, -0.5022}, {0.7733, -0.6163}, {0.7498, -0.7217}, {0.6785, -0.8147}, {0.5683, -0.8915}, {0.4279, -0.9484}, {0.2662, -0.9816}, {0.0918, -0.9875}, {-0.0864, -0.9623}, {-0.2596, -0.9022}, {-0.3971, -0.8296}, {-0.5218, -0.7463}, {-0.6327, -0.6516}, {-0.7289, -0.5447}, {-0.8096, -0.4249}, {-0.8739, -0.2914}, {-0.9209, -0.1437}, {-0.9498, 0.0192}, {-0.9596, 0.1978}, {-0.9441, 0.3399}, {-0.8995, 0.4701}, {-0.8289, 0.5878}, {-0.7354, 0.6921}, {-0.6218, 0.7825}, {-0.4912, 0.858}, {-0.3467, 0.9181}, {-0.1913, 0.9619}, {-0.0279, 0.9887}, {0.1404, 0.9978}, {0.3696, 0.9808}, {0.5598, 0.9331}, {0.7121, 0.8601}, {0.8275, 0.767}, {0.9071, 0.659}, {0.9517, 0.5413}, {0.9625, 0.4191}, {0.9404, 0.2978}, {0.8601, 0.1798}, {0.7257, 0.0912}, {0.5658, 0.0209}, {0.4087, -0.0423}, {0.2832, -0.1098}, {0.2176, -0.1926}, {0.2404, -0.3022}, {0.3429, -0.3708}, {0.4832, -0.3826}, {0.6271, -0.4042}, {0.7404, -0.5022}}], Polygon[{{0.5057, -0.6255}, {0.4636, -0.7097}, {0.3618, -0.7757}, {0.211, -0.8095}, {0.0221, -0.7974}, {-0.1943, -0.7255}, {-0.3252, -0.6538}, {-0.4398, -0.5675}, {-0.5367, -0.4647}, {-0.6145, -0.3435}, {-0.6718, -0.202}, {-0.7072, -0.0383}, {-0.7193, 0.1495}, {-0.7012, 0.2813}, {-0.6501, 0.399}, {-0.5702, 0.5016}, {-0.4659, 0.588}, {-0.3418, 0.657}, {-0.2022, 0.7077}, {-0.0516, 0.7389}, {0.1057, 0.7495}, {0.3584, 0.722}, {0.5254, 0.6501}, {0.616, 0.5495}, {0.6397, 0.4357}, {0.6057, 0.3245}, {0.5213, 0.2435}, {0.3919, 0.1784}, {0.2435, 0.1159}, {0.1022, 0.0426}, {-0.0061, -0.0548}, {-0.0553, -0.1897}, {-0.0193, -0.3755}, {0.1022, -0.4808}, {0.2827, -0.5144}, {0.4435, -0.5411}, {0.5057, -0.6255}}]}
dwPond07Pts[]:= {Polygon[{{1., 0.}, {0.9908, -0.216}, {0.964, -0.4044}, {0.9211, -0.5659}, {0.8633, -0.7009}, {0.7921, -0.8101}, {0.7088, -0.894}, {0.6148, -0.9533}, {0.5114, -0.9884}, {0.4, -1.}, {0.2338, -0.9769}, {0.1023, -0.9107}, {0.004, -0.806}, {-0.0629, -0.6675}, {-0.1, -0.5}, {-0.145, -0.3158}, {-0.223, -0.181}, {-0.3285, -0.0883}, {-0.456, -0.0304}, {-0.6, 0.}, {-0.7682, 0.0342}, {-0.894, 0.1006}, {-0.9727, 0.2167}, {-1., 0.4}, {-0.9879, 0.4987}, {-0.9515, 0.5954}, {-0.8906, 0.6875}, {-0.8049, 0.7723}, {-0.6944, 0.8475}, {-0.5589, 0.9104}, {-0.3981, 0.9585}, {-0.2118, 0.9892}, {0., 1.}, {0.1579, 0.9895}, {0.3052, 0.9589}, {0.441, 0.9096}, {0.5644, 0.8429}, {0.6743, 0.7601}, {0.7699, 0.6625}, {0.8502, 0.5515}, {0.9143, 0.4284}, {0.9613, 0.2946}, {0.9902, 0.1513}, {1., 0.}}], Polygon[{{0.75, 0.}, {0.742, -0.2352}, {0.7167, -0.4244}, {0.672, -0.569}, {0.6057, -0.6705}, {0.5157, -0.7304}, {0.4, -0.75}, {0.2618, -0.7029}, {0.1722, -0.5726}, {0.125, -0.375}, {0.087, -0.2212}, {0.0176, -0.0886}, {-0.077, 0.0213}, {-0.1905, 0.1071}, {-0.3169, 0.1671}, {-0.45, 0.2}, {-0.6066, 0.2476}, {-0.6825, 0.3443}, {-0.675, 0.4625}, {-0.6012, 0.5665}, {-0.5162, 0.6336}, {-0.3906, 0.6924}, {-0.22, 0.7341}, {0., 0.75}, {0.1608, 0.7353}, {0.3061, 0.6927}, {0.4341, 0.625}, {0.543, 0.5348}, {0.6308, 0.4246}, {0.6958, 0.2969}, {0.7361, 0.1546}, {0.75, 0.}}]}
dwPond08Pts[]:= {Polygon[{{0.5088, 0.7441}, {0.6229, 0.7771}, {0.7283, 0.7535}, {0.8213, 0.6822}, {0.8981, 0.572}, {0.955, 0.4316}, {0.9882, 0.2699}, {0.9941, 0.0955}, {0.9689, -0.0827}, {0.9088, -0.2559}, {0.8362, -0.3934}, {0.7529, -0.5181}, {0.6582, -0.629}, {0.5513, -0.7252}, {0.4315, -0.8059}, {0.298, -0.8702}, {0.1503, -0.9172}, {-0.0126, -0.9461}, {-0.1912, -0.9559}, {-0.3333, -0.9404}, {-0.4635, -0.8958}, {-0.5812, -0.8252}, {-0.6855, -0.7316}, {-0.7759, -0.6181}, {-0.8514, -0.4875}, {-0.9115, -0.343}, {-0.9553, -0.1876}, {-0.9821, -0.0242}, {-0.9912, 0.1441}, {-0.9742, 0.3733}, {-0.9265, 0.5635}, {-0.8535, 0.7158}, {-0.7604, 0.8312}, {-0.6524, 0.9108}, {-0.5347, 0.9554}, {-0.4125, 0.9662}, {-0.2912, 0.9441}, {-0.1731, 0.8638}, {-0.0846, 0.7294}, {-0.0143, 0.5695}, {0.0489, 0.4125}, {0.1164, 0.2869}, {0.1993, 0.2213}, {0.3088, 0.2441}, {0.3774, 0.3466}, {0.3892, 0.4869}, {0.4108, 0.6308}, {0.5088, 0.7441}}], Polygon[{{0.6321, 0.5094}, {0.7164, 0.4673}, {0.7823, 0.3655}, {0.8161, 0.2147}, {0.804, 0.0258}, {0.7321, -0.1906}, {0.6604, -0.3215}, {0.5741, -0.4361}, {0.4713, -0.533}, {0.3501, -0.6108}, {0.2086, -0.6681}, {0.0449, -0.7035}, {-0.1429, -0.7156}, {-0.2747, -0.6975}, {-0.3924, -0.6464}, {-0.495, -0.5664}, {-0.5814, -0.4622}, {-0.6504, -0.3381}, {-0.7011, -0.1985}, {-0.7322, -0.0479}, {-0.7429, 0.1094}, {-0.7154, 0.3621}, {-0.6435, 0.5291}, {-0.5428, 0.6197}, {-0.4291, 0.6434}, {-0.3179, 0.6094}, {-0.2368, 0.525}, {-0.1718, 0.3956}, {-0.1093, 0.2472}, {-0.036, 0.1059}, {0.0614, -0.0024}, {0.1963, -0.0515}, {0.3821, -0.0156}, {0.4874, 0.1059}, {0.5211, 0.2864}, {0.5477, 0.4472}, {0.6321, 0.5094}}]}
dwParkingLotPts[]:= {Polygon[{{-1., 1.}, {1., 1.}, {1., -1.}, {-1., -1.}}], Polygon[{{-0.425, -0.225}, {-0.125, -0.225}, {-0.125, -0.275}, {-0.425, -0.275}, {-0.425, -0.475}, {-0.125, -0.475}, {-0.125, -0.525}, {-0.425, -0.525}, {-0.425, -0.725}, {-0.125, -0.725}, {-0.125, -0.775}, {-0.775, -0.775}, {-0.775, -0.725}, {-0.475, -0.725}, {-0.475, -0.525}, {-0.775, -0.525}, {-0.775, -0.475}, {-0.475, -0.475}, {-0.475, -0.275}, {-0.775, -0.275}, {-0.775, -0.225}, {-0.475, -0.225}, {-0.475, -0.025}, {-0.775, -0.025}, {-0.775, 0.025}, {-0.475, 0.025}, {-0.475, 0.225}, {-0.775, 0.225}, {-0.775, 0.275}, {-0.475, 0.275}, {-0.475, 0.475}, {-0.775, 0.475}, {-0.775, 0.525}, {-0.475, 0.525}, {-0.475, 0.725}, {-0.775, 0.725}, {-0.775, 0.775}, {-0.125, 0.775}, {-0.125, 0.725}, {-0.425, 0.725}, {-0.425, 0.525}, {-0.125, 0.525}, {-0.125, 0.475}, {-0.425, 0.475}, {-0.425, 0.275}, {-0.125, 0.275}, {-0.125, 0.225}, {-0.425, 0.225}, {-0.425, 0.025}, {-0.125, 0.025}, {-0.125, -0.025}, {-0.425, -0.025}, {-0.425, -0.225}}], Polygon[{{0.475, -0.225}, {0.775, -0.225}, {0.775, -0.275}, {0.475, -0.275}, {0.475, -0.475}, {0.775, -0.475}, {0.775, -0.525}, {0.475, -0.525}, {0.475, -0.725}, {0.775, -0.725}, {0.775, -0.775}, {0.125, -0.775}, {0.125, -0.725}, {0.425, -0.725}, {0.425, -0.525}, {0.125, -0.525}, {0.125, -0.475}, {0.425, -0.475}, {0.425, -0.275}, {0.125, -0.275}, {0.125, -0.225}, {0.425, -0.225}, {0.425, -0.025}, {0.125, -0.025}, {0.125, 0.025}, {0.425, 0.025}, {0.425, 0.225}, {0.125, 0.225}, {0.125, 0.275}, {0.425, 0.275}, {0.425, 0.475}, {0.125, 0.475}, {0.125, 0.525}, {0.425, 0.525}, {0.425, 0.725}, {0.125, 0.725}, {0.125, 0.775}, {0.775, 0.775}, {0.775, 0.725}, {0.475, 0.725}, {0.475, 0.525}, {0.775, 0.525}, {0.775, 0.475}, {0.475, 0.475}, {0.475, 0.275}, {0.775, 0.275}, {0.775, 0.225}, {0.475, 0.225}, {0.475, 0.025}, {0.775, 0.025}, {0.775, -0.025}, {0.475, -0.025}, {0.475, -0.225}}]}
dwParkingLot2Pts[]:= {Polygon[{{-1., 1.}, {1., 1.}, {1., -1.}, {-1., -1.}}], Polygon[{{0.225, -0.425}, {0.225, -0.125}, {0.275, -0.125}, {0.275, -0.425}, {0.475, -0.425}, {0.475, -0.125}, {0.525, -0.125}, {0.525, -0.425}, {0.725, -0.425}, {0.725, -0.125}, {0.775, -0.125}, {0.775, -0.775}, {0.725, -0.775}, {0.725, -0.475}, {0.525, -0.475}, {0.525, -0.775}, {0.475, -0.775}, {0.475, -0.475}, {0.275, -0.475}, {0.275, -0.775}, {0.225, -0.775}, {0.225, -0.475}, {0.025, -0.475}, {0.025, -0.775}, {-0.025, -0.775}, {-0.025, -0.475}, {-0.225, -0.475}, {-0.225, -0.775}, {-0.275, -0.775}, {-0.275, -0.475}, {-0.475, -0.475}, {-0.475, -0.775}, {-0.525, -0.775}, {-0.525, -0.475}, {-0.725, -0.475}, {-0.725, -0.775}, {-0.775, -0.775}, {-0.775, -0.125}, {-0.725, -0.125}, {-0.725, -0.425}, {-0.525, -0.425}, {-0.525, -0.125}, {-0.475, -0.125}, {-0.475, -0.425}, {-0.275, -0.425}, {-0.275, -0.125}, {-0.225, -0.125}, {-0.225, -0.425}, {-0.025, -0.425}, {-0.025, -0.125}, {0.025, -0.125}, {0.025, -0.425}, {0.225, -0.425}}], Polygon[{{0.225, 0.475}, {0.225, 0.775}, {0.275, 0.775}, {0.275, 0.475}, {0.475, 0.475}, {0.475, 0.775}, {0.525, 0.775}, {0.525, 0.475}, {0.725, 0.475}, {0.725, 0.775}, {0.775, 0.775}, {0.775, 0.125}, {0.725, 0.125}, {0.725, 0.425}, {0.525, 0.425}, {0.525, 0.125}, {0.475, 0.125}, {0.475, 0.425}, {0.275, 0.425}, {0.275, 0.125}, {0.225, 0.125}, {0.225, 0.425}, {0.025, 0.425}, {0.025, 0.125}, {-0.025, 0.125}, {-0.025, 0.425}, {-0.225, 0.425}, {-0.225, 0.125}, {-0.275, 0.125}, {-0.275, 0.425}, {-0.475, 0.425}, {-0.475, 0.125}, {-0.525, 0.125}, {-0.525, 0.425}, {-0.725, 0.425}, {-0.725, 0.125}, {-0.775, 0.125}, {-0.775, 0.775}, {-0.725, 0.775}, {-0.725, 0.475}, {-0.525, 0.475}, {-0.525, 0.775}, {-0.475, 0.775}, {-0.475, 0.475}, {-0.275, 0.475}, {-0.275, 0.775}, {-0.225, 0.775}, {-0.225, 0.475}, {-0.025, 0.475}, {-0.025, 0.775}, {0.025, 0.775}, {0.025, 0.475}, {0.225, 0.475}}]}
dwGolfCoursePts[]:= {Polygon[{{-0.607, 0.9746}, {-0.462, 0.9663}, {-0.3171, 0.9459}, {-0.1735, 0.9136}, {-0.0323, 0.8694}, {0.1053, 0.8133}, {0.2381, 0.7456}, {0.3649, 0.6663}, {0.4846, 0.5756}, {0.5959, 0.4734}, {0.6977, 0.36}, {0.7888, 0.2353}, {0.868, 0.0996}, {0.924, -0.0352}, {0.9653, -0.1916}, {0.9877, -0.3585}, {0.9872, -0.5246}, {0.9597, -0.6791}, {0.9011, -0.8107}, {0.8075, -0.9084}, {0.6747, -0.961}, {0.525, -0.9792}, {0.3753, -0.9805}, {0.2299, -0.9616}, {0.0931, -0.9193}, {-0.0308, -0.8503}, {-0.1375, -0.7513}, {-0.2227, -0.6191}, {-0.282, -0.4504}, {-0.3215, -0.273}, {-0.3555, -0.1148}, {-0.3915, 0.0238}, {-0.4371, 0.1425}, {-0.4999, 0.2408}, {-0.5874, 0.3183}, {-0.707, 0.3746}, {-0.8429, 0.446}, {-0.9363, 0.5473}, {-0.9826, 0.6635}, {-0.9774, 0.7795}, {-0.916, 0.8801}, {-0.7941, 0.9501}, {-0.607, 0.9746}}], Polygon[{{0.5243, -0.7254}, {0.4542, -0.7909}, {0.3854, -0.8311}, {0.3192, -0.848}, {0.257, -0.8434}, {0.2002, -0.8194}, {0.1501, -0.7777}, {0.1079, -0.7205}, {0.075, -0.6495}, {0.0528, -0.5667}, {0.0426, -0.4741}, {0.0457, -0.3735}, {0.0565, -0.3109}, {0.078, -0.2459}, {0.1113, -0.181}, {0.1575, -0.1187}, {0.218, -0.0616}, {0.2937, -0.0121}, {0.3859, 0.027}, {0.4728, 0.0435}, {0.5495, 0.0353}, {0.6146, 0.0062}, {0.6665, -0.04}, {0.704, -0.0994}, {0.7253, -0.1683}, {0.7335, -0.2515}, {0.7289, -0.331}, {0.7131, -0.4066}, {0.6878, -0.4784}, {0.6546, -0.5462}, {0.6152, -0.61}, {0.5712, -0.6698}, {0.5243, -0.7254}}], Polygon[{{0.0144, -0.6382}, {0.0146, -0.6886}, {-0.0261, -0.7142}, {-0.0897, -0.6918}, {-0.1578, -0.598}, {-0.1973, -0.5059}, {-0.2251, -0.4245}, {-0.2435, -0.3504}, {-0.2552, -0.2801}, {-0.2625, -0.21}, {-0.268, -0.1367}, {-0.2579, -0.0151}, {-0.2156, 0.0594}, {-0.1538, 0.0932}, {-0.0853, 0.0926}, {-0.0227, 0.064}, {0.0212, 0.0137}, {0.0484, -0.0673}, {0.0455, -0.1375}, {0.0239, -0.2068}, {-0.0049, -0.2852}, {-0.023, -0.355}, {-0.0308, -0.426}, {-0.0276, -0.4974}, {-0.0128, -0.5684}, {0.0144, -0.6382}}], Polygon[{{0.743, 0.1746}, {0.7261, 0.1996}, {0.7066, 0.2244}, {0.6845, 0.2485}, {0.6595, 0.2716}, {0.6316, 0.2934}, {0.6005, 0.3134}, {0.5662, 0.3313}, {0.5284, 0.3468}, {0.487, 0.3594}, {0.4419, 0.3688}, {0.393, 0.3746}, {0.346, 0.3717}, {0.3072, 0.357}, {0.2764, 0.333}, {0.2536, 0.3024}, {0.2388, 0.2679}, {0.232, 0.232}, {0.2332, 0.1974}, {0.2425, 0.1668}, {0.2597, 0.1426}, {0.2848, 0.1277}, {0.318, 0.1246}, {0.3649, 0.1291}, {0.4083, 0.1327}, {0.4488, 0.1349}, {0.4871, 0.1355}, {0.5237, 0.134}, {0.5595, 0.13}, {0.595, 0.1232}, {0.6309, 0.1132}, {0.668, 0.0996}, {0.7146, 0.0903}, {0.7465, 0.1025}, {0.7579, 0.132}, {0.743, 0.1746}}], Polygon[{{-0.5789, 0.6937}, {-0.6003, 0.6157}, {-0.634, 0.5568}, {-0.6758, 0.5202}, {-0.7214, 0.5086}, {-0.7664, 0.525}, {-0.8165, 0.5706}, {-0.8472, 0.6191}, {-0.8603, 0.6691}, {-0.8578, 0.7194}, {-0.8414, 0.7687}, {-0.8029, 0.8087}, {-0.746, 0.8336}, {-0.6914, 0.8437}, {-0.6518, 0.8375}, {-0.611, 0.8126}, {-0.5824, 0.7657}, {-0.5789, 0.6937}}], Polygon[{{0.3364, 0.6166}, {0.2859, 0.6426}, {0.2389, 0.6575}, {0.1964, 0.6625}, {0.159, 0.6591}, {0.1276, 0.6484}, {0.1029, 0.6318}, {0.0859, 0.6107}, {0.0772, 0.5862}, {0.0748, 0.5356}, {0.084, 0.4933}, {0.1024, 0.459}, {0.1277, 0.4327}, {0.1573, 0.4142}, {0.1888, 0.4033}, {0.2199, 0.3998}, {0.2481, 0.4037}, {0.3158, 0.4186}, {0.3697, 0.4229}, {0.4112, 0.4209}, {0.4418, 0.4171}, {0.463, 0.4159}, {0.4761, 0.4215}, {0.4827, 0.4385}, {0.4811, 0.4663}, {0.4689, 0.4959}, {0.447, 0.5265}, {0.4169, 0.5573}, {0.3796, 0.5876}, {0.3364, 0.6166}}], Polygon[{{0.7983, -0.258}, {0.7985, -0.2948}, {0.7967, -0.331}, {0.7927, -0.3666}, {0.7865, -0.4017}, {0.7782, -0.4365}, {0.7675, -0.471}, {0.7546, -0.5053}, {0.7393, -0.5395}, {0.7216, -0.5737}, {0.7014, -0.608}, {0.6788, -0.6424}, {0.6536, -0.6771}, {0.633, -0.7128}, {0.6244, -0.748}, {0.6264, -0.7808}, {0.6377, -0.8093}, {0.657, -0.8319}, {0.683, -0.8466}, {0.7143, -0.8516}, {0.7497, -0.8452}, {0.7878, -0.8255}, {0.8273, -0.7906}, {0.848, -0.7649}, {0.866, -0.7355}, {0.8814, -0.703}, {0.8944, -0.6678}, {0.905, -0.6304}, {0.9135, -0.5913}, {0.9199, -0.551}, {0.9243, -0.5099}, {0.9269, -0.4686}, {0.9279, -0.4275}, {0.9272, -0.3871}, {0.9251, -0.3478}, {0.9216, -0.3102}, {0.917, -0.2747}, {0.9112, -0.2419}, {0.9045, -0.2121}, {0.8847, -0.1646}, {0.8614, -0.1498}, {0.8379, -0.1596}, {0.8173, -0.1862}, {0.803, -0.2216}, {0.7983, -0.258}}]}
dwGolfCourse2Pts[]:= {Polygon[{{0.6121, -0.9805}, {0.4671, -0.9722}, {0.3222, -0.9518}, {0.1785, -0.9195}, {0.0373, -0.8752}, {-0.1003, -0.8192}, {-0.233, -0.7515}, {-0.3599, -0.6722}, {-0.4795, -0.5814}, {-0.5908, -0.4793}, {-0.6926, -0.3658}, {-0.7837, -0.2412}, {-0.8629, -0.1055}, {-0.919, 0.0294}, {-0.9602, 0.1858}, {-0.9826, 0.3526}, {-0.9821, 0.5188}, {-0.9546, 0.6732}, {-0.8961, 0.8048}, {-0.8024, 0.9025}, {-0.6697, 0.9552}, {-0.52, 0.9733}, {-0.3703, 0.9746}, {-0.2248, 0.9557}, {-0.088, 0.9134}, {0.0359, 0.8444}, {0.1426, 0.7454}, {0.2278, 0.6132}, {0.2871, 0.4445}, {0.3265, 0.2672}, {0.3605, 0.109}, {0.3966, -0.0297}, {0.4422, -0.1483}, {0.505, -0.2466}, {0.5924, -0.3241}, {0.7121, -0.3805}, {0.848, -0.4518}, {0.9414, -0.5532}, {0.9877, -0.6694}, {0.9824, -0.7853}, {0.9211, -0.8859}, {0.7992, -0.956}, {0.6121, -0.9805}}], Polygon[{{-0.5193, 0.7196}, {-0.4491, 0.7851}, {-0.3803, 0.8253}, {-0.3141, 0.8421}, {-0.252, 0.8376}, {-0.1952, 0.8135}, {-0.145, 0.7719}, {-0.1028, 0.7146}, {-0.0699, 0.6436}, {-0.0477, 0.5608}, {-0.0375, 0.4682}, {-0.0406, 0.3677}, {-0.0514, 0.3051}, {-0.0729, 0.24}, {-0.1062, 0.1751}, {-0.1525, 0.1128}, {-0.2129, 0.0557}, {-0.2886, 0.0063}, {-0.3809, -0.0329}, {-0.4677, -0.0494}, {-0.5445, -0.0412}, {-0.6095, -0.0121}, {-0.6615, 0.0341}, {-0.6989, 0.0936}, {-0.7203, 0.1625}, {-0.7284, 0.2456}, {-0.7238, 0.3251}, {-0.708, 0.4007}, {-0.6827, 0.4725}, {-0.6495, 0.5403}, {-0.6101, 0.6042}, {-0.5662, 0.6639}, {-0.5193, 0.7196}}], Polygon[{{-0.0093, 0.6323}, {-0.0096, 0.6827}, {0.0312, 0.7084}, {0.0947, 0.686}, {0.1628, 0.5922}, {0.2023, 0.5}, {0.2301, 0.4187}, {0.2486, 0.3446}, {0.2603, 0.2742}, {0.2676, 0.2042}, {0.273, 0.1308}, {0.263, 0.0092}, {0.2207, -0.0653}, {0.1589, -0.0991}, {0.0903, -0.0985}, {0.0277, -0.0699}, {-0.0162, -0.0195}, {-0.0434, 0.0614}, {-0.0404, 0.1316}, {-0.0189, 0.2009}, {0.01, 0.2793}, {0.028, 0.3492}, {0.0358, 0.4202}, {0.0327, 0.4915}, {0.0179, 0.5625}, {-0.0093, 0.6323}}], Polygon[{{-0.7379, -0.1805}, {-0.721, -0.2055}, {-0.7015, -0.2302}, {-0.6794, -0.2544}, {-0.6545, -0.2775}, {-0.6265, -0.2992}, {-0.5954, -0.3193}, {-0.5611, -0.3372}, {-0.5233, -0.3526}, {-0.4819, -0.3652}, {-0.4369, -0.3746}, {-0.3879, -0.3805}, {-0.341, -0.3776}, {-0.3021, -0.3628}, {-0.2713, -0.3389}, {-0.2485, -0.3083}, {-0.2337, -0.2738}, {-0.2269, -0.2379}, {-0.2282, -0.2033}, {-0.2374, -0.1726}, {-0.2546, -0.1485}, {-0.2798, -0.1336}, {-0.3129, -0.1305}, {-0.3599, -0.135}, {-0.4033, -0.1385}, {-0.4438, -0.1408}, {-0.482, -0.1414}, {-0.5187, -0.1398}, {-0.5544, -0.1359}, {-0.5899, -0.1291}, {-0.6259, -0.1191}, {-0.6629, -0.1055}, {-0.7096, -0.0962}, {-0.7415, -0.1084}, {-0.7528, -0.1379}, {-0.7379, -0.1805}}], Polygon[{{0.584, -0.6996}, {0.6053, -0.6215}, {0.6391, -0.5627}, {0.6809, -0.526}, {0.7265, -0.5144}, {0.7715, -0.5309}, {0.8216, -0.5765}, {0.8522, -0.6249}, {0.8653, -0.6749}, {0.8628, -0.7253}, {0.8465, -0.7746}, {0.808, -0.8146}, {0.751, -0.8395}, {0.6965, -0.8496}, {0.6568, -0.8434}, {0.6161, -0.8184}, {0.5874, -0.7716}, {0.584, -0.6996}}], Polygon[{{-0.3313, -0.6225}, {-0.2808, -0.6485}, {-0.2339, -0.6634}, {-0.1913, -0.6684}, {-0.1539, -0.665}, {-0.1225, -0.6543}, {-0.0979, -0.6377}, {-0.0808, -0.6165}, {-0.0721, -0.592}, {-0.0697, -0.5415}, {-0.0789, -0.4991}, {-0.0974, -0.4649}, {-0.1226, -0.4386}, {-0.1522, -0.4201}, {-0.1838, -0.4092}, {-0.2148, -0.4057}, {-0.243, -0.4096}, {-0.3107, -0.4245}, {-0.3646, -0.4287}, {-0.4062, -0.4268}, {-0.4368, -0.423}, {-0.4579, -0.4217}, {-0.471, -0.4274}, {-0.4776, -0.4444}, {-0.4761, -0.4722}, {-0.4638, -0.5018}, {-0.442, -0.5324}, {-0.4118, -0.5632}, {-0.3745, -0.5935}, {-0.3313, -0.6225}}], Polygon[{{-0.7933, 0.2521}, {-0.7935, 0.289}, {-0.7916, 0.3251}, {-0.7876, 0.3607}, {-0.7815, 0.3959}, {-0.7731, 0.4307}, {-0.7625, 0.4652}, {-0.7495, 0.4995}, {-0.7342, 0.5337}, {-0.7165, 0.5678}, {-0.6964, 0.6021}, {-0.6737, 0.6365}, {-0.6485, 0.6712}, {-0.628, 0.7069}, {-0.6194, 0.7421}, {-0.6214, 0.7749}, {-0.6326, 0.8035}, {-0.6519, 0.826}, {-0.6779, 0.8407}, {-0.7092, 0.8458}, {-0.7446, 0.8393}, {-0.7827, 0.8196}, {-0.8223, 0.7848}, {-0.8429, 0.759}, {-0.8609, 0.7296}, {-0.8763, 0.6971}, {-0.8893, 0.6619}, {-0.8999, 0.6245}, {-0.9084, 0.5855}, {-0.9148, 0.5451}, {-0.9192, 0.5041}, {-0.9219, 0.4627}, {-0.9228, 0.4216}, {-0.9221, 0.3812}, {-0.92, 0.342}, {-0.9166, 0.3044}, {-0.9119, 0.2689}, {-0.9062, 0.236}, {-0.8995, 0.2063}, {-0.8797, 0.1588}, {-0.8564, 0.1439}, {-0.8328, 0.1537}, {-0.8122, 0.1803}, {-0.798, 0.2158}, {-0.7933, 0.2521}}]}

End[] (* End Private Context *)

EndPackage[]