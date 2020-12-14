(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwShapeGear[]:=
	CreateDialog[
		DynamicModule[{scale = .4},
			Pane[
				Column[{
					Dynamic@Show[
						
						Graphics[Join[{$dwShapeStyleWireframeColor}, dwRenderWireframe[]]],
						
						Graphics[{
							(* axes and frame *)
							{GrayLevel[.8], InfiniteLine[{{0, -1}, {0, 1}}], InfiniteLine[{{-1, 0}, {1, 0}}], Line[{{-1, -1}, {1, -1}, {1, 1}, {-1, 1}, {-1, -1}}]},
							(* check fit *)
							If[$dwShapeGearOptions["CheckFit"],{Hue[0,1,1,.25],EdgeForm[],Cases[dwGear[Sequence@@Prepend[Normal[$dwShapeGearOptions[]], "Size" -> scale]], _Disk, Infinity][[1]], Hue[0,1,1,1], Rotate[Text[Style["a",18], scale{0,1}, {0,-1}], -2Pi/(.875*$dwShapeGearOptions["Teeth"]), {0,0}]},{}],
							GrayLevel[.7],EdgeForm[Black],If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[Cases[dwGear[Sequence@@Prepend[Normal[$dwShapeGearOptions[]], "Size" -> scale]], _Polygon, Infinity][[1,1]],SplineClosed->True]],Cases[dwGear[Sequence@@Prepend[Normal[$dwShapeGearOptions[]], "Size"->scale]], _Polygon, Infinity][[1]]]
							},ImageSize->{200,200},PlotRange->If[$dwShapeGearOptions["CheckFit"],{{-.25,1.25},{0,1.5}},1.1]],
							
						Background->White],
					Row[{Checkbox[Dynamic@$dwShapeGearOptions["CheckFit"]],"Check tooth fit   ",Checkbox[Dynamic@$dwShapeGearOptions["AutoToothShape"]],"Auto tooth shape   "}],
					Row[{Dynamic@If[$dwShapeGearOptions["CheckFit"], "Pink area should not touch tooth 'a'.", ""]}],
					Row[{PopupMenu[Dynamic@$dwShapeGearHead,{Polygon,BSplineCurve}],Spacer[20],Dynamic@If[$dwShapeGearHead === BSplineCurve, Row[{Checkbox[Dynamic@$dwShapeGearOptions["SharpBSplineCurve"]],"Sharp BSplineCurve"}],Spacer[114]]}],
					Row[{Button["calculate auto tooth shape values",$dwShapeGearOptions["ToothHeight"]=(3/$dwShapeGearOptions["Teeth"]);$dwShapeGearOptions["ToothWidth"]=.1;$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=1/2.5;$dwShapeGearOptions["ToothBaseSize"]=0;$dwShapeGearOptions["ToothPeakSize"]=1.15/$dwShapeGearOptions["Teeth"]^1.58]}],
					Row@{Pane["scale ",ImageSize->50,Alignment->Right],Dynamic@Slider[Dynamic@scale,{.1,1,.01}],Spacer[5],Pane[Dynamic@scale,ImageSize->30,Alignment->Left]},
					Row[{Pane["teeth ",ImageSize->60,Alignment->Right],Slider[Dynamic@$dwShapeGearOptions["Teeth"],{2,32,1}],Spacer[5],Pane[Dynamic@$dwShapeGearOptions["Teeth"],ImageSize->40,Alignment->Left]}],
					Row[{Pane["height ",ImageSize->60,Alignment->Right],Slider[Dynamic@$dwShapeGearOptions["ToothHeight"],{0,1,.01}],Spacer[5],Pane[Dynamic@$dwShapeGearOptions["ToothHeight"],ImageSize->40,Alignment->Left]}],
					Row[{Pane["taper ",ImageSize->60,Alignment->Right],Slider[Dynamic@$dwShapeGearOptions["ToothWidth"],{0,1,.01}],Spacer[5],Pane[Dynamic@$dwShapeGearOptions["ToothWidth"],ImageSize->40,Alignment->Left]}],
					Row[{Pane["mid height ",ImageSize->60,Alignment->Right],Slider[Dynamic@$dwShapeGearOptions["ToothCurveMidPoint"],{0,1,.01}],Spacer[5],Pane[Dynamic@$dwShapeGearOptions["ToothCurveMidPoint"],ImageSize->40,Alignment->Left]}],
					Row[{Pane["mid width ",ImageSize->60,Alignment->Right],Slider[Dynamic@$dwShapeGearOptions["ToothCurve"],{0,1,.01}],Spacer[5],Pane[Dynamic@$dwShapeGearOptions["ToothCurve"],ImageSize->40,Alignment->Left]}],
					Row[{Pane["peak width ",ImageSize->60,Alignment->Right],Slider[Dynamic@$dwShapeGearOptions["ToothPeakSize"],{0,1,.01}],Spacer[5],Pane[Dynamic@$dwShapeGearOptions["ToothPeakSize"],ImageSize->40,Alignment->Left]}],
					Row[{Pane["base width ",ImageSize->60,Alignment->Right],Slider[Dynamic@$dwShapeGearOptions["ToothBaseSize"],{0,1,.01}],Spacer[5],Pane[Dynamic@$dwShapeGearOptions["ToothBaseSize"],ImageSize->40,Alignment->Left]}],
					Grid[{{
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->3,"ToothHeight"->1,"ToothWidth"->.1,"ToothCurveMidPoint"->.5,"ToothCurve"->.4,"ToothPeakSize"->.2,"ToothBaseSize"->0], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=3;$dwShapeGearOptions["ToothHeight"]=1;$dwShapeGearOptions["ToothWidth"]=.1;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=.4;$dwShapeGearOptions["ToothPeakSize"]=.2;$dwShapeGearOptions["ToothBaseSize"]=0,$dwPresetButtonStyle],						
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->5,"ToothHeight"->.62,"ToothWidth"->0,"ToothCurveMidPoint"->0,"ToothCurve"->0,"ToothPeakSize"->0,"ToothBaseSize"->0], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=5;$dwShapeGearOptions["ToothHeight"]=.62;$dwShapeGearOptions["ToothWidth"]=0;
							$dwShapeGearOptions["ToothCurveMidPoint"]=0;$dwShapeGearOptions["ToothCurve"]=0;$dwShapeGearOptions["ToothPeakSize"]=0;$dwShapeGearOptions["ToothBaseSize"]=0,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->8,"ToothHeight"->3/8,"ToothWidth"->.1,"ToothCurveMidPoint"->0.5,"ToothCurve"->0.5,"ToothPeakSize"->0.04,"ToothBaseSize"->0], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=8;$dwShapeGearOptions["ToothHeight"]=3/8;$dwShapeGearOptions["ToothWidth"]=.1;
							$dwShapeGearOptions["ToothCurveMidPoint"]=0.5;$dwShapeGearOptions["ToothCurve"]=0.5;$dwShapeGearOptions["ToothPeakSize"]=0.04;$dwShapeGearOptions["ToothBaseSize"]=0,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->6,"ToothHeight"->1,"ToothWidth"->.3696,"ToothCurveMidPoint"->.5,"ToothCurve"->0,"ToothPeakSize"->0,"ToothBaseSize"->.25], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=6;$dwShapeGearOptions["ToothHeight"]=1;$dwShapeGearOptions["ToothWidth"]=.3696;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=0;$dwShapeGearOptions["ToothPeakSize"]=0;$dwShapeGearOptions["ToothBaseSize"]=.25,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->8,"ToothHeight"->.5,"ToothWidth"->.24,"ToothCurveMidPoint"->0,"ToothCurve"->0,"ToothPeakSize"->0,"ToothBaseSize"->0], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=8;$dwShapeGearOptions["ToothHeight"]=.5;$dwShapeGearOptions["ToothWidth"]=.24;
							$dwShapeGearOptions["ToothCurveMidPoint"]=0;$dwShapeGearOptions["ToothCurve"]=0;$dwShapeGearOptions["ToothPeakSize"]=0;$dwShapeGearOptions["ToothBaseSize"]=0,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->8,"ToothHeight"->.5,"ToothWidth"->.36,"ToothCurveMidPoint"->0,"ToothCurve"->0,"ToothPeakSize"->0,"ToothBaseSize"->0], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=8;$dwShapeGearOptions["ToothHeight"]=.5;$dwShapeGearOptions["ToothWidth"]=.36;
							$dwShapeGearOptions["ToothCurveMidPoint"]=0;$dwShapeGearOptions["ToothCurve"]=0;$dwShapeGearOptions["ToothPeakSize"]=0;$dwShapeGearOptions["ToothBaseSize"]=0,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->8,"ToothHeight"->.4,"ToothWidth"->.2,"ToothCurveMidPoint"->1,"ToothCurve"->1,"ToothPeakSize"->.3,"ToothBaseSize"->.4], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=8;$dwShapeGearOptions["ToothHeight"]=.4;$dwShapeGearOptions["ToothWidth"]=.2;
							$dwShapeGearOptions["ToothCurveMidPoint"]=1;$dwShapeGearOptions["ToothCurve"]=1;$dwShapeGearOptions["ToothPeakSize"]=.3;$dwShapeGearOptions["ToothBaseSize"]=.4,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->12,"ToothHeight"->.36,"ToothWidth"->.15,"ToothCurveMidPoint"->0,"ToothCurve"->0,"ToothPeakSize"->0,"ToothBaseSize"->0], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=12;$dwShapeGearOptions["ToothHeight"]=.36;$dwShapeGearOptions["ToothWidth"]=.15;
							$dwShapeGearOptions["ToothCurveMidPoint"]=0;$dwShapeGearOptions["ToothCurve"]=0;$dwShapeGearOptions["ToothPeakSize"]=0;$dwShapeGearOptions["ToothBaseSize"]=0,$dwPresetButtonStyle]
						},{
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->6,"ToothHeight"->.3,"ToothWidth"->.31,"ToothCurveMidPoint"->0,"ToothCurve"->0,"ToothPeakSize"->.2,"ToothBaseSize"->.6], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=6;$dwShapeGearOptions["ToothHeight"]=.3;$dwShapeGearOptions["ToothWidth"]=.31;
							$dwShapeGearOptions["ToothCurveMidPoint"]=0;$dwShapeGearOptions["ToothCurve"]=0;$dwShapeGearOptions["ToothPeakSize"]=.2;$dwShapeGearOptions["ToothBaseSize"]=.6,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->9,"ToothHeight"->1,"ToothWidth"->0,"ToothCurveMidPoint"->0,"ToothCurve"->1,"ToothPeakSize"->1,"ToothBaseSize"->0], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=9;$dwShapeGearOptions["ToothHeight"]=1;$dwShapeGearOptions["ToothWidth"]=0;
							$dwShapeGearOptions["ToothCurveMidPoint"]=0;$dwShapeGearOptions["ToothCurve"]=1;$dwShapeGearOptions["ToothPeakSize"]=1;$dwShapeGearOptions["ToothBaseSize"]=0,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->12,"ToothHeight"->.6,"ToothWidth"->.37,"ToothCurveMidPoint"->.5,"ToothCurve"->.4,"ToothPeakSize"->0,"ToothBaseSize"->.04], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=12;$dwShapeGearOptions["ToothHeight"]=.6;$dwShapeGearOptions["ToothWidth"]=.37;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=.4;$dwShapeGearOptions["ToothPeakSize"]=0;$dwShapeGearOptions["ToothBaseSize"]=.04,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->16,"ToothHeight"->.5,"ToothWidth"->.8,"ToothCurveMidPoint"->0,"ToothCurve"->1,"ToothPeakSize"->1,"ToothBaseSize"->1], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=16;$dwShapeGearOptions["ToothHeight"]=.5;$dwShapeGearOptions["ToothWidth"]=.8;
							$dwShapeGearOptions["ToothCurveMidPoint"]=0;$dwShapeGearOptions["ToothCurve"]=1;$dwShapeGearOptions["ToothPeakSize"]=1;$dwShapeGearOptions["ToothBaseSize"]=1,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->9,"ToothHeight"->.5,"ToothWidth"->.49,"ToothCurveMidPoint"->1,"ToothCurve"->1,"ToothPeakSize"->1,"ToothBaseSize"->0], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=9;$dwShapeGearOptions["ToothHeight"]=.5;$dwShapeGearOptions["ToothWidth"]=.49;
							$dwShapeGearOptions["ToothCurveMidPoint"]=1;$dwShapeGearOptions["ToothCurve"]=1;$dwShapeGearOptions["ToothPeakSize"]=1;$dwShapeGearOptions["ToothBaseSize"]=0,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->5,"ToothHeight"->0,"ToothWidth"->.37,"ToothCurveMidPoint"->.5,"ToothCurve"->1,"ToothPeakSize"->.35,"ToothBaseSize"->.35], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=5;$dwShapeGearOptions["ToothHeight"]=0;$dwShapeGearOptions["ToothWidth"]=.37;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=1;$dwShapeGearOptions["ToothPeakSize"]=.35;$dwShapeGearOptions["ToothBaseSize"]=.35,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->10,"ToothHeight"->.5,"ToothWidth"->.56,"ToothCurveMidPoint"->1,"ToothCurve"->.6,"ToothPeakSize"->1,"ToothBaseSize"->.3], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=10;$dwShapeGearOptions["ToothHeight"]=.5;$dwShapeGearOptions["ToothWidth"]=.56;
							$dwShapeGearOptions["ToothCurveMidPoint"]=1;$dwShapeGearOptions["ToothCurve"]=.6;$dwShapeGearOptions["ToothPeakSize"]=1;$dwShapeGearOptions["ToothBaseSize"]=.3,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->32,"ToothHeight"->.5,"ToothWidth"->1,"ToothCurveMidPoint"->.5,"ToothCurve"->.8,"ToothPeakSize"->.3,"ToothBaseSize"->1], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=32;$dwShapeGearOptions["ToothHeight"]=.5;$dwShapeGearOptions["ToothWidth"]=1;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=.8;$dwShapeGearOptions["ToothPeakSize"]=.3;$dwShapeGearOptions["ToothBaseSize"]=1,$dwPresetButtonStyle]
						},{
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->2,"ToothHeight"->1,"ToothWidth"->.3696,"ToothCurveMidPoint"->.5,"ToothCurve"->0,"ToothPeakSize"->0,"ToothBaseSize"->.25], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=2;$dwShapeGearOptions["ToothHeight"]=1;$dwShapeGearOptions["ToothWidth"]=.3696;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=0;$dwShapeGearOptions["ToothPeakSize"]=0;$dwShapeGearOptions["ToothBaseSize"]=.25,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->2,"ToothHeight"->.26,"ToothWidth"->.74,"ToothCurveMidPoint"->.5,"ToothCurve"->.4,"ToothPeakSize"->0,"ToothBaseSize"->.75], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=2;$dwShapeGearOptions["ToothHeight"]=.26;$dwShapeGearOptions["ToothWidth"]=.74;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=.4;$dwShapeGearOptions["ToothPeakSize"]=0;$dwShapeGearOptions["ToothBaseSize"]=.75,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->2,"ToothHeight"->.25,"ToothWidth"->.74,"ToothCurveMidPoint"->.5,"ToothCurve"->.4,"ToothPeakSize"->0,"ToothBaseSize"->0], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=2;$dwShapeGearOptions["ToothHeight"]=.25;$dwShapeGearOptions["ToothWidth"]=.74;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=.4;$dwShapeGearOptions["ToothPeakSize"]=0;$dwShapeGearOptions["ToothBaseSize"]=0,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->2,"ToothHeight"->1,"ToothWidth"->0,"ToothCurveMidPoint"->.5,"ToothCurve"->.5,"ToothPeakSize"->.5,"ToothBaseSize"->1], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=2;$dwShapeGearOptions["ToothHeight"]=1;$dwShapeGearOptions["ToothWidth"]=0;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=.5;$dwShapeGearOptions["ToothPeakSize"]=.5;$dwShapeGearOptions["ToothBaseSize"]=1,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->2,"ToothHeight"->.25,"ToothWidth"->.74,"ToothCurveMidPoint"->0,"ToothCurve"->.5,"ToothPeakSize"->.85,"ToothBaseSize"->1], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=2;$dwShapeGearOptions["ToothHeight"]=.25;$dwShapeGearOptions["ToothWidth"]=.74;
							$dwShapeGearOptions["ToothCurveMidPoint"]=0;$dwShapeGearOptions["ToothCurve"]=.5;$dwShapeGearOptions["ToothPeakSize"]=.85;$dwShapeGearOptions["ToothBaseSize"]=1,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->2,"ToothHeight"->1,"ToothWidth"->1,"ToothCurveMidPoint"->.65,"ToothCurve"->.55,"ToothPeakSize"->.5,"ToothBaseSize"->1], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=2;$dwShapeGearOptions["ToothHeight"]=1;$dwShapeGearOptions["ToothWidth"]=1;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.65;$dwShapeGearOptions["ToothCurve"]=.55;$dwShapeGearOptions["ToothPeakSize"]=.5;$dwShapeGearOptions["ToothBaseSize"]=1,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->2,"ToothHeight"->0,"ToothWidth"->1,"ToothCurveMidPoint"->.5,"ToothCurve"->.5,"ToothPeakSize"->.5,"ToothBaseSize"->1], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=2;$dwShapeGearOptions["ToothHeight"]=0;$dwShapeGearOptions["ToothWidth"]=1;
							$dwShapeGearOptions["ToothCurveMidPoint"]=.5;$dwShapeGearOptions["ToothCurve"]=.5;$dwShapeGearOptions["ToothPeakSize"]=.5;$dwShapeGearOptions["ToothBaseSize"]=1,$dwPresetButtonStyle],
						Button[Dynamic@Graphics[If[$dwShapeGearHead===BSplineCurve,FilledCurve[$dwShapeGearHead[#[[1,1]],SplineClosed->True]],#[[1]]]&/@{Cases[dwGear["Teeth"->2,"ToothHeight"->1,"ToothWidth"->1,"ToothCurveMidPoint"->1,"ToothCurve"->.3,"ToothPeakSize"->.7,"ToothBaseSize"->1], _Polygon, Infinity]}, ImageSize->24],
							$dwShapeGearOptions["AutoToothShape"]=False;$dwShapeGearOptions["Teeth"]=2;$dwShapeGearOptions["ToothHeight"]=1;$dwShapeGearOptions["ToothWidth"]=1;
							$dwShapeGearOptions["ToothCurveMidPoint"]=1;$dwShapeGearOptions["ToothCurve"]=.3;$dwShapeGearOptions["ToothPeakSize"]=.7;$dwShapeGearOptions["ToothBaseSize"]=1,$dwPresetButtonStyle]
					}},Spacings->{0,0}],
					Row[{CancelButton[DialogReturn[]],
						DefaultButton[
						(* add dwGear to canvas *)	
						dwNewEmptyLayer["Head"->$dwShapeGearHead];
						$dwStyle[[$dwSelected[[1]]]]=ReplacePart[$dwFullDefaultStyle, 10->True];
						If[$dwShapeGearHead === Polygon, $dwShapeGearOptions[["SharpBSplineCurve"]] = False];
						$dwP[[$dwSelected[[1]]]]=Cases[dwGear["Size"->scale,Sequence@@Normal[$dwShapeGearOptions]], _Polygon, Infinity][[1,1]];
						dwUpdateBoundingBox[$dwSelected[[{1}]]];
						$dwPointQuantity = Length[Flatten[$dwP, 1]];
						DialogReturn[]
					]}]},
				Alignment->Center],
			ImageSize->310]
		],
		Background->LightGray, WindowTitle->"Gear",Modal->True]

Options[dwGear]={"Location"->{0,0},"Size"->.18,"Rotate"->0,"LineWeight"->1,"Color"->LightGray,"Teeth"->16,"ToothHeight"->.2,
	"ToothWidth"->.1,"ToothCurveMidPoint"->0,"ToothCurve"->0,"ToothBaseSize"->0,"ToothPeakSize"->0,"CheckFit"->False,
	"AutoToothShape"->False,"SharpBSplineCurve"->False};

dwGear[OptionsPattern[]]:=
	Block[{pts,inside1,inside2,outside,mid1,mid2,a,loc,rotate,s,lw,c,n,th=1-OptionValue["ToothHeight"],tw,tc,tcm,tbs,tps,checkFit,autoToothShape,head,distance},
	{loc,rotate,s,lw,c,n,tw,tc,tcm,tbs,tps,checkFit,autoToothShape}=
		OptionValue[{"Location","Rotate","Size","LineWeight","Color","Teeth","ToothWidth","ToothCurve","ToothCurveMidPoint","ToothBaseSize","ToothPeakSize","CheckFit","AutoToothShape"}];
	
		(* auto tooth shape *)
		If[autoToothShape,th=1-(3/n);tw=.1;tcm=.5;tc=1/2.5;tbs=0;tps=1.15/n^1.58];
		tw=tw (17/n);
		tbs=Min[tbs,tw];
		
		(* teeth top and bottom *)
		outside=Flatten[Table[{{Sin[a+tps],Cos[a+tps]},{Sin[tw+ a-tps],Cos[tw +a-tps]}},{a,0,2Pi,2Pi/n}],1];
		inside1=Table[th{Sin[a-tbs],Cos[a-tbs]},{a,(2Pi/n)/2,2Pi,2Pi/n}];
		inside2=Table[th{Sin[tw+a+tbs],Cos[tw+a+tbs]},{a,(2Pi/n)/2,2Pi,2Pi/n}];
		
		(* mid points for curve *)
		mid1=Table[(th+tcm(1-th)){Sin[a-(tc tw)],Cos[a-(tc tw)]},{a,(2Pi/n)/2,2Pi,2Pi/n}];
		mid2=Table[(th+tcm(1-th)){Sin[(tc tw)+tw+a],Cos[(tc tw)+tw+a]},{a,(2Pi/n)/2,2Pi,2Pi/n}];
		
		(* assemble points *)
		pts=Riffle[outside,mid1,3];
		pts=Riffle[pts,inside1,4];
		pts=Riffle[pts,inside2,5];
		pts=s Riffle[pts,mid2,6];
		
		(* double point quantity for BSplineCurve *)
		If[OptionValue["SharpBSplineCurve"],
			pts = Riffle[pts, pts];
			pts = Delete[pts, List[#] & /@ Rest[Range[0, Length[pts], 6]]][[2;;-2]]
		];

		(* remove excess points at end *)
		pts=pts[[;;-3]];
		
		(* delete duplicate points *)
		DeleteDuplicates[Round[pts,.0001]];
		
		Rotate[{If[checkFit,distance=Sqrt[Plus[Sequence@@((pts[[1]]-s inside1[[1]])^2)]];{Hue[0,1,1,.25],EdgeForm[],Disk[pts[[2]],distance],Disk[pts[[7]],distance]}],
		Opacity[1],c,CapForm["Round"],EdgeForm[AbsoluteThickness[lw]],Polygon[loc+#&/@pts],{GrayLevel[0,.2],Disk[loc,th .8 s]},Disk[loc,.3s],{GrayLevel[0,.5],Disk[loc,.15s]}},rotate,loc]

	]

End[] (* End Private Context *)

EndPackage[]