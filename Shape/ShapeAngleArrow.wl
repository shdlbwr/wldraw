(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

Options[dwAngleArrow]={"Location"->{0,0},"Size"->.15,"Segment"->{0,3Pi/2},"BreakWidthNumber"->0,"BreakCenter"->.5,
	"ArrowPosition"->"End","ArrowSize"->Small,"LineWeight"->1,"Resolution"->180,"Color"->Black,"ShowSegments"->{1,2}};

dwAngleArrow[OptionsPattern[]]:=
	Block[{temp=0,pts,loc,scale,segment,arrowSize,arrowPos,thickness,res,color,bWidth,bCtr,segment2={},p1, p2, p3, showSegs},
		{loc,scale,segment,arrowSize,arrowPos,thickness,res,color,bWidth,bCtr,showSegs}=
		OptionValue[{"Location","Size","Segment","ArrowSize","ArrowPosition","LineWeight","Resolution","Color","BreakWidthNumber","BreakCenter","ShowSegments"}];
			
		(* use mouseclicks for position *)
		If[$dwShowMouseClickPositions && $dwPrevious2ClickPtNoGrid =!= Null,
			{p1, p2, p3} = {Round[$dwClickPtNoGrid, $dwGridStep], Round[$dwPreviousClickPtNoGrid, $dwGridStep], Round[$dwPrevious2ClickPtNoGrid, $dwGridStep]};
			loc = p3;
			segment = 
				{
					(Pi/2)-(Quiet@ToPolarCoordinates[Subtract[p2,p3]]/.{Indeterminate->0})[[2]],
					(Pi/2)-(Quiet@ToPolarCoordinates[Subtract[p1,p3]]/.{Indeterminate->0})[[2]]
				};
			segment2 = 
				{
					(Pi/2)-(Quiet@ToPolarCoordinates[Subtract[p2,p3]]/.{Indeterminate->0})[[2]],
					If[(Pi/2)-(Quiet@ToPolarCoordinates[Subtract[p1,p3]]/.{Indeterminate->0})[[2]] < 
						(Pi/2)-(Quiet@ToPolarCoordinates[Subtract[p2,p3]]/.{Indeterminate->0})[[2]],
						(5Pi/2)-(Quiet@ToPolarCoordinates[Subtract[p1,p3]]/.{Indeterminate->0})[[2]],
						-(3Pi/2)-(Quiet@ToPolarCoordinates[Subtract[p1,p3]]/.{Indeterminate->0})[[2]]
					]
				}
		];
		
		(* returns arrow in each direction if mouseclicks used for position and ShowSegments is {1,2}; otherwise just one direction *)
		Flatten[Table[
			++temp;
			bWidth = Min[.2Abs[Subtract[Sequence@@seg]], bWidth];
			bWidth = If[bWidth==0,.0001,If[seg[[1]]<seg[[2]] || temp == 2,bWidth,-bWidth]];
			bWidth = If[temp == 2,-bWidth,bWidth];
			bCtr = Max[Min[1-(Abs[bWidth]/Abs[Subtract[Sequence@@seg]]), bCtr], (Abs[bWidth]/Abs[Subtract[Sequence@@seg]])];
			pts=If[seg[[1]]<seg[[2]],Pi/res,-Pi/res];
			{
				StrokeForm[{color, Opacity[1], AbsoluteThickness[thickness], AbsoluteDashing[{}], CapForm["Round"], JoinForm["Round"]}],
				Sequence@@If[Abs[bWidth] == .0001,
					{
						Switch[arrowPos,
							"Both", Arrowheads[{{-arrowSize},{arrowSize}}],
							"Start", Arrowheads[{{-arrowSize, 0}}],
							"End", Arrowheads[arrowSize],
							_, {}
						],
						If[arrowPos==="None",Line,Arrow][Table[loc +scale{Sin[x],Cos[x]},{x,seg[[1]],seg[[2]],pts}]]
					},
					{
						Arrowheads[arrowSize],
						If[arrowPos==="Both"||arrowPos==="Start",
							Arrow[Table[loc +scale{Sin[x],Cos[x]},{x,seg[[1]]+bCtr(seg[[2]]-seg[[1]])-bWidth,seg[[1]],-pts}]],
							Line[Table[loc +scale{Sin[x],Cos[x]},{x,seg[[1]]+bCtr(seg[[2]]-seg[[1]])-bWidth,seg[[1]],-pts}]]
						],
						If[arrowPos==="Both"||arrowPos==="End",
							Arrow[Table[loc +scale{Sin[x],Cos[x]},{x,seg[[1]]+bCtr(seg[[2]]-seg[[1]])+bWidth,seg[[2]],pts}]],
							Line[Table[loc +scale{Sin[x],Cos[x]},{x,seg[[1]]+bCtr(seg[[2]]-seg[[1]])+bWidth,seg[[2]],pts}]]
						]
					}
				]
			}, 
		{seg, If[segment2 === {}, {segment}, Switch[showSegs, {1}, {segment}, {2}, {segment2}, _, {segment, segment2}]]}]]
	]

End[] (* End Private Context *)

EndPackage[]