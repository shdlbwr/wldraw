(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *)

dwRenderTransformInfo[]:=
	If[$dwMode === "transform" && (Head[MousePosition["Graphics"]] === List && !CurrentValue[$dwCommandKey]), 
		Which[
			MemberQ[{"s1","s3","s5","s7"}, $dwSelectionBoundaryBoxNumber],
				{Inset[Style[" "<>ToString[Round[If[$dwRotateAngle < 0, 2Pi+$dwRotateAngle, $dwRotateAngle]*180/Pi, If[CurrentValue["ShiftKey"], 0.5, $dwRotateStep]]]<>"\[Degree]",$dwTransformInfoFontSize/$dwZoom], 
					MousePosition["Graphics"], {0,-2}, Background->GrayLevel[1,.5]]},
			MemberQ[{"s2","s4","s6","s8"}, $dwSelectionBoundaryBoxNumber],
				{Inset[Style[Row[Insert[100*$dwScale, "% ", {{2}, {3}}]], $dwTransformInfoFontSize/$dwZoom], MousePosition["Graphics"], {0,-2}, Background->GrayLevel[1,.5]]},
			True,
				{}
		],
		$dwSelectionBoundaryBoxNumber = Null;
		{}
	]

End[] (* End Private Context *)

EndPackage[]