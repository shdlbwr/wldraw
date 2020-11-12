(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 
	
dwHead[]:=
	ActionMenu[Dynamic@If[MemberQ[{"plotrange"}, $dwMode],
			"Select Object",
			If[$dwSelected === {},
				"Select Object",
				$dwHead[[If[Length[$dwSelected[[1]]] == 2, $dwSelected[[1, 1]], $dwSelected[[1]]]]]
			]
		], 
		{
			BezierCurve :> (
				dwSetUndo[]; 
				If[$dwP[[If[Length[$dwSelected[[1]]] == 2, $dwSelected[[1, 1]], $dwSelected[[1]]]]] === {},
					
					(* new path *)
					$dwHead[[If[Length[$dwSelected[[1]]] == 2, $dwSelected[[1, 1]], $dwSelected[[1]]]]] = BezierCurve;
					$dwObjectGradients[[If[Length[$dwSelected[[1]]] == 2, $dwSelected[[1, 1]], $dwSelected[[1]]]]] = {},
					
					(* existing path *)
					(* check for points <= 4 changing to BezierCurve; need to do before in case several objects selected *)
					Do[
						If[Length[$dwP[[s]]] <= 4,
							MessageDialog["Need at least 5 points for BezierCurve."];
							Break[]
						],
					{s, If[$dwMode === "point", #[[1]],#]&/@$dwSelected}];
					(* change heads *)
					Do[
						If[Length[$dwP[[s]]] > 4 && FreeQ[$dwCompoundPathLayers, s],
							$dwHead[[s]] = BezierCurve;
							Which[
								(* path ends with before handle - delete last coordinate *)
								Mod[Length[$dwP[[s]]], 3] == 0,
									$dwP[[s]] = $dwP[[s]][[;;-2]],
								(* path ends with main point - delete last two coordinates *)
								Mod[Length[$dwP[[s]]] + 2, 3] == 0,
									$dwP[[s]] = $dwP[[s]][[;;-3]],
								True, Nothing
							];
							$dwObjectGradients[[s]] = {},
							
							Nothing
						],
					{s, If[$dwMode === "point", #[[1]],#]&/@$dwSelected}];
					dwUpdateBoundingBox[If[$dwMode === "point", #[[1]],#]&/@$dwSelected]
					
				]; If[MemberQ[{"text", "point"}, $dwStyleMode], $dwStyleMode = "fill"]
			),
			BSplineCurve :> (dwSetUndo[]; Do[If[FreeQ[$dwCompoundPathLayers, s], $dwHead[[s]] = BSplineCurve; $dwObjectGradients[[s]] = {}, Nothing], {s, If[$dwMode === "point", #[[1]], #]&/@$dwSelected}]; dwUpdateBoundingBox[If[$dwMode === "point", #[[1]], #]&/@$dwSelected]; If[MemberQ[{"text", "point"}, $dwStyleMode], $dwStyleMode = "fill"]),
			Polygon :> (dwSetUndo[]; Do[If[FreeQ[$dwCompoundPathLayers, s], $dwHead[[s]] = Polygon; $dwObjectGradients[[s]] = {}, Nothing], {s, If[$dwMode === "point", #[[1]], #]&/@$dwSelected}]; dwUpdateBoundingBox[If[$dwMode === "point", #[[1]], #]&/@$dwSelected]; If[MemberQ[{"text", "point"}, $dwStyleMode], $dwStyleMode = "fill"]),
			Line :> (dwSetUndo[]; Do[If[FreeQ[$dwCompoundPathLayers, s], $dwHead[[s]] = Line; $dwObjectGradients[[s]] = $dwDefaultGradient, Nothing], {s, If[$dwMode === "point", #[[1]], #]&/@$dwSelected}]; dwUpdateBoundingBox[If[$dwMode === "point", #[[1]], #]&/@$dwSelected]; $dwStyleMode = "stroke"),
			Arrow :> (dwSetUndo[]; Do[If[FreeQ[$dwCompoundPathLayers, s], $dwHead[[s]] = Arrow; $dwObjectGradients[[s]] = {}, Nothing], {s, If[$dwMode === "point", #[[1]], #]&/@$dwSelected}]; dwUpdateBoundingBox[If[$dwMode === "point", #[[1]], #]&/@$dwSelected]; $dwStyleMode = "arrow"),
			Point :> (dwSetUndo[]; Do[If[FreeQ[$dwCompoundPathLayers, s], $dwHead[[s]] = Point; $dwObjectGradients[[s]] = {}, Nothing], {s, If[$dwMode === "point", #[[1]], #]&/@$dwSelected}]; dwUpdateBoundingBox[If[$dwMode === "point", #[[1]], #]&/@$dwSelected]; $dwStyleMode = "point")
		}, Enabled->Dynamic@If[$dwSelected === {} || MemberQ[{Text,Image,"Expression","Text3D"}, $dwHead[[If[Length[$dwSelected[[1]]] == 2, $dwSelected[[1, 1]], $dwSelected[[1]]]]]], False, True],
	BaselinePosition->Scaled[.35], ImageSize->{142, 26}]

End[] (* End Private Context *)

EndPackage[]