(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

dwExportToWSMicon[]:=
	Block[{filename, shortfilename, icongraphics, origin="{0,0}", lineColor, fillColor, linePattern, fillPattern, smoothStr, arrow, outputLine, outputPolygon},
	  		filename=SystemDialogInput["FileSave", "untitled.mo", WindowTitle->"Save SystemModeler model"];
			If[filename =!= $Canceled,
				filename = If[!StringMatchQ[filename,"*.mo"], filename, StringTake[filename, {1,-4}]];
				shortfilename = FileNameTake[filename];
				
				(* prepare WLDraw graphics for export - need Chop to remove scientific notation of zero *)
				icongraphics = 
					Table[
						If[FreeQ[{{}}, $dwP[[n]]] && FreeQ[{Text}, $dwHead[[n]]],
							fillColor = ToString[Round[255(List@@ColorConvert[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,1]],"RGB"])]];
							lineColor = ToString[Round[255(List@@ColorConvert[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,1]],"RGB"])]];
							fillPattern = If[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], FaceForm[_]]][[1]]]][[1,2,1]] == 1, "FillPattern.Solid", "FillPattern.None"];
							linePattern = If[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], StrokeForm[_]]][[1]]]][[1,2,1]] == 1, "LinePattern.Solid", "LinePattern.None"];
							arrow = 
								If[$dwHead[[n]] === Arrow || $dwStyle[[n,2]], 
									Switch[$dwStyle[[n, Flatten[Position[$dwStyle[[n]], Arrowheads[_]]][[1]]]][[1]], {{-$dwStyle[[n,3]], __}}, "{Arrow.Filled,Arrow.None}", {{_, __}}, "{Arrow.None,Arrow.Filled}", {{_, __}, {_, __}}, "{Arrow.Filled,Arrow.Filled}", _, "{Arrow.Filled,Arrow.None}"], 
									"{Arrow.None,Arrow.None}"
								];
							smoothStr = If[MemberQ[{BSplineCurve}, $dwHead[[n]]], "Smooth.Bezier", "Smooth.None"];
							outputLine = "Line(visible=true,origin="<>origin<>",points="<>ToString[Chop[100*$dwP[[n]]]]<>",color="<>lineColor<>",pattern="<>linePattern<>",arrow="<>arrow<>",arrowSize="<>Switch[$dwStyle[[n,3]],Small,"4",Medium,"8",Large,"12",_,"3"]<>",smooth="<>smoothStr<>")";
							outputPolygon = "Polygon(visible=true,origin="<>origin<>",lineColor="<>lineColor<>",fillColor="<>fillColor<>",pattern="<>linePattern<>",fillPattern="<>fillPattern<>",points="<>ToString[Chop[100*$dwP[[n]]]]<>",smooth="<>smoothStr<>")";
							Switch[$dwHead[[n]],
								BSplineCurve,
									If[$dwStyle[[n,1]], outputPolygon, outputLine],
								Line|Arrow,
									outputLine,
								Polygon,
									outputPolygon,
								_,
									Nothing
							],
							Nothing
						],
					{n,Length@$dwP}];
		
				(* update icon of WSM model *)
				Export[filename<>".mo", 
					{
						"model "<>shortfilename, 
						"  annotation(Icon(coordinateSystem(extent={{-100,-100},{100,100}},preserveAspectRatio=true,initialScale=0.1,grid={10,10}),graphics="<>ToString[icongraphics]<>"));",
						"end "<>shortfilename<>";"
					}, 
					{"Text", "Lines"}
				];
			];
	]

End[] (* End Private Context *)

EndPackage[]