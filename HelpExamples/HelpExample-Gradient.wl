(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 



(* ---------- examples ---------- *)
dwExampleGradientArrow[]:=
{"Gradient - Arrow","",
dwExampleGradientArrowPreview[],"",
Row[{"1. Choose ", Graphics[{Gray,Disk[]},ImageSize->24]," from the object preset popup menu ", dwObjectPresets[]/.{(ImageSize->_)->(ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth})}, "."}],
Row[{"2. Choose ", Graphics[{Thickness[1],CapForm["Butt"],Line[{{.1,0},{.1,1}},VertexColors->{Gray,GrayLevel[.95]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]," from the style preset popup menu ", dwStylePresets[], " to add gradient."}],
Row[{"3. Choose ", $dwIconPresetNoEdge, " from ", dwStylePresets[], " to remove edge."}],
Row[{"4. Choose ", $dwIconPolygonArrow/.{GrayLevel[.7]->Gray}, " from ", dwObjectPresets[]/.{(ImageSize->_)->(ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth})}, " then click 'OK'."}],
Row[{"5. Click ", Button[$dwIconCenterObject, Null, $dwButtonStyle], " to center arrow."}],
Row[{"6. Choose ", Graphics[{Thickness[1],CapForm["Butt"],Line[{{.1,0},{.1,1}},VertexColors->{GrayLevel[.1],GrayLevel[.75]}]},ImageSize->{45,15},ImagePadding->None,AspectRatio->1/3]," from ", dwStylePresets[], " to add gradient."}],
Row[{"7. Choose ", $dwIconPresetNoEdge, " from ", dwStylePresets[], " to remove edge."}]
}



(* ---------- previews ---------- *)
dwExampleGradientArrowPreview[]:= Graphics[{Texture[Graphics[{Opacity[1],Thickness[1],Line[{{0.5,0},{0.5,1/3},{0.5,2/3},{0.5,1}},VertexColors->{GrayLevel[0.5],GrayLevel[0.65],GrayLevel[0.8],GrayLevel[0.95]}]},ImagePadding->0,PlotRangePadding->0]],StrokeForm[Opacity[0]],EdgeForm[{GrayLevel[0],Opacity[0],AbsoluteThickness[1],Dashing[{}],CapForm["Round"],JoinForm["Round"]}],FaceForm[GrayLevel[0.8]],FilledCurve[{BezierCurve[{{0.,0.2},{0.11,0.2},{0.2,0.11},{0.2,0.},{0.2,-0.11},{0.11,-0.2},{0.,-0.2},{-0.11,-0.2},{-0.2,-0.11},{-0.2,0.},{-0.2,0.11},{-0.11,0.2},{0.,0.2}}]},VertexTextureCoordinates->{{0.5,1.},{0.775,1.},{1.,0.775},{1.,0.5},{1.,0.225},{0.775,0.},{0.5,0.},{0.225,0.},{0.,0.225},{0.,0.5},{0.,0.775},{0.225,1.},{0.5,1.}}],{Texture[Graphics[{Opacity[1],Thickness[1],Line[{{0.5,0},{0.5,1/3},{0.5,2/3},{0.5,1}},VertexColors->{GrayLevel[0.1],GrayLevel[0.31666666666666665],GrayLevel[0.5333333333333333],GrayLevel[0.75]}]},ImagePadding->0,PlotRangePadding->0]],Polygon[{{0.125,0.},{0.,-0.125},{0.,-0.0625},{-0.125,-0.0625},{-0.125,0.0625},{0.,0.0625},{0.,0.125}},VertexTextureCoordinates->{{1.,0.5},{0.5,0.},{0.5,0.25},{0.,0.25},{0.,0.75},{0.5,0.75},{0.5,1.}}]}},ImageSize->100]

End[] (* End Private Context *)

EndPackage[]