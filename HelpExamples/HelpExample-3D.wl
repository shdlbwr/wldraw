(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 



(* ---------- examples ---------- *)
dwAxoBoxProjection[]:=
{"3D box by axis projection","",
dwAxoBoxPreview[],"",
Row[{"1. Choose ", Graphics[{Gray,Polygon[{{0,0},{1,0},{1,1},{0,1}}]},ImageSize->24]," from the object preset popup menu ", dwObjectPresets[]/.{(ImageSize->_)->(ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth})}, "."}],
Row[{"2. Click ", Button[$dwIconObjectDupe,Null,$dwButtonStyle]," twice to create a total of three squares."}],
Row[{"3. Arrange squares then select, click ", Button[$dwIcon3D,Null,$dwButtonStyle]," and set projection for each."}],
Grid[{{Labeled[dwAxoBoxStep1[], "arrange squares"],
	Labeled[dwAxoBoxStep2[], Column[{"z projection", SetterBar["z", {"x","y","z"}]}, Alignment->Center]],
	Labeled[dwAxoBoxStep3[], Column[{"y projection", SetterBar["y", {"x","y","z"}]}, Alignment->Center]],
	Labeled[dwAxoBoxStep4[], Column[{"x projection", SetterBar["x", {"x","y","z"}]}, Alignment->Center]]
}}, Spacings->{2,0}],
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwAxoBoxExtrudePreview[],.5],"Extrusion"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwAxoBoxExtrude[]}, Appearance->None]
}}, Spacings->{2,0}]
}

dwAxoBoxExtrude[]:=
{"3D box by extrusion","",
dwAxoBoxExtrudePreview[],"",
Row[{"1. Choose ", Graphics[{Gray,Polygon[{{0,0},{1,0},{1,1},{0,1}}]},ImageSize->24]," from the object preset popup menu ", dwObjectPresets[]/.{(ImageSize->_)->(ImageSize->{1/3$dwToolWidth, 1/3$dwToolWidth})}, "."}],
Row[{"2. Click ", Button[$dwIcon3D,Null,$dwButtonStyle], " and set AXIS PROJECTION to ", SetterBar["z", {"x","y","z"}], "."}],
"",
Row[{"3. Set EXTRUDE values as shown below then click OK."}],
Show[Import[$dwFileDirectory<>"HelpExamples/axoExtrude.png"],ImageSize->330],
"",
Style["Related examples", Sequence@@$dwHelpSubheadStyle],
Grid[{{
	Button[Style[Column[{Magnify[dwAxoBoxPreview[],.5],"Projection"},Alignment->Center], 10], $dwHelpTopic = {$dwIconHelpExample, dwAxoBoxProjection[]}, Appearance->None]
}}, Spacings->{2,0}]
}



(* ---------- previews ---------- *)
dwAxoBoxPreview[]:=Graphics[{EdgeForm[GrayLevel[0]],GrayLevel[0.8],Polygon[{{0.,0.},{0.3464031798720365,0.10001209197236378},{0.14639108802858816,0.2732076359409653},{-0.20001209184344837,0.1731955439686015}}],Polygon[{{-0.19999301848608722,-0.17322321853539985},{0.000013027359483733236,-0.3464144618353745},{0,0},{-0.2000060458455709,0.17319124329997465}}],Polygon[{{0.000015582698858790378,-0.34640969350718065},{0.3464222532293993,-0.24639457853587737},{0.34640667053054053,0.10001511497130317},{0,-0}}]},ImageSize->{Automatic,100}]
dwAxoBoxExtrudePreview[]:=Graphics[{EdgeForm[GrayLevel[0]],GrayLevel[0.8],Polygon[{{0.,0.},{0.3464031798720365,0.10001209197236378},{0.14639108802858816,0.2732076359409653},{-0.20001209184344837,0.1731955439686015}}],Polygon[{{-0.19999301848608722,-0.17322321853539985},{0.000013027359483733236,-0.3464144618353745},{0,0},{-0.2000060458455709,0.17319124329997465}}],Polygon[{{0.000015582698858790378,-0.34640969350718065},{0.3464222532293993,-0.24639457853587737},{0.34640667053054053,0.10001511497130317},{0,-0}}]},ImageSize->{Automatic,100}]



(* ---------- extras ---------- *)
dwAxoBoxStep1[]:=Graphics[{{GrayLevel[0], AbsolutePointSize[4], AbsoluteThickness[1], Opacity[1], Dashing[{}], CapForm["Round"], JoinForm["Round"], Arrowheads[Medium], 
    StrokeForm[Opacity[0]], EdgeForm[{GrayLevel[0], Opacity[0], AbsoluteThickness[1], Dashing[{}], CapForm["Round"], JoinForm["Round"]}], 
    FaceForm[RGBColor[0.85, 0.85, 0.8500038147554742]], 
    Polygon[{{-0.55, -0.55}, {0.55, -0.55}, {0.55, 0.55}, {-0.55, 0.55}}]}, 
   {GrayLevel[0], AbsolutePointSize[4], AbsoluteThickness[1], Opacity[1], Dashing[{}], CapForm["Round"], JoinForm["Round"], Arrowheads[Medium], 
    StrokeForm[RGBColor[0.9, 0.9, 0.9]], 
    EdgeForm[{RGBColor[0.9, 0.9, 0.9], Opacity[1], AbsoluteThickness[1], Dashing[{}], CapForm["Round"], 
      JoinForm["Round"]}], FaceForm[GrayLevel[0.8]], Line[{{-0.55, -0.5}, {0.55, -0.5}}], Line[{{-0.55, -0.4}, {0.55, -0.4}}], 
    Line[{{-0.55, -0.3}, {0.55, -0.3}}], Line[{{-0.55, -0.2}, {0.55, -0.2}}], 
    Line[{{-0.55, -0.1}, {0.55, -0.1}}], Line[{{-0.55, 0.1}, {0.55, 0.1}}], 
    Line[{{-0.55, 0.2}, {0.55, 0.2}}], Line[{{-0.55, 0.3}, {0.55, 0.3}}], Line[{{-0.55, 0.4}, {0.55, 0.4}}], 
    Line[{{-0.55, 0.5}, {0.55, 0.5}}], Line[{{0.5, -0.55}, {0.5, 0.55}}], 
    Line[{{0.4, -0.55}, {0.4, 0.55}}], Line[{{0.3, -0.55}, 
      {0.3, 0.55}}], Line[{{0.2, -0.55}, {0.2, 0.55}}], 
    Line[{{0.1, -0.55}, {0.1, 0.55}}], 
    Line[{{-0.1, -0.55}, {-0.1, 0.55}}], 
    Line[{{-0.2, -0.55}, {-0.2, 0.55}}], Line[{{-0.3, -0.55}, {-0.3, 0.55}}], 
    Line[{{-0.4, -0.55}, {-0.4, 0.55}}], Line[{{-0.5, -0.55}, {-0.5, 0.55}}]}, 
   {GrayLevel[0], AbsolutePointSize[4], AbsoluteThickness[1], Opacity[1], Dashing[{}], CapForm["Round"], JoinForm["Round"], Arrowheads[Medium], 
    StrokeForm[RGBColor[0.7, 0.7, 0.7]], 
    EdgeForm[{RGBColor[0.7, 0.7, 0.7], Opacity[1], AbsoluteThickness[1], Dashing[{}], CapForm["Round"], 
      JoinForm["Round"]}], FaceForm[GrayLevel[0.8]], Line[{{-0.55, 0}, {0.55, 0}}]}, 
   {GrayLevel[0], AbsolutePointSize[4], AbsoluteThickness[1], Opacity[1], Dashing[{}], CapForm["Round"], JoinForm["Round"], Arrowheads[Medium], 
    StrokeForm[RGBColor[0.6, 0.6, 0.6]], EdgeForm[{RGBColor[0.6, 0.6, 0.6], Opacity[1], AbsoluteThickness[1], Dashing[{}], CapForm["Round"], 
      JoinForm["Round"]}], FaceForm[GrayLevel[0.8]], Line[{{0, -0.55}, {0, 0.55}}]}, 
   {GrayLevel[0], AbsolutePointSize[4], AbsoluteThickness[1], Opacity[1], Dashing[{}], CapForm["Round"], JoinForm["Round"], Arrowheads[Medium], 
    EdgeForm[{GrayLevel[0], Opacity[1], AbsoluteThickness[1], Dashing[{}], CapForm["Round"], JoinForm["Round"]}], FaceForm[GrayLevel[0.8]], 
    Polygon[{{0., 0.}, {0.4, 0.}, {0.4, 0.4}, {0., 0.4}}], Polygon[{{-0.4, -0.4}, {0., -0.4}, {0., 0}, 
      {-0.4, 0}}], Polygon[{{0., -0.4}, {0.4, -0.4}, 
      {0.4, 0}, {0., 0}}]}, {GrayLevel[0], AbsolutePointSize[4], AbsoluteThickness[1], 
    Opacity[1], Dashing[{}], CapForm["Round"], JoinForm["Round"], Arrowheads[{{Small, 1}}], 
    EdgeForm[{GrayLevel[0], Opacity[1], AbsoluteThickness[1], Dashing[{}], CapForm["Round"], JoinForm["Round"]}], FaceForm[GrayLevel[0.8]], 
    Arrow[{{-0.15, 0.15}, {0., 0.}}], 
    Inset[Rotate[Style["(0,0)", LineSpacing -> {0, 12, 10}, FontFamily -> "Source Sans Pro", FontSize -> 11, FontWeight -> Plain, FontSlant -> Plain, 
       FontTracking -> "Plain", FontColor -> GrayLevel[0], FontOpacity -> 1], 0.], {-0.15, 0.15}, {1, -1}]}}, ImageSize -> 100, 
  PlotRange -> {{-0.575, 0.575}, {-0.575, 0.575}}]
  
dwAxoBoxStep2[]:=Graphics[{EdgeForm[Black], GrayLevel[0.8], 
	Polygon[{{0., 0.}, {0.3464031798720365, 0.1}, {0.14639108802858816, 0.2732076359409653}, {-0.2, 0.1731955439686015}}], 
	Polygon[{{-0.4, -0.4}, {0., -0.4}, {0., 0}, {-0.4, 0}}], Polygon[{{0., -0.4}, {0.4, -0.4}, {0.4, 0}, {0., 0}}],
	Black,Text["z",{.1,.15}]}, ImageSize -> 100, PlotRange -> {{-0.425, 0.425}, {-0.425, 0.3}}]
     
dwAxoBoxStep3[]:=Graphics[{EdgeForm[Black], GrayLevel[0.8], 
	Polygon[{{0., 0.}, {0.3464031798720365, 0.10001209197236378}, {0.14639108802858816, 0.2732076359409653}, {-0.2, 0.1731955439686015}}], 
	Polygon[{{-0.2, -0.17322321853539985}, {0.000013027359483733236, -0.3464144618353745}, {0, 0}, {-0.2, 0.17319124329997465}}], Polygon[{{0., -0.4}, {0.4, -0.4}, {0.4, 0}, {0., 0}}],
	Black,Text["y",{-.1,-.1}]}, ImageSize -> 70, PlotRange -> {{-0.225, 0.425}, {-0.425, 0.3}}]
  
dwAxoBoxStep4[]:=Graphics[{EdgeForm[Black], GrayLevel[0.8],
	Polygon[{{0, 0}, {0.3464031798720365, 0.1}, {0.14639108802858816, 0.2732076359409653}, {-0.2, 0.1731955439686015}}], 
	Polygon[{{-0.2, -0.1732232185353998}, {0, -0.3464144618353744}, {0., 0.}, {-0.2, 0.17319124329997465}}], 
	Polygon[{{0, -0.34640969350718065}, {0.3464222532293993, -0.24639457853587737}, {0.34640667053054053, 0.10001511497130317}, {0, 0}}],
	Black,Text["x",{.19,-.13}]}, ImageSize -> 70, PlotRange -> {{-0.22501209184344834, 0.3714222532293993}, {-0.3714144618353744, 0.29820763594096533}}]

End[] (* End Private Context *)

EndPackage[]