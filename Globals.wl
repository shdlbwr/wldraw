(* Wolfram Language Package *)

BeginPackage["WLDraw`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(* saved to WLDraw file *)
WLDraw`$dwP = {}; (* object coordinates *)
WLDraw`$dwAnimate = {}; (* see dwAnimate[] of Objects.m for members*)
WLDraw`$dwHead = {}; (* object heads *)
WLDraw`$dwStyle = {}; (* object styles *)
WLDraw`$dwPlotRange = {{-1, -1}, {1, 1}}; (* graphics boundary *)
WLDraw`$dwTemplate = {Null, 1, {0, 0}, 0, .7}; (* tracing template: graphic, size, location, rotate, fade (fade not .5 - same as bezier handle color if black graphic) *)
WLDraw`$dwGroupLayers = {}; (* object grouping *)
WLDraw`$dwHideLayers = {}; (* hidden objects *)
WLDraw`$dwCompoundPathLayers = {}; (* compound paths *)
WLDraw`$dwLineGradients = {}; (* gradients for Line only *)

(* everything below is not saved to WLDraw file *)

(* file location *)
WLDraw`$dwFileDirectory = DirectoryName[$InputFileName];

(* machine dependent *)
WLDraw`$dwCommandKey = Switch[StringTake[$System, 3], "Mic", "AltKey", _, "CommandKey"];
WLDraw`$dwOptionKey = Switch[StringTake[$System, 3], "Mic", "ControlKey", _, "OptionKey"];
WLDraw`$dwShiftKey = Switch[StringTake[$System, 3], "Mic", "ShiftKey", _, "ShiftKey"];


(* textures *)
WLDraw`$dwObjectGradients = {}; (* gradient fills *)
WLDraw`$dwNumPtsToReplaceObjWithBoxes = 250; (* number of points before switching to single box - 500 causes issues when rotating *)

(* modes *)
WLDraw`$dwMode = "preview";(* anytimemove, canvas, draw, move, point, pointwireframe, plotrange, preview, splitPoint, splitShape1, splitShape2, toggleCornerPt, transform, wireframe, zoomwireframe *)
WLDraw`$dwStyleMode = "stroke";(* fill, stroke, arrow, point, text, image *)
WLDraw`$dwCurrentPreviewMode = "preview"; (* preview or wireframe *)
WLDraw`$dwModeBeforeAction = "preview"; (* used to return mode when changed for actions such as moving canvas *)
WLDraw`$dwDisableTransforms = False; (* to keep objects in place when command key is released prematurely while moving canvas with mouse drag *)
WLDraw`$dwShowImageBlends = False; (* image blends preview slow so set to false if problem; set in parameter dialog *)

(* dynamics *)
WLDraw`$dwSynchronousUpdating = Automatic; (* functions use False to delay graphics updating but alway set back to Automatic when finished *)
WLDraw`$dwSynchronousUpdatingInfo = Automatic; (* functions use False to delay info updating but alway set back to Automatic when finished *)
WLDraw`$dwObjectQuantity = 0; (* for dynamic status bar *)
WLDraw`$dwPointQuantity = 0; (* for dynamic status bar *)

(* canvas *)
WLDraw`$dwOverallSize = 768; (* adjust this to match pasted graphics *)
WLDraw`$dwImageResAdjustment = $dwOverallSize/4 (* scales to proper image size *); 
WLDraw`$dwImageResolution = 72 (* must be 72 to normalize image size for screen resolution *); 
WLDraw`$dwWindowSize = {1000, 806}; (* width, height *)
WLDraw`$dwStyleHeight = 80; (* needed to change if style section height changes *)
WLDraw`$dwToolWidth = 100; (* need to change if tool section width changes *)
WLDraw`$dwOrigin = {0, 0};
WLDraw`$dwOriginStart = {0, 0};
WLDraw`$dwOriginOffset = {0, .125}; (* to center in pane *)
WLDraw`$dwGridStepNone = .003125;
WLDraw`$dwGridStepVeryTiny = .0125; (* used to cause kernel crash *)
WLDraw`$dwGridStepTiny = .025;
WLDraw`$dwGridStepSmall = .05;
WLDraw`$dwGridStepMedium = .1;
WLDraw`$dwGridStepLarge = .2;
WLDraw`$dwGridSize = {{$dwGridStepMedium,0},{0,$dwGridStepMedium}};
WLDraw`$dwGridStep = $dwGridStepMedium;
WLDraw`$dwGridRenderIncrement = 2; (* 2 renders every other grid line; 1 renders all grid lines *)
WLDraw`$dwGrid2DRange = 2; (* 1 creates grid from -1 to 1 *)
WLDraw`$dwGrid3DRange = 1; (* 1 creates grid from -1 to 1 *)
WLDraw`$dwGridSnapMatrix = Flatten[Table[# + y*$dwGridSize[[2]]&/@Flatten[Table[x*-#, {x, -1, 1}]&/@$dwGridSize[[{1}]], 1], {y, -1, 1}], 1]; (* creates 9 point grid *)
WLDraw`$dwConstrainHAngle = 0; (* should always start at zero *)
WLDraw`$dwConstrainVAngle = 0; (* should always start at zero *)
WLDraw`$dwNudgeMultiplier = 5;
WLDraw`$dwCanvasMouseSpeed = 2;
WLDraw`$dwShowGrid = True;
WLDraw`$dwShowAxes = True;
WLDraw`$dwShowPlotRange = True;
WLDraw`$dwShowMouseClickPositions = False;
WLDraw`$dwFixScaleAspectRatioOpenedFile = False;
WLDraw`$dwPointDecreaseRate = .5;
WLDraw`$dwPointSmoothDecreaseRate = 0;
WLDraw`$dwTemplateRender = True;
WLDraw`$dwMousePos = {0,0};
WLDraw`$dwTransformInfoFontSize = 12;
WLDraw`$dwStylesheetTextFormats = {"NoStyle","TR","TI","Notes","TableText","ExampleText","InlineFormula"};(* stylesheet formats for text - replaces all text styles for graphics copied to clipboard *)
WLDraw`$dwFontFamilies = {"Arial","Comic Sans MS", "Helvetica","Source Sans Pro","Source Serif Pro","Source Code Pro","Times"};
WLDraw`$dwCurrentMouseAnnotation = Null; (* check if MouseDown is over empty space for ShiftKey drag selection *)
WLDraw`$dwShowSelectionBox = True; (* needed to hide selection for canvas snapshot *)

(* appearance *)
WLDraw`$dwCanvasBackgroundColor = GrayLevel[.95];
WLDraw`$dwPlotRangeColor = Hue[.125];
WLDraw`$dwSelectionColor = Hue[.58];
WLDraw`$dwStyleControlColor = White;(* highlight color of style navigation icon *)
WLDraw`$dwStyleControlHeadColor = GrayLevel[.7];
WLDraw`$dwHiliteColors = Hue[0, 1, .9];
WLDraw`$dwToolHiliteColor = White;
WLDraw`$dwToolColor = GrayLevel[.65];
WLDraw`$dwButtonBackgroundColor = GrayLevel[.3];
WLDraw`$dwButtonStyle = {Appearance->None, ImageSize->{33,33}, Background->$dwButtonBackgroundColor};
WLDraw`$dwPresetButtonStyle = {Appearance->"Palette", ImageSize->{33,33}, Background->None};
WLDraw`$dwButtonHighlight = GrayLevel[0];
WLDraw`$dwTooltipDelay = .5;
WLDraw`$dwShapeMenuIconSize = 36;
WLDraw`$dwMessageBarText = "";
WLDraw`$dwStyleButtonHeight = 24;

(* animate *)
WLDraw`$dwAnimateHandle = Null;
WLDraw`$dwAnimateClickPt = Null;
WLDraw`$dwAnimatePoints = False;
WLDraw`$dwStartAnimate = {};

(* axonometric *)
WLDraw`$dwAxoRotateOrder = "zxy";
WLDraw`$dwAxoAxisRotation = {0,0,0};(* x, y, z *)
WLDraw`$dwAxoExtrude = False;
WLDraw`$dwAxoExtrudeAmount = .2;
WLDraw`$dwAxoExtrudeOrder = 1;
WLDraw`$dwTilt = 35.2672;
WLDraw`$dwTurn = 45;
WLDraw`$dwAxoProjection = "Top";
WLDraw`$dwAxoVectorColor = Red;
WLDraw`$dwAxoVectorZActive = False;
WLDraw`$dwAxoVectorLoc = {};

(* shapes *)
WLDraw`$dwShapeOptions = {};
WLDraw`$dwShapeStylePreviewOrigin = {0, 0};
WLDraw`$dwShapeStylePreviewOriginStart = {0, 0};
WLDraw`$dwShapeStylePreviewDimensions = {300, 300};
WLDraw`$dwShapeStylePreviewDimensionsMagnify = .75;
WLDraw`$dwShapeStylePreviewDimensionsMagnifyStart = .75;
WLDraw`$dwShapeStylePreviewShowPoints = False;
WLDraw`$dwShapeStylePreviewSize = 140;
WLDraw`$dwShapeStyleMouseSpeed = .5;
WLDraw`$dwShapeStyleWireframeColor = LightGray;
WLDraw`$dwShapeArrayConnectors = {0,.5,.5,0,0,0,.5,.5,.5,0,.5};
WLDraw`$dwShapeArrayCharacters = Evaluate[SeedRandom[2]; RandomSample[CharacterRange["a", "z"], 26]];
WLDraw`$dwShapeArraySeed = 2;
WLDraw`$dwShapeArraySeedAmount = 0;
WLDraw`$dwShapeArrayFontFamily = "Source Sans Pro";
WLDraw`$dwGuillochePresets = {};

(* shapes *)
WLDraw`$dwShapeGearOptions = <|"Location"->{0,0},"Size"->.18,"Rotate"->0,"LineWeight"->1,"Color"->LightGray,"Teeth"->8,"ToothHeight"->3/8,
	"ToothWidth"->.1,"ToothCurveMidPoint"->.5,"ToothCurve"->.5,"ToothPeakSize"->0.04,"ToothBaseSize"->0,"CheckFit"->False,
	"AutoToothShape"->False,"SharpBSplineCurve"->False|>;
WLDraw`$dwShapeGearHead = Polygon;

(* Text3D is automatically created by 3D tool when text is selected; it is a hybrid of Text using the same options; text on path options are ignored*)
(* text - {rotation, string, alignment, 4-11 are style options, 12 is object number of BSplineCurve to follow, 13 is character space on BSplineCurve, 14 is text position on BSplineCurve} / location comes from $dwP *)
WLDraw`$dwDefaultTextStyle = {0, "text", {0,0}, FontFamily->"Source Sans Pro", FontSize->10, FontColor->Black, FontOpacity->1, FontSlant->Plain, FontWeight->Plain, FontTracking->"Plain", LineSpacing->{0,12,10}, None, 1, 0, TextAlignment->Center};
(* if new {character, space before} added below, add character to $dwTextOnCurveKerningChars also *)
WLDraw`$dwTextOnCurveKerning = {{"a",1},{"b",1},{"c",1},{"d",1},{"e",1},{"f",.5},{"g",1},{"h",1},{"i",.4},{"j",.5},{"k",1},{"l",.4},{"m",1.5},{"n",1},{"o",1},{"p",1},{"q",1},{"r",.7},{"s",1},{"t",.5},{"u",1},{"v",1},{"w",1.4},{"x",1},{"y",1},{"z",1},{"A",1.2},{"B",1.2},{"C",1.3},{"D",1.2},{"E",1.2},{"F",1.1},{"G",1.3},{"H",1.3},{"I",.45},{"J",1},{"K",1.2},{"L",1},{"M",1.5},{"N",1.2},{"O",1.3},{"P",1.2},{"Q",1.3},{"R",1.2},{"S",1.2},{"T",1.2},{"U",1.3},{"V",1.3},{"W",1.9},{"X",1.3},{"Y",1.2},{"Z",1.2}};
WLDraw`$dwTextOnCurveKerningChars = Join[CharacterRange["a", "z"], CharacterRange["A", "Z"]];
WLDraw`$dwTextOnCurveKerningOn = True;

(* {rotation, expression, magnify} / location comes from $dwP *)
WLDraw`$dwDefaultExpressionStyle = {0, Graphics3D[{EdgeForm[Hue[.6,.75,1]], Hue[.6,.75,1], PolyhedronData["RhombicHexecontahedron", "Faces"]}, Boxed->False, Lighting->"Neutral", Method->{"ShrinkWrap"->True}], .2};

(* image *)
WLDraw`$dwImageFilterHead = "None";
WLDraw`$dwImageFilterValue = 0;
WLDraw`$dwImageFilterValueMin = 0;
WLDraw`$dwImageFilterValueMax = 0;
WLDraw`$dwImageFilterColor = Black;
WLDraw`$dwImageFilterExtraValue = 0;
WLDraw`$dwImageFilterValuesDefault = {{$dwImageFilterHead, $dwImageFilterValue, $dwImageFilterValueMin, $dwImageFilterValueMax, $dwImageFilterColor, $dwImageFilterExtraValue}};
WLDraw`$dwImageMultipleFilterMax = 5;(* max number of combined filters *)
WLDraw`$dwImageFilterImageDefault = Rasterize[ArrayPlot[RandomReal[1, {20, 20}],ColorFunction->"Rainbow",Frame->False,ImagePadding->None,ImageSize->.1*$dwOverallSize,PlotRange->{0,1},PlotRangePadding->None], ImageResolution->$dwImageResolution];
(* {rotation, image, magnify, filter, remove background} / location comes from $dwP *)
WLDraw`$dwDefaultImageStyle = {0, $dwImageFilterImageDefault, 1, $dwImageFilterValuesDefault, False};
WLDraw`$dwImageFilterSquiggle = 15;
WLDraw`$dwImagePastedMaxSize = 1000;
WLDraw`$dwImageCameraMaxSize = 500;
WLDraw`$dwImageCanvasMaxSize = 500;
WLDraw`$dwImageShadowOpacity = 0.4;
WLDraw`$dwImageShadowBlur = 6;
WLDraw`$dwImageShadowOffset = {0.025, -0.025};
WLDraw`$dwImageShadowSpread = 0;
WLDraw`$dwImageShadowColor = Black;


(* style *)
(* 1. add new globals before WLDraw`$dwDefaultFaceStyle *)
(* 2. increment $dwStyleStart *)
(* 3. place new global in $dwFullDefaultStyle before $dwStrayColor *)
WLDraw`$dwStyleStart = 13; (* part where styles ($dwStrayColor) begin in $dwStyle *)
WLDraw`$dwFilledCurve = True;
WLDraw`$dwShowArrowheads = False;
WLDraw`$dwArrowheadSize = Medium;
WLDraw`$dwArrowheadStartPosition = 0;
WLDraw`$dwArrowheadEndPosition = 1;
WLDraw`$dwArrowheadQuantity = 3; (* must be 3 or greater; used for multiple arrowheads in same direction only *)
WLDraw`$dwArrowheadStyle = "Default";
WLDraw`$dwPointSize = 4;
WLDraw`$dwDefaultGradient = {None, {{0, GrayLevel[1]}, {1/3, GrayLevel[5/6]}, {2/3, GrayLevel[2/3]}, {1, GrayLevel[0.5]}}, 0, {0,1}};(* {type, values, degrees rotation, range} *)
WLDraw`$dwDefaultGradientParameters = {0, 0, 0, 0}; (* okay to increase list quantity; parameters are used differently for each gradient *)
WLDraw`$dwDefaultGradientPad = 20; (* image size increase/removal to remove dark edge of blurs *)
WLDraw`$dwDefaultSplineClosed = False;
WLDraw`$dwDefaultSplineDegree = 3;
WLDraw`$dwStrayColor = Black;
WLDraw`$dwStrayOpacity = Opacity[1];
WLDraw`$dwDefaultFaceStyle = FaceForm[{GrayLevel[.8], Opacity[1]}];
WLDraw`$dwDefaultStrokeStyle = StrokeForm[{GrayLevel[0], Opacity[1], AbsoluteThickness[1], AbsoluteDashing[{}], CapForm["Round"], JoinForm["Round"]}];
WLDraw`$dwDefaultArrowStyle = Arrowheads[$dwArrowheadSize];
WLDraw`$dwDefaultPointStyle = AbsolutePointSize[$dwPointSize];
(*	functions rely on order of globals before $dwStrayColor which they use as variables; add new variables before $dwStrayColor and advance $dwStyleStart above;
	$dwStrayColor and $dwStrayOpacity must be 1 and 2 after $dwStyleStart; 
	order of $dwDefaultFaceStyle to end not important *)
WLDraw`$dwFullDefaultStyle = {$dwFilledCurve, $dwShowArrowheads, $dwArrowheadSize, $dwArrowheadStartPosition, 
	$dwArrowheadEndPosition, $dwArrowheadStyle, $dwPointSize, $dwDefaultGradient, $dwDefaultGradientParameters, 
	$dwDefaultSplineClosed, $dwDefaultSplineDegree, $dwArrowheadQuantity, 
	$dwStrayColor, $dwStrayOpacity, $dwDefaultFaceStyle, $dwDefaultStrokeStyle, $dwDefaultArrowStyle, $dwDefaultPointStyle};
				
(* arrows *)
WLDraw`$dwSMArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[4.5 (#+{.4,0})] & /@ {{0, 0}, {-2, 0.55}, {-1.85, 0}, {-2, -0.55}}]}, ImageSize->18];
WLDraw`$dwMLArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[8 (#+{.8,0})] & /@ {{0, 0}, {-2, 0.55}, {-1.85, 0}, {-2, -0.55}}]}, ImageSize->18];
WLDraw`$dwXLArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[12 (#+{1,0})] & /@ {{0, 0}, {-2, 0.55}, {-1.85, 0}, {-2, -0.55}}]}, ImageSize->26];
WLDraw`$dwXXLArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[15 (#+{1,0})] & /@ {{0, 0}, {-2, 0.55}, {-1.85, 0}, {-2, -0.55}}]}, ImageSize->36];
WLDraw`$dwSOpenArrowhead = Graphics[Line[Offset[6 #] & /@ {{-1, 1/2}, {0, 0}, {-1, -1/2}}], ImageSize->12];
WLDraw`$dwSMOpenArrowhead = Graphics[Line[Offset[9 #] & /@ {{-1, 1/2}, {0, 0}, {-1, -1/2}}], ImageSize->18];
WLDraw`$dwMOpenArrowhead = Graphics[Line[Offset[12 #] & /@ {{-1, 1/2}, {0, 0}, {-1, -1/2}}], ImageSize->18];
WLDraw`$dwMLOpenArrowhead = Graphics[Line[Offset[15 #] & /@ {{-1, 1/2}, {0, 0}, {-1, -1/2}}], ImageSize->18];
WLDraw`$dwLOpenArrowhead = Graphics[Line[Offset[18 #] & /@ {{-1, 1/2}, {0, 0}, {-1, -1/2}}], ImageSize->21];
WLDraw`$dwXLOpenArrowhead = Graphics[Line[Offset[21 #] & /@ {{-1, 1/2}, {0, 0}, {-1, -1/2}}], ImageSize->26];
WLDraw`$dwXXLOpenArrowhead = Graphics[Line[Offset[24 #] & /@ {{-1, 1/2}, {0, 0}, {-1, -1/2}}], ImageSize->36];
WLDraw`$dwSShortArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[6 #] & /@ {{0, 1/2}, {1, 0}, {0, -1/2}}]}, ImageSize->12];
WLDraw`$dwSMShortArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[9 #] & /@ {{0, 1/2}, {1, 0}, {0, -1/2}}]}, ImageSize->18];
WLDraw`$dwMShortArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[12 #] & /@ {{0, 1/2}, {1, 0}, {0, -1/2}}]}, ImageSize->18];
WLDraw`$dwMLShortArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[15 #] & /@ {{0, 1/2}, {1, 0}, {0, -1/2}}]}, ImageSize->18];
WLDraw`$dwLShortArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[18 #] & /@ {{0, 1/2}, {1, 0}, {0, -1/2}}]}, ImageSize->21];
WLDraw`$dwXLShortArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[21 #] & /@ {{0, 1/2}, {1, 0}, {0, -1/2}}]}, ImageSize->26];
WLDraw`$dwXXLShortArrowhead = Graphics[{EdgeForm[{}],Polygon[Offset[24 #] & /@ {{0, 1/2}, {1, 0}, {0, -1/2}}]}, ImageSize->36];

(* settings *)
WLDraw`$dwPStart = Null;
WLDraw`$dwDefaultHead = Line;
WLDraw`$dwMinClosedBezierDistance = .025;
WLDraw`$dwMouseDownOnExistingBezierPoint = False;
WLDraw`$dwDupeOffset = {$dwGridStepMedium, $dwGridStepMedium};
WLDraw`$dwDupeQuantity = 1;
WLDraw`$dwShapeSymbols = {Line, Arrow, Polygon, BezierCurve, BSplineCurve, Point};
WLDraw`$dwShapeSymbolsAlternates = Line | Arrow | Polygon | BezierCurve | BSplineCurve | Point;
WLDraw`$dwStyleSymbols = {Opacity, FaceForm, StrokeForm, EdgeForm, CapForm, JoinForm, Dashing, Thickness, AbsoluteDashing, AbsoluteThickness, Arrowheads, GrayLevel, RGBColor, Hue, LABColor, LCHColor, LUVColor, XYZColor};
WLDraw`$dwDiscretizeResolution = 40;
WLDraw`$dwBlendResolution = 60;
WLDraw`$dwRoundCorner = .05;
WLDraw`$dwMaxNumberOfBlends = 50;
WLDraw`$dwStyleContentStart = {}; (* needed for Text rotation *)
WLDraw`$dwOffsetSize = .02;
WLDraw`$dwOffsetRes = .03;
WLDraw`$dwOffsetRemovePts = True;
WLDraw`$dwShapeMapDrawPreview = False;
WLDraw`$dwShapeMapContinent="NorthAmerica";
WLDraw`$dwShapeMapCountry="UnitedStates";
WLDraw`$dwShapeMapRegion=All;
WLDraw`$dwShapeMapProjection="LambertAzimuthal";
WLDraw`$dwPointIncreaseMoveAmount = 0;
WLDraw`$dwPointIncreaseQuantity = 1;
WLDraw`$dwPointEqualIncreaseQuantity = 30;
WLDraw`$dwPointEqualIncrease = False;
WLDraw`$dwConnectorForm = 1;
WLDraw`$dwColorFill = True;
WLDraw`$dwColorPalette="default";
WLDraw`$dwObjectMouseClickPad = .025;

(* selection *)
WLDraw`$dwClickPt = Null;
WLDraw`$dwClickPtNoGrid = Null;
WLDraw`$dwPreviousClickPtNoGrid = Null;
WLDraw`$dwPrevious2ClickPtNoGrid = Null;
WLDraw`$dwClickPtScaled = Null;
WLDraw`$dwSelected = {};
WLDraw`$dwSelectedPlotRange = {Null};
WLDraw`$dwBoundingBoxes = {}; (* list of bounding boxes for all objects *)
WLDraw`$dwBoundingBoxesStart = {};
WLDraw`$dwSelectionBoundary = {};
WLDraw`$dwSelectionBoundaryStart = {};
WLDraw`$dwSelectionBoundaryBoxNumber = Null;
WLDraw`$dwSelectionBoundaryPad = .025;
WLDraw`$dwRotateStart = 0;
WLDraw`$dwRotateAngle = 0;
WLDraw`$dwRotateStep = 5;
WLDraw`$dwSelectionCenter = {0,0};
WLDraw`$dwScale = {1,1};
WLDraw`$dwScaleStep = 5;
WLDraw`$dwTransformEach = False;
WLDraw`$dwTransformOrigin = {Center, Center};
WLDraw`$dwTransformEachCenter = {0,0};
WLDraw`$dwPointModeSelections = {};
WLDraw`$dwShearDirection = "Horizontal";
WLDraw`$dwShear = 0;
WLDraw`$dwDragSelectStart = Null;
WLDraw`$dwDragSelectEnd = Null;
WLDraw`$dwSmallObjectSize = .01;
WLDraw`$dwLargeObjectSize = .2;
WLDraw`$dwLimitPointSelection = True; (* always true since handles only render for selected object *)
WLDraw`$dwMaxSplineMovePoints = 100; (* max moving points of BSplineCurve or BezierCurve before switching to box for quicker movement *)
WLDraw`$dwDrawNewObject = False; (* default is always False; used to decide if path should be closed or extended when drawing *)

(* city builder *)
WLDraw`$dwCBbuildings = {};
WLDraw`$dwCBstreets = {};
WLDraw`$dwCBfinalLandPts = {};
WLDraw`$dwCBlandPts = {};
WLDraw`$dwCBconcretePts = {};
WLDraw`$dwCBlandPtsNoRural = {};
WLDraw`$dwCBheightSize = {};
WLDraw`$dwCBheightPattern = {};
WLDraw`$dwCBcityTreeIndexList = {};
WLDraw`$dwCBriverFlow = {};
WLDraw`$dwCBriverFinalPts = {};
WLDraw`$dwCBriverToBuildingArray = {};
WLDraw`$dwCBminMaxHeight = {.1,.3};
WLDraw`$dwCBscaleAll = 1;
WLDraw`$dwCBrandomTreeCtr = 0;
WLDraw`$dwCBbuildingCount = 0;
WLDraw`$dwCBwindowCount = 0;
WLDraw`$dwCBroofCount = 0;
WLDraw`$dwCBflatRoof1Count = 0;
WLDraw`$dwCBflatRoof2Count = 0;
WLDraw`$dwCBflatRoof3Count = 0;
WLDraw`$dwCBextraRoofCount = 0;
WLDraw`$dwCBtreeCount = 0;
WLDraw`$dwCBtreeObjCount = 0;
WLDraw`$dwCBdoorCount = 0;
WLDraw`$dwCBoverallBirdCount = 0;
WLDraw`$dwCBlineCount = 0; (* use this as a general counter since its total is not multiplied and nothing depends on it *)

(* help *)
WLDraw`$dwHelpTools = "";
WLDraw`$dwHelpTopic = {};
WLDraw`$dwHelpButtonNumber = 1;
WLDraw`$dwHelpBackgroundColor = GrayLevel[.7];
WLDraw`$dwHelpExampleColor = Darker[Orange,.2];
WLDraw`$dwHelpExampleTextSize = 12;
WLDraw`$dwHelpSubheadColor = Hue[.58,1,.8];
WLDraw`$dwHelpSubheadStyle = {15, Bold, $dwHelpSubheadColor};

(* zoom *)
WLDraw`$dwZoom = 1;
WLDraw`$dwZoomStartValue = 1;
WLDraw`$dwZoomStart = Null;
WLDraw`$dwZoomEnd = Null;
WLDraw`$dwZoomSteps = {.25, .5, .75, 1, 1.5, 2, 4, 8, 16}; (* 16 causes 3D nudge icon buttons to resize *)
WLDraw`$dwZoomMax = 1600;(* used with $dwCanvasMouseSpeed for zoom mouse dragging calculations *)

(* undo *)
WLDraw`$dwUndo = {dwGetCurrentState[]};
WLDraw`$dwRedo = {};
WLDraw`$dwUndoLimit = 100;

End[] (* End Private Context *)

EndPackage[]