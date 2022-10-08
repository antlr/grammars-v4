/*
* [The "BSD license"]
*
*  Copyright (c) 2014-2017 THALES GLOBAL SERVICES. All Rights Reserved
*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions
*  are met:
*
*  1. Redistributions of source code must retain the above copyright
*     notice, this list of conditions and the following disclaimer.
*  2. Redistributions in binary form must reproduce the above copyright
*     notice, this list of conditions and the following disclaimer in the
*     documentation and/or other materials provided with the distribution.
*  3. The name of the author may not be used to endorse or promote products
*     derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
*  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
*  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
*  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
*  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
*  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
grammar EDIF300;

goal : edif EOF ;

absolute : '(absolute'
	integerExpression
	')';

acLoad : '(acLoad'
	capacitanceValue
	')';

acLoadDisplay : '(acLoadDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

acLoadFactorCapacitance : capacitanceValue;

acLoadFactorTime : timeValue;

addDisplay : '(addDisplay'
	(  display  )*
	')';

ampere : '(ampere'
	unitExponent
	')';

and_ : '(and'
	(  booleanExpression  )*
	')';

angleValue : numberValue;

annotate : '(annotate'
	stringValue
	(  display  )*
	')';

approvedDate : '(approvedDate'
	date
	')';

approvedDateDisplay : '(approvedDateDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

arc : '(arc'
	startPoint
	throughPoint
	endPoint
	')';

ascii : '(ascii'
	')';

associatedInterconnectNameDisplay : '(associatedInterconnectNameDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

attachmentPoint : pointValue;

author : '(author'
	stringToken
	')';

backgroundColor : '(backgroundColor'
	color
	')';

baselineJustify : '(baselineJustify'
	')';

becomes : '(becomes'
	( logicNameRef | logicList | logicOneOf )
	')';

behaviorView : '(behaviorView'
	viewNameDef
	(  comment  |  nameInformation | userData
	)*
	')';

bidirectional : '(bidirectional'
	')';

bidirectionalPort : '(bidirectionalPort'
	(  bidirectionalPortAttributes  )?
	')';

bidirectionalPortAttributes : '(bidirectionalPortAttributes'
	(  dcFanInLoad  |  dcFanOutLoad  |  dcMaxFanIn  |  dcMaxFanOut  )*
	')';

bitOrder : '(bitOrder'
	( lsbToMsb | msbToLsb )
	')';

blue : scaledInteger;

boldStyle : '(boldStyle'
	')';

eboolean : '(boolean'
	booleanExpression
	')';

booleanConstant : '(booleanConstant'
	constantNameDef
	booleanToken
	')';

booleanConstantRef : '(booleanConstantRef'
	constantNameRef
	')';

booleanExpression : ( and_  | booleanParameterRef | booleanToken | stringEqual | integerEqual | lessThan | lessThanOrEqual | not_ | or_ | xor_ | booleanConstantRef )
;
booleanMap : '(booleanMap'
	booleanValue
	')';

booleanParameter : '(booleanParameter'
	parameterNameDef
	(  eboolean  |  nameInformation  )*
	')';

booleanParameterAssign : '(booleanParameterAssign'
	parameterNameRef
	booleanExpression
	')';

booleanParameterRef : '(booleanParameterRef'
	parameterNameRef
	')';

booleanToken : ( efalse  | etrue );

booleanValue : booleanToken;

borderPattern : '(borderPattern'
	pixelPattern
	')';

borderPatternVisible : '(borderPatternVisible'
	booleanValue
	')';

borderWidth : '(borderWidth'
	( lengthValue | minimalWidth )
	')';

bottomJustify : '(bottomJustify'
	')';

calculated : '(calculated'
	')';

candela : '(candela'
	unitExponent
	')';

capacitanceValue : miNoMaxValue;

caplineJustify : '(caplineJustify'
	')';

cell : '(cell'
	libraryObjectNameDef
	cellHeader
	(  cluster  |  comment  |  userData  |  viewGroup  )*
	')';

cellHeader : '(cellHeader'
	(  documentation  |  nameInformation  |  property_  |  status  )*

	')';

cellNameDisplay : '(cellNameDisplay'
	(  display  |  displayNameOverride  )*
	')';

cellPropertyDisplay : '(cellPropertyDisplay'
	propertyNameRef
	(  display  | propertyNameDisplay )*
	')';

cellPropertyDisplayOverride : '(cellPropertyDisplayOverride'
	propertyNameRef
	( addDisplay | replaceDisplay | removeDisplay )
	(  propertyNameDisplay  )?
	')';

cellPropertyOverride : '(cellPropertyOverride'
	propertyNameRef
	( typedValue | untyped )
	(  comment  | fixed | propertyOverride  )*
	')';

cellRef : '(cellRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

celsius : '(celsius'
	unitExponent
	')';

centerJustify : '(centerJustify'
	')';

characterEncoding : '(characterEncoding'
	( ascii | iso8859 | jisx0201 | jisx0208 )
	')';

checkDate : '(checkDate'
	date
	')';

checkDateDisplay : '(checkDateDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

circle : '(circle'
	pt1
	pt2
	')';

cluster : '(cluster'
	clusterNameDef
	einterface
	clusterHeader
	( schematicSymbol | schematicView | behaviorView  |  clusterConfiguration  |  comment  |  connectivityView  |  defaultClusterConfiguration | userData |  logicModelView  |  maskLayoutView  |  pcbLayoutView  |  symbolicLayoutView )*
	')';

clusterConfiguration : '(clusterConfiguration'
	clusterConfigurationNameDef
	( viewRef | leaf | unconfigured )
	(  comment  |  frameConfiguration  |  globalPortScope  | nameInformation | instanceConfiguration  |  property_  |  userData  )*
	')';

clusterConfigurationNameCaseSensitive : '(clusterConfigurationNameCaseSensitive'
	booleanToken
	')';

clusterConfigurationNameDef : nameDef;

clusterConfigurationNameRef : nameRef;

clusterConfigurationRef : '(clusterConfigurationRef'
	clusterConfigurationNameRef
	')';

clusterHeader : '(clusterHeader'
	(  documentation  |  nameInformation  |  property_  |  status  )*
	')';

clusterNameCaseSensitive : '(clusterNameCaseSensitive'
	booleanToken
	')';

clusterNameDef : nameDef;

clusterNameRef : nameRef;

clusterPropertyDisplay : '(clusterPropertyDisplay'
	propertyNameRef
	(  display  | propertyNameDisplay )*
	')';

clusterPropertyDisplayOverride : '(clusterPropertyDisplayOverride'
	propertyNameRef
	( addDisplay | replaceDisplay | removeDisplay )
	(  propertyNameDisplay  )?
	')';

clusterPropertyOverride : '(clusterPropertyOverride'
	propertyNameRef
	( typedValue | untyped )
	(  comment  |  fixed | propertyOverride  )*
	')';

clusterRef : '(clusterRef'
	clusterNameRef
	( cellRef )? 
	')';

color : '(color'
	red
	green
	blue
	')';

comment : '(comment'
	(  stringToken  )*
	')';

commentGraphics : '(commentGraphics'
	(  annotate  |  comment  |  figure  | originalBoundingBox |  propertyDisplay  |  userData  )*
	')';

companyName : '(companyName'
	stringToken
	')';

companyNameDisplay : '(companyNameDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

complementedName : '(complementedName'
	(  complementedNamePart  |  nameDimension  |  namePartSeparator  |  simpleName  )*
	')';

complementedNamePart : '(complementedNamePart'
	(  complementedNamePart  |  namePartSeparator  |  simpleName  )*
	')';

complexGeometry : '(complexGeometry'
	geometryMacroRef
	transform
	')';

complexName : '(complexName'
	(  complementedNamePart  |  nameDimension  |  namePartSeparator  |  simpleName  )*
	')';

compound : '(compound'
	(  logicNameRef  )*
	')';

condition : '(condition'
	booleanExpression
	')';

conditionDisplay : '(conditionDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

connectedSignalIndexGenerator : '(connectedSignalIndexGenerator'
	integerExpression
	')';

connectedSignalIndexGeneratorDisplay : '(connectedSignalIndexGeneratorDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

connectivityBus : '(connectivityBus'
	interconnectNameDef
	signalGroupRef
	interconnectHeader
	connectivityBusJoined
	(  comment  |  connectivityBusSlice  |  connectivitySubBus  |  userData  )*
	')';

connectivityBusJoined : '(connectivityBusJoined'
	portJoined
	(  connectivityRipperRef  )*
	')';

connectivityBusSlice : '(connectivityBusSlice'
	interconnectNameDef
	signalGroupRef
	interconnectHeader
	connectivityBusJoined
	(  comment  |  connectivityBusSlice  |  connectivitySubBus  |  userData  )*
	')';

connectivityFrameStructure : '(connectivityFrameStructure'
	connectivityFrameStructureNameDef
	frameRef
	(  comment  |  connectivityBus  |  connectivityFrameStructure  |  connectivityNet  |  connectivityRipper  |  timing  |  userData  )*
	')';

connectivityFrameStructureNameDef : nameDef;

connectivityNet : '(connectivityNet'
	interconnectNameDef
	signalRef
	interconnectHeader
	connectivityNetJoined
	(  comment  |  connectivitySubNet  |  userData  )*
	')';

connectivityNetJoined : '(connectivityNetJoined'
	( portJoined | joinedAsSignal )
	(  connectivityRipperRef  )*
	')';

connectivityRipper : '(connectivityRipper'
	connectivityRipperNameDef
	')';

connectivityRipperNameDef : nameDef;

connectivityRipperNameRef : nameRef;

connectivityRipperRef : '(connectivityRipperRef'
	connectivityRipperNameRef
	')';

connectivityStructure : '(connectivityStructure'
	(  comment  |  connectivityBus  |  connectivityFrameStructure  |  connectivityNet  |  connectivityRipper  |  localPortGroup  |  timing  |  userData  )*
	')';

connectivitySubBus : '(connectivitySubBus'
	interconnectNameDef
	interconnectHeader
	connectivityBusJoined
	(  comment  |  connectivityBusSlice  |  connectivitySubBus  |  userData  )*
	')';

connectivitySubNet : '(connectivitySubNet'
	interconnectNameDef
	interconnectHeader
	connectivityNetJoined
	(  comment  |  connectivitySubNet  |  userData  )*
	')';

connectivityTagGenerator : '(connectivityTagGenerator'
	stringExpression
	')';

connectivityTagGeneratorDisplay : '(connectivityTagGeneratorDisplay'
	(  display  )*
	')';

connectivityUnits : '(connectivityUnits'
	(  setCapacitance  |  setTime  )?
	')';

connectivityView : '(connectivityView'
	viewNameDef
	connectivityViewHeader
	logicalConnectivity
	connectivityStructure
	(  comment  |  userData  )*
	')';

connectivityViewHeader : '(connectivityViewHeader'
	connectivityUnits
	(  derivedFrom  |  documentation  |  nameInformation  |  previousVersion  |  property_  |  status  )*
	')';

constantNameDef : nameDef;

constantNameRef : nameRef;

constantValues : '(constantValues'
	(  booleanConstant  |  integerConstant  |  stringConstant  )*
	')';

contract : '(contract'
	stringToken
	')';

contractDisplay : '(contractDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

copyright : '(copyright'
	year
	(  stringToken  )*
	')';

copyrightDisplay : '(copyrightDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

cornerType : '(cornerType'
	( truncate | round_ | extend )
	')';

coulomb : '(coulomb'
	unitExponent
	')';

criticality : '(criticality'
	integerValue
	')';

criticalityDisplay : '(criticalityDisplay'
	(  display  )*
	')';

currentMap : '(currentMap'
	currentValue
	')';

currentValue : miNoMaxValue;

curve : '(curve'
	(  arc  |  pointValue  )*
	')';

dataOrigin : '(dataOrigin'
	stringToken
	(  version  )?
	')';

date : '(date'
	yearNumber
	monthNumber
	dayNumber
	')';

dayNumber : integerToken;

dcFanInLoad : '(dcFanInLoad'
	numberValue
	')';

dcFanInLoadDisplay : '(dcFanInLoadDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

dcFanOutLoad : '(dcFanOutLoad'
	numberValue
	')';

dcFanOutLoadDisplay : '(dcFanOutLoadDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

dcMaxFanIn : '(dcMaxFanIn'
	numberValue
	')';

dcMaxFanInDisplay : '(dcMaxFanInDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

dcMaxFanOut : '(dcMaxFanOut'
	numberValue
	')';

dcMaxFanOutDisplay : '(dcMaxFanOutDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

decimalToString : '(decimalToString'
	integerExpression
	(  minimumStringLength  )?
	')';

defaultClusterConfiguration : '(defaultClusterConfiguration'
	clusterConfigurationNameRef
	')';

defaultConnection : '(defaultConnection'
	globalPortRef
	')';

degree : '(degree'
	unitExponent
	')';

delay : '(delay'
	timeDelay
	')';

denominator : integerValue;

derivation : ( calculated  | measured | required_ );

derivedFrom : '(derivedFrom'
	viewRef
	(  reason  )?
	')';

design : '(design'
	designNameDef
	cellRef
	designHeader
	(  comment  |  designHierarchy  |  userData  )*
	')';

designator : '(designator'
	stringValue
	')';

designatorDisplay : '(designatorDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

designHeader : '(designHeader'
	designUnits
	(  documentation  |  nameInformation  |  property_  |  status  )*
	')';

designHierarchy : '(designHierarchy'
	designHierarchyNameDef
	clusterRef
	clusterConfigurationRef
	designHierarchyHeader
	(  occurrenceHierarchyAnnotate  )?
	')';

designHierarchyHeader : '(designHierarchyHeader'
	(  booleanParameterAssign  |  integerParameterAssign  |  nameInformation | numberParameterAssign  |  property_  |  stringParameterAssign  )*	
	')';

designHierarchyNameCaseSensitive : '(designHierarchyNameCaseSensitive'
	booleanToken
	')';

designHierarchyNameDef : nameDef;

designNameCaseSensitive : '(designNameCaseSensitive'
	booleanToken
	')';

designNameDef : nameDef;

designUnits : '(designUnits'
	(  setCapacitance  |  setTime  )*
	')';

diagram : '(diagram'
	diagramNameDef
	(  annotate  |  comment  |  figure  |  userData  )*
	')';

diagramNameDef : nameDef;

directionalPortAttributeOverride : '(directionalPortAttributeOverride'
	( inputPortAttributes | outputPortAttributes | bidirectionalPortAttributes )
	')';

display : '(display'
	( figureGroupNameRef | figureGroupOverride )*
	transform
	')';

displayAttributes : '(displayAttributes'
	(  borderPattern  |  borderPatternVisible  |  borderWidth  |  color  |  fillPattern  |  fillPatternVisible  |  fontRef  |  horizontalJustification  |  textHeight  |  verticalJustification  |  visible  )*
	')';

displayName : '(displayName'
	stringToken
	')';

displayNameOverride : '(displayNameOverride'
	stringToken
	( addDisplay | replaceDisplay | removeDisplay )
	')';

distanceValue : integerValue;

dividend : integerExpression;

divisor : integerExpression;

documentation : '(documentation'
	documentationNameDef
	documentationHeader
	(  section  )*
	')';

documentationHeader : '(documentationHeader'
	documentationUnits
	(  backgroundColor  |  nameInformation  |  status  )*
	')';

documentationNameCaseSensitive : '(documentationNameCaseSensitive'
	booleanToken
	')';

documentationNameDef : nameDef;

documentationUnits : '(documentationUnits'
	(  setAngle  |  setDistance  )*
	')';

dominates : '(dominates'
	(  logicNameRef  )*
	')';

dot : '(dot'
	pointValue
	')';

drawingDescription : '(drawingDescription'
	stringToken
	')';

drawingDescriptionDisplay : '(drawingDescriptionDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

drawingIdentification : '(drawingIdentification'
	stringToken
	')';

drawingIdentificationDisplay : '(drawingIdentificationDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

drawingSize : '(drawingSize'
	stringToken
	')';

drawingSizeDisplay : '(drawingSizeDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

duration : '(duration'
	timeValue
	')';

e : '(e'
	mantissa
	exponent
	')';

edif : '(edif'
	edifNameDef
	edifVersion
	edifHeader
	(  library_ | design  | comment  |  external_  |  userData  )*
	')';

edifHeader : '(edifHeader'
	edifLevel
	keywordMap
	unitDefinitions
	fontDefinitions
	physicalDefaults
	(  characterEncoding  |  constantValues  |  documentation  |  globalPortDefinitions  |  nameCaseSensitivity  |  nameInformation  |  physicalScaling  |  property_  |  status  )*
	')';

edifLevel : '(edifLevel'
	edifLevelValue
	')';
	

edifLevelValue : integerToken;

edifNameDef : nameDef;

edifVersion : '(edifVersion'
	mark
	issue
	subIssue
	')';

endPoint : pointValue;

endType : '(endType'
	( truncate | round_ | extend )
	')';

engineeringDate : '(engineeringDate'
	date
	')';

engineeringDateDisplay : '(engineeringDateDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

event : '(event'
	( portRef | portList | portSet | interconnectRef | interconnectSet )
	(  becomes  |  transition  )*
	')';

exponent : integerToken;

extend : '(extend'
	')';

extendForFrameMemberDef : forFrameMemberRef;

extendFrameDef : frameNameRef;

extendInstanceDef : instanceNameRef;

extendInstanceMemberDef : instanceMemberRef;

extendInterconnectDef : interconnectNameRef;

extendPageDef : pageNameRef;

extendPortDef : portNameRef;

extendPortMemberDef : portMemberRef;

extendSignalDef : signalNameRef;

extendSignalGroupDef : signalGroupNameRef;

extendSignalMemberDef : signalMemberRef;

external_ : '(external'
	libraryNameDef
	libraryHeader
	(  cell  |  comment  |  geometryMacro  |  pageBorderTemplate  |  pageTitleBlockTemplate  |  schematicFigureMacro  |  schematicForFrameBorderTemplate  |  schematicGlobalPortTemplate  |  schematicIfFrameBorderTemplate  |  schematicInterconnectTerminatorTemplate  |  schematicJunctionTemplate  |  schematicMasterPortTemplate  |  schematicOffPageConnectorTemplate  |  schematicOnPageConnectorTemplate  |  schematicOtherwiseFrameBorderTemplate  |  schematicRipperTemplate  |  schematicSymbolBorderTemplate  |  schematicSymbolPortTemplate  |  userData  )*
	')';

fahrenheit : '(fahrenheit'
	unitExponent
	')';

efalse : '(false'
	')';

farad : '(farad'
	unitExponent
	')';

figure : '(figure'
	( figureGroupNameRef | figureGroupOverride )
	(  circle  |  comment  |  complexGeometry  |  dot  |  openShape  |  path  |  polygon  |  rectangle  |  shape  |  userData  )*
	')';

figureGroup : '(figureGroup'
	figureGroupNameDef
	(  comment  |  cornerType  |  displayAttributes  |  endType  |  nameInformation  |  pathWidth  |  property_  |  userData  )*
	')';

figureGroupNameCaseSensitive : '(figureGroupNameCaseSensitive'
	booleanToken
	')';

figureGroupNameDef : nameDef;

figureGroupNameRef : nameRef;

figureGroupOverride : '(figureGroupOverride'
	figureGroupNameRef
	(  comment  |  cornerType  |  displayAttributes  |  endType  |  pathWidth  |  propertyOverride  )*
	')';

fillPattern : '(fillPattern'
	pixelPattern
	')';

fillPatternVisible : '(fillPatternVisible'
	booleanValue
	')';

firstIntegerExpression : integerExpression;

firstStringExpression : stringExpression;

fixed : '(fixed'
	')';

font : '(font'
	fontNameDef
	typeface
	fontProportions
	(  boldStyle  |  italicStyle  |  property_  |  proportionalFont  |  userData  )*
	')';

fontCapitalHeight : '(fontCapitalHeight'
	lengthValue
	')';

fontDefinitions : '(fontDefinitions'
	(  fonts  )?
	')';

fontDescent : '(fontDescent'
	lengthValue
	')';

fontFamily : stringToken;

fontHeight : '(fontHeight'
	lengthValue
	')';

fontNameDef : nameDef;

fontNameRef : nameRef;

fontProportions : '(fontProportions'
	fontHeight
	fontDescent
	fontCapitalHeight
	fontWidth
	')';

fontRef : '(fontRef'
	fontNameRef
	')';

fonts : '(fonts'
	setDistance
	(  font  )*
	')';

fontWidth : '(fontWidth'
	lengthValue
	')';

forbiddenEvent : '(forbiddenEvent'
	timeInterval
	(  event  )*
	')';

forFrame : '(forFrame'
	frameNameDef
	repetitionCount
	forFrameIndex
	logicalConnectivity
	(  comment  |  documentation  |  nameInformation  |  property_  |  userData  )*
	')';

forFrameAnnotate : '(forFrameAnnotate'
	extendForFrameMemberDef
	(  comment  |  forFrameAnnotate  |  ifFrameAnnotate  |  interconnectAnnotate  |  leafOccurrenceAnnotate  |  occurrenceAnnotate  |  otherwiseFrameAnnotate  )*
	')';

forFrameIndex : '(forFrameIndex'
	indexNameDef
	(  indexStart  |  indexStep  |  nameInformation  )*
	')';

forFrameIndexDisplay : '(forFrameIndexDisplay'
	(  indexEndDisplay  |  indexNameDisplay  |  indexStartDisplay  |  indexStepDisplay  )*
	')';

forFrameIndexNameCaseSensitive : '(forFrameIndexNameCaseSensitive'
	booleanToken
	')';

forFrameIndexRef : '(forFrameIndexRef'
	indexNameRef
	')';

forFrameMemberRef : '(forFrameMemberRef'
	frameNameRef
	indexValue
	')';

forFrameRef : '(forFrameRef'
	frameNameRef
	')';

frameConfiguration : '(frameConfiguration'
	frameNameRef
	(  frameConfiguration  |  instanceConfiguration  )*
	')';

frameNameCaseSensitive : '(frameNameCaseSensitive'
	booleanToken
	')';

frameNameDef : nameDef;

frameNameRef : nameRef;

frameRef : '(frameRef'
	frameNameRef
	')';

frequencyValue : miNoMaxValue;

fromBottom : '(fromBottom'
	')';

fromInteger : integerToken;

fromLeft : '(fromLeft'
	')';

fromRight : '(fromRight'
	')';

fromTop : '(fromTop'
	')';

generated : '(generated'
	')';

geometryMacro : '(geometryMacro'
	libraryObjectNameDef
	geometryMacroHeader
	(  circle  |  comment  |  complexGeometry  |  dot  |  openShape  |  path  |  polygon  |  rectangle  |  shape  |  userData  )*
	')';

geometryMacroHeader : '(geometryMacroHeader'
	geometryMacroUnits
	(  backgroundColor  |  documentation  |  nameInformation  |  originalBoundingBox  |  property_  |  status  )*
	')';

geometryMacroRef : '(geometryMacroRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

geometryMacroUnits : '(geometryMacroUnits'
	(  setAngle  )?
	')';

globalPort : '(globalPort'
	globalPortNameDef
	(  comment  |  nameInformation  |  property_  |  schematicGlobalPortAttributes  |  userData  )*
	')';

globalPortBundle : '(globalPortBundle'
	globalPortNameDef
	globalPortList
	(  comment  |  nameInformation | property_  |  userData  )*
	')';

globalPortDefinitions : '(globalPortDefinitions'
	(  globalPort  |  globalPortBundle  )*
	')';

globalPortList : '(globalPortList'
	(  globalPortRef  )*
	')';

globalPortNameCaseSensitive : '(globalPortNameCaseSensitive'
	booleanToken
	')';

globalPortNameDef : nameDef;

globalPortNameDisplay : '(globalPortNameDisplay'
	(  display  |  displayNameOverride  )*
	')';

globalPortNameRef : nameRef;

globalPortPropertyDisplay : '(globalPortPropertyDisplay'
	propertyNameRef
	(  display | propertyNameDisplay )*
	')';

globalPortRef : '(globalPortRef'
	globalPortNameRef
	')';

globalPortScope : '(globalPortScope'
	globalPortNameRef
	')';

green : scaledInteger;

henry : '(henry'
	unitExponent
	')';

hertz : '(hertz'
	unitExponent
	')';

horizontalJustification : '(horizontalJustification'
	( leftJustify | centerJustify | rightJustify )
	')';

hotspot : '(hotspot'
	pointValue
	(  hotspotConnectDirection  |  hotspotNameDisplay  |  nameInformation  |  schematicWireAffinity  )*
	')';

hotspotConnectDirection : '(hotspotConnectDirection'
	(  fromBottom  |  fromLeft  |  fromRight  |  fromTop  )*
	')';

hotspotGrid : '(hotspotGrid'
	lengthValue
	')';

hotspotNameCaseSensitive : '(hotspotNameCaseSensitive'
	booleanToken
	')';

hotspotNameDef : nameDef;

hotspotNameDisplay : '(hotspotNameDisplay'
	(  display  |  displayNameOverride  )*
	')';

hotspotNameRef : nameRef;

hourNumber : integerToken;

ieeeDesignation : stringToken;

ieeeSection : '(ieeeSection'
	(  integerToken  )*
	')';

ieeeStandard : '(ieeeStandard'
	ieeeDesignation
	year
	(  comment |ieeeSection )*
	')';

ifFrame : '(ifFrame'
	frameNameDef
	condition
	logicalConnectivity
	(  comment  |  nameInformation | documentation  |  property_  |  userData  )*
	')';

ifFrameAnnotate : '(ifFrameAnnotate'
	extendFrameDef
	(  comment  |  forFrameAnnotate  |  ifFrameAnnotate  |  interconnectAnnotate  |  leafOccurrenceAnnotate  |  occurrenceAnnotate  |  otherwiseFrameAnnotate  |  propertyOverride  )*
	')';

ifFrameRef : '(ifFrameRef'
	frameNameRef
	')';

ifFrameSet : '(ifFrameSet'
	(  ifFrameRef  )*
	')';

ignore : '(ignore'
	')';

implementationNameCaseSensitive : '(implementationNameCaseSensitive'
	booleanToken
	')';

implementationNameDef : nameDef;

implementationNameDisplay : '(implementationNameDisplay'
	(  display  |  displayNameOverride  )*
	')';

implementationNameRef : nameRef;

indexEndDisplay : '(indexEndDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

indexNameDef : nameDef;

indexNameDisplay : '(indexNameDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

indexNameRef : nameRef;

indexStart : '(indexStart'
	integerExpression
	')';

indexStartDisplay : '(indexStartDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

indexStep : '(indexStep'
	integerExpression
	')';

indexStepDisplay : '(indexStepDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

indexValue : '(indexValue'
	integerToken
	')';

input_ : '(input'
	')';

inputPort : '(inputPort'
	(  inputPortAttributes  )?
	')';

inputPortAttributes : '(inputPortAttributes'
	(  dcFanOutLoad  |  dcMaxFanIn  )*
	')';

instance : '(instance'
	instanceNameDef
	clusterRef
	(  booleanParameterAssign  |  cellPropertyOverride  |  clusterPropertyOverride  |  comment  |  designator  |  instanceNameGenerator  |  instancePortAttributes  |  instanceWidth  |  integerParameterAssign  |  nameInformation  |  numberParameterAssign  |  property_  |  stringParameterAssign  |  timing  |  userData  )*
	')';

instanceConfiguration : '(instanceConfiguration'
	instanceNameRef
	clusterConfigurationRef
	')';

instanceIndexValue : '(instanceIndexValue'
	')';

instanceMemberRef : '(instanceMemberRef'
	instanceNameRef
	indexValue
	')';

instanceNameCaseSensitive : '(instanceNameCaseSensitive'
	booleanToken
	')';

instanceNameDef : nameDef;

instanceNameDisplay : '(instanceNameDisplay'
	(  display  |  displayNameOverride  )*
	')';

instanceNameGenerator : '(instanceNameGenerator'
	stringExpression
	')';

instanceNameGeneratorDisplay : '(instanceNameGeneratorDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

instanceNameRef : nameRef;

instancePortAttributeDisplay : '(instancePortAttributeDisplay'
	symbolPortImplementationNameRef
	portRef
	(  portPropertyDisplayOverride | portAttributeDisplay )*
	')';

instancePortAttributes : '(instancePortAttributes'
	extendPortDef
	(  acLoad  |  comment  |  connectedSignalIndexGenerator  |  designator  |  directionalPortAttributeOverride  |  portDelay  |  portDelayOverride  |  portLoadDelay  |  portLoadDelayOverride  |  portPropertyOverride  |  property_  |  unused  )*
	')';

instancePropertyDisplay : '(instancePropertyDisplay'
	propertyNameRef
	(  display  )*
	( propertyNameDisplay )?
	(  display  )*
	')';

instancePropertyOverride : '(instancePropertyOverride'
	propertyNameRef
	( typedValue | untyped )
	(  comment  |  fixed | propertyOverride  )*
	')';

instanceRef : '(instanceRef'
	instanceNameRef
	')';

instanceWidth : '(instanceWidth'
	integerExpression
	')';

instanceWidthDisplay : '(instanceWidthDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

integer : '(integer'
	integerExpression
	')';

integerConstant : '(integerConstant'
	constantNameDef
	integerToken
	')';

integerConstantRef : '(integerConstantRef'
	constantNameRef
	')';

integerEqual : '(integerEqual'
	firstIntegerExpression
	secondIntegerExpression
	')';

integerExpression : ( integerParameterRef  | integerToken | integerProduct | integerSubtract | integerSum | integerRemainder | integerQuotient | stringLength | integerConstantRef | forFrameIndexRef | portIndexValue | signalIndexValue | absolute | instanceIndexValue )
;
integerParameter : '(integerParameter'
	parameterNameDef
	(  integer  |  nameInformation  )*
	')';

integerParameterAssign : '(integerParameterAssign'
	parameterNameRef
	integerExpression
	')';

integerParameterRef : '(integerParameterRef'
	parameterNameRef
	')';

integerProduct : '(integerProduct'
	(  integerExpression  )*
	')';

integerQuotient : '(integerQuotient'
	dividend
	divisor
	')';

integerRemainder : '(integerRemainder'
	dividend
	divisor
	')';

integerSubtract : '(integerSubtract'
	minuend
	subtrahend
	')';

integerSum : '(integerSum'
	(  integerExpression  )*
	')';

integerValue : integerToken;

interconnectAnnotate : '(interconnectAnnotate'
	extendInterconnectDef
	(  comment  |  interconnectAnnotate  |  criticality | interconnectDelay  |  property_  |  propertyOverride  )*
	')';

interconnectAttachedText : '(interconnectAttachedText'
	attachmentPoint
	(  annotate  |  connectivityTagGeneratorDisplay  |  criticalityDisplay  |  interconnectDelayDisplay  |  interconnectNameDisplay  |  interconnectPropertyDisplay  )*
	')';

interconnectDelay : '(interconnectDelay'
	interconnectDelayNameDef
	derivation
	delay
	(  becomes  |  transition  )*
	')';

interconnectDelayDisplay : '(interconnectDelayDisplay'
	interconnectDelayNameRef
	(  display  )*
	')';

interconnectDelayNameDef : nameDef;

interconnectDelayNameRef : nameRef;

interconnectHeader : '(interconnectHeader'
	(  criticality  |  documentation  |  interconnectDelay  |  nameInformation  |  property_  )*
	')';

interconnectNameCaseSensitive : '(interconnectNameCaseSensitive'
	booleanToken
	')';

interconnectNameDef : nameDef;

interconnectNameDisplay : '(interconnectNameDisplay'
	(  display  |  displayNameOverride  )*
	')';

interconnectNameRef : nameRef;

interconnectPropertyDisplay : '(interconnectPropertyDisplay'
	propertyNameRef
	(  display   )*
	( propertyNameDisplay )?
	(  display   )*
	')';

interconnectRef : '(interconnectRef'
	interconnectNameRef
	( pageRef )? 
	')';

interconnectSet : '(interconnectSet'
	(  interconnectRef  )*
	')';

einterface : '(interface'
	interfaceUnits
	(  designator  | booleanParameter  |  integerParameter  |  interfaceJoined  |  mustJoin  |  numberParameter  |  permutable  |  port  |  portBundle  |  stringParameter  |  timing  |  weakJoined  )*
	')';

interfaceJoined : '(interfaceJoined'
	(  portRef  )*
	')';

interfaceUnits : '(interfaceUnits'
	(  setCapacitance  |  setTime  )*
	')';

iso8859 : '(iso8859'
	iso8859Part
	')';

iso8859Part : integerToken;

isolated : '(isolated'
	')';

issue : integerToken;

italicStyle : '(italicStyle'
	')';

jisx0201 : '(jisx0201'
	')';

jisx0208 : '(jisx0208'
	')';

joinedAsSignal : '(joinedAsSignal'
	')';

joule : '(joule'
	unitExponent
	')';

k0KeywordLevel : '(k0KeywordLevel'
	')';

k1KeywordAlias : '(k1KeywordAlias'
	k1KeywordNameDef
	k1KeywordNameRef
	')';

k1KeywordLevel : '(k1KeywordLevel'
	(  k1KeywordAlias  )*
	')';

k1KeywordNameDef : IDENTIFIER;

k1KeywordNameRef : IDENTIFIER;

k2Actual : '(k2Actual'
	k2FormalNameRef
	')';

k2Build : '(k2Build'
	k1KeywordNameRef
	(  comment  |  k2Actual  |  k2Build  |  k2Literal  )*
	')';

k2Formal : '(k2Formal'
	k2FormalNameDef
	( k2Optional | k2Required )
	')';

k2FormalNameDef : IDENTIFIER;

k2FormalNameRef : IDENTIFIER;

k2Generate : '(k2Generate'
	(  comment  |  k2Actual  |  k2Build  |  k2Literal  )*
	')';

k2KeywordDefine : '(k2KeywordDefine'
	k1KeywordNameDef
	k2KeywordParameters
	k2Generate
	')';

k2KeywordLevel : '(k2KeywordLevel'
	(  k1KeywordAlias  |  k2KeywordDefine  )*
	')';

k2KeywordParameters : '(k2KeywordParameters'
	(  k2Formal  )*
	')';

k2Literal : '(k2Literal'
	(  IDENTIFIER  |  integerToken  |  stringToken  )*
	')';

k2Optional : '(k2Optional'
	( k2Literal | k2Actual | k2Build )
	')';

k2Required : '(k2Required'
	')';

k3Build : '(k3Build'
	k1KeywordNameRef
	(  comment  |  k2Actual  |  k2Literal  |  k3Build  |  k3ForEach  )*
	')';

k3Collector : '(k3Collector'
	')';

k3ForEach : '(k3ForEach'
	( k2FormalNameRef | k3FormalList )
	(  comment  |  k2Actual  |  k2Literal  |  k3Build  |  k3ForEach  )*
	')';

k3Formal : '(k3Formal'
	k2FormalNameDef
	( k2Optional | k2Required | k3Collector )
	')';

k3FormalList : '(k3FormalList'
	(  k2FormalNameRef  )*
	')';

k3Generate : '(k3Generate'
	(  comment  |  k2Actual  |  k2Build  |  k2Literal  |  k3ForEach  )*
	')';

k3KeywordDefine : '(k3KeywordDefine'
	k1KeywordNameDef
	k3KeywordParameters
	k3Generate
	')';

k3KeywordLevel : '(k3KeywordLevel'
	(  k1KeywordAlias  |  k3KeywordDefine  )*
	')';

k3KeywordParameters : '(k3KeywordParameters'
	(  k3Formal  )*
	')';

kelvin : '(kelvin'
	unitExponent
	')';

keywordMap : '(keywordMap'
	( k0KeywordLevel | k1KeywordLevel | k2KeywordLevel | k3KeywordLevel )
	(  comment  )*
	')';

kilogram : '(kilogram'
	unitExponent
	')';

leaf : '(leaf'
	')';

leafOccurrenceAnnotate : '(leafOccurrenceAnnotate'
	( extendInstanceDef | extendInstanceMemberDef )
	(  cellPropertyOverride  |  clusterPropertyOverride  |  comment | designator |  instancePropertyOverride  |  portAnnotate  |  property_  )*
	')';

leftJustify : '(leftJustify'
	')';

lengthValue : distanceValue;

lessThan : '(lessThan'
	(  integerExpression  )*
	')';

lessThanOrEqual : '(lessThanOrEqual'
	(  integerExpression  )*
	')';

library_ : '(library'
	libraryNameDef
	libraryHeader
	( cell | schematicInterconnectTerminatorTemplate | schematicJunctionTemplate |  schematicGlobalPortTemplate  |   schematicMasterPortTemplate  |  schematicOffPageConnectorTemplate  | schematicOnPageConnectorTemplate  | schematicRipperTemplate |  schematicSymbolBorderTemplate  |  schematicSymbolPortTemplate  | pageBorderTemplate | pageTitleBlockTemplate | comment  |  geometryMacro  |  schematicFigureMacro  |  schematicForFrameBorderTemplate  |  schematicIfFrameBorderTemplate | schematicOtherwiseFrameBorderTemplate  | userData )*
	')';

libraryHeader : '(libraryHeader'
	edifLevel
	nameCaseSensitivity
	technology
	(  backgroundColor  |  documentation  |  nameInformation  |  property_  |  status  )*
	')';

libraryNameCaseSensitive : '(libraryNameCaseSensitive'
	booleanToken
	')';

libraryNameDef : nameDef;

libraryNameRef : nameRef;

libraryObjectNameCaseSensitive : '(libraryObjectNameCaseSensitive'
	booleanToken
	')';

libraryObjectNameDef : nameDef;

libraryObjectNameRef : nameRef;

libraryRef : '(libraryRef'
	libraryNameRef
	')';

loadDelay : '(loadDelay'
	acLoadFactorTime
	acLoadFactorCapacitance
	')';

localPortGroup : '(localPortGroup'
	localPortGroupNameDef
	portList
	(  comment  | nameInformation | property_  |  userData  )*	
	')';

localPortGroupNameCaseSensitive : '(localPortGroupNameCaseSensitive'
	booleanToken
	')';

localPortGroupNameDef : nameDef;

localPortGroupNameRef : nameRef;

localPortGroupRef : '(localPortGroupRef'
	localPortGroupNameRef
	')';

logicalConnectivity : '(logicalConnectivity'
	(  comment  |  forFrame  |  ifFrame  |  instance  |  otherwiseFrame  |  signal  |  signalGroup  |  userData  )*
	')';

logicDefinitions : '(logicDefinitions'
	setVoltage
	setCurrent
	(  comment  |  logicValue  )*
	')';

logicList : '(logicList'
	(  ignore  |  logicNameRef  |  logicOneOf  )*
	')';

logicMapInput : '(logicMapInput'
	(  logicRef  )*
	')';

logicMapOutput : '(logicMapOutput'
	(  logicRef  )*
	')';

logicModelUnits : '(logicModelUnits'
	(  setCapacitance  |  setTime  )*
	')';

logicModelView : '(logicModelView'
	viewNameDef
	(  comment  | nameInformation | userData  )*
	')';

logicNameDef : nameDef;

logicNameRef : nameRef;

logicOneOf : '(logicOneOf'
	(  logicList  |  logicNameRef  )*
	')';

logicRef : '(logicRef'
	logicNameRef
	( libraryRef )? 
	')';

logicValue : '(logicValue'
	logicNameDef
	(  booleanMap  |  comment  |  compound  |  currentMap  |  dominates  |  isolated  |  logicMapInput  |  logicMapOutput  |  nameInformation  |  property_  |  resolves  |  strong  |  voltageMap  |  weak_  )*
	')';

lsbToMsb : '(lsbToMsb'
	')';

mantissa : integerToken;

mark : integerToken;

maskLayoutUnits : '(maskLayoutUnits'
	(  setAngle  |  setCapacitance  |  setDistance  |  setTime  )*
	')';

maskLayoutView : '(maskLayoutView'
	viewNameDef
	(  comment  | nameInformation | userData  )*	
	')';

measured : '(measured'
	')';

meter : '(meter'
	unitExponent
	')';

middleJustify : '(middleJustify'
	')';

minimalWidth : '(minimalWidth'
	')';

minimumStringLength : '(minimumStringLength'
	substringLength
	')';

miNoMax : '(miNoMax'
	miNoMaxValue
	')';

miNoMaxValue : ( numberValue  | mnm );

minuend : integerExpression;

minuteNumber : integerToken;

mixedDirection : '(mixedDirection'
	')';

mnm : '(mnm'
	( numberValue | undefined | unconstrained )
	( numberValue | undefined | unconstrained )
	( numberValue | undefined | unconstrained )
	')';

mole : '(mole'
	unitExponent
	')';

monthNumber : integerToken;

msbToLsb : '(msbToLsb'
	')';

mustJoin : '(mustJoin'
	(  interfaceJoined  |  portRef  |  weakJoined  )*
	')';

nameAlias : '(nameAlias'
	originalName
	(  displayName  |  generated  |  nameStructure  )*
	')';

nameCaseSensitivity : '(nameCaseSensitivity'
	(  clusterConfigurationNameCaseSensitive  |  clusterNameCaseSensitive  |  designHierarchyNameCaseSensitive  |  designNameCaseSensitive  |  documentationNameCaseSensitive  |  figureGroupNameCaseSensitive  |  forFrameIndexNameCaseSensitive  |  frameNameCaseSensitive  |  globalPortNameCaseSensitive  |  hotspotNameCaseSensitive  |  implementationNameCaseSensitive  |  instanceNameCaseSensitive  |  interconnectNameCaseSensitive  |  libraryNameCaseSensitive  |  libraryObjectNameCaseSensitive  |  localPortGroupNameCaseSensitive  |  pageNameCaseSensitive  |  parameterNameCaseSensitive  |  portNameCaseSensitive  |  propertyNameCaseSensitive  |  signalGroupNameCaseSensitive  |  signalNameCaseSensitive  |  viewGroupNameCaseSensitive  |  viewNameCaseSensitive  )*
	')';

nameDef : IDENTIFIER;

nameDimension : '(nameDimension'
	nameDimensionStructure
	(  bitOrder  )?
	')';

nameDimensionStructure : '(nameDimensionStructure'
	(  complementedName  |  complexName  |  integerValue  |  sequence  |  simpleName  )*
	')';

nameInformation : '(nameInformation'
	primaryName
	(  nameAlias  )*
	')';

namePartSeparator : '(namePartSeparator'
	stringToken
	')';

nameRef : IDENTIFIER;

nameStructure : '(nameStructure'
	( simpleName | complexName | complementedName )
	')';

narrowPort : '(narrowPort'
	')';

narrowWire : '(narrowWire'
	')';

noHotspotGrid : '(noHotspotGrid'
	')';

nominalHotspotGrid : '(nominalHotspotGrid'
	lengthValue
	')';

nonPermutable : '(nonPermutable'
	(  permutable  |  portRef  )*
	')';

not_ : '(not'
	booleanExpression
	')';

notInherited : '(notInherited'
	')';

number : '(number'
	numberExpression
	')';

numberExpression : ( numberValue  | numberParameterRef );

numberOfBasicUnits : scaledInteger;

numberOfNewUnits : scaledInteger;

numberParameter : '(numberParameter'
	parameterNameDef
	(  nameInformation  |  number  )*
	')';

numberParameterAssign : '(numberParameterAssign'
	parameterNameRef
	numberExpression
	')';

numberParameterRef : '(numberParameterRef'
	parameterNameRef
	')';

numberPoint : '(numberPoint'
	xNumberValue
	yNumberValue
	')';

numberValue : scaledInteger;

numerator : integerValue;

occurrenceAnnotate : '(occurrenceAnnotate'
	( extendInstanceDef | extendInstanceMemberDef )
	(  cellPropertyOverride  |  clusterPropertyOverride  |  comment | designator |  forFrameAnnotate  |  ifFrameAnnotate  |  instancePropertyOverride  |  interconnectAnnotate  |  leafOccurrenceAnnotate  |  occurrenceAnnotate  |  otherwiseFrameAnnotate  |  pageAnnotate  |  portAnnotate  |  property_  |  signalAnnotate  |  signalGroupAnnotate  |  timing  |  viewPropertyOverride  )*	  
	')';

occurrenceHierarchyAnnotate : '(occurrenceHierarchyAnnotate'
	(  cellPropertyOverride  |  clusterPropertyOverride  |  comment  |  forFrameAnnotate  |  ifFrameAnnotate  |  interconnectAnnotate  |  leafOccurrenceAnnotate  |  occurrenceAnnotate  |  otherwiseFrameAnnotate  |  pageAnnotate  |  portAnnotate  |  signalAnnotate  |  signalGroupAnnotate  |  timing  |  viewPropertyOverride  )*
	')';

offsetEvent : '(offsetEvent'
	event
	numberValue
	')';

ohm : '(ohm'
	unitExponent
	')';

openShape : '(openShape'
	curve
	')';

or_ : '(or'
	(  booleanExpression  )*
	')';

origin : '(origin'
	pointValue
	')';

originalBoundingBox : '(originalBoundingBox'
	rectangle
	')';

originalDrawingDate : '(originalDrawingDate'
	date
	')';

originalDrawingDateDisplay : '(originalDrawingDateDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

originalName : stringToken;

otherwiseFrame : '(otherwiseFrame'
	frameNameDef
	ifFrameSet
	logicalConnectivity
	(  comment  |  documentation  | nameInformation | property_  |  userData  )*
	')';

otherwiseFrameAnnotate : '(otherwiseFrameAnnotate'
	extendFrameDef
	(  comment  |  forFrameAnnotate  |  ifFrameAnnotate  |  interconnectAnnotate  |  leafOccurrenceAnnotate  |  occurrenceAnnotate  |  otherwiseFrameAnnotate  |  propertyOverride  )*
	')';

otherwiseFrameRef : '(otherwiseFrameRef'
	frameNameRef
	')';

output : '(output'
	')';

outputPort : '(outputPort'
	(  outputPortAttributes  )?
	')';

outputPortAttributes : '(outputPortAttributes'
	(  dcFanInLoad  |  dcMaxFanOut  )*
	')';

owner : '(owner'
	stringValue
	')';

page : '(page'
	pageNameDef
	pageHeader
	(  cellPropertyDisplay  |  clusterPropertyDisplay  |  comment  |  localPortGroup  |  pageCommentGraphics  |  pageTitleBlock  |  propertyDisplay  |  schematicBus  |  schematicForFrameImplementation  |  schematicGlobalPortImplementation  |  schematicIfFrameImplementation  |  schematicInstanceImplementation  |  schematicMasterPortImplementation  |  schematicNet  |  schematicOffPageConnectorImplementation  |  schematicOnPageConnectorImplementation  |  schematicOtherwiseFrameImplementation  |  schematicRipperImplementation  |  userData  |  viewPropertyDisplay  )*
	')';

pageAnnotate : '(pageAnnotate'
	extendPageDef
	(  interconnectAnnotate  )*
	')';

pageBorder : '(pageBorder'
	pageBorderTemplateRef
	transform
	(  propertyDisplayOverride  |  propertyOverride  )*
	')';

pageBorderTemplate : '(pageBorderTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	usableArea
	(  annotate  |  commentGraphics  |  figure  |  propertyDisplay  |  schematicComplexFigure  )*
	')';

pageBorderTemplateRef : '(pageBorderTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

pageCommentGraphics : '(pageCommentGraphics'
	(  annotate  |  comment  |  figure  |  schematicComplexFigure  |  userData  )*
	')';

pageHeader : '(pageHeader'
	(  backgroundColor  |  documentation  |  nameInformation  |  originalBoundingBox  |  pageBorder  |  pageSize  |  property_  |  status  )*
	')';

pageIdentification : '(pageIdentification'
	stringToken
	')';

pageIdentificationDisplay : '(pageIdentificationDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

pageNameCaseSensitive : '(pageNameCaseSensitive'
	booleanToken
	')';

pageNameDef : nameDef;

pageNameRef : nameRef;

pagePropertyDisplay : '(pagePropertyDisplay'
	propertyNameRef
	(  display | propertyNameDisplay)*
	')';

pageRef : '(pageRef'
	pageNameRef
	')';

pageSize : '(pageSize'
	rectangle
	')';

pageTitle : '(pageTitle'
	stringToken
	')';

pageTitleBlock : '(pageTitleBlock'
	implementationNameDef
	pageTitleBlockTemplateRef
	transform
	(  nameInformation  |  pagePropertyDisplay  |  pageTitleBlockAttributeDisplay  |  pageTitleBlockAttributes  |  property_  |  propertyDisplay  |  propertyDisplayOverride  |  propertyOverride  )*
	')';

pageTitleBlockAttributeDisplay : '(pageTitleBlockAttributeDisplay'
	(  approvedDateDisplay  |  checkDateDisplay  |  companyNameDisplay  |  contractDisplay  |  copyrightDisplay  |  drawingDescriptionDisplay  |  drawingIdentificationDisplay  |  drawingSizeDisplay  |  engineeringDateDisplay  |  originalDrawingDateDisplay  |  pageIdentificationDisplay  |  pageTitleDisplay  |  revisionDisplay  |  totalPagesDisplay  )*
	')';

pageTitleBlockAttributes : '(pageTitleBlockAttributes'
	(  approvedDate  |  checkDate  |  companyName  |  contract  |  drawingDescription  |  drawingIdentification  |  drawingSize  |  engineeringDate  |  originalDrawingDate  |  pageIdentification  |  pageTitle  |  revision  )*
	')';

pageTitleBlockTemplate : '(pageTitleBlockTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	(  annotate  |  commentGraphics  |  figure  |  pageTitleBlockAttributeDisplay  |  pageTitleBlockAttributes  |  propertyDisplay  |  schematicComplexFigure  )*
	')';

pageTitleBlockTemplateRef : '(pageTitleBlockTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

pageTitleDisplay : '(pageTitleDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

parameterDisplay : '(parameterDisplay'
	parameterNameRef
	( addDisplay | replaceDisplay | removeDisplay )
	(  parameterNameDisplay  )?
	')';

parameterNameCaseSensitive : '(parameterNameCaseSensitive'
	booleanToken
	')';

parameterNameDef : nameDef;

parameterNameDisplay : '(parameterNameDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

parameterNameRef : nameRef;

path : '(path'
	pointList
	')';

pathDelay : '(pathDelay'
	delay
	(  event  )*
	')';

pathWidth : '(pathWidth'
	( lengthValue | minimalWidth )
	')';

pcbLayoutUnits : '(pcbLayoutUnits'
	(  setAngle  |  setCapacitance  |  setDistance  |  setTime  )*
	')';

pcbLayoutView : '(pcbLayoutView'
	viewNameDef
	(  comment  | nameInformation | userData  )*
	')';

permutable : '(permutable'
	(  nonPermutable  |  permutable  |  portRef  )*
	')';

physicalDefaults : '(physicalDefaults'
	(  schematicRequiredDefaults  )?
	')';

physicalScaling : '(physicalScaling'
	(  comment  |  connectivityUnits  |  documentationUnits  |  geometryMacroUnits  |  interfaceUnits  |  logicModelUnits  |  maskLayoutUnits  |  pcbLayoutUnits  |  schematicUnits  |  symbolicLayoutUnits  )*
	')';

pixelPattern : '(pixelPattern'
	rowSize
	(  pixelRow  )*
	')';

pixelRow : '(pixelRow'
	(  booleanToken  )*
	')';

point : '(point'
	pointValue
	')';

pointList : '(pointList'
	(  pointValue  )*
	')';

pointValue : pt;

polygon : '(polygon'
	pointList
	')';

port : '(port'
	portNameDef?
	portDirection?
	(  acLoad  |  comment  |  defaultConnection  |  designator  |  nameInformation  |  portDelay  |  portLoadDelay  |  portNameGenerator  |  portWidth  |  property_  |  schematicPortAttributes  |  unused  |  userData  )*
	')';

portAnnotate : '(portAnnotate'
	( extendPortDef | extendPortMemberDef )
	(  acLoad  |  comment  |  designator  |  directionalPortAttributeOverride  |  portDelay  |  portDelayOverride  |  portLoadDelay  |  portLoadDelayOverride  |  portPropertyOverride  |  property_  )*
	')';

portAttributeDisplay : '(portAttributeDisplay'
	(  acLoadDisplay  |  connectedSignalIndexGeneratorDisplay  |  dcFanInLoadDisplay  |  dcFanOutLoadDisplay  |  dcMaxFanInDisplay  |  dcMaxFanOutDisplay  |  designatorDisplay  |  portDelayDisplay  |  portLoadDelayDisplay  |  portNameDisplay  |  portNameGeneratorDisplay  |  portPropertyDisplay  )*
	')';

portBundle : '(portBundle'
	portNameDef
	portList
	(  comment  | nameInformation | property_  |  userData  | designator)*
	')';

portDelay : '(portDelay'
	portDelayNameDef
	derivation
	delay
	(  becomes  |  transition  )*
	')';

portDelayDisplay : '(portDelayDisplay'
	portDelayNameRef
	(  display  )*
	')';

portDelayNameDef : nameDef;

portDelayNameRef : nameRef;

portDelayOverride : '(portDelayOverride'
	portDelayNameRef
	derivation
	delay
	(  becomes  |  transition  )*
	')';

portDirection : ( inputPort  | outputPort | bidirectionalPort | unspecifiedDirectionPort );

portDirectionIndicator : ( input_  | output | bidirectional | unspecified | unrestricted | mixedDirection );

portIndexValue : '(portIndexValue'
	')';

portInstanceRef : '(portInstanceRef'
	( portNameRef | portMemberRef )
	( instanceRef | instanceMemberRef )
	')';

portJoined : '(portJoined'
	(  globalPortRef  |  localPortGroupRef  |  portInstanceRef  |  portRef  )*
	')';

portList : '(portList'
	(  portRef  )*
	')';

portLoadDelay : '(portLoadDelay'
	portLoadDelayNameDef
	derivation
	loadDelay
	(  becomes  |  transition  )*
	')';

portLoadDelayDisplay : '(portLoadDelayDisplay'
	portLoadDelayNameRef
	(  display  )*
	')';

portLoadDelayNameDef : nameDef;

portLoadDelayNameRef : nameRef;

portLoadDelayOverride : '(portLoadDelayOverride'
	portLoadDelayNameRef
	derivation
	loadDelay
	(  becomes  |  transition  )*
	')';

portMemberRef : '(portMemberRef'
	portNameRef
	indexValue
	')';

portNameCaseSensitive : '(portNameCaseSensitive'
	booleanToken
	')';

portNameDef : nameDef;

portNameDisplay : '(portNameDisplay'
	(  display  |  displayNameOverride  )*
	')';

portNameGenerator : '(portNameGenerator'
	stringExpression
	')';

portNameGeneratorDisplay : '(portNameGeneratorDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

portNameRef : nameRef;

portPropertyDisplay : '(portPropertyDisplay'
	propertyNameRef
	(  display | propertyNameDisplay )*
	')';

portPropertyDisplayOverride : '(portPropertyDisplayOverride'
	propertyNameRef
	( addDisplay | replaceDisplay | removeDisplay )
	(  propertyNameDisplay  )?
	')';

portPropertyOverride : '(portPropertyOverride'
	propertyNameRef
	( typedValue | untyped )
	(  comment  | fixed | propertyOverride  )*
	')';

portRef : '(portRef'
	portNameRef
	')';

portSet : '(portSet'
	(  portRef  )*
	')';

portWidth : '(portWidth'
	integerExpression
	')';

presentLogicValue : ( logicNameRef | logicList | logicOneOf );

previousLogicValue : ( logicNameRef | logicList | logicOneOf );

previousVersion : '(previousVersion'
	viewRef
	(  reason  )?
	')';

primaryName : '(primaryName'
	originalName
	(  displayName  |  generated  |  nameStructure  )*
	')';

program : '(program'
	stringValue
	(  version  )?
	')';

property_ : '(property'
	propertyNameDef
	( typedValue | untyped )
	(  comment  |  nameInformation  |  owner  |  property_  |  propertyInheritanceControl  |  unitRef  )*
	')';

propertyDisplay : '(propertyDisplay'
	propertyNameRef
	(  display | propertyNameDisplay )*
	')';

propertyDisplayOverride : '(propertyDisplayOverride'
	propertyNameRef
	( addDisplay | replaceDisplay | removeDisplay )
	(  propertyNameDisplay  )?
	')';

propertyInheritanceControl : '(propertyInheritanceControl'
	( fixed | notInherited )
	')';

propertyNameCaseSensitive : '(propertyNameCaseSensitive'
	booleanToken
	')';

propertyNameDef : nameDef;

propertyNameDisplay : '(propertyNameDisplay'
	(  display  |  displayNameOverride  )*
	')';

propertyNameRef : nameRef;

propertyOverride : '(propertyOverride'
	propertyNameRef
	( typedValue | untyped )
	(  comment | fixed |  propertyOverride  )*
	')';

proportionalFont : '(proportionalFont'
	')';

pt : '(pt'
	xCoordinate
	yCoordinate
	')';

pt1 : pointValue;

pt2 : pointValue;

radian : '(radian'
	unitExponent
	')';

reason : '(reason'
	stringToken
	')';

rectangle : '(rectangle'
	pt1
	pt2
	')';

red : scaledInteger;

removeDisplay : '(removeDisplay'
	')';

repetitionCount : '(repetitionCount'
	integerExpression
	')';

repetitionCountDisplay : '(repetitionCountDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

replaceDisplay : '(replaceDisplay'
	(  display  )*
	')';

required_ : '(required'
	')';

resolves : '(resolves'
	(  logicNameRef  )*
	')';

revision : '(revision'
	stringToken
	')';

revisionDisplay : '(revisionDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

rightJustify : '(rightJustify'
	')';

ripperHotspot : '(ripperHotspot'
	hotspotNameDef
	hotspot
	')';

ripperHotspotRef : '(ripperHotspotRef'
	hotspotNameRef
	schematicRipperImplementationRef
	')';

rotation : '(rotation'
	angleValue
	')';

round_ : '(round'
	')';

rowSize : integerToken;

scaledInteger : ( integerToken  | e );

scaleX : '(scaleX'
	numerator
	denominator
	')';

scaleY : '(scaleY'
	numerator
	denominator
	')';

schematicBus : '(schematicBus'
	interconnectNameDef
	signalGroupRef
	schematicInterconnectHeader
	schematicBusJoined
	(  comment  |  schematicBusDetails  |  schematicBusSlice  |  schematicInterconnectAttributeDisplay  |  userData  )*
	')';

schematicBusDetails : '(schematicBusDetails'
	( schematicBusGraphics | schematicSubBusSet )
	')';

schematicBusGraphics : '(schematicBusGraphics'
	(  comment  |  figure  |  schematicComplexFigure  |  userData  )*
	')';

schematicBusJoined : '(schematicBusJoined'
	(portJoined | ripperHotspotRef  |  schematicGlobalPortImplementationRef  |  schematicInterconnectTerminatorImplementationRef  |  schematicJunctionImplementationRef  |  schematicMasterPortImplementationRef  |  schematicOffPageConnectorImplementationRef  |  schematicOnPageConnectorImplementationRef  |  schematicSymbolPortImplementationRef  )*
	')';

schematicBusSlice : '(schematicBusSlice'
	interconnectNameDef
	signalGroupRef
	schematicInterconnectHeader
	schematicBusJoined
	(  comment  |  schematicBusDetails  |  schematicBusSlice  |  schematicInterconnectAttributeDisplay  |  userData  )*
	')';

schematicComplexFigure : '(schematicComplexFigure'
	schematicFigureMacroRef
	transform
	(  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicFigureMacro : '(schematicFigureMacro'
	libraryObjectNameDef
	schematicTemplateHeader
	(  annotate  |  comment  |  commentGraphics  |  figure  |  propertyDisplay  |  schematicComplexFigure  |  userData  )*
	')';

schematicFigureMacroRef : '(schematicFigureMacroRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicForFrameBorder : '(schematicForFrameBorder'
	schematicForFrameBorderTemplateRef
	transform
	(  forFrameIndexDisplay  |  propertyDisplayOverride  |  propertyOverride  |  repetitionCountDisplay  )*
	')';

schematicForFrameBorderTemplate : '(schematicForFrameBorderTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	usableArea
	(  annotate  |  commentGraphics  |  figure  |  forFrameIndexDisplay  |  propertyDisplay  |  repetitionCountDisplay  |  schematicComplexFigure  )*
	')';

schematicForFrameBorderTemplateRef : '(schematicForFrameBorderTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicForFrameImplementation : '(schematicForFrameImplementation'
	implementationNameDef
	forFrameRef
	schematicForFrameImplementationHeader
	schematicFrameImplementationDetails
	')';

schematicForFrameImplementationHeader : '(schematicForFrameImplementationHeader'
	(  schematicForFrameBorder  )?
	')';

schematicFrameImplementationDetails : '(schematicFrameImplementationDetails'
	(  cellPropertyDisplay  |  clusterPropertyDisplay  |  comment  |  commentGraphics  |  propertyDisplay  |  schematicBus  |  schematicForFrameImplementation  |  schematicGlobalPortImplementation  |  schematicIfFrameImplementation  |  schematicInstanceImplementation  |  schematicMasterPortImplementation  |  schematicNet  |  schematicOffPageConnectorImplementation  |  schematicOnPageConnectorImplementation  |  schematicOtherwiseFrameImplementation  |  schematicRipperImplementation  |  userData  |  viewPropertyDisplay  )*
	')';

schematicGlobalPortAttributes : '(schematicGlobalPortAttributes'
	(  ieeeStandard  |  schematicPortAcPower  |  schematicPortAnalog  |  schematicPortChassisGround  |  schematicPortClock  |  schematicPortDcPower  |  schematicPortEarthGround  |  schematicPortGround  |  schematicPortNonLogical  |  schematicPortOpenCollector  |  schematicPortOpenEmitter  |  schematicPortPower  |  schematicPortThreeState  )*
	')';

schematicGlobalPortImplementation : '(schematicGlobalPortImplementation'
	implementationNameDef
	schematicGlobalPortTemplateRef
	globalPortRef
	transform
	(  globalPortNameDisplay  |  globalPortPropertyDisplay  |  implementationNameDisplay  |  nameInformation  |  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicGlobalPortImplementationRef : '(schematicGlobalPortImplementationRef'
	implementationNameRef
	')';

schematicGlobalPortTemplate : '(schematicGlobalPortTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	(hotspot)?
	(  annotate  |  commentGraphics  |  figure  |  globalPortNameDisplay  |  implementationNameDisplay  |  propertyDisplay  |  schematicComplexFigure  |  schematicGlobalPortAttributes  )*
	(hotspot)?
	')';

schematicGlobalPortTemplateRef : '(schematicGlobalPortTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicIfFrameBorder : '(schematicIfFrameBorder'
	schematicIfFrameBorderTemplateRef
	transform
	( conditionDisplay | propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicIfFrameBorderTemplate : '(schematicIfFrameBorderTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	usableArea
	(  annotate  |  commentGraphics | conditionDisplay | figure  |  propertyDisplay  |  schematicComplexFigure  )*
	')';

schematicIfFrameBorderTemplateRef : '(schematicIfFrameBorderTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicIfFrameImplementation : '(schematicIfFrameImplementation'
	implementationNameDef
	ifFrameRef
	schematicIfFrameImplementationHeader
	schematicFrameImplementationDetails
	')';

schematicIfFrameImplementationHeader : '(schematicIfFrameImplementationHeader'
	(  schematicIfFrameBorder  )?
	')';

schematicImplementation : '(schematicImplementation'
	(  page | totalPages )*
	')';

schematicInstanceImplementation : '(schematicInstanceImplementation'
	implementationNameDef
	instanceRef
	schematicSymbolRef
	transform
	(  cellNameDisplay  |  cellPropertyDisplayOverride  |  clusterPropertyDisplayOverride  |  designatorDisplay  |  implementationNameDisplay  |  instanceNameDisplay  |  instanceNameGeneratorDisplay  |  instancePortAttributeDisplay  |  instancePropertyDisplay  |  instanceWidthDisplay  |  nameInformation  |  pageCommentGraphics  |  parameterDisplay  |  propertyDisplayOverride  |  propertyOverride  |  timingDisplay  |  viewNameDisplay  )*
	')';

schematicInstanceImplementationRef : '(schematicInstanceImplementationRef'
	implementationNameRef
	')';

schematicInterconnectAttributeDisplay : '(schematicInterconnectAttributeDisplay'
	(  connectivityTagGeneratorDisplay  |  criticalityDisplay  |  interconnectAttachedText  |  interconnectDelayDisplay  |  interconnectNameDisplay  |  interconnectPropertyDisplay  )*
	')';

schematicInterconnectHeader : '(schematicInterconnectHeader'
	(  criticality  |  documentation  |  interconnectDelay  |  nameInformation  |  property_  |  schematicInterconnectTerminatorImplementation  |  schematicJunctionImplementation  |  schematicWireStyle  )*
	')';

schematicInterconnectTerminatorImplementation : '(schematicInterconnectTerminatorImplementation'
	implementationNameDef
	schematicInterconnectTerminatorTemplateRef
	transform
	(  implementationNameDisplay  |  nameInformation  |  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicInterconnectTerminatorImplementationRef : '(schematicInterconnectTerminatorImplementationRef'
	implementationNameRef
	')';

schematicInterconnectTerminatorTemplate : '(schematicInterconnectTerminatorTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	hotspot
	(  commentGraphics  |  figure | implementationNameDisplay |  propertyDisplay  |  schematicComplexFigure  )*
	')';

schematicInterconnectTerminatorTemplateRef : '(schematicInterconnectTerminatorTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicJunctionImplementation : '(schematicJunctionImplementation'
	implementationNameDef
	schematicJunctionTemplateRef
	transform
	(  implementationNameDisplay  |  nameInformation  |  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicJunctionImplementationRef : '(schematicJunctionImplementationRef'
	implementationNameRef
	')';

schematicJunctionTemplate : '(schematicJunctionTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	hotspot
	(  commentGraphics  |  figure | implementationNameDisplay |  propertyDisplay  |  schematicComplexFigure  )*
	')';

schematicJunctionTemplateRef : '(schematicJunctionTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicMasterPortImplementation : '(schematicMasterPortImplementation'
	implementationNameDef
	schematicMasterPortTemplateRef
	( portRef | localPortGroupRef )
	transform
	(  implementationNameDisplay  |  nameInformation  |  portAttributeDisplay  |  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicMasterPortImplementationRef : '(schematicMasterPortImplementationRef'
	implementationNameRef
	')';

schematicMasterPortTemplate : '(schematicMasterPortTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	hotspot
	portDirectionIndicator
	(  annotate  |  commentGraphics  |  figure  |  implementationNameDisplay  |  portAttributeDisplay  |  propertyDisplay  |  schematicComplexFigure  |  schematicPortStyle  )*
	')';

schematicMasterPortTemplateRef : '(schematicMasterPortTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicMetric : '(schematicMetric'
	setDistance
	( hotspotGrid | noHotspotGrid )
	(  nominalHotspotGrid  )?
	')';

schematicNet : '(schematicNet'
	interconnectNameDef
	signalRef
	schematicInterconnectHeader
	schematicNetJoined
	(  comment  |  schematicInterconnectAttributeDisplay  |  schematicNetDetails  |  userData  )*
	')';

schematicNetDetails : '(schematicNetDetails'
	( schematicNetGraphics | schematicSubNetSet )
	')';

schematicNetGraphics : '(schematicNetGraphics'
	(  comment  |  figure  |  schematicComplexFigure  |  userData  )*
	')';

schematicNetJoined : '(schematicNetJoined'
	( portJoined | joinedAsSignal )?
	(  ripperHotspotRef  |  schematicGlobalPortImplementationRef  |  schematicInterconnectTerminatorImplementationRef  |  schematicJunctionImplementationRef  |  schematicMasterPortImplementationRef  |  schematicOffPageConnectorImplementationRef  |  schematicOnPageConnectorImplementationRef  |  schematicSymbolPortImplementationRef  )*
	( portJoined | joinedAsSignal )?
	(  ripperHotspotRef  |  schematicGlobalPortImplementationRef  |  schematicInterconnectTerminatorImplementationRef  |  schematicJunctionImplementationRef  |  schematicMasterPortImplementationRef  |  schematicOffPageConnectorImplementationRef  |  schematicOnPageConnectorImplementationRef  |  schematicSymbolPortImplementationRef  )*
	')';

schematicOffPageConnectorImplementation : '(schematicOffPageConnectorImplementation'
	implementationNameDef
	schematicOffPageConnectorTemplateRef
	transform
	(  associatedInterconnectNameDisplay  |  implementationNameDisplay  |  nameInformation  |  property_  |  propertyDisplay  |  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicOffPageConnectorImplementationRef : '(schematicOffPageConnectorImplementationRef'
	implementationNameRef
	')';

schematicOffPageConnectorTemplate : '(schematicOffPageConnectorTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	hotspot
	(  annotate  |  associatedInterconnectNameDisplay  |  commentGraphics  |  figure  |  implementationNameDisplay  |  propertyDisplay  |  schematicComplexFigure  )*
	')';

schematicOffPageConnectorTemplateRef : '(schematicOffPageConnectorTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicOnPageConnectorImplementation : '(schematicOnPageConnectorImplementation'
	implementationNameDef
	schematicOnPageConnectorTemplateRef
	transform
	(  associatedInterconnectNameDisplay  |  implementationNameDisplay  |  nameInformation  |  property_  |  propertyDisplay  |  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicOnPageConnectorImplementationRef : '(schematicOnPageConnectorImplementationRef'
	implementationNameRef
	')';

schematicOnPageConnectorTemplate : '(schematicOnPageConnectorTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	hotspot
	(  annotate  |  associatedInterconnectNameDisplay  |  commentGraphics  |  figure  |  implementationNameDisplay  |  propertyDisplay  |  schematicComplexFigure  )*
	')';

schematicOnPageConnectorTemplateRef : '(schematicOnPageConnectorTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicOtherwiseFrameBorder : '(schematicOtherwiseFrameBorder'
	schematicOtherwiseFrameBorderTemplateRef
	transform
	(  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicOtherwiseFrameBorderTemplate : '(schematicOtherwiseFrameBorderTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	usableArea
	(  annotate  |  commentGraphics  |  figure  |  propertyDisplay  |  schematicComplexFigure  )*
	')';

schematicOtherwiseFrameBorderTemplateRef : '(schematicOtherwiseFrameBorderTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicOtherwiseFrameImplementation : '(schematicOtherwiseFrameImplementation'
	implementationNameDef
	otherwiseFrameRef
	schematicOtherwiseFrameImplementationHeader
	schematicFrameImplementationDetails
	')';

schematicOtherwiseFrameImplementationHeader : '(schematicOtherwiseFrameImplementationHeader'
	(  schematicOtherwiseFrameBorder  )?
	')';

schematicPortAcPower : '(schematicPortAcPower'
	(  schematicPortAcPowerRecommendedFrequency  |  schematicPortAcPowerRecommendedVoltageRms  )*
	')';

schematicPortAcPowerRecommendedFrequency : '(schematicPortAcPowerRecommendedFrequency'
	frequencyValue
	')';

schematicPortAcPowerRecommendedVoltageRms : '(schematicPortAcPowerRecommendedVoltageRms'
	voltageValue
	')';

schematicPortAnalog : '(schematicPortAnalog'
	')';

schematicPortAttributes : '(schematicPortAttributes'
	(  ieeeStandard  |  schematicPortAcPower  |  schematicPortAnalog  |  schematicPortChassisGround  |  schematicPortClock  |  schematicPortDcPower  |  schematicPortEarthGround  |  schematicPortGround  |  schematicPortNonLogical  |  schematicPortOpenCollector  |  schematicPortOpenEmitter  |  schematicPortPower  |  schematicPortThreeState  )*
	')';

schematicPortChassisGround : '(schematicPortChassisGround'
	(  schematicPortAnalog  )?
	')';

schematicPortClock : '(schematicPortClock'
	(  ieeeStandard  )?
	')';

schematicPortDcPower : '(schematicPortDcPower'
	(  schematicPortAnalog  |  schematicPortDcPowerRecommendedVoltage  )*
	')';

schematicPortDcPowerRecommendedVoltage : '(schematicPortDcPowerRecommendedVoltage'
	voltageValue
	')';

schematicPortEarthGround : '(schematicPortEarthGround'
	(  schematicPortAnalog  )?
	')';

schematicPortGround : '(schematicPortGround'
	(  schematicPortAnalog  )?
	')';

schematicPortNonLogical : '(schematicPortNonLogical'
	')';

schematicPortOpenCollector : '(schematicPortOpenCollector'
	')';

schematicPortOpenEmitter : '(schematicPortOpenEmitter'
	')';

schematicPortPower : '(schematicPortPower'
	')';

schematicPortStyle : '(schematicPortStyle'
	( narrowPort | widePort )
	')';

schematicPortThreeState : '(schematicPortThreeState'
	')';

schematicRequiredDefaults : '(schematicRequiredDefaults'
	schematicMetric
	fontRef
	textHeight
	')';

schematicRipperImplementation : '(schematicRipperImplementation'
	implementationNameDef
	schematicRipperTemplateRef
	transform
	(  implementationNameDisplay  |  nameInformation  |  property_  |  propertyDisplay  |  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicRipperImplementationRef : '(schematicRipperImplementationRef'
	implementationNameRef
	')';

schematicRipperTemplate : '(schematicRipperTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	(  annotate  |  commentGraphics  |  figure | implementationNameDisplay |  propertyDisplay  |  ripperHotspot  |  schematicComplexFigure  )*
	')';

schematicRipperTemplateRef : '(schematicRipperTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicSubBus : '(schematicSubBus'
	interconnectNameDef
	schematicSubInterconnectHeader
	schematicBusJoined
	(  comment  |  schematicBusDetails  |  schematicBusSlice  |  schematicInterconnectAttributeDisplay  |  userData  )*
	')';

schematicSubBusSet : '(schematicSubBusSet'
	(  schematicSubBus  )*
	')';

schematicSubInterconnectHeader : '(schematicSubInterconnectHeader'
	(  criticality  |  documentation  |  interconnectDelay  |  nameInformation  |  property_  )*
	')';

schematicSubNet : '(schematicSubNet'
	interconnectNameDef
	schematicSubInterconnectHeader
	schematicNetJoined
	(  comment  |  schematicInterconnectAttributeDisplay  |  schematicNetDetails  |  userData  )*
	')';

schematicSubNetSet : '(schematicSubNetSet'
	(  schematicSubNet  )*
	')';

schematicSymbol : '(schematicSymbol'
	viewNameDef
	schematicSymbolHeader
	(  annotate  |  cellNameDisplay  |  cellPropertyDisplay  |  clusterPropertyDisplay  |  comment  |  commentGraphics  |  designatorDisplay  |  figure  |  implementationNameDisplay  |  instanceNameDisplay  |  instanceNameGeneratorDisplay  |  instanceWidthDisplay  |  parameterDisplay  |  propertyDisplay  |  schematicComplexFigure  |  schematicSymbolPortImplementation  |  userData  |  viewNameDisplay  )*
	')';

schematicSymbolBorder : '(schematicSymbolBorder'
	schematicSymbolBorderTemplateRef
	transform
	(  propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicSymbolBorderTemplate : '(schematicSymbolBorderTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	usableArea
	(  annotate  |  commentGraphics  |  figure  |  propertyDisplay  |  schematicComplexFigure  )*
	')';

schematicSymbolBorderTemplateRef : '(schematicSymbolBorderTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicSymbolHeader : '(schematicSymbolHeader'
	schematicUnits
	(  backgroundColor  |  derivedFrom  |  documentation  |  nameInformation  |  originalBoundingBox  |  pageSize  |  previousVersion  |  property_  |  schematicSymbolBorder  |  status  )*
	')';

schematicSymbolPortImplementation : '(schematicSymbolPortImplementation'
	symbolPortImplementationNameDef
	portRef
	schematicSymbolPortTemplateRef
	transform
	( portAttributeDisplay | propertyDisplayOverride  |  propertyOverride  )*
	')';

schematicSymbolPortImplementationRef : '(schematicSymbolPortImplementationRef'
	symbolPortImplementationNameRef
	schematicInstanceImplementationRef
	')';

schematicSymbolPortTemplate : '(schematicSymbolPortTemplate'
	libraryObjectNameDef
	schematicTemplateHeader
	hotspot
	portDirectionIndicator
	(  annotate  |  commentGraphics  |  figure  |  implementationNameDisplay  |  portAttributeDisplay  |  propertyDisplay  |  schematicComplexFigure  |  schematicPortAttributes  |  schematicPortStyle  )*
	')';

schematicSymbolPortTemplateRef : '(schematicSymbolPortTemplateRef'
	libraryObjectNameRef
	( libraryRef )? 
	')';

schematicSymbolRef : '(schematicSymbolRef'
	viewNameRef
	')';

schematicTemplateHeader : '(schematicTemplateHeader'
	schematicUnits
	(  backgroundColor  |  documentation  |  nameInformation  |  originalBoundingBox  |  property_  |  status  )*
	')';

schematicUnits : '(schematicUnits'
	(  schematicMetric  |  setAngle  |  setCapacitance  |  setFrequency  |  setTime  |  setVoltage  )*
	')';

schematicView : '(schematicView'
	viewNameDef
	schematicViewHeader
	logicalConnectivity
	schematicImplementation
	(  comment  |  userData  )*
	')';

schematicViewHeader : '(schematicViewHeader'
	schematicUnits
	(  derivedFrom  |  documentation  |  nameInformation  |  previousVersion  |  property_  |  status  )*
	')';

schematicWireAffinity : '(schematicWireAffinity'
	( narrowWire | wideWire | unrestricted )
	')';

schematicWireStyle : '(schematicWireStyle'
	( narrowWire | wideWire )
	')';

second : '(second'
	unitExponent
	')';

secondIntegerExpression : integerExpression;

secondNumber : integerToken;

secondStringExpression : stringExpression;

section : '(section'
	sectionNameDef
	sectionTitle
	(  diagram  |  section  |  stringValue  )*
	')';

sectionNameDef : nameDef;

sectionTitle : '(sectionTitle'
	stringToken
	')';

sequence : '(sequence'
	fromInteger
	toInteger
	(  step  )?
	')';

setAngle : '(setAngle'
	unitRef
	')';

setCapacitance : '(setCapacitance'
	unitRef
	')';

setCurrent : '(setCurrent'
	unitRef
	')';

setDistance : '(setDistance'
	unitRef
	')';

setFrequency : '(setFrequency'
	unitRef
	')';

setTime : '(setTime'
	unitRef
	')';

setVoltage : '(setVoltage'
	unitRef
	')';

shape : '(shape'
	curve
	')';

siemens : '(siemens'
	unitExponent
	')';

signal : '(signal'
	signalNameDef
	signalJoined
	(  connectivityTagGenerator  |  nameInformation  |  property_  |  signalWidth  )*
	')';

signalAnnotate : '(signalAnnotate'
	( extendSignalDef | extendSignalMemberDef )
	(  comment  |  property_  |  propertyOverride  )*
	')';

signalGroup : '(signalGroup'
	signalGroupNameDef
	signalList
	(  property_ | nameInformation )*
	')';

signalGroupAnnotate : '(signalGroupAnnotate'
	extendSignalGroupDef
	(  comment  |  property_  |  propertyOverride  )*
	')';

signalGroupNameCaseSensitive : '(signalGroupNameCaseSensitive'
	booleanToken
	')';

signalGroupNameDef : nameDef;

signalGroupNameRef : nameRef;

signalGroupRef : '(signalGroupRef'
	signalGroupNameRef
	')';

signalIndexValue : '(signalIndexValue'
	')';

signalJoined : '(signalJoined'
	(  globalPortRef  |  portInstanceRef  |  portMemberRef  |  portRef  )*
	')';

signalList : '(signalList'
	(  signalGroupRef  |  signalRef  )*
	')';

signalMemberRef : '(signalMemberRef'
	signalNameRef
	indexValue
	')';

signalNameCaseSensitive : '(signalNameCaseSensitive'
	booleanToken
	')';

signalNameDef : nameDef;

signalNameRef : nameRef;

signalRef : '(signalRef'
	signalNameRef
	')';

signalWidth : '(signalWidth'
	integerExpression
	')';

simpleName : stringToken;

startPoint : pointValue;

status : '(status'
	(  comment  |  copyright  |  userData  |  written  )*
	')';

step : '(step'
	integerValue
	')';

string : '(string'
	stringExpression
	')';

stringConcatenate : '(stringConcatenate'
	(  stringExpression  )*
	')';

stringConstant : '(stringConstant'
	constantNameDef
	stringToken
	')';

stringConstantRef : '(stringConstantRef'
	constantNameRef
	')';

stringEqual : '(stringEqual'
	firstStringExpression
	secondStringExpression
	')';

stringExpression : ( stringParameterRef  | stringToken | stringConstantRef | stringConcatenate | substring | decimalToString | stringPrefix | stringSuffix )
;
stringLength : '(stringLength'
	stringExpression
	')';

stringParameter : '(stringParameter'
	parameterNameDef
	(  nameInformation  |  string  )*
	')';

stringParameterAssign : '(stringParameterAssign'
	parameterNameRef
	stringExpression
	')';

stringParameterRef : '(stringParameterRef'
	parameterNameRef
	')';

stringPrefix : '(stringPrefix'
	stringExpression
	substringLength
	')';

stringSuffix : '(stringSuffix'
	stringExpression
	substringOffset
	')';

stringValue : stringToken;

strong : '(strong'
	logicNameRef
	')';

subIssue : integerToken;

substring : '(substring'
	stringExpression
	substringLength
	substringOffset
	')';

substringLength : integerExpression;

substringOffset : integerExpression;

subtrahend : integerExpression;

symbolicLayoutUnits : '(symbolicLayoutUnits'
	(  setAngle  |  setCapacitance  |  setDistance  |  setTime  )*
	')';

symbolicLayoutView : '(symbolicLayoutView'
	viewNameDef
	(  comment  | userData  )*
	( nameInformation )?
	(  comment  | userData  )*
	')';

symbolPortImplementationNameDef : nameDef;

symbolPortImplementationNameRef : nameRef;

technology : '(technology'
	physicalScaling
	(  comment  |  figureGroup | logicDefinitions |  userData  )*
	')';
	
textHeight : '(textHeight'
	lengthValue
	')';

throughPoint : numberPoint;

time : '(time'
	hourNumber
	minuteNumber
	secondNumber
	')';

timeDelay : timeValue;

timeInterval : '(timeInterval'
	( event | offsetEvent )
	( event | offsetEvent | duration )
	')';

timeStamp : '(timeStamp'
	date
	time
	')';

timeValue : miNoMaxValue;

timing : '(timing'
	timingNameDef
	derivation
	(  comment  |  forbiddenEvent  |  pathDelay  |  property_  |  userData  )*
	')';

timingDisplay : '(timingDisplay'
	timingNameRef
	(  display  )*
	')';

timingNameDef : nameDef;

timingNameRef : nameRef;

toInteger : integerToken;

topJustify : '(topJustify'
	')';

totalPages : '(totalPages'
	integerToken
	')';

totalPagesDisplay : '(totalPagesDisplay'
	( addDisplay | replaceDisplay | removeDisplay )
	')';

transform : '(transform'
	(  origin  |  rotation  |  scaleX  |  scaleY  )*
	')';

transition : '(transition'
	previousLogicValue
	presentLogicValue
	')';

etrue : '(true'
	')';

truncate : '(truncate'
	')';

typedValue : ( eboolean  | integer | miNoMax | number | point | string );

typeface : '(typeface'
	fontFamily
	(  typefaceSuffix  )?
	')';

typefaceSuffix : '(typefaceSuffix'
	stringToken
	')';

unconfigured : '(unconfigured'
	')';

unconstrained : '(unconstrained'
	')';

undefined : '(undefined'
	')';

unit : '(unit'
	unitNameDef
	numberOfNewUnits
	numberOfBasicUnits
	(  ampere  |  candela  |  celsius  |  coulomb  |  degree  |  fahrenheit  |  farad  |  henry  |  hertz  |  joule  |  kelvin  |  kilogram  |  meter  |  mole  |  ohm  |  radian  |  second  |  siemens  |  volt  |  watt  |  weber  )*
	')';

unitDefinitions : '(unitDefinitions'
	(  unit  )*
	')';

unitExponent : scaledInteger;

unitNameDef : nameDef;

unitNameRef : nameRef;

unitRef : '(unitRef'
	unitNameRef
	')';
	
unrestricted : '(unrestricted'
	')';

unspecified : '(unspecified'
	')';

unspecifiedDirectionPort : '(unspecifiedDirectionPort'
	')';

untyped : '(untyped'
	')';

unused : '(unused'
	')';

usableArea : '(usableArea'
	(  rectangle  )*
	')';

userData : '(userData'
	userDataTag
	(  IDENTIFIER  |  integerToken  |  stringToken  |  userData  )*
	')';

userDataTag : IDENTIFIER;

version : '(version'
	stringValue
	')';

verticalJustification : '(verticalJustification'
	( bottomJustify | baselineJustify | middleJustify | caplineJustify | topJustify )
	')';

viewGroup : '(viewGroup'
	viewGroupNameDef
	viewGroupHeader
	(  comment  |  userData  |  viewGroupRef  |  viewRef  )*
	')';

viewGroupHeader : '(viewGroupHeader'
	(  documentation  |  nameInformation  |  property_  |  reason  )*
	')';

viewGroupNameCaseSensitive : '(viewGroupNameCaseSensitive'
	booleanToken
	')';

viewGroupNameDef : nameDef;

viewGroupNameRef : nameRef;

viewGroupRef : '(viewGroupRef'
	viewGroupNameRef
	')';

viewNameCaseSensitive : '(viewNameCaseSensitive'
	booleanToken
	')';

viewNameDef : nameDef;

viewNameDisplay : '(viewNameDisplay'
	(  display  |  displayNameOverride  )*
	')';

viewNameRef : nameRef;

viewPropertyDisplay : '(viewPropertyDisplay'
	propertyNameRef
	(  display | propertyNameDisplay )*
	')';

viewPropertyOverride : '(viewPropertyOverride'
	propertyNameRef
	( typedValue | untyped )
	(  comment  | fixed | propertyOverride  )*
	')';

viewRef : '(viewRef'
	viewNameRef
	( clusterRef )? 
	')';

visible : '(visible'
	booleanValue
	')';

volt : '(volt'
	unitExponent
	')';

voltageMap : '(voltageMap'
	voltageValue
	')';

voltageValue : miNoMaxValue;

watt : '(watt'
	unitExponent
	')';

weak_ : '(weak'
	logicNameRef
	')';

weakJoined : '(weakJoined'
	(  interfaceJoined  |  portRef  )*
	')';

weber : '(weber'
	unitExponent
	')';

widePort : '(widePort'
	')';

wideWire : '(wideWire'
	')';

written : '(written'
	timeStamp
	(  author  |  comment  |  dataOrigin  |  program  |  userData  )*
	')';

xCoordinate : integerValue;

xNumberValue : numberValue;

xor_ : '(xor'
	(  booleanExpression  )*
	')';

yCoordinate : integerValue;

year : '(year'
	( yearNumber )*
	')';

yearNumber : integerToken;

yNumberValue : numberValue;

integerToken :DECIMAL_LITERAL;
stringToken :STRING_LITERAL;

IDENTIFIER : (SPECIAL)* (DIG)* (LETTER|'&'|UNDERLINE) ( LETTER | DIG | UNDERLINE |SPECIAL)* ;

STRING_LITERAL : '"' (~'"')* '"' ;

DECIMAL_LITERAL : INTEGER ( '.' INTEGER* )? ;

fragment
INTEGER : ('-'|'+')? DIG (DIG)* ;
fragment
LETTER : UPCASE |LOWCASE ;
fragment
UPCASE: [A-Z];
fragment
LOWCASE: [a-z];
fragment
DIG: [0-9];
fragment
UNDERLINE : '_';
fragment
SPECIAL: '!' | '#' | '$' | '&'| '*'| '+' | ',' | '-' | '.' | '/' | ':' | ';' |
'<' | '=' | '>' | '?' | '@' | '[' | ']'| '^' | '`' | '{' | '|' | '}' | '~'| '\\' ; 

WS : [ \t\r\n]+ -> skip;
