/*
 * Grammar for text-based (AKA ASCII) encoding of FBX format,
 * a format originally developed by Kaydara but now owned by Autodesk.
 *
 * This experimental parser tries to recognise the individual node names
 * and more strictly checks their contents. It is likely very incomplete
 * as we don't have official documentation for the format, but parses all
 * current examples fine.
 */
grammar FBXSemantic;

// PARSER RULES

start
    : nodeFBXHeaderExtension
      nodeGlobalSettings?
      nodeCreationTime?
      nodeCreator?
      nodeSceneInfo?
      nodeDocuments?
      nodeReferences?
      nodeDefinitions
      nodeObjects
      nodeRelations?
      nodeConnections
      nodeTakes
      nodeVersion5?
      EOF
    ;

nodeFBXHeaderExtension
    : 'FBXHeaderExtension' COLON OPEN_BRACE
      nodeFBXHeaderVersion
      nodeFBXVersion
      nodeCreationTimeStamp?
      nodeCreator?
      nodeSceneInfo?
      nodeOtherFlags?
      CLOSE_BRACE
    ;

nodeFBXHeaderVersion: 'FBXHeaderVersion' COLON value=INTEGER;
nodeFBXVersion: 'FBXVersion' COLON value=INTEGER;

nodeCreationTimeStamp
    : 'CreationTimeStamp' COLON OPEN_BRACE
      nodeVersion
      nodeYear
      nodeMonth
      nodeDay
      nodeHour
      nodeMinute
      nodeSecond
      nodeMillisecond
      CLOSE_BRACE
    ;

nodeYear: 'Year' COLON value=INTEGER;
nodeMonth: 'Month' COLON value=INTEGER;
nodeDay: 'Day' COLON value=INTEGER;
nodeHour: 'Hour' COLON value=INTEGER;
nodeMinute: 'Minute' COLON value=INTEGER;
nodeSecond: 'Second' COLON value=INTEGER;
nodeMillisecond: 'Millisecond' COLON value=INTEGER;

nodeCreationTime: 'CreationTime' COLON value=STRING;
nodeCreator: 'Creator' COLON value=STRING;

nodeSceneInfo
    : 'SceneInfo' COLON x=STRING COMMA y=STRING OPEN_BRACE
      nodeType
      nodeVersion
      nodeMetaData
      ( nodeProperties60 | nodeProperties70 )
      CLOSE_BRACE
    ;

nodeMetaData
    : 'MetaData' COLON OPEN_BRACE
      nodeVersion
      nodeTitle
      nodeSubject
      nodeAuthor
      nodeKeywords
      nodeRevision
      nodeComment
      CLOSE_BRACE
    ;

nodeTitle: 'Title' COLON value=STRING;
nodeSubject: 'Subject' COLON value=STRING;
nodeAuthor: 'Author' COLON value=STRING;
nodeKeywords: 'Keywords' COLON value=STRING;
nodeRevision: 'Revision' COLON value=STRING;
nodeComment: 'Comment' COLON value=STRING;

nodeOtherFlags
    : 'OtherFlags' COLON
      OPEN_BRACE
      nodeFlagPLE
      CLOSE_BRACE
      ;

nodeFlagPLE: 'FlagPLE' COLON x=INTEGER;

nodeDocuments
    : 'Documents' COLON OPEN_BRACE
      nodeCount
      nodeDocument*
      CLOSE_BRACE
    ;

nodeDocument
    : 'Document' COLON x=INTEGER COMMA a=STRING COMMA b=STRING OPEN_BRACE
      ( nodeProperties60 | nodeProperties70 )?
      nodeRootNode?
      CLOSE_BRACE
    ;

nodeRootNode: 'RootNode' COLON value=INTEGER;

nodeReferences
    : 'References' COLON OPEN_BRACE
      CLOSE_BRACE
    ;

nodeDefinitions
    : 'Definitions' COLON OPEN_BRACE
      nodeVersion
      nodeCount
      nodeObjectType*
      CLOSE_BRACE
    ;

nodeObjectType
    : 'ObjectType' COLON objectType=STRING OPEN_BRACE
      nodeCount
      nodePropertyTemplate?
      CLOSE_BRACE
    ;

nodeCount: 'Count' COLON value=INTEGER;

nodePropertyTemplate
    : 'PropertyTemplate' COLON x=STRING OPEN_BRACE
      ( nodeProperties60 | nodeProperties70 )
      CLOSE_BRACE
    ;

nodeObjects
    : 'Objects' COLON OPEN_BRACE
      nodeNodeAttribute*
      nodeGeometry*
      nodeModel*
      nodeAnimationStack*
      nodeAnimationLayer*
      nodeAnimationCurveNode*
      nodeAnimationCurve*
      nodeCollectionExclusive*
      nodeMaterial*
      nodePose*
      nodeGlobalSettings?
      CLOSE_BRACE
    ;

nodeRelations
    : 'Relations' COLON OPEN_BRACE
      nodeModel*
      nodeMaterial*
      CLOSE_BRACE
    ;

nodeNodeAttribute
    : 'NodeAttribute' COLON x=INTEGER COMMA objectId=STRING COMMA objectType=STRING OPEN_BRACE
      ( nodeProperties60 | nodeProperties70 )?
      nodeTypeFlags
      CLOSE_BRACE
    ;

nodeTypeFlags: 'TypeFlags' COLON value=STRING;

nodeGeometry
    : 'Geometry' COLON ( objectNumber=INTEGER COMMA )? objectId=STRING COMMA objectType=STRING OPEN_BRACE
      nodeVertices
      nodePolygonVertexIndex
      nodeGeometryVersion
      nodeLayerElementMaterial
      nodeLayer
      CLOSE_BRACE
    ;

nodeModel
    : 'Model' COLON ( objectNumber=INTEGER COMMA )? objectId=STRING COMMA objectType=STRING OPEN_BRACE
      nodeVersion?
      ( nodeProperties60 | nodeProperties70 )?
      nodeMultiLayer?
      nodeMultiTake?
      nodeShading?
      nodeCulling?
      nodeVertices?
      nodePolygonVertexIndex?
      nodeGeometryVersion?
      nodeLayerElementNormal?
      nodeLayerElementSmoothing?
      nodeLayerElementUV?
      nodeLayerElementTexture?
      nodeLayerElementMaterial?
      nodeLayer?
      CLOSE_BRACE
    ;

nodeMultiTake: 'MultiTake' COLON value=INTEGER;
nodeShading: 'Shading' COLON value=('Y'|'N');
nodeCulling: 'Culling' COLON value=STRING;

nodeVertices
    : 'Vertices' COLON
      ( value=decimalList
      | '*' count=INTEGER OPEN_BRACE
        'a' COLON value=decimalList
        CLOSE_BRACE
      )
    ;

nodePolygonVertexIndex
    : 'PolygonVertexIndex' COLON
      ( value=integerList
      | '*' count=INTEGER OPEN_BRACE
        'a' COLON value=integerList
        CLOSE_BRACE
      )
    ;

nodeGeometryVersion: 'GeometryVersion' COLON value=INTEGER;

nodeLayerElementNormal
    : 'LayerElementNormal' COLON x=INTEGER OPEN_BRACE
      nodeVersion
      nodeName
      nodeMappingInformationType
      nodeReferenceInformationType
      nodeNormals
      CLOSE_BRACE
    ;

nodeLayerElementSmoothing
    : 'LayerElementSmoothing' COLON x=INTEGER OPEN_BRACE
      nodeVersion
      nodeName
      nodeMappingInformationType
      nodeReferenceInformationType
      nodeSmoothing
      CLOSE_BRACE
    ;

nodeLayerElementUV
    : 'LayerElementUV' COLON x=INTEGER OPEN_BRACE
      nodeVersion
      nodeName
      nodeMappingInformationType
      nodeReferenceInformationType
      nodeUV
      nodeUVIndex
      CLOSE_BRACE
    ;

nodeLayerElementTexture
    : 'LayerElementTexture' COLON x=INTEGER OPEN_BRACE
      nodeVersion
      nodeName
      nodeMappingInformationType
      nodeReferenceInformationType
      nodeBlendMode
      nodeTextureAlpha
      nodeTextureId
      CLOSE_BRACE
    ;

nodeLayerElementMaterial
    : 'LayerElementMaterial' COLON x=INTEGER OPEN_BRACE
      nodeVersion
      nodeName
      nodeMappingInformationType
      nodeReferenceInformationType
      nodeMaterials
      CLOSE_BRACE
    ;

nodeName: 'Name' COLON value=STRING;
nodeMappingInformationType: 'MappingInformationType' COLON value=STRING;
nodeReferenceInformationType: 'ReferenceInformationType' COLON value=STRING;
nodeNormals: 'Normals' COLON value=decimalList;
nodeSmoothing: 'Smoothing' COLON value=INTEGER;
nodeUV: 'UV' COLON value=decimalList;
nodeUVIndex: 'UVIndex' COLON value=integerList;
nodeBlendMode: 'BlendMode' COLON value=STRING;
nodeTextureAlpha: 'TextureAlpha' COLON value=INTEGER;
nodeTextureId: 'TextureId' COLON value=STRING?;

nodeMaterials
    : 'Materials' COLON
      ( value=integerList
      | '*' count=INTEGER OPEN_BRACE
        'a' COLON value=integerList
        CLOSE_BRACE
      )
    ;

nodeLayer
    : 'Layer' COLON x=INTEGER OPEN_BRACE
      nodeVersion
      nodeLayerElement*
      CLOSE_BRACE
    ;

nodeLayerElement
    : 'LayerElement' COLON OPEN_BRACE
      nodeType
      nodeTypedIndex
      CLOSE_BRACE
    ;

nodeTypedIndex: 'TypedIndex' COLON value=INTEGER;

nodeAnimationStack
    : 'AnimationStack' COLON ( objectNumber=INTEGER COMMA )? objectId=STRING COMMA objectType=STRING OPEN_BRACE
      ( nodeProperties60 | nodeProperties70 )
      CLOSE_BRACE
    ;

nodeAnimationLayer
    : 'AnimationLayer' COLON ( objectNumber=INTEGER COMMA )? objectId=STRING COMMA objectType=STRING OPEN_BRACE
      CLOSE_BRACE
    ;

nodeAnimationCurveNode
    : 'AnimationCurveNode' COLON ( objectNumber=INTEGER COMMA )? objectId=STRING COMMA objectType=STRING OPEN_BRACE
      ( nodeProperties60 | nodeProperties70 )
      CLOSE_BRACE
    ;

nodeAnimationCurve
    : 'AnimationCurve' COLON ( objectNumber=INTEGER COMMA )? objectId=STRING COMMA objectType=STRING OPEN_BRACE
      nodeDefault
      nodeKeyVer
      nodeKeyTime
      nodeKeyValueFloat
      nodeKeyAttrFlags
      nodeKeyAttrDataFloat
      nodeKeyAttrRefCount
      CLOSE_BRACE
    ;

nodeDefault: 'Default' COLON value=INTEGER;
nodeKeyVer: 'KeyVer' COLON value=INTEGER;

nodeKeyTime
    : 'KeyTime' COLON '*' count=INTEGER OPEN_BRACE
      'a' COLON value=integerList
      CLOSE_BRACE
    ;

nodeKeyValueFloat
    : 'KeyValueFloat' COLON '*' count=INTEGER OPEN_BRACE
      'a' COLON value=decimalList
      CLOSE_BRACE
    ;

nodeKeyAttrFlags
    : 'KeyAttrFlags' COLON '*' count=INTEGER OPEN_BRACE
      'a' COLON value=integerList
      CLOSE_BRACE
    ;

nodeKeyAttrDataFloat
    : 'KeyAttrDataFloat' COLON '*' count=INTEGER OPEN_BRACE
      'a' COLON value=decimalList
      CLOSE_BRACE
    ;

nodeKeyAttrRefCount
    : 'KeyAttrRefCount' COLON '*' count=INTEGER OPEN_BRACE
      'a' COLON value=integerList
      CLOSE_BRACE
    ;

nodeCollectionExclusive
    : 'CollectionExclusive' COLON ( objectNumber=INTEGER COMMA )? objectId=STRING COMMA objectType=STRING OPEN_BRACE
       CLOSE_BRACE
    ;

nodeMaterial
    : 'Material' COLON ( objectNumber=INTEGER COMMA )? objectId=STRING COMMA objectType=STRING OPEN_BRACE
      ( nodeVersion
        nodeShadingModel
        nodeMultiLayer
        ( nodeProperties60 | nodeProperties70 )
      )?
      CLOSE_BRACE
    ;

nodeShadingModel: 'ShadingModel' COLON value=STRING;

nodeMultiLayer: 'MultiLayer' COLON value=INTEGER;

nodePose
    : 'Pose' COLON ( objectNumber=INTEGER COMMA )? objectId=STRING COMMA objectType=STRING OPEN_BRACE
      nodeType
      nodeVersion
      ( nodeProperties60 | nodeProperties70 )
      nodeNbPoseNodes
      nodePoseNode
      CLOSE_BRACE
    ;

nodeNbPoseNodes: 'NbPoseNodes' COLON value=INTEGER;

nodePoseNode
    : 'PoseNode' COLON OPEN_BRACE
      nodeNode
      nodeMatrix
      CLOSE_BRACE
    ;

nodeNode: 'Node' COLON value=STRING;

nodeMatrix: 'Matrix' COLON value=decimalList;

nodeGlobalSettings
    : 'GlobalSettings' COLON OPEN_BRACE
      nodeVersion
      ( nodeProperties60 | nodeProperties70 )
      CLOSE_BRACE
    ;

nodeType: 'Type' COLON value=STRING;

nodeProperties60
    : 'Properties60' COLON OPEN_BRACE
      nodeProperty*
      CLOSE_BRACE
    ;

nodeProperty: 'Property' COLON propertyName=STRING COMMA propertyType=STRING COMMA propertyValue=anyValueList;

nodeProperties70
    : 'Properties70' COLON OPEN_BRACE
      nodeP*
      CLOSE_BRACE
    ;

nodeP
    : 'P'
      COLON propertyName=STRING
      COMMA propertyType1=STRING
      COMMA propertyType2=STRING
      COMMA propertyValue=anyValueList
    ;

nodeConnections
    : 'Connections' COLON OPEN_BRACE
      ( nodeConnect | nodeC )*
      CLOSE_BRACE
    ;

nodeConnect: 'Connect' COLON x=STRING COMMA from=STRING COMMA to=STRING;

nodeC: 'C' COLON x=STRING COMMA from=INTEGER COMMA to=INTEGER ( COMMA connectType=STRING )?;

nodeTakes
    : 'Takes' COLON OPEN_BRACE
      nodeCurrent?
      nodeTake*
      CLOSE_BRACE
    ;

nodeTake
    : 'Take' COLON name=STRING OPEN_BRACE
      nodeFileName
      nodeLocalTime
      nodeReferenceTime
      CLOSE_BRACE
    ;

nodeFileName: 'FileName' COLON value=STRING;
nodeLocalTime: 'LocalTime' COLON value=integerList;
nodeReferenceTime: 'ReferenceTime' COLON value=integerList;

nodeCurrent: 'Current' COLON value=STRING;

nodeVersion5
    : 'Version5' COLON OPEN_BRACE
      nodeAmbientRenderSettings
      nodeFogOptions
      nodeSettings
      nodeRendererSetting
      CLOSE_BRACE
    ;

nodeAmbientRenderSettings
    : 'AmbientRenderSettings' COLON OPEN_BRACE
      nodeVersion
      nodeAmbientLightColor
      CLOSE_BRACE
    ;

nodeAmbientLightColor: 'AmbientLightColor' COLON value=decimalList;

nodeFogOptions
    : 'FogOptions' COLON OPEN_BRACE
      nodeFogEnable
      nodeFogMode
      nodeFogDensity
      nodeFogStart
      nodeFogEnd
      nodeFogColor
      CLOSE_BRACE
    ;

nodeFogEnable: 'FogEnable' COLON value=INTEGER;
nodeFogMode: 'FogMode' COLON value=INTEGER;
nodeFogDensity: 'FogDensity' COLON value=DECIMAL;
nodeFogStart: 'FogStart' COLON value=DECIMAL;
nodeFogEnd: 'FogEnd' COLON value=DECIMAL;
nodeFogColor: 'FogColor' COLON value=decimalList;

nodeSettings
    : 'Settings' COLON OPEN_BRACE
      nodeFrameRate
      nodeTimeFormat
      nodeSnapOnFrames
      nodeReferenceTimeIndex
      nodeTimeLineStartTime
      nodeTimeLineStopTime
      CLOSE_BRACE
    ;

nodeFrameRate: 'FrameRate' COLON value=STRING;
nodeTimeFormat: 'TimeFormat' COLON value=INTEGER;
nodeSnapOnFrames: 'SnapOnFrames' COLON value=INTEGER;
nodeReferenceTimeIndex: 'ReferenceTimeIndex' COLON value=INTEGER;
nodeTimeLineStartTime: 'TimeLineStartTime' COLON value=INTEGER;
nodeTimeLineStopTime: 'TimeLineStopTime' COLON value=INTEGER;

nodeRendererSetting
    : 'RendererSetting' COLON OPEN_BRACE
      nodeDefaultCamera
      nodeDefaultViewingMode
      CLOSE_BRACE
    ;

nodeDefaultCamera: 'DefaultCamera' COLON value=STRING;
nodeDefaultViewingMode: 'DefaultViewingMode' COLON value=INTEGER;

nodeVersion: 'Version' COLON value=INTEGER;

integerList: ( integer ( COMMA integer )* )?;
integer: value=INTEGER;

decimalList: ( decimal ( COMMA decimal )* )?;
decimal: value=( INTEGER | DECIMAL );

anyValueList: ( anyValue ( COMMA anyValue )* )?;
anyValue: value=( INTEGER | DECIMAL | STRING );

// LEXER RULES

INTEGER : '-'? ( DIGIT )+;
DECIMAL : INTEGER ( '.' DIGIT* )? ( 'e' INTEGER )?;

// Unknown behaviour: What if I want to escape a quote?
STRING : '"' ( ~["] )* '"';

IDENTIFIER : ( 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' )+;

fragment DIGIT : '0' .. '9';

QUOTE : '"';
COLON : ':';
COMMA : ',';
OPEN_BRACE : '{';
CLOSE_BRACE : '}';

COMMENT : ';' NON_NL* NL -> skip;

WS : ( ' ' | '\t' | NL )+ -> skip;
NL : ( '\r' '\n'? | '\n' );
NON_NL : ~[\r\n];
