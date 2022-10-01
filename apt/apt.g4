//  Generated with UniGrammar (https://gitlab.com/KOLANICH/UniGrammar.py)
//  for antlr4 (https://github.com/antlr/antlr4) backend
grammar apt;

//  productions
record: commented=commenterR? rType=TypeR WSS options=optionsR? uri=uriR WSS distribution=wordWithDash components=componentsR WSS? EOF;
wordWithDashSegment: Word | Dash;
wordWithDash: wordWithDashSegment+;
component: WSS cId=wordWithDash;
componentsR: component+;

optionsR: openingBrace=OptionsStart pairs=optionsList closingBrace=OptionsEnd WSS;
optionsList: firstOption=optionR restOptions=additionalOptions;
additionalOptions: additionalOption*;
additionalOption: separator=OptionsSeparator option=optionR;
optionR: key=OptionName OptionNameValueSeparator value=optionValue;

wordWithPlus: Plus word=Word;
uriSchema: word=Word restWords=restSchemaWords;
restSchemaWords: wordWithPlus*;
genericURI: schema=uriSchema Colon restOfURI=nonSpaceString;

uriR: cdromURI | genericURI;

commenterR: CommentMarker WSS;

optionValueSegment: Word | PunctuationAllowedInOptionValue | Dash | OptionName | CdromSchema | TypeR | Plus | Colon;
optionValue: optionValueSegment+;
nonSquareBracketStringSegment: NonWhitespaceNonOptionValueNonSquareRightBracketNonEq | optionValueSegment | OptionNameValueSeparator;
nonSquareBracketString: nonSquareBracketStringSegment+;
nonSpaceStringSegment: nonSquareBracketStringSegment | OptionsEnd;
nonSpaceString: nonSpaceStringSegment+;

singleTickEnclosedString: SingleTick nonSquareBracketString SingleTick;
doubleTickEnclosedString: DoubleTick nonSquareBracketString DoubleTick;
tickEnclosedString: singleTickEnclosedString | doubleTickEnclosedString;
enclosedString: OptionsStart tickEnclosedString OptionsEnd;
cdromURI: CdromSchema Colon enclosedString nonSpaceString;


//  keywords
TypeR: 'deb' | 'deb-src';
OptionName: 'arch' | 'lang' | 'target' | 'pdiffs' | 'by-hash' | 'valid-until-max' | 'allow-downgrade-to-insecure' | 'allow-weak' | 'allow-insecure' | 'trusted' | 'signed-by' | 'check-valid-until' | 'valid-until-min' | 'check-date' | 'inrelease-path' | 'date-max-future';
CdromSchema: 'cdrom:';


//  tokens
Word: WordChar+;
WSS: WS+;

//  characters
WS: [ \t\n\r\u{000b}\u{000c}];
PunctuationAllowedInOptionValue: [/.];
OptionsStart: '[';
OptionsEnd: ']';
OptionNameValueSeparator: '=';
CommentMarker: '#';
Plus: '+';
Colon: ':';
OptionsSeparator: ',';
Dash: '-';
SingleTick: '\'';
DoubleTick: '"';
WordChar: [0-9A-Z_a-z];
NonWhitespaceNonOptionValueNonSquareRightBracketNonEq: ~[\t-\r "#'+-:=A-\]_a-z];


