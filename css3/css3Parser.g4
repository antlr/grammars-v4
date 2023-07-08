parser grammar css3Parser;

options {
    tokenVocab=css3Lexer;
}

stylesheet
    : ws ( charset ( Comment | Space | Cdo | Cdc )* )* ( imports ( Comment | Space | Cdo | Cdc )* )* ( namespace_ ( Comment | Space | Cdo | Cdc )* )* ( nestedStatement ( Comment | Space | Cdo | Cdc )* )* EOF
    ;

charset
    : Charset ws String_ ws ';' ws    # goodCharset
    | Charset ws String_ ws           # badCharset
    ;

imports
    : Import ws ( String_ | url ) ws mediaQueryList ';' ws     # goodImport
    | Import ws ( String_ | url ) ws ';' ws                    # goodImport
    | Import ws ( String_ | url ) ws mediaQueryList            # badImport
    | Import ws ( String_ | url ) ws                           # badImport
    ;

// Namespaces
// https://www.w3.org/TR/css-namespaces-3/
namespace_
    : Namespace ws (namespacePrefix ws)? ( String_ | url ) ws ';' ws    # goodNamespace
    | Namespace ws (namespacePrefix ws)? ( String_ | url ) ws           # badNamespace
    ;

namespacePrefix
    : ident
    ;

// Media queries
// https://www.w3.org/TR/css3-mediaqueries/
media
    : Media ws mediaQueryList groupRuleBody ws
    ;

mediaQueryList
    : ( mediaQuery ( Comma ws mediaQuery )* )? ws
    ;

mediaQuery
    : ( MediaOnly | Not )? ws mediaType ws ( And ws mediaExpression )*
    | mediaExpression ( And ws mediaExpression )*
    ;

mediaType
    : ident
    ;

mediaExpression
    : '(' ws mediaFeature ( ':' ws expr )? ')' ws    // Grammar allows for 'and(', which gets tokenized as Function. In practice, people always insert space before '(' to have it work on Chrome.
    ;

mediaFeature
    : ident ws
    ;

// Page
page
    : Page ws pseudoPage? '{' ws declaration? ( ';' ws declaration? )* '}' ws
    ;

pseudoPage
    : ':' ident ws
    ;

// Selectors
// https://www.w3.org/TR/css3-selectors/
selectorGroup
    : selector ( Comma ws selector )*
    ;

selector
    : simpleSelectorSequence ws ( combinator simpleSelectorSequence ws )*
    ;

combinator
    : Plus ws
    | Greater ws
    | Tilde ws
    | Space ws
    ;

simpleSelectorSequence
    : ( typeSelector | universal ) ( Hash | className | attrib | pseudo | negation )*
    | ( Hash | className | attrib | pseudo | negation )+
    ;

typeSelector
    : typeNamespacePrefix? elementName
    ;

typeNamespacePrefix
    : ( ident | '*' )? '|'
    ;

elementName
    : ident
    ;

universal
    : typeNamespacePrefix? '*'
    ;

className
    : '.' ident
    ;

attrib
    : '[' ws typeNamespacePrefix? ident ws ( ( PrefixMatch | SuffixMatch | SubstringMatch | '=' | Includes | DashMatch ) ws ( ident | String_ ) ws )? ']'
    ;

pseudo
    /* '::' starts a pseudo-element, ':' a pseudo-class */
    /* Exceptions: :first-line, :first-letter, :before And :after. */
    /* Note that pseudo-elements are restricted to one per selector And */
    /* occur MediaOnly in the last simple_selector_sequence. */
    : ':' ':'? ( ident | functionalPseudo )
    ;

functionalPseudo
    : Function_ ws expression ')'
    ;

expression
    /* In CSS3, the expressions are identifiers, strings, */
    /* or of the form "an+b" */
    : ( ( Plus | Minus | Dimension | UnknownDimension | Number | String_ | ident ) ws )+
    ;

negation
    : PseudoNot ws negationArg ws ')'
    ;

negationArg
    : typeSelector
    | universal
    | Hash
    | className
    | attrib
    | pseudo
    ;

// Rules
operator_
    : '/' ws      # goodOperator
    | Comma ws    # goodOperator
    | Space ws    # goodOperator
    | '=' ws      # badOperator  // IE filter and DXImageTransform function
    ;

property_
    : ident ws       # goodProperty
    | Variable ws    # goodProperty
    | '*' ident      # badProperty  // IE hacks
    | '_' ident      # badProperty  // IE hacks
    ;

ruleset
    : selectorGroup '{' ws declarationList? '}' ws    # knownRuleset
    | any_* '{' ws declarationList? '}' ws             # unknownRuleset
    ;

declarationList
    : ( ';' ws )* declaration ws ( ';' ws declaration? )*
    ;

declaration
    : property_ ':' ws expr prio?    # knownDeclaration
    | property_ ':' ws value         # unknownDeclaration
    ;

prio
    : Important ws
    ;

value
    : ( any_ | block | AtKeyword ws )+
    ;

expr
    : term ( operator_? term )*
    ;

term
    : number ws              # knownTerm
    | percentage ws          # knownTerm
    | dimension ws           # knownTerm
    | String_ ws              # knownTerm
    | UnicodeRange ws        # knownTerm
    | ident ws               # knownTerm
    | var_                   # knownTerm
    | url ws                 # knownTerm
    | hexcolor               # knownTerm
    | calc                   # knownTerm
    | function_              # knownTerm
    | unknownDimension ws    # unknownTerm
    | dxImageTransform       # badTerm
    ;

function_
    : Function_ ws expr ')' ws
    ;

dxImageTransform
    : DxImageTransform ws expr ')' ws    // IE DXImageTransform function
    ;

hexcolor
    : Hash ws
    ;

number
    : ( Plus | Minus )? Number
    ;

percentage
    : ( Plus | Minus )? Percentage
    ;

dimension
    : ( Plus | Minus )? Dimension
    ;

unknownDimension
    : ( Plus | Minus )? UnknownDimension
    ;

// Error handling
any_
    : ident ws
    | number ws
    | percentage ws
    | dimension ws
    | unknownDimension ws
    | String_ ws
    //| Delim ws    // Not implemented yet
    | url ws
    | Hash ws
    | UnicodeRange ws
    | Includes ws
    | DashMatch ws
    | ':' ws
    | Function_ ws ( any_ | unused )* ')' ws
    | '(' ws ( any_ | unused )* ')' ws
    | '[' ws ( any_ | unused )* ']' ws
    ;

atRule
    : AtKeyword ws any_* ( block | ';' ws )    # unknownAtRule
    ;

unused
    : block
    | AtKeyword ws
    | ';' ws
    | Cdo ws
    | Cdc ws
    ;

block
    : '{' ws (  declarationList | nestedStatement | any_ | block | AtKeyword ws | ';' ws )* '}' ws
    ;

// Conditional
// https://www.w3.org/TR/css3-conditional/
nestedStatement
    : ruleset
    | media
    | page
    | fontFaceRule
    | keyframesRule
    | supportsRule
    | viewport
    | counterStyle
    | fontFeatureValuesRule
    | atRule
    ;

groupRuleBody
    : '{' ws nestedStatement* '}' ws
    ;

supportsRule
    : Supports ws supportsCondition ws groupRuleBody
    ;

supportsCondition
    : supportsNegation
    | supportsConjunction
    | supportsDisjunction
    | supportsConditionInParens
    ;

supportsConditionInParens
    : '(' ws supportsCondition ws ')'
    | supportsDeclarationCondition
    | generalEnclosed
    ;

supportsNegation
    : Not ws Space ws supportsConditionInParens
    ;

supportsConjunction
    : supportsConditionInParens ( ws Space ws And ws Space ws supportsConditionInParens )+
    ;

supportsDisjunction
    : supportsConditionInParens ( ws Space ws Or ws Space ws supportsConditionInParens )+
    ;

supportsDeclarationCondition
    : '(' ws declaration ')'
    ;

generalEnclosed
    : ( Function_ | '(' ) ( any_ | unused )* ')'
    ;

// Url
// https://www.w3.org/TR/css3-values/#urls
url
    : Url_ ws String_ ws ')'
    | Url
    ;

// Variable
// https://www.w3.org/TR/css-variables-1
var_
    : Var ws Variable ws ')' ws
    ;

// Calc
// https://www.w3.org/TR/css3-values/#calc-syntax
calc
    : Calc ws calcSum ')' ws
    ;

calcSum
    : calcProduct ( Space ws ( Plus | Minus ) ws Space ws calcProduct )*
    ;

calcProduct
    : calcValue ( '*' ws calcValue | '/' ws number ws )*
    ;

calcValue
    : number ws
    | dimension ws
    | unknownDimension ws
    | percentage ws
    | '(' ws calcSum ')' ws
    ;

// Font face
// https://www.w3.org/TR/2013/CR-css-fonts-3-20131003/#font-face-rule
fontFaceRule
    : FontFace ws '{' ws fontFaceDeclaration? ( ';' ws fontFaceDeclaration? )* '}' ws
    ;

fontFaceDeclaration
    : property_ ':' ws expr     # knownFontFaceDeclaration
    | property_ ':' ws value    # unknownFontFaceDeclaration
    ;

// Animations
// https://www.w3.org/TR/css3-animations/
keyframesRule
    : Keyframes ws Space ws ident ws '{' ws keyframeBlock* '}' ws
    ;

keyframeBlock
    : ( keyframeSelector '{' ws declarationList? '}' ws )
    ;

keyframeSelector
    : ( From | To | Percentage ) ws ( Comma ws ( From | To | Percentage ) ws )*
    ;

// Viewport
// https://www.w3.org/TR/css-device-adapt-1/
viewport
    : Viewport ws '{' ws declarationList? '}' ws
    ;

// Counter style
// https://www.w3.org/TR/css-counter-styles-3/
counterStyle
    : CounterStyle ws ident ws '{' ws declarationList? '}' ws
    ;

// Font feature values
// https://www.w3.org/TR/css-fonts-3/
fontFeatureValuesRule
    : FontFeatureValues ws fontFamilyNameList ws '{' ws featureValueBlock* '}' ws
    ;

fontFamilyNameList
    : fontFamilyName ( ws Comma ws fontFamilyName )*
    ;

fontFamilyName
    : String_
    | ident ( ws ident )*
    ;

featureValueBlock
    : featureType ws '{' ws featureValueDefinition? ( ws ';' ws featureValueDefinition? )* '}' ws
    ;

featureType
    : AtKeyword
    ;

featureValueDefinition
    : ident ws ':' ws number ( ws number )*
    ;

// The specific words can be identifiers too
ident
    : Ident
    | MediaOnly
    | Not
    | And
    | Or
    | From
    | To
    ;

// Comments might be part of CSS hacks, thus pass them to visitor to decide whether to skip
// Spaces are significant around '+' '-' '(', thus they should not be skipped
ws
    : ( Comment | Space )*
    ;
