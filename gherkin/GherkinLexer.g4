lexer grammar GherkinLexer;

fragment LANGUAGE_KEYWORD : WS* '#' WS* 'language' WS* ':' WS*;
LANGUAGE_HEADER : LANGUAGE_KEYWORD 'en' LINE_END -> mode(DEFAULT_MODE) ;

AF_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'af' LINE_END -> mode(AF), type(LANGUAGE_HEADER) ;
AM_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'am' LINE_END -> mode(AM), type(LANGUAGE_HEADER) ;
AN_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'an' LINE_END -> mode(AN), type(LANGUAGE_HEADER) ;
AR_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ar' LINE_END -> mode(AR), type(LANGUAGE_HEADER) ;
AST_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ast' LINE_END -> mode(AST), type(LANGUAGE_HEADER) ;
AZ_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'az' LINE_END -> mode(AZ), type(LANGUAGE_HEADER) ;
BG_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'bg' LINE_END -> mode(BG), type(LANGUAGE_HEADER) ;
BM_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'bm' LINE_END -> mode(BM), type(LANGUAGE_HEADER) ;
BS_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'bs' LINE_END -> mode(BS), type(LANGUAGE_HEADER) ;
CA_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ca' LINE_END -> mode(CA), type(LANGUAGE_HEADER) ;
CS_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'cs' LINE_END -> mode(CS), type(LANGUAGE_HEADER) ;
CY_GB_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'cy-GB' LINE_END -> mode(CY_GB), type(LANGUAGE_HEADER) ;
DA_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'da' LINE_END -> mode(DA), type(LANGUAGE_HEADER) ;
DE_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'de' LINE_END -> mode(DE), type(LANGUAGE_HEADER) ;
EL_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'el' LINE_END -> mode(EL), type(LANGUAGE_HEADER) ;
EM_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'em' LINE_END -> mode(EM), type(LANGUAGE_HEADER) ;
EN_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'en' LINE_END -> mode(EN), type(LANGUAGE_HEADER) ;
EN_SCOUSE_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'en-Scouse' LINE_END -> mode(EN_SCOUSE), type(LANGUAGE_HEADER) ;
EN_AU_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'en-au' LINE_END -> mode(EN_AU), type(LANGUAGE_HEADER) ;
EN_LOL_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'en-lol' LINE_END -> mode(EN_LOL), type(LANGUAGE_HEADER) ;
EN_OLD_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'en-old' LINE_END -> mode(EN_OLD), type(LANGUAGE_HEADER) ;
EN_PIRATE_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'en-pirate' LINE_END -> mode(EN_PIRATE), type(LANGUAGE_HEADER) ;
EO_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'eo' LINE_END -> mode(EO), type(LANGUAGE_HEADER) ;
ES_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'es' LINE_END -> mode(ES), type(LANGUAGE_HEADER) ;
ET_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'et' LINE_END -> mode(ET), type(LANGUAGE_HEADER) ;
FA_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'fa' LINE_END -> mode(FA), type(LANGUAGE_HEADER) ;
FI_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'fi' LINE_END -> mode(FI), type(LANGUAGE_HEADER) ;
FR_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'fr' LINE_END -> mode(FR), type(LANGUAGE_HEADER) ;
GA_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ga' LINE_END -> mode(GA), type(LANGUAGE_HEADER) ;
GJ_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'gj' LINE_END -> mode(GJ), type(LANGUAGE_HEADER) ;
GL_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'gl' LINE_END -> mode(GL), type(LANGUAGE_HEADER) ;
HE_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'he' LINE_END -> mode(HE), type(LANGUAGE_HEADER) ;
HI_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'hi' LINE_END -> mode(HI), type(LANGUAGE_HEADER) ;
HR_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'hr' LINE_END -> mode(HR), type(LANGUAGE_HEADER) ;
HT_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ht' LINE_END -> mode(HT), type(LANGUAGE_HEADER) ;
HU_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'hu' LINE_END -> mode(HU), type(LANGUAGE_HEADER) ;
ID_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'id' LINE_END -> mode(ID), type(LANGUAGE_HEADER) ;
IS_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'is' LINE_END -> mode(IS), type(LANGUAGE_HEADER) ;
IT_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'it' LINE_END -> mode(IT), type(LANGUAGE_HEADER) ;
JA_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ja' LINE_END -> mode(JA), type(LANGUAGE_HEADER) ;
JV_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'jv' LINE_END -> mode(JV), type(LANGUAGE_HEADER) ;
KA_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ka' LINE_END -> mode(KA), type(LANGUAGE_HEADER) ;
KN_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'kn' LINE_END -> mode(KN), type(LANGUAGE_HEADER) ;
KO_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ko' LINE_END -> mode(KO), type(LANGUAGE_HEADER) ;
LT_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'lt' LINE_END -> mode(LT), type(LANGUAGE_HEADER) ;
LU_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'lu' LINE_END -> mode(LU), type(LANGUAGE_HEADER) ;
LV_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'lv' LINE_END -> mode(LV), type(LANGUAGE_HEADER) ;
MK_CYRL_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'mk-Cyrl' LINE_END -> mode(MK_CYRL), type(LANGUAGE_HEADER) ;
MK_LATN_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'mk-Latn' LINE_END -> mode(MK_LATN), type(LANGUAGE_HEADER) ;
MN_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'mn' LINE_END -> mode(MN), type(LANGUAGE_HEADER) ;
NE_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ne' LINE_END -> mode(NE), type(LANGUAGE_HEADER) ;
NL_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'nl' LINE_END -> mode(NL), type(LANGUAGE_HEADER) ;
NO_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'no' LINE_END -> mode(NO), type(LANGUAGE_HEADER) ;
PA_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'pa' LINE_END -> mode(PA), type(LANGUAGE_HEADER) ;
PL_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'pl' LINE_END -> mode(PL), type(LANGUAGE_HEADER) ;
PT_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'pt' LINE_END -> mode(PT), type(LANGUAGE_HEADER) ;
RO_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ro' LINE_END -> mode(RO), type(LANGUAGE_HEADER) ;
RU_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ru' LINE_END -> mode(RU), type(LANGUAGE_HEADER) ;
SK_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'sk' LINE_END -> mode(SK), type(LANGUAGE_HEADER) ;
SL_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'sl' LINE_END -> mode(SL), type(LANGUAGE_HEADER) ;
SR_CYRL_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'sr-Cyrl' LINE_END -> mode(SR_CYRL), type(LANGUAGE_HEADER) ;
SR_LATN_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'sr-Latn' LINE_END -> mode(SR_LATN), type(LANGUAGE_HEADER) ;
SV_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'sv' LINE_END -> mode(SV), type(LANGUAGE_HEADER) ;
TA_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ta' LINE_END -> mode(TA), type(LANGUAGE_HEADER) ;
TH_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'th' LINE_END -> mode(TH), type(LANGUAGE_HEADER) ;
TE_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'te' LINE_END -> mode(TE), type(LANGUAGE_HEADER) ;
TLH_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'tlh' LINE_END -> mode(TLH), type(LANGUAGE_HEADER) ;
TR_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'tr' LINE_END -> mode(TR), type(LANGUAGE_HEADER) ;
TT_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'tt' LINE_END -> mode(TT), type(LANGUAGE_HEADER) ;
UK_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'uk' LINE_END -> mode(UK), type(LANGUAGE_HEADER) ;
UR_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'ur' LINE_END -> mode(UR), type(LANGUAGE_HEADER) ;
UZ_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'uz' LINE_END -> mode(UZ), type(LANGUAGE_HEADER) ;
VI_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'vi' LINE_END -> mode(VI), type(LANGUAGE_HEADER) ;
ZH_CN_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'zh-CN' LINE_END -> mode(ZH_CN), type(LANGUAGE_HEADER) ;
ZH_TW_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'zh-TW' LINE_END -> mode(ZH_TW), type(LANGUAGE_HEADER) ;
MR_LANGUAGE_HEADER : LANGUAGE_KEYWORD 'mr' LINE_END -> mode(MR), type(LANGUAGE_HEADER) ;
//////////////////////////////////////////////////////////////////////////
FEATURE_KEYWORD : ('Feature'
	| 'Business Need'
	| 'Ability') ':' -> channel(HIDDEN) ;
SCENARIO_KEYWORD : ('Scenario' | 'Example') ':' -> channel(HIDDEN) ;
SCENARIO_OUTLINE_KEYWORD : 'Scenario Outline:' -> channel(HIDDEN);
BACKGROUND_KEYWORD : 'Background:' ;
EXAMPLES_KEYWORD : 'Examples:' | 'Scenarios:';
RULE_KEYWORD : 'Rule:' ;
STARTING_STEP_KEYWORD : GIVEN_KEYWORD
	| WHEN_KEYWORD
	| THEN_KEYWORD
	| WILD_KEYWORD
	;
ALTERNATIVE_STEP_KEYWORD : AND_KEYWORD
	| BUT_KEYWORD
	| GIVEN_KEYWORD
	;
GIVEN_KEYWORD : 'Given ' ;
WHEN_KEYWORD : 'When ' ;
THEN_KEYWORD : 'Then ' ;
WILD_KEYWORD : '* ' ;
AND_KEYWORD : 'And ';
BUT_KEYWORD : 'But ';
fragment CAPTURE_DATA : '<' ~[>\t\r\n ]'>' ;
fragment DOCSTRING_DOUBLE_QUOTES : WS* '"""' (CAPTURE_DATA | ~'"' | '"' ~'"')*?  '"""' LINE_END ;
fragment DOCSTRING_BACKTICKS : WS* '```' (~'`' | CAPTURE_DATA | '`' ~'`').*? '```' LINE_END;
fragment TAG : '@'~[ \r\n\t@]+ ;
fragment ESCAPE_SEQUENCE : '\\' [\\|\n]* ;
fragment CELL_CHARACTER
	:	CAPTURE_DATA
	| ~[\r\n|\\]
	|	ESCAPE_SEQUENCE
	;
fragment CELL_DATA : WS* CELL_CHARACTER* '|';

DOCSTRING : DOCSTRING_DOUBLE_QUOTES | DOCSTRING_BACKTICKS ;
TAGS : WS* TAG (WS* TAG)* (COMMENT? | LINE_END);
FEATURE_TITLE : WS* FEATURE_KEYWORD ~[\r\n]* LINE_END ;
BACKGROUND_TITLE : WS* BACKGROUND_KEYWORD ~[\r\n]* COMMENT? LINE_END ;
EXAMPLES_TITLE : WS* EXAMPLES_KEYWORD ~[\r\n]* COMMENT? LINE_END ;
SCENARIO_TITLE : WS* SCENARIO_KEYWORD ~[\r\n]* LINE_END ;
SCENARIO_OUTLINE_TITLE : WS* SCENARIO_OUTLINE_KEYWORD (CAPTURE_DATA | ~[\r\n])* LINE_END ;
RULE_TITLE : WS* RULE_KEYWORD ~[\r\n]* LINE_END ;

GIVEN_STEP : WS* GIVEN_KEYWORD ~[ @\r\n|] ~[\r\n]* LINE_END;
WHEN_STEP : WS* WHEN_KEYWORD ~[ @\r\n|] ~[\r\n]* LINE_END;
THEN_STEP : WS* THEN_KEYWORD ~[ @\r\n|] ~[\r\n]* LINE_END;
AND_STEP : WS* AND_KEYWORD ~[ @\r\n|] ~[\r\n]* LINE_END;
BUT_STEP : WS* BUT_KEYWORD ~[ @\r\n|] ~[\r\n]* LINE_END;
WILD_STEP : WS* WILD_KEYWORD ~[ @\r\n|] ~[\r\n]* LINE_END;

DATA_ROW : WS* '|' CELL_DATA+ LINE_END ;
INVALID_LANGUAGE_HEADER : LANGUAGE_KEYWORD ~[\r\n]* LINE_END ;
COMMENT : WS* '#' ~[\r\n]* LINE_END -> channel(HIDDEN) ;
LINE_END : WS* (NEWLINE+ | EOF) -> skip;
NEWLINE : [\r\n] -> skip ;
WS : [ \t] -> skip;
LONG_DESCRIPTION : WS* ~[ @\r\n|] ~[\r\n]* LINE_END ;
///////////////////////////////////////////////////

//Afrikaans
//Afrikaans
mode AF;
	AF_FEATURE : ( 
	('Funksie'
		| 'Besigheid Behoefte'
		| 'Vermoë'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    AF_BACKGROUND : (
    
	('Agtergrond'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	AF_SCENARIO : (

	('Voorbeeld'
		| 'Situasie'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	AF_SCENARIO_OUTLINE : (

	('Situasie Uiteensetting'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	AF_EXAMPLES : (

	('Voorbeelde'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	AF_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	AF_GIVEN : (

	('Gegewe '
	)   ) -> type(GIVEN_KEYWORD) ;

	AF_WHEN : (

	('Wanneer '
	)   ) -> type(WHEN_KEYWORD) ;

	AF_THEN : (

	('Dan '
	)   ) -> type(THEN_KEYWORD) ;

	AF_AND : (

	('En '
	)   ) -> type(AND_KEYWORD) ;

	AF_BUT : (

	('Maar '
	)   ) -> type(BUT_KEYWORD) ;

	AF_STARTING_STEP_KEYWORD : (

                AF_GIVEN
		| AF_WHEN
		| AF_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	AF_ALTERNATIVE_STEP_KEYWORD : (

                AF_AND
		| AF_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    AF_FEATURE_TITLE : WS* AF_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    AF_BACKGROUND_TITLE : WS* AF_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    AF_EXAMPLES_TITLE : WS* AF_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    AF_SCENARIO_TITLE : WS* AF_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    AF_SCENARIO_OUTLINE_TITLE : WS* AF_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    AF_RULE_TITLE : WS* AF_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        AF_GIVEN_STEP : WS* AF_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	AF_WHEN_STEP : WS* AF_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	AF_THEN_STEP : WS* AF_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	AF_AND_STEP : WS* AF_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	AF_BUT_STEP : WS* AF_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Armenian
//հայերեն
mode AM;
	AM_FEATURE : ( 
	('Ֆունկցիոնալություն'
		| 'Հատկություն'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    AM_BACKGROUND : (
    
	('Կոնտեքստ'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	AM_SCENARIO : (

	('Օրինակ'
		| 'Սցենար'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	AM_SCENARIO_OUTLINE : (

	('Սցենարի կառուցվացքը'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	AM_EXAMPLES : (

	('Օրինակներ'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	AM_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	AM_GIVEN : (

	('Դիցուք '
	)   ) -> type(GIVEN_KEYWORD) ;

	AM_WHEN : (

	('Եթե '
		| 'Երբ '
	)   ) -> type(WHEN_KEYWORD) ;

	AM_THEN : (

	('Ապա '
	)   ) -> type(THEN_KEYWORD) ;

	AM_AND : (

	('Եվ '
	)   ) -> type(AND_KEYWORD) ;

	AM_BUT : (

	('Բայց '
	)   ) -> type(BUT_KEYWORD) ;

	AM_STARTING_STEP_KEYWORD : (

                AM_GIVEN
		| AM_WHEN
		| AM_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	AM_ALTERNATIVE_STEP_KEYWORD : (

                AM_AND
		| AM_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    AM_FEATURE_TITLE : WS* AM_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    AM_BACKGROUND_TITLE : WS* AM_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    AM_EXAMPLES_TITLE : WS* AM_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    AM_SCENARIO_TITLE : WS* AM_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    AM_SCENARIO_OUTLINE_TITLE : WS* AM_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    AM_RULE_TITLE : WS* AM_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        AM_GIVEN_STEP : WS* AM_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	AM_WHEN_STEP : WS* AM_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	AM_THEN_STEP : WS* AM_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	AM_AND_STEP : WS* AM_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	AM_BUT_STEP : WS* AM_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Aragonese
//Aragonés
mode AN;
	AN_FEATURE : ( 
	('Caracteristica'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    AN_BACKGROUND : (
    
	('Antecedents'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	AN_SCENARIO : (

	('Eixemplo'
		| 'Caso'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	AN_SCENARIO_OUTLINE : (

	('Esquema del caso'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	AN_EXAMPLES : (

	('Eixemplos'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	AN_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	AN_GIVEN : (

	('Dau '
		| 'Dada '
		| 'Daus '
		| 'Dadas '
	)   ) -> type(GIVEN_KEYWORD) ;

	AN_WHEN : (

	('Cuan '
	)   ) -> type(WHEN_KEYWORD) ;

	AN_THEN : (

	('Alavez '
		| 'Allora '
		| 'Antonces '
	)   ) -> type(THEN_KEYWORD) ;

	AN_AND : (

	('Y '
		| 'E '
	)   ) -> type(AND_KEYWORD) ;

	AN_BUT : (

	('Pero '
	)   ) -> type(BUT_KEYWORD) ;

	AN_STARTING_STEP_KEYWORD : (

                AN_GIVEN
		| AN_WHEN
		| AN_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	AN_ALTERNATIVE_STEP_KEYWORD : (

                AN_AND
		| AN_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    AN_FEATURE_TITLE : WS* AN_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    AN_BACKGROUND_TITLE : WS* AN_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    AN_EXAMPLES_TITLE : WS* AN_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    AN_SCENARIO_TITLE : WS* AN_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    AN_SCENARIO_OUTLINE_TITLE : WS* AN_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    AN_RULE_TITLE : WS* AN_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        AN_GIVEN_STEP : WS* AN_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	AN_WHEN_STEP : WS* AN_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	AN_THEN_STEP : WS* AN_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	AN_AND_STEP : WS* AN_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	AN_BUT_STEP : WS* AN_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Arabic
//العربية
mode AR;
	AR_FEATURE : ( 
	('خاصية'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    AR_BACKGROUND : (
    
	('الخلفية'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	AR_SCENARIO : (

	('مثال'
		| 'سيناريو'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	AR_SCENARIO_OUTLINE : (

	('سيناريو مخطط'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	AR_EXAMPLES : (

	('امثلة'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	AR_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	AR_GIVEN : (

	('بفرض '
	)   ) -> type(GIVEN_KEYWORD) ;

	AR_WHEN : (

	('متى '
		| 'عندما '
	)   ) -> type(WHEN_KEYWORD) ;

	AR_THEN : (

	('اذاً '
		| 'ثم '
	)   ) -> type(THEN_KEYWORD) ;

	AR_AND : (

	('و '
	)   ) -> type(AND_KEYWORD) ;

	AR_BUT : (

	('لكن '
	)   ) -> type(BUT_KEYWORD) ;

	AR_STARTING_STEP_KEYWORD : (

                AR_GIVEN
		| AR_WHEN
		| AR_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	AR_ALTERNATIVE_STEP_KEYWORD : (

                AR_AND
		| AR_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    AR_FEATURE_TITLE : WS* AR_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    AR_BACKGROUND_TITLE : WS* AR_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    AR_EXAMPLES_TITLE : WS* AR_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    AR_SCENARIO_TITLE : WS* AR_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    AR_SCENARIO_OUTLINE_TITLE : WS* AR_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    AR_RULE_TITLE : WS* AR_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        AR_GIVEN_STEP : WS* AR_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	AR_WHEN_STEP : WS* AR_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	AR_THEN_STEP : WS* AR_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	AR_AND_STEP : WS* AR_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	AR_BUT_STEP : WS* AR_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Asturian
//asturianu
mode AST;
	AST_FEATURE : ( 
	('Carauterística'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    AST_BACKGROUND : (
    
	('Antecedentes'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	AST_SCENARIO : (

	('Exemplo'
		| 'Casu'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	AST_SCENARIO_OUTLINE : (

	('Esbozu del casu'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	AST_EXAMPLES : (

	('Exemplos'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	AST_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	AST_GIVEN : (

	('Dáu '
		| 'Dada '
		| 'Daos '
		| 'Daes '
	)   ) -> type(GIVEN_KEYWORD) ;

	AST_WHEN : (

	('Cuando '
	)   ) -> type(WHEN_KEYWORD) ;

	AST_THEN : (

	('Entós '
	)   ) -> type(THEN_KEYWORD) ;

	AST_AND : (

	('Y '
		| 'Ya '
	)   ) -> type(AND_KEYWORD) ;

	AST_BUT : (

	('Peru '
	)   ) -> type(BUT_KEYWORD) ;

	AST_STARTING_STEP_KEYWORD : (

                AST_GIVEN
		| AST_WHEN
		| AST_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	AST_ALTERNATIVE_STEP_KEYWORD : (

                AST_AND
		| AST_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    AST_FEATURE_TITLE : WS* AST_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    AST_BACKGROUND_TITLE : WS* AST_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    AST_EXAMPLES_TITLE : WS* AST_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    AST_SCENARIO_TITLE : WS* AST_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    AST_SCENARIO_OUTLINE_TITLE : WS* AST_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    AST_RULE_TITLE : WS* AST_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        AST_GIVEN_STEP : WS* AST_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	AST_WHEN_STEP : WS* AST_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	AST_THEN_STEP : WS* AST_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	AST_AND_STEP : WS* AST_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	AST_BUT_STEP : WS* AST_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Azerbaijani
//Azərbaycanca
mode AZ;
	AZ_FEATURE : ( 
	('Özəllik'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    AZ_BACKGROUND : (
    
	('Keçmiş'
		| 'Kontekst'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	AZ_SCENARIO : (

	('Nümunə'
		| 'Ssenari'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	AZ_SCENARIO_OUTLINE : (

	('Ssenarinin strukturu'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	AZ_EXAMPLES : (

	('Nümunələr'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	AZ_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	AZ_GIVEN : (

	('Tutaq ki '
		| 'Verilir '
	)   ) -> type(GIVEN_KEYWORD) ;

	AZ_WHEN : (

	('Əgər '
		| 'Nə vaxt ki '
	)   ) -> type(WHEN_KEYWORD) ;

	AZ_THEN : (

	('O halda '
	)   ) -> type(THEN_KEYWORD) ;

	AZ_AND : (

	('Və '
		| 'Həm '
	)   ) -> type(AND_KEYWORD) ;

	AZ_BUT : (

	('Amma '
		| 'Ancaq '
	)   ) -> type(BUT_KEYWORD) ;

	AZ_STARTING_STEP_KEYWORD : (

                AZ_GIVEN
		| AZ_WHEN
		| AZ_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	AZ_ALTERNATIVE_STEP_KEYWORD : (

                AZ_AND
		| AZ_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    AZ_FEATURE_TITLE : WS* AZ_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    AZ_BACKGROUND_TITLE : WS* AZ_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    AZ_EXAMPLES_TITLE : WS* AZ_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    AZ_SCENARIO_TITLE : WS* AZ_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    AZ_SCENARIO_OUTLINE_TITLE : WS* AZ_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    AZ_RULE_TITLE : WS* AZ_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        AZ_GIVEN_STEP : WS* AZ_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	AZ_WHEN_STEP : WS* AZ_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	AZ_THEN_STEP : WS* AZ_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	AZ_AND_STEP : WS* AZ_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	AZ_BUT_STEP : WS* AZ_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Bulgarian
//български
mode BG;
	BG_FEATURE : ( 
	('Функционалност'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    BG_BACKGROUND : (
    
	('Предистория'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	BG_SCENARIO : (

	('Пример'
		| 'Сценарий'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	BG_SCENARIO_OUTLINE : (

	('Рамка на сценарий'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	BG_EXAMPLES : (

	('Примери'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	BG_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	BG_GIVEN : (

	('Дадено '
	)   ) -> type(GIVEN_KEYWORD) ;

	BG_WHEN : (

	('Когато '
	)   ) -> type(WHEN_KEYWORD) ;

	BG_THEN : (

	('То '
	)   ) -> type(THEN_KEYWORD) ;

	BG_AND : (

	('И '
	)   ) -> type(AND_KEYWORD) ;

	BG_BUT : (

	('Но '
	)   ) -> type(BUT_KEYWORD) ;

	BG_STARTING_STEP_KEYWORD : (

                BG_GIVEN
		| BG_WHEN
		| BG_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	BG_ALTERNATIVE_STEP_KEYWORD : (

                BG_AND
		| BG_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    BG_FEATURE_TITLE : WS* BG_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    BG_BACKGROUND_TITLE : WS* BG_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    BG_EXAMPLES_TITLE : WS* BG_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    BG_SCENARIO_TITLE : WS* BG_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    BG_SCENARIO_OUTLINE_TITLE : WS* BG_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    BG_RULE_TITLE : WS* BG_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        BG_GIVEN_STEP : WS* BG_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	BG_WHEN_STEP : WS* BG_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	BG_THEN_STEP : WS* BG_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	BG_AND_STEP : WS* BG_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	BG_BUT_STEP : WS* BG_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Malay
//Bahasa Melayu
mode BM;
	BM_FEATURE : ( 
	('Fungsi'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    BM_BACKGROUND : (
    
	('Latar Belakang'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	BM_SCENARIO : (

	('Senario'
		| 'Situasi'
		| 'Keadaan'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	BM_SCENARIO_OUTLINE : (

	('Kerangka Senario'
		| 'Kerangka Situasi'
		| 'Kerangka Keadaan'
		| 'Garis Panduan Senario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	BM_EXAMPLES : (

	('Contoh'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	BM_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	BM_GIVEN : (

	('Diberi '
		| 'Bagi '
	)   ) -> type(GIVEN_KEYWORD) ;

	BM_WHEN : (

	('Apabila '
	)   ) -> type(WHEN_KEYWORD) ;

	BM_THEN : (

	('Maka '
		| 'Kemudian '
	)   ) -> type(THEN_KEYWORD) ;

	BM_AND : (

	('Dan '
	)   ) -> type(AND_KEYWORD) ;

	BM_BUT : (

	('Tetapi '
		| 'Tapi '
	)   ) -> type(BUT_KEYWORD) ;

	BM_STARTING_STEP_KEYWORD : (

                BM_GIVEN
		| BM_WHEN
		| BM_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	BM_ALTERNATIVE_STEP_KEYWORD : (

                BM_AND
		| BM_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    BM_FEATURE_TITLE : WS* BM_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    BM_BACKGROUND_TITLE : WS* BM_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    BM_EXAMPLES_TITLE : WS* BM_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    BM_SCENARIO_TITLE : WS* BM_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    BM_SCENARIO_OUTLINE_TITLE : WS* BM_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    BM_RULE_TITLE : WS* BM_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        BM_GIVEN_STEP : WS* BM_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	BM_WHEN_STEP : WS* BM_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	BM_THEN_STEP : WS* BM_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	BM_AND_STEP : WS* BM_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	BM_BUT_STEP : WS* BM_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Bosnian
//Bosanski
mode BS;
	BS_FEATURE : ( 
	('Karakteristika'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    BS_BACKGROUND : (
    
	('Pozadina'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	BS_SCENARIO : (

	('Primjer'
		| 'Scenariju'
		| 'Scenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	BS_SCENARIO_OUTLINE : (

	('Scenariju-obris'
		| 'Scenario-outline'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	BS_EXAMPLES : (

	('Primjeri'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	BS_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	BS_GIVEN : (

	('Dato '
	)   ) -> type(GIVEN_KEYWORD) ;

	BS_WHEN : (

	('Kada '
	)   ) -> type(WHEN_KEYWORD) ;

	BS_THEN : (

	('Zatim '
	)   ) -> type(THEN_KEYWORD) ;

	BS_AND : (

	('I '
		| 'A '
	)   ) -> type(AND_KEYWORD) ;

	BS_BUT : (

	('Ali '
	)   ) -> type(BUT_KEYWORD) ;

	BS_STARTING_STEP_KEYWORD : (

                BS_GIVEN
		| BS_WHEN
		| BS_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	BS_ALTERNATIVE_STEP_KEYWORD : (

                BS_AND
		| BS_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    BS_FEATURE_TITLE : WS* BS_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    BS_BACKGROUND_TITLE : WS* BS_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    BS_EXAMPLES_TITLE : WS* BS_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    BS_SCENARIO_TITLE : WS* BS_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    BS_SCENARIO_OUTLINE_TITLE : WS* BS_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    BS_RULE_TITLE : WS* BS_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        BS_GIVEN_STEP : WS* BS_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	BS_WHEN_STEP : WS* BS_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	BS_THEN_STEP : WS* BS_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	BS_AND_STEP : WS* BS_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	BS_BUT_STEP : WS* BS_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Catalan
//català
mode CA;
	CA_FEATURE : ( 
	('Característica'
		| 'Funcionalitat'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    CA_BACKGROUND : (
    
	('Rerefons'
		| 'Antecedents'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	CA_SCENARIO : (

	('Exemple'
		| 'Escenari'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	CA_SCENARIO_OUTLINE : (

	('Esquema de l\'escenari'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	CA_EXAMPLES : (

	('Exemples'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	CA_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	CA_GIVEN : (

	('Donat '
		| 'Donada '
		| 'Atès '
		| 'Atesa '
	)   ) -> type(GIVEN_KEYWORD) ;

	CA_WHEN : (

	('Quan '
	)   ) -> type(WHEN_KEYWORD) ;

	CA_THEN : (

	('Aleshores '
		| 'Cal '
	)   ) -> type(THEN_KEYWORD) ;

	CA_AND : (

	('I '
	)   ) -> type(AND_KEYWORD) ;

	CA_BUT : (

	('Però '
	)   ) -> type(BUT_KEYWORD) ;

	CA_STARTING_STEP_KEYWORD : (

                CA_GIVEN
		| CA_WHEN
		| CA_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	CA_ALTERNATIVE_STEP_KEYWORD : (

                CA_AND
		| CA_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    CA_FEATURE_TITLE : WS* CA_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    CA_BACKGROUND_TITLE : WS* CA_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    CA_EXAMPLES_TITLE : WS* CA_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    CA_SCENARIO_TITLE : WS* CA_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    CA_SCENARIO_OUTLINE_TITLE : WS* CA_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    CA_RULE_TITLE : WS* CA_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        CA_GIVEN_STEP : WS* CA_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	CA_WHEN_STEP : WS* CA_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	CA_THEN_STEP : WS* CA_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	CA_AND_STEP : WS* CA_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	CA_BUT_STEP : WS* CA_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Czech
//Česky
mode CS;
	CS_FEATURE : ( 
	('Požadavek'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    CS_BACKGROUND : (
    
	('Pozadí'
		| 'Kontext'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	CS_SCENARIO : (

	('Příklad'
		| 'Scénář'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	CS_SCENARIO_OUTLINE : (

	('Náčrt Scénáře'
		| 'Osnova scénáře'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	CS_EXAMPLES : (

	('Příklady'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	CS_RULE : (

	('Pravidlo'
	) ':'  ) -> type(RULE_KEYWORD) ;

	CS_GIVEN : (

	('Pokud '
		| 'Za předpokladu '
	)   ) -> type(GIVEN_KEYWORD) ;

	CS_WHEN : (

	('Když '
	)   ) -> type(WHEN_KEYWORD) ;

	CS_THEN : (

	('Pak '
	)   ) -> type(THEN_KEYWORD) ;

	CS_AND : (

	('A také '
		| 'A '
	)   ) -> type(AND_KEYWORD) ;

	CS_BUT : (

	('Ale '
	)   ) -> type(BUT_KEYWORD) ;

	CS_STARTING_STEP_KEYWORD : (

                CS_GIVEN
		| CS_WHEN
		| CS_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	CS_ALTERNATIVE_STEP_KEYWORD : (

                CS_AND
		| CS_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    CS_FEATURE_TITLE : WS* CS_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    CS_BACKGROUND_TITLE : WS* CS_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    CS_EXAMPLES_TITLE : WS* CS_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    CS_SCENARIO_TITLE : WS* CS_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    CS_SCENARIO_OUTLINE_TITLE : WS* CS_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    CS_RULE_TITLE : WS* CS_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        CS_GIVEN_STEP : WS* CS_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	CS_WHEN_STEP : WS* CS_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	CS_THEN_STEP : WS* CS_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	CS_AND_STEP : WS* CS_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	CS_BUT_STEP : WS* CS_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Welsh
//Cymraeg
mode CY_GB;
	CY_GB_FEATURE : ( 
	('Arwedd'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    CY_GB_BACKGROUND : (
    
	('Cefndir'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	CY_GB_SCENARIO : (

	('Enghraifft'
		| 'Scenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	CY_GB_SCENARIO_OUTLINE : (

	('Scenario Amlinellol'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	CY_GB_EXAMPLES : (

	('Enghreifftiau'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	CY_GB_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	CY_GB_GIVEN : (

	('Anrhegedig a '
	)   ) -> type(GIVEN_KEYWORD) ;

	CY_GB_WHEN : (

	('Pryd '
	)   ) -> type(WHEN_KEYWORD) ;

	CY_GB_THEN : (

	('Yna '
	)   ) -> type(THEN_KEYWORD) ;

	CY_GB_AND : (

	('A '
	)   ) -> type(AND_KEYWORD) ;

	CY_GB_BUT : (

	('Ond '
	)   ) -> type(BUT_KEYWORD) ;

	CY_GB_STARTING_STEP_KEYWORD : (

                CY_GB_GIVEN
		| CY_GB_WHEN
		| CY_GB_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	CY_GB_ALTERNATIVE_STEP_KEYWORD : (

                CY_GB_AND
		| CY_GB_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    CY_GB_FEATURE_TITLE : WS* CY_GB_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    CY_GB_BACKGROUND_TITLE : WS* CY_GB_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    CY_GB_EXAMPLES_TITLE : WS* CY_GB_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    CY_GB_SCENARIO_TITLE : WS* CY_GB_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    CY_GB_SCENARIO_OUTLINE_TITLE : WS* CY_GB_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    CY_GB_RULE_TITLE : WS* CY_GB_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        CY_GB_GIVEN_STEP : WS* CY_GB_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	CY_GB_WHEN_STEP : WS* CY_GB_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	CY_GB_THEN_STEP : WS* CY_GB_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	CY_GB_AND_STEP : WS* CY_GB_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	CY_GB_BUT_STEP : WS* CY_GB_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Danish
//dansk
mode DA;
	DA_FEATURE : ( 
	('Egenskab'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    DA_BACKGROUND : (
    
	('Baggrund'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	DA_SCENARIO : (

	('Eksempel'
		| 'Scenarie'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	DA_SCENARIO_OUTLINE : (

	('Abstrakt Scenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	DA_EXAMPLES : (

	('Eksempler'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	DA_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	DA_GIVEN : (

	('Givet '
	)   ) -> type(GIVEN_KEYWORD) ;

	DA_WHEN : (

	('Når '
	)   ) -> type(WHEN_KEYWORD) ;

	DA_THEN : (

	('Så '
	)   ) -> type(THEN_KEYWORD) ;

	DA_AND : (

	('Og '
	)   ) -> type(AND_KEYWORD) ;

	DA_BUT : (

	('Men '
	)   ) -> type(BUT_KEYWORD) ;

	DA_STARTING_STEP_KEYWORD : (

                DA_GIVEN
		| DA_WHEN
		| DA_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	DA_ALTERNATIVE_STEP_KEYWORD : (

                DA_AND
		| DA_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    DA_FEATURE_TITLE : WS* DA_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    DA_BACKGROUND_TITLE : WS* DA_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    DA_EXAMPLES_TITLE : WS* DA_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    DA_SCENARIO_TITLE : WS* DA_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    DA_SCENARIO_OUTLINE_TITLE : WS* DA_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    DA_RULE_TITLE : WS* DA_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        DA_GIVEN_STEP : WS* DA_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	DA_WHEN_STEP : WS* DA_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	DA_THEN_STEP : WS* DA_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	DA_AND_STEP : WS* DA_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	DA_BUT_STEP : WS* DA_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//German
//Deutsch
mode DE;
	DE_FEATURE : ( 
	('Funktionalität'
		| 'Funktion'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    DE_BACKGROUND : (
    
	('Grundlage'
		| 'Hintergrund'
		| 'Voraussetzungen'
		| 'Vorbedingungen'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	DE_SCENARIO : (

	('Beispiel'
		| 'Szenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	DE_SCENARIO_OUTLINE : (

	('Szenariogrundriss'
		| 'Szenarien'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	DE_EXAMPLES : (

	('Beispiele'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	DE_RULE : (

	('Rule'
		| 'Regel'
	) ':'  ) -> type(RULE_KEYWORD) ;

	DE_GIVEN : (

	('Angenommen '
		| 'Gegeben sei '
		| 'Gegeben seien '
	)   ) -> type(GIVEN_KEYWORD) ;

	DE_WHEN : (

	('Wenn '
	)   ) -> type(WHEN_KEYWORD) ;

	DE_THEN : (

	('Dann '
	)   ) -> type(THEN_KEYWORD) ;

	DE_AND : (

	('Und '
	)   ) -> type(AND_KEYWORD) ;

	DE_BUT : (

	('Aber '
	)   ) -> type(BUT_KEYWORD) ;

	DE_STARTING_STEP_KEYWORD : (

                DE_GIVEN
		| DE_WHEN
		| DE_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	DE_ALTERNATIVE_STEP_KEYWORD : (

                DE_AND
		| DE_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    DE_FEATURE_TITLE : WS* DE_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    DE_BACKGROUND_TITLE : WS* DE_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    DE_EXAMPLES_TITLE : WS* DE_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    DE_SCENARIO_TITLE : WS* DE_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    DE_SCENARIO_OUTLINE_TITLE : WS* DE_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    DE_RULE_TITLE : WS* DE_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        DE_GIVEN_STEP : WS* DE_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	DE_WHEN_STEP : WS* DE_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	DE_THEN_STEP : WS* DE_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	DE_AND_STEP : WS* DE_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	DE_BUT_STEP : WS* DE_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Greek
//Ελληνικά
mode EL;
	EL_FEATURE : ( 
	('Δυνατότητα'
		| 'Λειτουργία'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    EL_BACKGROUND : (
    
	('Υπόβαθρο'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	EL_SCENARIO : (

	('Παράδειγμα'
		| 'Σενάριο'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	EL_SCENARIO_OUTLINE : (

	('Περιγραφή Σεναρίου'
		| 'Περίγραμμα Σεναρίου'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	EL_EXAMPLES : (

	('Παραδείγματα'
		| 'Σενάρια'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	EL_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	EL_GIVEN : (

	('Δεδομένου '
	)   ) -> type(GIVEN_KEYWORD) ;

	EL_WHEN : (

	('Όταν '
	)   ) -> type(WHEN_KEYWORD) ;

	EL_THEN : (

	('Τότε '
	)   ) -> type(THEN_KEYWORD) ;

	EL_AND : (

	('Και '
	)   ) -> type(AND_KEYWORD) ;

	EL_BUT : (

	('Αλλά '
	)   ) -> type(BUT_KEYWORD) ;

	EL_STARTING_STEP_KEYWORD : (

                EL_GIVEN
		| EL_WHEN
		| EL_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	EL_ALTERNATIVE_STEP_KEYWORD : (

                EL_AND
		| EL_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    EL_FEATURE_TITLE : WS* EL_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    EL_BACKGROUND_TITLE : WS* EL_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    EL_EXAMPLES_TITLE : WS* EL_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    EL_SCENARIO_TITLE : WS* EL_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    EL_SCENARIO_OUTLINE_TITLE : WS* EL_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    EL_RULE_TITLE : WS* EL_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        EL_GIVEN_STEP : WS* EL_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	EL_WHEN_STEP : WS* EL_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	EL_THEN_STEP : WS* EL_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	EL_AND_STEP : WS* EL_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	EL_BUT_STEP : WS* EL_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Emoji
//😀
mode EM;
	EM_FEATURE : ( 
	('📚'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    EM_BACKGROUND : (
    
	('💤'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	EM_SCENARIO : (

	('🥒'
		| '📕'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	EM_SCENARIO_OUTLINE : (

	('📖'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	EM_EXAMPLES : (

	('📓'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	EM_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	EM_GIVEN : (

	('😐'
	)   ) -> type(GIVEN_KEYWORD) ;

	EM_WHEN : (

	('🎬'
	)   ) -> type(WHEN_KEYWORD) ;

	EM_THEN : (

	('🙏'
	)   ) -> type(THEN_KEYWORD) ;

	EM_AND : (

	('😂'
	)   ) -> type(AND_KEYWORD) ;

	EM_BUT : (

	('😔'
	)   ) -> type(BUT_KEYWORD) ;

	EM_STARTING_STEP_KEYWORD : (

                EM_GIVEN
		| EM_WHEN
		| EM_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	EM_ALTERNATIVE_STEP_KEYWORD : (

                EM_AND
		| EM_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    EM_FEATURE_TITLE : WS* EM_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    EM_BACKGROUND_TITLE : WS* EM_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    EM_EXAMPLES_TITLE : WS* EM_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    EM_SCENARIO_TITLE : WS* EM_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    EM_SCENARIO_OUTLINE_TITLE : WS* EM_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    EM_RULE_TITLE : WS* EM_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        EM_GIVEN_STEP : WS* EM_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	EM_WHEN_STEP : WS* EM_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	EM_THEN_STEP : WS* EM_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	EM_AND_STEP : WS* EM_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	EM_BUT_STEP : WS* EM_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//English
//English
mode EN;
	EN_FEATURE : ( 
	('Feature'
		| 'Business Need'
		| 'Ability'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    EN_BACKGROUND : (
    
	('Background'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	EN_SCENARIO : (

	('Example'
		| 'Scenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	EN_SCENARIO_OUTLINE : (

	('Scenario Outline'
		| 'Scenario Template'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	EN_EXAMPLES : (

	('Examples'
		| 'Scenarios'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	EN_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	EN_GIVEN : (

	('Given '
	)   ) -> type(GIVEN_KEYWORD) ;

	EN_WHEN : (

	('When '
	)   ) -> type(WHEN_KEYWORD) ;

	EN_THEN : (

	('Then '
	)   ) -> type(THEN_KEYWORD) ;

	EN_AND : (

	('And '
	)   ) -> type(AND_KEYWORD) ;

	EN_BUT : (

	('But '
	)   ) -> type(BUT_KEYWORD) ;

	EN_STARTING_STEP_KEYWORD : (

                EN_GIVEN
		| EN_WHEN
		| EN_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	EN_ALTERNATIVE_STEP_KEYWORD : (

                EN_AND
		| EN_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    EN_FEATURE_TITLE : WS* EN_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    EN_BACKGROUND_TITLE : WS* EN_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    EN_EXAMPLES_TITLE : WS* EN_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    EN_SCENARIO_TITLE : WS* EN_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    EN_SCENARIO_OUTLINE_TITLE : WS* EN_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    EN_RULE_TITLE : WS* EN_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        EN_GIVEN_STEP : WS* EN_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	EN_WHEN_STEP : WS* EN_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	EN_THEN_STEP : WS* EN_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	EN_AND_STEP : WS* EN_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	EN_BUT_STEP : WS* EN_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Scouse
//Scouse
mode EN_SCOUSE;
	EN_SCOUSE_FEATURE : ( 
	('Feature'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    EN_SCOUSE_BACKGROUND : (
    
	('Dis is what went down'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	EN_SCOUSE_SCENARIO : (

	('The thing of it is'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	EN_SCOUSE_SCENARIO_OUTLINE : (

	('Wharrimean is'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	EN_SCOUSE_EXAMPLES : (

	('Examples'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	EN_SCOUSE_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	EN_SCOUSE_GIVEN : (

	('Givun '
		| 'Youse know when youse got '
	)   ) -> type(GIVEN_KEYWORD) ;

	EN_SCOUSE_WHEN : (

	('Wun '
		| 'Youse know like when '
	)   ) -> type(WHEN_KEYWORD) ;

	EN_SCOUSE_THEN : (

	('Dun '
		| 'Den youse gotta '
	)   ) -> type(THEN_KEYWORD) ;

	EN_SCOUSE_AND : (

	('An '
	)   ) -> type(AND_KEYWORD) ;

	EN_SCOUSE_BUT : (

	('Buh '
	)   ) -> type(BUT_KEYWORD) ;

	EN_SCOUSE_STARTING_STEP_KEYWORD : (

                EN_SCOUSE_GIVEN
		| EN_SCOUSE_WHEN
		| EN_SCOUSE_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	EN_SCOUSE_ALTERNATIVE_STEP_KEYWORD : (

                EN_SCOUSE_AND
		| EN_SCOUSE_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    EN_SCOUSE_FEATURE_TITLE : WS* EN_SCOUSE_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    EN_SCOUSE_BACKGROUND_TITLE : WS* EN_SCOUSE_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    EN_SCOUSE_EXAMPLES_TITLE : WS* EN_SCOUSE_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    EN_SCOUSE_SCENARIO_TITLE : WS* EN_SCOUSE_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    EN_SCOUSE_SCENARIO_OUTLINE_TITLE : WS* EN_SCOUSE_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    EN_SCOUSE_RULE_TITLE : WS* EN_SCOUSE_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        EN_SCOUSE_GIVEN_STEP : WS* EN_SCOUSE_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	EN_SCOUSE_WHEN_STEP : WS* EN_SCOUSE_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	EN_SCOUSE_THEN_STEP : WS* EN_SCOUSE_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	EN_SCOUSE_AND_STEP : WS* EN_SCOUSE_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	EN_SCOUSE_BUT_STEP : WS* EN_SCOUSE_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Australian
//Australian
mode EN_AU;
	EN_AU_FEATURE : ( 
	('Pretty much'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    EN_AU_BACKGROUND : (
    
	('First off'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	EN_AU_SCENARIO : (

	('Awww, look mate'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	EN_AU_SCENARIO_OUTLINE : (

	('Reckon it\'s like'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	EN_AU_EXAMPLES : (

	('You\'ll wanna'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	EN_AU_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	EN_AU_GIVEN : (

	('Y\'know '
	)   ) -> type(GIVEN_KEYWORD) ;

	EN_AU_WHEN : (

	('It\'s just unbelievable '
	)   ) -> type(WHEN_KEYWORD) ;

	EN_AU_THEN : (

	('But at the end of the day I reckon '
	)   ) -> type(THEN_KEYWORD) ;

	EN_AU_AND : (

	('Too right '
	)   ) -> type(AND_KEYWORD) ;

	EN_AU_BUT : (

	('Yeah nah '
	)   ) -> type(BUT_KEYWORD) ;

	EN_AU_STARTING_STEP_KEYWORD : (

                EN_AU_GIVEN
		| EN_AU_WHEN
		| EN_AU_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	EN_AU_ALTERNATIVE_STEP_KEYWORD : (

                EN_AU_AND
		| EN_AU_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    EN_AU_FEATURE_TITLE : WS* EN_AU_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    EN_AU_BACKGROUND_TITLE : WS* EN_AU_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    EN_AU_EXAMPLES_TITLE : WS* EN_AU_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    EN_AU_SCENARIO_TITLE : WS* EN_AU_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    EN_AU_SCENARIO_OUTLINE_TITLE : WS* EN_AU_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    EN_AU_RULE_TITLE : WS* EN_AU_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        EN_AU_GIVEN_STEP : WS* EN_AU_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	EN_AU_WHEN_STEP : WS* EN_AU_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	EN_AU_THEN_STEP : WS* EN_AU_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	EN_AU_AND_STEP : WS* EN_AU_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	EN_AU_BUT_STEP : WS* EN_AU_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//LOLCAT
//LOLCAT
mode EN_LOL;
	EN_LOL_FEATURE : ( 
	('OH HAI'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    EN_LOL_BACKGROUND : (
    
	('B4'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	EN_LOL_SCENARIO : (

	('MISHUN'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	EN_LOL_SCENARIO_OUTLINE : (

	('MISHUN SRSLY'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	EN_LOL_EXAMPLES : (

	('EXAMPLZ'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	EN_LOL_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	EN_LOL_GIVEN : (

	('I CAN HAZ '
	)   ) -> type(GIVEN_KEYWORD) ;

	EN_LOL_WHEN : (

	('WEN '
	)   ) -> type(WHEN_KEYWORD) ;

	EN_LOL_THEN : (

	('DEN '
	)   ) -> type(THEN_KEYWORD) ;

	EN_LOL_AND : (

	('AN '
	)   ) -> type(AND_KEYWORD) ;

	EN_LOL_BUT : (

	('BUT '
	)   ) -> type(BUT_KEYWORD) ;

	EN_LOL_STARTING_STEP_KEYWORD : (

                EN_LOL_GIVEN
		| EN_LOL_WHEN
		| EN_LOL_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	EN_LOL_ALTERNATIVE_STEP_KEYWORD : (

                EN_LOL_AND
		| EN_LOL_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    EN_LOL_FEATURE_TITLE : WS* EN_LOL_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    EN_LOL_BACKGROUND_TITLE : WS* EN_LOL_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    EN_LOL_EXAMPLES_TITLE : WS* EN_LOL_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    EN_LOL_SCENARIO_TITLE : WS* EN_LOL_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    EN_LOL_SCENARIO_OUTLINE_TITLE : WS* EN_LOL_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    EN_LOL_RULE_TITLE : WS* EN_LOL_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        EN_LOL_GIVEN_STEP : WS* EN_LOL_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	EN_LOL_WHEN_STEP : WS* EN_LOL_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	EN_LOL_THEN_STEP : WS* EN_LOL_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	EN_LOL_AND_STEP : WS* EN_LOL_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	EN_LOL_BUT_STEP : WS* EN_LOL_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Old English
//Englisc
mode EN_OLD;
	EN_OLD_FEATURE : ( 
	('Hwaet'
		| 'Hwæt'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    EN_OLD_BACKGROUND : (
    
	('Aer'
		| 'Ær'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	EN_OLD_SCENARIO : (

	('Swa'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	EN_OLD_SCENARIO_OUTLINE : (

	('Swa hwaer swa'
		| 'Swa hwær swa'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	EN_OLD_EXAMPLES : (

	('Se the'
		| 'Se þe'
		| 'Se ðe'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	EN_OLD_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	EN_OLD_GIVEN : (

	('Thurh '
		| 'Þurh '
		| 'Ðurh '
	)   ) -> type(GIVEN_KEYWORD) ;

	EN_OLD_WHEN : (

	('Tha '
		| 'Þa '
		| 'Ða '
	)   ) -> type(WHEN_KEYWORD) ;

	EN_OLD_THEN : (

	('Tha '
		| 'Þa '
		| 'Ða '
		| 'Tha the '
		| 'Þa þe '
		| 'Ða ðe '
	)   ) -> type(THEN_KEYWORD) ;

	EN_OLD_AND : (

	('Ond '
		| '7 '
	)   ) -> type(AND_KEYWORD) ;

	EN_OLD_BUT : (

	('Ac '
	)   ) -> type(BUT_KEYWORD) ;

	EN_OLD_STARTING_STEP_KEYWORD : (

                EN_OLD_GIVEN
		| EN_OLD_WHEN
		| EN_OLD_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	EN_OLD_ALTERNATIVE_STEP_KEYWORD : (

                EN_OLD_AND
		| EN_OLD_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    EN_OLD_FEATURE_TITLE : WS* EN_OLD_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    EN_OLD_BACKGROUND_TITLE : WS* EN_OLD_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    EN_OLD_EXAMPLES_TITLE : WS* EN_OLD_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    EN_OLD_SCENARIO_TITLE : WS* EN_OLD_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    EN_OLD_SCENARIO_OUTLINE_TITLE : WS* EN_OLD_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    EN_OLD_RULE_TITLE : WS* EN_OLD_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        EN_OLD_GIVEN_STEP : WS* EN_OLD_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	EN_OLD_WHEN_STEP : WS* EN_OLD_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	EN_OLD_THEN_STEP : WS* EN_OLD_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	EN_OLD_AND_STEP : WS* EN_OLD_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	EN_OLD_BUT_STEP : WS* EN_OLD_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Pirate
//Pirate
mode EN_PIRATE;
	EN_PIRATE_FEATURE : ( 
	('Ahoy matey!'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    EN_PIRATE_BACKGROUND : (
    
	('Yo-ho-ho'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	EN_PIRATE_SCENARIO : (

	('Heave to'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	EN_PIRATE_SCENARIO_OUTLINE : (

	('Shiver me timbers'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	EN_PIRATE_EXAMPLES : (

	('Dead men tell no tales'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	EN_PIRATE_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	EN_PIRATE_GIVEN : (

	('Gangway! '
	)   ) -> type(GIVEN_KEYWORD) ;

	EN_PIRATE_WHEN : (

	('Blimey! '
	)   ) -> type(WHEN_KEYWORD) ;

	EN_PIRATE_THEN : (

	('Let go and haul '
	)   ) -> type(THEN_KEYWORD) ;

	EN_PIRATE_AND : (

	('Aye '
	)   ) -> type(AND_KEYWORD) ;

	EN_PIRATE_BUT : (

	('Avast! '
	)   ) -> type(BUT_KEYWORD) ;

	EN_PIRATE_STARTING_STEP_KEYWORD : (

                EN_PIRATE_GIVEN
		| EN_PIRATE_WHEN
		| EN_PIRATE_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	EN_PIRATE_ALTERNATIVE_STEP_KEYWORD : (

                EN_PIRATE_AND
		| EN_PIRATE_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    EN_PIRATE_FEATURE_TITLE : WS* EN_PIRATE_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    EN_PIRATE_BACKGROUND_TITLE : WS* EN_PIRATE_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    EN_PIRATE_EXAMPLES_TITLE : WS* EN_PIRATE_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    EN_PIRATE_SCENARIO_TITLE : WS* EN_PIRATE_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    EN_PIRATE_SCENARIO_OUTLINE_TITLE : WS* EN_PIRATE_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    EN_PIRATE_RULE_TITLE : WS* EN_PIRATE_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        EN_PIRATE_GIVEN_STEP : WS* EN_PIRATE_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	EN_PIRATE_WHEN_STEP : WS* EN_PIRATE_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	EN_PIRATE_THEN_STEP : WS* EN_PIRATE_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	EN_PIRATE_AND_STEP : WS* EN_PIRATE_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	EN_PIRATE_BUT_STEP : WS* EN_PIRATE_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Esperanto
//Esperanto
mode EO;
	EO_FEATURE : ( 
	('Trajto'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    EO_BACKGROUND : (
    
	('Fono'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	EO_SCENARIO : (

	('Ekzemplo'
		| 'Scenaro'
		| 'Kazo'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	EO_SCENARIO_OUTLINE : (

	('Konturo de la scenaro'
		| 'Skizo'
		| 'Kazo-skizo'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	EO_EXAMPLES : (

	('Ekzemploj'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	EO_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	EO_GIVEN : (

	('Donitaĵo '
		| 'Komence '
	)   ) -> type(GIVEN_KEYWORD) ;

	EO_WHEN : (

	('Se '
	)   ) -> type(WHEN_KEYWORD) ;

	EO_THEN : (

	('Do '
	)   ) -> type(THEN_KEYWORD) ;

	EO_AND : (

	('Kaj '
	)   ) -> type(AND_KEYWORD) ;

	EO_BUT : (

	('Sed '
	)   ) -> type(BUT_KEYWORD) ;

	EO_STARTING_STEP_KEYWORD : (

                EO_GIVEN
		| EO_WHEN
		| EO_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	EO_ALTERNATIVE_STEP_KEYWORD : (

                EO_AND
		| EO_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    EO_FEATURE_TITLE : WS* EO_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    EO_BACKGROUND_TITLE : WS* EO_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    EO_EXAMPLES_TITLE : WS* EO_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    EO_SCENARIO_TITLE : WS* EO_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    EO_SCENARIO_OUTLINE_TITLE : WS* EO_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    EO_RULE_TITLE : WS* EO_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        EO_GIVEN_STEP : WS* EO_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	EO_WHEN_STEP : WS* EO_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	EO_THEN_STEP : WS* EO_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	EO_AND_STEP : WS* EO_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	EO_BUT_STEP : WS* EO_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Spanish
//español
mode ES;
	ES_FEATURE : ( 
	('Característica'
		| 'Necesidad del negocio'
		| 'Requisito'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    ES_BACKGROUND : (
    
	('Antecedentes'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	ES_SCENARIO : (

	('Ejemplo'
		| 'Escenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	ES_SCENARIO_OUTLINE : (

	('Esquema del escenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	ES_EXAMPLES : (

	('Ejemplos'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	ES_RULE : (

	('Regla'
		| 'Regla de negocio'
	) ':'  ) -> type(RULE_KEYWORD) ;

	ES_GIVEN : (

	('Dado '
		| 'Dada '
		| 'Dados '
		| 'Dadas '
	)   ) -> type(GIVEN_KEYWORD) ;

	ES_WHEN : (

	('Cuando '
	)   ) -> type(WHEN_KEYWORD) ;

	ES_THEN : (

	('Entonces '
	)   ) -> type(THEN_KEYWORD) ;

	ES_AND : (

	('Y '
		| 'E '
	)   ) -> type(AND_KEYWORD) ;

	ES_BUT : (

	('Pero '
	)   ) -> type(BUT_KEYWORD) ;

	ES_STARTING_STEP_KEYWORD : (

                ES_GIVEN
		| ES_WHEN
		| ES_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	ES_ALTERNATIVE_STEP_KEYWORD : (

                ES_AND
		| ES_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    ES_FEATURE_TITLE : WS* ES_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    ES_BACKGROUND_TITLE : WS* ES_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    ES_EXAMPLES_TITLE : WS* ES_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    ES_SCENARIO_TITLE : WS* ES_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    ES_SCENARIO_OUTLINE_TITLE : WS* ES_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    ES_RULE_TITLE : WS* ES_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        ES_GIVEN_STEP : WS* ES_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	ES_WHEN_STEP : WS* ES_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	ES_THEN_STEP : WS* ES_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	ES_AND_STEP : WS* ES_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	ES_BUT_STEP : WS* ES_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Estonian
//eesti keel
mode ET;
	ET_FEATURE : ( 
	('Omadus'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    ET_BACKGROUND : (
    
	('Taust'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	ET_SCENARIO : (

	('Juhtum'
		| 'Stsenaarium'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	ET_SCENARIO_OUTLINE : (

	('Raamjuhtum'
		| 'Raamstsenaarium'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	ET_EXAMPLES : (

	('Juhtumid'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	ET_RULE : (

	('Reegel'
	) ':'  ) -> type(RULE_KEYWORD) ;

	ET_GIVEN : (

	('Eeldades '
	)   ) -> type(GIVEN_KEYWORD) ;

	ET_WHEN : (

	('Kui '
	)   ) -> type(WHEN_KEYWORD) ;

	ET_THEN : (

	('Siis '
	)   ) -> type(THEN_KEYWORD) ;

	ET_AND : (

	('Ja '
	)   ) -> type(AND_KEYWORD) ;

	ET_BUT : (

	('Kuid '
	)   ) -> type(BUT_KEYWORD) ;

	ET_STARTING_STEP_KEYWORD : (

                ET_GIVEN
		| ET_WHEN
		| ET_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	ET_ALTERNATIVE_STEP_KEYWORD : (

                ET_AND
		| ET_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    ET_FEATURE_TITLE : WS* ET_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    ET_BACKGROUND_TITLE : WS* ET_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    ET_EXAMPLES_TITLE : WS* ET_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    ET_SCENARIO_TITLE : WS* ET_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    ET_SCENARIO_OUTLINE_TITLE : WS* ET_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    ET_RULE_TITLE : WS* ET_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        ET_GIVEN_STEP : WS* ET_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	ET_WHEN_STEP : WS* ET_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	ET_THEN_STEP : WS* ET_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	ET_AND_STEP : WS* ET_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	ET_BUT_STEP : WS* ET_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Persian
//فارسی
mode FA;
	FA_FEATURE : ( 
	('وِیژگی'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    FA_BACKGROUND : (
    
	('زمینه'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	FA_SCENARIO : (

	('مثال'
		| 'سناریو'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	FA_SCENARIO_OUTLINE : (

	('الگوی سناریو'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	FA_EXAMPLES : (

	('نمونه ها'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	FA_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	FA_GIVEN : (

	('با فرض '
	)   ) -> type(GIVEN_KEYWORD) ;

	FA_WHEN : (

	('هنگامی '
	)   ) -> type(WHEN_KEYWORD) ;

	FA_THEN : (

	('آنگاه '
	)   ) -> type(THEN_KEYWORD) ;

	FA_AND : (

	('و '
	)   ) -> type(AND_KEYWORD) ;

	FA_BUT : (

	('اما '
	)   ) -> type(BUT_KEYWORD) ;

	FA_STARTING_STEP_KEYWORD : (

                FA_GIVEN
		| FA_WHEN
		| FA_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	FA_ALTERNATIVE_STEP_KEYWORD : (

                FA_AND
		| FA_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    FA_FEATURE_TITLE : WS* FA_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    FA_BACKGROUND_TITLE : WS* FA_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    FA_EXAMPLES_TITLE : WS* FA_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    FA_SCENARIO_TITLE : WS* FA_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    FA_SCENARIO_OUTLINE_TITLE : WS* FA_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    FA_RULE_TITLE : WS* FA_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        FA_GIVEN_STEP : WS* FA_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	FA_WHEN_STEP : WS* FA_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	FA_THEN_STEP : WS* FA_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	FA_AND_STEP : WS* FA_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	FA_BUT_STEP : WS* FA_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Finnish
//suomi
mode FI;
	FI_FEATURE : ( 
	('Ominaisuus'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    FI_BACKGROUND : (
    
	('Tausta'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	FI_SCENARIO : (

	('Tapaus'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	FI_SCENARIO_OUTLINE : (

	('Tapausaihio'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	FI_EXAMPLES : (

	('Tapaukset'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	FI_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	FI_GIVEN : (

	('Oletetaan '
	)   ) -> type(GIVEN_KEYWORD) ;

	FI_WHEN : (

	('Kun '
	)   ) -> type(WHEN_KEYWORD) ;

	FI_THEN : (

	('Niin '
	)   ) -> type(THEN_KEYWORD) ;

	FI_AND : (

	('Ja '
	)   ) -> type(AND_KEYWORD) ;

	FI_BUT : (

	('Mutta '
	)   ) -> type(BUT_KEYWORD) ;

	FI_STARTING_STEP_KEYWORD : (

                FI_GIVEN
		| FI_WHEN
		| FI_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	FI_ALTERNATIVE_STEP_KEYWORD : (

                FI_AND
		| FI_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    FI_FEATURE_TITLE : WS* FI_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    FI_BACKGROUND_TITLE : WS* FI_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    FI_EXAMPLES_TITLE : WS* FI_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    FI_SCENARIO_TITLE : WS* FI_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    FI_SCENARIO_OUTLINE_TITLE : WS* FI_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    FI_RULE_TITLE : WS* FI_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        FI_GIVEN_STEP : WS* FI_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	FI_WHEN_STEP : WS* FI_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	FI_THEN_STEP : WS* FI_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	FI_AND_STEP : WS* FI_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	FI_BUT_STEP : WS* FI_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//French
//français
mode FR;
	FR_FEATURE : ( 
	('Fonctionnalité'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    FR_BACKGROUND : (
    
	('Contexte'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	FR_SCENARIO : (

	('Exemple'
		| 'Scénario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	FR_SCENARIO_OUTLINE : (

	('Plan du scénario'
		| 'Plan du Scénario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	FR_EXAMPLES : (

	('Exemples'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	FR_RULE : (

	('Règle'
	) ':'  ) -> type(RULE_KEYWORD) ;

	FR_GIVEN : (

	('Soit '
		| 'Sachant que '
		| 'Sachant qu\''
		| 'Sachant '
		| 'Etant donné que '
		| 'Etant donné qu\''
		| 'Etant donné '
		| 'Etant donnée '
		| 'Etant donnés '
		| 'Etant données '
		| 'Étant donné que '
		| 'Étant donné qu\''
		| 'Étant donné '
		| 'Étant donnée '
		| 'Étant donnés '
		| 'Étant données '
	)   ) -> type(GIVEN_KEYWORD) ;

	FR_WHEN : (

	('Quand '
		| 'Lorsque '
		| 'Lorsqu\''
	)   ) -> type(WHEN_KEYWORD) ;

	FR_THEN : (

	('Alors '
		| 'Donc '
	)   ) -> type(THEN_KEYWORD) ;

	FR_AND : (

	('Et que '
		| 'Et qu\''
		| 'Et '
	)   ) -> type(AND_KEYWORD) ;

	FR_BUT : (

	('Mais que '
		| 'Mais qu\''
		| 'Mais '
	)   ) -> type(BUT_KEYWORD) ;

	FR_STARTING_STEP_KEYWORD : (

                FR_GIVEN
		| FR_WHEN
		| FR_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	FR_ALTERNATIVE_STEP_KEYWORD : (

                FR_AND
		| FR_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    FR_FEATURE_TITLE : WS* FR_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    FR_BACKGROUND_TITLE : WS* FR_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    FR_EXAMPLES_TITLE : WS* FR_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    FR_SCENARIO_TITLE : WS* FR_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    FR_SCENARIO_OUTLINE_TITLE : WS* FR_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    FR_RULE_TITLE : WS* FR_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        FR_GIVEN_STEP : WS* FR_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	FR_WHEN_STEP : WS* FR_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	FR_THEN_STEP : WS* FR_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	FR_AND_STEP : WS* FR_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	FR_BUT_STEP : WS* FR_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Irish
//Gaeilge
mode GA;
	GA_FEATURE : ( 
	('Gné'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    GA_BACKGROUND : (
    
	('Cúlra'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	GA_SCENARIO : (

	('Sampla'
		| 'Cás'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	GA_SCENARIO_OUTLINE : (

	('Cás Achomair'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	GA_EXAMPLES : (

	('Samplaí'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	GA_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	GA_GIVEN : (

	('Cuir i gcás go'
		| 'Cuir i gcás nach'
		| 'Cuir i gcás gur'
		| 'Cuir i gcás nár'
	)   ) -> type(GIVEN_KEYWORD) ;

	GA_WHEN : (

	('Nuair a'
		| 'Nuair nach'
		| 'Nuair ba'
		| 'Nuair nár'
	)   ) -> type(WHEN_KEYWORD) ;

	GA_THEN : (

	('Ansin'
	)   ) -> type(THEN_KEYWORD) ;

	GA_AND : (

	('Agus'
	)   ) -> type(AND_KEYWORD) ;

	GA_BUT : (

	('Ach'
	)   ) -> type(BUT_KEYWORD) ;

	GA_STARTING_STEP_KEYWORD : (

                GA_GIVEN
		| GA_WHEN
		| GA_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	GA_ALTERNATIVE_STEP_KEYWORD : (

                GA_AND
		| GA_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    GA_FEATURE_TITLE : WS* GA_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    GA_BACKGROUND_TITLE : WS* GA_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    GA_EXAMPLES_TITLE : WS* GA_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    GA_SCENARIO_TITLE : WS* GA_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    GA_SCENARIO_OUTLINE_TITLE : WS* GA_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    GA_RULE_TITLE : WS* GA_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        GA_GIVEN_STEP : WS* GA_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	GA_WHEN_STEP : WS* GA_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	GA_THEN_STEP : WS* GA_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	GA_AND_STEP : WS* GA_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	GA_BUT_STEP : WS* GA_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Gujarati
//ગુજરાતી
mode GJ;
	GJ_FEATURE : ( 
	('લક્ષણ'
		| 'વ્યાપાર જરૂર'
		| 'ક્ષમતા'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    GJ_BACKGROUND : (
    
	('બેકગ્રાઉન્ડ'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	GJ_SCENARIO : (

	('ઉદાહરણ'
		| 'સ્થિતિ'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	GJ_SCENARIO_OUTLINE : (

	('પરિદ્દશ્ય રૂપરેખા'
		| 'પરિદ્દશ્ય ઢાંચો'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	GJ_EXAMPLES : (

	('ઉદાહરણો'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	GJ_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	GJ_GIVEN : (

	('આપેલ છે '
	)   ) -> type(GIVEN_KEYWORD) ;

	GJ_WHEN : (

	('ક્યારે '
	)   ) -> type(WHEN_KEYWORD) ;

	GJ_THEN : (

	('પછી '
	)   ) -> type(THEN_KEYWORD) ;

	GJ_AND : (

	('અને '
	)   ) -> type(AND_KEYWORD) ;

	GJ_BUT : (

	('પણ '
	)   ) -> type(BUT_KEYWORD) ;

	GJ_STARTING_STEP_KEYWORD : (

                GJ_GIVEN
		| GJ_WHEN
		| GJ_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	GJ_ALTERNATIVE_STEP_KEYWORD : (

                GJ_AND
		| GJ_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    GJ_FEATURE_TITLE : WS* GJ_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    GJ_BACKGROUND_TITLE : WS* GJ_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    GJ_EXAMPLES_TITLE : WS* GJ_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    GJ_SCENARIO_TITLE : WS* GJ_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    GJ_SCENARIO_OUTLINE_TITLE : WS* GJ_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    GJ_RULE_TITLE : WS* GJ_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        GJ_GIVEN_STEP : WS* GJ_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	GJ_WHEN_STEP : WS* GJ_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	GJ_THEN_STEP : WS* GJ_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	GJ_AND_STEP : WS* GJ_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	GJ_BUT_STEP : WS* GJ_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Galician
//galego
mode GL;
	GL_FEATURE : ( 
	('Característica'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    GL_BACKGROUND : (
    
	('Contexto'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	GL_SCENARIO : (

	('Exemplo'
		| 'Escenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	GL_SCENARIO_OUTLINE : (

	('Esbozo do escenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	GL_EXAMPLES : (

	('Exemplos'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	GL_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	GL_GIVEN : (

	('Dado '
		| 'Dada '
		| 'Dados '
		| 'Dadas '
	)   ) -> type(GIVEN_KEYWORD) ;

	GL_WHEN : (

	('Cando '
	)   ) -> type(WHEN_KEYWORD) ;

	GL_THEN : (

	('Entón '
		| 'Logo '
	)   ) -> type(THEN_KEYWORD) ;

	GL_AND : (

	('E '
	)   ) -> type(AND_KEYWORD) ;

	GL_BUT : (

	('Mais '
		| 'Pero '
	)   ) -> type(BUT_KEYWORD) ;

	GL_STARTING_STEP_KEYWORD : (

                GL_GIVEN
		| GL_WHEN
		| GL_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	GL_ALTERNATIVE_STEP_KEYWORD : (

                GL_AND
		| GL_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    GL_FEATURE_TITLE : WS* GL_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    GL_BACKGROUND_TITLE : WS* GL_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    GL_EXAMPLES_TITLE : WS* GL_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    GL_SCENARIO_TITLE : WS* GL_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    GL_SCENARIO_OUTLINE_TITLE : WS* GL_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    GL_RULE_TITLE : WS* GL_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        GL_GIVEN_STEP : WS* GL_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	GL_WHEN_STEP : WS* GL_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	GL_THEN_STEP : WS* GL_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	GL_AND_STEP : WS* GL_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	GL_BUT_STEP : WS* GL_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Hebrew
//עברית
mode HE;
	HE_FEATURE : ( 
	('תכונה'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    HE_BACKGROUND : (
    
	('רקע'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	HE_SCENARIO : (

	('דוגמא'
		| 'תרחיש'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	HE_SCENARIO_OUTLINE : (

	('תבנית תרחיש'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	HE_EXAMPLES : (

	('דוגמאות'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	HE_RULE : (

	('כלל'
	) ':'  ) -> type(RULE_KEYWORD) ;

	HE_GIVEN : (

	('בהינתן '
	)   ) -> type(GIVEN_KEYWORD) ;

	HE_WHEN : (

	('כאשר '
	)   ) -> type(WHEN_KEYWORD) ;

	HE_THEN : (

	('אז '
		| 'אזי '
	)   ) -> type(THEN_KEYWORD) ;

	HE_AND : (

	('וגם '
	)   ) -> type(AND_KEYWORD) ;

	HE_BUT : (

	('אבל '
	)   ) -> type(BUT_KEYWORD) ;

	HE_STARTING_STEP_KEYWORD : (

                HE_GIVEN
		| HE_WHEN
		| HE_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	HE_ALTERNATIVE_STEP_KEYWORD : (

                HE_AND
		| HE_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    HE_FEATURE_TITLE : WS* HE_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    HE_BACKGROUND_TITLE : WS* HE_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    HE_EXAMPLES_TITLE : WS* HE_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    HE_SCENARIO_TITLE : WS* HE_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    HE_SCENARIO_OUTLINE_TITLE : WS* HE_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    HE_RULE_TITLE : WS* HE_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        HE_GIVEN_STEP : WS* HE_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	HE_WHEN_STEP : WS* HE_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	HE_THEN_STEP : WS* HE_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	HE_AND_STEP : WS* HE_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	HE_BUT_STEP : WS* HE_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Hindi
//हिंदी
mode HI;
	HI_FEATURE : ( 
	('रूप लेख'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    HI_BACKGROUND : (
    
	('पृष्ठभूमि'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	HI_SCENARIO : (

	('परिदृश्य'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	HI_SCENARIO_OUTLINE : (

	('परिदृश्य रूपरेखा'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	HI_EXAMPLES : (

	('उदाहरण'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	HI_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	HI_GIVEN : (

	('अगर '
		| 'यदि '
		| 'चूंकि '
	)   ) -> type(GIVEN_KEYWORD) ;

	HI_WHEN : (

	('जब '
		| 'कदा '
	)   ) -> type(WHEN_KEYWORD) ;

	HI_THEN : (

	('तब '
		| 'तदा '
	)   ) -> type(THEN_KEYWORD) ;

	HI_AND : (

	('और '
		| 'तथा '
	)   ) -> type(AND_KEYWORD) ;

	HI_BUT : (

	('पर '
		| 'परन्तु '
		| 'किन्तु '
	)   ) -> type(BUT_KEYWORD) ;

	HI_STARTING_STEP_KEYWORD : (

                HI_GIVEN
		| HI_WHEN
		| HI_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	HI_ALTERNATIVE_STEP_KEYWORD : (

                HI_AND
		| HI_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    HI_FEATURE_TITLE : WS* HI_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    HI_BACKGROUND_TITLE : WS* HI_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    HI_EXAMPLES_TITLE : WS* HI_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    HI_SCENARIO_TITLE : WS* HI_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    HI_SCENARIO_OUTLINE_TITLE : WS* HI_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    HI_RULE_TITLE : WS* HI_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        HI_GIVEN_STEP : WS* HI_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	HI_WHEN_STEP : WS* HI_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	HI_THEN_STEP : WS* HI_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	HI_AND_STEP : WS* HI_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	HI_BUT_STEP : WS* HI_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Croatian
//hrvatski
mode HR;
	HR_FEATURE : ( 
	('Osobina'
		| 'Mogućnost'
		| 'Mogucnost'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    HR_BACKGROUND : (
    
	('Pozadina'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	HR_SCENARIO : (

	('Primjer'
		| 'Scenarij'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	HR_SCENARIO_OUTLINE : (

	('Skica'
		| 'Koncept'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	HR_EXAMPLES : (

	('Primjeri'
		| 'Scenariji'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	HR_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	HR_GIVEN : (

	('Zadan '
		| 'Zadani '
		| 'Zadano '
		| 'Ukoliko '
	)   ) -> type(GIVEN_KEYWORD) ;

	HR_WHEN : (

	('Kada '
		| 'Kad '
	)   ) -> type(WHEN_KEYWORD) ;

	HR_THEN : (

	('Onda '
	)   ) -> type(THEN_KEYWORD) ;

	HR_AND : (

	('I '
	)   ) -> type(AND_KEYWORD) ;

	HR_BUT : (

	('Ali '
	)   ) -> type(BUT_KEYWORD) ;

	HR_STARTING_STEP_KEYWORD : (

                HR_GIVEN
		| HR_WHEN
		| HR_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	HR_ALTERNATIVE_STEP_KEYWORD : (

                HR_AND
		| HR_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    HR_FEATURE_TITLE : WS* HR_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    HR_BACKGROUND_TITLE : WS* HR_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    HR_EXAMPLES_TITLE : WS* HR_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    HR_SCENARIO_TITLE : WS* HR_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    HR_SCENARIO_OUTLINE_TITLE : WS* HR_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    HR_RULE_TITLE : WS* HR_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        HR_GIVEN_STEP : WS* HR_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	HR_WHEN_STEP : WS* HR_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	HR_THEN_STEP : WS* HR_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	HR_AND_STEP : WS* HR_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	HR_BUT_STEP : WS* HR_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Creole
//kreyòl
mode HT;
	HT_FEATURE : ( 
	('Karakteristik'
		| 'Mak'
		| 'Fonksyonalite'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    HT_BACKGROUND : (
    
	('Kontèks'
		| 'Istorik'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	HT_SCENARIO : (

	('Senaryo'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	HT_SCENARIO_OUTLINE : (

	('Plan senaryo'
		| 'Plan Senaryo'
		| 'Senaryo deskripsyon'
		| 'Senaryo Deskripsyon'
		| 'Dyagram senaryo'
		| 'Dyagram Senaryo'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	HT_EXAMPLES : (

	('Egzanp'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	HT_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	HT_GIVEN : (

	('Sipoze '
		| 'Sipoze ke '
		| 'Sipoze Ke '
	)   ) -> type(GIVEN_KEYWORD) ;

	HT_WHEN : (

	('Lè '
		| 'Le '
	)   ) -> type(WHEN_KEYWORD) ;

	HT_THEN : (

	('Lè sa a '
		| 'Le sa a '
	)   ) -> type(THEN_KEYWORD) ;

	HT_AND : (

	('Ak '
		| 'Epi '
		| 'E '
	)   ) -> type(AND_KEYWORD) ;

	HT_BUT : (

	('Men '
	)   ) -> type(BUT_KEYWORD) ;

	HT_STARTING_STEP_KEYWORD : (

                HT_GIVEN
		| HT_WHEN
		| HT_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	HT_ALTERNATIVE_STEP_KEYWORD : (

                HT_AND
		| HT_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    HT_FEATURE_TITLE : WS* HT_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    HT_BACKGROUND_TITLE : WS* HT_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    HT_EXAMPLES_TITLE : WS* HT_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    HT_SCENARIO_TITLE : WS* HT_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    HT_SCENARIO_OUTLINE_TITLE : WS* HT_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    HT_RULE_TITLE : WS* HT_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        HT_GIVEN_STEP : WS* HT_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	HT_WHEN_STEP : WS* HT_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	HT_THEN_STEP : WS* HT_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	HT_AND_STEP : WS* HT_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	HT_BUT_STEP : WS* HT_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Hungarian
//magyar
mode HU;
	HU_FEATURE : ( 
	('Jellemző'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    HU_BACKGROUND : (
    
	('Háttér'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	HU_SCENARIO : (

	('Példa'
		| 'Forgatókönyv'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	HU_SCENARIO_OUTLINE : (

	('Forgatókönyv vázlat'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	HU_EXAMPLES : (

	('Példák'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	HU_RULE : (

	('Szabály'
	) ':'  ) -> type(RULE_KEYWORD) ;

	HU_GIVEN : (

	('Amennyiben '
		| 'Adott '
	)   ) -> type(GIVEN_KEYWORD) ;

	HU_WHEN : (

	('Majd '
		| 'Ha '
		| 'Amikor '
	)   ) -> type(WHEN_KEYWORD) ;

	HU_THEN : (

	('Akkor '
	)   ) -> type(THEN_KEYWORD) ;

	HU_AND : (

	('És '
	)   ) -> type(AND_KEYWORD) ;

	HU_BUT : (

	('De '
	)   ) -> type(BUT_KEYWORD) ;

	HU_STARTING_STEP_KEYWORD : (

                HU_GIVEN
		| HU_WHEN
		| HU_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	HU_ALTERNATIVE_STEP_KEYWORD : (

                HU_AND
		| HU_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    HU_FEATURE_TITLE : WS* HU_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    HU_BACKGROUND_TITLE : WS* HU_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    HU_EXAMPLES_TITLE : WS* HU_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    HU_SCENARIO_TITLE : WS* HU_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    HU_SCENARIO_OUTLINE_TITLE : WS* HU_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    HU_RULE_TITLE : WS* HU_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        HU_GIVEN_STEP : WS* HU_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	HU_WHEN_STEP : WS* HU_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	HU_THEN_STEP : WS* HU_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	HU_AND_STEP : WS* HU_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	HU_BUT_STEP : WS* HU_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Indonesian
//Bahasa Indonesia
mode ID;
	ID_FEATURE : ( 
	('Fitur'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    ID_BACKGROUND : (
    
	('Dasar'
		| 'Latar Belakang'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	ID_SCENARIO : (

	('Skenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	ID_SCENARIO_OUTLINE : (

	('Skenario konsep'
		| 'Garis-Besar Skenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	ID_EXAMPLES : (

	('Contoh'
		| 'Misal'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	ID_RULE : (

	('Rule'
		| 'Aturan'
	) ':'  ) -> type(RULE_KEYWORD) ;

	ID_GIVEN : (

	('Dengan '
		| 'Diketahui '
		| 'Diasumsikan '
		| 'Bila '
		| 'Jika '
	)   ) -> type(GIVEN_KEYWORD) ;

	ID_WHEN : (

	('Ketika '
	)   ) -> type(WHEN_KEYWORD) ;

	ID_THEN : (

	('Maka '
		| 'Kemudian '
	)   ) -> type(THEN_KEYWORD) ;

	ID_AND : (

	('Dan '
	)   ) -> type(AND_KEYWORD) ;

	ID_BUT : (

	('Tapi '
		| 'Tetapi '
	)   ) -> type(BUT_KEYWORD) ;

	ID_STARTING_STEP_KEYWORD : (

                ID_GIVEN
		| ID_WHEN
		| ID_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	ID_ALTERNATIVE_STEP_KEYWORD : (

                ID_AND
		| ID_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    ID_FEATURE_TITLE : WS* ID_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    ID_BACKGROUND_TITLE : WS* ID_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    ID_EXAMPLES_TITLE : WS* ID_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    ID_SCENARIO_TITLE : WS* ID_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    ID_SCENARIO_OUTLINE_TITLE : WS* ID_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    ID_RULE_TITLE : WS* ID_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        ID_GIVEN_STEP : WS* ID_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	ID_WHEN_STEP : WS* ID_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	ID_THEN_STEP : WS* ID_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	ID_AND_STEP : WS* ID_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	ID_BUT_STEP : WS* ID_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Icelandic
//Íslenska
mode IS;
	IS_FEATURE : ( 
	('Eiginleiki'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    IS_BACKGROUND : (
    
	('Bakgrunnur'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	IS_SCENARIO : (

	('Atburðarás'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	IS_SCENARIO_OUTLINE : (

	('Lýsing Atburðarásar'
		| 'Lýsing Dæma'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	IS_EXAMPLES : (

	('Dæmi'
		| 'Atburðarásir'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	IS_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	IS_GIVEN : (

	('Ef '
	)   ) -> type(GIVEN_KEYWORD) ;

	IS_WHEN : (

	('Þegar '
	)   ) -> type(WHEN_KEYWORD) ;

	IS_THEN : (

	('Þá '
	)   ) -> type(THEN_KEYWORD) ;

	IS_AND : (

	('Og '
	)   ) -> type(AND_KEYWORD) ;

	IS_BUT : (

	('En '
	)   ) -> type(BUT_KEYWORD) ;

	IS_STARTING_STEP_KEYWORD : (

                IS_GIVEN
		| IS_WHEN
		| IS_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	IS_ALTERNATIVE_STEP_KEYWORD : (

                IS_AND
		| IS_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    IS_FEATURE_TITLE : WS* IS_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    IS_BACKGROUND_TITLE : WS* IS_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    IS_EXAMPLES_TITLE : WS* IS_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    IS_SCENARIO_TITLE : WS* IS_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    IS_SCENARIO_OUTLINE_TITLE : WS* IS_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    IS_RULE_TITLE : WS* IS_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        IS_GIVEN_STEP : WS* IS_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	IS_WHEN_STEP : WS* IS_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	IS_THEN_STEP : WS* IS_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	IS_AND_STEP : WS* IS_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	IS_BUT_STEP : WS* IS_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Italian
//italiano
mode IT;
	IT_FEATURE : ( 
	('Funzionalità'
		| 'Esigenza di Business'
		| 'Abilità'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    IT_BACKGROUND : (
    
	('Contesto'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	IT_SCENARIO : (

	('Esempio'
		| 'Scenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	IT_SCENARIO_OUTLINE : (

	('Schema dello scenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	IT_EXAMPLES : (

	('Esempi'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	IT_RULE : (

	('Regola'
	) ':'  ) -> type(RULE_KEYWORD) ;

	IT_GIVEN : (

	('Dato '
		| 'Data '
		| 'Dati '
		| 'Date '
	)   ) -> type(GIVEN_KEYWORD) ;

	IT_WHEN : (

	('Quando '
	)   ) -> type(WHEN_KEYWORD) ;

	IT_THEN : (

	('Allora '
	)   ) -> type(THEN_KEYWORD) ;

	IT_AND : (

	('E '
	)   ) -> type(AND_KEYWORD) ;

	IT_BUT : (

	('Ma '
	)   ) -> type(BUT_KEYWORD) ;

	IT_STARTING_STEP_KEYWORD : (

                IT_GIVEN
		| IT_WHEN
		| IT_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	IT_ALTERNATIVE_STEP_KEYWORD : (

                IT_AND
		| IT_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    IT_FEATURE_TITLE : WS* IT_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    IT_BACKGROUND_TITLE : WS* IT_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    IT_EXAMPLES_TITLE : WS* IT_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    IT_SCENARIO_TITLE : WS* IT_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    IT_SCENARIO_OUTLINE_TITLE : WS* IT_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    IT_RULE_TITLE : WS* IT_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        IT_GIVEN_STEP : WS* IT_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	IT_WHEN_STEP : WS* IT_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	IT_THEN_STEP : WS* IT_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	IT_AND_STEP : WS* IT_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	IT_BUT_STEP : WS* IT_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Japanese
//日本語
mode JA;
	JA_FEATURE : ( 
	('フィーチャ'
		| '機能'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    JA_BACKGROUND : (
    
	('背景'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	JA_SCENARIO : (

	('シナリオ'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	JA_SCENARIO_OUTLINE : (

	('シナリオアウトライン'
		| 'シナリオテンプレート'
		| 'テンプレ'
		| 'シナリオテンプレ'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	JA_EXAMPLES : (

	('例'
		| 'サンプル'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	JA_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	JA_GIVEN : (

	('前提'
	)   ) -> type(GIVEN_KEYWORD) ;

	JA_WHEN : (

	('もし'
	)   ) -> type(WHEN_KEYWORD) ;

	JA_THEN : (

	('ならば'
	)   ) -> type(THEN_KEYWORD) ;

	JA_AND : (

	('かつ'
	)   ) -> type(AND_KEYWORD) ;

	JA_BUT : (

	('しかし'
		| '但し'
		| 'ただし'
	)   ) -> type(BUT_KEYWORD) ;

	JA_STARTING_STEP_KEYWORD : (

                JA_GIVEN
		| JA_WHEN
		| JA_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	JA_ALTERNATIVE_STEP_KEYWORD : (

                JA_AND
		| JA_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    JA_FEATURE_TITLE : WS* JA_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    JA_BACKGROUND_TITLE : WS* JA_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    JA_EXAMPLES_TITLE : WS* JA_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    JA_SCENARIO_TITLE : WS* JA_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    JA_SCENARIO_OUTLINE_TITLE : WS* JA_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    JA_RULE_TITLE : WS* JA_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        JA_GIVEN_STEP : WS* JA_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	JA_WHEN_STEP : WS* JA_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	JA_THEN_STEP : WS* JA_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	JA_AND_STEP : WS* JA_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	JA_BUT_STEP : WS* JA_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Javanese
//Basa Jawa
mode JV;
	JV_FEATURE : ( 
	('Fitur'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    JV_BACKGROUND : (
    
	('Dasar'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	JV_SCENARIO : (

	('Skenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	JV_SCENARIO_OUTLINE : (

	('Konsep skenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	JV_EXAMPLES : (

	('Conto'
		| 'Contone'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	JV_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	JV_GIVEN : (

	('Nalika '
		| 'Nalikaning '
	)   ) -> type(GIVEN_KEYWORD) ;

	JV_WHEN : (

	('Manawa '
		| 'Menawa '
	)   ) -> type(WHEN_KEYWORD) ;

	JV_THEN : (

	('Njuk '
		| 'Banjur '
	)   ) -> type(THEN_KEYWORD) ;

	JV_AND : (

	('Lan '
	)   ) -> type(AND_KEYWORD) ;

	JV_BUT : (

	('Tapi '
		| 'Nanging '
		| 'Ananging '
	)   ) -> type(BUT_KEYWORD) ;

	JV_STARTING_STEP_KEYWORD : (

                JV_GIVEN
		| JV_WHEN
		| JV_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	JV_ALTERNATIVE_STEP_KEYWORD : (

                JV_AND
		| JV_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    JV_FEATURE_TITLE : WS* JV_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    JV_BACKGROUND_TITLE : WS* JV_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    JV_EXAMPLES_TITLE : WS* JV_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    JV_SCENARIO_TITLE : WS* JV_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    JV_SCENARIO_OUTLINE_TITLE : WS* JV_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    JV_RULE_TITLE : WS* JV_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        JV_GIVEN_STEP : WS* JV_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	JV_WHEN_STEP : WS* JV_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	JV_THEN_STEP : WS* JV_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	JV_AND_STEP : WS* JV_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	JV_BUT_STEP : WS* JV_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Georgian
//ქართველი
mode KA;
	KA_FEATURE : ( 
	('თვისება'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    KA_BACKGROUND : (
    
	('კონტექსტი'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	KA_SCENARIO : (

	('მაგალითად'
		| 'სცენარის'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	KA_SCENARIO_OUTLINE : (

	('სცენარის ნიმუში'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	KA_EXAMPLES : (

	('მაგალითები'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	KA_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	KA_GIVEN : (

	('მოცემული'
	)   ) -> type(GIVEN_KEYWORD) ;

	KA_WHEN : (

	('როდესაც'
	)   ) -> type(WHEN_KEYWORD) ;

	KA_THEN : (

	('მაშინ'
	)   ) -> type(THEN_KEYWORD) ;

	KA_AND : (

	('და'
	)   ) -> type(AND_KEYWORD) ;

	KA_BUT : (

	('მაგ­რამ'
	)   ) -> type(BUT_KEYWORD) ;

	KA_STARTING_STEP_KEYWORD : (

                KA_GIVEN
		| KA_WHEN
		| KA_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	KA_ALTERNATIVE_STEP_KEYWORD : (

                KA_AND
		| KA_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    KA_FEATURE_TITLE : WS* KA_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    KA_BACKGROUND_TITLE : WS* KA_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    KA_EXAMPLES_TITLE : WS* KA_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    KA_SCENARIO_TITLE : WS* KA_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    KA_SCENARIO_OUTLINE_TITLE : WS* KA_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    KA_RULE_TITLE : WS* KA_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        KA_GIVEN_STEP : WS* KA_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	KA_WHEN_STEP : WS* KA_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	KA_THEN_STEP : WS* KA_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	KA_AND_STEP : WS* KA_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	KA_BUT_STEP : WS* KA_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Kannada
//ಕನ್ನಡ
mode KN;
	KN_FEATURE : ( 
	('ಹೆಚ್ಚಳ'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    KN_BACKGROUND : (
    
	('ಹಿನ್ನೆಲೆ'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	KN_SCENARIO : (

	('ಉದಾಹರಣೆ'
		| 'ಕಥಾಸಾರಾಂಶ'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	KN_SCENARIO_OUTLINE : (

	('ವಿವರಣೆ'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	KN_EXAMPLES : (

	('ಉದಾಹರಣೆಗಳು'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	KN_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	KN_GIVEN : (

	('ನೀಡಿದ '
	)   ) -> type(GIVEN_KEYWORD) ;

	KN_WHEN : (

	('ಸ್ಥಿತಿಯನ್ನು '
	)   ) -> type(WHEN_KEYWORD) ;

	KN_THEN : (

	('ನಂತರ '
	)   ) -> type(THEN_KEYWORD) ;

	KN_AND : (

	('ಮತ್ತು '
	)   ) -> type(AND_KEYWORD) ;

	KN_BUT : (

	('ಆದರೆ '
	)   ) -> type(BUT_KEYWORD) ;

	KN_STARTING_STEP_KEYWORD : (

                KN_GIVEN
		| KN_WHEN
		| KN_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	KN_ALTERNATIVE_STEP_KEYWORD : (

                KN_AND
		| KN_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    KN_FEATURE_TITLE : WS* KN_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    KN_BACKGROUND_TITLE : WS* KN_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    KN_EXAMPLES_TITLE : WS* KN_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    KN_SCENARIO_TITLE : WS* KN_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    KN_SCENARIO_OUTLINE_TITLE : WS* KN_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    KN_RULE_TITLE : WS* KN_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        KN_GIVEN_STEP : WS* KN_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	KN_WHEN_STEP : WS* KN_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	KN_THEN_STEP : WS* KN_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	KN_AND_STEP : WS* KN_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	KN_BUT_STEP : WS* KN_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Korean
//한국어
mode KO;
	KO_FEATURE : ( 
	('기능'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    KO_BACKGROUND : (
    
	('배경'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	KO_SCENARIO : (

	('시나리오'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	KO_SCENARIO_OUTLINE : (

	('시나리오 개요'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	KO_EXAMPLES : (

	('예'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	KO_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	KO_GIVEN : (

	('조건'
		| '먼저'
	)   ) -> type(GIVEN_KEYWORD) ;

	KO_WHEN : (

	('만일'
		| '만약'
	)   ) -> type(WHEN_KEYWORD) ;

	KO_THEN : (

	('그러면'
	)   ) -> type(THEN_KEYWORD) ;

	KO_AND : (

	('그리고'
	)   ) -> type(AND_KEYWORD) ;

	KO_BUT : (

	('하지만'
		| '단'
	)   ) -> type(BUT_KEYWORD) ;

	KO_STARTING_STEP_KEYWORD : (

                KO_GIVEN
		| KO_WHEN
		| KO_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	KO_ALTERNATIVE_STEP_KEYWORD : (

                KO_AND
		| KO_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    KO_FEATURE_TITLE : WS* KO_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    KO_BACKGROUND_TITLE : WS* KO_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    KO_EXAMPLES_TITLE : WS* KO_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    KO_SCENARIO_TITLE : WS* KO_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    KO_SCENARIO_OUTLINE_TITLE : WS* KO_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    KO_RULE_TITLE : WS* KO_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        KO_GIVEN_STEP : WS* KO_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	KO_WHEN_STEP : WS* KO_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	KO_THEN_STEP : WS* KO_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	KO_AND_STEP : WS* KO_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	KO_BUT_STEP : WS* KO_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Lithuanian
//lietuvių kalba
mode LT;
	LT_FEATURE : ( 
	('Savybė'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    LT_BACKGROUND : (
    
	('Kontekstas'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	LT_SCENARIO : (

	('Pavyzdys'
		| 'Scenarijus'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	LT_SCENARIO_OUTLINE : (

	('Scenarijaus šablonas'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	LT_EXAMPLES : (

	('Pavyzdžiai'
		| 'Scenarijai'
		| 'Variantai'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	LT_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	LT_GIVEN : (

	('Duota '
	)   ) -> type(GIVEN_KEYWORD) ;

	LT_WHEN : (

	('Kai '
	)   ) -> type(WHEN_KEYWORD) ;

	LT_THEN : (

	('Tada '
	)   ) -> type(THEN_KEYWORD) ;

	LT_AND : (

	('Ir '
	)   ) -> type(AND_KEYWORD) ;

	LT_BUT : (

	('Bet '
	)   ) -> type(BUT_KEYWORD) ;

	LT_STARTING_STEP_KEYWORD : (

                LT_GIVEN
		| LT_WHEN
		| LT_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	LT_ALTERNATIVE_STEP_KEYWORD : (

                LT_AND
		| LT_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    LT_FEATURE_TITLE : WS* LT_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    LT_BACKGROUND_TITLE : WS* LT_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    LT_EXAMPLES_TITLE : WS* LT_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    LT_SCENARIO_TITLE : WS* LT_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    LT_SCENARIO_OUTLINE_TITLE : WS* LT_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    LT_RULE_TITLE : WS* LT_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        LT_GIVEN_STEP : WS* LT_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	LT_WHEN_STEP : WS* LT_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	LT_THEN_STEP : WS* LT_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	LT_AND_STEP : WS* LT_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	LT_BUT_STEP : WS* LT_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Luxemburgish
//Lëtzebuergesch
mode LU;
	LU_FEATURE : ( 
	('Funktionalitéit'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    LU_BACKGROUND : (
    
	('Hannergrond'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	LU_SCENARIO : (

	('Beispill'
		| 'Szenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	LU_SCENARIO_OUTLINE : (

	('Plang vum Szenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	LU_EXAMPLES : (

	('Beispiller'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	LU_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	LU_GIVEN : (

	('ugeholl '
	)   ) -> type(GIVEN_KEYWORD) ;

	LU_WHEN : (

	('wann '
	)   ) -> type(WHEN_KEYWORD) ;

	LU_THEN : (

	('dann '
	)   ) -> type(THEN_KEYWORD) ;

	LU_AND : (

	('an '
		| 'a '
	)   ) -> type(AND_KEYWORD) ;

	LU_BUT : (

	('awer '
		| 'mä '
	)   ) -> type(BUT_KEYWORD) ;

	LU_STARTING_STEP_KEYWORD : (

                LU_GIVEN
		| LU_WHEN
		| LU_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	LU_ALTERNATIVE_STEP_KEYWORD : (

                LU_AND
		| LU_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    LU_FEATURE_TITLE : WS* LU_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    LU_BACKGROUND_TITLE : WS* LU_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    LU_EXAMPLES_TITLE : WS* LU_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    LU_SCENARIO_TITLE : WS* LU_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    LU_SCENARIO_OUTLINE_TITLE : WS* LU_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    LU_RULE_TITLE : WS* LU_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        LU_GIVEN_STEP : WS* LU_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	LU_WHEN_STEP : WS* LU_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	LU_THEN_STEP : WS* LU_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	LU_AND_STEP : WS* LU_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	LU_BUT_STEP : WS* LU_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Latvian
//latviešu
mode LV;
	LV_FEATURE : ( 
	('Funkcionalitāte'
		| 'Fīča'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    LV_BACKGROUND : (
    
	('Konteksts'
		| 'Situācija'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	LV_SCENARIO : (

	('Piemērs'
		| 'Scenārijs'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	LV_SCENARIO_OUTLINE : (

	('Scenārijs pēc parauga'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	LV_EXAMPLES : (

	('Piemēri'
		| 'Paraugs'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	LV_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	LV_GIVEN : (

	('Kad '
	)   ) -> type(GIVEN_KEYWORD) ;

	LV_WHEN : (

	('Ja '
	)   ) -> type(WHEN_KEYWORD) ;

	LV_THEN : (

	('Tad '
	)   ) -> type(THEN_KEYWORD) ;

	LV_AND : (

	('Un '
	)   ) -> type(AND_KEYWORD) ;

	LV_BUT : (

	('Bet '
	)   ) -> type(BUT_KEYWORD) ;

	LV_STARTING_STEP_KEYWORD : (

                LV_GIVEN
		| LV_WHEN
		| LV_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	LV_ALTERNATIVE_STEP_KEYWORD : (

                LV_AND
		| LV_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    LV_FEATURE_TITLE : WS* LV_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    LV_BACKGROUND_TITLE : WS* LV_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    LV_EXAMPLES_TITLE : WS* LV_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    LV_SCENARIO_TITLE : WS* LV_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    LV_SCENARIO_OUTLINE_TITLE : WS* LV_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    LV_RULE_TITLE : WS* LV_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        LV_GIVEN_STEP : WS* LV_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	LV_WHEN_STEP : WS* LV_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	LV_THEN_STEP : WS* LV_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	LV_AND_STEP : WS* LV_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	LV_BUT_STEP : WS* LV_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Macedonian
//Македонски
mode MK_CYRL;
	MK_CYRL_FEATURE : ( 
	('Функционалност'
		| 'Бизнис потреба'
		| 'Можност'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    MK_CYRL_BACKGROUND : (
    
	('Контекст'
		| 'Содржина'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	MK_CYRL_SCENARIO : (

	('Пример'
		| 'Сценарио'
		| 'На пример'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	MK_CYRL_SCENARIO_OUTLINE : (

	('Преглед на сценарија'
		| 'Скица'
		| 'Концепт'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	MK_CYRL_EXAMPLES : (

	('Примери'
		| 'Сценарија'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	MK_CYRL_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	MK_CYRL_GIVEN : (

	('Дадено '
		| 'Дадена '
	)   ) -> type(GIVEN_KEYWORD) ;

	MK_CYRL_WHEN : (

	('Кога '
	)   ) -> type(WHEN_KEYWORD) ;

	MK_CYRL_THEN : (

	('Тогаш '
	)   ) -> type(THEN_KEYWORD) ;

	MK_CYRL_AND : (

	('И '
	)   ) -> type(AND_KEYWORD) ;

	MK_CYRL_BUT : (

	('Но '
	)   ) -> type(BUT_KEYWORD) ;

	MK_CYRL_STARTING_STEP_KEYWORD : (

                MK_CYRL_GIVEN
		| MK_CYRL_WHEN
		| MK_CYRL_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	MK_CYRL_ALTERNATIVE_STEP_KEYWORD : (

                MK_CYRL_AND
		| MK_CYRL_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    MK_CYRL_FEATURE_TITLE : WS* MK_CYRL_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    MK_CYRL_BACKGROUND_TITLE : WS* MK_CYRL_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    MK_CYRL_EXAMPLES_TITLE : WS* MK_CYRL_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    MK_CYRL_SCENARIO_TITLE : WS* MK_CYRL_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    MK_CYRL_SCENARIO_OUTLINE_TITLE : WS* MK_CYRL_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    MK_CYRL_RULE_TITLE : WS* MK_CYRL_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        MK_CYRL_GIVEN_STEP : WS* MK_CYRL_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	MK_CYRL_WHEN_STEP : WS* MK_CYRL_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	MK_CYRL_THEN_STEP : WS* MK_CYRL_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	MK_CYRL_AND_STEP : WS* MK_CYRL_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	MK_CYRL_BUT_STEP : WS* MK_CYRL_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Macedonian (Latin)
//Makedonski (Latinica)
mode MK_LATN;
	MK_LATN_FEATURE : ( 
	('Funkcionalnost'
		| 'Biznis potreba'
		| 'Mozhnost'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    MK_LATN_BACKGROUND : (
    
	('Kontekst'
		| 'Sodrzhina'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	MK_LATN_SCENARIO : (

	('Scenario'
		| 'Na primer'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	MK_LATN_SCENARIO_OUTLINE : (

	('Pregled na scenarija'
		| 'Skica'
		| 'Koncept'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	MK_LATN_EXAMPLES : (

	('Primeri'
		| 'Scenaria'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	MK_LATN_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	MK_LATN_GIVEN : (

	('Dadeno '
		| 'Dadena '
	)   ) -> type(GIVEN_KEYWORD) ;

	MK_LATN_WHEN : (

	('Koga '
	)   ) -> type(WHEN_KEYWORD) ;

	MK_LATN_THEN : (

	('Togash '
	)   ) -> type(THEN_KEYWORD) ;

	MK_LATN_AND : (

	('I '
	)   ) -> type(AND_KEYWORD) ;

	MK_LATN_BUT : (

	('No '
	)   ) -> type(BUT_KEYWORD) ;

	MK_LATN_STARTING_STEP_KEYWORD : (

                MK_LATN_GIVEN
		| MK_LATN_WHEN
		| MK_LATN_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	MK_LATN_ALTERNATIVE_STEP_KEYWORD : (

                MK_LATN_AND
		| MK_LATN_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    MK_LATN_FEATURE_TITLE : WS* MK_LATN_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    MK_LATN_BACKGROUND_TITLE : WS* MK_LATN_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    MK_LATN_EXAMPLES_TITLE : WS* MK_LATN_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    MK_LATN_SCENARIO_TITLE : WS* MK_LATN_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    MK_LATN_SCENARIO_OUTLINE_TITLE : WS* MK_LATN_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    MK_LATN_RULE_TITLE : WS* MK_LATN_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        MK_LATN_GIVEN_STEP : WS* MK_LATN_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	MK_LATN_WHEN_STEP : WS* MK_LATN_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	MK_LATN_THEN_STEP : WS* MK_LATN_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	MK_LATN_AND_STEP : WS* MK_LATN_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	MK_LATN_BUT_STEP : WS* MK_LATN_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Mongolian
//монгол
mode MN;
	MN_FEATURE : ( 
	('Функц'
		| 'Функционал'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    MN_BACKGROUND : (
    
	('Агуулга'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	MN_SCENARIO : (

	('Сценар'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	MN_SCENARIO_OUTLINE : (

	('Сценарын төлөвлөгөө'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	MN_EXAMPLES : (

	('Тухайлбал'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	MN_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	MN_GIVEN : (

	('Өгөгдсөн нь '
		| 'Анх '
	)   ) -> type(GIVEN_KEYWORD) ;

	MN_WHEN : (

	('Хэрэв '
	)   ) -> type(WHEN_KEYWORD) ;

	MN_THEN : (

	('Тэгэхэд '
		| 'Үүний дараа '
	)   ) -> type(THEN_KEYWORD) ;

	MN_AND : (

	('Мөн '
		| 'Тэгээд '
	)   ) -> type(AND_KEYWORD) ;

	MN_BUT : (

	('Гэхдээ '
		| 'Харин '
	)   ) -> type(BUT_KEYWORD) ;

	MN_STARTING_STEP_KEYWORD : (

                MN_GIVEN
		| MN_WHEN
		| MN_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	MN_ALTERNATIVE_STEP_KEYWORD : (

                MN_AND
		| MN_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    MN_FEATURE_TITLE : WS* MN_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    MN_BACKGROUND_TITLE : WS* MN_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    MN_EXAMPLES_TITLE : WS* MN_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    MN_SCENARIO_TITLE : WS* MN_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    MN_SCENARIO_OUTLINE_TITLE : WS* MN_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    MN_RULE_TITLE : WS* MN_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        MN_GIVEN_STEP : WS* MN_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	MN_WHEN_STEP : WS* MN_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	MN_THEN_STEP : WS* MN_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	MN_AND_STEP : WS* MN_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	MN_BUT_STEP : WS* MN_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Nepali
//नेपाली
mode NE;
	NE_FEATURE : ( 
	('सुविधा'
		| 'विशेषता'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    NE_BACKGROUND : (
    
	('पृष्ठभूमी'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	NE_SCENARIO : (

	('परिदृश्य'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	NE_SCENARIO_OUTLINE : (

	('परिदृश्य रूपरेखा'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	NE_EXAMPLES : (

	('उदाहरण'
		| 'उदाहरणहरु'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	NE_RULE : (

	('नियम'
	) ':'  ) -> type(RULE_KEYWORD) ;

	NE_GIVEN : (

	('दिइएको '
		| 'दिएको '
		| 'यदि '
	)   ) -> type(GIVEN_KEYWORD) ;

	NE_WHEN : (

	('जब '
	)   ) -> type(WHEN_KEYWORD) ;

	NE_THEN : (

	('त्यसपछि '
		| 'अनी '
	)   ) -> type(THEN_KEYWORD) ;

	NE_AND : (

	('र '
		| 'अनी '
	)   ) -> type(AND_KEYWORD) ;

	NE_BUT : (

	('तर '
	)   ) -> type(BUT_KEYWORD) ;

	NE_STARTING_STEP_KEYWORD : (

                NE_GIVEN
		| NE_WHEN
		| NE_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	NE_ALTERNATIVE_STEP_KEYWORD : (

                NE_AND
		| NE_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    NE_FEATURE_TITLE : WS* NE_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    NE_BACKGROUND_TITLE : WS* NE_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    NE_EXAMPLES_TITLE : WS* NE_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    NE_SCENARIO_TITLE : WS* NE_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    NE_SCENARIO_OUTLINE_TITLE : WS* NE_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    NE_RULE_TITLE : WS* NE_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        NE_GIVEN_STEP : WS* NE_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	NE_WHEN_STEP : WS* NE_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	NE_THEN_STEP : WS* NE_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	NE_AND_STEP : WS* NE_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	NE_BUT_STEP : WS* NE_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Dutch
//Nederlands
mode NL;
	NL_FEATURE : ( 
	('Functionaliteit'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    NL_BACKGROUND : (
    
	('Achtergrond'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	NL_SCENARIO : (

	('Voorbeeld'
		| 'Scenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	NL_SCENARIO_OUTLINE : (

	('Abstract Scenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	NL_EXAMPLES : (

	('Voorbeelden'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	NL_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	NL_GIVEN : (

	('Gegeven '
		| 'Stel '
	)   ) -> type(GIVEN_KEYWORD) ;

	NL_WHEN : (

	('Als '
		| 'Wanneer '
	)   ) -> type(WHEN_KEYWORD) ;

	NL_THEN : (

	('Dan '
	)   ) -> type(THEN_KEYWORD) ;

	NL_AND : (

	('En '
	)   ) -> type(AND_KEYWORD) ;

	NL_BUT : (

	('Maar '
	)   ) -> type(BUT_KEYWORD) ;

	NL_STARTING_STEP_KEYWORD : (

                NL_GIVEN
		| NL_WHEN
		| NL_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	NL_ALTERNATIVE_STEP_KEYWORD : (

                NL_AND
		| NL_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    NL_FEATURE_TITLE : WS* NL_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    NL_BACKGROUND_TITLE : WS* NL_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    NL_EXAMPLES_TITLE : WS* NL_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    NL_SCENARIO_TITLE : WS* NL_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    NL_SCENARIO_OUTLINE_TITLE : WS* NL_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    NL_RULE_TITLE : WS* NL_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        NL_GIVEN_STEP : WS* NL_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	NL_WHEN_STEP : WS* NL_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	NL_THEN_STEP : WS* NL_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	NL_AND_STEP : WS* NL_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	NL_BUT_STEP : WS* NL_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Norwegian
//norsk
mode NO;
	NO_FEATURE : ( 
	('Egenskap'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    NO_BACKGROUND : (
    
	('Bakgrunn'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	NO_SCENARIO : (

	('Eksempel'
		| 'Scenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	NO_SCENARIO_OUTLINE : (

	('Scenariomal'
		| 'Abstrakt Scenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	NO_EXAMPLES : (

	('Eksempler'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	NO_RULE : (

	('Regel'
	) ':'  ) -> type(RULE_KEYWORD) ;

	NO_GIVEN : (

	('Gitt '
	)   ) -> type(GIVEN_KEYWORD) ;

	NO_WHEN : (

	('Når '
	)   ) -> type(WHEN_KEYWORD) ;

	NO_THEN : (

	('Så '
	)   ) -> type(THEN_KEYWORD) ;

	NO_AND : (

	('Og '
	)   ) -> type(AND_KEYWORD) ;

	NO_BUT : (

	('Men '
	)   ) -> type(BUT_KEYWORD) ;

	NO_STARTING_STEP_KEYWORD : (

                NO_GIVEN
		| NO_WHEN
		| NO_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	NO_ALTERNATIVE_STEP_KEYWORD : (

                NO_AND
		| NO_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    NO_FEATURE_TITLE : WS* NO_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    NO_BACKGROUND_TITLE : WS* NO_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    NO_EXAMPLES_TITLE : WS* NO_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    NO_SCENARIO_TITLE : WS* NO_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    NO_SCENARIO_OUTLINE_TITLE : WS* NO_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    NO_RULE_TITLE : WS* NO_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        NO_GIVEN_STEP : WS* NO_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	NO_WHEN_STEP : WS* NO_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	NO_THEN_STEP : WS* NO_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	NO_AND_STEP : WS* NO_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	NO_BUT_STEP : WS* NO_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Panjabi
//ਪੰਜਾਬੀ
mode PA;
	PA_FEATURE : ( 
	('ਖਾਸੀਅਤ'
		| 'ਮੁਹਾਂਦਰਾ'
		| 'ਨਕਸ਼ ਨੁਹਾਰ'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    PA_BACKGROUND : (
    
	('ਪਿਛੋਕੜ'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	PA_SCENARIO : (

	('ਉਦਾਹਰਨ'
		| 'ਪਟਕਥਾ'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	PA_SCENARIO_OUTLINE : (

	('ਪਟਕਥਾ ਢਾਂਚਾ'
		| 'ਪਟਕਥਾ ਰੂਪ ਰੇਖਾ'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	PA_EXAMPLES : (

	('ਉਦਾਹਰਨਾਂ'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	PA_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	PA_GIVEN : (

	('ਜੇਕਰ '
		| 'ਜਿਵੇਂ ਕਿ '
	)   ) -> type(GIVEN_KEYWORD) ;

	PA_WHEN : (

	('ਜਦੋਂ '
	)   ) -> type(WHEN_KEYWORD) ;

	PA_THEN : (

	('ਤਦ '
	)   ) -> type(THEN_KEYWORD) ;

	PA_AND : (

	('ਅਤੇ '
	)   ) -> type(AND_KEYWORD) ;

	PA_BUT : (

	('ਪਰ '
	)   ) -> type(BUT_KEYWORD) ;

	PA_STARTING_STEP_KEYWORD : (

                PA_GIVEN
		| PA_WHEN
		| PA_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	PA_ALTERNATIVE_STEP_KEYWORD : (

                PA_AND
		| PA_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    PA_FEATURE_TITLE : WS* PA_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    PA_BACKGROUND_TITLE : WS* PA_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    PA_EXAMPLES_TITLE : WS* PA_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    PA_SCENARIO_TITLE : WS* PA_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    PA_SCENARIO_OUTLINE_TITLE : WS* PA_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    PA_RULE_TITLE : WS* PA_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        PA_GIVEN_STEP : WS* PA_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	PA_WHEN_STEP : WS* PA_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	PA_THEN_STEP : WS* PA_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	PA_AND_STEP : WS* PA_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	PA_BUT_STEP : WS* PA_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Polish
//polski
mode PL;
	PL_FEATURE : ( 
	('Właściwość'
		| 'Funkcja'
		| 'Aspekt'
		| 'Potrzeba biznesowa'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    PL_BACKGROUND : (
    
	('Założenia'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	PL_SCENARIO : (

	('Przykład'
		| 'Scenariusz'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	PL_SCENARIO_OUTLINE : (

	('Szablon scenariusza'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	PL_EXAMPLES : (

	('Przykłady'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	PL_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	PL_GIVEN : (

	('Zakładając '
		| 'Mając '
		| 'Zakładając, że '
	)   ) -> type(GIVEN_KEYWORD) ;

	PL_WHEN : (

	('Jeżeli '
		| 'Jeśli '
		| 'Gdy '
		| 'Kiedy '
	)   ) -> type(WHEN_KEYWORD) ;

	PL_THEN : (

	('Wtedy '
	)   ) -> type(THEN_KEYWORD) ;

	PL_AND : (

	('Oraz '
		| 'I '
	)   ) -> type(AND_KEYWORD) ;

	PL_BUT : (

	('Ale '
	)   ) -> type(BUT_KEYWORD) ;

	PL_STARTING_STEP_KEYWORD : (

                PL_GIVEN
		| PL_WHEN
		| PL_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	PL_ALTERNATIVE_STEP_KEYWORD : (

                PL_AND
		| PL_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    PL_FEATURE_TITLE : WS* PL_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    PL_BACKGROUND_TITLE : WS* PL_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    PL_EXAMPLES_TITLE : WS* PL_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    PL_SCENARIO_TITLE : WS* PL_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    PL_SCENARIO_OUTLINE_TITLE : WS* PL_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    PL_RULE_TITLE : WS* PL_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        PL_GIVEN_STEP : WS* PL_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	PL_WHEN_STEP : WS* PL_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	PL_THEN_STEP : WS* PL_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	PL_AND_STEP : WS* PL_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	PL_BUT_STEP : WS* PL_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Portuguese
//português
mode PT;
	PT_FEATURE : ( 
	('Funcionalidade'
		| 'Característica'
		| 'Caracteristica'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    PT_BACKGROUND : (
    
	('Contexto'
		| 'Cenário de Fundo'
		| 'Cenario de Fundo'
		| 'Fundo'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	PT_SCENARIO : (

	('Exemplo'
		| 'Cenário'
		| 'Cenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	PT_SCENARIO_OUTLINE : (

	('Esquema do Cenário'
		| 'Esquema do Cenario'
		| 'Delineação do Cenário'
		| 'Delineacao do Cenario'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	PT_EXAMPLES : (

	('Exemplos'
		| 'Cenários'
		| 'Cenarios'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	PT_RULE : (

	('Regra'
	) ':'  ) -> type(RULE_KEYWORD) ;

	PT_GIVEN : (

	('Dado '
		| 'Dada '
		| 'Dados '
		| 'Dadas '
	)   ) -> type(GIVEN_KEYWORD) ;

	PT_WHEN : (

	('Quando '
	)   ) -> type(WHEN_KEYWORD) ;

	PT_THEN : (

	('Então '
		| 'Entao '
	)   ) -> type(THEN_KEYWORD) ;

	PT_AND : (

	('E '
	)   ) -> type(AND_KEYWORD) ;

	PT_BUT : (

	('Mas '
	)   ) -> type(BUT_KEYWORD) ;

	PT_STARTING_STEP_KEYWORD : (

                PT_GIVEN
		| PT_WHEN
		| PT_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	PT_ALTERNATIVE_STEP_KEYWORD : (

                PT_AND
		| PT_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    PT_FEATURE_TITLE : WS* PT_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    PT_BACKGROUND_TITLE : WS* PT_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    PT_EXAMPLES_TITLE : WS* PT_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    PT_SCENARIO_TITLE : WS* PT_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    PT_SCENARIO_OUTLINE_TITLE : WS* PT_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    PT_RULE_TITLE : WS* PT_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        PT_GIVEN_STEP : WS* PT_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	PT_WHEN_STEP : WS* PT_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	PT_THEN_STEP : WS* PT_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	PT_AND_STEP : WS* PT_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	PT_BUT_STEP : WS* PT_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Romanian
//română
mode RO;
	RO_FEATURE : ( 
	('Functionalitate'
		| 'Funcționalitate'
		| 'Funcţionalitate'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    RO_BACKGROUND : (
    
	('Context'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	RO_SCENARIO : (

	('Exemplu'
		| 'Scenariu'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	RO_SCENARIO_OUTLINE : (

	('Structura scenariu'
		| 'Structură scenariu'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	RO_EXAMPLES : (

	('Exemple'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	RO_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	RO_GIVEN : (

	('Date fiind '
		| 'Dat fiind '
		| 'Dată fiind'
		| 'Dati fiind '
		| 'Dați fiind '
		| 'Daţi fiind '
	)   ) -> type(GIVEN_KEYWORD) ;

	RO_WHEN : (

	('Cand '
		| 'Când '
	)   ) -> type(WHEN_KEYWORD) ;

	RO_THEN : (

	('Atunci '
	)   ) -> type(THEN_KEYWORD) ;

	RO_AND : (

	('Si '
		| 'Și '
		| 'Şi '
	)   ) -> type(AND_KEYWORD) ;

	RO_BUT : (

	('Dar '
	)   ) -> type(BUT_KEYWORD) ;

	RO_STARTING_STEP_KEYWORD : (

                RO_GIVEN
		| RO_WHEN
		| RO_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	RO_ALTERNATIVE_STEP_KEYWORD : (

                RO_AND
		| RO_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    RO_FEATURE_TITLE : WS* RO_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    RO_BACKGROUND_TITLE : WS* RO_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    RO_EXAMPLES_TITLE : WS* RO_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    RO_SCENARIO_TITLE : WS* RO_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    RO_SCENARIO_OUTLINE_TITLE : WS* RO_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    RO_RULE_TITLE : WS* RO_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        RO_GIVEN_STEP : WS* RO_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	RO_WHEN_STEP : WS* RO_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	RO_THEN_STEP : WS* RO_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	RO_AND_STEP : WS* RO_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	RO_BUT_STEP : WS* RO_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Russian
//русский
mode RU;
	RU_FEATURE : ( 
	('Функция'
		| 'Функциональность'
		| 'Функционал'
		| 'Свойство'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    RU_BACKGROUND : (
    
	('Предыстория'
		| 'Контекст'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	RU_SCENARIO : (

	('Пример'
		| 'Сценарий'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	RU_SCENARIO_OUTLINE : (

	('Структура сценария'
		| 'Шаблон сценария'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	RU_EXAMPLES : (

	('Примеры'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	RU_RULE : (

	('Правило'
	) ':'  ) -> type(RULE_KEYWORD) ;

	RU_GIVEN : (

	('Допустим '
		| 'Дано '
		| 'Пусть '
	)   ) -> type(GIVEN_KEYWORD) ;

	RU_WHEN : (

	('Когда '
		| 'Если '
	)   ) -> type(WHEN_KEYWORD) ;

	RU_THEN : (

	('То '
		| 'Затем '
		| 'Тогда '
	)   ) -> type(THEN_KEYWORD) ;

	RU_AND : (

	('И '
		| 'К тому же '
		| 'Также '
	)   ) -> type(AND_KEYWORD) ;

	RU_BUT : (

	('Но '
		| 'А '
		| 'Иначе '
	)   ) -> type(BUT_KEYWORD) ;

	RU_STARTING_STEP_KEYWORD : (

                RU_GIVEN
		| RU_WHEN
		| RU_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	RU_ALTERNATIVE_STEP_KEYWORD : (

                RU_AND
		| RU_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    RU_FEATURE_TITLE : WS* RU_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    RU_BACKGROUND_TITLE : WS* RU_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    RU_EXAMPLES_TITLE : WS* RU_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    RU_SCENARIO_TITLE : WS* RU_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    RU_SCENARIO_OUTLINE_TITLE : WS* RU_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    RU_RULE_TITLE : WS* RU_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        RU_GIVEN_STEP : WS* RU_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	RU_WHEN_STEP : WS* RU_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	RU_THEN_STEP : WS* RU_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	RU_AND_STEP : WS* RU_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	RU_BUT_STEP : WS* RU_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Slovak
//Slovensky
mode SK;
	SK_FEATURE : ( 
	('Požiadavka'
		| 'Funkcia'
		| 'Vlastnosť'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    SK_BACKGROUND : (
    
	('Pozadie'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	SK_SCENARIO : (

	('Príklad'
		| 'Scenár'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	SK_SCENARIO_OUTLINE : (

	('Náčrt Scenáru'
		| 'Náčrt Scenára'
		| 'Osnova Scenára'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	SK_EXAMPLES : (

	('Príklady'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	SK_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	SK_GIVEN : (

	('Pokiaľ '
		| 'Za predpokladu '
	)   ) -> type(GIVEN_KEYWORD) ;

	SK_WHEN : (

	('Keď '
		| 'Ak '
	)   ) -> type(WHEN_KEYWORD) ;

	SK_THEN : (

	('Tak '
		| 'Potom '
	)   ) -> type(THEN_KEYWORD) ;

	SK_AND : (

	('A '
		| 'A tiež '
		| 'A taktiež '
		| 'A zároveň '
	)   ) -> type(AND_KEYWORD) ;

	SK_BUT : (

	('Ale '
	)   ) -> type(BUT_KEYWORD) ;

	SK_STARTING_STEP_KEYWORD : (

                SK_GIVEN
		| SK_WHEN
		| SK_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	SK_ALTERNATIVE_STEP_KEYWORD : (

                SK_AND
		| SK_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    SK_FEATURE_TITLE : WS* SK_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    SK_BACKGROUND_TITLE : WS* SK_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    SK_EXAMPLES_TITLE : WS* SK_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    SK_SCENARIO_TITLE : WS* SK_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    SK_SCENARIO_OUTLINE_TITLE : WS* SK_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    SK_RULE_TITLE : WS* SK_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        SK_GIVEN_STEP : WS* SK_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	SK_WHEN_STEP : WS* SK_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	SK_THEN_STEP : WS* SK_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	SK_AND_STEP : WS* SK_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	SK_BUT_STEP : WS* SK_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Slovenian
//Slovenski
mode SL;
	SL_FEATURE : ( 
	('Funkcionalnost'
		| 'Funkcija'
		| 'Možnosti'
		| 'Moznosti'
		| 'Lastnost'
		| 'Značilnost'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    SL_BACKGROUND : (
    
	('Kontekst'
		| 'Osnova'
		| 'Ozadje'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	SL_SCENARIO : (

	('Primer'
		| 'Scenarij'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	SL_SCENARIO_OUTLINE : (

	('Struktura scenarija'
		| 'Skica'
		| 'Koncept'
		| 'Oris scenarija'
		| 'Osnutek'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	SL_EXAMPLES : (

	('Primeri'
		| 'Scenariji'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	SL_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	SL_GIVEN : (

	('Dano '
		| 'Podano '
		| 'Zaradi '
		| 'Privzeto '
	)   ) -> type(GIVEN_KEYWORD) ;

	SL_WHEN : (

	('Ko '
		| 'Ce '
		| 'Če '
		| 'Kadar '
	)   ) -> type(WHEN_KEYWORD) ;

	SL_THEN : (

	('Nato '
		| 'Potem '
		| 'Takrat '
	)   ) -> type(THEN_KEYWORD) ;

	SL_AND : (

	('In '
		| 'Ter '
	)   ) -> type(AND_KEYWORD) ;

	SL_BUT : (

	('Toda '
		| 'Ampak '
		| 'Vendar '
	)   ) -> type(BUT_KEYWORD) ;

	SL_STARTING_STEP_KEYWORD : (

                SL_GIVEN
		| SL_WHEN
		| SL_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	SL_ALTERNATIVE_STEP_KEYWORD : (

                SL_AND
		| SL_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    SL_FEATURE_TITLE : WS* SL_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    SL_BACKGROUND_TITLE : WS* SL_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    SL_EXAMPLES_TITLE : WS* SL_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    SL_SCENARIO_TITLE : WS* SL_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    SL_SCENARIO_OUTLINE_TITLE : WS* SL_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    SL_RULE_TITLE : WS* SL_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        SL_GIVEN_STEP : WS* SL_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	SL_WHEN_STEP : WS* SL_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	SL_THEN_STEP : WS* SL_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	SL_AND_STEP : WS* SL_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	SL_BUT_STEP : WS* SL_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Serbian
//Српски
mode SR_CYRL;
	SR_CYRL_FEATURE : ( 
	('Функционалност'
		| 'Могућност'
		| 'Особина'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    SR_CYRL_BACKGROUND : (
    
	('Контекст'
		| 'Основа'
		| 'Позадина'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	SR_CYRL_SCENARIO : (

	('Пример'
		| 'Сценарио'
		| 'Пример'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	SR_CYRL_SCENARIO_OUTLINE : (

	('Структура сценарија'
		| 'Скица'
		| 'Концепт'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	SR_CYRL_EXAMPLES : (

	('Примери'
		| 'Сценарији'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	SR_CYRL_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	SR_CYRL_GIVEN : (

	('За дато '
		| 'За дате '
		| 'За дати '
	)   ) -> type(GIVEN_KEYWORD) ;

	SR_CYRL_WHEN : (

	('Када '
		| 'Кад '
	)   ) -> type(WHEN_KEYWORD) ;

	SR_CYRL_THEN : (

	('Онда '
	)   ) -> type(THEN_KEYWORD) ;

	SR_CYRL_AND : (

	('И '
	)   ) -> type(AND_KEYWORD) ;

	SR_CYRL_BUT : (

	('Али '
	)   ) -> type(BUT_KEYWORD) ;

	SR_CYRL_STARTING_STEP_KEYWORD : (

                SR_CYRL_GIVEN
		| SR_CYRL_WHEN
		| SR_CYRL_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	SR_CYRL_ALTERNATIVE_STEP_KEYWORD : (

                SR_CYRL_AND
		| SR_CYRL_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    SR_CYRL_FEATURE_TITLE : WS* SR_CYRL_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    SR_CYRL_BACKGROUND_TITLE : WS* SR_CYRL_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    SR_CYRL_EXAMPLES_TITLE : WS* SR_CYRL_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    SR_CYRL_SCENARIO_TITLE : WS* SR_CYRL_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    SR_CYRL_SCENARIO_OUTLINE_TITLE : WS* SR_CYRL_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    SR_CYRL_RULE_TITLE : WS* SR_CYRL_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        SR_CYRL_GIVEN_STEP : WS* SR_CYRL_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	SR_CYRL_WHEN_STEP : WS* SR_CYRL_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	SR_CYRL_THEN_STEP : WS* SR_CYRL_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	SR_CYRL_AND_STEP : WS* SR_CYRL_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	SR_CYRL_BUT_STEP : WS* SR_CYRL_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Serbian (Latin)
//Srpski (Latinica)
mode SR_LATN;
	SR_LATN_FEATURE : ( 
	('Funkcionalnost'
		| 'Mogućnost'
		| 'Mogucnost'
		| 'Osobina'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    SR_LATN_BACKGROUND : (
    
	('Kontekst'
		| 'Osnova'
		| 'Pozadina'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	SR_LATN_SCENARIO : (

	('Scenario'
		| 'Primer'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	SR_LATN_SCENARIO_OUTLINE : (

	('Struktura scenarija'
		| 'Skica'
		| 'Koncept'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	SR_LATN_EXAMPLES : (

	('Primeri'
		| 'Scenariji'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	SR_LATN_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	SR_LATN_GIVEN : (

	('Za dato '
		| 'Za date '
		| 'Za dati '
	)   ) -> type(GIVEN_KEYWORD) ;

	SR_LATN_WHEN : (

	('Kada '
		| 'Kad '
	)   ) -> type(WHEN_KEYWORD) ;

	SR_LATN_THEN : (

	('Onda '
	)   ) -> type(THEN_KEYWORD) ;

	SR_LATN_AND : (

	('I '
	)   ) -> type(AND_KEYWORD) ;

	SR_LATN_BUT : (

	('Ali '
	)   ) -> type(BUT_KEYWORD) ;

	SR_LATN_STARTING_STEP_KEYWORD : (

                SR_LATN_GIVEN
		| SR_LATN_WHEN
		| SR_LATN_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	SR_LATN_ALTERNATIVE_STEP_KEYWORD : (

                SR_LATN_AND
		| SR_LATN_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    SR_LATN_FEATURE_TITLE : WS* SR_LATN_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    SR_LATN_BACKGROUND_TITLE : WS* SR_LATN_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    SR_LATN_EXAMPLES_TITLE : WS* SR_LATN_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    SR_LATN_SCENARIO_TITLE : WS* SR_LATN_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    SR_LATN_SCENARIO_OUTLINE_TITLE : WS* SR_LATN_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    SR_LATN_RULE_TITLE : WS* SR_LATN_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        SR_LATN_GIVEN_STEP : WS* SR_LATN_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	SR_LATN_WHEN_STEP : WS* SR_LATN_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	SR_LATN_THEN_STEP : WS* SR_LATN_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	SR_LATN_AND_STEP : WS* SR_LATN_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	SR_LATN_BUT_STEP : WS* SR_LATN_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Swedish
//Svenska
mode SV;
	SV_FEATURE : ( 
	('Egenskap'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    SV_BACKGROUND : (
    
	('Bakgrund'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	SV_SCENARIO : (

	('Scenario'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	SV_SCENARIO_OUTLINE : (

	('Abstrakt Scenario'
		| 'Scenariomall'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	SV_EXAMPLES : (

	('Exempel'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	SV_RULE : (

	('Regel'
	) ':'  ) -> type(RULE_KEYWORD) ;

	SV_GIVEN : (

	('Givet '
	)   ) -> type(GIVEN_KEYWORD) ;

	SV_WHEN : (

	('När '
	)   ) -> type(WHEN_KEYWORD) ;

	SV_THEN : (

	('Så '
	)   ) -> type(THEN_KEYWORD) ;

	SV_AND : (

	('Och '
	)   ) -> type(AND_KEYWORD) ;

	SV_BUT : (

	('Men '
	)   ) -> type(BUT_KEYWORD) ;

	SV_STARTING_STEP_KEYWORD : (

                SV_GIVEN
		| SV_WHEN
		| SV_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	SV_ALTERNATIVE_STEP_KEYWORD : (

                SV_AND
		| SV_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    SV_FEATURE_TITLE : WS* SV_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    SV_BACKGROUND_TITLE : WS* SV_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    SV_EXAMPLES_TITLE : WS* SV_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    SV_SCENARIO_TITLE : WS* SV_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    SV_SCENARIO_OUTLINE_TITLE : WS* SV_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    SV_RULE_TITLE : WS* SV_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        SV_GIVEN_STEP : WS* SV_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	SV_WHEN_STEP : WS* SV_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	SV_THEN_STEP : WS* SV_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	SV_AND_STEP : WS* SV_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	SV_BUT_STEP : WS* SV_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Tamil
//தமிழ்
mode TA;
	TA_FEATURE : ( 
	('அம்சம்'
		| 'வணிக தேவை'
		| 'திறன்'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    TA_BACKGROUND : (
    
	('பின்னணி'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	TA_SCENARIO : (

	('உதாரணமாக'
		| 'காட்சி'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	TA_SCENARIO_OUTLINE : (

	('காட்சி சுருக்கம்'
		| 'காட்சி வார்ப்புரு'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	TA_EXAMPLES : (

	('எடுத்துக்காட்டுகள்'
		| 'காட்சிகள்'
		| 'நிலைமைகளில்'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	TA_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	TA_GIVEN : (

	('கொடுக்கப்பட்ட '
	)   ) -> type(GIVEN_KEYWORD) ;

	TA_WHEN : (

	('எப்போது '
	)   ) -> type(WHEN_KEYWORD) ;

	TA_THEN : (

	('அப்பொழுது '
	)   ) -> type(THEN_KEYWORD) ;

	TA_AND : (

	('மேலும்  '
		| 'மற்றும் '
	)   ) -> type(AND_KEYWORD) ;

	TA_BUT : (

	('ஆனால்  '
	)   ) -> type(BUT_KEYWORD) ;

	TA_STARTING_STEP_KEYWORD : (

                TA_GIVEN
		| TA_WHEN
		| TA_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	TA_ALTERNATIVE_STEP_KEYWORD : (

                TA_AND
		| TA_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    TA_FEATURE_TITLE : WS* TA_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    TA_BACKGROUND_TITLE : WS* TA_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    TA_EXAMPLES_TITLE : WS* TA_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    TA_SCENARIO_TITLE : WS* TA_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    TA_SCENARIO_OUTLINE_TITLE : WS* TA_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    TA_RULE_TITLE : WS* TA_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        TA_GIVEN_STEP : WS* TA_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	TA_WHEN_STEP : WS* TA_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	TA_THEN_STEP : WS* TA_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	TA_AND_STEP : WS* TA_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	TA_BUT_STEP : WS* TA_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Thai
//ไทย
mode TH;
	TH_FEATURE : ( 
	('โครงหลัก'
		| 'ความต้องการทางธุรกิจ'
		| 'ความสามารถ'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    TH_BACKGROUND : (
    
	('แนวคิด'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	TH_SCENARIO : (

	('เหตุการณ์'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	TH_SCENARIO_OUTLINE : (

	('สรุปเหตุการณ์'
		| 'โครงสร้างของเหตุการณ์'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	TH_EXAMPLES : (

	('ชุดของตัวอย่าง'
		| 'ชุดของเหตุการณ์'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	TH_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	TH_GIVEN : (

	('กำหนดให้ '
	)   ) -> type(GIVEN_KEYWORD) ;

	TH_WHEN : (

	('เมื่อ '
	)   ) -> type(WHEN_KEYWORD) ;

	TH_THEN : (

	('ดังนั้น '
	)   ) -> type(THEN_KEYWORD) ;

	TH_AND : (

	('และ '
	)   ) -> type(AND_KEYWORD) ;

	TH_BUT : (

	('แต่ '
	)   ) -> type(BUT_KEYWORD) ;

	TH_STARTING_STEP_KEYWORD : (

                TH_GIVEN
		| TH_WHEN
		| TH_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	TH_ALTERNATIVE_STEP_KEYWORD : (

                TH_AND
		| TH_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    TH_FEATURE_TITLE : WS* TH_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    TH_BACKGROUND_TITLE : WS* TH_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    TH_EXAMPLES_TITLE : WS* TH_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    TH_SCENARIO_TITLE : WS* TH_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    TH_SCENARIO_OUTLINE_TITLE : WS* TH_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    TH_RULE_TITLE : WS* TH_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        TH_GIVEN_STEP : WS* TH_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	TH_WHEN_STEP : WS* TH_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	TH_THEN_STEP : WS* TH_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	TH_AND_STEP : WS* TH_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	TH_BUT_STEP : WS* TH_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Telugu
//తెలుగు
mode TE;
	TE_FEATURE : ( 
	('గుణము'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    TE_BACKGROUND : (
    
	('నేపథ్యం'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	TE_SCENARIO : (

	('ఉదాహరణ'
		| 'సన్నివేశం'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	TE_SCENARIO_OUTLINE : (

	('కథనం'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	TE_EXAMPLES : (

	('ఉదాహరణలు'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	TE_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	TE_GIVEN : (

	('చెప్పబడినది '
	)   ) -> type(GIVEN_KEYWORD) ;

	TE_WHEN : (

	('ఈ పరిస్థితిలో '
	)   ) -> type(WHEN_KEYWORD) ;

	TE_THEN : (

	('అప్పుడు '
	)   ) -> type(THEN_KEYWORD) ;

	TE_AND : (

	('మరియు '
	)   ) -> type(AND_KEYWORD) ;

	TE_BUT : (

	('కాని '
	)   ) -> type(BUT_KEYWORD) ;

	TE_STARTING_STEP_KEYWORD : (

                TE_GIVEN
		| TE_WHEN
		| TE_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	TE_ALTERNATIVE_STEP_KEYWORD : (

                TE_AND
		| TE_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    TE_FEATURE_TITLE : WS* TE_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    TE_BACKGROUND_TITLE : WS* TE_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    TE_EXAMPLES_TITLE : WS* TE_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    TE_SCENARIO_TITLE : WS* TE_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    TE_SCENARIO_OUTLINE_TITLE : WS* TE_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    TE_RULE_TITLE : WS* TE_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        TE_GIVEN_STEP : WS* TE_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	TE_WHEN_STEP : WS* TE_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	TE_THEN_STEP : WS* TE_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	TE_AND_STEP : WS* TE_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	TE_BUT_STEP : WS* TE_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Klingon
//tlhIngan
mode TLH;
	TLH_FEATURE : ( 
	('Qap'
		| 'Qu\'meH \'ut'
		| 'perbogh'
		| 'poQbogh malja\''
		| 'laH'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    TLH_BACKGROUND : (
    
	('mo\''
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	TLH_SCENARIO : (

	('lut'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	TLH_SCENARIO_OUTLINE : (

	('lut chovnatlh'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	TLH_EXAMPLES : (

	('ghantoH'
		| 'lutmey'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	TLH_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	TLH_GIVEN : (

	('ghu\' noblu\' '
		| 'DaH ghu\' bejlu\' '
	)   ) -> type(GIVEN_KEYWORD) ;

	TLH_WHEN : (

	('qaSDI\' '
	)   ) -> type(WHEN_KEYWORD) ;

	TLH_THEN : (

	('vaj '
	)   ) -> type(THEN_KEYWORD) ;

	TLH_AND : (

	('\'ej '
		| 'latlh '
	)   ) -> type(AND_KEYWORD) ;

	TLH_BUT : (

	('\'ach '
		| '\'a '
	)   ) -> type(BUT_KEYWORD) ;

	TLH_STARTING_STEP_KEYWORD : (

                TLH_GIVEN
		| TLH_WHEN
		| TLH_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	TLH_ALTERNATIVE_STEP_KEYWORD : (

                TLH_AND
		| TLH_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    TLH_FEATURE_TITLE : WS* TLH_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    TLH_BACKGROUND_TITLE : WS* TLH_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    TLH_EXAMPLES_TITLE : WS* TLH_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    TLH_SCENARIO_TITLE : WS* TLH_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    TLH_SCENARIO_OUTLINE_TITLE : WS* TLH_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    TLH_RULE_TITLE : WS* TLH_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        TLH_GIVEN_STEP : WS* TLH_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	TLH_WHEN_STEP : WS* TLH_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	TLH_THEN_STEP : WS* TLH_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	TLH_AND_STEP : WS* TLH_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	TLH_BUT_STEP : WS* TLH_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Turkish
//Türkçe
mode TR;
	TR_FEATURE : ( 
	('Özellik'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    TR_BACKGROUND : (
    
	('Geçmiş'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	TR_SCENARIO : (

	('Örnek'
		| 'Senaryo'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	TR_SCENARIO_OUTLINE : (

	('Senaryo taslağı'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	TR_EXAMPLES : (

	('Örnekler'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	TR_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	TR_GIVEN : (

	('Diyelim ki '
	)   ) -> type(GIVEN_KEYWORD) ;

	TR_WHEN : (

	('Eğer ki '
	)   ) -> type(WHEN_KEYWORD) ;

	TR_THEN : (

	('O zaman '
	)   ) -> type(THEN_KEYWORD) ;

	TR_AND : (

	('Ve '
	)   ) -> type(AND_KEYWORD) ;

	TR_BUT : (

	('Fakat '
		| 'Ama '
	)   ) -> type(BUT_KEYWORD) ;

	TR_STARTING_STEP_KEYWORD : (

                TR_GIVEN
		| TR_WHEN
		| TR_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	TR_ALTERNATIVE_STEP_KEYWORD : (

                TR_AND
		| TR_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    TR_FEATURE_TITLE : WS* TR_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    TR_BACKGROUND_TITLE : WS* TR_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    TR_EXAMPLES_TITLE : WS* TR_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    TR_SCENARIO_TITLE : WS* TR_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    TR_SCENARIO_OUTLINE_TITLE : WS* TR_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    TR_RULE_TITLE : WS* TR_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        TR_GIVEN_STEP : WS* TR_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	TR_WHEN_STEP : WS* TR_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	TR_THEN_STEP : WS* TR_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	TR_AND_STEP : WS* TR_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	TR_BUT_STEP : WS* TR_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Tatar
//Татарча
mode TT;
	TT_FEATURE : ( 
	('Мөмкинлек'
		| 'Үзенчәлеклелек'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    TT_BACKGROUND : (
    
	('Кереш'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	TT_SCENARIO : (

	('Сценарий'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	TT_SCENARIO_OUTLINE : (

	('Сценарийның төзелеше'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	TT_EXAMPLES : (

	('Үрнәкләр'
		| 'Мисаллар'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	TT_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	TT_GIVEN : (

	('Әйтик '
	)   ) -> type(GIVEN_KEYWORD) ;

	TT_WHEN : (

	('Әгәр '
	)   ) -> type(WHEN_KEYWORD) ;

	TT_THEN : (

	('Нәтиҗәдә '
	)   ) -> type(THEN_KEYWORD) ;

	TT_AND : (

	('Һәм '
		| 'Вә '
	)   ) -> type(AND_KEYWORD) ;

	TT_BUT : (

	('Ләкин '
		| 'Әмма '
	)   ) -> type(BUT_KEYWORD) ;

	TT_STARTING_STEP_KEYWORD : (

                TT_GIVEN
		| TT_WHEN
		| TT_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	TT_ALTERNATIVE_STEP_KEYWORD : (

                TT_AND
		| TT_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    TT_FEATURE_TITLE : WS* TT_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    TT_BACKGROUND_TITLE : WS* TT_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    TT_EXAMPLES_TITLE : WS* TT_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    TT_SCENARIO_TITLE : WS* TT_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    TT_SCENARIO_OUTLINE_TITLE : WS* TT_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    TT_RULE_TITLE : WS* TT_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        TT_GIVEN_STEP : WS* TT_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	TT_WHEN_STEP : WS* TT_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	TT_THEN_STEP : WS* TT_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	TT_AND_STEP : WS* TT_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	TT_BUT_STEP : WS* TT_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Ukrainian
//Українська
mode UK;
	UK_FEATURE : ( 
	('Функціонал'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    UK_BACKGROUND : (
    
	('Передумова'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	UK_SCENARIO : (

	('Приклад'
		| 'Сценарій'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	UK_SCENARIO_OUTLINE : (

	('Структура сценарію'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	UK_EXAMPLES : (

	('Приклади'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	UK_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	UK_GIVEN : (

	('Припустимо '
		| 'Припустимо, що '
		| 'Нехай '
		| 'Дано '
	)   ) -> type(GIVEN_KEYWORD) ;

	UK_WHEN : (

	('Якщо '
		| 'Коли '
	)   ) -> type(WHEN_KEYWORD) ;

	UK_THEN : (

	('То '
		| 'Тоді '
	)   ) -> type(THEN_KEYWORD) ;

	UK_AND : (

	('І '
		| 'А також '
		| 'Та '
	)   ) -> type(AND_KEYWORD) ;

	UK_BUT : (

	('Але '
	)   ) -> type(BUT_KEYWORD) ;

	UK_STARTING_STEP_KEYWORD : (

                UK_GIVEN
		| UK_WHEN
		| UK_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	UK_ALTERNATIVE_STEP_KEYWORD : (

                UK_AND
		| UK_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    UK_FEATURE_TITLE : WS* UK_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    UK_BACKGROUND_TITLE : WS* UK_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    UK_EXAMPLES_TITLE : WS* UK_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    UK_SCENARIO_TITLE : WS* UK_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    UK_SCENARIO_OUTLINE_TITLE : WS* UK_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    UK_RULE_TITLE : WS* UK_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        UK_GIVEN_STEP : WS* UK_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	UK_WHEN_STEP : WS* UK_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	UK_THEN_STEP : WS* UK_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	UK_AND_STEP : WS* UK_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	UK_BUT_STEP : WS* UK_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Urdu
//اردو
mode UR;
	UR_FEATURE : ( 
	('صلاحیت'
		| 'کاروبار کی ضرورت'
		| 'خصوصیت'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    UR_BACKGROUND : (
    
	('پس منظر'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	UR_SCENARIO : (

	('منظرنامہ'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	UR_SCENARIO_OUTLINE : (

	('منظر نامے کا خاکہ'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	UR_EXAMPLES : (

	('مثالیں'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	UR_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	UR_GIVEN : (

	('اگر '
		| 'بالفرض '
		| 'فرض کیا '
	)   ) -> type(GIVEN_KEYWORD) ;

	UR_WHEN : (

	('جب '
	)   ) -> type(WHEN_KEYWORD) ;

	UR_THEN : (

	('پھر '
		| 'تب '
	)   ) -> type(THEN_KEYWORD) ;

	UR_AND : (

	('اور '
	)   ) -> type(AND_KEYWORD) ;

	UR_BUT : (

	('لیکن '
	)   ) -> type(BUT_KEYWORD) ;

	UR_STARTING_STEP_KEYWORD : (

                UR_GIVEN
		| UR_WHEN
		| UR_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	UR_ALTERNATIVE_STEP_KEYWORD : (

                UR_AND
		| UR_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    UR_FEATURE_TITLE : WS* UR_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    UR_BACKGROUND_TITLE : WS* UR_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    UR_EXAMPLES_TITLE : WS* UR_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    UR_SCENARIO_TITLE : WS* UR_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    UR_SCENARIO_OUTLINE_TITLE : WS* UR_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    UR_RULE_TITLE : WS* UR_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        UR_GIVEN_STEP : WS* UR_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	UR_WHEN_STEP : WS* UR_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	UR_THEN_STEP : WS* UR_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	UR_AND_STEP : WS* UR_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	UR_BUT_STEP : WS* UR_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Uzbek
//Узбекча
mode UZ;
	UZ_FEATURE : ( 
	('Функционал'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    UZ_BACKGROUND : (
    
	('Тарих'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	UZ_SCENARIO : (

	('Сценарий'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	UZ_SCENARIO_OUTLINE : (

	('Сценарий структураси'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	UZ_EXAMPLES : (

	('Мисоллар'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	UZ_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	UZ_GIVEN : (

	('Агар '
	)   ) -> type(GIVEN_KEYWORD) ;

	UZ_WHEN : (

	('Агар '
	)   ) -> type(WHEN_KEYWORD) ;

	UZ_THEN : (

	('Унда '
	)   ) -> type(THEN_KEYWORD) ;

	UZ_AND : (

	('Ва '
	)   ) -> type(AND_KEYWORD) ;

	UZ_BUT : (

	('Лекин '
		| 'Бирок '
		| 'Аммо '
	)   ) -> type(BUT_KEYWORD) ;

	UZ_STARTING_STEP_KEYWORD : (

                UZ_GIVEN
		| UZ_WHEN
		| UZ_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	UZ_ALTERNATIVE_STEP_KEYWORD : (

                UZ_AND
		| UZ_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    UZ_FEATURE_TITLE : WS* UZ_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    UZ_BACKGROUND_TITLE : WS* UZ_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    UZ_EXAMPLES_TITLE : WS* UZ_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    UZ_SCENARIO_TITLE : WS* UZ_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    UZ_SCENARIO_OUTLINE_TITLE : WS* UZ_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    UZ_RULE_TITLE : WS* UZ_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        UZ_GIVEN_STEP : WS* UZ_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	UZ_WHEN_STEP : WS* UZ_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	UZ_THEN_STEP : WS* UZ_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	UZ_AND_STEP : WS* UZ_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	UZ_BUT_STEP : WS* UZ_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Vietnamese
//Tiếng Việt
mode VI;
	VI_FEATURE : ( 
	('Tính năng'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    VI_BACKGROUND : (
    
	('Bối cảnh'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	VI_SCENARIO : (

	('Tình huống'
		| 'Kịch bản'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	VI_SCENARIO_OUTLINE : (

	('Khung tình huống'
		| 'Khung kịch bản'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	VI_EXAMPLES : (

	('Dữ liệu'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	VI_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	VI_GIVEN : (

	('Biết '
		| 'Cho '
	)   ) -> type(GIVEN_KEYWORD) ;

	VI_WHEN : (

	('Khi '
	)   ) -> type(WHEN_KEYWORD) ;

	VI_THEN : (

	('Thì '
	)   ) -> type(THEN_KEYWORD) ;

	VI_AND : (

	('Và '
	)   ) -> type(AND_KEYWORD) ;

	VI_BUT : (

	('Nhưng '
	)   ) -> type(BUT_KEYWORD) ;

	VI_STARTING_STEP_KEYWORD : (

                VI_GIVEN
		| VI_WHEN
		| VI_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	VI_ALTERNATIVE_STEP_KEYWORD : (

                VI_AND
		| VI_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    VI_FEATURE_TITLE : WS* VI_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    VI_BACKGROUND_TITLE : WS* VI_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    VI_EXAMPLES_TITLE : WS* VI_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    VI_SCENARIO_TITLE : WS* VI_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    VI_SCENARIO_OUTLINE_TITLE : WS* VI_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    VI_RULE_TITLE : WS* VI_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        VI_GIVEN_STEP : WS* VI_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	VI_WHEN_STEP : WS* VI_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	VI_THEN_STEP : WS* VI_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	VI_AND_STEP : WS* VI_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	VI_BUT_STEP : WS* VI_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Chinese simplified
//简体中文
mode ZH_CN;
	ZH_CN_FEATURE : ( 
	('功能'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    ZH_CN_BACKGROUND : (
    
	('背景'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	ZH_CN_SCENARIO : (

	('场景'
		| '剧本'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	ZH_CN_SCENARIO_OUTLINE : (

	('场景大纲'
		| '剧本大纲'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	ZH_CN_EXAMPLES : (

	('例子'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	ZH_CN_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	ZH_CN_GIVEN : (

	('假如'
		| '假设'
		| '假定'
	)   ) -> type(GIVEN_KEYWORD) ;

	ZH_CN_WHEN : (

	('当'
	)   ) -> type(WHEN_KEYWORD) ;

	ZH_CN_THEN : (

	('那么'
	)   ) -> type(THEN_KEYWORD) ;

	ZH_CN_AND : (

	('而且'
		| '并且'
		| '同时'
	)   ) -> type(AND_KEYWORD) ;

	ZH_CN_BUT : (

	('但是'
	)   ) -> type(BUT_KEYWORD) ;

	ZH_CN_STARTING_STEP_KEYWORD : (

                ZH_CN_GIVEN
		| ZH_CN_WHEN
		| ZH_CN_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	ZH_CN_ALTERNATIVE_STEP_KEYWORD : (

                ZH_CN_AND
		| ZH_CN_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    ZH_CN_FEATURE_TITLE : WS* ZH_CN_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    ZH_CN_BACKGROUND_TITLE : WS* ZH_CN_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    ZH_CN_EXAMPLES_TITLE : WS* ZH_CN_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    ZH_CN_SCENARIO_TITLE : WS* ZH_CN_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    ZH_CN_SCENARIO_OUTLINE_TITLE : WS* ZH_CN_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    ZH_CN_RULE_TITLE : WS* ZH_CN_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        ZH_CN_GIVEN_STEP : WS* ZH_CN_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	ZH_CN_WHEN_STEP : WS* ZH_CN_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	ZH_CN_THEN_STEP : WS* ZH_CN_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	ZH_CN_AND_STEP : WS* ZH_CN_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	ZH_CN_BUT_STEP : WS* ZH_CN_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Chinese traditional
//繁體中文
mode ZH_TW;
	ZH_TW_FEATURE : ( 
	('功能'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    ZH_TW_BACKGROUND : (
    
	('背景'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	ZH_TW_SCENARIO : (

	('場景'
		| '劇本'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	ZH_TW_SCENARIO_OUTLINE : (

	('場景大綱'
		| '劇本大綱'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	ZH_TW_EXAMPLES : (

	('例子'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	ZH_TW_RULE : (

	('Rule'
	) ':'  ) -> type(RULE_KEYWORD) ;

	ZH_TW_GIVEN : (

	('假如'
		| '假設'
		| '假定'
	)   ) -> type(GIVEN_KEYWORD) ;

	ZH_TW_WHEN : (

	('當'
	)   ) -> type(WHEN_KEYWORD) ;

	ZH_TW_THEN : (

	('那麼'
	)   ) -> type(THEN_KEYWORD) ;

	ZH_TW_AND : (

	('而且'
		| '並且'
		| '同時'
	)   ) -> type(AND_KEYWORD) ;

	ZH_TW_BUT : (

	('但是'
	)   ) -> type(BUT_KEYWORD) ;

	ZH_TW_STARTING_STEP_KEYWORD : (

                ZH_TW_GIVEN
		| ZH_TW_WHEN
		| ZH_TW_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	ZH_TW_ALTERNATIVE_STEP_KEYWORD : (

                ZH_TW_AND
		| ZH_TW_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    ZH_TW_FEATURE_TITLE : WS* ZH_TW_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    ZH_TW_BACKGROUND_TITLE : WS* ZH_TW_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    ZH_TW_EXAMPLES_TITLE : WS* ZH_TW_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    ZH_TW_SCENARIO_TITLE : WS* ZH_TW_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    ZH_TW_SCENARIO_OUTLINE_TITLE : WS* ZH_TW_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    ZH_TW_RULE_TITLE : WS* ZH_TW_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        ZH_TW_GIVEN_STEP : WS* ZH_TW_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	ZH_TW_WHEN_STEP : WS* ZH_TW_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	ZH_TW_THEN_STEP : WS* ZH_TW_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	ZH_TW_AND_STEP : WS* ZH_TW_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	ZH_TW_BUT_STEP : WS* ZH_TW_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);


//Marathi
//मराठी
mode MR;
	MR_FEATURE : ( 
	('वैशिष्ट्य'
		| 'सुविधा'
	) ':'
		) -> type(FEATURE_KEYWORD) ;

    MR_BACKGROUND : (
    
	('पार्श्वभूमी'
	) ':'
		) -> type(BACKGROUND_KEYWORD);
	MR_SCENARIO : (

	('परिदृश्य'
	) ':'
	) -> type(SCENARIO_KEYWORD);

	MR_SCENARIO_OUTLINE : (

	('परिदृश्य रूपरेखा'
	) 
	) -> type(SCENARIO_OUTLINE_KEYWORD);

	MR_EXAMPLES : (

	('उदाहरण'
	) ':'  ) -> type(EXAMPLES_KEYWORD) ;

	MR_RULE : (

	('नियम'
	) ':'  ) -> type(RULE_KEYWORD) ;

	MR_GIVEN : (

	('जर'
		| 'दिलेल्या प्रमाणे '
	)   ) -> type(GIVEN_KEYWORD) ;

	MR_WHEN : (

	('जेव्हा '
	)   ) -> type(WHEN_KEYWORD) ;

	MR_THEN : (

	('मग '
		| 'तेव्हा '
	)   ) -> type(THEN_KEYWORD) ;

	MR_AND : (

	('आणि '
		| 'तसेच '
	)   ) -> type(AND_KEYWORD) ;

	MR_BUT : (

	('पण '
		| 'परंतु '
	)   ) -> type(BUT_KEYWORD) ;

	MR_STARTING_STEP_KEYWORD : (

                MR_GIVEN
		| MR_WHEN
		| MR_THEN
		| WILD_KEYWORD
		) -> type(STARTING_STEP_KEYWORD);

    
	MR_ALTERNATIVE_STEP_KEYWORD : (

                MR_AND
		| MR_BUT
		) -> type(ALTERNATIVE_STEP_KEYWORD);

    
    MR_FEATURE_TITLE : WS* MR_FEATURE ~[\r\n]* WS* LINE_END -> type(FEATURE_TITLE) ;

    MR_BACKGROUND_TITLE : WS* MR_BACKGROUND ~[\r\n]* COMMENT? LINE_END -> type(BACKGROUND_TITLE) ;

    MR_EXAMPLES_TITLE : WS* MR_EXAMPLES ~[\r\n]* COMMENT? LINE_END -> type(EXAMPLES_TITLE);

    MR_SCENARIO_TITLE : WS* MR_SCENARIO ~[\r\n]* LINE_END -> type(SCENARIO_TITLE);

    MR_SCENARIO_OUTLINE_TITLE : WS* MR_SCENARIO_OUTLINE ~[\r\n]* LINE_END -> type(SCENARIO_OUTLINE_TITLE) ;

    MR_RULE_TITLE : WS* MR_RULE ~[\r\n]* LINE_END -> type(RULE_TITLE);

        MR_GIVEN_STEP : WS* MR_GIVEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(GIVEN_STEP);
	MR_WHEN_STEP : WS* MR_WHEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(WHEN_STEP);
	MR_THEN_STEP : WS* MR_THEN ~[ @\r\n|] ~[\r\n]* LINE_END -> type(THEN_STEP);
	MR_AND_STEP : WS* MR_AND ~[ @\r\n|] ~[\r\n]* LINE_END -> type(AND_STEP);
	MR_BUT_STEP : WS* MR_BUT ~[ @\r\n|] ~[\r\n]* LINE_END -> type(BUT_STEP);

