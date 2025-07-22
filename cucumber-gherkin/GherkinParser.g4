parser grammar GherkinParser; 

options { tokenVocab=GherkinLexer; } 

gherkinDocument : LANGUAGE_HEADER? COMMENT* feature
	| COMMENT+
	| EOF
	;
feature : TAGS*? FEATURE_TITLE LONG_DESCRIPTION*?  background?  (ruleBlock | scenario)*;
ruleBlock : RULE_TITLE  LONG_DESCRIPTION*  background? scenario*;
background : BACKGROUND_TITLE  LONG_DESCRIPTION*? stepBody? ;
scenario : TAGS*? (SCENARIO_TITLE | SCENARIO_OUTLINE_TITLE) LONG_DESCRIPTION* stepBody? examplesBody*;
stepBody :  startingStep  (anyStep)* ;
examplesBody : TAGS*?  EXAMPLES_TITLE  LONG_DESCRIPTION*?  DATA_ROW* ;
startingStep : (GIVEN_STEP | WHEN_STEP | THEN_STEP | WILD_STEP) (DATA_ROW* | DOCSTRING) ;
anyStep : (GIVEN_STEP | WHEN_STEP | THEN_STEP | AND_STEP | BUT_STEP | WILD_STEP) (DATA_ROW* | DOCSTRING) ;
