grammar SemanticVersion;

fragment A : [aA];
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];

fragment POSITIVE_DIGIT: [1-9];
fragment DIGIT: '0' | POSITIVE_DIGIT;

fragment LETTER: [A-Za-z];

DASH: '-';
PLUS: '+';
DOT: '.';

//most common pre-release "modifiers"
fragment ALPHA: A L P H A;
fragment BETA: B E T A;
fragment RC: (R C) | (R E L E A S E (DASH | DOT) C A N D I D A T E);
fragment SNAPSHOT: S N A P S H O T;
fragment PREVIEW: P | P R E | P R E V I E W;
fragment DEV: D E V | D E V E L | D E V E L O P M E N T;
fragment MILESTONE: M T | M I L E S T O N E;
fragment DAILY: D A I L Y;
fragment NIGHTLY: N I G H T L Y;
fragment BUILD: B L D | B U I L D;
fragment TEST: T E S T;
fragment EXPERIMENTAL: E X P E R I M E N T A L;

NUMBER: '0' | POSITIVE_DIGIT+;

TAG: ALPHA
    | BETA
    | RC
    | SNAPSHOT
    | PREVIEW
    | DEV
    | MILESTONE
    | DAILY
    | NIGHTLY
    | BUILD
    | TEST
    | EXPERIMENTAL
    ;

IDENTIFIER: LETTER+;

patch: value = NUMBER;
minor: value = NUMBER;
major: value = NUMBER;

build:
      value = NUMBER                                             #BuildNumber
    | DASH                                                       #BuildDash
    | tag = TAG (DOT | DASH)? version = (NUMBER | IDENTIFIER)?   #BuildTagged
    | value = IDENTIFIER                                         #BuildIdentifier
    | left = build (DOT | DASH)? right = build                   #BuildIdentifierExtended
    ;

preRelease:
      value = NUMBER                                            #PreReleaseNumber
    | DASH                                                      #PreReleaseDash
    | tag = TAG (DOT | DASH)? version = (NUMBER | IDENTIFIER)?  #PreReleaseTagged
    | value = IDENTIFIER                                        #PreReleaseIdentifier
    | left = preRelease (DOT | DASH)? right = preRelease        #PreReleaseIdentifierExtended
    ;

versionCore: major DOT minor DOT patch;

semver:
    versionCore                               #SemverWithCore
  | versionCore DASH preRelease               #SemverWithPreRelease
  | versionCore PLUS build                    #SemverWithBuild
  | versionCore DASH preRelease PLUS build    #SemverWithPreReleaseAndBuild
  ;