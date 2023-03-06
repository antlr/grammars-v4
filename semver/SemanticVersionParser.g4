parser grammar SemanticVersionParser;

options { tokenVocab = SemanticVersionLexer; }

tag: ALPHA
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

build:
      value = NUMBER                                             #BuildNumber
    | DASH                                                       #BuildDash
    | tag (DOT | DASH)? version = (NUMBER | IDENTIFIER)?         #BuildTagged
    | value = IDENTIFIER                                         #BuildIdentifier
    | left = build (DOT | DASH)? right = build                   #BuildIdentifierExtended
    ;

preRelease:
      value = NUMBER                                            #PreReleaseNumber
    | DASH                                                      #PreReleaseDash
    | tag (DOT | DASH)? version = (NUMBER | IDENTIFIER)?        #PreReleaseTagged
    | value = IDENTIFIER                                        #PreReleaseIdentifier
    | left = preRelease (DOT | DASH)? right = preRelease        #PreReleaseIdentifierExtended
    ;

versionCore: major = NUMBER DOT minor = NUMBER DOT patch = NUMBER;

semver: versionCore (DASH preRelease (PLUS build)? | PLUS build)? EOF;