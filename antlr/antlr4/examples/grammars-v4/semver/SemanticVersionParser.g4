// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar SemanticVersionParser;

options {
    tokenVocab = SemanticVersionLexer;
}

tag
    : ALPHA
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

build
    : value = NUMBER                                     # BuildNumber
    | DASH                                               # BuildDash
    | tag (DOT | DASH)? version = (NUMBER | IDENTIFIER)? # BuildTagged
    | value = IDENTIFIER                                 # BuildIdentifier
    | left = build (DOT | DASH)? right = build           # BuildIdentifierExtended
    ;

preRelease
    : value = NUMBER                                     # PreReleaseNumber
    | DASH                                               # PreReleaseDash
    | tag (DOT | DASH)? version = (NUMBER | IDENTIFIER)? # PreReleaseTagged
    | value = IDENTIFIER                                 # PreReleaseIdentifier
    | left = preRelease (DOT | DASH)? right = preRelease # PreReleaseIdentifierExtended
    ;

versionCore
    : major = NUMBER DOT minor = NUMBER DOT patch = NUMBER
    ;

semver
    : versionCore (DASH preRelease (PLUS build)? | PLUS build)? EOF
    ;