grammar Graphemes;

Extend: [\p{Grapheme_Cluster_Break=Extend}];
ZWJ: '\u200D';
SpacingMark: [\p{Grapheme_Cluster_Break=SpacingMark}];
fragment VS15: '\uFE0E';
fragment VS16: '\uFE0F';
fragment NonspacingMark: [\p{Nonspacing_Mark}];
fragment TextPresentationCharacter: [\p{EmojiPresentation=TextDefault}];
fragment EmojiPresentationCharacter: [\p{EmojiPresentation=EmojiDefault}];
fragment TextPresentationSequence: EmojiPresentationCharacter VS15;
fragment EmojiPresentationSequence: TextPresentationCharacter VS16;
fragment EmojiCharacter: [\p{Emoji}];
fragment EmojiModifierSequence:
    EmojiCharacter ('\u{1F3FB}' | '\u{1F3FC}' | '\u{1F3FD}' | '\u{1F3FE}' | '\u{1F3FF}');
fragment EmojiFlagSequence:
    [\p{Grapheme_Cluster_Break=Regional_Indicator}] [\p{Grapheme_Cluster_Break=Regional_Indicator}];
fragment ExtendedPictographic: [\p{Extended_Pictographic}];
fragment EnclosingMark: [\p{General_Category=Enclosing_Mark}];
fragment EmojiCombiningSequence:
  (   EmojiPresentationSequence
    | TextPresentationSequence
    | EmojiPresentationCharacter )
  EnclosingMark*;
EmojiCoreSequence:
    EmojiModifierSequence
  | EmojiCombiningSequence
  | EmojiFlagSequence;
fragment EmojiZWJElement:
    EmojiCharacter
  | EmojiPresentationSequence
  | EmojiModifierSequence;
EmojiZWJSequence:
    EmojiZWJElement (ZWJ EmojiZWJElement)+;
fragment TagBase:
    EmojiCharacter
  | EmojiModifierSequence
  | EmojiPresentationSequence;
fragment TagSpec:
    [\u{E0020}-\u{E007E}]+;
fragment TagTerm: '\u{E007F}';
fragment EmojiTagSequence:
    TagBase TagSpec TagTerm;
emoji_sequence:
  (   EmojiZWJSequence
    | EmojiCoreSequence
    | EmojiTagSequence )
  ( Extend | ZWJ | SpacingMark )*;

Prepend: [\p{Grapheme_Cluster_Break=Prepend}];
NonControl: [\P{Grapheme_Cluster_Break=Control}];
CRLF: [\p{Grapheme_Cluster_Break=CR}][\p{Grapheme_Cluster_Break=LF}];
HangulSyllable:
    [\p{Grapheme_Cluster_Break=L}]* [\p{Grapheme_Cluster_Break=V}]+ [\p{Grapheme_Cluster_Break=T}]*
  | [\p{Grapheme_Cluster_Break=L}]* [\p{Grapheme_Cluster_Break=LV}] [\p{Grapheme_Cluster_Break=V}]* [\p{Grapheme_Cluster_Break=T}]*
  | [\p{Grapheme_Cluster_Break=L}]* [\p{Grapheme_Cluster_Break=LVT}] [\p{Grapheme_Cluster_Break=T}]*
  | [\p{Grapheme_Cluster_Break=L}]+
  | [\p{Grapheme_Cluster_Break=T}]+;

grapheme_cluster:
    CRLF
  | Prepend* ( emoji_sequence | HangulSyllable | NonControl ) ( Extend | ZWJ | SpacingMark )*;

graphemes: grapheme_cluster* EOF;
