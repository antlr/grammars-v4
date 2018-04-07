/*
    A parser for the SGF-format.

    This file is copyrighted. You may use it under the terms of the AGPL 3.0.
    See agpl-3.0.txt for legal details.
 */

grammar sgf;

collection      :   gameTree+;
gameTree        :   '(' sequence gameTree* ')';
sequence        :   node+;
node            :   ';' property*;
property        :
                    move
                |   setup
                |   nodeAnnotation
                |   moveAnnotation
                |   markup
                |   root
                |   gameInfo
                |   timing
                |   misc
                |   loa
                |   go
                |   privateProp
                ;

move            :
                    COLOR (NONE | TEXT)
                |   'KO' NONE
                |   'MN' TEXT
                ;

setup           :
                    'AB' TEXT+
                |   'AE' TEXT+
                |   'AW' TEXT+
                |   'PL' TEXT
                ;

nodeAnnotation  :
                    'C'  TEXT
                |   'DM' TEXT
                |   'GB' TEXT
                |   'GW' TEXT
                |   'HO' TEXT
                |   'N'  TEXT
                |   'UC' TEXT
                |   'V'  TEXT
                ;

moveAnnotation:
                    'BM' TEXT
                |   'DO' NONE
                |   'IT' NONE
                |   'TE' TEXT
                ;

markup          :
                    'AR' TEXT+
                |   'CR' TEXT+
                |   'DD' (NONE | TEXT+)
                |   'LB' TEXT+
                |   'LN' TEXT+
                |   'MA' TEXT+
                |   'SL' TEXT+
                |   'SQ' TEXT+
                |   'TR' TEXT+
                ;

root            :   'AP' TEXT
                |   'CA' TEXT
                |   'FF' TEXT
                |   'GM' TEXT
                |   'ST' TEXT
                |   'SZ' TEXT
                ;

gameInfo        :
                    'AN' TEXT
                |   'BR' TEXT
                |   'BT' TEXT
                |   'CP' TEXT
                |   'DT' TEXT
                |   'EV' TEXT
                |   'GN' TEXT
                |   'GC' TEXT
                |   'ON' TEXT
                |   'OT' TEXT
                |   'PB' TEXT
                |   'PC' TEXT
                |   'PW' TEXT
                |   'RE' TEXT
                |   'RO' TEXT
                |   'RU' TEXT
                |   'SO' TEXT
                |   'TM' TEXT
                |   'US' TEXT
                |   'WR' TEXT
                |   'WT' TEXT
                ;

timing        :
                    'BL' TEXT
                |   'OB' TEXT
                |   'OW' TEXT
                |   'WL' TEXT
                ;

misc            :
                    'FG' (NONE | TEXT)
                |   'PM' TEXT
                |   'VW' TEXT+
                ;

loa             :
                    'AS' TEXT
                |   'IP' TEXT
                |   'IY' TEXT
                |   'SE' TEXT
                |   'SU' TEXT
                ;

go              :
                    'HA' TEXT
                |   'KM' TEXT
                |   'TB' (NONE | TEXT+)
                |   'TW' (NONE | TEXT+)
                ;

privateProp     :   UCLETTER (NONE | TEXT+);

COLOR           :   ('W'|'B');
UCLETTER        :   'A'..'Z' 'A'..'Z'+;
NONE            :   '[]';
TEXT            :   '['('\\]'|.)*?']';

WS              :   [ \n\r\t]+ -> skip;
