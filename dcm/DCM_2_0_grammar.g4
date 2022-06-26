
// Parser
grammar DCM_2_0_grammar;

konservierung
   : ( '\n' )* 'KONSERVIERUNG_FORMAT 2.0' ( '\n' )+ kons_kopf kons_rumpf EOF
   ;

kons_kopf
   : ( modulkopf_info )? ( funktionsdef )? ( variantendef )?
   ;

modulkopf_info
   : ( mod_zeile )+
   ;

mod_zeile
   : mod_anf_zeile ( mod_fort_zeile )*
   ;

mod_anf_zeile
   : 'MODULKOPF' mod_ele_name mod_ele_wert
   ;

mod_fort_zeile
   : 'MODULKOPF' mod_ele_wert
   ;

mod_ele_name
   : NAME
   ;

mod_ele_wert
   : TEXT ( '\n' )+
   ;

funktionsdef
   : 'FUNKTIONEN' '\n' ( funktionszeile )+ 'END' ( '\n' )+
   ;

funktionszeile
   : 'FKT' NAME fkt_version fkt_langname
   ;

fkt_version
   : TEXT
   ;

fkt_langname
   : TEXT ( '\n' )+
   ;

variantendef
   : 'VARIANTENKODIERUNG' '\n' ( variantenkrit )+ 'END' ( '\n' )+
   ;

variantenkrit
   : 'KRITERIUM' krit_name ( krit_wert )* ( '\n' )+
   ;

krit_name
   : NAME
   ;

krit_wert
   : NAME
   ;

kons_rumpf
   : ( kenngroesse )*
   ;

kenngroesse
   : ( kennwert | kennwerteblock | kennlinie | kennfeld | gruppenstuetzstellen | kenntext )
   ;

kennwert
   : 'FESTWERT' NAME '\n' kgr_info ( einheit_w )? 'WERT' realzahl '\n' 'END' ( '\n' )+ | 'FESTWERT' NAME '\n' kgr_info ( einheit_w )? 'TEXT' TEXT '\n' 'END' ( '\n' )+
   ;

kennwerteblock
   : 'FESTWERTEBLOCK' NAME anzahl_x '\n' kgr_info ( einheit_w )? ( werteliste_kwb )+ 'END' ( '\n' )+
   ;

kennlinie
   : ( 'KENNLINIE' ) NAME anzahl_x '\n' kgr_info ( einheit_x )? ( einheit_w )? ( sst_liste_x )+ ( werteliste )+ 'END' ( '\n' )+ | ( 'FESTKENNLINIE' ) NAME anzahl_x '\n' kgr_info ( einheit_x )? ( einheit_w )? ( sst_liste_x )+ ( werteliste )+ 'END' ( '\n' )+ | ( 'GRUPPENKENNLINIE' ) NAME anzahl_x '\n' kgr_info ( einheit_x )? ( einheit_w )? ( sst_liste_x )+ ( werteliste )+ 'END' ( '\n' )+
   ;

kennfeld
   : ( 'KENNFELD' ) NAME anzahl_x anzahl_y '\n' kgr_info ( einheit_x )? ( einheit_y )? ( einheit_w )? ( sst_liste_x )+ kf_zeile_liste 'END' ( '\n' )+ | ( 'FESTKENNFELD' ) NAME anzahl_x anzahl_y '\n' kgr_info ( einheit_x )? ( einheit_y )? ( einheit_w )? ( sst_liste_x )+ kf_zeile_liste 'END' ( '\n' )+ | ( 'GRUPPENKENNFELD' ) NAME anzahl_x anzahl_y '\n' kgr_info ( einheit_x )? ( einheit_y )? ( einheit_w )? ( sst_liste_x )+ kf_zeile_liste 'END' ( '\n' )+
   ;

gruppenstuetzstellen
   : 'STUETZSTELLENVERTEILUNG' NAME anzahl_x '\n' kgr_info ( einheit_x )? ( sst_liste_x )+ 'END' ( '\n' )+
   ;

kenntext
   : 'TEXTSTRING' NAME '\n' kgr_info 'TEXT' TEXT '\n' 'END' ( '\n' )+
   ;

kgr_info
   : ( langname )? ( displayname )? ( var_abhangigkeiten )? ( funktionszugehorigkeit )?
   ;

einheit_x
   : 'EINHEIT_X' TEXT '\n'
   ;

einheit_y
   : 'EINHEIT_Y' TEXT '\n'
   ;

einheit_w
   : 'EINHEIT_W' TEXT '\n'
   ;

langname
   : 'LANGNAME' TEXT '\n'
   ;

displayname
   : 'DISPLAYNAME' ( NAME | TEXT ) '\n'
   ;

var_abhangigkeiten
   : 'VAR' var_abh ( ',' var_abh )* '\n'
   ;

var_abh
   : NAME '=' NAME
   ;

funktionszugehorigkeit
   : 'FUNKTION' ( NAME )+ '\n'
   ;

anzahl_x
   : INT
   ;

anzahl_y
   : INT
   ;

werteliste
   : 'WERT' ( realzahl )+ '\n'
   ;

werteliste_kwb
   : ( 'WERT' ( realzahl )+ '\n' | 'TEXT' ( TEXT )+ '\n' )
   ;

sst_liste_x
   : ( 'ST/X' ( realzahl )+ '\n' | 'ST_TX/X' ( TEXT )+ '\n' )
   ;

kf_zeile_liste
   : ( kf_zeile_liste_r+ | kf_zeile_liste_tx+ )
   ;

kf_zeile_liste_r
   : ( 'ST/Y' realzahl '\n' werteliste+ )
   ;

kf_zeile_liste_tx
   : ( 'ST_TX/Y' TEXT '\n' werteliste+ )
   ;

realzahl
   : ( INT | FLOAT )
   ;

NAME
   : LETTER ( LETTER | '0' .. '9' | '[' | ']' | '.' )*
   ;


fragment LETTER
   : 'A' .. 'Z' | 'a' .. 'z' | '_'
   ;


TEXT
   : '"' ( EscapeSequence | ~ ( '\\' | '"' ) )* '"'
   ;


fragment EscapeSequence
   : '\\' ( 'b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\' )
   ;


fragment QUOTE
   : '"'
   ;


INT
   : MINUS? ( '0' | '1' .. '9' '0' .. '9'* )
   ;


FLOAT
   : MINUS? ( '0' .. '9' )+ '.' ( '0' .. '9' )* Exponent? | MINUS? '.' ( '0' .. '9' )+ Exponent? | MINUS? ( '0' .. '9' )+ Exponent
   ;


MINUS
   : '-'
   ;


fragment Exponent
   : ( 'e' | 'E' ) ( '+' | '-' )? ( '0' .. '9' )+
   ;


WS
   : ( ' ' | '\r' | '\t' | '\u000C' ) ->skip
   ;


COMMENT
   : ( '*' | '!' | '.' ) .*? '\n' ->skip
   ;
