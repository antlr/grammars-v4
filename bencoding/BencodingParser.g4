parser grammar BencodingParser;

options {
  tokenVocab=BencodingLexer;
}

// https://wiki.theory.org/BitTorrentSpecification#Bencoding
data
 : values EOF
 ;

values
 : value*
 ;

value
 : integer
 | STRING
 | list
 | dict
 ;

integer
 : INT_START INTEGER END
 ;

list
 : LIST_START values END
 ;

dict
 : DICT_START key_value* END
 ;

key_value
 : STRING value
 ;