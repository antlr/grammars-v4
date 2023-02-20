/*

 Copyright 2008 by Nathaniel Harward <nharward@gmail.com>

 ANTLRv3 grammar for the memcached protocol, as described by the document
 http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt

 Please email with any corrections/errors you might find.

/*
    Ported to Antlr4 by Tom Everett <tom@khubla.com>

    The memcached protocol is now described here: https://github.com/memcached/memcached/blob/master/doc/protocol.txt
*/

grammar memcached_protocol;

command_line
   : (storage_command | retrieval_command | delete_command | increment_command | decrement_command | statistics_command | flush_command | version_command | verbosity_command | quit_command)+ EOF
   ;

storage_command
   : ((storage_command_name key flags exptime bytes) | ('cas' key flags exptime bytes cas_unique)) noreply?
   ;

storage_command_name
   : 'set'
   | 'add'
   | 'replace'
   | 'append'
   | 'prepend'
   ;

retrieval_command
   : ('get' | 'gets') key +
   ;

delete_command
   : 'delete' key time? noreply?
   ;

increment_command
   : 'incr' key value noreply?
   ;

decrement_command
   : 'decr' key value noreply?
   ;

statistics_command
   : 'stats' statistics_option?
   ;

statistics_option
   : 'items'
   | 'slabs'
   | 'sizes'
   ;

flush_command
   : 'flush_all' delay? noreply?
   ;

version_command
   : 'version'
   ;

verbosity_command
   : 'verbosity' verbosity_level
   ;

quit_command
   : 'quit'
   ;

storage_response
   : error_response
   | 'STORED'
   | 'NOT_STORED'
   | 'EXISTS'
   | 'NOT_FOUND'
   ;

retrieval_response
   : error_response
   | ('VALUE' key flags bytes cas_unique?)
   | end
   ;

deletion_response
   : error_response
   | 'DELETED'
   | 'NOT_FOUND'
   ;

incr_or_decr_response
   : error_response
   | 'NOT_FOUND'
   | INTEGER
   ;

statistics_response
   : error_response
   | general_statistic
   | size_statistic
   | end
   ;

error_response
   : general_error
   | client_error_message
   | server_error_message
   ;

general_statistic
   : 'STAT' statistic_name statistic_value
   ;

size_statistic
   : size count
   ;

general_error
   : 'ERROR'
   ;

client_error_message
   : 'CLIENT_ERROR' .+?
   ;

server_error_message
   : 'SERVER_ERROR' .+?
   ;

end
   : 'END'
   ;

noreply
   : 'noreply'
   ;

key
   : .
   ;

flags
   : INTEGER
   ;

exptime
   : INTEGER
   ;

bytes
   : INTEGER
   ;

cas_unique
   : INTEGER
   ;

value
   : INTEGER
   ;

time
   : INTEGER
   ;

delay
   : INTEGER
   ;

verbosity_level
   : INTEGER
   ;

statistic_name
   : WORD
   ;

statistic_value
   : .
   ;

size
   : INTEGER
   ;

count
   : INTEGER
   ;


INTEGER
   : DIGIT +
   ;


WORD
   : PRINTABLE_CHAR +
   ;


fragment DIGIT
   : '0' .. '9'
   ;


fragment PRINTABLE_CHAR
   : '!' .. '~'
   ;


WHITESPACE
   : (' ' | '\t' | '\r' | '\n' | '\u000C') + -> skip
   ;
