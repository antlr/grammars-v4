/*
BSD License

Copyright (c) 2026, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
grammar sudoers;

file_
   : (statement? NEWLINE)* statement? EOF
   ;

statement
   : alias
   | default_entry
   | user_spec
   ;

alias
   : 'User_Alias' user_alias_spec (':' user_alias_spec)*
   | 'Runas_Alias' runas_alias_spec (':' runas_alias_spec)*
   | 'Host_Alias' host_alias_spec (':' host_alias_spec)*
   | 'Cmnd_Alias' cmnd_alias_spec (':' cmnd_alias_spec)*
   | 'Cmd_Alias' cmnd_alias_spec (':' cmnd_alias_spec)*
   ;

user_alias
   : NAME
   ;

user_alias_spec
   : user_alias '=' user_list
   ;

runas_alias
   : NAME
   ;

runas_alias_spec
   : runas_alias '=' runas_list
   ;

host_alias
   : NAME
   ;

host_alias_spec
   : host_alias '=' host_list
   ;

cmnd_alias
   : NAME
   ;

cmnd_alias_spec
   : cmnd_alias '=' cmnd_list
   ;

user_list
   : user (',' user)*
   ;

user
   : '!'* user_name
   | '!'* HASH uid
   | '!'* '%' group
   | '!'* '%#' gid
   | '!'* '+' netgroup
   | '!'* '%:' nonunix_group
   | '!'* '%:#' nonunix_gid
   | '!'* user_alias
   | '!'* 'ALL'
   ;

runas_list
   : runas_member (',' runas_member)*
   ;

runas_member
   : '!'* user_name
   | '!'* HASH uid
   | '!'* '%' group
   | '!'* '%#' gid
   | '!'* '%:' nonunix_group
   | '!'* '%:#' nonunix_gid
   | '!'* '+' netgroup
   | '!'* runas_alias
   | '!'* 'ALL'
   ;

host_list
   : host (',' host)*
   ;

host
   : '!'* host_name
   | '!'* network ('/' netmask)?
   | '!'* '+' netgroup
   | '!'* host_alias
   | '!'* 'ALL'
   ;

cmnd_list
   : cmnd (',' cmnd)*
   ;

command_name
   : file_name
   | file_name args
   | file_name '""'
   ;

cmnd
   : '!'* command_name
   | '!'* directory
   | '!'* 'sudoedit'
   | '!'* cmnd_alias
   | '!'* 'ALL'
   ;

default_type
   : 'Defaults'
   | 'Defaults' '@' host_list
   | 'Defaults' ':' user_list
   | 'Defaults' '!' cmnd_list
   | 'Defaults' '>' runas_list
   ;

default_entry
   : default_type parameter_list
   ;

parm
   : NAME
   ;

parameter_list
   : parameter (',' parameter)*
   ;

parameter
   : parm '=' value
   | parm '+=' value
   | parm '-=' value
   | '!'* parm
   ;

user_spec
   : user_list host_list '=' cmnd_spec_list (':' host_list '=' cmnd_spec_list)*
   ;

cmnd_spec_list
   : cmnd_spec (',' cmnd_spec)*
   ;

cmnd_spec
   : runas_spec? option_spec* tag_spec* cmnd
   ;

runas_spec
   : '(' runas_list? (':' runas_list)? ')'
   ;

option_spec
   : (selinux_spec | solaris_priv_spec | date_spec | timeout_spec | chdir_spec | chroot_spec)
   ;

selinux_spec
   : 'ROLE=' NAME
   | 'TYPE=' NAME
   ;

solaris_priv_spec
   : 'PRIVS=' NAME
   | 'LIMITPRIVS=' NAME
   ;

date_spec
   : 'NOTBEFORE=' NAME
   | 'NOTAFTER=' NAME
   ;

timeout_spec
   : 'TIMEOUT=' NUM
   ;

chdir_spec
   : 'CWD=' directory
   ;

chroot_spec
   : 'CHROOT=' directory
   ;

tag_spec
   : ('NOPASSWD:' | 'PASSWD:' | 'NOEXEC:' | 'EXEC:' | 'SETENV:' | 'NOSETENV:' | 'LOG_INPUT:' | 'NOLOG_INPUT:' | 'LOG_OUTPUT:' | 'NOLOG_OUTPUT:')
   ;

network
   : IP_ADDR
   ;

host_name
   : NAME ('.' NAME)*
   ;

file_name
   : PATH
   | NAME
   ;

directory
   : PATH
   | NAME
   ;

user_name
   : NAME
   ;

uid
   : NUM
   ;

group
   : NAME
   ;

value
   : NAME
   | PATH
   | STRING
   ;

gid
   : NUM
   ;

netgroup
   : NAME
   ;

nonunix_group
   : NAME
   ;

nonunix_gid
   : NUM
   ;

args
   : NAME
   ;

netmask
   : NUM
   | IP_ADDR
   ;

IP_ADDR
   : NUM '.' NUM '.' NUM '.' NUM
   ;

NUM
   : [0-9]+
   ;

NAME
   : [a-zA-Z_] [a-zA-Z0-9_]*
   ;

PATH
   : '/' [a-zA-Z_] (~[ \t\r\n,:=])*
   ;

STRING
   : '"' (~["\r\n])* '"'
   ;

NEWLINE
   : ('\r'? '\n')+
   ;

CONTINUATION
   : '\\' '\r'? '\n' -> skip
   ;

COMMENT
   : '#' ~[\r\n]* -> skip
   ;

HASH
   : '#'
   ;

WS
   : [ \t]+ -> skip
   ;
