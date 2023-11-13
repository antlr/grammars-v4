/*
BSD License

Copyright (c) 2023, Tom Everett
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
   : alias* EOF
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
   : user
   | user ',' user_list
   ;

user
   : '!'* user_name
   | '!'* '#' uid
   | '!'* '%' group
   | '!'* '%#' gid
   | '!'* '+' netgroup
   | '!'* '%:' nonunix_group
   | '!'* '%:#' nonunix_gid
   | '!'* user_alias
   ;

runas_list
   : runas_member
   | runas_member ',' runas_list
   ;

runas_member
   : '!'* user_name
   | '!'* '#' uid
   | '!'* '%' group
   | '!'* '%#' gid
   | '!'* '%:' nonunix_group
   | '!'* '%:#' nonunix_gid
   | '!'* '+' netgroup
   | '!'* runas_alias
   | '!'* 'ALL'
   ;

host_list
   : host
   | host ',' host_list
   ;

host
   : '!'* host_name
   | '!'* ip_addr
   | '!'* network ('/' netmask)?
   | '!'* '+' netgroup
   | '!'* host_alias
   | '!'* 'ALL'
   ;

cmnd_list
   : cmnd
   | cmnd ',' cmnd_list
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
   : parameter
   | parameter ',' parameter_list
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
   : cmnd_spec
   | cmnd_spec ',' cmnd_spec_list
   ;

cmnd_spec
   : runas_spec? option_spec* (tag_spec ':')* cmnd
   ;

runas_spec
   : '(' runas_list? (':' runas_list)? ')'
   ;

option_spec
   : (selinux_spec | solaris_priv_spec | date_spec | timeout_spec | chdir_spec | chroot_spec)
   ;

selinux_spec
   : ('ROLE=role' | 'TYPE=type')
   ;

solaris_priv_spec
   : ('PRIVS=privset' | 'LIMITPRIVS=privset')
   ;

date_spec
   : ('NOTBEFORE=timestamp' | 'NOTAFTER=timestamp')
   ;

timeout_spec
   : 'TIMEOUT=timeout'
   ;

chdir_spec
   : 'CWD=directory'
   ;

chroot_spec
   : 'CHROOT=directory'
   ;

tag_spec
   : ('NOPASSWD:' | 'PASSWD:' | 'NOEXEC:' | 'EXEC:' | 'SETENV:' | 'NOSETENV:' | 'LOG_INPUT:' | 'NOLOG_INPUT:' | 'LOG_OUTPUT:' | 'NOLOG_OUTPUT:')
   ;

network
   : IP_ADDR
   ;

ip_addr
   : IP_ADDR
   ;

host_name
   : NAME ('.' NAME)*
   ;

file_name
   : NAME
   ;

directory
   : NAME
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
   ;

IP_ADDR
   : NUM '.' NUM '.' NUM '.' NUM
   ;

NUM
   : [0-9]+
   ;

NAME
   : [A-Z] ([A-Z] [0-9])*
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

