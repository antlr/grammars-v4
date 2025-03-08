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
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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
grammar pf;

file_
   : line* EOF
   ;

line
   : (option | pf_rule | antispoof_rule | queue_rule | anchor_rule | anchor_close | load_anchor | table_rule | include | var)
   ;

var
   : STRING '=' QUOTED_STRING
   ;

option
   : 'set' (('timeout' (timeout | '{' timeout_list '}'))? | ('ruleset-optimization' ('none' | 'basic' | 'profile')?)? | ('optimization' ('default' | 'normal' | 'high-latency' | 'satellite' | 'aggressive' | 'conservative')?)? ('limit' (limit_item | '{' limit_list '}'))? | ('loginterface' (interface_name | 'none'))? | ('block-policy' ('drop' | 'return'))? | ('state-policy' ('if-bound' | 'floating'))? ('state-defaults' state_opts)? ('fingerprints' filename)? | ('skip on' ifspec)? | ('debug' ('emerg' | 'alert' | 'crit' | 'err' | 'warning' | 'notice' | 'info' | 'debug' | 'urgent'))? | ('reassemble' ('yes' | 'no') ('no-df')?)?)
   ;

pf_rule
   : action direction? (logging | 'quick')* onspec? af? protospec? hosts filteropts?
   ;

onspec
   : 'on' (ifspec | 'rdomain' NUMBER)
   ;

direction
   : 'in'
   | 'out'
   ;

logging
   : 'log' ('(' logopts ')')?
   ;

logopts
   : logopt ((',')? logopts)? NUMBER
   ;

logopt
   : 'all'
   | 'matches'
   | 'user'
   | 'to' interface_name
   ;

filteropts
   : filteropt (','? filteropt)*
   ;

filteropt
   : user
   | group
   | flags
   | icmp_type
   | icmp6_type
   | 'tos' tos
   | ('no' | 'keep' | 'modulate' | 'synproxy') 'state' ('(' state_opts ')')?
   | 'scrub' '(' scrubopts ')'?
   | 'fragment'
   | 'allow-opts'
   | 'once'
   | 'divert-packet' 'port' port
   | 'divert-reply'
   | 'divert-to' host 'port' port
   | 'label' QUOTED_STRING
   | 'tag' STRING
   | '!'? 'tagged' STRING
   | 'max-pkt-rate' NUMBER '/' seconds
   | 'set delay' NUMBER
   | 'set prio' (NUMBER | '(' NUMBER ((',')? NUMBER)? ')')
   | 'set queue' (STRING | '(' STRING ((',')? STRING)? ')')
   | 'rtable' NUMBER
   | 'probability' NUMBER '%'
   | 'prio' NUMBER
   | 'af-to' af 'from' (redirhost | '{' redirhost_list '}') ('to' (redirhost | '{' redirhost_list '}'))?
   | 'binat-to' (redirhost | '{' redirhost_list '}') (portspec)? (pooltype)?
   | 'rdr-to' (redirhost | '{' redirhost_list '}') (portspec)? (pooltype)?
   | 'nat-to' (redirhost | '{' redirhost_list '}') (portspec)? (pooltype)? ('static-port')?
   | route
   | ('set tos' tos)
   | (('!')? 'received-on' (interface_name | interface_group))
   ;

scrubopts
   : scrubopt ((',')? scrubopt)?
   ;

scrubopt
   : 'no-df'
   | 'min-ttl' NUMBER
   | 'max-mss' NUMBER
   | 'reassemble tcp'
   | 'random-id'
   ;

antispoof_rule
   : 'antispoof' ('log')? ('quick')? 'for' ifspec (af)? ('label' STRING)?
   ;

table_rule
   : 'table' '<' STRING '>' (tableopts)?
   ;

tableopts
   : tableopt+
   ;

tableopt
   : 'persist'
   | 'const'
   | 'counters'
   | 'file' QUOTED_STRING
   | '{' (tableaddrs)? '}'
   ;

tableaddrs
   : tableaddr_spec ((',')? tableaddrs)?
   ;

tableaddr_spec
   : ('!')? tableaddr ('/' mask_bits)?
   ;

tableaddr
   : hostname
   | ifspec
   | 'self'
   | IPV4_DOTTED_QUAD
   | IPV6_COLONED_HEX
   ;

queue_rule
   : 'queue' STRING ('on' interface_name)? queueopts_list
   ;

anchor_rule
   : 'anchor' (STRING | QUOTED_STRING)? (('in' | 'out'))? ('on' ifspec)? af? protospec? hosts filteropts? ('{')?
   ;

anchor_close
   : '}'
   ;

load_anchor
   : 'load anchor' STRING 'from' filename
   ;

queueopts_list
   : queueopts+
   ;

queueopts
   : (('bandwidth' bandwidth) | ('min' bandwidth) | ('max' bandwidth) | ('parent' STRING) | ('default'))
   | (('flows' NUMBER) | ('quantum' NUMBER))
   | ('qlimit' NUMBER)
   ;

bandwidth
   : bandwidth_spec ('burst' bandwidth_spec 'for' NUMBER 'ms')?
   ;

bandwidth_spec
   : NUMBER ('K' | 'M' | 'G')?
   ;

action
   : 'pass'
   | 'match'
   | 'block' return?
   | 'scrub'
   ;

return
   : 'drop'
   | 'return'
   | 'return-rst' ('(' 'ttl' NUMBER ')')?
   | 'return-icmp' ('(' icmpcode ((',')? icmp6code)? ')')?
   | 'return-icmp6' ('(' icmp6code ')')?
   ;

icmpcode
   : (icmp_code_name | icmp_code_NUMBER)
   ;

icmp6code
   : (icmp6_code_name | icmp6_code_NUMBER)
   ;

ifspec
   : (('!')? (interface_name | interface_group))
   | '{' interface_list '}'
   ;

interface_list
   : ('!')? (interface_name | interface_group) ((',')? interface_list)?
   ;

route
   : ('route-to' | 'reply-to' | 'dup-to') (redirhost | '{' redirhost_list '}')
   ;

af
   : 'inet'
   | 'inet6'
   ;

protospec
   : 'proto' (proto_name | proto_NUMBER | '{' proto_list '}')
   ;

proto_list
   : (proto_name | proto_NUMBER) ((',')? proto_list)?
   ;

hosts
   : 'all'
   | fromspec? tospec?
   ;

fromspec
   : 'from' ('any' | 'no-route' | 'urpf-failed' | 'self' | host | host_list | 'route' (STRING | QUOTED_STRING)) port? os?
   ;

tospec
   : 'to' ('any' | 'no-route' | 'self' | host | host_list | 'route' (STRING | QUOTED_STRING)) port?
   ;

host
   : ('!')? (address ('weight' NUMBER)? | address ('/' mask_bits)? ('weight' NUMBER)? | '<' STRING '>')
   ;

redirhost
   : address ('/' mask_bits)?
   ;

address
   : (interface_name | interface_group | '(' (interface_name | interface_group) ')' | hostname | IPV4_DOTTED_QUAD | IPV6_COLONED_HEX)
   ;

host_list
   : '{' host (','? host)? '}'
   ;

redirhost_list
   : redirhost (',' redirhost)?
   ;

port
   : 'port' (unary_op | binary_op | '{' op_list '}')
   ;

portspec
   : 'port' (NUMBER | name) (':' ('*' | NUMBER | name))?
   ;

os
   : 'os' (os_name | '{' os_list '}')
   ;

user
   : 'user' (unary_op | binary_op | '{' op_list '}')
   ;

group
   : 'group' (unary_op | binary_op | '{' op_list '}')
   ;

unary_op
   : ('=' | '!=' | '<' | '<=' | '>' | '>=')? (name | NUMBER)
   ;

binary_op
   : NUMBER ('<>' | '><' | ':') NUMBER
   ;

op_list
   : (unary_op | binary_op) ((',')? op_list)?
   ;

os_name
   : operating_system_name
   ;

os_list
   : os_name (',' os_name)?
   ;

flags
   : 'flags' flag_set ('/' flag_set)*
   ;

flag_set
   : ('F' | 'S' | 'R' | 'P' | 'A' | 'U' | 'E' | 'W' | 'SA')
   ;

icmp_type
   : 'icmp-type' (icmp_type_code | '{' icmp_list '}')
   ;

icmp6_type
   : 'icmp6-type' (icmp_type_code | '{' icmp_list '}')
   ;

icmp_type_code
   : (icmp_type_name | icmp_type_NUMBER) ('code' (icmp_code_name | icmp_code_NUMBER))?
   ;

icmp_list
   : icmp_type_code (',' icmp_type_code)*
   ;

tos
   : ('lowdelay' | 'throughput' | 'reliability' | ('0x')? NUMBER)
   ;

state_opts
   : state_opt (','? state_opt)*
   ;

state_opt
   : ('max' NUMBER | 'no-sync' | timeout | 'sloppy' | 'pflow' | 'source-track' (('rule' | 'global'))? | 'max-src-nodes' NUMBER | 'max-src-states' NUMBER | 'max-src-conn' NUMBER | 'max-src-conn-rate' NUMBER '/' NUMBER | 'overload' '<' STRING '>' ('flush' ('global')?)? | 'if-bound' | 'floating')
   ;

timeout_list
   : timeout (',' timeout)*
   ;

timeout
   : ('tcp.first' | 'tcp.opening' | 'tcp.established' | 'tcp.closing' | 'tcp.finwait' | 'tcp.closed' | 'udp.first' | 'udp.single' | 'udp.multiple' | 'icmp.first' | 'icmp.error' | 'other.first' | 'other.single' | 'other.multiple' | 'frag' | 'interval' | 'src.track' | 'adaptive.start' | 'adaptive.end') NUMBER
   ;

limit_list
   : limit_item (',' limit_item)*
   ;

limit_item
   : ('states' | 'frags' | 'src-nodes' | 'tables' | 'table-entries') NUMBER
   ;

pooltype
   : ('bitmask' | 'least-states' | 'random' | 'round-robin' | 'source-hash' ((hex_key | string_key))?) ('sticky-address')?
   ;

include
   : 'include' filename
   ;

operating_system_name
   : QUOTED_STRING
   ;

icmp_type_name
   : STRING
   ;

icmp_type_NUMBER
   : NUMBER
   ;

icmp_code_name
   : STRING
   ;

icmp6_code_name
   : STRING
   ;

icmp6_code_NUMBER
   : NUMBER
   ;

icmp_code_NUMBER
   : NUMBER
   ;

interface_name
   : STRING
   ;

seconds
   : NUMBER
   ;

interface_group
   : STRING
   ;

hostname
   : STRING
   ;

proto_name
   : STRING
   ;

proto_NUMBER
   : NUMBER
   ;

name
   : STRING
   ;

mask_bits
   : NUMBER
   ;

filename
   : QUOTED_STRING
   ;

hex_key
   : HEX
   ;

string_key
   : QUOTED_STRING
   ;

ARROW
   : '->'
   ;

IPV4_DOTTED_QUAD
   : NUMBER '.' NUMBER '.' NUMBER '.' NUMBER
   ;

IPV6_COLONED_HEX
   : [0-9a-f] ':' [0-9a-f] ':' [0-9a-f] ':' [0-9a-f] ':' [0-9a-f] ':' [0-9a-f] ':' [0-9a-f] ':' [0-9a-f]
   ;

NUMBER
   : [0-9]+
   ;

HEX
   : '0' ('X' | 'x') [0-9a-fA-F]+
   ;

STRING
   : '$'? [a-zA-Z] [a-zA-Z0-9_]*
   ;

QUOTED_STRING
   : '"' ~ '"'* '"'
   ;

CONTINUATION
   : '\\' -> skip
   ;

COMMENT
   : '#' ~ ('\r' | '\n')* -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

