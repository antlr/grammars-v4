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
/*
*  Use encoding 'ISO_8859_1' not 'UTF-8'
*/

parser grammar clazzParser;

options
   { tokenVocab = clazzLexer; }

clazz
   : magic minor_version major_version constant_pool access_flags this_class super_class
     interfaces fields methods attributes EOF
   ;

minor_version
   : u2
   ;

major_version
   : u2
   ;

constant_pool_count returns [int value]
   : count=u2 {$value = $count.value;}
   ;

magic
   : XCA XFE XBA XBE
   ;

constant_pool locals [int remaining]
   : count=constant_pool_count {$remaining = $count.value - 1;}
     ({$remaining > 0}? item=constant {$remaining -= $item.slots;})*
     {$remaining == 0}?
   ;

constant returns [int slots = 1]
   : X01 length=u2 bytes[$length.value]
   | (X03 | X04) u4
   | (X05 | X06) u4 u4 {$slots = 2;}
   | (X07 | X08 | X10 | X13 | X14) u2
   | (X09 | X0A | X0B | X0C | X11 | X12) u2 u2
   | X0F u1 u2
   ;

access_flags
   : u2
   ;

this_class
   : u2
   ;

super_class
   : u2
   ;

interfaces locals [int remaining]
   : count=u2 {$remaining = $count.value;}
     ({$remaining > 0}? u2 {$remaining--;})*
     {$remaining == 0}?
   ;

fields locals [int remaining]
   : count=u2 {$remaining = $count.value;}
     ({$remaining > 0}? field_info {$remaining--;})*
     {$remaining == 0}?
   ;

methods locals [int remaining]
   : count=u2 {$remaining = $count.value;}
     ({$remaining > 0}? method_info {$remaining--;})*
     {$remaining == 0}?
   ;

field_info
   : access_flags u2 u2 attributes
   ;

method_info
   : access_flags u2 u2 attributes
   ;

attributes locals [int remaining]
   : count=u2 {$remaining = $count.value;}
     ({$remaining > 0}? attribute_info {$remaining--;})*
     {$remaining == 0}?
   ;

attribute_info
   : u2 length=u4 bytes[$length.value]
   ;

bytes[long remaining]
   : ({$remaining > 0}? u1 {$remaining--;})* {$remaining == 0}?
   ;

u1 returns [int value]
   : b=. {$value = $b.text.charAt(0);}
   ;

u2 returns [int value]
   : high=u1 low=u1 {$value = ($high.value << 8) | $low.value;}
   ;

u4 returns [long value]
   : high=u2 low=u2 {$value = ((long) $high.value << 16) | $low.value;}
   ;
