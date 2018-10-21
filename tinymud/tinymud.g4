/*
BSD License

Copyright (c) 2018, Tom Everett
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

grammar tinymud;

prog
   : (line? EOL) + line?
   ;

line
   : chowncommand
   | createcommand
   | describecommand
   | digcommand
   | failcommand
   | findcommand
   | linkcommand
   | lockcommand
   | namecommand
   | ofailcommand
   | opencommand
   | osuccesscommand
   | setcommand
   | successcommand
   | unlinkcommand
   | unlockcommand
   ;

chowncommand
   : '@chown' object '=' player
   ;

createcommand
   : '@create' name ('=' cost)?
   ;

describecommand
   : '@describe' object ('=' description)?
   ;

digcommand
   : '@dig' name
   ;

failcommand
   : '@fail' name ('=' description)?
   ;

findcommand
   : '@find' name?
   ;

linkcommand
   : '@link' object '=' (number | dir | room)
   ;

lockcommand
   : '@lock' object '=' key
   ;

namecommand
   : '@name' object '=' name password?
   ;

ofailcommand
   : '@ofail' object ('=' message)?
   ;

opencommand
   : '@open' dir (';' dir)* ('=' number)?
   ;

osuccesscommand
   : '@osuccess' object ('=' message)?
   ;

setcommand
   : '@set' object '=' '!'? flag
   ;

successcommand
   : '@success' object ('=' message)?
   ;

unlinkcommand
   : '@unlink' dir
   ;

unlockcommand
   : '@unlock' object
   ;

object
   : STRING
   ;

player
   : STRING
   ;

name
   : STRING
   ;

description
   : STRING
   ;

cost
   : NUMBER
   ;

key
   : NUMBER
   ;

password
   : STRING
   ;

message
   : STRING
   ;

dir
   : STRING
   ;

number
   : NUMBER
   ;

room
   : STRING
   ;

flag
   : NUMBER
   ;


STRING
   : [a-zA-Z] [a-zA-Z0-9_]*
   ;


NUMBER
   : [0-9] +
   ;


EOL
   : [r\n]
   ;


WS
   : [ \t] -> skip
   ;
