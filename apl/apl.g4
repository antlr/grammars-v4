/*
BSD License

Copyright (c) 2021, Tom Everett
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

grammar apl;

// https://en.wikipedia.org/wiki/Miscellaneous_Technical
APL_I_BEAM: '\u2336';
APL_SQUISH_QUAD :'\u2337';
APL_QUAD_DIVIDE: '\u2339';
APL_QUAD_DIAMOND: '\u233A';
APL_QUAD_JOT: '\u233B';
APL_QUAD_CIRCLE: '\u233C';
APL_CIRCLE_STILE: '\u233D';
APL_CIRCLE_JOT:'\u233E';
APL_SLASH_BAR:'\u233F';
APL_BACKSLASH_BAR: '\u2340';
APL_QUAD_SLASH: '\u2341';
APL_QUAD_BACKSLASH: '\u2342';
APL_QUAD_LESS_THAN: '\u2343';
APL_QUAD_GREATER_THAN: '\u2344';
APL_LEFTWARDS_VANE:  '\u2345';
APL_RIGHTWARDS_VANE:  '\u2346';
APL_QUAD_LEFTWARDS_ARROW: '\u2347';
APL_QUAD_RIGHTWARDS_ARROW: '\u2348';
APL_CIRCLE_BACKSLASH: '\u2349';
APL_DOWN_TACK_UNDERBAR: '\u234A';
APL_DELTA_STILE: '\u234B';
APL_QUAD_DOWN_CARET:'\u234C';
APL_QUAD_DELTA: '\u234D';
APL_DOWN_TACK_JOT: '\u234E';
APL_UPWARDS_VANE: '\u234F';
APL_QUAD_UPWARDS_ARROW: '\u2350';
APL_UP_TACK_OVERVBAR: '\u2351';
WS
   : [ \r\n\t] + -> skip
   ;
