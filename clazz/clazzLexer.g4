/*
BSD License

Copyright (c) 2017, Tom Everett
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

lexer grammar clazzLexer;


X00
   : '\u0000'
   ;


X01
   : '\u0001'
   ;


X02
   : '\u0002'
   ;


X03
   : '\u0003'
   ;


X04
   : '\u0004'
   ;


X05
   : '\u0005'
   ;


X06
   : '\u0006'
   ;


X07
   : '\u0007'
   ;


X08
   : '\u0008'
   ;


X09
   : '\u0009'
   ;


X0A
   : '\u000A'
   ;


X0B
   : '\u000B'
   ;


X0C
   : '\u000C'
   ;


X0D
   : '\u000D'
   ;


X0E
   : '\u000E'
   ;


X0F
   : '\u000F'
   ;


X10
   : '\u0010'
   ;


X11
   : '\u0011'
   ;


X12
   : '\u0012'
   ;


X13
   : '\u0013'
   ;


X14
   : '\u0014'
   ;


X15
   : '\u0015'
   ;


X16
   : '\u0016'
   ;


X17
   : '\u0017'
   ;


X18
   : '\u0018'
   ;


X19
   : '\u0019'
   ;


X1A
   : '\u001A'
   ;


X1B
   : '\u001B'
   ;


X1C
   : '\u001C'
   ;


X1D
   : '\u001D'
   ;


X1E
   : '\u001E'
   ;


X1F
   : '\u001F'
   ;


X20
   : '\u0020'
   ;


X21
   : '\u0021'
   ;


X22
   : '\u0022'
   ;


X23
   : '\u0023'
   ;


X24
   : '\u0024'
   ;


X25
   : '\u0025'
   ;


X26
   : '\u0026'
   ;


X27
   : '\u0027'
   ;


X28
   : '\u0028'
   ;


X29
   : '\u0029'
   ;


X2A
   : '\u002A'
   ;


X2B
   : '\u002B'
   ;


X2C
   : '\u002C'
   ;


X2D
   : '\u002D'
   ;


X2E
   : '\u002E'
   ;


X2F
   : '\u002F'
   ;


X30
   : '\u0030'
   ;


X31
   : '\u0031'
   ;


X32
   : '\u0032'
   ;


X33
   : '\u0033'
   ;


X34
   : '\u0034'
   ;


X35
   : '\u0035'
   ;


X36
   : '\u0036'
   ;


X37
   : '\u0037'
   ;


X38
   : '\u0038'
   ;


X39
   : '\u0039'
   ;


X3A
   : '\u003A'
   ;


X3B
   : '\u003B'
   ;


X3C
   : '\u003C'
   ;


X3D
   : '\u003D'
   ;


X3E
   : '\u003E'
   ;


X3F
   : '\u003F'
   ;


X40
   : '\u0040'
   ;


X41
   : '\u0041'
   ;


X42
   : '\u0042'
   ;


X43
   : '\u0043'
   ;


X44
   : '\u0044'
   ;


X45
   : '\u0045'
   ;


X46
   : '\u0046'
   ;


X47
   : '\u0047'
   ;


X48
   : '\u0048'
   ;


X49
   : '\u0049'
   ;


X4A
   : '\u004A'
   ;


X4B
   : '\u004B'
   ;


X4C
   : '\u004C'
   ;


X4D
   : '\u004D'
   ;


X4E
   : '\u004E'
   ;


X4F
   : '\u004F'
   ;


X50
   : '\u0050'
   ;


X51
   : '\u0051'
   ;


X52
   : '\u0052'
   ;


X53
   : '\u0053'
   ;


X54
   : '\u0054'
   ;


X55
   : '\u0055'
   ;


X56
   : '\u0056'
   ;


X57
   : '\u0057'
   ;


X58
   : '\u0058'
   ;


X59
   : '\u0059'
   ;


X5A
   : '\u005A'
   ;


X5B
   : '\u005B'
   ;


X5C
   : '\u005C'
   ;


X5D
   : '\u005D'
   ;


X5E
   : '\u005E'
   ;


X5F
   : '\u005F'
   ;


X60
   : '\u0060'
   ;


X61
   : '\u0061'
   ;


X62
   : '\u0062'
   ;


X63
   : '\u0063'
   ;


X64
   : '\u0064'
   ;


X65
   : '\u0065'
   ;


X66
   : '\u0066'
   ;


X67
   : '\u0067'
   ;


X68
   : '\u0068'
   ;


X69
   : '\u0069'
   ;


X6A
   : '\u006A'
   ;


X6B
   : '\u006B'
   ;


X6C
   : '\u006C'
   ;


X6D
   : '\u006D'
   ;


X6E
   : '\u006E'
   ;


X6F
   : '\u006F'
   ;


X70
   : '\u0070'
   ;


X71
   : '\u0071'
   ;


X72
   : '\u0072'
   ;


X73
   : '\u0073'
   ;


X74
   : '\u0074'
   ;


X75
   : '\u0075'
   ;


X76
   : '\u0076'
   ;


X77
   : '\u0077'
   ;


X78
   : '\u0078'
   ;


X79
   : '\u0079'
   ;


X7A
   : '\u007A'
   ;


X7B
   : '\u007B'
   ;


X7C
   : '\u007C'
   ;


X7D
   : '\u007D'
   ;


X7E
   : '\u007E'
   ;


X7F
   : '\u007F'
   ;


X80
   : '\u0080'
   ;


X81
   : '\u0081'
   ;


X82
   : '\u0082'
   ;


X83
   : '\u0083'
   ;


X84
   : '\u0084'
   ;


X85
   : '\u0085'
   ;


X86
   : '\u0086'
   ;


X87
   : '\u0087'
   ;


X88
   : '\u0088'
   ;


X89
   : '\u0089'
   ;


X8A
   : '\u008A'
   ;


X8B
   : '\u008B'
   ;


X8C
   : '\u008C'
   ;


X8D
   : '\u008D'
   ;


X8E
   : '\u008E'
   ;


X8F
   : '\u008F'
   ;


X90
   : '\u0090'
   ;


X91
   : '\u0091'
   ;


X92
   : '\u0092'
   ;


X93
   : '\u0093'
   ;


X94
   : '\u0094'
   ;


X95
   : '\u0095'
   ;


X96
   : '\u0096'
   ;


X97
   : '\u0097'
   ;


X98
   : '\u0098'
   ;


X99
   : '\u0099'
   ;


X9A
   : '\u009A'
   ;


X9B
   : '\u009B'
   ;


X9C
   : '\u009C'
   ;


X9D
   : '\u009D'
   ;


X9E
   : '\u009E'
   ;


X9F
   : '\u009F'
   ;


XA0
   : '\u00A0'
   ;


XA1
   : '\u00A1'
   ;


XA2
   : '\u00A2'
   ;


XA3
   : '\u00A3'
   ;


XA4
   : '\u00A4'
   ;


XA5
   : '\u00A5'
   ;


XA6
   : '\u00A6'
   ;


XA7
   : '\u00A7'
   ;


XA8
   : '\u00A8'
   ;


XA9
   : '\u00A9'
   ;


XAA
   : '\u00AA'
   ;


XAB
   : '\u00AB'
   ;


XAC
   : '\u00AC'
   ;


XAD
   : '\u00AD'
   ;


XAE
   : '\u00AE'
   ;


XAF
   : '\u00AF'
   ;


XB0
   : '\u00B0'
   ;


XB1
   : '\u00B1'
   ;


XB2
   : '\u00B2'
   ;


XB3
   : '\u00B3'
   ;


XB4
   : '\u00B4'
   ;


XB5
   : '\u00B5'
   ;


XB6
   : '\u00B6'
   ;


XB7
   : '\u00B7'
   ;


XB8
   : '\u00B8'
   ;


XB9
   : '\u00B9'
   ;


XBA
   : '\u00BA'
   ;


XBB
   : '\u00BB'
   ;


XBC
   : '\u00BC'
   ;


XBD
   : '\u00BD'
   ;


XBE
   : '\u00BE'
   ;


XBF
   : '\u00BF'
   ;


XC0
   : '\u00C0'
   ;


XC1
   : '\u00C1'
   ;


XC2
   : '\u00C2'
   ;


XC3
   : '\u00C3'
   ;


XC4
   : '\u00C4'
   ;


XC5
   : '\u00C5'
   ;


XC6
   : '\u00C6'
   ;


XC7
   : '\u00C7'
   ;


XC8
   : '\u00C8'
   ;


XC9
   : '\u00C9'
   ;


XCA
   : '\u00CA'
   ;


XCB
   : '\u00CB'
   ;


XCC
   : '\u00CC'
   ;


XCD
   : '\u00CD'
   ;


XCE
   : '\u00CE'
   ;


XCF
   : '\u00CF'
   ;


XD0
   : '\u00D0'
   ;


XD1
   : '\u00D1'
   ;


XD2
   : '\u00D2'
   ;


XD3
   : '\u00D3'
   ;


XD4
   : '\u00D4'
   ;


XD5
   : '\u00D5'
   ;


XD6
   : '\u00D6'
   ;


XD7
   : '\u00D7'
   ;


XD8
   : '\u00D8'
   ;


XD9
   : '\u00D9'
   ;


XDA
   : '\u00DA'
   ;


XDB
   : '\u00DB'
   ;


XDC
   : '\u00DC'
   ;


XDD
   : '\u00DD'
   ;


XDE
   : '\u00DE'
   ;


XDF
   : '\u00DF'
   ;


XE0
   : '\u00E0'
   ;


XE1
   : '\u00E1'
   ;


XE2
   : '\u00E2'
   ;


XE3
   : '\u00E3'
   ;


XE4
   : '\u00E4'
   ;


XE5
   : '\u00E5'
   ;


XE6
   : '\u00E6'
   ;


XE7
   : '\u00E7'
   ;


XE8
   : '\u00E8'
   ;


XE9
   : '\u00E9'
   ;


XEA
   : '\u00EA'
   ;


XEB
   : '\u00EB'
   ;


XEC
   : '\u00EC'
   ;


XED
   : '\u00ED'
   ;


XEE
   : '\u00EE'
   ;


XEF
   : '\u00EF'
   ;


XF0
   : '\u00F0'
   ;


XF1
   : '\u00F1'
   ;


XF2
   : '\u00F2'
   ;


XF3
   : '\u00F3'
   ;


XF4
   : '\u00F4'
   ;


XF5
   : '\u00F5'
   ;


XF6
   : '\u00F6'
   ;


XF7
   : '\u00F7'
   ;


XF8
   : '\u00F8'
   ;


XF9
   : '\u00F9'
   ;


XFA
   : '\u00FA'
   ;


XFB
   : '\u00FB'
   ;


XFC
   : '\u00FC'
   ;


XFD
   : '\u00FD'
   ;


XFE
   : '\u00FE'
   ;


XFF
   : '\u00FF'
   ;
