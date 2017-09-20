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

http://www.gtin.info/

*/

grammar gtin;

gtin
   : (gtin8 | gtin12 | gtin13 | gtin14 | supplemental_code) EOF
   ;

gtin8
   : ean8
   ;

ean8
   : any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit
   ;

gtin12
   : upc
   ;

gtin13
   : ean13
   ;

gtin14
   : ean14
   ;

upc
   : (upc_a | upc_e)
   ;

// 12 digits (1+5+5+1)
upc_a
   : num_system upc_a_manufacturer upc_a_product check_code
   ;

upc_a_manufacturer
   : upc_a_5
   ;

upc_a_product
   : upc_a_5
   ;

upc_a_5
   : any_digit any_digit any_digit any_digit any_digit
   ;

upc_e
   : any_digit any_digit any_digit any_digit any_digit any_digit
   ;

num_system
   : any_digit
   ;

check_code
   : any_digit
   ;

supplemental_code
   : supplemental_code_5
   | supplemental_code_2
   ;

supplemental_code_5
   : any_digit any_digit any_digit any_digit any_digit
   ;

supplemental_code_2
   : any_digit any_digit
   ;

// 13 digits (3+9+1)
ean13
   : (ean13_ismn | ean13_issn | ean13_bookland | ean13_generic)
   ;

ean13_generic
   : gs1_prefix ean_13_manprod check_code
   ;

// (4+9)
ean13_ismn
   : gs1_prefix_ismn ismn_publisher_number ismn_item_number check_code
   ;

// 4 digits
gs1_prefix_ismn
   : '9' '7' '9' '0'
   ;

// 4 digits
ismn_publisher_number
   : any_digit any_digit any_digit any_digit
   ;

// 4 digits
ismn_item_number
   : any_digit any_digit any_digit any_digit
   ;

ean13_bookland
   : (gs1_prefix_bookland_1 | gs1_prefix_bookland_2) bookland_isbn
   ;

// 9 digits that form the ISBN
bookland_isbn
   : any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit
   ;

// 4 digits
gs1_prefix_bookland_1
   : '9' '7' '9' ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')
   ;

gs1_prefix_bookland_2
   : '9' '7' '8' any_digit
   ;

gs1_prefix_issn
   : '9' '7' '7'
   ;

// (3+9+1)
ean13_issn
   : gs1_prefix_issn issn check_code
   ;

// 9 digits that form the ISSN
issn
   : any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit
   ;

// 9 digits in two groups of variable length
ean_13_manprod
   : any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit
   ;

gs1_prefix
   : any_digit any_digit any_digit
   ;

ean14
   : ean14_packaging ean14_product check_code
   ;

// 2 digits
ean14_appid
   : any_digit any_digit
   ;

// 1 digit
ean14_packaging
   : ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8')
   ;

// 12 digits
ean14_product
   : any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit any_digit
   ;

any_digit
   : DIGIT_0
   | DIGIT_1
   | DIGIT_2
   | DIGIT_3
   | DIGIT_4
   | DIGIT_5
   | DIGIT_6
   | DIGIT_7
   | DIGIT_8
   | DIGIT_9
   ;


DIGIT_0
   : '0'
   ;


DIGIT_1
   : '1'
   ;


DIGIT_2
   : '2'
   ;


DIGIT_3
   : '3'
   ;


DIGIT_4
   : '4'
   ;


DIGIT_5
   : '5'
   ;


DIGIT_6
   : '6'
   ;


DIGIT_7
   : '7'
   ;


DIGIT_8
   : '8'
   ;


DIGIT_9
   : '9'
   ;


HYPHEN
   : '-' -> skip
   ;


WS
   : [ \t\r\n] + -> skip
   ;
