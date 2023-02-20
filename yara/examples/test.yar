/* comment */

include "r1.yar"

import "pe"
import "cuckoo" /*text*/

include "r1.yar"
include "r1.yar" /*text*/

//comment

rule _d0
{
 strings:
  $a = { 11 }
  $b = { ?? }
  $c = { ?? ?? }
  $d = { ?? ?? 0? }
  $e = { ?? ?? ?0 }
  $f = "text"
  $g = "te\nxt"
 condition:
  1 of them
}

private rule _dummy3 : Tag1 Tag2 //comment
{
    meta: /*text*/
        i1 = "a" //comment
        i2 = "b" /*text*/
        i3 = 0x00
        i4 = 0
        i5 = false

    strings: /*text*/
        $text_1 = "test" private /*text*/
        $text_1a = "test" private base64
        $text_1b = "test" private base64(" ")

        $text_11 = { ff } private nocase
        $text_6 = { 00 } nocase
        $text_6 = { 00 11 } ascii
        $text_6 = { ?0 0f fD ?? a? } xor

        $text_2 = { a1 ff [-/*text*/] a1 ff } base64wide
        $text_5 = { a1 ff [1] ?? ff } fullword
        $text_3 = { a1 ff [1-] a2 ff /*text*/ }
        $text_4 = { a1 ff [1-2] a2 ff } /*text*/

        $text_1 = { ( ff ff /*text*/ | fa ff | ?? ) }
        $text_1 = { ff ( ff ff | fa ff /*text*/ | ?? ) ff }
        /*text*/
    condition: /*text*/
        false /*text*/
}

include "r1.yar"

global rule dummy1
{
    condition:
        false //and false
          and va contains "tt"
          and va matches "ss"
}

rule dummy2
{
    meta:
        id1 = "txt1"
    condition:
        false //and false
}

rule dummy3
{
    meta:
        id1 = "txt1"
    strings:
        $s = "t"
    condition:
        false //and false
}

rule r4{
 meta:
  p = "v"
 strings: /**/
  $a = "1"
  $b = { 0f ?? ?f f? }
  $c = { ff [-] f? }
  $c = { (?a | b?) [1-][2] (?? | 0d) f? }
  $a = "1"  /* comment */
  $b = { ff  /* comment */ }
  $c = { ff [1 /* jump */ ] (aa|bb /* alt */) ff }
 condition: /* comment */
  /* test */
  $a[1] == 1  /* comment */
  and uint16(0) == 0x5A4D  /* comment */
  and uint16be(0) == 0x5A4D  /* comment */
  and int16(0) == 0x5A4D  /* comment */
  and int16be(0) == 0x5A4D  /* comment */
  and uint32(uint32(0x3C)) == 0x00004550  /* comment */
  and uint32be(uint32(0x3C)) == 0x00004550  /* comment */
  and int32(uint32(0x3C)) == 0x00004550  /* comment */
  and int32be(uint32(0x3C)) == 0x00004550  /* comment */
  and $a at 100
  and #a == 1
  and @a[1] == 0x00
  and @a == 0x00
  and !a == 1
  and !a[1] == 1
  and $a in (entrypoint..entrypoint + 10)
  and $a at entrypoint + 2
  and $a at pe.entry_point
  and 1 of ($a*)
  and 1 of ($*)
  and none of ($a*, $b*) in (1..100)
  and all of ($a*, $b*) in (1..100)
  and any of ($a*, $b*) in (1..100)
  and 2 of them
  and $a
}

rule rf1
{
 strings:
    $a = "t"
 condition:
  for any of ($a) : ( $ )
}

rule rf2
{
 strings:
    $a = "t"
 condition:
  for any of ($a) : ( $ at pe.entry_point )
}

rule rf3
{
 strings:
    $a = "t"
 condition:
  for any of ($a,$b) : ( # > 3 )
}

rule rf4
{
 strings:
    $a = "t"
 condition:
  for any of ($a,$b) : ( @ > 3 )
}

rule rf5
{
 strings:
    $a = "t"
 condition:
  for any i in (1..#a) : ( @a[i] + 10 == @b[i] )
}

rule rf6
{
 strings:
    $a = "t"
 condition:
  for any section in pe.sections : ( section.name == ".text" )
}

rule r5
{
 strings:
    $ = "t"
    $ = "e"
 condition:
  1 of them
}

include "r1.yar"

rule rr1
{
 strings:
  $a = "1"
  $b = "2"

 condition:
  $a
}

rule rr2
{
 strings:
  $a = "1"
 condition:
  any of (rr*)
}

include "r1.yar"
