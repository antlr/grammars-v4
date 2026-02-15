-- B611012.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- OBJECTIVE:
--     Check that the prefix of an Old attribute cannot contain a Result
--     attribute.
--
--     Check that the prefix of an Old attribute cannot contain another Old
--     attribute.
--
--     Check that the prefix of an Old attribute cannot contain the loop
--     parameter of an enclosing quantified expression.
--
-- CHANGE HISTORY:
--     26 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
package B611012 is

    -- 'Result checks:

    function Foo (N : in Natural) return Natural is (N*2);

    function F01 (N : in Natural) return Natural
       with Post => F01'Result = N'Old;                       -- OK. {21;1}

    function F02 (N : in Natural) return Natural
       with Post => F02'Result'Old = N;                       -- ERROR: {21;5}

    function F03 (S : in String; N : in Natural)
       return Natural
       with Post => S(F03'Result)'Old in 'A' .. 'F';          -- ERROR: {21;15}

    function F04 (N : in Natural) return Natural
       with Post => N = Foo(F04'Result)'Old;                  -- ERROR: {25;1}

    function F05 (N : in Natural) return Natural
       with Post => N = Foo(F01(F05'Result/2))'Old;           -- ERROR: {25;1}

    -- 'Old 'Old checks:

    procedure P06 (N : in out Natural)
       with Post => N = N'Old * 2;                            -- OK. {25;5}

    procedure P07 (N : in out Natural)
       with Post => N = Natural'(N'Old * 2)'Old;              -- ERROR: {25;1}

    procedure P08 (N : in out Natural)
       with Post => N'Old'Old = N;                            -- ERROR: {21;5}

    procedure P09 (S : in String; N : in out Natural)
       with Post => S(N'Old)'Old in '0' .. '9' | 'A' .. 'Z';  -- ERROR: {21;28}

    procedure P10 (N : in out Natural)
       with Post => Foo(N'Old)'Old in 1 .. 92;                -- ERROR: {21;12}

    procedure P11 (N : in out Natural)
       with Post => Foo(F01(N'Old / 2) - 4)'Old in 1 .. 92;   -- ERROR: {21;12}

    -- Quantified expr:

    procedure P12 (S : in out String)
       with Post =>
          (for all I in S'range => S'Old(I) in 'A' .. 'Z');   -- OK. {36;19}

    procedure P14 (S : in out String)
       with Post =>
          (for all I in S'range => S(I)'Old in 'A' .. 'Z');   -- ERROR: {36;16}

    procedure P15(S : in out String)
       with Post =>
          (for all I in S'range => S(I'Old) in 'A' .. 'Z');   -- ERROR: {38;17}

    procedure P16(S : in out String)
       with Post =>
          (for all I in S'range =>
                               S(Foo(I))'Old in 'A' .. 'Z');  -- ERROR: {36;16}

end B611012;


