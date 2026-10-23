-- B3A10042.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--     See B3A10040.A.
--
-- TEST DESCRIPTION
--     See B3A10040.A.
--
-- TEST FILES:
--     This test consists of the following files:
--         B3A10040.A
--         B3A10041.A
--      -> B3A10042.A
--
-- PASS/FAIL CRITERIA:
--     See B3A10040.A.
--
-- CHANGE HISTORY:
--     29 May 2008  RLB  Created test based on submitted tests.
--!

with Ada.Strings.Unbounded;
with B3A10040;
package body B3A10042 is
    type T21 is (X);                                        -- ERROR:
    type T22 is range -100 .. 100;                          -- ERROR:
    type T23 is digits 6;                                   -- ERROR:
    type T24 is delta 0.125 range -10.0 .. 10.0;            -- ERROR:
    type T25 is access constant B3A10040.TPriv;             -- ERROR:
    type T26 is array (Positive range <>) of Character;     -- ERROR:
    type T27 is record B : Boolean; end record;             -- ERROR:
    type T28 is tagged null record;                         -- OK.
    protected type T29 is                                   -- ERROR:
       procedure Foo;
    private
       N : Natural;
    end T29;
    protected type T2A is new B3A10040.Interf with          -- OK.
       procedure Foo;
    private
       N : Natural;
    end T2A;
    task type T2B;                                          -- ERROR:
    task type T2C is new B3A10040.Interf with end T2C;      -- OK.
    type T2D is limited interface;                          -- OK.
    type T2E is new B3A10040.UPriv;                         -- ERROR:
    type T2F is new B3A10040.TPriv with null record;        -- OK.
    type T2G is new Ada.Strings.Unbounded.Unbounded_String; -- ERROR:

    subtype T2H is B3A10040.TPriv;                          -- ERROR:
       -- Can't complete with a subtype.

    protected body T29 is                                   -- OPTIONAL ERROR:
       procedure Foo is begin null; end Foo;
    end T29;

    protected body T2A is
       procedure Foo is begin null; end Foo;
    end T2A;

    procedure Proc1 is
        -- Try a sample of these cases:
        type T31 is tagged;                          -- POSSIBLE ERROR: [Set31]
        type T32 is tagged;                          -- POSSIBLE ERROR: [Set32]
        type T33 is tagged;                          -- POSSIBLE ERROR: [Set33]
        type T34 is tagged;                          -- POSSIBLE ERROR: [Set34]
        type T35 is tagged;                          -- POSSIBLE ERROR: [Set35]
        type T36 is tagged;                          -- POSSIBLE ERROR: [Set36]

        type T31 is range -200 .. 200;               -- POSSIBLE ERROR: [Set31]
        type T32 is access constant B3A10040.TPriv;  -- POSSIBLE ERROR: [Set32]
        type T33 is record B : Boolean; end record;  -- POSSIBLE ERROR: [Set33]
        protected type T34 is                        -- POSSIBLE ERROR: [Set34]
           procedure Foo;
        private
           N : Natural;
        end T34;
        type T35 is new B3A10040.UPriv;              -- POSSIBLE ERROR: [Set35]
        type T36 is new Ada.Strings.Unbounded.Unbounded_String; -- POSSIBLE ERROR: [Set36]
        protected body T34 is                        -- OPTIONAL ERROR:
           procedure Foo is begin null; end Foo;
        end T34;
    begin
        declare
            -- Try a sample of these cases:
            type T41 is tagged;                      -- POSSIBLE ERROR: [Set41]
            type T42 is tagged;                      -- POSSIBLE ERROR: [Set42]
            type T43 is tagged;                      -- POSSIBLE ERROR: [Set43]
            type T44 is tagged;                      -- POSSIBLE ERROR: [Set44]
            type T45 is tagged;                      -- POSSIBLE ERROR: [Set45]

	    type T41 is new Character;               -- POSSIBLE ERROR: [Set41]
            type T42 is access all B3A10040.TPriv;   -- POSSIBLE ERROR: [Set42]
            type T43 is new String(1..10);           -- POSSIBLE ERROR: [Set43]
            type T44 is new B3A10040.UPriv;          -- POSSIBLE ERROR: [Set44]
            type T45 is new Ada.Strings.Unbounded.Unbounded_String; -- POSSIBLE ERROR: [Set45]
       begin
           null;
       end;
    end Proc1;

    task body T2B is                                 -- OPTIONAL ERROR:
    begin
        null;
    end T2B;

    task body T2C is
        -- Try a sample of these cases:
        type T51 is tagged;                          -- POSSIBLE ERROR: [Set51]
        type T52 is tagged;                          -- POSSIBLE ERROR: [Set52]
        type T53 is tagged;                          -- POSSIBLE ERROR: [Set53]
        type T54 is tagged;                          -- POSSIBLE ERROR: [Set54]
        type T55 is tagged;                          -- POSSIBLE ERROR: [Set55]

        type T51 is (False, True, Undecidable);      -- POSSIBLE ERROR: [Set51]
        type T52 is access all B3A10040.TPriv;       -- POSSIBLE ERROR: [Set52]
        type T53 is null record;                     -- POSSIBLE ERROR: [Set53]
        type T54 is new B3A10040.UPriv;              -- POSSIBLE ERROR: [Set54]
        type T55 is new Ada.Strings.Unbounded.Unbounded_String; -- POSSIBLE ERROR: [Set55]
    begin
        null;
    end T2C;

end B3A10042; -- ERROR: T2J (from spec.) undefined.
