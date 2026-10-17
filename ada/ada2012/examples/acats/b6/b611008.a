-- B611008.A
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
--    Check F'Result rules:
--       (1) The prefix of a Result attribute cannot be a procedure or entry.
--       (2) The prefix of a Result attribute cannot be an object.
--       (3) The prefix of a Result attribute cannot be a type, package, task,
--           or protected type.
--       (4) F'Result is not allowed in the postcondition expression for
--           some other function.
--       (5) F'Result is not allowed in a precondition expression for F.
--
-- CHANGE HISTORY:
--     24 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
package B611008 is

    -- Various entities to use in following tests:

    Obj01 : Natural;

    procedure P02 (N : in out Natural);

    type Tiny03 is range 0 .. 99;

    task type Tsk04 is
       entry E04 (N : in out Natural);
    end Tsk04;

    protected type Prot05 is
       function F05 return Natural;
       procedure P05 (N : in out Natural);
       entry E05 (N : in out Natural);
    end Prot05;

    task Tsk06 is
       entry E06 (N : in out Natural);
    end Tsk06;

    protected Prot07 is
       function F07 return Natural;
       procedure P07 (N : in out Natural);
       entry E07 (N : in out Natural);
    end Prot07;

    package Pack08 is
       function F08 return Natural;
       procedure P08 (N : in out Natural);
    end Pack08;

    -- Tests:

    function F10 (N : in Natural) return Natural
       with Post => F10'Result > N;                        -- OK. {21;5}

    procedure P11 (N : in out Natural)
       with Post => P11'Result > N;                        -- ERROR: (1) {21;5}

    procedure P12 (N : in out Natural)
       with Post => Prot07.E07'Result > N;                 -- ERROR: (1) {21;5}

    procedure P13 (N : in out Natural)
       with Post => Obj01'Result > N;                      -- ERROR: (2) {21;5}

    function F14 (N : in Natural) return Natural
       with Post => Tsk06'Result > N;                      -- ERROR: (3) {21;5}

    function F15 (T : in Tiny03) return Tiny03
       with Post => Tiny03'Result > T;                     -- ERROR: (3) {21;5}

    function F16 (N : in Natural) return Natural
       with Post => Pack08'Result > N;                     -- ERROR: (3) {21;5}

    function F17 (N : in Natural) return Natural
       with Post => Prot07'Result > N;                     -- ERROR: (3) {21;5}

    protected type Prot18 is
       function F18 (N : in Natural) return Natural
          with Post => F18'Result > N;                     -- OK. {24;5}

       procedure P18 (N : in out Natural)
          with Post => P18'Result > N;                     -- ERROR: (1) {24;5}

       entry E18 (N : in out Natural)
          with Post => E18'Result > N;                     -- ERROR: (1) {24;5}
    private
       M : Natural;
    end Prot18;

    task type Tsk19 is
       entry E19 (N : in out Natural)
          with Post => E19'Result > N;                     -- ERROR: (1) {24;5}
    end Tsk19;

    function F20 (N : in Natural) return Natural
       with Post => F10'Result > N;                        -- ERROR: (4) {21;5}

    function F21 (N : in Natural) return Natural
       with Post => Prot07.F07'Result > N;                 -- ERROR: (4) {21;5}

    function F22 (N : in Natural) return Natural
       with Post => Pack08.F08'Result > N;                 -- ERROR: (4) {21;5}

    function F23 (N : in Natural) return Natural
       with Pre => F23'Result > N;                         -- ERROR: (5) {20;5}

    procedure P24 (N : in out Natural)
       with Post => F20'Result > N;                        -- ERROR: (4) {21;5}

    protected type Prot25 is
       function F25 (N : in Natural) return Natural
          with Post => F25'Result > N;                     -- OK. {24;5}

       function F251 (N : in Natural) return Natural
          with Post => F25'Result > N;                     -- ERROR: (4) {24;5}

       function F252 (N : in Natural) return Natural
          with Post => Pack08.F08'Result > N;              -- ERROR: (4) {24;5}

       function F253 (N : in Natural) return Natural
          with Pre => F253'Result > N;                     -- ERROR: (5) {23;5}

       procedure P25 (N : in out Natural)
          with Post => F25'Result > N;                     -- ERROR: (4) {24;5}

       entry P254 (N : in out Natural)
          with Post => F25'Result > N;                     -- ERROR: (4) {24;5}
    private
       M : Natural;
    end Prot25;


    type Root is tagged null record;

    function F40 (Obj : in Root; N : in Natural) return Natural
       with Post'Class => F40'Result > N;                  -- OK. {27;5}

    procedure P41 (Obj : in out Root; N : in out Natural)
       with Post'Class => P41'Result > N;                  -- ERROR: (1) {27;5}

    procedure P42 (Obj : in out Root; N : in out Natural)
       with Post'Class => Obj01'Result > N;                -- ERROR: (2) {27;5}

    function F43 (Obj : in Root; N : in Natural) return Natural
       with Post'Class => F40'Result > N;                  -- ERROR: (4) {27;5}

    function F44 (Obj : in Root; N : in Natural) return Natural
       with Pre'Class => F44'Result > N;                   -- ERROR: (5) {26;5}

end B611008;


