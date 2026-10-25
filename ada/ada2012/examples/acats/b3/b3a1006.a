-- B3A1006.A
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
--*
--
-- OBJECTIVE:
--
--     Check that the name of an untagged incomplete view cannot be used as
--     the subtype_mark of a parameter in a subprogram_body, entry_body,
--     or accept_statement. Check that the name of an incomplete view cannot
--     be used as the subtype_mark of the result of a function body.
--
--     Part A: Tagged and untagged incomplete types.
--
-- TEST DESCRIPTION:
--
--     We test both normal and tagged incomplete types.
--
--     We treat this as a separate test, as any possible test will have
--     multiple errors -- the incomplete type will necessarily violate
--     freezing rules 13.14(3/3) and 3.11.1(8). Because of this, we can
--     use each type only once, and we have to allow errors to be detected
--     at multiple points. We also can't test legal tagged incomplete cases,
--     as they'll still be illegal because of freezing.
--
--     We also try an example of using an incomplete type with the Class
--     attribute as any example of such a use has other errors as well and
--     as such isn't worth a separate test.
--
-- PASS/FAIL CRITERIA:
--     The test contains several lines marked POSSIBLE ERROR: [Setnn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--     implementation to pass.
--
--     Because of the freezing rules as explained in TEST DESCRIPTION, the
--     first POSSIBLE ERROR of each set might report that the type is not
--     completed; the second POSSIBLE ERROR might report that the use of the
--     incomplete type is illegal; and the third POSSIBLE ERROR might report
--     that the completion is too late. Any combination of these is considered
--     passing. (Of course, the exact text of error messages does not figure
--     into whether a test passes or fails.)
--
-- CHANGE HISTORY:
--     13 Mar 2014  RLB  Created test from the ashes of B3A1A01 (Ada 2012
--                       required a complete redo of the objective).
--
procedure B3A1006 is

    type Untagged_Inc_1;                           -- POSSIBLE ERROR: [Set01]

    procedure Proc11 (A : Untagged_Inc_1) is       -- POSSIBLE ERROR: [Set01]
    begin
       null;
    end Proc11;

    type Untagged_Inc_1 is null record;            -- POSSIBLE ERROR: [Set01]


    type Untagged_Inc_2;                           -- POSSIBLE ERROR: [Set02]

    function Func12 return Untagged_Inc_2 is       -- POSSIBLE ERROR: [Set02]
    begin
       return (raise Program_Error); -- So we have the required return stmt.
    end Func12;

    type Untagged_Inc_2 is null record;            -- POSSIBLE ERROR: [Set02]


    type Tagged_Inc_3 is tagged;                   -- POSSIBLE ERROR: [Set03]

    function Func13 return Tagged_Inc_3 is         -- POSSIBLE ERROR: [Set03]
    begin
       return (raise Program_Error); -- So we have the required return stmt.
    end Func13;

    type Tagged_Inc_3 is tagged null record;       -- POSSIBLE ERROR: [Set03]


    type Untagged_Inc_4;                           -- POSSIBLE ERROR: [Set04]

    protected type PT4 is
        entry E (B : in out Untagged_Inc_4);       -- OK.
    private
        C : Character := 'R';
    end PT4;

    protected body PT4 is
        entry E (B : in out Untagged_Inc_4)        -- POSSIBLE ERROR: [Set04]
           when True is
        begin
           C := 'B';
        end E;
    end PT4;

    type Untagged_Inc_4 is null record;            -- POSSIBLE ERROR: [Set04]


    type Tagged_Inc_5 is tagged;                   -- POSSIBLE ERROR: [Set05]

    protected type PT5 is
        entry E (B : in out Tagged_Inc_5);         -- OK.
    private
        C : Character := 'R';
    end PT5;

    protected body PT5 is
        entry E (B : in out Tagged_Inc_5)          -- POSSIBLE ERROR: [Set05]
           when True is
        begin
           C := 'B';
        end E;
    end PT5;

    type Tagged_Inc_5 is tagged null record;       -- POSSIBLE ERROR: [Set05]


    type Untagged_Inc_6;                           -- POSSIBLE ERROR: [Set06]

    task type Tsk6 is
        entry E (B : in out Untagged_Inc_6);       -- OK.
    end Tsk6;

    task body Tsk6 is
    begin
        accept E (B : in out Untagged_Inc_6) do    -- POSSIBLE ERROR: [Set06]
           null;
        end E;
    end Tsk6;

    type Untagged_Inc_6 is null record;            -- POSSIBLE ERROR: [Set06]


    type Tagged_Inc_7 is tagged;                   -- POSSIBLE ERROR: [Set07]

    task type Tsk7 is
        entry E (B : in out Tagged_Inc_7);         -- OK.
    end Tsk7;

    task body Tsk7 is
    begin
        accept E (B : in out Tagged_Inc_7) do      -- POSSIBLE ERROR: [Set07]
           null;
        end E;
    end Tsk7;

    type Tagged_Inc_7 is tagged null record;       -- POSSIBLE ERROR: [Set07]


    type Tagged_Inc_8 is tagged;                   -- POSSIBLE ERROR: [Set08]

    function Func18 return Tagged_Inc_8'Class is   -- POSSIBLE ERROR: [Set08]
    begin
       return (raise Program_Error); -- So we have the required return stmt.
    end Func18;

    type Tagged_Inc_8 is tagged null record;       -- POSSIBLE ERROR: [Set08]


begin
   null;
end B3A1006;
