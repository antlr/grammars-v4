-- B611014.A
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
--     Check that an Old attribute reference is illegal if the prefix does not
--     statically denote an object, and the use of Old appears in:
--        (A) any part of an if expression other than the first condition;
--        (B) the dependent expression of a case expression;
--        (C) the predicate of a quantified expression;
--        (D) the right operand of a short circuit control form;
--        (E) a membership choice other than the first in a membership
--            operation.
--
-- TEST DESCRIPTION:
--     The definition of "statically denotes" is found in 4.9(14-17).
--     For an entity to be statically denoted, the name has to be a direct
--     name, expanded name, or character literal; and if the name denotes
--     a renaming declaration, the renamed entity has to be a name that
--     statically denotes a declaration. (It can also be an attribute, we're
--     ignoring that possibility here.)
--
--     In particular, neither a selected_component nor an indexed_component
--     ever statically denote anything. That's true even if a selected
--     component is of a normal record component that does not depend on
--     discriminants, and even if an indexed_component has a static index
--     expression.
--
--     However, the ARG is considering relaxing this rule somehow; therefore
--     we restrict this test to cases where evaluating the prefix might
--     fail.
--
--     Thus, we try prefixes of 'Old that are:
--        (1) selected_components for a depends-on-a-discriminant record
--            component;
--        (2) a dereference of an access type;
--        (3) indexed_components with an index of a global variable;
--        (4) indexed_components with an index of a parameter;
--        (5) a function call.
--
-- CHANGE HISTORY:
--     15 Nov 2016   RLB   Created test.
--     16 Nov 2016   RLB   Corrected two test cases. Redid some test cases
--                         to ensure that they are not covered by any proposed
--                         change to these rules.
--     20 Jan 2016   RLB   Corrected error location code.
--!
package B611014 is

    Glob : Natural;

    function Fun (N : in Natural) return Natural is (N*2);

    type Arr is array (1..10) of Natural;

    Table : Arr;

    type Rec (D : Boolean) is record
       C : Character;
       case D is
           when True => N : Natural;
           when False => null;
       end case;
    end record;

    type Giant is (Fee, Fi, Fo, Fum);

    type Root is tagged record
        N : Natural;
    end record;


    procedure P01 (N : in out Natural)
       with Post => (if N > 0 then Table(N)'Old = 1);      -- ERROR: A4 {36;6}
       -- This was one of the motivating examples for this rule.

    procedure P02 (N : access Natural)
       with Post => (if N /= null then N.all'Old = 1);     -- ERROR: A2 {40;6}
       -- This was another motivating example for this rule.

    procedure P03 (R : in out Rec)
       with Post => (if R.D then R.N'Old = 1);             -- ERROR: A1 {34;6}
       -- This was another motivating example for this rule.

    procedure P04 (N : in out Positive)
       with Post => (if Table(N)'Old > 0 then N = 1);      -- OK. A4 {25;17}
       -- First condition is not checked.

    procedure P05 (N : in out Natural)
       with Post => (if N > 0 then N'Old > 1);             -- OK. A {36;5}
       -- Prefix of Old statically denotes N.

    procedure P06 (N : in out Natural)
       with Post => (if N > 0 then Table(N) = 1
                     elsif Fun(N)'Old = 0 then N = 1);     -- ERROR: A5 {28;17}
       -- In second condition.

    procedure P07 (R : in out Rec; N : in Natural)
       with Post => (if N > 0 then R.D
                     elsif N = 0 then R.N'Old = R.N);      -- ERROR: A1 {39;8}
       -- In second condition.

    procedure P08 (R : in out Rec; N : in Natural)
       with Post => (if N > 0 then R.D
                     elsif N = 0 then R'Old.N = R.N);      -- OK. A1 {39;8}
       -- In second condition.

    procedure R01 (Obj : in Root; N : in out Natural)
       with Post'Class => (if N > 0 then
                         Table(Glob)'Old = 1);             -- ERROR: A3 {26;6}


    procedure P10 (G : in out Giant; R : in out Rec)
       with Post => (case G is
                       when Fee => R.N'Old = 2,            -- ERROR: B1 {36;5}
                       when others => True);

    procedure P11 (G : in out Giant; R : access Rec)
       with Post => (case G is
                       when Fum => R.all'Old.C = 'A',      -- ERROR: B2 {36;7}
                       when others => True);

    procedure P12 (G : in out Giant; N : in out Natural)
       with Post => (case G is
                       when Fee => N'Old > 0,              -- OK. {36;5}
                       when Fi  => N'Old < 0,              -- OK. {36;5}
                       when Fo  => Table(N)'Old = 1,       -- ERROR: B4 {36;5}
                       when Fum  => G'Old /= Fum);         -- OK. {37;9}

    procedure P13 (G : in out Giant; N : in out Natural)
       with Post => (case G is
                       when Fee | Fi | Fo => N'Old > 0,    -- OK. {46;5}
                       when Fum => Table(Glob)'Old = 1);   -- ERROR: B3 {36;6}

    procedure P14 (G : in out Giant; N : in out Natural)
       with Post => (case G is
                       when Fee | Fi => Fun(N)'Old > 0,    -- ERROR: B5 {41;5}
                       when others => True);

    procedure P15 (N : in out Natural)
       with Post => (case Fun(N)'Old is                    -- OK. {27;3}
                       when 1 | 2 | 3 => N'Old > 0,        -- OK. {42;5}
                       when others => True);

    procedure R11 (Obj : in Root; G : in out Giant; N : in out Natural)
       with Post'Class => (case G is
                       when Fee | Fi | Fo => N'Old > 0,    -- OK. {46;5}
                       when Fum => Table(N)'Old = 1);      -- ERROR: B4 {36;6}


    procedure P20 (R : in out Rec)
       with Post => (for all I in 1.. 10 => R.N'Old = 1);  -- ERROR: C1 {45;6}

    procedure P21 (N : in out Natural)
       with Post => (for some I in 1.. 10 =>
                                        Table(N)'Old = 1); -- ERROR: C4 {41;6}

    procedure P22 (R : access Rec)
       with Post => (for some I in 1.. 10 => R.all'Old.D); -- ERROR: C2 {46;2}

    procedure P23 (N : in out Natural)
       with Post => (for all I in 1.. N => Fun(N)'Old = 1);-- ERROR: C5 {44;6}

    procedure P24 (N : in out Natural)
       with Post => (for some I in 1.. N => N'Old = 1);    -- OK. C {45;6}

    procedure R21 (Obj : in Root; N : in out Natural)
       with Post'Class =>
                (for all I in 1.. 8 => Fun(N)'Old = 1);    -- ERROR: C5 {41;6}


    procedure P30 (N : in out Natural)
       with Post => (N > 0 and then Table(N)'Old = 1);     -- ERROR: D4 {37;6}
       -- This was another of the motivating examples for this rule.

    procedure P31 (N : access Natural)
       with Post => (N /= null and then N.all'Old = 1);    -- ERROR: D2 {41;6}
       -- This was another motivating example for this rule.

    procedure P32 (R : in out Rec)
       with Post => (R.D and then R.N'Old = 1);            -- ERROR: D1 {35;6}
       -- This was another motivating example for this rule.

    procedure P33 (R : in out Rec)
       with Post => (Fun(Boolean'Pos(R.D))'Old = 12        -- OK. D5 {22;5}
                                    and then R.N = 1);
       -- Left operand is not checked.

    procedure P34 (N : in out Natural)
       with Post => (N > 0 or else N'Old = 1);             -- OK. D {36;6}

    procedure P35 (N : in out Natural)
       with Post => (N > 0 or else Fun(N)'Old = 0);        -- ERROR: D5 {36;6}

    procedure P36 (N : in out Natural)
       with Post => (N > 0 or else
                         Table(Glob)'Old = 1);             -- ERROR: D3 {26;6}


    procedure R31 (Obj : in Root; N : in out Natural)
       with Post'Class => (N > 0 and then
                           Table(Glob)'Old = 1);           -- ERROR: D3 {26;6}


    procedure P40 (N : in out Natural)
       with Post => (N in 0 | Table(N)'Old);               -- ERROR: E4 {31;2}

    procedure P41 (N : in out Natural)
       with Post => (N in Table(N)'Old | 1);               -- OK. E4 {27;6}

    procedure P42 (R : in out Rec)
       with Post => (R.D in True | (R.N'Old = 4));         -- ERROR: E1 {37;7}

    procedure P43 (R : access Rec)
       with Post => (R.D in True | (R.all'Old.C = 'B'));   -- ERROR: E2 {37;9}

    procedure P44 (N : in out Natural)
       with Post => (N in 0 | Fun(N)'Old);                 -- ERROR: E5 {31;2}

    procedure P45 (N : in out Natural)
       with Post => (N in Fun(N)'Old | 1);                 -- OK. E5 {27;6}

    procedure R41 (Obj : in Root; N : in out Natural)
       with Post'Class => (Obj.N in 1 | Table(Glob)'Old);  -- ERROR: E3 {41;2}

    procedure R42 (Obj : in Root; N : in out Natural)
       with Post'Class => (Obj'Old.N in 1 | Table(Glob));  -- OK. E {28;21}

    procedure R43 (Obj : in Root; N : in out Natural)
       with Post'Class => (Obj.N in Table(Glob)'Old | 2);  -- OK. E3 {37;6}

end B611014;


