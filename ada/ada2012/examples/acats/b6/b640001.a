-- B640001.A
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
--      Check that an abstract nondispatching operation is not considered for
--      name resolution.
--
-- TEST DESCRIPTION:
--      Ada 2005 revised 6.4(8) to ignore abstract nondispatching operations
--      for resolution. (See AI95-00310-1).
--
--      Specifically, we check cases that would be ambiguous in Ada 95.
--
-- CHANGE HISTORY:
--    4 Jun 2004   PHL   Initial version.
--   19 Dec 2014   RLB   Minor changes for issuance (name changes, comments).
--   13 Mar 2015   RLB   Eliminated tab characters.
--
--!
procedure B640001 is

    -- Functions

    -- Untagged case, AI95-00310-1 applies.

    package P1 is

        type T1 is array (1 .. 10) of Boolean;
        type T2 is access constant T1;

        function F (X : Float) return T1 is abstract;
        function F (X : Float) return T2;

        X : constant Boolean := F (2.0) (4);     -- OK.

    end P1;


    package body P1 is
        function F (X : Float) return T2 is
        begin
           return null;
        end F;
    end P1;


    -- Nastier untagged case, AI95-00310-1 applies.

    package P2 is

        type T1 is array (1 .. 10) of Boolean;
        type T2 is array (1 .. 5) of T1;

        function F (X : Integer) return T1 is abstract;
        function F return T2;

        X : constant Boolean := F (2) (4);       -- OK.

    end P2;


    package body P2 is
        function F return T2 is
        begin
           return (others => (others => False));
        end F;
    end P2;


    -- Untagged derived case, AI95-00310-1 applies.

    package P3 is

        type T1 is range 1 .. 10;

        function "+" (X, Y : T1) return Boolean;
        function F (X : T1) return Float;
        function F (X : Boolean) return Float;

        X : Float := F (T1'(1) + 1);             -- ERROR:

        type T2 is new T1 range 3 .. 3;

        function "+" (X, Y : T2) return T2 is abstract;

        Y : Float := F (3 + T2'(3));             -- OK.

    end P3;


    package body P3 is
        function "+" (X, Y : T1) return Boolean is
        begin
            return X = Y;
        end "+";
        function F (X : T1) return Float is
        begin
            return Float(X);
        end F;
        function F (X : Boolean) return Float is
        begin
            return 0.0;
        end F;
    end P3;


    -- Tagged case, dispatching operation, AI95-00310-1 does not apply.

    package P4 is

        type T1 is abstract tagged
            record
                C : Float;
            end record;
        type T2 is access T1;

        function F (X : Float) return T1 is abstract;
        function F (X : Float) return T2;

        X : Float := F (1.0).C;                  -- ERROR:

    end P4;


    package body P4 is
        function F (X : Float) return T2 is
        begin
           return null;
        end F;
    end P4;


    -- Tagged case, notdispatching operation, AI95-00310-1 applies.

    package P5 is

        type T1 is abstract tagged
            record
                C : Float;
            end record;
        type T2 is access T1;

        package Nested is
            function F (X : Float) return T1 is abstract;
            function F (X : Float) return T2;
        end Nested;

        X : Float := Nested.F (1.0).C;           -- OK.

    end P5;


    package body P5 is
        package body Nested is
            function F (X : Float) return T2 is
            begin
                return null;
            end F;
        end Nested;
    end P5;


    -- Procedures

    -- Untagged case, AI95-00310-1 applies.

    package P6 is

        type T1 is (Red, Green, Blue);

        procedure Q (X : T1) is abstract;
        package Nested is
            procedure Q (X : T1; Y : Character := '?');
        end Nested;

    end P6;

    package body P6 is
        package body Nested is
            procedure Q (X : T1; Y : Character := '?') is
            begin
                null;
            end Q;
        end Nested;
        use Nested;
    begin
        Q (Blue);                                -- OK.
    end P6;


    -- Tagged case, dispatching operation, AI95-00310-1 does not apply.

    package P7 is

        type T1 is abstract tagged
            record
                C : Float;
            end record;

        procedure Q (X : T1) is abstract;
        package Nested is
            procedure Q (X : T1; Y : Character := '?');
        end Nested;

        type T2 is new T1 with null record;
        procedure Q (X : T2);
    end P7;

    package body P7 is
        procedure Q (X : T2) is
        begin
            null;
        end Q;
        package body Nested is
            procedure Q (X : T1; Y : Character := '?') is
            begin
                null;
            end Q;
        end Nested;
        use Nested;
        function F return T1'Class is
        begin
            return T1'Class (T2'(C => 3.0));
        end F;
    begin
        Q (F);                                   -- ERROR:
    end P7;


    -- Tagged case, notdispatching operation, AI95-00310-1 applies.

    package P8 is

        type T1 is tagged
            record
                C : Float;
            end record;

        type T2 is new T1 with null record;
    end P8;

    package body P8 is
        procedure Q (X : T1) is abstract;
        package Nested is
            procedure Q (X : T1; Y : Character := '?');
        end Nested;
        package body Nested is
            procedure Q (X : T1; Y : Character := '?') is
            begin
                null;
            end Q;
        end Nested;
        use Nested;
    begin
        Q ((C => -1.0));                         -- OK.
    end P8;


begin
    null;
end B640001;
