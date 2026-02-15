-- BA110142.A
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
-- OBJECTIVE
--      See BA110140.A.
--
-- TEST DESCRIPTION
--      See BA110140.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         BA110140.A
--         BA110141.A
--      -> BA110142.A
--
-- PASS/FAIL CRITERIA:
--      See BA110140.A.
--
-- CHANGE HISTORY:
--    30 NOV 2004   PHL   Initial version.
--    23 Mar 2007   RLB   Created ACATS version from submitted test.
--    17 Aug 2007   RLB   Fixed name conflict.
--    13 Mar 2014   RLB   Changed results as needed for Ada 2012. (Tests
--                        for these features are found in the tests for
--                        subclause 3.10.1, so we don't need to have them here.)
--
--!

limited with BA11014;
with Ada.Exceptions, System;
package BA11014_B is
    type Local_Tagged is tagged null record;
private

    -- Check that the appropriate types are visible and can be used as
    -- incomplete types (and tagged incomplete types, where that is
    -- appropriate).

    type At1 is access BA11014.T1; -- OK
    type At2 is access all BA11014.T2; -- OK
    procedure P3 (X3 : in out BA11014.T3); -- OK
    type At4 is access procedure (X4 : access BA11014.T4); -- OK
    function F5 (X5 : BA11014.T5) return Integer; -- OK
    procedure P6 (X6 : access BA11014.T6); -- OK
    function F7 (X7 : BA11014.T7'Class) return Float; -- OK
    type ATA1 is access constant BA11014.TA1; -- OK
    type APT1X is not null access all BA11014.PT1; -- OK
    type AIT1 is access all BA11014.IT1'Class; -- OK


    type Apt1 is access BA11014.P.T1; -- OK
    type Apt2 is access all BA11014.P.T2; -- OK
    procedure P3 (X3 : in out BA11014.P.T3); -- OK
    type Apt4 is access procedure (X4 : access BA11014.P.T4); -- OK
    function F5 (X5 : BA11014.P.T5) return Integer; -- OK
    procedure P6 (X6 : access BA11014.P.T6); -- OK
    function F7 (X7 : BA11014.P.T7'Class) return Float; -- OK
    type ApTA1 is access constant BA11014.P.TA1; -- OK
    type ApPT1 is not null access all BA11014.P.PT1; -- OK
    type ApIT1 is access all BA11014.P.IT1'Class; -- OK

    type Apqt1 is access BA11014.P.Q.T1; -- OK
    type Apqt2 is access all BA11014.P.Q.T2; -- OK
    procedure P3 (X3 : in out BA11014.P.Q.T3); -- OK
    type Apqt4 is access procedure (X4 : access BA11014.P.Q.T4); -- OK
    function F5 (X5 : BA11014.P.Q.T5) return Integer; -- OK
    procedure P6 (X6 : access BA11014.P.Q.T6); -- OK
    function F7 (X7 : BA11014.P.Q.T7'Class) return Float; -- OK
    type ApqTA1 is access constant BA11014.P.Q.TA1; -- OK
    type ApqPT1 is not null access all BA11014.P.Q.PT1; -- OK
    type ApqIT1 is access all BA11014.P.Q.IT1'Class; -- OK

    -- Check that the types that are visible cannot be used as complete types.

    X1 : BA11014.T1 := 10; -- ERROR:
    X2 : constant BA11014.T2 := null; -- ERROR:
    X3 : BA11014.T3 := (C1 => 3); -- ERROR:
    procedure P4 (X4 : BA11014.T4); -- OK. (Starting with Ada 2012.)
    type T5 is new BA11014.T5 with null record; -- ERROR:
    function F6 return BA11014.T6; -- OK. (Starting with Ada 2012.)
    procedure P7 (X7 : BA11014.T7 := (BA11014.T3 with null record)); -- ERROR:
    XT1 : BA11014.TA1; -- ERROR:
    XP1 : BA11014.PT1; -- ERROR:
    type T9 is new Local_Tagged and BA11014.IT1 with null record; -- ERROR:

    Px1 : BA11014.P.T1 := 10; -- ERROR:
    Px2 : constant BA11014.P.T2 := null; -- ERROR:
    Px3 : BA11014.P.T3 := (C1 => 3); -- ERROR:
    procedure P4 (X4 : BA11014.P.T4); -- OK. (Starting with Ada 2012.)
    type Pt5 is new BA11014.P.T5 with null record; -- ERROR:
    function F6 return BA11014.P.T6; -- OK. (Starting with Ada 2012.)
    procedure P7 (X7 : BA11014.P.T7 := (BA11014.P.T3 with null record)); -- ERROR:
    PXT1 : BA11014.P.TA1; -- ERROR:
    PXP1 : BA11014.P.PT1; -- ERROR:
    type PT9 is new Local_Tagged and BA11014.P.IT1 with null record; -- ERROR:

    Pqx1 : BA11014.P.Q.T1 := 10; -- ERROR:
    Pqx2 : constant BA11014.P.Q.T2 := null; -- ERROR:
    Pqx3 : BA11014.P.Q.T3 := (C1 => 3); -- ERROR:
    procedure P4 (X4 : BA11014.P.Q.T4); -- OK. (Starting with Ada 2012.)
    type Pqt5 is new BA11014.P.Q.T5 with null record; -- ERROR:
    function F6 return BA11014.P.Q.T6; -- OK. (Starting with Ada 2012.)
    procedure P7 (X7 : BA11014.P.Q.T7 :=
                     (BA11014.P.Q.T3 with null record)); -- ERROR:
    PqXT1 : BA11014.P.Q.TA1; -- ERROR:
    PqXP1 : BA11014.P.Q.PT1; -- ERROR:
    type PqT9 is new Local_Tagged and BA11014.P.Q.IT1 with null record; -- ERROR:

    -- Check that objects, subprograms, and exceptions are not visible.

    type Att2 is access procedure (X2 : access BA11014.T2 :=
                                      BA11014.X2'Access); -- ERROR:
    procedure Pp3 (X3 : BA11014.T3 := BA11014.X3); -- ERROR:
    type Att4 is access function (X4 : access constant BA11014.T4 :=
                                     BA11014.X4'Access) return Float; -- ERROR:
    procedure P1 (X1 : access BA11014.T1) renames BA11014.P1; -- ERROR:
    function F7 (X7 : BA11014.T7) return Boolean renames BA11014.F7; -- ERROR:
    X5_Name : constant String :=
       Ada.Exceptions.Exception_Name (BA11014.X5'Identity); -- ERROR:
    X6 : String (1..BA11014.X6); -- ERROR:
    X7 : constant Boolean := BA11014.TA2'Callable; -- ERROR:
    X8 : constant Natural := BA11014.PT2.Count; -- ERROR:


    type Aptt2 is access procedure (X2 : access BA11014.P.T2 :=
                                       BA11014.P.X2'Access); -- ERROR:
    procedure Pp3 (X3 : BA11014.P.T3 := BA11014.P.X3); -- ERROR:
    type Aptt4 is access
                     function (X4 : access constant BA11014.P.T4 :=
                                  BA11014.P.X4'Access) return Float; -- ERROR:
    procedure P1 (X1 : access BA11014.P.T1) renames BA11014.P.P1; -- ERROR:
    function F7 (X7 : BA11014.P.T7) return Boolean renames BA11014.P.F7; -- ERROR:
    PX5_Name : constant String :=
       Ada.Exceptions.Exception_Name (BA11014.P.X5'Identity); -- ERROR:
    PX6 : String (1..BA11014.P.X6); -- ERROR:
    PX7 : constant Boolean := BA11014.P.TA2'Callable; -- ERROR:
    PX8 : constant Natural := BA11014.P.PT2.Count; -- ERROR:


    type Apqtt2 is access procedure (X2 : access BA11014.P.Q.T2 :=
                                        BA11014.P.Q.X2'Access); -- ERROR:
    procedure Pp3 (X3 : BA11014.P.Q.T3 := BA11014.P.Q.X3); -- ERROR:
    type Apqtt4 is access function
                             (X4 : access constant BA11014.P.Q.T4 :=
                                 BA11014.P.Q.X4'Access) return Float; -- ERROR:
    procedure P1 (X1 : access BA11014.P.Q.T1) renames BA11014.P.Q.P1; -- ERROR:
    function F7 (X7 : BA11014.P.Q.T7) return Boolean
       renames BA11014.P.Q.F7; -- ERROR:
    PQX5_Name : constant String :=
       Ada.Exceptions.Exception_Name (BA11014.P.Q.X5'Identity); -- ERROR:
    PQX6 : String (1..BA11014.P.Q.X6); -- ERROR:
    PQX7 : constant Boolean := BA11014.P.Q.TA2'Callable; -- ERROR:
    PQX8 : constant Natural := BA11014.P.Q.PT2.Count; -- ERROR:

    -- Check that the types exported from renamings and instantiations are not
    -- visible.

    A1 : System.Address := BA11014.Anfr'Address; -- ERROR:
    A2 : System.Address := BA11014.Andr'Address; -- ERROR:

    procedure Pf (X : access BA11014.Anfr.Generator); -- ERROR:
    procedure Pd (X : access BA11014.Andr.Generator); -- ERROR:
    procedure Ppf (X : access BA11014.P.Anfr.Generator); -- ERROR:
    procedure Ppd (X : access BA11014.P.Andr.Generator); -- ERROR:
    procedure Ppqf (X : access BA11014.P.Q.Anfr.Generator); -- ERROR:
    procedure Ppqd (X : access BA11014.P.Q.Andr.Generator); -- ERROR:

    -- Check that the types declared in the private part are not visible.

    type At8 is access BA11014.T8; -- ERROR:
    procedure P9 (X9 : out BA11014.T9); -- ERROR:
    type Apt8 is access BA11014.P.T8; -- ERROR:
    procedure Pp9 (X9 : out BA11014.P.T9); -- ERROR:
    type Apqt8 is access BA11014.P.Q.T8; -- ERROR:
    procedure Ppq9 (X9 : out BA11014.P.Q.T9); -- ERROR:

end BA11014_B;
