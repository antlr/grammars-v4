-- B8300022.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687 and
--     F08630-91-C-0015, the U.S. Government obtained unlimited rights in the
--     software and documentation contained herein.  Unlimited rights are
--     defined in DFAR 252.227-7013(a)(19).  By making this public release,
--     the Government intends to confer upon all recipients unlimited rights
--     equal to those held by the Government.  These rights include rights to
--     use, duplicate, release or disclose the released technical data and
--     computer software in whole or in part, in any manner and for any purpose
--     whatsoever, and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      See B8300020.A.
--
-- TEST DESCRIPTION:
--      See B8300020.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B8300020.A
--         B8300021.A
--      -> B8300022.A
--         B8300023.A
--         B8300024.A
--         B8300025.AM
--
-- PASS/FAIL CRITERIA:
--      See B8300020.A.
--
-- CHANGE HISTORY:
--      29 JUN 1999   RAD   Initial Version
--      23 SEP 1999   RLB   Revised to insure that units don't depend on other
--                          units containing errors.
--
--!

with B8300025_1_Unrelated; use B8300025_1_Unrelated;
package B8300025_1.Public.Pub is

    type T15 is new T10 with null record; -- OK
    type T16 is new T11 with null record; -- OK
    type T17 is new T12 with null record; -- OK
    type T18 is new T13 with null record; -- OK

    type T20 is new T2 with
        record
            Same_Name: Boolean; -- ERROR:
        end record;
    type T21(Same_Name: Natural) -- ERROR:
      is new T2 with null record;
    type T22 is new T3 with
        record
            Same_Name: Natural := Natural'Last; -- ERROR:
        end record;
    type T23(Different_Name: Natural) is
      new T3(Same_Name => Different_Name) with
        record
            Same_Name: Natural; -- OK; old discriminant is gone.
        end record;
    type T24 is new T3(Same_Name => 10) with
        record
            Same_Name: Natural; -- ERROR:
        end record;

    type T25 is new New_T2 with
        record
            Same_Name: Boolean; -- ERROR:
        end record;
    type T26(Same_Name: Natural) -- ERROR:
      is new New_T2 with null record;
    type T27 is new New_T3 with
        record
            Same_Name: Natural := Natural'Last; -- ERROR:
        end record;
    type T28(Different_Name: Natural) is
      new New_T3(Same_Name => Different_Name) with
        record
            Same_Name: Natural; -- OK; old discriminant is gone.
        end record;
    type T29 is new New_T3(Same_Name => 10) with
        record
            Same_Name: Natural; -- ERROR:
        end record;
    type T30 is new New_T3_Diff with
        record
            Same_Name: Integer; -- OK
        end record;

end B8300025_1.Public.Pub;

