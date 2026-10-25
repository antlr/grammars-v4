-- B610002.A
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
--     Check that an access parameter cannot have a mode specified.
--
-- TEST DESCRIPTION
--     This is technically a syntax test, which we usually don't test.
--     However, the Ada grammar requires three token lookahead or an
--     explosion of productions, as "not null" is legal both when a
--     mode is allowed (with a subtype_mark) and when it is not allowed
--     (with an access definition). A three token lookahead is beyond the
--     capabilities of most automated parser generators. Thus, it is likely
--     that this check will be implemented outside of the syntax; in that
--     case it would be easy to omit.
--
-- CHANGE HISTORY:
--      21 Mar 2007   RLB   Created test from submitted test.
--
--!

procedure B610002 is

    type Acc_String is access all String;

    procedure Proc1 (A : in access constant String) is -- ERROR:
    begin
        null;
    end Proc1;

    procedure Proc2 (A : out access String) is -- ERROR:
    begin
        null;
    end Proc2;

    procedure Proc3 (A : in out not null access String) is -- ERROR:
    begin
        null;
    end Proc3;

    procedure Proc4 (A : in out not null Acc_String) is -- OK.
    begin
        null;
    end Proc4;

    procedure Proc5 (A : in not null Acc_String) is -- OK.
    begin
        null;
    end Proc5;

begin
    null;
end B610002;
