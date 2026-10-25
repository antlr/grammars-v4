-- B420001.A
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
-- OBJECTIVE
--     Check that if the index subtype of a string type is a modular subtype
--     whose lower bound is zero, then the evaluation of a null string_literal
--     raises Constraint_Error. This was confirmed by AI95-00138.
--     Note: If the null string_literal is a static expression (which is usual),
--     then the Constraint_Error does not happen at run time, but is caught at
--     compile time by RM-4.9(34).
--
-- TEST DESCRIPTION
--     Declare some modular types, and write illegal null string literals.
--     We also declare some legal null string literals, using subtypes
--     that don't start at zero.
--
-- CHANGE HISTORY:
--      29 JUN 1999   RAD   Initial Version
--      23 SEP 1999   RLB   Improved comments, renamed, issued.
--
--!

with System;
procedure B420001 is
    type M1 is mod 1;
    type String_1 is array(M1 range <>) of Character;
    Null_1: constant String_1 := ""; -- ERROR: Would raise C_E.

    type M2 is mod 2;
    type String_2 is array(M2 range <>) of Character;
    subtype String_2_2 is String_2(1..0);
    Null_2: constant String_2 := ""; -- ERROR: Would raise C_E.
    OK_Null_2: String_2_2 := ""; -- OK

    type M3 is mod 3;
    type String_3 is array(M3 range <>) of Character;
    subtype String_3_3 is String_3(2..1);
    Null_3: constant String_3 := ""; -- ERROR: Would raise C_E.
    OK_Null_3: String_3_3 := ""; -- OK

    type M4 is mod 4;
    type String_4 is array(M4 range <>) of Character;
    subtype String_4_4 is String_4(3..2);
    Null_4: constant String_4 := ""; -- ERROR: Would raise C_E.
    OK_Null_4: String_4_4 := ""; -- OK

    type M5 is mod 5;
    type String_5 is array(M5 range <>) of Character;
    subtype String_5_5 is String_5(4..3);
    Null_5: constant String_5 := ""; -- ERROR: Would raise C_E.
    OK_Null_5: String_5_5 := ""; -- OK

    type M_Max_Bin is mod System.Max_Binary_Modulus;
    type String_Max_Bin is array(M_Max_Bin range <>) of Character;
    subtype String_Max_Bin_Max_Bin is String_Max_Bin(1000..100);
    Null_Max_Bin: constant String_Max_Bin := ""; -- ERROR:
    OK_Null_Max_Bin: String_Max_Bin_Max_Bin := ""; -- OK
        -- Would raise C_E.

    type M_Max_Non_Bin is mod System.Max_Nonbinary_Modulus;
    type String_Max_Non_Bin is array(M_Max_Non_Bin range <>) of Character;
    subtype String_Max_Non_Bin_Max_Non_Bin is String_Max_Non_Bin(1000..100);
    Null_Max_Non_Bin: constant String_Max_Non_Bin := ""; -- ERROR:
    OK_Null_Max_Non_Bin: String_Max_Non_Bin_Max_Non_Bin := ""; -- OK
        -- Would raise C_E.

    type M_Bin is mod System.Max_Binary_Modulus/2;
    type String_Bin is array(M_Bin range <>) of Character;
    subtype String_Bin_Bin is String_Bin(10..5);
    Null_Bin: constant String_Bin := ""; -- ERROR:
    OK_Null_Bin: String_Bin_Bin := ""; -- OK
        -- Would raise C_E.

    type M_Non_Bin is mod System.Max_Nonbinary_Modulus-1;
    type String_Non_Bin is array(M_Non_Bin range <>) of Character;
    subtype String_Non_Bin_Non_Bin is String_Non_Bin(1..0);
    Null_Non_Bin: constant String_Non_Bin := ""; -- ERROR:
    OK_Null_Non_Bin: String_Non_Bin_Non_Bin := ""; -- OK
        -- Would raise C_E.

begin
    null;
end B420001;
