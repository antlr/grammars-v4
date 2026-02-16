-- B26002A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT (") MUST BE DOUBLED TO BE
-- USED WITHIN STRING LITERALS AS A DATA CHARACTER.

-- DCB 1/16/80
-- JRK 10/29/80
-- TBN 10/14/85     RENAMED FROM B26002A.ADA AND FIXED LINE LENGTH.

PROCEDURE B26002A  IS

        C1 : STRING(1..1);
        C3 : STRING(1..5);

BEGIN
        C1 := """;      -- ERROR: QUOTE MUST BE DOUBLED IN
                        --        STRING LITERALS.
        NULL;

        C3 := "ABCD"";  -- ERROR: QUOTE MUST BE DOUBLED IN
                        --        STRING LITERALS.
        NULL;

        C3 := ""BCDE";  -- ERROR: QUOTE MUST BE DOUBLED IN
                        --        STRING LITERALS.
        NULL;

        C3 := "AB"DE";  -- ERROR: QUOTE MUST BE DOUBLED IN
                        --        STRING LITERALS.
        NULL;

END B26002A;
