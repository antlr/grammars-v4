-- B35506C.ADA

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
-- CHECK THAT THE ARGUMENT TO T'VALUE MUST HAVE THE BASE TYPE STRING.

-- RJW 2/20/86

PROCEDURE  B35506C  IS
     
     TYPE T IS RANGE -1000 .. 1000;
     T1 : T;
     
     STR : STRING (1 .. 20) := (OTHERS => '0');

     SUBTYPE SSTRING IS STRING;
     SUB_STRING : SSTRING (1 .. 20) := (OTHERS => '0');

     TYPE NSTRING IS NEW STRING;
     NEW_STRING : NSTRING (1 .. 20) := (OTHERS => '0');

     TYPE ARR IS ARRAY (POSITIVE RANGE <> ) OF CHARACTER;
     NOT_STRING : ARR (1 .. 20) := (OTHERS => '0');

     T_ARRAY : ARRAY (1 .. 20) OF CHARACTER := (OTHERS => '0');

     CHAR : CHARACTER := '0';
BEGIN
     T1 := T'VALUE (STR);               -- OK.
     T1 := T'VALUE (SUB_STRING);        -- OK.
     T1 := T'VALUE (NOT_STRING);        -- ERROR: ARGUMENT NOT OF 
                                        --        BASE TYPE STRING.
     T1 := T'VALUE (NEW_STRING);        -- ERROR: ARGUMENT NOT OF 
                                        --        BASE TYPE STRING.
     T1 := T'VALUE (T_ARRAY);           -- ERROR: ARGUMENT NOT OF 
                                        --        BASE TYPE STRING.
     T1 := T'VALUE(CHAR);               -- ERROR: ARGUMENT NOT OF
                                        --        BASE TYPE STRING.
END B35506C ;
