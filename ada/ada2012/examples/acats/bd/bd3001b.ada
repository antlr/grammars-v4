-- BD3001B.ADA

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
-- OBJECTIVE:
--     CHECK THAT AN ENUMERATION REPRESENTATION CLAUSE CANNOT BE GIVEN
--     FOR A NAME DECLARED BY AN OBJECT OR SUBTYPE DECLARATION, OR FOR
--     A PRIVATE OR INCOMPLETE TYPE PRIOR TO THE FULL DECLARATION.

-- HISTORY:
--     DHH 8/22/88 CREATED ORIGINAL TEST.

PROCEDURE BD3001B IS

     TYPE ENUM IS (A,B,C,D,E);

     TYPE ENUM1 IS (A,B,C,D,E);
     SUBTYPE ENUM1A IS ENUM1;

     FOR ENUM1A USE (1, 2, 3, 4, 5);                          -- ERROR:

     OBJ : ENUM;

     FOR OBJ USE (1, 2, 3, 4, 5);                             -- ERROR:

     PACKAGE P IS
          TYPE ENUM;
          TYPE ACC_ENUM IS ACCESS ENUM;

          FOR ENUM USE (1, 2, 3, 4, 5);                       -- ERROR:

          TYPE ENUM IS (A,B,C,D,E);

          TYPE PRIV IS PRIVATE;
          TYPE PRIV1 IS PRIVATE;

          FOR PRIV1 USE (1, 2, 3, 4, 5);                      -- ERROR:

     PRIVATE
          FOR PRIV USE (1, 2, 3, 4, 5);                       -- ERROR:

          TYPE PRIV IS (A,B,C,D,E);
          TYPE PRIV1 IS (A,B,C,D,E);
     END P;
BEGIN
     NULL;
END BD3001B;
