-- BC3005A.ADA

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
-- CHECK THAT IN A GENERIC INSTANTIATION:

     -- ACTUAL OBJECT PARAMETERS CANNOT BE PASSED TO FORMAL TYPE
     -- PARAMETERS NOR TO FORMAL SUBPROGRAM PARAMETERS.

     -- ACTUAL TYPE PARAMETERS CANNOT BE PASSED TO FORMAL OBJECT
     -- PARAMETERS NOR TO FORMAL SUBPROGRAM PARAMETERS.

     -- ACTUAL SUBPROGRAM PARAMETERS CANNOT BE PASSED TO FORMAL 
     -- OBJECT PARAMETERS NOR TO FORMAL TYPE PARAMETERS.

-- ASL 8/14/81

PROCEDURE BC3005A IS

     PROCEDURE PROC1;
     PROCEDURE PROC2;

     X,Y : INTEGER := 5;

     TYPE COLOR IS (RED,BLUE,YELLOW);
     TYPE SEX IS (MALE,FEMALE);

     GENERIC
          GFOBJ : INTEGER;
          TYPE GFTYPE IS (<>);
          WITH PROCEDURE GFPROC;
     PACKAGE P IS
     END P;

     PACKAGE Z1 IS NEW P(X,Y,PROC1);         -- ERROR: OBJ -> TYPE.
     PACKAGE Z2 IS NEW P(X,COLOR,Y);         -- ERROR: OBJ -> SUBPR.
     PACKAGE Z3 IS NEW P(COLOR,SEX,PROC1);   -- ERROR: TYPE -> OBJ.
     PACKAGE Z4 IS NEW P(X,COLOR,SEX);       -- ERROR: TYPE -> SUBPR.
     PACKAGE Z5 IS NEW P(PROC1,COLOR,PROC2); -- ERROR: SUBPR -> OBJ.
     PACKAGE Z6 IS NEW P(X,PROC1,PROC2);     -- ERROR: SUBPR -> TYPE.

     PROCEDURE PROC1 IS
     BEGIN
          NULL;
     END PROC1;

     PROCEDURE PROC2 IS
     BEGIN
          NULL;
     END PROC2;
BEGIN
     NULL;
END BC3005A;
