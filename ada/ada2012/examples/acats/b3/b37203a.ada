-- B37203A.ADA

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
-- CHECK THAT A DISCRIMINANT CONSTRAINT IS NOT OMITTED WHERE IT IS
-- REQUIRED, I.E., CHECK  THAT A DISCRIMINANT CONSTRAINT CANNOT BE
-- OMITTED FROM A SUBTYPE INDICATION FOR A TYPE_MARK HAVING
-- DISCRIMINANTS WITH NO DEFAULT VALUES WHEN THE SUBTYPE IS USED
-- IN AN OBJECT DECLARATION FOR A VARIABLE, AN ARRAY_TYPE_DEFINITION OR
-- A RECORD COMPONENT_DECLARATON.

-- ASL 7/2/81
-- RJW 1/16/86 - RENAMED FROM B37203A.ADA.  ADDED INITIAL VALUES.
-- PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.

PROCEDURE B37203A IS

     TYPE REC(DISC : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     OBJ2 : REC;                              -- ERROR: UNCONSTRAINED.

     TYPE ARR IS ARRAY(1..10) OF REC;         -- ERROR: UNCONSTRAINED.

     ARROBJ : ARRAY(1..10) OF REC :=          -- ERROR: UNCONSTRAINED.
          (1 .. 10 => (DISC => 5));

     TYPE REC2 IS
          RECORD
               COMP : REC := (DISC => 5);     -- ERROR: UNCONSTRAINED.
               COMP2 : REC;                   -- ERROR: UNCONSTRAINED.
          END RECORD;

     PACKAGE PACK IS
          TYPE PRI IS PRIVATE;
          TYPE LIM IS LIMITED PRIVATE;
     PRIVATE
          TYPE PRI IS NEW REC;                -- ERROR: UNCONSTRAINED.
          TYPE LIM IS NEW REC;                -- ERROR: UNCONSTRAINED.
     END PACK;

BEGIN
     NULL;
END B37203A;
