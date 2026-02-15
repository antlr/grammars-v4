-- BD2A03A.ADA

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
--     CHECK THAT A SIZE SPECIFICATION CANNOT BE GIVEN IN A PACKAGE
--     SPECIFICATION FOR AN ARRAY TYPE, A DERIVED RECORD TYPE, AND A
--     FIXED-POINT TYPE ALL DECLARED IN AN INNER PACKAGE SPECIFICATION.

-- HISTORY:
--     JKC 03/16/88  CREATED ORIGINAL TEST.

PROCEDURE BD2A03A IS

     PACKAGE P IS

          PACKAGE INNER_P IS
               TYPE FIXED_TYPE IS DELTA 0.25 RANGE 1.0..4.0;
               TYPE ARRAY_TYPE IS ARRAY (1..4) OF INTEGER;
               TYPE RECORD_TYPE IS
                    RECORD
                         UNIT1 : INTEGER := 1;
                         UNIT2 : INTEGER := 2;
                         UNIT3 : INTEGER := 3;
                    END RECORD;
               TYPE DERIVED_R IS NEW RECORD_TYPE;
          END INNER_P;

          USE INNER_P;

          FOR FIXED_TYPE'SIZE USE INTEGER'SIZE;               -- ERROR:
          FOR ARRAY_TYPE'SIZE USE 8*INTEGER'SIZE;             -- ERROR:
          FOR DERIVED_R'SIZE  USE 6*INTEGER'SIZE;             -- ERROR:
     END P;

BEGIN
     NULL;
END BD2A03A;
