-- BD2B03A.ADA

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
--     CHECK THAT A COLLECTION SIZE SPECIFICATION CANNOT BE GIVEN:
--     - IN A PACKAGE SPECIFICATION FOR A TYPE DECLARED IN AN INNER
--       PACKAGE SPECIFICATION;
--     - IN A PACKAGE OR TASK SPECIFICATION FOR A TYPE DECLARED IN AN
--       ENCLOSING PACKAGE SPECIFICATION;
--     - IN A PACKAGE BODY FOR A TYPE DECLARED IN THE CORRESPONDING
--       PACKAGE SPECIFICATION.

-- HISTORY:
--     JKC 04/06/88  CREATED ORIGINAL TEST.
--     BCB 03/29/90  SPLIT ORIGINAL TEST INTO BD2B03A.ADA, BD2B03B.ADA,
--                   AND BD2B03C.ADA.

PROCEDURE BD2B03A IS

     PACKAGE P IS
          TYPE ACC2 IS ACCESS INTEGER;
          TYPE ACC6 IS ACCESS STRING;

          PACKAGE INNER_P IS
               TYPE ARRAY_TYPE IS ARRAY (1..4) OF INTEGER;
               TYPE ACC1 IS ACCESS ARRAY_TYPE;
          END INNER_P;

          USE INNER_P;

          FOR ACC1'STORAGE_SIZE USE 512;                       -- ERROR:

          PACKAGE INNER_Q IS
               FOR ACC2'STORAGE_SIZE USE 512;                  -- ERROR:
          END INNER_Q;

     END P;

     PACKAGE BODY P IS
          FOR ACC6'STORAGE_SIZE USE 1024;                      -- ERROR:
     END P;

     PACKAGE Q IS
          TYPE ACC4 IS ACCESS INTEGER;

          TASK T IS
               FOR ACC4'STORAGE_SIZE USE 1024;                 -- ERROR:
          END T;
     END Q;

     PACKAGE BODY Q IS
          TASK BODY T IS
          BEGIN
               NULL;
          END T;
     END Q;

BEGIN
     NULL;
END BD2B03A;
