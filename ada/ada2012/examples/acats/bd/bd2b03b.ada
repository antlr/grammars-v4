-- BD2B03B.ADA

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
--     CHECK THAT A COLLECTION SIZE SPECIFICATION CANNOT BE GIVEN
--     IN A PACKAGE OR TASK SPECIFICATION FOR A TYPE DECLARED IN AN
--     ENCLOSING DECLARATIVE PART.

-- HISTORY:
--     BCB 03/29/90  CREATED ORIGINAL TEST FROM SPLIT OF BD2B03A.ADA.

PROCEDURE BD2B03B IS

BEGIN
     DECLARE
          TYPE ACC3 IS ACCESS INTEGER;

          PACKAGE S IS
               FOR ACC3'STORAGE_SIZE USE 1024;                 -- ERROR:
          END S;

          TYPE ACC5 IS ACCESS STRING;

          TASK T2 IS
               FOR ACC5'STORAGE_SIZE USE 1024;                 -- ERROR:
          END T2;


          TASK BODY T2 IS
          BEGIN
               NULL;
          END T2;

     BEGIN
          NULL;
     END;

END BD2B03B;
