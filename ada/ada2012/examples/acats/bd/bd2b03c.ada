-- BD2B03C.ADA

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
--     IN A DECLARATIVE PART AFTER THE OCCURRENCE OF A BODY.

-- HISTORY:
--     BCB 03/29/90  CREATED ORIGINAL TEST FROM SPLIT OF BD2B03A.ADA.

PROCEDURE BD2B03C IS

BEGIN
     DECLARE
          TYPE ACC7 IS ACCESS STRING;

          FUNCTION F RETURN INTEGER IS
          BEGIN
               RETURN 5;
          END F;

          FOR ACC7'STORAGE_SIZE USE 1024;                      -- ERROR:

     BEGIN
          NULL;
     END;

END BD2B03C;
