-- BD2B02A.ADA

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
--     CHECK THAT TWO COLLECTION SIZE SPECIFICATIONS, EVEN IF THE SAME,
--     CANNOT BE GIVEN FOR THE SAME TYPE.

-- HISTORY:
--     JKC 04/06/88  CREATED ORIGINAL TEST.

PROCEDURE BD2B02A IS

     TYPE ARRAY_TYPE IS ARRAY (1..4) OF INTEGER;
     TYPE ACCESS_TYPE IS ACCESS ARRAY_TYPE;
     FOR ACCESS_TYPE'STORAGE_SIZE USE 1024;
     FOR ACCESS_TYPE'STORAGE_SIZE USE 1024;        -- ERROR:  DUPLICATE.

     TYPE A_TYPE IS ACCESS ARRAY_TYPE;
     FOR A_TYPE'STORAGE_SIZE USE 1024;
     FOR A_TYPE'STORAGE_SIZE USE 2048;             -- ERROR:  DUPLICATE.

BEGIN
     NULL;
END BD2B02A;
