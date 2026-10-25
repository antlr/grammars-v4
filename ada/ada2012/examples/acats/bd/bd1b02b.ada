-- BD1B02B.ADA

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
--     CHECK THAT A COLLECTION SIZE CLAUSE FOR TYPE T CANNOT FOLLOW AN
--     EXPRESSION OR RANGE INCLUDING AN ATTRIBUTE OF: A SUBTYPE OF T; A
--     RECORD OR ARRAY WITH A (SUB)COMPONENT OF A SUBTYPE OF T.

-- HISTORY:
--     DHH 04/01/88 CREATED ORIGINAL TEST.
--     BCB 09/26/90 CHANGED OCCURRENCES OF 'SIZE TO 'STORAGE_SIZE IN
--                  ERROR LINES TO MEET TEST OBJECTIVE.
--     PWN 12/19/94 CORRECTED -- ERROR: INCONSITENCIES

PROCEDURE BD1B02B IS

-- SUBTYPE

     TYPE TEST IS
          RECORD
               X : INTEGER;
               Y : STRING(1 .. 7);
          END RECORD;
     TYPE T IS ACCESS TEST;

     SUBTYPE SUB_T IS T;

     OBJ : INTEGER := SUB_T'SIZE;
     FOR T'STORAGE_SIZE USE 1024;                     -- ERROR:

-- RECORD COMPONENT
     TYPE TEST1 IS
          RECORD
               X : INTEGER;
               Y : STRING(1 .. 7);
          END RECORD;
     TYPE T1 IS ACCESS TEST1;

     SUBTYPE SUB_T1 IS T1;

     TYPE REC IS
          RECORD
               Z : SUB_T1;
          END RECORD;
     TREC : INTEGER := REC'SIZE;
     FOR T1'STORAGE_SIZE USE 1024;                    -- ERROR:

-- RECORD SUBCOMPONENT
     TYPE TEST2 IS
          RECORD
               X : INTEGER;
               Y : STRING(1 .. 7);
          END RECORD;
     TYPE T2 IS ACCESS TEST2;

     SUBTYPE SUB_T2 IS T2;

     TYPE EC IS
          RECORD
               Z : SUB_T2;
          END RECORD;

     TYPE REC1 IS
          RECORD
               Z : EC;
          END RECORD;
     TREC1 : INTEGER := REC1'SIZE;
     FOR T2'STORAGE_SIZE USE 1024;                    -- ERROR:

-- ARRAY COMPONENT
     TYPE TEST3 IS
          RECORD
               X : INTEGER;
               Y : STRING(1 .. 7);
          END RECORD;
     TYPE T3 IS ACCESS TEST3;

     SUBTYPE SUB_T3 IS T3;

     TYPE ARR IS ARRAY(1 .. 5) OF SUB_T3;
     TARR : INTEGER := ARR'SIZE;
     FOR T3'STORAGE_SIZE USE 1024;                    -- ERROR:

-- ARRAY SUBCOMPONENT
     TYPE TEST4 IS
          RECORD
               X : INTEGER;
               Y : STRING(1 .. 7);
          END RECORD;
     TYPE T4 IS ACCESS TEST4;

     SUBTYPE SUB_T4 IS T4;

     TYPE ARR4 IS ARRAY(1 .. 5) OF SUB_T4;
     TYPE ARY IS ARRAY(1 .. 3) OF ARR4;
     TARY : INTEGER := ARY'SIZE;
     FOR T4'STORAGE_SIZE USE 1024;                    -- ERROR:

BEGIN
     NULL;
END BD1B02B;
