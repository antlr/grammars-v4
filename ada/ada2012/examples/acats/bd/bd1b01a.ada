-- BD1B01A.ADA

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
--     CHECK THAT A SIZE CLAUSE FOR TYPE T CANNOT FOLLOW AN OBJECT
--     DECLARATION WHOSE TYPE MARK IS: A SUBTYPE OF T; A RECORD OR
--     ARRAY WITH A (SUB)COMPONENT OF A SUBTYPE OF T; AN ARRAY TYPE
--     WITH AN INDEX SUBTYPE OF A SUBTYPE OF T.

-- HISTORY:
--     DHH 04/01/88 CREATED ORIGINAL TEST.
--     DHH 03/27/89 CHANGED SIZES TO SMALLER NUMBERS.
--     BCB 03/13/90 CHANGED "--  ERROR:" TO "-- ERROR:".

PROCEDURE BD1B01A IS

-- SUBTYPE
     TYPE T IS (RED, BLUE, YELLOW, GREEN);

     SUBTYPE SUB_T IS T;

     OBJ : SUB_T;
     FOR T'SIZE USE 2;                    -- ERROR:

-- RECORD COMPONENT
     TYPE T1 IS (RED, BLUE, YELLOW, GREEN);

     SUBTYPE SUB_T1 IS T1;

     TYPE REC IS
          RECORD
               Z : SUB_T1;
          END RECORD;
     TREC : REC;
     FOR T1'SIZE USE 2;                    -- ERROR:

-- RECORD SUBCOMPONENT
     TYPE T2 IS (RED, BLUE, YELLOW, GREEN);

     SUBTYPE SUB_T2 IS T2;

     TYPE EC IS
          RECORD
               Z : SUB_T2;
          END RECORD;

     TYPE REC1 IS
          RECORD
               Z : EC;
          END RECORD;
     TREC1 : REC1;
     FOR T2'SIZE USE 2;                    -- ERROR:

-- ARRAY COMPONENT
     TYPE T3 IS (RED, BLUE, YELLOW, GREEN);

     SUBTYPE SUB_T3 IS T3;

     TYPE ARR IS ARRAY(1 .. 5) OF SUB_T3;
     TARR : ARR;
     FOR T3'SIZE USE 2;                    -- ERROR:

-- ARRAY SUBCOMPONENT
     TYPE T4 IS (RED, BLUE, YELLOW, GREEN);

     SUBTYPE SUB_T4 IS T4;

     TYPE ARR4 IS ARRAY(1 .. 5) OF SUB_T4;
     TYPE ARY IS ARRAY(1 .. 3) OF ARR4;
     TARY : ARY;
     FOR T4'SIZE USE 2;                    -- ERROR:

-- INDEX SUBTYPE
     TYPE T5 IS (RED, BLUE, YELLOW, GREEN);

     SUBTYPE SUB_T5 IS T5;

     TYPE ARRY IS ARRAY(SUB_T5 RANGE RED .. YELLOW) OF BOOLEAN;
     TARRY : ARRY;
     FOR T5'SIZE USE 2;                    -- ERROR:

BEGIN
     NULL;
END BD1B01A;
