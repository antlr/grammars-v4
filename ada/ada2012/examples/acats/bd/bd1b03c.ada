-- BD1B03C.ADA

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
--     CHECK THAT A TASK STORAGE SIZE CLAUSE FOR T CANNOT FOLLOW THE
--     USE, AS AN ACTUAL GENERIC PARAMETER OF: A SUBTYPE OF T; A
--     RECORD OR ARRAY WITH A (SUB)COMPONENT OF A SUBTYPE OF T.

-- HISTORY:
--     DHH 04/01/88 CREATED ORIGINAL TEST.
--     BCB 03/13/90 CHANGED "--  ERROR:" TO "-- ERROR:".

PROCEDURE BD1B03C IS

-- SUBTYPE
     TASK TYPE T IS
          ENTRY START;
     END T;

     PACKAGE P IS
          SUBTYPE SUB_T IS T;

          GENERIC
               TYPE PT IS LIMITED PRIVATE;
          PACKAGE RTN IS
          END RTN;

     END P;
     USE P;

     PACKAGE NEW_P IS NEW P.RTN(SUB_T);
     FOR T'STORAGE_SIZE USE 1024;                    -- ERROR:

-- RECORD COMPONENT
     TASK TYPE T1 IS
          ENTRY START;
     END T1;

     PACKAGE P1 IS
          SUBTYPE SUB_T1 IS T1;

          TYPE REC IS
               RECORD
                    Z : SUB_T1;
               END RECORD;

          GENERIC
               TYPE PT IS LIMITED PRIVATE;
          PACKAGE RTN IS
          END RTN;
     END P1;
     USE P1;

     PACKAGE NEW_P1 IS NEW P1.RTN(REC);
     FOR T1'STORAGE_SIZE USE 1024;                   -- ERROR:

-- RECORD SUBCOMPONENT
     TASK TYPE T2 IS
          ENTRY START;
     END T2;

     PACKAGE P2 IS
          SUBTYPE SUB_T2 IS T2;

          TYPE EC IS
               RECORD
                    Z : SUB_T2;
               END RECORD;

          TYPE REC1 IS
               RECORD
                    Z : EC;
               END RECORD;

          GENERIC
               TYPE PT IS LIMITED PRIVATE;
          PACKAGE RTN IS
          END RTN;
     END P2;
     USE P2;

     PACKAGE NEW_P2 IS NEW P2.RTN(REC1);
     FOR T2'STORAGE_SIZE USE 1024;                   -- ERROR:

-- ARRAY COMPONENT
     TASK TYPE T3 IS
          ENTRY START;
     END T3;

     PACKAGE P3 IS
          SUBTYPE SUB_T3 IS T3;

          TYPE ARR IS ARRAY(1 .. 5) OF SUB_T3;

          GENERIC
               TYPE PT IS LIMITED PRIVATE;
          PACKAGE RTN IS
          END RTN;
     END P3;
     USE P3;

     PACKAGE NEW_P3 IS NEW P3.RTN(ARR);
     FOR T3'STORAGE_SIZE USE 1024;                   -- ERROR:

-- ARRAY SUBCOMPONENT
     TASK TYPE T4 IS
          ENTRY START;
     END T4;

     PACKAGE P4 IS
          SUBTYPE SUB_T4 IS T4;

          TYPE ARR4 IS ARRAY(1 .. 5) OF SUB_T4;
          TYPE ARY IS ARRAY(1 .. 3) OF ARR4;

          GENERIC
               TYPE PT IS LIMITED PRIVATE;
          PACKAGE RTN IS
          END RTN;
     END P4;
     USE P4;

     PACKAGE NEW_P4 IS NEW P4.RTN(ARY);
     FOR T4'STORAGE_SIZE USE 1024;                   -- ERROR:

-----------------------------------
     TASK BODY T IS
     BEGIN
          ACCEPT START;
     END T;

-----------------------------------
     TASK BODY T1 IS
     BEGIN
          ACCEPT START;
     END T1;

-----------------------------------
     TASK BODY T2 IS
     BEGIN
          ACCEPT START;
     END T2;

-----------------------------------
     TASK BODY T3 IS
     BEGIN
          ACCEPT START;
     END T3;

-----------------------------------
     TASK BODY T4 IS
     BEGIN
          ACCEPT START;
     END T4;

BEGIN
     NULL;
END BD1B03C;
