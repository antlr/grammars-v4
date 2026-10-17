-- B74103I.ADA

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
--     CHECK THAT BEFORE THE FULL DECLARATION OF A PRIVATE TYPE,
--        A NAME DENOTING THE PRIVATE TYPE
--        A NAME THAT DENOTES A SUBTYPE OF THE PRIVATE TYPE, AND
--        A NAME THAT DENOTES A COMPOSITE TYPE WITH A SUBCOMPONENT OF
--          THE PRIVATE TYPE OR SUBTYPE
--     MAY NOT BE USED IN A RENAMING DECLARATION FOR AN OBJECT.

-- HISTORY:
--     BCB 08/24/88  CREATED ORIGINAL TEST.

PROCEDURE B74103I IS

     PACKAGE P IS
          TYPE P1 IS PRIVATE;
          TYPE LP2 IS LIMITED PRIVATE;

          TYPE ACC_P1 IS ACCESS P1;
          TYPE ACC_LP2 IS ACCESS LP2;

          FUNCTION F_P1 RETURN ACC_P1;
          FUNCTION F_LP2 RETURN ACC_LP2;

          CP1 : CONSTANT P1;
          CLP2 : CONSTANT LP2;

          SUBTYPE SP1 IS P1;
          SUBTYPE SLP2 IS LP2;

          TYPE ACC_SP1 IS ACCESS SP1;
          TYPE ACC_SLP2 IS ACCESS SLP2;

          FUNCTION F_SP1 RETURN ACC_SP1;
          FUNCTION F_SLP2 RETURN ACC_SLP2;

          TYPE ARR1_P1 IS ARRAY ( 1 .. 2 ) OF P1;
          TYPE ARR2_LP2 IS ARRAY ( 1 .. 2 ) OF LP2;
          TYPE ARR3_SP1 IS ARRAY ( 1 .. 2 ) OF SP1;
          TYPE ARR4_SLP2 IS ARRAY ( 1 .. 2 ) OF SLP2;
          TYPE ARR51 IS ARRAY ( 1 .. 2 ) OF ARR1_P1;
          TYPE ARR52 IS ARRAY ( 1 .. 2 ) OF ARR2_LP2;
          TYPE ARR53 IS ARRAY ( 1 .. 2 ) OF ARR3_SP1;
          TYPE ARR54 IS ARRAY ( 1 .. 2 ) OF ARR4_SLP2;

          TYPE ACC_ARR51 IS ACCESS ARR51;
          TYPE ACC_ARR52 IS ACCESS ARR52;
          TYPE ACC_ARR53 IS ACCESS ARR53;
          TYPE ACC_ARR54 IS ACCESS ARR54;

          FUNCTION F_ARR51 RETURN ACC_ARR51;
          FUNCTION F_ARR52 RETURN ACC_ARR52;
          FUNCTION F_ARR53 RETURN ACC_ARR53;
          FUNCTION F_ARR54 RETURN ACC_ARR54;

          TYPE REC1 IS
               RECORD
                    C1 : P1;
               END RECORD;

          TYPE REC2 IS
               RECORD
                    C2 : LP2;
               END RECORD;

          TYPE REC3 IS
               RECORD
                    C3 : SP1;
               END RECORD;

          TYPE REC4 IS
               RECORD
                    C4 : SLP2;
               END RECORD;

          TYPE REC51 IS
               RECORD
                    C5 : REC1;
               END RECORD;

          TYPE REC52 IS
               RECORD
                    C6 : REC2;
               END RECORD;

          TYPE REC53 IS
               RECORD
                    C7 : REC3;
               END RECORD;

          TYPE REC54 IS
               RECORD
                    C8 : REC4;
               END RECORD;

          TYPE ACC_REC51 IS ACCESS REC51;
          TYPE ACC_REC52 IS ACCESS REC52;
          TYPE ACC_REC53 IS ACCESS REC53;
          TYPE ACC_REC54 IS ACCESS REC54;

          FUNCTION F_REC51 RETURN ACC_REC51;
          FUNCTION F_REC52 RETURN ACC_REC52;
          FUNCTION F_REC53 RETURN ACC_REC53;
          FUNCTION F_REC54 RETURN ACC_REC54;

          X1 : P1 RENAMES F_P1.ALL;                    -- ERROR:
          X2 : LP2 RENAMES F_LP2.ALL;                  -- ERROR:
          X3 : SP1 RENAMES F_SP1.ALL;                  -- ERROR:
          X4 : SLP2 RENAMES F_SLP2.ALL;                -- ERROR:
          X5 : ARR51 RENAMES F_ARR51.ALL;              -- ERROR:
          X6 : ARR52 RENAMES F_ARR52.ALL;              -- ERROR:
          X7 : ARR53 RENAMES F_ARR53.ALL;              -- ERROR:
          X8 : ARR54 RENAMES F_ARR54.ALL;              -- ERROR:
          X9 : REC51 RENAMES F_REC51.ALL;              -- ERROR:
          X10 : REC52 RENAMES F_REC52.ALL;             -- ERROR:
          X11 : REC53 RENAMES F_REC53.ALL;             -- ERROR:
          X12 : REC54 RENAMES F_REC54.ALL;             -- ERROR:

     PRIVATE
          TYPE P1 IS NEW INTEGER;
          TYPE LP2 IS NEW INTEGER;

          CP1 : CONSTANT P1 := 3;
          CLP2 : CONSTANT LP2 := 4;
     END P;

     PACKAGE BODY P IS
          FUNCTION F_P1 RETURN ACC_P1 IS
               Z : ACC_P1 := NEW P1'(CP1);
          BEGIN
               RETURN Z;
          END F_P1;

          FUNCTION F_LP2 RETURN ACC_LP2 IS
               Z : ACC_LP2 := NEW LP2'(CLP2);
          BEGIN
               RETURN Z;
          END F_LP2;

          FUNCTION F_SP1 RETURN ACC_SP1 IS
               Z : ACC_SP1 := NEW SP1'(CP1);
          BEGIN
               RETURN Z;
          END F_SP1;

          FUNCTION F_SLP2 RETURN ACC_SLP2 IS
               Z : ACC_SLP2 := NEW SLP2'(CLP2);
          BEGIN
               RETURN Z;
          END F_SLP2;

          FUNCTION F_ARR51 RETURN ACC_ARR51 IS
               Z : ACC_ARR51 := NEW ARR51'((CP1,CP1),(CP1,CP1));
          BEGIN
               RETURN Z;
          END F_ARR51;

          FUNCTION F_ARR52 RETURN ACC_ARR52 IS
               Z : ACC_ARR52 := NEW ARR52'((CLP2,CLP2),(CLP2,CLP2));
          BEGIN
               RETURN Z;
          END F_ARR52;

          FUNCTION F_ARR53 RETURN ACC_ARR53 IS
               Z : ACC_ARR53 := NEW ARR53'((CP1,CP1),(CP1,CP1));
          BEGIN
               RETURN Z;
          END F_ARR53;

          FUNCTION F_ARR54 RETURN ACC_ARR54 IS
               Z : ACC_ARR54 := NEW ARR54'((CLP2,CLP2),(CLP2,CLP2));
          BEGIN
               RETURN Z;
          END F_ARR54;

          FUNCTION F_REC51 RETURN ACC_REC51 IS
               Z : ACC_REC51 := NEW REC51'(C5 => (C1 => CP1));
          BEGIN
               RETURN Z;
          END F_REC51;

          FUNCTION F_REC52 RETURN ACC_REC52 IS
               Z : ACC_REC52 := NEW REC52'(C6 => (C2 => CLP2));
          BEGIN
               RETURN Z;
          END F_REC52;

          FUNCTION F_REC53 RETURN ACC_REC53 IS
               Z : ACC_REC53 := NEW REC53'(C7 => (C3 => CP1));
          BEGIN
               RETURN Z;
          END F_REC53;

          FUNCTION F_REC54 RETURN ACC_REC54 IS
               Z : ACC_REC54 := NEW REC54'(C8 => (C4 => CLP2));
          BEGIN
               RETURN Z;
          END F_REC54;
     END P;

BEGIN
     NULL;
END B74103I;
