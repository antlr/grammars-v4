-- B95070A.ADA

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
-- CHECK THAT A FORMAL IN PARAMETER CANNOT BE USED AS THE TARGET OF AN
-- ASSIGNMENT STATEMENT OR AS AN ACTUAL PARAMETER WHOSE MODE IS OUT
-- OR IN OUT.

-- JWC 7/10/85

PROCEDURE B95070A IS

     TYPE TABLE IS ARRAY (1..3) OF INTEGER;

     TYPE REC IS
          RECORD
               I    : INTEGER;
               A    : TABLE;
          END RECORD;

     TYPE PTR IS ACCESS INTEGER;

     TASK T1 IS

          ENTRY E0 (I : IN INTEGER; T : IN TABLE; R : IN REC;
                    P : IN PTR);

     END T1;

     TASK BODY T1 IS
     BEGIN

          ACCEPT E0 (I : IN INTEGER; T : IN TABLE; R : IN REC;
                     P : IN PTR) DO

               DECLARE
                    TASK T2 IS

                         ENTRY E1 (I : IN OUT INTEGER);

                         ENTRY E2 (I : OUT INTEGER);

                         ENTRY E3 (T : IN OUT TABLE);

                         ENTRY E4 (T : OUT TABLE);

                         ENTRY E5 (R : IN OUT REC);

                         ENTRY E6 (R : OUT REC);

                         ENTRY E7 (P : IN OUT PTR);

                         ENTRY E8 (P : OUT PTR);

                    END T2;

                    TASK BODY T2 IS
                    BEGIN
                         NULL;
                    END T2;

               BEGIN

                    I := 3;       -- ERROR: IN PARAMETER ASSIGNED TO.
                    T2.E1 (I);    -- ERROR: IN PARAMETER USED AS IN OUT.
                    T2.E2 (I);    -- ERROR: IN PARAMETER USED AS OUT.

                    T := (1,2,3); -- ERROR: IN PARAMETER ASSIGNED TO.
                    T2.E3 (T);    -- ERROR: IN PARAMETER USED AS IN OUT.
                    T2.E4 (T);    -- ERROR: IN PARAMETER USED AS OUT.

                    T(1) := 3;    -- ERROR: IN PARAMETER ASSIGNED TO.
                    T2.E1 (T(1)); -- ERROR: IN PARAMETER USED AS IN OUT.
                    T2.E2 (T(1)); -- ERROR: IN PARAMETER USED AS OUT.

                    R := (1,(1,2,3)); -- ERROR: IN PARAMETER
                                  -- ASSIGNED TO.
                    T2.E5 (R);    -- ERROR: IN PARAMETER USED AS IN OUT.
                    T2.E6 (R);    -- ERROR: IN PARAMETER USED AS OUT.

                    R.I := 3;     -- ERROR: IN PARAMETER ASSIGNED TO.
                    T2.E1 (R.I);  -- ERROR: IN PARAMETER USED AS IN OUT.
                    T2.E2 (R.I);  -- ERROR: IN PARAMETER USED AS OUT.

                    P := NULL;    -- ERROR: IN PARAMETER ASSIGNED TO.
                    T2.E7 (P);    -- ERROR: IN PARAMETER USED AS IN OUT.
                    T2.E8 (P);    -- ERROR: IN PARAMETER USED AS OUT.
               END;

          END E0;
     END T1;

BEGIN
     NULL;
END B95070A;
