-- BE2101E.ADA

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
-- CHECK THAT SEQUENTIAL_IO CANNOT BE INSTANTIATED WITH LIMITED TYPES,
-- INCLUDING TASK TYPES AND COMPOSITE TYPES CONTAINING LIMITED
-- COMPONENTS.

-- SPS 9/27/82
-- TBN 2/12/86     SPLIT TEST.  PUT DIRECT_IO INTO BE2101J-B.ADA.

WITH SEQUENTIAL_IO;
PROCEDURE BE2101E IS

     TASK TYPE TSK IS
          ENTRY E1;
     END TSK;

     TYPE ARR IS ARRAY (1 .. 10) OF TSK;

     TYPE REC IS RECORD
          T : TSK;
          AR : ARR;
     END RECORD;

     TYPE RC (D : INTEGER := 1) IS RECORD
          C1 : INTEGER;
          CASE D IS
               WHEN 1 .. 10 =>
                    C2 : TSK;
               WHEN 11 .. 20 =>
                    C3 : ARR;
               WHEN 21 .. 30 =>
                    C4 : REC;
               WHEN OTHERS =>
                    C5 : INTEGER;
          END CASE;
     END RECORD;

     PACKAGE PV IS
          TYPE LP IS LIMITED PRIVATE;
     PRIVATE
          TYPE LP IS NEW INTEGER RANGE 1 .. 10;
     END PV;

     TYPE NARR IS NEW ARR;
     TYPE NRC IS NEW RC (13);
     SUBTYPE SRC IS RC (35);

     USE PV;

     PACKAGE S1 IS NEW SEQUENTIAL_IO (LP);        -- ERROR: LP.
     PACKAGE S2 IS NEW SEQUENTIAL_IO (TSK);       -- ERROR: TSK.
     PACKAGE S3 IS NEW SEQUENTIAL_IO (ARR);       -- ERROR: ARR.
     PACKAGE S4 IS NEW SEQUENTIAL_IO (REC);       -- ERROR: REC.
     PACKAGE S5 IS NEW SEQUENTIAL_IO (RC);        -- ERROR: RC.
     PACKAGE S6 IS NEW SEQUENTIAL_IO (NARR);      -- ERROR: NARR.
     PACKAGE S8 IS NEW SEQUENTIAL_IO (NRC);       -- ERROR: NRC.
     PACKAGE S9 IS NEW SEQUENTIAL_IO (SRC);       -- ERROR: SRC.

     TASK BODY TSK IS
     BEGIN
          NULL;
     END TSK;

BEGIN
     NULL;
END BE2101E;
