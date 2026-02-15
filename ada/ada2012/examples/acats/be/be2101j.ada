-- BE2101J.ADA

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
-- CHECK THAT DIRECT_IO CANNOT BE INSTANTIATED WITH LIMITED TYPES,
-- INCLUDING TASK TYPES AND COMPOSITE TYPES CONTAINING LIMITED
-- COMPONENTS.

-- TBN 2/12/86

WITH DIRECT_IO;

PROCEDURE BE2101J IS

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

     PACKAGE D1 IS NEW DIRECT_IO (LP);            -- ERROR: LP.
     PACKAGE D2 IS NEW DIRECT_IO (TSK);           -- ERROR: TSK.
     PACKAGE D3 IS NEW DIRECT_IO (ARR);           -- ERROR: ARR.
     PACKAGE D4 IS NEW DIRECT_IO (REC);           -- ERROR: REC.
     PACKAGE D5 IS NEW DIRECT_IO (RC);            -- ERROR: RC.
     PACKAGE D6 IS NEW DIRECT_IO (NARR);          -- ERROR: NARR.
     PACKAGE D8 IS NEW DIRECT_IO (NRC);           -- ERROR: NRC.
     PACKAGE D9 IS NEW DIRECT_IO (SRC);           -- ERROR: SRC.

     TASK BODY TSK IS
     BEGIN
          NULL;
     END TSK;

BEGIN
     NULL;
END BE2101J;
