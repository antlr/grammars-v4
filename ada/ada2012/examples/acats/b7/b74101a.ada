-- B74101A.ADA

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
-- CHECK THAT A PRIVATE TYPE CANNOT BE FULLY DECLARED AS
-- A TYPE FOR WHICH ASSIGNMENT IS NOT AVAILABLE.

-- CHANGE HISTORY:
--      13 Apr 1981   DAT
--      20 Apr 1982   SPS
--      05 Jan 1983   VKG
--      23 Sep 1983   JBG
--      22 Apr 2021   RLB   Added error location indicators.
--!
PROCEDURE B74101A IS

     PACKAGE PK IS

          TYPE L IS LIMITED PRIVATE;
          TYPE L1 IS LIMITED PRIVATE;
          TYPE L2 IS LIMITED PRIVATE;
          TYPE ARL2 IS LIMITED PRIVATE;

          TYPE P1 IS PRIVATE;
          TYPE P2 IS PRIVATE;
          TYPE P3 IS PRIVATE;
          TYPE P5 IS PRIVATE;
          TYPE P6 IS PRIVATE;
          TYPE P7 IS PRIVATE;
          TYPE P8 IS PRIVATE;
          TYPE P8A IS PRIVATE;
          TYPE P9 IS PRIVATE;
          TYPE P10 IS PRIVATE;
          TYPE P11 IS PRIVATE;
          TYPE P12 IS PRIVATE;
          TYPE P13 IS PRIVATE;
          TYPE P14 IS PRIVATE;
          TYPE P15 IS PRIVATE;
          TYPE P16 IS PRIVATE;
          TYPE P17 IS PRIVATE;
          TYPE P18 IS PRIVATE;
          TYPE P19 IS PRIVATE;
          TYPE P20 IS PRIVATE;
          TYPE P21 IS PRIVATE;
          TYPE P22 IS PRIVATE;
          TYPE P23 IS PRIVATE;
          TYPE P24 IS PRIVATE;
          TYPE P25 IS PRIVATE;
          TYPE P26 IS PRIVATE;
          TYPE P27 IS PRIVATE;
          TYPE P28 IS PRIVATE;
          TYPE P29 IS PRIVATE;
          TYPE P30 IS PRIVATE;
          TYPE P31 IS PRIVATE;

          PACKAGE PK1 IS
               TYPE LP IS LIMITED PRIVATE;
               TYPE LP1 IS LIMITED PRIVATE;
               TYPE PP IS PRIVATE;
          PRIVATE
               TYPE LP IS (E1);
               TYPE LP1 IS NEW INTEGER;
               TYPE PP IS NEW LP1;
          END PK1;
          USE PK1;

          TYPE R1 IS RECORD
               C : L;
          END RECORD;
          TYPE A1 IS ARRAY (0..1) OF L;

          TASK TYPE TSK;
          TYPE A2 IS ARRAY (0..1) OF TSK;
          TYPE R2 IS RECORD
               C : TSK;
          END RECORD;

          TYPE RR2 IS ARRAY (1..2) OF R2;

     PRIVATE

          TYPE L IS RANGE 0 .. 1;
          TYPE L3 IS NEW L;
          TYPE L1 IS RANGE 0 .. 1;
          TYPE L4 IS NEW L1;
          TYPE ARL2 IS ARRAY(0..1) OF L2;

          TYPE P1 IS NEW L3;                 -- OK. {11;1}
          TYPE P2 IS NEW L1;                 -- OK. {11;1}
          TYPE P3 IS NEW LP;                 -- ERROR: LP IS LIM PRIV. {11;1}
          TYPE P5 IS NEW TSK;                -- ERROR: NO := FOR TSK. {11;1}
          TYPE P6 IS NEW R1;                 -- OK. {11;1}
          TYPE P7 IS NEW A1;                 -- OK. {11;1}
          TYPE P8 IS NEW R2;                 -- ERROR: NO := FOR R2. {11;1}
          TYPE P8A IS NEW RR2;               -- ERROR: NO := FOR RR2. {11;1}
          TYPE P9 IS NEW A2;                 -- ERROR: NO := FOR A2. {11;1}
          TYPE P10 IS NEW PP;                -- OK. {11;1}
          TYPE P11 IS NEW L4;                -- OK. {11;1}
          TYPE P12 IS NEW LP1;               -- ERROR: LP1 IS LIM PRIV. {11;1}
          TYPE P13 IS RECORD
               C : L3; END RECORD;           -- OK. {1:11;1}
          TYPE P14 IS RECORD
               C : L1; END RECORD;           -- OK. {1:11;1}
          TYPE P15 IS RECORD
               C : L2; END RECORD;           -- ERROR: L2 LIM PRIV. {1:11;1}
          TYPE P16 IS RECORD
               C : LP; END RECORD;           -- ERROR: LP LIM PRIV. {1:11;1}
          TYPE P17 IS RECORD
               C : TSK; END RECORD;          -- ERROR: NO := FOR TSK. {1:11;1}
          TYPE P18 IS RECORD
               C : R1; END RECORD;           -- OK. {1:11;1}
          TYPE P19 IS RECORD
               C : A1; END RECORD;           -- OK. {1:11;1}
          TYPE P20 IS RECORD
               C : R2; END RECORD;           -- ERROR: NO := FOR R2. {1:11;1}
          TYPE P21 IS RECORD
               C : A2; END RECORD;           -- ERROR: NO := FOR A2. {1:11;1}
          TYPE P22 IS RECORD
               C : ARL2;
               END RECORD;                   -- ERROR: NO := FOR ARL2. {2:11;1}
          TYPE P23 IS ARRAY (0..1) OF L3;    -- OK. {11;1}
          TYPE P24 IS ARRAY (0..1) OF L1;    -- OK. {11;1}
          TYPE P25 IS ARRAY (0..1) OF LP;    -- ERROR: LP LIM PRIV. {11;1}
          TYPE P26 IS ARRAY (0..1) OF L2;    -- ERROR: L2 LIM PRIV. {11;1}
          TYPE P27 IS ARRAY (0..1) OF TSK;   -- ERROR: NO := FOR TSK. {11;1}
          TYPE P28 IS ARRAY (0..1) OF R1;    -- OK. {11;1}
          TYPE P29 IS ARRAY (0..1) OF A1;    -- OK. {11;1}
          TYPE P30 IS ARRAY (0..1) OF R2;    -- ERROR: NO := FOR R2. {11;1}
          TYPE P31 IS ARRAY (0..1) OF A2;    -- ERROR: NO := FOR A2. {11;1}

          TYPE L2 IS NEW L1;

     END PK;

     PACKAGE BODY PK IS
          TASK BODY TSK IS
          BEGIN NULL; END TSK;
     BEGIN NULL; END PK;

BEGIN
     NULL;
END B74101A;
