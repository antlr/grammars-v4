-- BC3404D.ADA

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
-- CHECK THAT THE COMPONENT BASE TYPE OF A GENERIC ARRAY TYPE MUST BE
-- THE SAME AS THE COMPONENT BASE TYPE OF THE ACTUAL PARAMETER.

-- CHECK WHEN THE ACTUAL PARAMETER'S COMPONENT TYPE IS A GENERIC FORMAL
-- PARAMETER DECLARED IN AN ENCLOSING GENERIC UNIT.

-- SPS 6/23/82
-- JRL 11/14/95  Removed Ada95-incompatible cases (corresponding component
--               subtypes must statically match).
-- PWN 03/28/96  Restored checks in Ada95 legal format.

PROCEDURE BC3404D IS

     GENERIC
          TYPE T IS (<>);
          TYPE TP IS RANGE <>;
          TYPE PV IS PRIVATE;
          TYPE LP IS LIMITED PRIVATE;
          TYPE ARR IS ARRAY (INTEGER) OF INTEGER;
          TYPE ACC IS ACCESS INTEGER;
     PACKAGE PACK IS
          SUBTYPE CT IS T RANGE T'VAL(1) .. T'VAL(3);
          TYPE NT IS NEW T;
          TYPE NPV IS NEW PV;
          SUBTYPE SACC IS ACC;
          TYPE NACC IS NEW ACC;
          SUBTYPE NATURAL IS INTEGER RANGE 1..INTEGER'LAST;

          TYPE AR_T IS ARRAY (NATURAL) OF T;
          TYPE AR_TP IS ARRAY (NATURAL) OF TP;
          TYPE AR_CT IS ARRAY (NATURAL) OF CT;
          TYPE AR_NT IS ARRAY (NATURAL) OF NT;
          TYPE AR_PV IS ARRAY (NATURAL) OF PV;
          TYPE AR_NPV IS ARRAY (NATURAL) OF NPV;
          TYPE AR_LP IS ARRAY (NATURAL) OF LP;
          TYPE AR_ARR IS ARRAY (NATURAL) OF ARR;
          TYPE AR_ACC IS ARRAY (NATURAL) OF ACC;
          TYPE AR_SACC IS ARRAY (NATURAL) OF SACC;
          TYPE AR_NACC IS ARRAY (NATURAL) OF NACC;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF T;
          PACKAGE PAR_T IS END PAR_T;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF PV;
          PACKAGE PAR_PV IS END PAR_PV;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF ACC;
          PACKAGE PAR_ACC IS END PAR_ACC;

          PACKAGE PAR_T1 IS NEW PAR_T (AR_T);        -- OK.
          PACKAGE PAR_T2 IS NEW PAR_T (AR_TP);       -- ERROR: AR_TP.
          PACKAGE PAR_T3 IS NEW PAR_T (AR_PV);       -- ERROR: AR_PV.
          PACKAGE PAR_T4 IS NEW PAR_T (AR_LP);       -- ERROR: AR_LP.
          PACKAGE PAR_T5 IS NEW PAR_T (AR_CT);       -- ERROR: AR_CT.
          PACKAGE PAR_T6 IS NEW PAR_T (AR_NT);       -- ERROR: AR_NT.

          PACKAGE PAR_PV1 IS NEW PAR_PV (AR_PV);     -- OK.
          PACKAGE PAR_PV2 IS NEW PAR_PV (AR_LP);     -- ERROR: AR_LP.
          PACKAGE PAR_PV3 IS NEW PAR_PV (AR_T);      -- ERROR: AR_T.
          PACKAGE PAR_PV4 IS NEW PAR_PV (AR_ACC);    -- ERROR: AR_ACC.
          PACKAGE PAR_PV5 IS NEW PAR_PV (AR_NPV);    -- ERROR: AR_NPV.

          PACKAGE PAR_ACC1 IS NEW PAR_ACC (AR_ACC);  -- OK.
          PACKAGE PAR_ACC2 IS NEW PAR_ACC (AR_ARR);  -- ERROR: AR_ARR.
          PACKAGE PAR_ACC3 IS NEW PAR_ACC (AR_PV);   -- ERROR: AR_PV.
          PACKAGE PAR_ACC4 IS NEW PAR_ACC (AR_LP);   -- ERROR: AR_LP.
          PACKAGE PAR_ACC5 IS NEW PAR_ACC (AR_SACC); -- OK.
          PACKAGE PAR_ACC6 IS NEW PAR_ACC (AR_NACC); -- ERROR: AR_NACC.

     END PACK;

BEGIN
     NULL;
END BC3404D;
