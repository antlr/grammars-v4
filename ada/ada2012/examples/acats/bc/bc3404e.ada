-- BC3404E.ADA

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

-- TEST FIXED, FLOAT AND TASK TYPES.

-- CHECK WHEN THE COMPONENT TYPES ARE NOT GENERIC FORMAL PARAMETERS.

-- SPS 6/24/82

PROCEDURE BC3404E IS

     TYPE FIX IS DELTA 0.1 RANGE 1.0 .. 3.0;
     SUBTYPE SFIX IS FIX RANGE 1.0 .. 3.0;
     TYPE NFIX IS NEW FIX;

     SUBTYPE SFL IS FLOAT;
     TYPE NFL IS NEW FLOAT;

     TASK TYPE TSK IS
          ENTRY X;
     END;
     SUBTYPE STSK IS TSK;
     TYPE NTSK IS NEW TSK;
     SUBTYPE NATURAL IS INTEGER RANGE 1..INTEGER'LAST;

     TYPE AR_FIX IS ARRAY (NATURAL) OF FIX;
     TYPE AR_SFIX IS ARRAY (NATURAL) OF SFIX;
     TYPE AR_NFIX IS ARRAY (NATURAL) OF NFIX;
     TYPE AR_FL IS ARRAY (NATURAL) OF FLOAT;
     TYPE AR_NFL IS ARRAY (NATURAL) OF NFL;
     TYPE AR_SFL IS ARRAY (NATURAL) OF SFL;
     TYPE AR_TSK IS ARRAY (NATURAL) OF TSK;
     TYPE AR_STSK IS ARRAY (NATURAL) OF STSK;
     TYPE AR_NTSK IS ARRAY (NATURAL) OF NTSK;

     TASK BODY TSK IS
     BEGIN
          ACCEPT X DO
               NULL;
          END X;
     END TSK;

     GENERIC
          TYPE FT IS ARRAY (NATURAL) OF FIX;
     PACKAGE PAR_FIX IS END PAR_FIX;

     GENERIC
          TYPE FT IS ARRAY (NATURAL) OF FLOAT;
     PACKAGE PAR_FL IS END PAR_FL;

     GENERIC
          TYPE FT IS ARRAY (NATURAL) OF TSK;
     PACKAGE PAR_TSK IS END PAR_TSK;

     PACKAGE PAR_FIX1 IS NEW PAR_FIX (AR_FIX);    -- OK.
     PACKAGE PAR_FIX2 IS NEW PAR_FIX (AR_SFIX);   -- OK.
     PACKAGE PAR_FIX3 IS NEW PAR_FIX (AR_NFIX);   -- ERROR: AR_NFIX.

     PACKAGE PAR_FL1 IS NEW PAR_FL (AR_FL);       -- OK.
     PACKAGE PAR_FL2 IS NEW PAR_FL (AR_SFL);      -- OK.
     PACKAGE PAR_FL3 IS NEW PAR_FL (AR_NFL);      -- ERROR: AR_NFL.

     PACKAGE PAR_TSK1 IS NEW PAR_TSK (AR_TSK);    -- OK.
     PACKAGE PAR_TSK2 IS NEW PAR_TSK (AR_STSK);   -- OK.
     PACKAGE PAR_TSK3 IS NEW PAR_TSK (AR_NTSK);   -- ERROR: AR_NTSK.

BEGIN
     NULL;
END BC3404E;
