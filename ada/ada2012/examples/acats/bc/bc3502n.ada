-- BC3502N.ADA

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
-- CHECK THAT WHEN A GENERIC FORMAL TYPE IS AN ACCESS TYPE,
-- THE FORMAL TYPE IS ONLY MATCHED WHEN ITS DESIGNATED BASE TYPE
-- IS THE SAME AS THE DESIGNATED TYPE OF THE ACTUAL PARAMETER.

-- CHECK WHEN THE DESIGNATED TYPE IS NOT A GENERIC FORMAL TYPE.

-- CHECK WHEN THE DESIGNATED TYPE IS A FIXED OR FLOAT TYPE.

-- SPS 7/19/82

PROCEDURE BC3502N IS

     SUBTYPE FL IS FLOAT;
     TYPE NFL IS NEW FLOAT;
     TYPE AFLOAT IS ACCESS FLOAT;
     TYPE AFL IS ACCESS FL;
     TYPE ANFL IS ACCESS NFL;

     GENERIC
          TYPE FT IS ACCESS FLOAT;
     PACKAGE P IS END P;

     PACKAGE P1 IS NEW P (AFLOAT);           -- OK.
     PACKAGE P2 IS NEW P (AFL);              -- OK.
     PACKAGE P3 IS NEW P (ANFL);             -- ERROR: ANFL.

     TYPE FX IS DELTA 0.1 RANGE 1.0 .. 3.0;
     TYPE FXX IS DELTA 0.1 RANGE 1.0 .. 3.0;
     TYPE NFX IS NEW FX;
     SUBTYPE SFX IS FX;
     TYPE AFX IS ACCESS FX;
     TYPE AFXX IS ACCESS FXX;
     TYPE ANFX IS ACCESS NFX;
     TYPE ASFX IS ACCESS SFX;

     GENERIC
          TYPE FT IS ACCESS FX;
     PACKAGE PP IS END PP;

     PACKAGE PP1 IS NEW PP (AFX);             -- OK.
     PACKAGE PP2 IS NEW PP (ASFX);            -- OK.
     PACKAGE PP3 IS NEW PP (AFXX);            -- ERROR: AFXX.
     PACKAGE PP4 IS NEW PP (ANFX);            -- ERROR: ANFX.

BEGIN
     NULL;
END BC3502N;
