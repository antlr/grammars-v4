-- BC3502O.ADA

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
-- IS THE SAME AS THE DESIGNATED BASE TYPE OF THE ACTUAL PARAMETER.

-- CHECK WHEN THE DESIGNATED TYPE IS A GENERIC FORMAL TYPE DECLARED
-- IN THE SAME FORMAL PART AS FT.

-- CHECK WHEN THE DESIGNATED TYPE IS A FIXED OR FLOAT TYPE.

-- SPS 7/19/82

PROCEDURE BC3502O IS

     SUBTYPE FL IS FLOAT;
     TYPE NFL IS NEW FLOAT;
     TYPE AFL IS ACCESS FL;
     TYPE ANFL IS ACCESS NFL;
     TYPE AFLOAT IS ACCESS FLOAT;

     TYPE FX IS DELTA 0.1 RANGE 1.0 .. 3.0;
     TYPE FXX IS DELTA 0.1 RANGE 1.0 .. 3.0;
     TYPE NFX IS NEW FX;
     SUBTYPE SFX IS FX;
     TYPE AFX IS ACCESS FX;
     TYPE AFXX IS ACCESS FXX;
     TYPE ANFX IS ACCESS NFX;
     TYPE ASFX IS ACCESS SFX;

     GENERIC
          TYPE T IS DIGITS <>;
          TYPE FT IS ACCESS T;
     PACKAGE P IS END P;

     GENERIC
          TYPE T IS DELTA <>;
          TYPE FT IS ACCESS T;
     PACKAGE PP IS END PP;

     PACKAGE P1 IS NEW P (FLOAT, AFLOAT);    -- OK.
     PACKAGE P2 IS NEW P (FLOAT, AFL);       -- OK.
     PACKAGE P3 IS NEW P (FLOAT, ANFL);      -- ERROR: ANFL.

     PACKAGE PP1 IS NEW PP (FX, AFX);        -- OK.
     PACKAGE PP2 IS NEW PP (FX, AFXX);       -- ERROR: AFXX.
     PACKAGE PP3 IS NEW PP (FX, ANFX);       -- ERROR: ANFX.
     PACKAGE PP4 IS NEW PP (FX, ASFX);       -- OK.

BEGIN
     NULL;
END BC3502O;
