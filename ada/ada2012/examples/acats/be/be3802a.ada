-- BE3802A.ADA

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
-- CHECK THAT FLOAT_IO MUST BE INSTANTIATED WITH A FLOAT TYPE AND
-- FIXED_IO MUST BE INSTANTIATED WITH A FIXED TYPE.

-- SPS 9/7/82

WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE BE3802A IS

     TYPE N_FLOAT IS NEW FLOAT;
     SUBTYPE SFL IS FLOAT RANGE 1.0 .. 10.0;
     TYPE FL1 IS DIGITS 6;
     TYPE FL2 IS DIGITS 3 RANGE 1.25 .. 2.15;
     TYPE FIX IS DELTA 0.5 RANGE 0.0 .. 10.0;
     SUBTYPE SFX IS FIX DELTA 1.0;
     TYPE INT IS NEW INTEGER;

     PACKAGE FL1_IO IS NEW FLOAT_IO (FLOAT);      -- OK.
     PACKAGE FL2_IO IS NEW FLOAT_IO (N_FLOAT);    -- OK.
     PACKAGE FL3_IO IS NEW FLOAT_IO (SFL);        -- OK.
     PACKAGE FL4_IO IS NEW FLOAT_IO (FL1);        -- OK.
     PACKAGE FL5_IO IS NEW FLOAT_IO (FL2);        -- OK.
     PACKAGE FL6_IO IS NEW FLOAT_IO (FIX);        -- ERROR: FIX.
     PACKAGE FL7_IO IS NEW FLOAT_IO (SFX);        -- ERROR: SFX.
     PACKAGE FL8_IO IS NEW FLOAT_IO (INT);        -- ERROR: INT.

     PACKAGE FX1_IO IS NEW FIXED_IO (FIX);        -- OK.
     PACKAGE FX2_IO IS NEW FIXED_IO (SFX);        -- OK.
     PACKAGE FX3_IO IS NEW FIXED_IO (FLOAT);      -- ERROR: FLOAT.
     PACKAGE FX4_IO IS NEW FIXED_IO (FL1);        -- ERROR: FL1.
     PACKAGE FX5_IO IS NEW FIXED_IO (INT);        -- ERROR: INT.
     PACKAGE FX6_IO IS NEW FIXED_IO (INTEGER);    -- ERROR: INTEGER.

BEGIN
     NULL;
END BE3802A;
