-- BE3803A.ADA

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
-- CHECK THAT GET AND PUT FOR FLOAT AND FIXED POINT NUMBERS ARE NOT
-- AVAILABLE WITHOUT INSTANTIATING FIXED_IO OR FLOAT_IO.

-- SPS 10/6/82
-- EG  05/30/84

WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE BE3803A IS

     TYPE FIX IS DELTA 0.2 RANGE 1.0 .. 10.0;
     TYPE FL IS NEW FLOAT RANGE 1.0 .. 10.0;

     A : FIX;
     B : FL;
     C : CHARACTER;
     D : FLOAT;
     FT :FILE_TYPE;
     P : POSITIVE;
     ST : STRING (1..10);

BEGIN

     PUT (A);            -- ERROR: A.
     PUT (B);            -- ERROR: B.
     PUT (C);            -- OK.
     PUT (D);            -- ERROR: D.

     PUT (ST, A);        -- ERROR: A.
     PUT (ST, B);        -- ERROR: B.
     PUT (ST, D);        -- ERROR: D.

     PUT (FT, A);        -- ERROR: A.
     PUT (FT, B);        -- ERROR: B.
     PUT (FT, C);        -- OK.
     PUT (FT, D);        -- ERROR: D.

     GET (A);            -- ERROR: A.
     GET (B);            -- ERROR: B.
     GET (C);            -- OK.
     GET (D);            -- ERROR: D.

     GET (ST, A, P);     -- ERROR: A.
     GET (ST, B, P);     -- ERROR: B.
     GET (ST, D, P);     -- ERROR: D.

     GET (FT, A);        -- ERROR: A.
     GET (FT, B);        -- ERROR: B.
     GET (FT, C);        -- OK.
     GET (FT, D);        -- ERROR: D.

END BE3803A;
