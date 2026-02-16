-- BE3703A.ADA

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
-- CHECK THAT GET AND PUT FOR INTEGER TYPES ARE NOT AVAILABLE WITHOUT
-- INSTANTIATING INTEGER_IO FOR THAT TYPE.

-- SPS 10/1/82

WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE BE3703A IS

     FT : FILE_TYPE;
     TYPE NI IS NEW INTEGER RANGE 0 .. 6;
     TYPE SI IS NEW INTEGER RANGE 1 .. 50;
     X : NI;
     Y : SI;
     Z : INTEGER;

     PACKAGE NIO IS NEW INTEGER_IO (NI);
     USE NIO;

BEGIN
     GET (FT, X);             -- OK.
     GET (FT, Y);             -- ERROR: Y.
     GET (FT, Z);             -- ERROR: Z.

     GET (X);                 -- OK.
     GET (Y);                 -- ERROR: Y.
     GET (Z);                 -- ERROR: Z.

     PUT (FT, X);             -- OK.
     PUT (FT, Y);             -- ERROR: Y.
     PUT (FT, Z);             -- ERROR: Z.

     PUT (X);                 -- OK.
     PUT (Y);                 -- ERROR: Y.
     PUT (Z);                 -- ERROR: Z.

END BE3703A;
