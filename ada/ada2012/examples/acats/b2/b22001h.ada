-- B22001H.ADA

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
-- CHECK THAT AN IDENTIFIER, RESERVED WORD, COMPOUND SYMBOL,
--  INTEGER LITERAL, CHARACTER LITERAL, STRING LITERAL, OR COMMENT 
--  CANNOT BE CONTINUED ACROSS A LINE BOUNDARY.

-- COMMENT CROSSES LINE BOUNDARY.

-- DCB 12/18/79
-- JRK 4/21/80
-- JRK 12/16/80
-- CPP 5/23/84

PROCEDURE B22001H IS

     TYPE INTE IS NEW INTEGER;
     I  : INTEGER;
     EX : INTEGER;
     I1 : INTEGER;
     B1 : BOOLEAN;
     C1 : CHARACTER;
     S1 : STRING (1..6);

     I2 : INTEGER;
     I3 : INTEGER;

BEGIN

     -- THIS COMMENT WILL BE ILLEGAL BECAUSE IT WILL CROSS A LINE
BOUNDARY.;            -- ERROR: PRECEDING COMMENT CROSSES LINE BOUNDARY.
     NULL;

END B22001H;
