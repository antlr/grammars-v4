-- B58003A.ADA

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
-- CHECK THAT THE TYPE OF A RETURN STATEMENT'S EXPRESSION MUST MATCH
--    THE RETURN TYPE SPECIFIED IN THE FUNCTION'S SPECIFICATION.

-- DCB 2/11/80
-- JRK 11/3/80
-- SPS 11/3/82
-- SPS 11/22/82

PROCEDURE B58003A IS

     SUBTYPE INT IS INTEGER RANGE 0..10;
     I1 : INTEGER;

     FUNCTION FNC2 RETURN INT IS
          I2 : INTEGER RANGE 5..15 := 6;
     BEGIN
          RETURN I2;     -- LEGAL: TYPES MATCH, SUBTYPES OVERLAP.
     END FNC2;

     FUNCTION FNC1 RETURN INTEGER IS
     BEGIN
          RETURN 'Z';    -- ERROR: EXPRESSION TYPE DOES NOT MATCH.
     END FNC1;

BEGIN
     I1 := FNC2;
     I1 := FNC1;
END B58003A;
