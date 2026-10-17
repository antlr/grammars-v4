-- BD1B06J.ADA

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
-- OBJECTIVE:
--     CHECK THAT A 'SMALL CLAUSE FOR T IS ILLEGAL IF AN EXPRESSION IN
--     THE CLAUSE CONTAINS A FORCING OCCURRENCE OF T.

-- HISTORY:
--     DHH 08/19/88 CREATED ORIGINAL TEST.
--     BCB 10/03/90 DELETED THE CASE WHICH USED A TYPE CONVERSION, A
--                  NON-STATIC EXPRESSION.  ALSO DELETED THE CASE WHICH
--                  USED A BOOLEAN SUBTYPE DECLARATION.
--     SAIC 12/02/95 Removed obsolete attribute 'Safe_Small for ACVC 2.0.1.

PROCEDURE BD1B06J IS

BEGIN

     DECLARE
          TYPE T IS DELTA 0.25 RANGE -10.0 .. 10.0;
          SUBTYPE SUB_T IS T;

          FOR T'SMALL USE SUB_T'DELTA;              -- ERROR: ATTRIBUTE.

     BEGIN
          NULL;
     END;

     DECLARE
          TYPE T IS DELTA 0.25 RANGE -10.0 .. 10.0;
          SUBTYPE SUB_T IS T;

          FOR T'SMALL USE SUB_T'(0.125);        -- ERROR: QUALIFIED EXP.

     BEGIN
          NULL;
     END;

     DECLARE
          TYPE T IS DELTA 0.25 RANGE -10.0 .. 10.0;
          SUBTYPE SUB_T IS T;
          SUBTYPE SUB_SUB_T IS SUB_T;

          FOR T'SMALL USE SUB_SUB_T'DELTA;      -- ERROR: ATTRIBUTE.

     BEGIN
          NULL;
     END;

END BD1B06J;
