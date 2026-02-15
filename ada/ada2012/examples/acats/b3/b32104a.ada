-- B32104A.ADA

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
--     CHECK THAT UNCONSTRAINED ARRAY DEFINITIONS ARE NOT PERMITTED
--     IN OBJECT DECLARATIONS.

-- HISTORY:
--     BCB 01/20/88  CREATED ORIGINAL TEST.
--     THS 03/28/90  MOVED "-- ERROR:" TO THE RIGHT OF THE CONSTRUCT
--                   IN ERROR.
--     DTN 11/30/95  REMOVED CONFORMANCE CHECKS WHERE RULES RELAXED.
--     PWN 02/01/96  RESTORED CHECKS IN ADA 95 LEGAL FORMAT.
--                   3.3.1(9);6.0, 3.3.1(33.c);6.0


PROCEDURE B32104A IS

     TYPE INT IS (JOHN, VINCE, DAVE, TOM, MIKE);

     V1 : ARRAY(INTEGER RANGE <>) OF FLOAT;     -- ERROR: UNCONSTRAINED
                                                -- ARRAY DEFINITION IN
                                                -- OBJECT DECLARATION.

     V2 : CONSTANT ARRAY(INT RANGE <>)          -- OK.
          OF INTEGER := (1,2,3,4,5);            
                                                

     V3 : CONSTANT ARRAY(INTEGER RANGE <>)      -- OK.
          OF INTEGER := (1,2);                  
                                                

     V4 : ARRAY(INT RANGE <>) OF INTEGER;       -- ERROR: UNCONSTRAINED
                                                -- ARRAY DEFINITION IN
                                                -- OBJECT DECLARATION.

BEGIN
     NULL;
END B32104A;
