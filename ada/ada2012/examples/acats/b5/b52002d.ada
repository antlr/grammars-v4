-- B52002D.ADA

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
-- CHECK THAT THE LEFT SIDE OF AN ASSIGNMENT STMT MUST BE A VARIABLE.

-- LHS CANNOT BE A LITERAL.


-- DCB 1/31/80
-- JRK 10/30/80
-- RM  06/09/82  (REVISION FOR ADA-82)
-- RM  07/13/82  (SEPARATE "SYNTAX" FROM "SEMANTICS" (NON-CF SYNTAX))
-- SPS 03/21/83

PROCEDURE B52002D IS

     I1 : INTEGER := 1 ;

BEGIN

     99 := I1;          -- ERROR: LITERAL ON LEFT.

END B52002D;
