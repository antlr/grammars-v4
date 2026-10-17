-- B83031F.ADA

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
--     CHECK THAT IF AN IMPLICIT DECLARATION OF A PREDEFINED OPERATOR IS
--     HIDDEN BY A GENERIC FORMAL SUBPROGRAM DECLARATION, THEN A USE OF
--     THE COMMON IDENTIFIER OF HOMOGRAPHS MUST BE REJECTED IF IT WOULD
--     BE LEGAL FOR THE HIDDEN IMPLICIT DECLARATION BUT ILLEGAL FOR THE
--     VISIBLE EXPLICIT DECLARATION.

-- HISTORY:
--     BCB 09/19/88  CREATED ORIGINAL TEST.

PROCEDURE B83031F IS

BEGIN
     DECLARE
          TYPE INT IS RANGE -20 .. 20;

          N : INT := 5;

          GENERIC
               WITH FUNCTION "*" (L, R : INT) RETURN INT;
          PACKAGE P IS
          END P;

          PACKAGE BODY P IS
          BEGIN
               N := "*" (LEFT => 2, RIGHT => 2);       -- ERROR:
          END P;
     BEGIN
          NULL;
     END;

END B83031F;
