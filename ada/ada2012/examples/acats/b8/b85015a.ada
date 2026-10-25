-- B85015A.ADA

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
-- CHECK THAT A RENAMED ENTRY CANNOT BE USED AS THE PREFIX TO THE
-- 'COUNT ATTRIBUTE.

-- EG  02/22/84
-- RLB 03/15/07 Corrected limited function returns. Removed cases made
--              legal by the Amendment.

PROCEDURE B85015A IS

     TASK TYPE T IS
          ENTRY E;
     END T;

     TSK : T;

     FUNCTION FUN RETURN T;

     PROCEDURE PROC1 RENAMES FUN.E;
     PROCEDURE PROC2 RENAMES PROC1;

     TASK BODY T IS
          I   : INTEGER;
     BEGIN
          I := PROC1'COUNT;                             -- ERROR: C.
          I := PROC2'COUNT;                             -- ERROR: C.
     END T;

     FUNCTION FUN RETURN T IS
     BEGIN
          RETURN TSK : T;
     END FUN;

BEGIN

     NULL;

END B85015A;
