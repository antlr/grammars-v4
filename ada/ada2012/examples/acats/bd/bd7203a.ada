-- BD7203A.ADA

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
--     CHECK THAT THE PREFIX OF THE 'SIZE ATTRIBUTE CANNOT BE A
--     SUBPROGRAM, GENERIC UNIT, PACKAGE, NAMED NUMBER, LABEL, ENTRY,
--     ENTRY FAMILY, EXCEPTION, OR AN ATTRIBUTE OTHER THAN T'BASE.

-- HISTORY:
--     JET 08/24/88  CREATED ORIGINAL TEST.

PROCEDURE BD7203A IS

     I : INTEGER;

     GENERIC
     PROCEDURE GPROC;

     PACKAGE PACK IS
          J : INTEGER;
     END PACK;

     C : CONSTANT := 0;

     TASK T IS
          ENTRY E;
          ENTRY F(1..3);
     END T;

     PROCEDURE PROC IS
     BEGIN
          NULL;
     END PROC;

     PROCEDURE GPROC IS
     BEGIN
          NULL;
     END GPROC;

     TASK BODY T IS
     BEGIN
          ACCEPT E DO
               NULL;
          END E;
          ACCEPT F(1) DO
               NULL;
          END F;
     END T;

BEGIN
<<START_LABEL>>
     I := PROC'SIZE;                    -- ERROR: SUPROGRAM.
     I := GPROC'SIZE;                   -- ERROR: GENERIC UNIT.
     I := PACK'SIZE;                    -- ERROR: PACKAGE.
     I := C'SIZE;                       -- ERROR: NAMED NUMBER.
     I := START_LABEL'SIZE;             -- ERROR: LABEL.
     I := T.E'SIZE;                     -- ERROR: ENTRY.
     I := T.F'SIZE;                     -- ERROR: ENTRY FAMILY.
     I := CONSTRAINT_ERROR'SIZE;        -- ERROR: EXCEPTION.
     I := INTEGER'FIRST'SIZE;           -- ERROR: ATTRIBUTE.
END BD7203A;
