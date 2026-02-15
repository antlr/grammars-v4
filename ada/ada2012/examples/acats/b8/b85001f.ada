-- B85001F.ADA

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
--     CHECK THAT AN OBJECT RENAMING DECLARATION CANNOT RENAME AN
--     ENUMERATION LITERAL, BLOCK NAME, LOOP NAME, LABEL, TYPE, SUBTYPE,
--     EXCEPTION, SUBPROGRAM, PACKAGE, GENERIC UNIT, ENTRY, ENTRY
--     FAMILY, OR AN ATTRIBUTE WHICH IS A FUNCTION.

-- HISTORY:
--     JET 07/22/88  CREATED ORIGINAL TEST.
--     PWN 10/27/95  REMOVED CHECK FOR A REMOVED RESTRICTION.
--     PWN 12/03/95  CORRECTED FORMATING PROBLEM.
--     PWN 03/28/96  Restored check in Ada95 legal format.

PROCEDURE B85001F IS

     TYPE ENUM IS (SEMPER, FIDELIS);
     SUBTYPE ST IS INTEGER;

     PROCEDURE PROC;

     GENERIC
     PROCEDURE GPROC;

     PACKAGE PACK IS
          TYPE T IS RANGE 0 .. 10;
     END PACK;

     TASK TSK IS
          ENTRY E;
          ENTRY F(ENUM);
     END TSK;

     A : ENUM RENAMES FIDELIS;                -- OK.
     B : INTEGER RENAMES ENUM;                -- ERROR: TYPE NAME.
     C : INTEGER RENAMES ST;                  -- ERROR: SUBTYPE.
     D : INTEGER RENAMES PROGRAM_ERROR;       -- ERROR: EXCEPTION.
     E : INTEGER RENAMES PROC;                -- ERROR: SUBPROGRAM.
     F : INTEGER RENAMES PACK;                -- ERROR: PACKAGE.
     G : INTEGER RENAMES GPROC;               -- ERROR: GENERIC UNIT.
     H : INTEGER RENAMES TSK.E;               -- ERROR: ENTRY.
     I : INTEGER RENAMES TSK.F;               -- ERROR: ENTRY FAMILY.
     J : INTEGER RENAMES INTEGER'PRED;        -- ERROR: ATTRIBUTE.

     PROCEDURE PROC IS
     BEGIN
          NULL;
     END PROC;

     PROCEDURE GPROC IS
     BEGIN
          NULL;
     END GPROC;

     TASK BODY TSK IS
     BEGIN
          ACCEPT E DO
               NULL;
          END E;
          ACCEPT F(SEMPER) DO
               NULL;
          END F;
          ACCEPT F(FIDELIS) DO
               NULL;
          END F;
     END TSK;

BEGIN
<<LABEL_NAME>>
     NULL;
LOOP_NAME:
     LOOP
BLOCK_NAME:
          DECLARE
               K: INTEGER RENAMES BLOCK_NAME; -- ERROR: BLOCK NAME.
               L : INTEGER RENAMES LOOP_NAME;  -- ERROR: LOOP NAME.
               M : INTEGER RENAMES LABEL_NAME; -- ERROR: LABEL NAME.
          BEGIN
               NULL;
          END BLOCK_NAME;
     END LOOP LOOP_NAME;
END B85001F;
