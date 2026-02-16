-- B85008G.ADA

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
--     CHECK THAT AN EXCEPTION RENAMING DECLARATION CANNOT RENAME AN
--     ENUMERATION LITERAL, BLOCK NAME, LOOP NAME, LABEL, TYPE, SUBTYPE,
--     SUBPROGRAM, PACKAGE, GENERIC UNIT, ENTRY, OR ENTRY FAMILY.

-- HISTORY:
--     JET 07/25/88  CREATED ORIGINAL TEST.
--     THS 03/29/90  SPLIT TEST TO B85008I.ADA.

PROCEDURE B85008G IS

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

     A : EXCEPTION RENAMES FIDELIS;           -- ERROR: ENUM LITERAL.
     B : EXCEPTION RENAMES ENUM;              -- ERROR: TYPE NAME.
     C : EXCEPTION RENAMES ST;                -- ERROR: SUBTYPE.
     D : EXCEPTION RENAMES PROC;              -- ERROR: SUBPROGRAM.
     E : EXCEPTION RENAMES PACK;              -- ERROR: PACKAGE.
     F : EXCEPTION RENAMES GPROC;             -- ERROR: GENERIC UNIT.
     G : EXCEPTION RENAMES TSK.E;             -- ERROR: ENTRY.
     H : EXCEPTION RENAMES TSK.F;             -- ERROR: ENTRY FAMILY.

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
               J : EXCEPTION RENAMES BLOCK_NAME; -- ERROR: BLOCK NAME.
               K : EXCEPTION RENAMES LOOP_NAME;  -- ERROR: LOOP NAME.
               L : EXCEPTION RENAMES LABEL_NAME; -- ERROR: LABEL NAME.
          BEGIN
               NULL;
          END BLOCK_NAME;
     END LOOP LOOP_NAME;
END B85008G;
