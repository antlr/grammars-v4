-- B83027D.ADA

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
--     IF A DECLARATION IN THE DISCRIMINANT PART OF:  A PRIVATE TYPE
--     DECLARATION, AN INCOMPLETE TYPE DECLARATION, OR A GENERIC FORMAL
--     TYPE DECLARATION HIDES AN OUTER DECLARATION OF A HOMOGRAPH, THEN
--     CHECK THAT A USE OF THE COMMON IDENTIFIER WHICH WOULD BE A LEGAL
--     REFERENCE TO THE OUTER DECLARATION MUST BE REJECTED IF IT IS
--     ILLEGAL AS A REFERENCE TO THE INNER.

-- HISTORY:
--     BCB 09/06/88  CREATED ORIGINAL TEST.
--     RLB 02/06/18  CORRECTED FORMAT OF ERROR TAGS, ADDED ERROR LOCATION
--                   INDICATORS.

PROCEDURE B83027D IS

     GENERIC
          TYPE T IS PRIVATE;
          X : T;
     FUNCTION GEN_FUN RETURN T;

     FUNCTION GEN_FUN RETURN T IS
     BEGIN
          RETURN X;
     END GEN_FUN;

BEGIN

     ONE:
     DECLARE             -- PRIVATE TYPE DECLARATION.
          INT : INTEGER := 1;

          TYPE ENUM IS (ONE, TWO, THREE, FOUR);

          FUNCTION F IS NEW GEN_FUN (INTEGER, INT);

          PACKAGE P_ONE IS
               TYPE INNER (F : ENUM := ONE;
                           Y : INTEGER := F) IS PRIVATE;  -- ERROR: {43;13}
          PRIVATE
               TYPE INNER (F : ENUM := ONE; Y : INTEGER := F) IS RECORD
                                                -- OPTIONAL ERROR: {1:16}
                    NULL;
               END RECORD;
          END P_ONE;

     BEGIN  -- ONE
          NULL;
     END ONE;

     TWO:
     DECLARE             -- INCOMPLETE TYPE DECLARATION.
          INT : INTEGER := 1;

          TYPE ENUM IS (ONE, TWO, THREE, FOUR);

          FUNCTION F IS NEW GEN_FUN (INTEGER, INT);

          TYPE INNER (F : ENUM := ONE;
                      Y : INTEGER := F);               -- ERROR: {38;2}

          TYPE INNER (F : ENUM := ONE; Y : INTEGER := F) IS RECORD
                                                   -- OPTIONAL ERROR: {1:11}
               NULL;
          END RECORD;

     BEGIN  -- TWO
          NULL;
     END TWO;

     THREE:
     DECLARE             -- GENERIC FORMAL TYPE DECLARATION.
          INT : INTEGER := 1;

          TYPE ENUM IS (ONE, TWO, THREE, FOUR);

          TYPE F IS NEW INTEGER;

          GENERIC
               TYPE INNER (F : ENUM;
                           Y : F) IS PRIVATE;          -- ERROR: {32;13}
          PACKAGE P_THREE IS
          END P_THREE;

     BEGIN  -- THREE
          NULL;
     END THREE;

END B83027D;
