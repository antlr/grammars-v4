-- B35501A.ADA

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
-- CHECK THAT THE ARGUMENT TO 'SUCC, 'PRED,  AND 'POS
-- MUST BE A DISCRETE TYPE. MATCHING OF PARAMETER AND
-- ATTRIBUTE TYPE IS TESTED ELSEWHERE.

-- DAT 3/26/81
-- PWN 11/09/95  REMOVED 'PRED AND 'SUCC CHECKS WHERE RULES RELAXED.
-- PWN 03/21/96  Restored checks in Ada 95 legal format.

PROCEDURE B35501A IS

     SUBTYPE S IS STRING (1..1);
     TYPE F IS DELTA 1.0 RANGE 0.0 .. 2.0;

     PACKAGE PKG IS
          TYPE P IS PRIVATE;
          TYPE L IS LIMITED PRIVATE;
          OP : CONSTANT P;
          OL : CONSTANT L;
     PRIVATE
          TYPE P IS RANGE 0 .. 2;
          TYPE L IS (A, B, C);
          OP : CONSTANT P := 1;
          OL : CONSTANT L := B;
     END PKG;
     USE PKG;

     R : BOOLEAN;
     I : INTEGER;

BEGIN

     R := S'SUCC("A") IN S;                  -- ERROR: NOT DISCRETE.
     R := CHARACTER'SUCC("A") IN CHARACTER;  -- ERROR: NOT DISCRETE.
     R := F'SUCC(1.0) IN F;                  -- OK.
     R := INTEGER'SUCC(1.0) IN INTEGER;      -- ERROR: NOT DISCRETE.
     R := P'SUCC(OP) = OP;                   -- ERROR: PRIVATE.
     R := OL IN L;                           -- OK.
     R := L'SUCC(OL) IN L;                   -- ERROR: LIM PRIV.

     R := S'PRED("B") IN S;                  -- ERROR: NOT DISCRETE.
     R := CHARACTER'PRED("B") IN CHARACTER;  -- ERROR: NOT DISCRETE.
     R := F'PRED(1.0) IN F;                  -- OK.
     R := INTEGER'PRED(1.0) IN INTEGER;      -- ERROR: NOT DISCRETE.
     R := P'PRED(OP) IN P;                   -- ERROR: NOT DISCRETE.
     R := L'PRED(OL) IN L;                   -- ERROR: NOT DISCRETE.

     I := S'POS("A");                        -- ERROR: NOT DISCRETE.
     I := CHARACTER'POS("A");                -- ERROR: NOT DISCRETE.
     I := F'POS(1.0);                        -- ERROR: NOT DISCRETE.
     I := INTEGER'POS(1.0);                  -- ERROR: NOT DISCRETE.
     I := P'POS(OP);                         -- ERROR: NOT DISCRETE.
     I := L'POS(OL);                         -- ERROR: NOT DISCRETE.

END B35501A;
