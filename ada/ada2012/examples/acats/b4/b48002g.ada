-- B48002G.ADA

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
-- CHECK THAT ILLEGAL FORMS OF ALLOCATORS ARE FORBIDDEN. IN PARTICULAR,
-- FOR ALLOCATORS OF THE FORM "NEW T X", WHERE X IS AN INDEX CONSTRAINT,
-- CHECK THAT IF T IS AN UNCONSTRAINED ARRAY TYPE OR AN UNCONSTRAINED
-- ACCESS TYPE WHOSE DESIGNATED TYPE IS AN UNCONSTRAINED ARRAY TYPE,
-- THEN X MUST BE A LEGAL INDEX CONSTRAINT FOR T, NAMELY:
--   A) THE NUMBER OF DISCRETE RANGES IN THE INDEX CONSTRAINT CANNOT BE
--      LESS THAN OR GREATER THAN THE NUMBER OF INDEXES IN THE ARRAY
--      TYPE BEING CONSTRAINED.
--   B) THE BASE TYPE OF EACH DISCRETE RANGE IN X CANNOT BE DIFFERENT
--      FROM THE BASE TYPE OF THE CORRESPONDING INDEX.

-- EG  08/02/84
-- RLB 11/19/19  Added error location indicators.

PROCEDURE B48002G IS

     TYPE INT_TYPE1 IS (A, B, C, D, E);
     TYPE INT_TYPE2 IS (A, B, C, D, E, F);
     TYPE UA IS ARRAY(INT_TYPE1 RANGE <>, INT_TYPE1 RANGE <>)
                     OF INTEGER;

     TYPE A_UA  IS ACCESS UA;
     TYPE AA_UA IS ACCESS A_UA;

     V_A_UA1, V_A_UA2, V_A_UA3, V_A_UA4, V_A_UA5  : A_UA;
     V_AA_UA1, V_AA_UA2, V_AA_UA3, V_AA_UA4, V_AA_UA5 : AA_UA;

BEGIN

     V_A_UA1 := NEW UA;                      -- ERROR: TOO FEW INDEXES. {17}
     V_A_UA2 := NEW UA(A .. C);              -- ERROR: TOO FEW INDEXES. {17}
     V_A_UA3 := NEW UA(A .. C, B .. F);      -- ERROR: BASE TYPE.       {17}
     V_A_UA4 := NEW UA(A .. D, B .. F);      -- ERROR: BASE TYPE.       {17}
     V_A_UA5 := NEW UA(A .. B,
                       A .. B, A .. B);      -- ERROR: TOO MANY INDEXES.{1:17}

     V_AA_UA1 := NEW A_UA;                   -- OK.                     {18}
     V_AA_UA2 := NEW A_UA(A .. C);           -- ERROR: TOO FEW INDEXES. {18}
     V_AA_UA3 := NEW A_UA(A .. C, B .. F);   -- ERROR: BASE TYPE.       {18}
     V_AA_UA4 := NEW A_UA(A .. F, A .. B);   -- ERROR: BASE TYPE.       {18}
     V_AA_UA5 := NEW A_UA(A .. B,
                          A .. B, A .. B);   -- ERROR: TOO MANY INDEXES.{1:18}

END B48002G;
