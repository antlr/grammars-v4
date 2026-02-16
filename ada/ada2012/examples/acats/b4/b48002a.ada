-- B48002A.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T X", WHERE X IS:
--   A) A DISCRIMINANT CONSTRAINT OR A VALUE OF TYPE T ENCLOSED IN
--      PARENTHESES, CHECK THAT T CANNOT BE:
--        A SCALAR TYPE,
--        A CONSTRAINED RECORD TYPE,
--        A CONSTRAINED PRIVATE TYPE,
--        A CONSTRAINED LIMITED TYPE, OR
--        AN ARRAY TYPE (CONSTRAINED OR UNCONSTRAINED).
--   B) AN INDEX CONSTRAINT, CHECK THAT T CANNOT BE A SCALAR, RECORD,
--      PRIVATE, OR LIMITED PRIVATE TYPE, NOR CAN T BE A CONSTRAINED
--      ARRAY TYPE.

-- EG  07/19/84
-- RLB 11/19/19  Added error location indicators.

PROCEDURE B48002A IS

BEGIN

     -- SCALAR TYPE

     DECLARE

          TYPE A_INT IS ACCESS INTEGER;

          VINT0, VINT1, VINT2, VINT3, VINT4, VINT5, VINT6 : A_INT;

     BEGIN

          VINT0 := NEW INTEGER;         -- OK.
          VINT1 := NEW INTEGER(1);      -- ERROR: NOT T'(EXP).         {11}
          VINT2 := NEW INTEGER((2));    -- ERROR: NOT T'(EXP).         {11}
          VINT3 := NEW INTEGER(1, 2);   -- ERROR: DISCRIMINANT         {11}
                                        --        CONSTRAINT CANNOT BE
                                        --        APPLIED TO THIS TYPE.
          VINT4 := NEW INTEGER(1 .. 3); -- ERROR: INDEX CONSTRAINT     {11}
                                        --        CANNOT BE APPLIED
                                        --        TO THIS TYPE.
          VINT5 := NEW INTEGER(1 => 2, 2 => 1);   -- ERROR: NOT T'(AGG) {11}
          VINT6 := NEW INTEGER(OTHERS => 0); -- ERROR: NOT T'(AGG)     {11}

     END;

     -- UNCONSTRAINED RECORD TYPE (WITH AT LEAST ONE DISCRIMINANT)

     DECLARE

          TYPE INT IS RANGE 1 .. 4;
          TYPE INT2 IS (A1, A2, A3, A4);
          TYPE BOOLARRAY IS ARRAY(INT RANGE <>) OF BOOLEAN;
          TYPE UR(D1 : INTEGER; D2 : INTEGER) IS
               RECORD
                    C : INTEGER;
               END RECORD;
          TYPE A_UR IS ACCESS UR;

          VUR0, VUR1, VUR2, VUR3, VUR4, VUR5, VUR6, VUR7, VUR8 : A_UR;

     BEGIN

          VUR0 := NEW UR;               -- ERROR: NEED DEFAULT         {11}
                                        --        VALUE.
          VUR1 := NEW UR(1);            -- ERROR: MISSING CONSTRAINT.  {11}
          VUR2 := NEW UR((2));          -- ERROR: MISSING CONSTRAINT.  {11}
          VUR3 := NEW UR(1, 2);         -- OK.                         {11}
          VUR4 := NEW UR(A1, A3);       -- ERROR: NOT VALID TYPE       {11}
                                        --        FOR DISCRIMINANT.
          VUR5 := NEW UR(1 .. 3);       -- ERROR: INDEX CONSTRAINT     {11}
                                        --        CANNOT BE APPLIED TO
                                        --        THIS TYPE.
          VUR6 := NEW UR(1, 2, 3);      -- ERROR: NOT T'(AGG).         {11}
          VUR7 := NEW UR((1, 2, 3));    -- ERROR: NOT T'(AGG).         {11}
          VUR8 := NEW UR(D1 | D2 => 2,
                         C => 3);       -- ERROR: NOT T'(AGG).         {1:11}

     END;

     -- CONSTRAINED RECORD TYPE (WITH OR WITHOUT DISCRIMINANTS)

     DECLARE

          TYPE UR(D1, D2 : INTEGER) IS
               RECORD
                    C : INTEGER := 7;
               END RECORD;
          SUBTYPE CR IS UR(2,3);
          TYPE A_CR IS ACCESS CR;

          VCR0, VCR1, VCR2, VCR3, VCR4, VCR5, VCR6 : A_CR;

     BEGIN

          VCR0 := NEW CR;               -- OK.
          VCR1 := NEW CR(1);            -- ERROR: ALREADY CONSTRAINED. {11}
          VCR2 := NEW CR((2));          -- ERROR: ALREADY CONSTRAINED. {11}
          VCR3 := NEW CR(1, 2);         -- ERROR: CANNOT BE FURTHER    {11}
                                        --        CONSTRAINED.
          VCR4 := NEW CR(1 .. 3);       -- ERROR: ILLEGAL INDEX        {11}
                                        --        CONSTRAINT.
          VCR5 := NEW CR(D1 => 2, D2 => 1,
                         C => 3);       -- ERROR: NOT T'(AGG).         {1:11}
          VCR6 := NEW CR(1, 2, 3);      -- ERROR: NOT T'(AGG).         {11}

     END;

     -- UNCONSTRAINED PRIVATE TYPE (WITH AT LEAST ONE DISCRIMINANT)

     DECLARE

          PACKAGE P IS

               TYPE UP(D1, D2 : INTEGER) IS PRIVATE;

          PRIVATE

               TYPE UP(D1, D2 : INTEGER) IS
                    RECORD
                         C : INTEGER;
                    END RECORD;

          END P;

          USE P;

          TYPE A_UP IS ACCESS P.UP;

          VUP0, VUP1, VUP2, VUP3, VUP4, VUP5, VUP6 : A_UP;

     BEGIN

          VUP0 := NEW P.UP;             -- ERROR: DEFAULT VALUE.       {11}
          VUP1 := NEW P.UP(1);          -- ERROR: MISSING CONSTRAINT.  {11}
          VUP2 := NEW P.UP((2));        -- ERROR: MISSING CONSTRAINT.  {11}
          VUP3 := NEW P.UP(1, 2);       -- OK.                         {11}
          VUP4 := NEW P.UP(1 .. 3);     -- ERROR: INDEX CONSTRAINT     {11}
                                        --        CANNOT BE APPLIED TO
                                        --        THIS TYPE.
          VUP5 := NEW P.UP(D1 => 2, D2 => 1,
                           C => 3);     -- ERROR: NOT T'(AGG).         {1:11}
          VUP6 := NEW P.UP(1, 2, 3);    -- ERROR: NOT T'(AGG).         {11}

     END;

     -- CONSTRAINED PRIVATE TYPE (WITH OR WITHOUT DISCRIMINANTS)

     DECLARE

          PACKAGE P IS

               TYPE UP(D1, D2 : INTEGER) IS PRIVATE;

          PRIVATE

               TYPE UP(D1, D2 : INTEGER) IS
                    RECORD
                         C : INTEGER;
                    END RECORD;

          END P;

          USE P;

          SUBTYPE CP IS P.UP(12, 13);
          TYPE A_CP IS ACCESS CP;

          VCP0, VCP1, VCP2, VCP3, VCP4, VCP5, VCP6 : A_CP;

     BEGIN

          VCP0 := NEW CP;               -- OK.
          VCP1 := NEW CP(1);            -- ERROR: ALREADY CONSTRAINED. {11}
          VCP2 := NEW CP((2));          -- ERROR: ALREADY CONSTRAINED. {11}
          VCP3 := NEW CP(1, 2);         -- ERROR: CANNOT BE FURTHER    {11}
                                        --        CONSTRAINED.
          VCP4 := NEW CP(1 .. 3);       -- ERROR: ILLEGAL INDEX        {11}
                                        --        CONSTRAINT.
          VCP5 := NEW CP(D1 => 2, D2 => 1,
                         C => 3);       -- ERROR: NOT T'(AGG).         {1:11}
          VCP6 := NEW CP(1, 2, 3);      -- ERROR: NOT T'(AGG).         {11}

     END;

     -- UNCONSTRAINED LIMITED TYPE (WITH AT LEAST ONE DISCRIMINANT)

     DECLARE

          PACKAGE LP IS

               TYPE UL(D1, D2 : INTEGER) IS LIMITED PRIVATE;

          PRIVATE

               TYPE UL(D1, D2 : INTEGER) IS
                    RECORD
                         C : INTEGER;
                    END RECORD;

          END LP;

          USE LP;

          TYPE A_UL IS ACCESS UL;

          VUL0, VUL1, VUL2, VUL3, VUL4, VUL5, VUL6 : A_UL;

     BEGIN

          VUL0 := NEW UL;               -- ERROR: DEFAULT VALUE.       {11}
          VUL1 := NEW UL(1);            -- ERROR: MISSING CONSTRAINT.  {11}
          VUL2 := NEW UL((2));          -- ERROR: MISSING CONSTRAINT.  {11}
          VUL3 := NEW UL(1, 2);         -- OK.                         {11}
          VUL4 := NEW UL(1 .. 3);       -- ERROR: INDEX CONSTRAINT     {11}
                                        --        CANNOT BE APPLIED TO
                                        --        THIS TYPE.
          VUL5 := NEW UL(D1 => 2, D2 => 1,
                         C => 3);       -- ERROR: NOT T'(AGG).         {1:11}
          VUL6 := NEW UL(1, 2, 3);      -- ERROR: NOT T'(AGG).         {11}

     END;

     -- CONSTRAINED LIMITED TYPE (WITH OR WITHOUT DISCRIMINANTS)

     DECLARE

          PACKAGE LP IS

               TYPE UL(D1, D2 : INTEGER) IS LIMITED PRIVATE;

          PRIVATE

               TYPE UL(D1, D2 : INTEGER) IS
                    RECORD
                         C : INTEGER;
                    END RECORD;

          END LP;

          USE LP;

          SUBTYPE CL IS UL(1, 1);

          TYPE A_CL IS ACCESS CL;

          VCL0, VCL1, VCL2, VCL3, VCL4, VCL5, VCL6 : A_CL;

     BEGIN

          VCL0 := NEW CL;               -- OK.
          VCL1 := NEW CL(1);            -- ERROR: ALREADY CONSTRAINED. {11}
          VCL2 := NEW CL((2));          -- ERROR: ALREADY CONSTRAINED. {11}
          VCL3 := NEW CL(1, 2);         -- ERROR: CANNOT BE FURTHER    {11}
                                        --        CONSTRAINED.
          VCL4 := NEW CL(1 .. 3);       -- ERROR: ILLEGAL INDEX        {11}
                                        --        CONSTRAINT.
          VCL5 := NEW CL(D1 => 2, D2 => 1,
                         C => 3);       -- ERROR: NOT T'(AGG).         {1:11}
          VCL6 := NEW CL(OTHERS => 0);  -- ERROR: NOT T'(AGG).         {11}

     END;

     -- UNCONSTRAINED ARRAY TYPE (COMPONENT TYPE IS NOT A LIMITED TYPE)

     DECLARE

          SUBTYPE INT IS INTEGER RANGE 1 .. 4;
          TYPE UA IS ARRAY(INT RANGE <>) OF INTEGER;
          TYPE A_UA IS ACCESS UA;

          VUA0, VUA1, VUA2, VUA3, VUA4, VUA5, VUA6 : A_UA;

     BEGIN

          VUA0 := NEW UA;               -- ERROR: CANNOT BE            {11}
                                        --        ALLOCATED.
          VUA1 := NEW UA(1);            -- ERROR: INVALID CONSTRAINT.  {11}
          VUA2 := NEW UA((2));          -- ERROR: INVALID CONSTRAINT.  {11}
          VUA3 := NEW UA(1, 2);         -- ERROR: DISCRIMINANT         {11}
                                        --        CONSTRAINT CANNOT BE
                                        --        APPLIED TO THIS TYPE.
          VUA4 := NEW UA(1 .. 3);       -- OK.                         {11}
          VUA5 := NEW UA(1 => 2, 2 => 1);    -- ERROR: NOT T'(AGG).    {11}
          VUA6 := NEW UA(1, 2, 3);      -- ERROR: NOT T'(AGG).         {11}

     END;

     -- CONSTRAINED ARRAY TYPE (COMPONENT TYPE IS NOT A LIMITED TYPE)

     DECLARE

          TYPE UA IS ARRAY(INTEGER RANGE <>) OF INTEGER;
          SUBTYPE CA IS UA(1 .. 4);
          TYPE A_CA IS ACCESS CA;

          VCA0, VCA1, VCA2, VCA3, VCA4, VCA5, VCA6 : A_CA;

     BEGIN

          VCA0 := NEW CA;               -- OK.                         {11}
          VCA1 := NEW CA(1);            -- ERROR: INVALID CONSTRAINT.  {11}
          VCA2 := NEW CA((2));          -- ERROR: INVALID CONSTRAINT.  {11}
          VCA3 := NEW CA(1, 2);         -- ERROR: ILLEGAL              {11}
                                        --        DISCRIMINANT FOR TYPE.
          VCA4 := NEW CA(1 .. 3);       -- ERROR: CANNOT BE FURTHER    {11}
                                        --        CONSTRAINED.
          VCA5 := NEW CA(1 => 2, 2 => 1);    -- ERROR: NOT T'(AGG).    {11}
          VCA6 := NEW CA(1, 2, 3);      -- ERROR: NOT T'(AGG).         {11}

     END;

END B48002A;
