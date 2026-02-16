-- B32201A.ADA

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
-- (A) CHECK THAT THE FOLLOWING ATTRIBUTES CANNOT APPEAR IN A NUMBER
--    DECLARATION:
--
--    (A1) BECAUSE THEY ARE NOT OF TYPE UNIVERSAL INTEGER:
--
--        'ADDRESS       'FIRST         'VALUE         'PRED
--        'MACHINE_OVERFLOWS            'CONSTRAINED   'BASE
--        'LAST          'SUCC          'IMAGE         'VAL
--        'MACHING_ROUNDS               'RANGE         'TERMINATED
--
--    (A2) BECAUSE THEY ARE NOT STATIC:
--
--        'LAST_BIT      'FIRST_BIT     'COUNT         'LENGTH
--        'POSITION      'STORAGE_SIZE
--
--    (A4) BECAUSE IT IS NOT STATIC IF THE ARG IS NOT STATIC:
--
--        'POS
--
-- (B) CHECK THAT A USER-DEFINED FUNCTION, A USER-DEFINED OPERATOR, OR
--    THE OPERATOR  '&'  CANNOT APPEAR IN A NUMBER DECLARATION.
--
-- (C) CHECK THAT A STRING LITERAL OR CHARACTER LITERAL CANNOT APPEAR
--    IN A NUMBER DECLARATION.


-- RM  03/02/81
-- VKG 01/05/83
-- SPS 2/4/83
-- DTN 11/30/95  REMOVED CONFORMANCE CHECKS WHERE RULES RELAXED.
-- PWN 04/11/96  Restored checks in Ada95 legal format.
-- RLB 11/19/19  Added error location indicators.

WITH SYSTEM; USE SYSTEM;
PROCEDURE  B32201A  IS
BEGIN


     -------------------------------------------------------------------
     --------------------  ATTRIBUTES ON THE LIST  ---------------------

     DECLARE

          MY_ADDRESS  : CONSTANT :=  B32201A'ADDRESS;       -- ERROR: A1  {11}
          A02         : CONSTANT :=  INTEGER'BASE'FIRST;    -- OK.        {11}
          A03         : CONSTANT :=  INTEGER'SIZE;          -- OK.        {11}
          A04         : CONSTANT :=  INTEGER'FIRST;         -- OK.        {11}
          A05         : CONSTANT :=  INTEGER'LAST;          -- OK.        {11}
          TYPE  ARRAY1   IS  ARRAY( 1..3 ) OF INTEGER;
          A06         : CONSTANT :=  ARRAY1'FIRST;          -- OK.        {11}
          A07         : CONSTANT :=  ARRAY1'LAST;           -- OK.        {11}
          A101        : CONSTANT :=  INTEGER'POS(17);       -- OK.        {11}
          A102        : CONSTANT :=  CHARACTER'POS('B');    -- OK.        {11}
          A11         : CONSTANT :=  INTEGER'VAL(17);       -- OK.        {11}
          A12         : CONSTANT :=  INTEGER'PRED(17);      -- OK.        {11}
          A13         : CONSTANT :=  INTEGER'SUCC(17);      -- OK.        {11}
          A14         : CONSTANT :=  BOOLEAN'POS(
                                 FLOAT'MACHINE_ROUNDS );    -- OK.        {11}
          A15         : CONSTANT :=  BOOLEAN'POS(
                                 FLOAT'MACHINE_OVERFLOWS);  -- OK.        {11}
          A16         : CONSTANT :=  ARRAY1'LENGTH;         -- OK.        {11}
          A17         : CONSTANT :=  ARRAY1'RANGE'SIZE;     -- ERROR:     {11}
                                           -- ARRAY1'RANGE NOT A SUBTYPE
          TYPE  R ( A :  INTEGER ) IS  RECORD NULL; END RECORD;
          R1          :  R(3);
          A181        : CONSTANT :=  BOOLEAN'POS(
                                 R1'CONSTRAINED );          -- ERROR: A4 {1:11}
          A182        : CONSTANT :=  R1'CONSTRAINED;        -- ERROR: A1  {11}
          A19         : CONSTANT :=  R1.A'POSITION;         -- ERROR: A2  {11}
          A20         : CONSTANT :=  R1.A'FIRST_BIT;        -- ERROR: A2  {11}
          A21         : CONSTANT :=  R1.A'LAST_BIT;         -- ERROR: A2  {11}
          TYPE  ACC1  IS ACCESS INTEGER;
          A22         : CONSTANT :=  ACC1'STORAGE_SIZE;     -- ERROR: A2  {11}

          TASK  TSK1  IS
               ENTRY  E1;
          END TSK1;

          A23         : CONSTANT :=  TSK1'STORAGE_SIZE;     -- ERROR: A2  {11}
          A24         : CONSTANT :=  BOOLEAN'POS(
                                 TSK1'TERMINATED );         -- ERROR: A4 {1:11}

          SUBTYPE L IS INTEGER RANGE
               BOOLEAN'POS (R1'CONSTRAINED) .. 5;           -- NONSTATIC

          A26         : CONSTANT := L'POS (0);              -- ERROR: A3  {11}
          A27         : CONSTANT := L'SIZE;                 -- ERROR: A3  {11}

          TASK BODY  TSK1  IS
               A25    : CONSTANT :=  E1'COUNT;              -- ERROR: A2  {16}
          BEGIN
               ACCEPT  E1;
          END TSK1;

     BEGIN

          NULL;

     END;


     -------------------------------------------------------------------
     ------------------------  OPERATORS  ------------------------------

     DECLARE

          TYPE  ARR  IS  ARRAY(1..2) OF INTEGER;
          SUBTYPE LARGEINT IS INTEGER RANGE 0..INTEGER'LAST;
          NULL_SLICE  :  ARR  := ( 7 , 11 );       -- TO BE SLICED LATER

          FUNCTION  RECURSIVE_MINUS
                         ( P , Q : LARGEINT )
                    RETURN  LARGEINT IS
          BEGIN
               IF  P >= Q  THEN
                    RETURN  STANDARD."-"( P , Q );
               ELSE
                    RETURN  0;
               END IF;
          END;

          FUNCTION  "-"
                         ( P , Q : LARGEINT )
                    RETURN  LARGEINT IS
          BEGIN
               IF  P >= Q  THEN
                    RETURN  STANDARD."-"( P , Q );
               ELSE
                    RETURN  0;
               END IF;
          END;

     BEGIN

          DECLARE

               B1   : CONSTANT :=  RECURSIVE_MINUS(21,23);  -- ERROR: B   {16}
               B2   : CONSTANT :=  7 + 5 - 19;              -- OK.        {16}
               B3   : CONSTANT :=  17 & NULL_SLICE(1..0);   -- ERROR: B   {16}

          BEGIN
               NULL;
          END;

     END;


     -------------------------------------------------------------------
     ------------  STRING LITERALS, CHARACTER LITERALS  ----------------

     DECLARE

          C1          : CONSTANT :=  "ABRACADABRA";         -- ERROR: C   {11}
          C2          : CONSTANT :=  'A';                   -- ERROR: C   {11}
          C3          : CONSTANT :=  ASCII.DEL;             -- ERROR: C   {11}

     BEGIN

          NULL;

     END;

     -------------------------------------------------------------------


END B32201A;
