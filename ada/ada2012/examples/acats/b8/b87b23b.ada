-- B87B23B.ADA

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
-- CHECK THAT ON OVERLOAD PREFIX OR INDEX EXPRESSION CAN BE RESOLVED
-- BECAUSE:
--   A) THE PREFIX OF AN INDEXED COMPONENT MUST EITHER DENOTE AN
--      ENTRY FAMILY OR ITS TYPE MUST BE AN ARRAY TYPE.
--   B) FOR ENTRY FAMILIES, THERE MUST BE A SINGLE EXPRESSION
--      HAVING THE TYPE OF THE FAMILY INDEX.
--   C) FOR ARRAY TYPES, THERE MUST BE AS MANY EXPRESSIONS AS ARRAY
--      DIMENSIONS, AND EACH EXPRESSION MUST BE OF THE CORRESPONDING
--      INDEX TYPE.

-- CASE OF CALLING VS INDEXING WITH DERIVATION MIXED IN.

-- IN THE FOLLOWING COMMENTS, LET:
--    [] DENOTE INDEXING, AND
--    () DENOTE CALL.

-- RFB 05/12/84
-- EG  05/30/84
-- JRK 7/23/84

PROCEDURE B87B23B IS

     PACKAGE PACK IS

          TYPE MY_CHARA IS ('A', 'B', 'C', 'D', 'E');
          TYPE MY_STRA  IS ARRAY (POSITIVE RANGE <>) OF MY_CHARA;
                                        -- S_A[I] -> C_A.

          FUNCTION FA RETURN MY_CHARA;  -- FA#1() -> C_A.

          FUNCTION FB (C : MY_CHARA := 'A') RETURN MY_STRA; 
                                        -- FB#1(C_A) -> S_A.
          FUNCTION FB (I : INTEGER := 1)    RETURN MY_STRA; 
                                        -- FB#2(I) -> S_A.

     END PACK;

     USE PACK;

     TYPE MY_CHARB IS NEW MY_CHARA;     -- FA#2() -> C_B,
                                        -- FB#3(C_B) -> S_A.
     TYPE MY_STRB  IS NEW MY_STRA;      -- FB#4(C_A) -> S_B,
                                        -- FB#5(I) -> S_B,
                                        -- S_B[I] -> C_A.

     CA : MY_CHARA;
     SA : MY_STRA(1 .. 10);

     CB : MY_CHARB;
     SB : MY_STRB(1 .. 10);

     PACKAGE BODY PACK IS

          FUNCTION FA RETURN MY_CHARA IS -- FA#1() -> C_A,
                                         -- FA#2() -> C_B.
          BEGIN
               RETURN 'A';
          END;

          FUNCTION FB (C : MY_CHARA := 'A') RETURN MY_STRA IS 
                                        -- FB#1(C_A) -> S_A,
                                        -- FB#3(C_B) -> S_A,
                                        -- FB#4(C_A) -> S_B.
          BEGIN
               RETURN "ABC"; 
          END;

          FUNCTION FB (I : INTEGER := 1) RETURN MY_STRA IS    
                                        -- FB#2(I) -> S_A,
                                        -- FB#5(I) -> S_B.
          BEGIN 
               RETURN "DE";
          END;            

     END;

BEGIN

     CA := FA;                          -- FA#1().
     SA := FB(CA);                      -- FB#1(CA).

     SA := FB(MY_CHARA'(FA));           -- FB#1(FA#1()).
     SA := FB(MY_CHARB'(FA));           -- FB#3(FA#2()).

     SA := FB(2);                       -- FB#2(2).

     CB := FA;                          -- FA#2().
     SB := FB(FA);                      -- FB#4(FA#1()).
     SB := FB(2);                       -- FB#5(2).

     SB := FB(FB(MY_CHARB'('B'))(3));   -- FB#4(FB#3('B')[3]).

     SA := FB;                          -- ERROR: AMBIGUOUS:
                                        --        FB#1(),
                                        --        FB#2(),
                                        --        FB#3().
     SB := FB;                          -- ERROR: AMBIGUOUS:
                                        --        FB#4(),
                                        --        FB#5().

     SA := FB(FB(3));                   -- ERROR: AMBIGUOUS:
                                        --        FB#1(FB#1()[3]),
                                        --        FB#1(FB#2()[3]),
                                        --        FB#1(FB#3()[3]),
                                        --        FB#1(FB#4()[3]),
                                        --        FB#1(FB#5()[3]).
     SB := FB(FB(3));                   -- ERROR: AMBIGUOUS:
                                        --        FB#4(FB#1()[3]),
                                        --        FB#4(FB#2()[3]),
                                        --        FB#4(FB#3()[3]),
                                        --        FB#4(FB#4()[3]),
                                        --        FB#4(FB#5()[3]).

     SB := FB(CB);                      -- ERROR: UNRESOLVABLE.

     CA := FB(FA)(1);                   -- ERROR: AMBIGUOUS:
                                        --        FB#1(FA#1())[1],
                                        --        FB#3(FA#2())[1],
                                        --        FB#4(FA#1())[1].
     CB := FB(FA)(1);                   -- ERROR: UNRESOLVABLE.

     SA := FB(FB(MY_CHARA'('B'))(3));   -- ERROR: AMBIGUOUS:
                                        --        FB#1(FB#1('B')[3]),
                                        --        FB#1(FB#4('B')[3]).
     SB := FB(FB(MY_CHARA'('B'))(3));   -- ERROR: AMBIGUOUS:
                                        --        FB#4(FB#1('B')[3]),
                                        --        FB#4(FB#4('B')[3]).

END B87B23B;
