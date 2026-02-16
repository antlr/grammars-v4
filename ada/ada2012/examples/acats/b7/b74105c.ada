-- B74105C.ADA

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
-- CHECK THAT THE FULL TYPE DECLARATION OF A PRIVATE TYPE WITHOUT
-- DISCRIMINANTS MAY BE A CONSTRAINED TYPE WITH DISCRIMINANTS

-- PART 2 : CHECK THAT THE DISCRIMINANTS ARE NOT VISIBLE OUTSIDE THE
-- PACKAGE THAT DECLARES THE PRIVATE TYPE.

-- DSJ 5/2/83

PROCEDURE B74105C IS

     TYPE REC1 (D : INTEGER) IS
          RECORD
               C1, C2 : INTEGER ;
          END RECORD ;

     TYPE REC2 (F : INTEGER := 0) IS
          RECORD
               E1, E2 : INTEGER ;
          END RECORD ;

     TYPE REC3 IS NEW REC1 (D => 1) ;

     TYPE REC4 IS NEW REC2 (F => 2) ;

     PACKAGE PACK1 IS
          TYPE P1 IS PRIVATE ;
          TYPE P2 IS PRIVATE ;
          TYPE P3 IS PRIVATE ;
          TYPE P4 IS PRIVATE ;
     PRIVATE
          TYPE P1 IS NEW REC3 ;              -- LEGAL
          TYPE P2 IS NEW REC4 ;              -- LEGAL
          TYPE P3 IS NEW REC1 (D => 5) ;     -- LEGAL
          TYPE P4 IS NEW REC2 (F => 7) ;     -- LEGAL
     END PACK1 ;

     USE PACK1 ;

     X1 : P1 ; 
     X2 : P2 ; 
     X3 : P3 ; 
     X4 : P4 ; 

     N1 : INTEGER := X1.D'SIZE ;        -- ERROR: X1.D NOT VISIBLE
     N2 : INTEGER := X2.F'SIZE ;        -- ERROR: X2.F NOT VISIBLE
     N3 : INTEGER := X3.D'SIZE ;        -- ERROR: X3.D NOT VISIBLE
     N4 : INTEGER := X4.F'SIZE ;        -- ERROR: X4.F NOT VISIBLE

BEGIN

     NULL ; 

END B74105C ;
