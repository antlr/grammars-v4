-- B63103A.ADA

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
-- CHECK THAT UNIVERSAL REAL LITERALS IN DEFAULT EXPRESSIONS MUST HAVE
-- THE SAME VALUE IN CONFORMING FORMAL PARTS. 

-- BHS 7/16/84

PROCEDURE B63103A IS

     TYPE REAL IS NEW FLOAT;

     PACKAGE PACK1 IS
          PROCEDURE P1 (X : REAL := 0.333_333_333_333_333_333_33);
     PRIVATE
          PROCEDURE P2 (Y : REAL := 3#0.1#);
     END PACK1;

     PACKAGE BODY PACK1 IS                                       
          PROCEDURE P1 (X : REAL := 3#0.1#) IS     -- ERROR: 0.333...
                                                   -- AND 3#0.1# DO NOT
                                                   -- CONFORM. 
          BEGIN  
               NULL;  
          END P1;

          PROCEDURE P2 (Y : REAL
                        := 16#0.555_555_555_555_555_555#) IS  -- ERROR: 
                                                    -- 3#0.1# AND
                                                    -- 16#0.555_555...#
                                                    -- DO NOT CONFORM.
          BEGIN
               NULL;
          END P2;
     END PACK1;


     GENERIC
     PROCEDURE P3 (Z : REAL := 16#0.555_555_555_555_555_555#);

     PROCEDURE P3 (Z : REAL 
                   := 0.333_333_333_333_333_333_33) IS  -- ERROR: 
                                                   -- 16#0.555_555...#
                                                   -- AND 0.333... DO
                                                   -- NOT CONFORM.
     BEGIN
          NULL;
     END P3;


     GENERIC
     PROCEDURE P4 (ZZ : REAL := 0.333_333_333_333_333_333_33);

     PROCEDURE P4 (ZZ : REAL := 3#0.1#) IS      -- ERROR: 0.333... AND
                                                -- 3#0.1# DO NOT
                                                -- CONFORM. 
     BEGIN
          NULL;
     END P4;


     PACKAGE OK_PACK IS
          PROCEDURE P5 (OK : REAL := 0.75);
     END OK_PACK;

     PACKAGE BODY OK_PACK IS
          PROCEDURE P5 (OK : REAL := 2#0.11#) IS    -- OK.
          BEGIN
               NULL;
          END P5;
     END OK_PACK;

BEGIN
     NULL;
END B63103A;
