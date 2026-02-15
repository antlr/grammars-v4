-- B55B01B.ADA

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
-- CHECK THAT A LOOP_PARAMETER CANNOT BE USED AS A GENERIC IN OUT
-- PARAMETER.

-- SPS 3/2/83

PROCEDURE B55B01B IS

     I1 : INTEGER := 0;

     GENERIC
     PROCEDURE P ( P1 : OUT INTEGER; P2 : IN OUT INTEGER ) ;

     GENERIC
          PR : IN OUT INTEGER;
     PACKAGE GP1 IS END GP1;

     GENERIC
          PR : IN INTEGER;
     PACKAGE GP2 IS END GP2;

     PROCEDURE P ( P1 : OUT INTEGER; P2 : IN OUT INTEGER ) IS
     BEGIN
          NULL;
     END P;

BEGIN

     FOR I IN 1..10 LOOP

          DECLARE
               PACKAGE PKG1 IS NEW GP1 (I);       -- ERROR: PARAMETER
                                                  -- USED AS AN IN OUT
                                                  -- GENERIC PARAMETER.
               PACKAGE PKG2 IS NEW GP2 (I);       -- OK.
               PROCEDURE PROC IS NEW P;
          BEGIN
               PROC (I1, I);                      -- ERROR: PARAMETER
                                                  -- USED AS AN IN OUT
                                                  -- PARAMETER.
               PROC (I, I1);                      -- ERROR: PARAMETER
                                                  -- USED AS AN OUT
                                                  -- PARAMETER.
          END;
     END LOOP;

END B55B01B;
