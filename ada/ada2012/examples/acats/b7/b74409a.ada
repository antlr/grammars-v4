 -- B74409A.ADA
 
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
 -- CHECK THAT IF A COMPOSITE TYPE IS DECLARED IN THE SAME PACKAGE 
 -- AS A LIMITED PRIVATE TYPE AND HAS A COMPONENT OF THAT TYPE, 
 -- THE COMPOSITE TYPE IS TREATED AS A LIMITED TYPE UNTIL THE 
 -- EARLIEST PLACE WITHIN THE IMMEDIATE SCOPE OF THE DECLARATION 
 -- OF THE COMPOSITE TYPE AND AFTER THE FULL DECLARATION OF THE 
 -- LIMITED PRIVATE TYPE.
 
 -- DSJ 5/5/83
 -- JRK 10/26/83
 -- KAS 11/16/95 ALLOW "=" ON NON-LIMITED TYPE
 -- KAS 11/29/95 REMOVE ADA95 CASE FOR 2.0.1
 -- PWN 03/28/96 Restored Ada95 case for 2.1
 
 PROCEDURE B74409A IS
 
      PACKAGE P IS
 
           TYPE LP IS LIMITED PRIVATE;
           TYPE NP IS PRIVATE;
 
           PACKAGE Q IS
                TYPE LP_ARRAY IS ARRAY (1 .. 2) OF LP;
           END Q;
 
      PRIVATE
           TYPE LP IS NEW INTEGER;
           TYPE NP IS NEW Q.LP_ARRAY;    -- ERROR: LP_ARRAY IS LIMITED.
      END P;
 
      PACKAGE BODY P IS
           USE Q;
 
           FUNCTION "=" (L, R : LP_ARRAY) RETURN BOOLEAN IS  -- LEGAL.
           BEGIN
                RETURN TRUE;
           END;
 
           GENERIC
                TYPE T IS PRIVATE;          -- NOTE: NOT LIMITED PRIVATE.
           PACKAGE A IS
                -- IRRELEVANT DETAILS.
           END A;
 
           PACKAGE NEW_A IS NEW A (LP_ARRAY);      -- ERROR: LP_ARRAY IS
                                                   --        LIMITED.
 
           PACKAGE BODY Q IS
                FUNCTION "=" (L, R : LP_ARRAY)    -- NOTE: OK IN ADA95
                             RETURN BOOLEAN IS
                BEGIN
                     RETURN TRUE;
                END "=";
 
                PACKAGE ANOTHER_NEW_A IS NEW A (LP_ARRAY);   -- LEGAL.
           END Q;
      END P;
 
 BEGIN
 
      NULL;
 
 END B74409A;
 
