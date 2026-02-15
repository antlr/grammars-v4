-- B37311A.ADA

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
-- CHECK THAT EVEN WHEN THE CONTEXT INDICATES THAT A DISCRIMINANT
-- COVERS A SMALLER RANGE OF VALUES THAN PERMITTED BY ITS SUBTYPE,
-- AN OTHERS ALTERNATIVE IS REQUIRED IF THE SUBTYPE VALUE RANGE
-- IS NOT FULLY COVERED.

--     ASL 07/14/81
--     SPS 12/07/82
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

PROCEDURE  B37311A IS

     SUBTYPE INT IS INTEGER RANGE 1..10;
     TYPE REC (DISC : INT) IS
          RECORD
               CASE DISC IS
                    WHEN 1 =>
                         CASE DISC IS
                              WHEN 1 => NULL;
                         END CASE;         -- ERROR: {2:16;1} MISSING CHOICES.
                    WHEN 2..4 => NULL;
                    WHEN 5..10 =>
                         CASE DISC IS
                              WHEN 5..10 => NULL;
                         END CASE;         -- ERROR: {2:16;1} MISSING CHOICES.
               END CASE;
          END RECORD;

BEGIN
     NULL;
END B37311A;
