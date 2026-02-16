-- B37309B.ADA

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
-- OBJECTIVE:
--     CHECK THAT IF A DISCRIMINANT HAS A STATIC SUBTYPE, AN OTHERS
--     CHOICE MUST NOT BE OMITTED IF ONE OR MORE VALUES IN THE
--     SUBTYPE'S RANGE ARE MISSING.
--     CHECK THAT VALUES OUTSIDE THE RANGE OF THE SUBTYPE ARE FORBIDDEN.

-- HISTORY:
--     ASL 07/10/81
--     SPS 12/07/82
--     RJW 01/21/86 RENAMED FROM -AB.
--                  ADDED TEST FOR CHOICE OUTSIDE OF RANGE.
--     DHH 08/15/88 REVISED HEADER AND REMOVED DYNAMIC SUBTYPES.
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

PROCEDURE B37309B IS

     SUBTYPE STATCHAR IS CHARACTER RANGE 'I'..'N';
     SUBTYPE SSTAT IS STATCHAR RANGE 'I'..'M';
     TYPE NEWSTAT IS NEW CHARACTER RANGE 'A'..'D';

     TYPE REC1(DISC : STATCHAR) IS
          RECORD
               CASE DISC IS
                    WHEN 'I' => NULL;
                    WHEN 'J' => NULL;
                    WHEN 'L' => NULL;
                    WHEN 'M' => NULL;
               END CASE; END RECORD;     -- ERROR: {5:16;13} MISSING CHOICES.

     TYPE REC2(DISC : NEWSTAT) IS
          RECORD
               CASE DISC IS
                    WHEN 'B' => NULL;
                    WHEN 'C' => NULL;
                    WHEN 'D' => NULL;
               END CASE; END RECORD;     -- ERROR: {4:16;13} MISSING OTHERS
                                         --        CHOICE.

     TYPE REC3(DISC : SSTAT) IS
          RECORD
               CASE DISC IS
                    WHEN 'I' => NULL;
                    WHEN 'J' => NULL;
                    WHEN 'K' => NULL;
                    WHEN 'L' => NULL;
                    WHEN 'M' => NULL;
                    WHEN 'N' => NULL;    -- ERROR: {6:16;1} CHOICE OUTSIDE
                                         --        OF RANGE.
               END CASE; END RECORD;

BEGIN
     NULL;
END B37309B;
