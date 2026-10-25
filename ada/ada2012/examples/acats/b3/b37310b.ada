-- B37310B.ADA

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
-- CHECK THAT IF A DISCRIMINANT HAS A DYNAMIC SUBTYPE, AN OTHERS
-- CHOICE MUST NOT BE OMITTED IF ONE OR MORE VALUES IN THE BASE
-- TYPE'S RANGE ARE MISSING.

--     ASL 07/10/81
--     SPS 12/7/82
--     SPS 02/17/83
--     RLB 02/08/18 ADDED ERROR LOCATION INDICATORS TO REFLECT COMMON
--                  ERROR REPORTING STRATEGIES.

PROCEDURE B37310B IS

     ACHAR : CHARACTER := 'A';
     ECHAR : CHARACTER := 'E';
     JCHAR : CHARACTER := 'J';
     MCHAR : CHARACTER := 'M';
     SUBTYPE STATCHAR IS CHARACTER RANGE 'I'..'N';
     SUBTYPE DYNCHAR IS CHARACTER RANGE ACHAR..ECHAR;
     SUBTYPE SSC IS STATCHAR RANGE JCHAR..MCHAR;
     SUBTYPE SDYN IS DYNCHAR RANGE 'B' .. 'D';
     TYPE LETTER IS NEW CHARACTER RANGE 'A'..'Z';
     SUBTYPE DYNLETTER IS LETTER RANGE LETTER(ECHAR)..LETTER(JCHAR);

     TYPE REC1(DISC : SSC) IS
          RECORD
               CASE DISC IS
                    WHEN ' '..ASCII.DEL => NULL;
               END CASE; END RECORD;       -- ERROR: {2:16;13} MISSING CHOICES.

     TYPE REC2(DISC : DYNCHAR) IS
          RECORD
               CASE DISC IS
                    WHEN ASCII.NUL..ASCII.TILDE => NULL;
               END CASE; END RECORD;       -- ERROR: {2:16;13} MISSING CHOICE.

     TYPE REC3(DISC: DYNCHAR) IS
          RECORD
               CASE DISC IS
                    WHEN ' '..CHARACTER'LAST => NULL;
               END CASE; END RECORD;       -- ERROR: {2:16;13} MISSING CHOICES.

     TYPE REC4(DISC : DYNLETTER) IS
          RECORD
               CASE DISC IS
                    WHEN LETTER'VAL(0)..'W'   => NULL;
                    WHEN 'Y'..LETTER'VAL(127) => NULL;
               END CASE; END RECORD;       -- ERROR: {3:16;13} MISSING CHOICE.

     TYPE REC5(DISC : SDYN) IS
          RECORD
               CASE DISC IS
                    WHEN 'B' => NULL;
                    WHEN 'C' => NULL;
                    WHEN 'D' => NULL;
               END CASE;                   -- ERROR: {4:16;1} MISSING CHOICES.
          END RECORD;

BEGIN
     NULL;
END B37310B;
