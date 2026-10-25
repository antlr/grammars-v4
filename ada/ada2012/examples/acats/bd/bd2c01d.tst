-- BD2C01D.TST

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
--     CHECK THAT A TASK STORAGE SIZE CANNOT BE GIVEN FOR A TASK
--     DECLARED TO BE A SINGLE TASK OR A NAME DECLARED BY A SUBTYPE
--     DECLARATION.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY:
--     JKC 04/06/88  CREATED ORIGINAL TEST.
--     BCB 04/14/89  CHANGED EXTENSION TO '.TST'.  ADDED A MACRO TO
--                   TASK STORAGE_SIZE CLAUSES.

PROCEDURE BD2C01D IS

     TASK T;
     FOR T'STORAGE_SIZE USE $TASK_STORAGE_SIZE;       -- ERROR:

     TASK TYPE T1;
     SUBTYPE T2 IS T1;
     FOR T2'STORAGE_SIZE USE $TASK_STORAGE_SIZE;      -- ERROR:

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

     TASK BODY T1 IS
     BEGIN
          NULL;
     END T1;

BEGIN
     NULL;
END BD2C01D;
