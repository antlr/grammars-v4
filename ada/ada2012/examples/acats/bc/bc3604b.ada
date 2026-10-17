-- BC3604B.ADA

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
-- CHECK THAT PARAMETER MODES OF ACTUALS ARE NOT USED IN RESOLVING
-- OVERLOADED ACTUALS FOR GENERIC FORMAL PROCEDURES.

-- PWB  4/17/86

PROCEDURE BC3604B IS

     GENERIC
          WITH PROCEDURE FORMAL ( X : IN INTEGER );
     PACKAGE GEN_PACK IS
     END GEN_PACK;

     GENERIC
          WITH PROCEDURE FORMAL ( Y : OUT INTEGER );
     PROCEDURE GEN_PROC;

     GENERIC
          WITH PROCEDURE FORMAL ( Z : IN OUT INTEGER );
     FUNCTION GEN_FUNC RETURN INTEGER;

     PROCEDURE GEN_PROC IS
     BEGIN
          NULL;
     END GEN_PROC;

     FUNCTION GEN_FUNC RETURN INTEGER IS
     BEGIN
          RETURN 0;
     END GEN_FUNC;

     PACKAGE IN_PACK IS
          PROCEDURE ACTUAL ( X : IN INTEGER );
     END IN_PACK;

     PACKAGE OUT_PACK IS
          PROCEDURE ACTUAL ( X : OUT INTEGER );
     END OUT_PACK;

     PACKAGE IN_OUT_PACK IS
          PROCEDURE ACTUAL ( X : IN OUT INTEGER );
     END IN_OUT_PACK;

     PACKAGE BODY IN_PACK IS
          PROCEDURE ACTUAL ( X : IN INTEGER ) IS
          BEGIN
               NULL;
          END ACTUAL;
     END IN_PACK;

     PACKAGE BODY OUT_PACK IS
          PROCEDURE ACTUAL ( X : OUT INTEGER ) IS
          BEGIN
               NULL;
          END ACTUAL;
     END OUT_PACK;

     PACKAGE BODY IN_OUT_PACK IS
          PROCEDURE ACTUAL ( X : IN OUT INTEGER ) IS
          BEGIN
               NULL;
          END ACTUAL;
     END IN_OUT_PACK;

     USE IN_PACK, OUT_PACK, IN_OUT_PACK;

     PACKAGE EXPECT_IN_PARM IS 
          NEW GEN_PACK (ACTUAL);           -- ERROR: ACTUAL AMBIGUOUS.

     PROCEDURE EXPECT_OUT_PARM IS
          NEW GEN_PROC (ACTUAL);           -- ERROR: ACTUAL AMBIGUOUS.

     FUNCTION EXPECT_IN_OUT_PARM IS
          NEW GEN_FUNC (ACTUAL);           -- ERROR: ACTUAL AMBIGUOUS.

BEGIN     -- BC3604B
     NULL;
END BC3604B;
