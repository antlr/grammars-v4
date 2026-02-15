-- B37401A.ADA

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
-- CHECK THAT THE 'CONSTRAINED ATTRIBUTE CANNOT BE APPLIED TO A
-- RECORD, PRIVATE OR LIMITED PRIVATE OBJECT WHOSE TYPE HAS NO
-- DISCRIMINANTS, OR TO AN ARRAY OBJECT.
-- CHECK FOR
-- OBJECTS DESIGNATED BY AN ACCESS VALUE AND OBJECTS DECLARED DIRECTLY
-- OR AS FORMAL PARAMETERS OF SUBPROGRAMS AND GENERIC UNITS.

-- CHECK THAT 'CONSTRAINED CANNOT BE APPLIED TO A RECORD TYPE WITH
-- DISCRIMINANTS.


-- ASL 7/6/81
-- SPS 2/10/83
-- JWC 10/7/85   RENAMED FROM B37204A-AB.ADA.
--               ADDED CHECKS OF OBJECTS DESIGNATED BY ACCESS VALUE.
--               ADDED CHECKS TO THE VALUE OF A FUNCTION RETURNING A
--               RECORD OBJECT THAT HAS DISCRIMANTS.
-- JRK 11/15/85  ADDED PRIVATE, LIMITED PRIVATE, AND ACCESS CASES.
--               ADDED NON-OBJECT CASE.
-- RJW 5/07/86   CORRECTED TEST ERROR: PLACEMENT OF DECLARATIONS OF 
--               PRIVATE, LIMITED PRIVATE AND ACCESS TYPES.
-- KAS 11/16/95  LOOSENED FOR ADA95
-- PWN 02/16/96  Restored 3 cases as Ada 95 checks.
PROCEDURE B37401A IS
 
      SUBTYPE B IS BOOLEAN;
 
      TYPE REC2(DISC : INTEGER := 1) IS
           RECORD
                NULL;
           END RECORD;
 
      SUBTYPE SREC2 IS REC2 (5);
 
      TYPE REC2_NAME IS ACCESS REC2;
 
      TYPE REC IS
           RECORD
                COMP : REC2;
           END RECORD;
 
      R : REC;
      ERR1 : B := R'CONSTRAINED;               -- ERROR: 'CONSTRAINED.
 
      TYPE REC_NAME IS ACCESS REC;
      R_N : REC_NAME := NEW REC;
      ERR2 : B := R_N.ALL'CONSTRAINED;         -- ERROR: 'CONSTRAINED.
 
      I : INTEGER;
      ERR3 : B := I'CONSTRAINED;               -- ERROR: 'CONSTRAINED.
 
      TYPE ARR IS ARRAY(1..10) OF INTEGER;
      A : ARR;
      ERR4 : B := A'CONSTRAINED;               -- ERROR: 'CONSTRAINED.
 
      TYPE ACARR IS ACCESS ARR;
      AC : ACARR := NEW ARR;
      ERR5 : B := AC.ALL'CONSTRAINED;          -- ERROR: 'CONSTRAINED.
 
      GENERIC
           TYPE Q IS PRIVATE;
           NODISC : IN OUT Q;
      PACKAGE GPACK IS
           ERR6 : B := NODISC'CONSTRAINED;     -- ERROR: 'CONSTRAINED.
           X : Q;
           ERR7 : B := X'CONSTRAINED;          -- ERROR: 'CONSTRAINED.
      END GPACK;
 
      GENERIC
           TYPE R IS PRIVATE;
      PACKAGE HPACK IS
           X : R;
      END HPACK;
 
      PACKAGE INST2 IS NEW HPACK(SREC2);
      OK8 : B := INST2.X'CONSTRAINED;          -- OK.
 
      PACKAGE PACK1 IS
           TYPE TPRIV1 IS PRIVATE;
           TYPE TPRIV2 IS PRIVATE;
           TYPE ACTP1 IS ACCESS TPRIV1;
           TYPE ACTP2 IS ACCESS TPRIV2;
           TYPE TLIM1 IS LIMITED PRIVATE;
           TYPE TLIM2 IS LIMITED PRIVATE;
           TYPE ACTL1 IS ACCESS TLIM1;
           TYPE ACTL2 IS ACCESS TLIM2;
      PRIVATE
           TYPE TPRIV1 IS NEW REC;
           XTPRIV1 : TPRIV1;
           ERR12 : B := XTPRIV1'CONSTRAINED;   -- ERROR: 'CONSTRAINED.
           A_TP : ACTP1 := NEW TPRIV1;
           ERR13 : B := A_TP.ALL'CONSTRAINED;  -- ERROR: 'CONSTRAINED.
           TYPE TPRIV2 IS NEW REC2(DISC => 3);
           XTPRIV2 : TPRIV2;
           OK14 : B := XTPRIV2'CONSTRAINED;    -- OK.
           A_TP2 : ACTP2 := NEW TPRIV2;
           OK15 : B := A_TP2.ALL'CONSTRAINED;  -- OK.
           TYPE TLIM1 IS NEW REC;
           XTLIM1 : TLIM1;
           ERR16 : B := XTLIM1'CONSTRAINED;    -- ERROR: 'CONSTRAINED.
           A_TL : ACTL1 := NEW TLIM1;
           ERR17 : B := A_TL.ALL'CONSTRAINED;  -- ERROR: 'CONSTRAINED.
           TYPE TLIM2 IS NEW REC2(3);
           XTLIM2 : TLIM2;
           OK18 : B := XTLIM2'CONSTRAINED;     -- OK.
           A_TL2 : ACTL2 := NEW TLIM2;
           OK19 : B := A_TL2.ALL'CONSTRAINED;  -- OK.
      END PACK1;
      USE PACK1;
 
      XTP2  : TPRIV2;
      ERR20 : B := XTP2'CONSTRAINED;           -- ERROR: 'CONSTRAINED.
      XTL2  : TLIM2;
      ERR21 : B := XTL2'CONSTRAINED;           -- ERROR: 'CONSTRAINED.
      ATP2  : ACTP2 := NEW TPRIV2;
      ERR22 : B := ATP2.ALL'CONSTRAINED;       -- ERROR: 'CONSTRAINED.
      ATL2  : ACTL2 := NEW TLIM2;
      ERR23 : B := ATL2.ALL'CONSTRAINED;       -- ERROR: 'CONSTRAINED.
 
      ERR24 : B := REC2'CONSTRAINED;           -- ERROR: 'CONSTRAINED
                                               --        APPLIED TO
                                               --        NON-OBJECT.
 
      PROCEDURE P (PARM : IN OUT REC; PARM2 : REC) IS
           ERR9 : B := PARM'CONSTRAINED;       -- ERROR: 'CONSTRAINED.
           ERR10 : B := PARM2'CONSTRAINED;     -- ERROR: 'CONSTRAINED.
      BEGIN
           NULL;
      END P;
 
      FUNCTION F (PARM : REC) RETURN REC IS
           ERR11 : B := PARM'CONSTRAINED;      -- ERROR: 'CONSTRAINED.
      BEGIN
           RETURN PARM;
      END F;
 
      FUNCTION F1 RETURN REC2 IS
           X : REC2;
      BEGIN
           RETURN X;
      END F1;
 
      FUNCTION F2 RETURN REC2_NAME IS
           X : REC2_NAME;
      BEGIN
           X := NEW REC2;
           RETURN X;
      END F2;
 
BEGIN
      DECLARE
 
           OK25 : B := F1'CONSTRAINED;        -- OK.
           OK26 : B := F2'CONSTRAINED;        -- OK.
           OK27 : B := F2.ALL'CONSTRAINED;    -- OK.
           R2N  : REC2_NAME := NEW REC2;
           OK28  : B := R2N'CONSTRAINED;       -- OK.
           OK29  : B := R2N.ALL'CONSTRAINED;   -- OK.
 
      BEGIN
           NULL;
      END;
 
END B37401A;
