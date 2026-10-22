-- BC1226A.ADA

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
--     CHECK THAT ONLY CORRECT OPERATIONS ARE AVAILABLE FOR GENERIC
--     FORMAL PRIVATE TYPES.

-- HISTORY:
--     DAT 09/18/81  CREATED ORIGINAL TEST.
--     SPS 05/04/82
--     JBG 04/22/83
--     BCB 08/01/88  MODIFIED HEADER FORMAT, ADDED OPTIONAL ERR
--                   MESSAGES, DELETED ALL ALLOWABLE OPERATIONS.
--     THS 02/20/90  CORRECTED UNINTENTIONAL ERRORS.
--     KAS 11/16/95  ALLOW DEFERRED CONSTANTS FOR NON PRIVATE TYPES
--     KAS 11/30/95  DEFERRED SOME CHANGES TO 2.1
--     PWN 03/28/96  Restored changes deferred to 2.1
--     RLB 03/19/07  Updated error descriptions to reflect Amendment 1 changes.

WITH SYSTEM; USE SYSTEM;
PROCEDURE BC1226A IS

     GENERIC
          TYPE T IS PRIVATE;
          TYPE U IS LIMITED PRIVATE;
          TYPE T1 (B : BOOLEAN) IS PRIVATE;
          TYPE U1 (B : BOOLEAN) IS LIMITED PRIVATE;
     PACKAGE PKG IS
          VT : T;
          VU : U;
          XT : T(TRUE);                      -- ERROR: CONSTRAINED.
          XU : U(TRUE);                      -- ERROR: CONSTRAINED.
          VT1 : T1 (TRUE);
          VU1 : U1 (TRUE);
          CT : CONSTANT T := VT;
          CU : CONSTANT U := VU;             -- ERROR: Init w/ limited obj.
          SUBTYPE S0 IS T (TRUE);            -- ERROR: (TRUE).
          SUBTYPE S00 IS U (TRUE);           -- ERROR: (TRUE).
          SUBTYPE ST1 IS T1 (TRUE);
          SUBTYPE SU1 IS U1 (TRUE);
          CT1 : CONSTANT ST1 := VT1;
          CU1 : CONSTANT SU1 := VU1;         -- ERROR: Init w/ limited obj.
          SUBTYPE ST IS T;
          SUBTYPE SU IS U;
          DCT : CONSTANT T;                  -- OK.
          DCU : CONSTANT U;                  -- ERROR: DEFERRED.
          DCT1 : CONSTANT ST1;               -- OK.
          DCU1 : CONSTANT SU1;               -- ERROR: DEFERRED.
          OST : ST;
          OST1 : ST1;
          OSU : SU;
          OSU1 : SU1;
          OX1 : ST := T(VU);                 -- ERROR: VU.
          OX2 : ST1 := ST1'(VU1);            -- ERROR: VU1.
          NUMBER : CONSTANT := CT;           -- ERROR: NO TYPE.
          TYPE AU IS ACCESS U;
          TYPE AU1 IS ACCESS U1;
          VAU : AU := NEW U'(VU);            -- ERROR: Obj init value.
          VAU2 : AU := NEW U (TRUE);         -- ERROR: CONSTRAINT.
          B : BOOLEAN;
     PRIVATE
          DCT : CONSTANT T := VT;
          DCT1 : CONSTANT ST1 := OST1;
     END PKG;

     PACKAGE BODY PKG IS
     BEGIN
          B := VT'CONSTRAINED;               -- ERROR: OBJECT.
          B := VU'CONSTRAINED;               -- ERROR: OBJECT.
          B := VT > VT ;                     -- ERROR: PRIVATE.
          B := VU = VU ;                     -- ERROR: LIMITED.
          B := VT <= CT ;                    -- ERROR: PRIVATE.
          B := VT + VT = VT ;                -- ERROR: PRIVATE.
          B := VU.B ;                        -- ERROR: B.
          B := U'ADDRESS /= U'ADDRESS;       -- ERROR: U.
          B := VT IN VT .. VT ;              -- ERROR: .. .
          B := VT ** 2 /= 0 ;                -- ERROR: **.
          B := ABS (VT) /= VT ;              -- ERROR: PRIVATE.

          DECLARE
               TYPE AU IS ARRAY (1..0) OF U;
               V : AU;
               B : BOOLEAN := V = V;         -- ERROR: V IS LIMITED.
          BEGIN
               VU := VU;                     -- ERROR: LIMITED.
               VU1 := VU1;                   -- ERROR: LIMITED.
          END;

     END;
BEGIN
     NULL;
END BC1226A;
