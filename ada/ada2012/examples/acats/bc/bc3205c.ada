-- BC3205C.ADA

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
--     CHECK THAT AN INSTANTIATION IS ILLEGAL IF A FORMAL LIMITED/NON-
--     LIMITED PRIVATE TYPE WITH UNKNOWN DISCRIMINANTS IS USED IN AN
--     ALLOCATOR, A VARIABLE OBJECT DECLARATION, A RECORD COMPONENT
--     DECLARATION, OR AN ARRAY TYPE DEFINITION.

--     TEST WHEN THE INSTANTIATIONS, GENERIC SPECIFICATION, AND GENERIC
--     BODY ARE IN SEPARATE PROGRAM UNITS WITHIN THE SAME COMPILATION.
--     THE INSTANTIATIONS ARE COMPILED BEFORE THE GENERIC BODY.

-- HISTORY:
--     SPS 07/14/82
--     JBG 03/8/84
--     JBG 04/29/85
--     THS 04/16/90  SPLIT TEST TO BC3205I.ADA.
--     JRL 05/03/96  CHANGED OBJECTIVE AND TEST TO REFLECT ADA95 RULES.


GENERIC
     TYPE P (<>) IS PRIVATE;
     TYPE L IS LIMITED PRIVATE;
PACKAGE BC3205C_PK2 IS
   procedure Dummy;
END BC3205C_PK2;

GENERIC
     TYPE P IS PRIVATE;
     TYPE L (<>) IS LIMITED PRIVATE;
PACKAGE BC3205C_PK3 IS
   procedure Dummy;
END BC3205C_PK3;

GENERIC
     TYPE PV (<>) IS PRIVATE;
     TYPE LP IS LIMITED PRIVATE;
PACKAGE BC3205C_PK4 IS
   procedure Dummy;
END BC3205C_PK4;

GENERIC
     TYPE PV IS PRIVATE;
     TYPE LP (<>) IS LIMITED PRIVATE;
PACKAGE BC3205C_PK5 IS
   procedure Dummy;
END BC3205C_PK5;

GENERIC
     TYPE PRV (<>) IS PRIVATE;
     TYPE LIM IS LIMITED PRIVATE;
PACKAGE BC3205C_PK6 IS
   procedure Dummy;
END BC3205C_PK6;

GENERIC
     TYPE PRV IS PRIVATE;
     TYPE LIM (<>) IS LIMITED PRIVATE;
PACKAGE BC3205C_PK7 IS
   procedure Dummy;
END BC3205C_PK7;

GENERIC
     TYPE LMP (<>) IS LIMITED PRIVATE;
     TYPE PVT IS PRIVATE;
PACKAGE BC3205C_PK8 IS
   procedure Dummy;
END BC3205C_PK8;

GENERIC
     TYPE LMP IS LIMITED PRIVATE;
     TYPE PVT (<>) IS PRIVATE;
PACKAGE BC3205C_PK9 IS
   procedure Dummy;
END BC3205C_PK9;

WITH BC3205C_PK2, BC3205C_PK3, BC3205C_PK4, BC3205C_PK5,
     BC3205C_PK6, BC3205C_PK7, BC3205C_PK8, BC3205C_PK9;
PROCEDURE BC3205C IS

     TYPE REC(D : INTEGER) IS RECORD NULL; END RECORD;
     SUBTYPE CREC IS REC(4);

     PACKAGE N2 IS NEW BC3205C_PK2(REC, CREC);    -- OK.
     PACKAGE N3 IS NEW BC3205C_PK3(CREC, REC);    -- OK.
     PACKAGE N4 IS NEW BC3205C_PK4(REC, CREC);    -- OK.
     PACKAGE N5 IS NEW BC3205C_PK5(CREC, REC);    -- OK.
     PACKAGE N6 IS NEW BC3205C_PK6(REC, CREC);    -- OK.
     PACKAGE N7 IS NEW BC3205C_PK7(CREC, REC);    -- OK.
     PACKAGE N8 IS NEW BC3205C_PK8(REC, CREC);    -- OK.
     PACKAGE N9 IS NEW BC3205C_PK9(CREC, REC);    -- OK.
BEGIN
     NULL;
END BC3205C;

PACKAGE BODY BC3205C_PK2 IS

     procedure Dummy is begin null; end Dummy;

     TYPE AP IS ACCESS P;
     TYPE AL IS ACCESS L;
     NP : AP := NEW P;                    -- ERROR: P is indefinite.
     NL : AL := NEW L;

BEGIN
     NULL;
END BC3205C_PK2;

PACKAGE BODY BC3205C_PK3 IS

     procedure Dummy is begin null; end Dummy;

     TYPE AP IS ACCESS P;
     TYPE AL IS ACCESS L;
     NP : AP := NEW P;
     NL : AL := NEW L;                    -- ERROR: L is indefinite.

BEGIN
     NULL;
END BC3205C_PK3;

PACKAGE BODY BC3205C_PK4 IS

     procedure Dummy is begin null; end Dummy;

     VP : PV;                             -- ERROR: PV is indefinite.
     VL : LP;

BEGIN
     NULL;
END BC3205C_PK4;

PACKAGE BODY BC3205C_PK5 IS

     procedure Dummy is begin null; end Dummy;

     VP : PV;
     VL : LP;                             -- ERROR: LP is indefinite.

BEGIN
     NULL;
END BC3205C_PK5;

PACKAGE BODY BC3205C_PK6 IS

     procedure Dummy is begin null; end Dummy;

     TYPE ARP IS ARRAY (INTEGER) OF PRV;  -- ERROR: PRV is indefinite.
     TYPE ARL IS ARRAY (INTEGER) OF LIM;

BEGIN
     NULL;
END BC3205C_PK6;

PACKAGE BODY BC3205C_PK7 IS

     procedure Dummy is begin null; end Dummy;

     TYPE ARP IS ARRAY (INTEGER) OF PRV;
     TYPE ARL IS ARRAY (INTEGER) OF LIM;  -- ERROR: LIM is indefinite.

BEGIN
     NULL;
END BC3205C_PK7;

PACKAGE BODY BC3205C_PK8 IS

     procedure Dummy is begin null; end Dummy;

     TYPE RC IS RECORD
          RCP : PVT;
          RCL : LMP;                      -- ERROR: LMP is indefinite.
     END RECORD;

BEGIN
     NULL;
END BC3205C_PK8;

PACKAGE BODY BC3205C_PK9 IS

     procedure Dummy is begin null; end Dummy;

     TYPE RC IS RECORD
          RCP : PVT;                      -- ERROR: PVT is indefinite.
          RCL : LMP;
     END RECORD;

BEGIN
     NULL;
END BC3205C_PK9;
