-- BD7302A.ADA

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
--     THE PREFIX OF THE ATTRIBUTES 'MACHINE_RADIX, 'MACHINE_MANTISSA,
--     'MACHINE_EMAX, AND MACHINE_EMIN CANNOT BE A FIXED POINT OR AN
--     INTEGER TYPE.

-- HISTORY:
--     DHH 08/31/88  CREATED ORIGINAL TEST.
--     PWN 12/19/94  CORRECTED -- ERROR: INCONSITENCIES
--     PWN 10/27/95  MACHINE_RADIX LEGAL FOR FIXED_POINT TYPES IN ADA 95.
--     PWN 04/11/96  Restored checks in Ada95 legal format.

PROCEDURE BD7302A IS

     TYPE FIXED IS DELTA 0.125 RANGE 0.0 .. 10.0;

     TYPE DERVD IS NEW FIXED;

     TYPE INTGR IS NEW INTEGER;

     SUBTYPE SUB_I IS INTEGER;

     A : INTEGER := FIXED'MACHINE_RADIX;               -- OK.
     B : INTEGER := DERVD'MACHINE_RADIX;               -- OK.
     C : INTEGER := INTGR'MACHINE_RADIX;               -- ERROR:
     D : INTEGER := SUB_I'MACHINE_RADIX;               -- ERROR:

     E : INTEGER := FIXED'MACHINE_MANTISSA;            -- ERROR:
     F : INTEGER := DERVD'MACHINE_MANTISSA;            -- ERROR:
     G : INTEGER := INTGR'MACHINE_MANTISSA;            -- ERROR:
     H : INTEGER := SUB_I'MACHINE_MANTISSA;            -- ERROR:

     I : INTEGER := FIXED'MACHINE_EMAX;                -- ERROR:
     J : INTEGER := DERVD'MACHINE_EMAX;                -- ERROR:
     K : INTEGER := INTGR'MACHINE_EMAX;                -- ERROR:
     L : INTEGER := SUB_I'MACHINE_EMAX;                -- ERROR:

     M : INTEGER := FIXED'MACHINE_EMIN;                -- ERROR:
     N : INTEGER := DERVD'MACHINE_EMIN;                -- ERROR:
     O : INTEGER := INTGR'MACHINE_EMIN;                -- ERROR:
     P : INTEGER := SUB_I'MACHINE_EMIN;                -- ERROR:

BEGIN
     NULL;
END BD7302A;
