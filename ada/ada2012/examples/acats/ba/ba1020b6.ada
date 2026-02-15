-- BA1020B6M.ADA

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
-- CHECK THAT IF A PACKAGE THAT HAS SUBUNITS IS RECOMPILED
--   AS A SUBPROGRAM, THE SUBUNITS ARE NO LONGER ACCESSIBLE.

-- SEPARATE FILES ARE:
--   BA1020B0  A LIBRARY PACKAGE   (PAC2PROC).
--   BA1020B1  A SUBUNIT PROCEDURE (P).
--   BA1020B2  A LIBRARY PROCEDURE (PAC2PROC).
--   BA1020B3  A LIBRARY PACKAGE   (PAC2FUN).
--   BA1020B4  A SUBUNIT FUNCTION  (F).
--   BA1020B5  A LIBRARY FUNCTION  (PAC2FUN).
--   BA1020B6M THE MAIN PROCEDURE.

-- WKB 6/17/81
-- SPS 11/9/82

WITH PAC2PROC, PAC2FUN;
PROCEDURE BA1020B6M IS

     INVOKED : BOOLEAN := FALSE;

BEGIN

     PAC2PROC.P (INVOKED);           -- ERROR: UNACCESSIBLE PROCEDURE.
     INVOKED := FALSE;

     IF PAC2FUN.F THEN               -- ERROR: UNACCESSIBLE FUNCTION.
          INVOKED := FALSE;
     END IF;

END BA1020B6M;
