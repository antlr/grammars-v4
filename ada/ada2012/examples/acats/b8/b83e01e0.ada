-- B83E01E0M.ADA

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
--     CHECK THAT A FORMAL PARAMETER OF A SUBPROGRAM SUBUNIT, IN A
--     SEPARATE FILE FROM THE SPECIFICATION CANNOT  BE IDENTICAL TO
--     ANY OTHER IDENTIFIERS DECLARED IN THE SUBUNIT.

-- SEPARATE FILES:
--     B83E01E0M CONTAINS THE PROCEDURE WHICH HOLDS THE SPECIFICATIONS
--               FOR THE SUBUNITS. THIS FILE SHOULD COMPILE WITHOUT
--               ERRORS.
--     B83E01E1  CONTAINS THE SUBUNIT BODY FOR B83E01E_PROC1.
--     B83E01E2  CONTAINS THE SUBUNIT BODY FOR B83E01E_PROC2.
--     B83E01E3  CONTAINS THE SUBUNIT BODY FOR B83E01E_PROC3.

-- HISTORY:
--     DHH 09/15/88  CREATED ORIGINAL TEST.
--     MCH 04/09/90  SPLIT COMPILATIONS INTO SEPARATE FILES TO AVOID
--                   CONFLICTS WITH AI-000255.

PROCEDURE B83E01E0M IS

     PROCEDURE B83E01E_PROC1(PARAM1, PARAM2, PARAM3, PARAM4, PARAM5,
                             PARAM6, PARAM7 : INTEGER) IS SEPARATE;

     PROCEDURE B83E01E_PROC2(PARAM1, PARAM2, PARAM3, PARAM4 :
                             IN OUT INTEGER) IS SEPARATE;

     FUNCTION B83E01E_PROC3(PARAM1, PARAM2, PARAM3, PARAM4, PARAM5,
                            PARAM6 :INTEGER) RETURN BOOLEAN IS SEPARATE;
BEGIN
     NULL;
END B83E01E0M;
