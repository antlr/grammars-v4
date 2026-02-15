-- BD7101H.ADA

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
--     CHECK THAT NO INTEGER TYPE CAN BE DECLARED WITH VALUES LESS
--     THAN MIN_INT OR WITH VALUES GREATER THAN MAX_INT.  (USE
--     MIN_INT - 1 .. MIN_INT + 1 AND MAX_INT - 1 .. MAX_INT + 1.)

-- HISTORY:
--     LDC  06/14/88 CREATED ORIGINAL TEST.
--     WMC  04/07/92 REMOVED UNNECESSARY REFERENCE TO PACKAGE REPORT.

WITH SYSTEM; USE SYSTEM;

PROCEDURE BD7101H IS

     TYPE MIN_MAX1 IS RANGE MIN_INT .. MAX_INT;        -- OK.
     TYPE MIN_MAX2 IS RANGE MIN_INT +1 .. MAX_INT -1;  -- OK.

     TYPE MAX IS RANGE MAX_INT -1 .. MAX_INT +1;       -- ERROR:
     TYPE MIN IS RANGE MIN_INT -1 .. MIN_INT +1;       -- ERROR:

BEGIN
     NULL;
END BD7101H;
