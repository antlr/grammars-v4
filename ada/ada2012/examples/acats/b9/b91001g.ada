-- B91001G.ADA

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
-- CHECK THAT A LENGTH CLAUSE IS NOT ALLOWED IN A TASK SPECIFICATION
--   (FOR 'SIZE, 'STORAGE_SIZE, AND 'SMALL).

-- JRK 9/16/81
-- ABW 7/23/82
-- TBN 1/28/86     ADDED CHECKS THAT 'SIZE AND 'STORAGE_SIZE CANNOT BE
--                 USED IN A TASK SPECIFICATION.

PROCEDURE B91001G IS

     TASK TYPE T IS
          ENTRY E;
          FOR T'SMALL USE 1.0;           -- ERROR: FIXED POINT ATTRIBUTE
                                         --        APPLIED TO TASK TYPE.
          FOR T'SIZE USE 1 * 8;          -- ERROR: T'SIZE.
          FOR T'STORAGE_SIZE USE 5 * 8;  -- ERROR: T'STORAGE_SIZE.
     END T;

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

BEGIN
     NULL;
END B91001G;
