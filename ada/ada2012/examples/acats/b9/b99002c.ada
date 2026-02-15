-- B99002C.ADA

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
-- CHECK THAT COUNT IS NOT ALLOWED WITHIN A PROGRAM UNIT INTERNAL TO A
-- TASK BODY.

-- SPS 1/24/83

PROCEDURE  B99002C  IS
BEGIN

     DECLARE

          TASK TYPE  T_TYPE  IS
               ENTRY  E ;
          END  T_TYPE ;

          TASK BODY  T_TYPE  IS

               PACKAGE T_PACK IS
               END T_PACK;

               GENERIC
               PACKAGE T_GP IS
               END T_GP;

               TASK T1 IS
                    ENTRY E1;
               END T1;

               PROCEDURE T_PROC IS
               BEGIN
                    IF E'COUNT = 4 THEN      -- ERROR: INSIDE PROCEDURE.
                         NULL;
                    END IF;
               END T_PROC;

               PACKAGE BODY T_PACK IS
               BEGIN
                    IF  E'COUNT = 0  THEN    -- ERROR: INSIDE PACKAGE.
                         NULL;
                    END IF;
               END T_PACK;

               PACKAGE BODY T_GP IS
               BEGIN
                    IF E'COUNT = 1 THEN      -- ERROR: INSIDE GENERIC.
                         NULL;
                    END IF;
               END T_GP;

               TASK BODY T1 IS
               BEGIN
                    IF E'COUNT = 1 THEN      -- ERROR: INSIDE TASK.
                         NULL;
                    END IF;
               END T1;

          BEGIN
               NULL;
          END  T_TYPE ;

     BEGIN
          NULL;
     END ;

END  B99002C ;
