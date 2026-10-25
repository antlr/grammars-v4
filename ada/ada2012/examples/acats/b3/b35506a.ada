-- B35506A.ADA

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
--     CHECK THAT THE ARGUMENT TO  T'SUCC ,  T'PRED ,  T'POS , AND
--     T'IMAGE MUST HAVE THE BASE TYPE OF  T .

-- HISTORY:
--     RM  03/13/81  CREATED ORIGINAL TEST.
--     BCB 08/01/88  MODIFIED HEADER FORMAT AND ADDED CHECKS FOR 'IMAGE.
--     THS 03/28/90  MOVED "-- ERROR:" TO THE RIGHT OF THE CONSTRUCT
--                   IN ERROR.


PROCEDURE  B35506A  IS
BEGIN

     DECLARE

          TYPE  ENUM   IS  ( AA , BB , CC , DD , EE , FF , GG ) ;

          TYPE  NEW_I  IS  NEW INTEGER   ;
          TYPE  NEW_C  IS  NEW CHARACTER ;
          TYPE  NEW_E  IS  NEW ENUM      ;

          IVAR :               INTEGER   :=  7  ;
          CVAR :               CHARACTER := 'A' ;
          EVAR :               ENUM      :=  AA ;

          ICON :      CONSTANT INTEGER   :=  7  ;
          CCON :      CONSTANT CHARACTER := 'A' ;
          ECON :      CONSTANT ENUM      :=  AA ;

          NEW_IVAR :           NEW_I     :=  7  ;
          NEW_CVAR :           NEW_C     := 'A' ;
          NEW_EVAR :           NEW_E     :=  AA ;

          NEW_ICON :  CONSTANT NEW_I     :=  7  ;
          NEW_CCON :  CONSTANT NEW_C     := 'A' ;
          NEW_ECON :  CONSTANT NEW_E     :=  AA ;

     BEGIN


          IF   NEW_I'SUCC(    IVAR) =  NEW_I'SUCC(    IVAR)-- ERROR:
          THEN  NULL ;
          END IF;

          IF CHARACTER'SUCC(NEW_CVAR)= CHARACTER'SUCC(NEW_CVAR)-- ERROR:
          THEN  NULL ;
          END IF;

          IF   NEW_E'SUCC(    ECON) =  NEW_E'SUCC(    ECON)-- ERROR:
          THEN  NULL ;
          END IF;


          IF   NEW_I'PRED(    ICON) =  NEW_I'PRED(    ICON)-- ERROR:
          THEN  NULL ;
          END IF;

          IF   NEW_C'PRED(    CVAR) =  NEW_C'PRED(    CVAR)-- ERROR:
          THEN  NULL ;
          END IF;

          IF   ENUM 'PRED(NEW_EVAR) =  ENUM 'PRED(NEW_EVAR)-- ERROR:
          THEN  NULL ;
          END IF;

          IF INTEGER'POS (NEW_IVAR) =INTEGER'POS (NEW_IVAR)-- ERROR:
          THEN  NULL ;
          END IF;

          IF   NEW_C'POS (    CCON) =  NEW_C'POS (    CCON)-- ERROR:
          THEN  NULL ;
          END IF;

          IF   NEW_E'POS (    EVAR) =  NEW_E'POS (    EVAR)-- ERROR:
          THEN  NULL ;
          END IF;

          IF NEW_I'IMAGE (ICON) = NEW_I'IMAGE (ICON) -- ERROR:
               THEN NULL;
          END IF;

          IF
          CHARACTER'IMAGE(NEW_CVAR) = CHARACTER'IMAGE(NEW_CVAR)-- ERROR:
          THEN NULL;
          END IF;

          IF NEW_E'IMAGE(EVAR) = NEW_E'IMAGE(EVAR)  -- ERROR:
               THEN NULL;
          END IF;

     END ;


END B35506A ;
