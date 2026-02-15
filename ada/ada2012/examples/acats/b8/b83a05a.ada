-- B83A05A.ADA

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
-- CHECK THAT A LOOP PARAMETER CAN BE SPELLED THE SAME AS A LABEL
--    OCCURRING PRIOR TO THE LOOP BUT NOT THE SAME AS A LABEL OCCURRING
--    WITHIN THE LOOP.

-- RM 02/11/80
-- SPS 11/10/82
-- JBG 11/11/83
-- JRK 12/20/83

-- FORM OF BOUNDS            * RANGE                    _R
--                           * TYPEMARK                 _TM
--                           * TYPEMARK + RANGE         _TMR

-- ORIGIN OF TYPEMARK        * TYPE                     _T
--                           * SUBTYPE                  _ST
--                           * DERIVED TYPE             _DT

-- PLACE OF LABEL            * PRIOR TO THE LOOP        _P         * A *
--                           * IN SOME PRIOR LOOP       _PL        * B *
--                           * IN THE LOOP              _L         * C *

-- TYPE OF THE VALUES        * INTEGER                  _I         * 1 *
--                           * BOOLEAN, CHARACTER       _B , _C    * 2 *
--                           * USER-DEFINED ENUMERATION _E         * 3 *
                                                            
-- CASES TESTED:  (3-BY-3 GRAECO-LATIN SQ.)       |   R  TM  TMR
--                                            ----+--------------
--                                              T |  A1  B2  C3
--                                                |  |
--   (EXTRA FRILLS: 2 EMBEDDED LOOPS,          ST |  B3  C1  A2
--                  3 KINDS OF DER.T.)            |
--                                             DT |  C2  A3  B1

PROCEDURE  B83A05A  IS

     TYPE     ENUMERATION      IS  ( AA , BB , CC , DD , EE );
     TYPE     ENUM2            IS  ( AA2 , BB2 , CC2 , DD2 , EE2 );
     SUBTYPE  INTEGER_SUBTYPE  IS  INTEGER RANGE 1..10; 
     SUBTYPE  BOOLEAN_SUBTYPE  IS  BOOLEAN RANGE FALSE..TRUE; 
     TYPE     DER_INTEGER      IS  NEW INTEGER_SUBTYPE; 
     TYPE     DER_CHARACTER    IS  NEW CHARACTER; 
     TYPE     DER_ENUM         IS  NEW ENUMERATION RANGE AA..AA; 

BEGIN                                                       

     << LAB_T_R_P_I >>            NULL;     -- A1
     << LAB_ST_TMR_P_B >>         NULL;     -- A2
     << LAB_D_TM_P_E >>           NULL;     -- A3

     FOR  LAB_T_R_P_I  IN  INTEGER'FIRST..INTEGER'FIRST+1  LOOP --- A1
          << LAB_T_TM_PL_C >>       NULL;     -- B2
     END LOOP;


     BEGIN

          << LAB_ST_R_PL_E >>       NULL;     -- B3

          FOR  LAB_T_TM_PL_C  IN  CHARACTER'('A') .. 'B'  LOOP  --- B2
               NULL; 
          END LOOP;

     END; 

     FOR  LAB_T_TMR_L_E  IN  ENUMERATION RANGE AA..DD  LOOP     --- C3
          << LAB_T_TMR_L_E >>       NULL;     -- ERROR: LABEL NOT VIS.
     END LOOP;

     FOR  LAB_ST_TM_L_I  IN  INTEGER_SUBTYPE  LOOP       --- C1
          FOR  LAB_ST_R_PL_E  IN  BB2..CC2  LOOP         --- B3
          << LAB_D_TMR_PL_I >>      NULL;     -- B1
          << LAB_ST_TM_L_I >>       NULL;     -- ERROR: LABEL NOT VIS.
          END LOOP;
     END LOOP;

     FOR  LAB_ST_TMR_P_B  IN  BOOLEAN_SUBTYPE  LOOP             --- A2
          NULL; 
     END LOOP;

     FOR  LAB_D_R_L_C  IN
          DER_CHARACTER'('A')..DER_CHARACTER'('E')  LOOP        --- C2
          << LAB_D_R_L_C >>         NULL;      -- ERROR: LABEL NOT VIS.
     END LOOP;

     FOR  LAB_D_TM_P_E  IN  DER_ENUM  LOOP                      --- A3
          NULL; 
     END LOOP;

     FOR  LAB_D_TMR_PL_I  IN  DER_INTEGER RANGE 1..10  LOOP     --- B1
          NULL; 
     END LOOP;

END B83A05A;
