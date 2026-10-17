-- BC3405F.ADA

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
-- CHECK THAT THE COMPONENT TYPE OF A GENERIC FORMAL TYPE AND THE ACTUAL
-- ARRAY TYPE MUST BE EITHER CONSTRAINED OR UNCONSTRAINED.

-- CHECK WHEN THE COMPONENT TYPE IS AN ACCESS TYPE DESIGNATING OBJECTS
-- OF A TYPE WITH DISCRIMINANTS.

-- CHECK WHEN THE COMPONENT TYPE IS A GENERIC FORMAL PARAMETER
-- DECLARED IN AN ENCLOSING GENERIC UNIT.

-- SPS 6/28/82
-- JBG 3/9/83

PROCEDURE BC3405F IS

     GENERIC
          TYPE PV (D:INTEGER) IS PRIVATE;
          TYPE LP (D:INTEGER) IS LIMITED PRIVATE;
     PACKAGE PACK IS
          SUBTYPE CPV IS PV(D => 3);
          SUBTYPE CLP IS LP(D => 3);

          TYPE APV IS ACCESS PV;
          TYPE ACPV IS ACCESS CPV;
          SUBTYPE APVC IS APV(D => 3);

          TYPE ALP IS ACCESS LP;
          TYPE ACLP IS ACCESS CLP;
          SUBTYPE ALPC IS ALP(D => 3);
          SUBTYPE NATURAL IS INTEGER RANGE 1..INTEGER'LAST;

          TYPE AR_APV   IS ARRAY (NATURAL) OF APV;
          TYPE AR_ACPV  IS ARRAY (NATURAL) OF ACPV;
          TYPE AR_APVC  IS ARRAY (NATURAL) OF APVC;
          TYPE AR_ALP   IS ARRAY (NATURAL) OF ALP;
          TYPE AR_ACLP  IS ARRAY (NATURAL) OF ACLP;
          TYPE AR_ALPC  IS ARRAY (NATURAL) OF ALPC;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF APV;
          PACKAGE PAPV IS END PAPV;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF APVC;
          PACKAGE PAPVC IS END PAPVC;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF ALP;
          PACKAGE PALP IS END PALP;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF ALPC;
          PACKAGE PALPC IS END PALPC;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF ACPV;
          PACKAGE PACPV IS END PACPV;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF ACLP;
          PACKAGE PACLP IS END PACLP;

          PACKAGE PAPV1 IS NEW PAPV(AR_APV);       -- OK.
          PACKAGE PAPV2 IS NEW PAPV(AR_APVC);      -- ERROR: AR_APVC.

          PACKAGE PAPVC1 IS NEW PAPVC(AR_APV);     -- ERROR: AR_APV.
          PACKAGE PAPVC2 IS NEW PAPVC(AR_APVC);    -- OK.

          PACKAGE PACPV1 IS NEW PACPV(AR_ACPV);    -- OK.

          PACKAGE PALP1 IS NEW PALP(AR_ALP);       -- OK.
          PACKAGE PALP2 IS NEW PALP(AR_ALPC);      -- ERROR: AR_ALPC.

          PACKAGE PALPC1 IS NEW PALPC(AR_ALP);     -- ERROR: AR_ALP.
          PACKAGE PALPC2 IS NEW PALPC(AR_ALPC);    -- OK.

          PACKAGE PACLP1 IS NEW PACLP(AR_ACLP);    -- OK.
          
     END PACK;
BEGIN
     NULL;
END BC3405F;
