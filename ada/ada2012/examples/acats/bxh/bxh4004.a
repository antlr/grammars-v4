-- BXH4004.A
--
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
--
-- OBJECTIVE
--     Check pragma Restrictions.
--     Check that the application of the configuration pragma Restrictions
--     with the specific restriction:
--       No_Unchecked_Deallocation
--     disallows the use of Unchecked_Deallocation;
--
--     Check that the application of the configuration pragma Restrictions
--     with the specific restriction:
--       Immediate_Reclamation
--     is accepted.
--
-- TEST DESCRIPTION
--     The test requires that the configuration pragma
--     Restrictions(No_Unchecked_Deallocation) be processed.  Any use of
--     Unchecked_Deallocation should be disallowed.
--
--     The test requires that the configuration pragma
--     Restrictions(Immediate_Reclamation) be processed.  No other testing
--     is possible.
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for a compiler attempting validation
--      for the Safety and Security Annex.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      26 OCT 95   SAIC   Initial version
--      04 NOV 96   SAIC   Revised for release 2.1
--
--!

---------------------------- CONFIGURATION PRAGMAS -----------------------

pragma Restrictions(No_Unchecked_Deallocation);                   -- OK
                                                -- configuration pragma

pragma Restrictions(Immediate_Reclamation);                       -- OK
                                                -- configuration pragma

------------------------ END OF CONFIGURATION PRAGMAS --------------------


------------------------------------------------------------------- BXH4004

with Unchecked_Deallocation;                                      -- ERROR:
                 -- pragma Restrictions(No_Unchecked_Deallocation) in force

procedure BXH4004 is
  type I_Point is access Integer;

  Ptr : I_Point;

  procedure Release is
     new Unchecked_Deallocation(Integer,I_Point);        -- OPTIONAL ERROR:
                 -- pragma Restrictions(No_Unchecked_Deallocation) in force
    -- earlier error message on the context clause may supersede this error
begin
  Ptr := new Integer;
  Ptr.all := 17;
  Release( Ptr );                                        -- OPTIONAL ERROR:


  declare
    type Another_I_Point is access Integer;
    type Handles is array(1..1000) of Another_I_Point;
  begin
    for I in 1..10_000 loop
      declare
        A_Big_One : array(1..10_000) of Handles;
      begin
        A_Big_One( I )( I/10+1 ) := new Integer'(I);
      end;
      -- reclamation should be performed here;
      -- if it does not happen, this loop should bomb
   end loop;
  end;

end BXH4004;
