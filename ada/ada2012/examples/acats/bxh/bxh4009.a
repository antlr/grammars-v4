-- BXH4009.A
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
--       No_Access_Subprograms
--     is accepted.
--
-- TEST DESCRIPTION
--     The test requires that the configuration pragma
--     Restrictions(No_Access_Subprograms) be processed.  Any attempt to
--     define an access to procedure or access to function type should be
--     disallowed.
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
--      05 NOV 96   SAIC   Revised for 2.1 release
--
--!

---------------------------- CONFIGURATION PRAGMAS -----------------------

pragma Restrictions(No_Access_Subprograms);                       -- OK
                                                -- configuration pragma

------------------------ END OF CONFIGURATION PRAGMAS --------------------

------------------------------------------------------------------- BXH4009

procedure BXH4009 is

  type Access_Procedure is access procedure;                      -- ERROR:
                     -- pragma Restrictions(No_Access_Subprograms) in force

  type Access_Function is access function (F: Float) return Float;-- ERROR:
                     -- pragma Restrictions(No_Access_Subprograms) in force

  procedure Proc is
  begin null; end Proc;  

  function Func (F: Float) return Float is
  begin return 0.0; end Func;

  PA : Access_Procedure := Proc'Access;                  -- OPTIONAL ERROR:
                                 -- consequence of illegal type declaration

  FA : Access_Function := Func'Access;                   -- OPTIONAL ERROR:
                                 -- consequence of illegal type declaration

begin  -- Main test procedure.

  null;

end BXH4009;
