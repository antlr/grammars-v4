-- B393010.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
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
-- OBJECTIVE:
--      Check that abstract primitive subprograms for an interface type
--      declared in a visible part are not allowed in the private part
--      (unless they are overriding an inherited subprogram).
--
-- CHANGE HISTORY:
--      28 Oct 07   RLB     Created test based on existing ACATS test B393007.
--      07 Feb 18   RLB     Added error location indicators to reflect common
--                          error reporting strategies. Fixed overlong lines.
--
--!

package B393010 is

   package Pkg1 is
      type Root is limited interface;

      procedure My_Op (Obj : in Root) is abstract;            -- OK. {7;1}
      function "+" (Obj : in Root) return Root is abstract;   -- OK. {7;1}
   end Pkg1;

   package Pkg2 is
      type New_Interf is interface and Pkg1.Root;
      -- Inherits My_Op & "+"
   private
      procedure My_Op (Obj : in New_Interf) is abstract;     -- OK. {7;1}
      function "+" (Obj : in New_Interf)
          return New_Interf is abstract;                     -- OK. {1:7;1}
      procedure Another_Op (Obj : in New_Interf) is abstract;-- ERROR: {7;1}
      function My_Func (Flag : Boolean)
          return New_Interf is abstract;                     -- ERROR: {1:7;1}

      type Private_New_Interf is synchronized interface and Pkg1.Root;
      procedure Another_Op (Obj : in Private_New_Interf)
          is abstract;                                       -- OK. {1:7;1}
      function My_Func (Flag : Boolean) return Private_New_Interf
          is abstract;                                       -- OK. {1:7;1}
   end Pkg2;

   package Pkg3 is
      type Root_3 is task interface;
      function "+" (Op : Root_3) return Root_3 is abstract;  -- OK. {1:7;1}
   private
      function "-" (Op : Root_3) return Root_3 is abstract;  -- ERROR: {7;1}
      procedure An_Op (Op : in out Root_3) is null;          -- OK. {7;1}
      procedure Second_Op (Op : Root_3) is abstract;         -- ERROR: {7;1}
      procedure Third_Op (Code : in Character; Obj : in out Root_3)
           is abstract;                                      -- ERROR: {1:7;1}
   end Pkg3;

   package Pkg4 is
      type New_Interf4 is protected interface and Pkg1.Root;
   private
      procedure My_Op (Obj : in New_Interf4) is abstract;    -- OK. {7;1}
      function "+" (Obj : in New_Interf4)
         return New_Interf4 is abstract;                     -- OK. {1:7;1}
   end Pkg4;

   package Pkg5 is
      type New_Interf5 is limited interface and Pkg1.Root;
      -- Inherits My_Op & "+"
   private
      procedure My_Op (Obj : in New_Interf5) is abstract;    -- OK. {7;1}
      function "+" (Obj : in New_Interf5)
          return New_Interf5 is abstract;                    -- OK. {1:7;1}
      procedure Another_Op (Obj : in New_Interf5) is abstract; -- ERROR: {7;1}
      function My_Func (Obj : New_Interf5)
          return Natural is abstract;                        -- ERROR: {1:7;1}
      procedure Third_Op (Obj : in New_Interf5) is null;     -- OK. {7;1}
   end Pkg5;

   package Pkg6 is
      type Interf6 is limited interface;
      procedure Pub_Op (Pi : in Float; Obj : in out Interf6)
           is abstract;                                      -- OK. {1:7;1}
   private
      procedure Another_Op (Obj : access Interf6) is abstract; -- ERROR: {7;1}
      function My_Func (Obj : access Interf6)
         return Natural is abstract;                         -- ERROR: {1:7;1}
      function Acc_Func (Count : Natural)
         return access Interf6 is abstract;                  -- ERROR: {1:7;1}
      procedure Third_Op (Obj : access Interf6) is null;     -- OK. {7;1}
   end Pkg6;

end B393010;
