-- B730005.A
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
-- OBJECTIVE:
--      Check that the ancestor type of a private extension may not be a
--      class-wide type.
--
--         Check for the basic case.
--
--         Check for the generic case, where the ancestor type is the
--         class-wide type of a formal tagged private type or formal
--         private extension.
--
--         Check for the instance case, where the ancestor type is a formal
--         (tagged or untagged) private type or formal private extension, and
--         the corresponding actual type is a class-wide type. Verify this
--         rule in the visible and private part of an instance. In the
--         private part, check specifically for the case where the parents
--         of the partial and full views are different.
--
-- TEST DESCRIPTION:
--      For the basic case, declare a tagged type T. Attempt to declare a
--      private extension with an ancestor type T'Class.
--
--      For the generic case, declare a generic package with a formal tagged
--      private type T1 and a formal private extension T2, and attempt to
--      declare private extensions with ancestor types T1'Class and T2'Class.
--
--      For the visible part of an instance, declare generic packages with
--      formal tagged private types and formal private extensions T, and
--      declare private extensions with ancestor types T. Attempt to
--      instantiate the generics with class-wide actual types. Include a
--      case where the class-wide actual is passed through to a nested
--      instantiation.
--
--      For the private part of an instance, declare a generic package as
--      follows, and attempt to instantiate with a specific actual
--      corresponding to T and a class-wide actual corresponding to DT:
--
--         generic
--            type T  (<>) is tagged private;
--            type DT (<>) is new T with private;
--         package GP is
--            type PE is new T with private;
--         private
--            type PE is new DT with ...
--         end GP;
--
--
-- CHANGE HISTORY:
--      27 Feb 95   SAIC    Initial prerelease version.
--      08 May 95   SAIC    Added nested instantiation case.
--
--!

package B730005 is

   type Parent is tagged null record;

--
-- Basic cases:
--

   type PrivExt is new Parent'Class with private;                     -- ERROR:
                             -- Ancestor of private extension must be specific.


--
-- Generic cases:
--

   generic
      type FP is tagged private;
      type FD is new Parent with private;
   package GP_Errors is
      type PE_FP is new FP'Class with private;                        -- ERROR:
                             -- Ancestor of private extension must be specific.

      type PE_FD is new FD'Class with private;                        -- ERROR:
                             -- Ancestor of private extension must be specific.
   private
      type PE_FP is new FP with null record;                 -- OPTIONAL ERROR:
      type PE_FD is new FD with null record;                 -- OPTIONAL ERROR:
   end GP_Errors;


--
-- Visible part of instance:
--

   generic
      type FP (<>) is tagged private;
   package Vis_Priv is
      type Der is new FP with private;
   private
      type Der is new FP with null record;
   end Vis_Priv;

   generic
      type FD (<>) is new Parent with private;
   package Vis_Der is
      type Der is new FD with private;
   private
      type Der is new FD with null record;
   end Vis_Der;

   generic
      type FP (<>) is tagged private;
   package Wrapper is
      package Nested is new Vis_Priv(FP);
   end Wrapper;


   package Vis_Priv_Instance is new Vis_Priv (Parent'Class);          -- ERROR:
                             -- Ancestor of private extension must be specific.

   package Vis_Der_Instance is new Vis_Der (Parent'Class);            -- ERROR:
                             -- Ancestor of private extension must be specific.

   package Wrapper_Instance is new Wrapper (Parent'Class);            -- ERROR:
                             -- Ancestor of private extension must be specific.


--
-- Private part of instance:
--

   generic
      type T  (<>) is tagged private;
      type DT (<>) is new T with private;
   package Pri_Gen is
      type PE is new T with private;
   private
      type PE is new DT with null record;
   end Pri_Gen;

   package Pri_Instance_OK is new Pri_Gen (Parent, Parent'Class);     -- ERROR:
                  -- Parent of full view of private extension must be specific.


private
   type PrivExt is new Parent with null record;              -- OPTIONAL ERROR:
end B730005;
