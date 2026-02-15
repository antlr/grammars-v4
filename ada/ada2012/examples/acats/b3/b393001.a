-- B393001.A
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
--      Check that:
--      Objects and aggregates may not be defined or allocated of an abstract
--      type.
--      The type of a component may not be abstract.
--      A function defined with an abstract result type must be declared
--      abstract.
--      If an abstract subprogram is defined as a primitive subprogram of a
--      a tagged type, then the tagged type must be abstract.
--      The full type of a non-abstract private extension may not be abstract.
--      The full type of an abstract private extension may be non-abstract.
--
-- TEST DESCRIPTION:
--      This test declares abstract (private) types, abstract (private)
--      extensions, a tagged type and a private extension.  Verify that both
--      basic and aliased object declarations are illegal if the type is an
--      abstract type. Verify that components of an abstract type are illegal
--      for both records and arrays.  Verify that a function defined with an
--      abstract result type is illegal if the function is not abstract.
--      Verify that abstract primitive subprograms declared for non-abstract
--      tagged types are illegal. Verify that it is illegal when a full type
--      of a non-abstract private extension is declared abstract.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      03 Dec 95   SAIC    ACVC 2.0.1 fixes: Moved declaration of function
--                          Return_Vis_Abstract(Abstract_Visible) to before
--                          that of partial view of Abstract_Extension.
--                          Declared overriding Return_Vis_Abstract for full
--                          view of Abstract_Extension. Modified commentary.
--      07 Feb 18   RLB     Added error location indicators to reflect common
--                          error reporting strategies.
--
--!

package B393001 is

   type Private_UnTagged is private;

   type Private_Tagged is private;

   type Surprise is private;

   type Abstract_Private is abstract tagged private;

   type Abstract_Visible is abstract tagged record
      Component : String (1 .. 7);
   end record;

   function Return_Vis_Abstract return Abstract_Visible
      is abstract;                                           -- OK. {1:4;1}

   type Abstract_Ext_In_Full is new              -- A concrete private
     Abstract_Visible with private;              -- extension.

   type Abstract_Extension is abstract           -- An abstract private
     new Abstract_Visible with private;          -- extension.

   type Abstract_Ptr is access Abstract_Visible;

   function Return_Pri_Abstract return Abstract_Private
      is abstract;                                           -- OK. {1:4;1}

   Object1 : Abstract_Visible;                               -- ERROR: {4;1}
                                                 -- Type of object is abstract.
   Object2 : aliased Abstract_Visible;                       -- ERROR: {4;1}
                                                 -- Type of object is abstract.
   Abs_Ptr : Abstract_Ptr := new Abstract_Visible;           -- ERROR: {4;1}
                                       -- Type of allocated object is abstract.

   type Rec1 is record
      Component : Abstract_Visible;                          -- ERROR: {7;1}
   end record;                                -- Type of component is abstract.

   type Rec2 is record
      Component : Abstract_Ptr := new Abstract_Visible;      -- ERROR: {7;1}
   end record;                        -- Trying to allocate an abstract object.

   type Rec3 is abstract tagged record
      Component : Abstract_Private;                          -- ERROR: {7;1}
   end record;                                -- Type of component is abstract.

   type Arr1 is array (1 .. 5) of Abstract_Private;          -- ERROR: {4;1}
                                              -- Type of component is abstract.

   type Arr2 is array (1 .. 5) of Abstract_Ptr;              -- OK. {4;1}

   Object3 : Arr2 := (others => new Abstract_Visible);       -- ERROR: {33;2}
                                      -- Trying to allocate an abstract object.

   function Non_Abstract_Func1 return Abstract_Private;      -- ERROR: {4;1}
                       -- Function is not abstract but result type is abstract.

   function Non_Abstract_Func2 (Left, Right : Integer)
     return Abstract_Private;                                -- ERROR: {1:4;1}
                       -- Function is not abstract but result type is abstract.

   function Abstract_Func1 (Left, Right : Private_UnTagged)
     return Private_UnTagged is abstract;                    -- OK. {1:4;1}

   procedure Abstract_Proc1 (From    : in out Private_UnTagged;
                             Element :    out Boolean)
                                              is abstract;   -- OK. {2:4;1}

   function Abstract_Func2 (Left, Right : Private_Tagged)
     return Private_Tagged is abstract;                      -- ERROR: {1:4;1}
                  -- Primitive abstract subprogram of non-abstract tagged type.

   procedure Abstract_Proc2 (From    : in out Private_Tagged;
                             Element :    out Boolean)
                                                is abstract; -- ERROR: {2:4;1}
                  -- Primitive abstract subprogram of non-abstract tagged type.

private
   type Private_UnTagged is record
      I : Integer;
   end record;

   type Private_Tagged is tagged null record;          -- OPTIONAL ERROR: {4;1}
                       -- Type must be abstract tagged for primitive abstract
                       -- subprograms (Abstract_Func2, Abstract_Proc2).

   type Abstract_Private is abstract tagged null record;

   type Abstract_Ext_In_Full is
     abstract new Abstract_Visible with null record;         -- ERROR: {1:4;1}
                       -- Private extension is not abstract but full type is
                       -- abstract.

   type Abstract_Extension is
     new Abstract_Visible with null record;                  -- OK. {1:4;1}
   -- must not have any abstract dispatching operations!
   function Return_Vis_Abstract return Abstract_Extension;   -- OK. {4;1}

   type Surprise is abstract tagged null record;             -- ERROR: {4;1}
                                               -- Partial view is not abstract.

end B393001;
