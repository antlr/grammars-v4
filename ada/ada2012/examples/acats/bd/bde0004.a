-- BDE0004.A
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
-- TEST OBJECTIVE:
--      Check that the explicit declaration of a primitive subprogram of a
--      tagged type must occur before the type is frozen.  Check for cases
--      where the tagged type is frozen by the occurrence of a generic
--      instantiation.
--
--      Check that the tagged type is not frozen by a nonstatic expression 
--      that is a default name.
--
-- TESTS DESCRIPTION:
--      The test declares tagged types, formal private and derived types and 
--      primitive operations for these types.  The test verifies that explicit
--      primitive subprogram declaration is illegal after the tagged type is 
--      frozen, and legal otherwise.
--
--
-- CHANGE HISTORY:
--      04 Mar 95   SAIC    Initial prerelease version.
--      07 Jun 95   SAIC    Modified test description per reviewers.  Moved
--                          instantiations of the private types into the 
--                          private part.  
--
--!


package BDE0004 is

   generic
      type GTag is tagged private;
   package GPk is end;

   type Tag_Type is tagged record
      I : Integer;
   end record;

   procedure Op1 (P : Tag_Type);                                      -- OK.
   package IPk is new GPk (Tag_Type);   

   -- The instantiation IPk of GPk above freezes Tag_Type. The declaration of
   -- Op2 is therefore illegal. 

   procedure Op2 (P : Tag_Type);                                      -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   subtype Index is Natural range 1 .. 10;
   type Tag_T1 is tagged record
      I : Integer;
   end record;

   generic
      type GArr_Of_Tag_Type is array (Index) of Tag_T1;
   package GPk1 is end;

   type Arr_Of_Tag_T1 is array (Index) of Tag_T1;
   procedure Op3 (P : Tag_T1);                                        -- OK.
   package IPk1 is new GPk1 (Arr_Of_Tag_T1); 

   -- The instantiation IPk1 of GPk1 above freezes Arr_Of_Tag_T1, which in 
   -- turn freezes Tag_T1. The declaration of Op4 is therefore illegal.

   procedure Op4 (P : Tag_T1);                                        -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   type Tag_T2 is tagged record
      I : Integer;
   end record;
   generic
      type GTag_T2 is new Tag_T2 with private;
   package GPk2 is end;

   type New_T2 is new Tag_T2 with private;
   procedure Op5 (P : New_T2);                                        -- OK.

   type Tag_T3 is tagged private;
   procedure Op8 (P : Tag_T3); 
   generic
     with procedure FP (X : Tag_T3);
   package GPk3 is end;
 
   generic
      type Tag_T4 (<>) is tagged private;
   package GPk4 is end;

   type ITag_T4 is tagged null record;
   package IPk4 is new GPk4 (ITag_T4'Class);   

   -- The instantiation IPk4 of GPk4 above freezes ITag_T4'Class, which in 
   -- turn freezes ITag_T4.  The declaration of Op10 is therefore illegal. 

   procedure Op10 (P : ITag_T4);                                      -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   type Tag_T5 is tagged record
      I : Integer := 0;
   end record;
   procedure AProc (P : Tag_T5'Class);

   generic
      with procedure FProc (P : Tag_T5'Class);
   package GPk5 is end;

   package IPk5 is new GPk5 (FProc => AProc);   

   -- The instantiation IPk5 of GPk5 above freezes Tag_T5'Class, and thus 
   -- Tag_T5. The declaration of Op11 is therefore illegal. 

   procedure Op11(P : Tag_T5);                                        -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   type Tag_T6 is tagged record
      I : Integer := 0;
   end record;
   procedure Op12 (P : Tag_T6);

   generic
      with procedure GOp (P : Tag_T6) is Op12;
   package GPk6 is end;

   -- The default name Op12 does not freeze Tag_T6. The declaration of Func6 is
   -- therefore legal.

   function Func6 return Tag_T6;                                      -- OK.
   package IPk6 is new GPk6;          -- no actual         

   -- The instantiation Ipk6 of GPk6 above uses default name Op12, so it 
   -- freezes Op12, which in turn freezes Tag_T6. The declaration of Op13 is 
   -- therefore illegal.

   procedure Op13 (P : Tag_T6);                                       -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

private
   type New_T2 is new Tag_T2 with null record;
   package IPk2 is new GPk2 (New_T2);   

   -- The instantiation IPk2 of GPk2 above freezes New_T2, which in turn 
   -- freezes Tag_T2. The declaration of Op6 and Op7 are therefore illegal. 

   procedure Op6 (P : New_T2);                                        -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   procedure Op7 (P : Tag_T2);                                        -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   type Tag_T3 is tagged null record;

   package IPk3 is new GPk3 (Op8);  

   -- The instantiation IPk3 of GPk3 above freezes Op8 which in turn 
   -- freezes Tag_T3.  The declaration of Op9 is therefore illegal. 

   procedure Op9 (P : Tag_T3);                                        -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

end BDE0004;
