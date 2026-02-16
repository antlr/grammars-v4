-- BDE0003.A
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
--      Check that the explicit declaration of a primitive subprogram of a
--      tagged type must occur before the type is frozen.  Check for cases
--      where the tagged type is frozen by the completion of a deferred 
--      constant declaration.  Check also that the deferred constant 
--      declaration itself does not freeze the type.
--
--      Check that a deferred constant is completed before the constant is
--      frozen.
--
-- TEST DESCRIPTION:
--
--      The test declares tagged types, deferred constants, and primitive 
--      operations for these types.  The test verifies that explicit primitive
--      subprogram declaration is illegal after the tagged type is frozen, and
--      legal otherwise.
--
--
-- CHANGE HISTORY:
--      31 Mar 95   SAIC    Initial prerelease version.
--      07 Jun 95   SAIC    Modified test description per reviewers.
--
--!

package BDE0003 is
   type Tag_T1 is tagged record
      I : Integer := 0;
   end record;

   ObjA : constant Tag_T1;                  

   -- The declaration of ObjA above is a deferred constant, and does
   -- not freeze the parent type Tag_T1. The declaration of Op is
   -- therefore legal.

   procedure Op1 (P : Tag_T1);                                        -- OK.

   type Tag_T2 is tagged record
      B : Boolean := True;
   end record;
   type Arr_Of_Tag_T2 is array (1 .. 5) of Tag_T2;
   type Rec_W_Arr_Of_Tag_T2 is tagged record
      C : Arr_Of_Tag_T2;
   end record;

   ObjB : constant Rec_W_Arr_Of_Tag_T2;    

   -- The declaration of ObjB above is a deferred constant, and does not
   -- freeze either Rec_W_Arr_Of_Tag_T2, Arr_Of_Tag_T2, or Tag_T2. The 
   -- declarations of Op2 and Op3 are therefore legal.

   procedure Op2 (P : access Rec_W_Arr_Of_Tag_T2);                    -- OK.
   procedure Op3 (P : Tag_T2);                                        -- OK.

   type Tag_T3 is tagged record
      I : Integer := 0;
   end record;
   type Rec_W_Tag_T3 is record
     C : Tag_T3;
   end record;
   type Arr_Of_Rec_W_Tag_T3 is array (1 .. 5) of Rec_W_Tag_T3;

   ObjC : constant Arr_Of_Rec_W_Tag_T3;   

   -- The declaration of ObjC above is a deferred constant, and does not 
   -- freeze either Rec_W_Tag_T3, Arr_Of_Rec_W_Tag_T3, or Tag_T3. The 
   -- declarations of Op4, Op5 and Op6 are therefore legal.

   procedure Op4 (P : Arr_Of_Rec_W_Tag_T3);                           -- OK.
   procedure Op5 (P : Rec_W_Tag_T3);                                  -- OK.
   procedure Op6 (P : access Tag_T3);                                 -- OK.

   type Rec is record            -- not tagged
      I : Integer := 0;
   end record;
   ObjD : constant Rec;

   -- The declaration of ObjE freezes ObjD before the completion.

   ObjE : Rec := ObjD;                                                -- ERROR:
                                          -- ObjD was frozen before completion.

   type Button is (On, Off);
   ObjF : constant Button;
   generic
      GObj : Button;                       
   package GPk is end;

   -- The instantiation Ipk of GPk freezes ObjF before the completion.

   package IPk is new GPk (ObjF);                                     -- ERROR:
                                          -- ObjF was frozen before completion.

private
   ObjA : constant Tag_T1 := (I => 10);  

   -- The completion of ObjA above freezes Tag_T1. The declaration of Func1
   -- is therefore illegal. 

   function Func1 return Tag_T1;                                      -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   ObjB : constant Rec_W_Arr_Of_Tag_T2 := (C => (1..5 => (B => False)));

   -- The completion of ObjB above freezes Rec_W_Arr_Of_Tag_T2, which in turn 
   -- freezes Rec_W_Arr_Of_Tag_T2, and Tag_T2. The declarations of Op10 and 
   -- Op11 are therefore illegal. 

   procedure Op10 (P : access Rec_W_Arr_Of_Tag_T2);                   -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   procedure Op11 (P : Tag_T2);                                       -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   ObjC : constant Arr_Of_Rec_W_Tag_T3 := (1..5 => (C => (I => 23)));  

   -- The completion of ObjC above freezes Arr_Of_Rec_W_Tag_T3, which in turn 
   -- freezes Rec_W_Tag_T3, and Tag_T3.  Therefore, the declaration of Op12 is 
   -- illegal.  

   procedure Op12 (P : access Tag_T3);                                -- ERROR:
                              -- Primitive declaration of a frozen tagged type.

   ObjD : constant Rec    := (I => 59);                                  
   ObjF : constant Button := On;

end BDE0003;
