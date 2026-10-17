-- B940005.A
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
--      Check the visibility of local subprograms and the private parts
--      of protected objects
--
-- TEST DESCRIPTION: 
--      In the first block: attempt to access an element in the
--      private part of a protected object, then call a procedure that
--      is local to the body of the object.  In the second block:
--      duplicate the names of subprograms and entries in the main
--      part of the specification and in the private part.  In the
--      third block: duplicate the bodies simulating the erroneous 
--      declaration of local routines with the same names.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


procedure B940005 is

begin

   --================================================================
 
   -- Visibility of private elements and local procedures

   declare -- block 1

      protected Object is
         function Visible_Function return Boolean;
      private
         Non_Visible_Element : integer;
      end Object;

      -----------

      protected body Object is

            procedure Local_Procedure is
            begin
               null;
            end Local_Procedure;

            -- This is a local procedure with the same name as the function
            procedure Visible_Function is
            begin
               null;
            end Visible_Function;

         function Visible_Function return Boolean is
         begin
            return true;
         end Visible_Function;

      end Object;


   begin

      if Object.Visible_Function then      -- O.K.
         null; 
      end if;

      Object.Non_Visible_Element := 1;                               -- ERROR:
                                                            -- Private element
      Object.Local_Procedure;                                        -- ERROR:
                                                      -- Not in Object's Spec.

   end;  -- declare  block 1
     
   --================================================================
   declare -- block 2

      -- Collision between the parts

      protected Object is
         function Some_Function return Boolean;
         procedure Some_Procedure;
         entry Some_Entry;
      private
         function Some_Function return Boolean;                      -- ERROR:
                                                       -- duplicate definition
         procedure Some_Procedure;                                   -- ERROR:
                                                       -- duplicate definition
         entry Some_Entry;                                           -- ERROR:
                                                       -- duplicate definition

      end Object;

      -----------

      protected body Object is

         -- define one set of bodies for the duplicates

         function Some_Function return Boolean is                    
         begin                                               
            return true;
         end Some_Function;

         procedure Some_Procedure is                                 
         begin                                               
            null;
         end Some_Procedure;

         entry Some_Entry when true is                               
         begin                                               
            null;
         end Some_Entry;

      end Object;

   begin
      null;
   end; -- declare block 2

   --================================================================
   declare -- block 3

      -- Duplicate bodies

      protected Object is
         function Some_Function return Boolean;
         procedure Some_Procedure;
         entry Some_Entry;
      private

         -- A procedure with the same name as a function
         procedure Some_Function (Param : natural);       -- OK 

         procedure Private_Proc; 

      end Object;

      -----------

      protected body Object is

                 -- Simulate the declaration of local routines which,
                 -- erroneously, have the same names as the visible ones
                 --
                 function Some_Function return Boolean is
                 begin                                
                    return true;
                 end Some_Function;

                 procedure Some_Procedure is    
                 begin                       
                    null;
                 end Some_Procedure;

                 entry Some_Entry when true is  
                 begin                       
                    null;
                 end Some_Entry;

                 procedure Private_Proc is
                 begin
                    null;
                 end Private_Proc;

         -- Because of the local declarations these will be in error
         function Some_Function return Boolean is                    -- ERROR:
         begin                                               -- duplicate name
            return true;
         end Some_Function;

         procedure Some_Procedure is                                 -- ERROR:
         begin                                               -- duplicate name
            null;
         end Some_Procedure;

         entry Some_Entry when true is                               -- ERROR:
         begin                                               -- duplicate name
            null;
         end Some_Entry;

         procedure Private_Proc is                                   -- ERROR:
         begin                                               -- duplicate name
            null;
         end Private_Proc;

         -- A procedure with the same name as a function
         procedure Some_Function (Param : natural) is
         begin
            null;
         end Some_Function;

      end Object;


   begin
     null;
   end; -- declare block 2

end B940005;
