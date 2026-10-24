-- B952004.A
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
--      Check that an entry_declaration in a task declaration cannot 
--      contain a specification for an access parameter.
--      Check that an accept_statement is not allowed within an 
--      asynchronous_select inner to the enclosing task_body.
--
-- TEST DESCRIPTION:
--      Test attempts to make an illegal entry_declaration
--      and put an accept_statement inside an accept for the same entry
--      and inside an asynchronous_select.
--
--
-- CHANGE HISTORY:
--      17 OCT 95   SAIC    ACVC 2.1
--       6 MAY 96   SAIC    Incorporated Reviewer comments
--
--!


procedure B952004 is

   type Some_Type is record
         Foo : Integer;
       end record;

   task Bad_Entry is
      entry Naughty (Ok : in Some_Type;
                     Bad : access Some_Type);                     -- ERROR:
                          -- entry_declaration cannot have access parameter
   end Bad_Entry;
   task body Bad_Entry is separate;   -- not provided

   task T2 is
      entry E0;
      entry E1 (X : in Integer);
      entry Family (Character);
   end T2;

   task body T2 is
      L : Integer;
   begin
      accept E0 do
         accept E1 (X : in Integer) do                               -- OK.
           L := X;
         end E1;
      end E0;

      accept E0 do
         accept E0;                                               -- ERROR:
                        -- accept_statement cannot be nested inside another 
                        -- accept_statement for same entry_declaration
      end E0;

      -- nested accepts but with a select in between
      accept E0 do
         select
            accept E1 (X : in Integer) do
                L := X;
            end E1;
         or
            accept E0;                                            -- ERROR:
                        -- accept_statement cannot be nested inside another 
                        -- accept_statement for same entry_declaration
         end select;
      end E0;

      select
         delay 1.0;
      then abort
         accept E0;                                               -- ERROR:
                -- accept_statement cannot be inside an asynchronous_select
      end select;

      -- attempt nested accepts from the same family
      accept Family('A') do
         declare  -- just to add distance between accepts
            Foo : Character;
         begin
            accept Family('B') do                                 -- ERROR:
                   -- nested accepts for the same entry, even if the family
                   -- members are different, is not permitted.
               null;
            end Family;
         end;
      end Family;
   end T2;
begin
   null;
end B952004;
