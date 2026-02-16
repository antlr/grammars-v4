-- B954004.A
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
--      Check that a requeue_statement is only allowed directly within
--      an entry_body or accept_statement.
--
-- TEST DESCRIPTION:
--      This test declares a protected object and a task.  Requeue
--      statements are placed in both legal and illegal locations.
--
--
-- CHANGE HISTORY:
--       9 Apr 96   SAIC    Initial release for ACVC 2.1
--       8 May 96   SAIC    Incorporated Reviewer comments.
--
--!


procedure B954004 is

   -- This is the object that all the requeues reference.

   protected type POT1 is
      entry Entry_1;
   end POT1;

   protected body POT1 is
      entry Entry_1 when True is
      begin
         null;
      end Entry_1;
   end POT1;

   Prot_Global : POT1;

   ---------------

   protected type POT2 is
      entry Entry_2 (Int : Integer);
      procedure Proc; 
      function Func return Integer;
   end POT2;

   protected body POT2 is
      entry Entry_2 (Int : Integer) when True is
      begin
         requeue Prot_Global.Entry_1;                          -- OK.

         if Int = 3 then
            requeue Prot_Global.Entry_1;                       -- OK.
         end if;

         declare
            X : Integer;
         begin
            requeue Prot_Global.Entry_1;                       -- OK.
         end;

         declare
            procedure Nested_Callable_Construct is
            begin
               requeue Prot_Global.Entry_1;                    -- ERROR:
                     -- entry body must be innermost callable construct.
            end Nested_Callable_Construct;
         begin
            Nested_Callable_Construct;
         end;

         declare
            package Enclosing_Body is
               procedure P;
            end Enclosing_Body;
            package body Enclosing_Body is
               procedure P is
               begin null; end P;
            begin  -- package init
               requeue Prot_Global.Entry_1;                    -- ERROR:
                     -- entry body must be innermost enclosing body.
            end Enclosing_Body;
         begin
            Enclosing_Body.P;
         end;
      end Entry_2;

      procedure Proc is
      begin
         requeue Prot_Global.Entry_1;                          -- ERROR:
                                        -- not within an entry or accept
      end Proc;

      function Func return Integer is
      begin
         requeue Prot_Global.Entry_1;                          -- ERROR:
                                        -- not within an entry or accept
         return 3;
      end Func;
   end POT2;

   ----------------

   task ATask is
      entry Entry_3 (Int : Integer);
   end ATask;

   task body ATask is
   begin

      accept Entry_3 (Int : Integer) do
         requeue Prot_Global.Entry_1;                          -- OK.

         if Int = 3 then
            requeue Prot_Global.Entry_1;                       -- OK.
         end if;

         declare
            X : Integer;
         begin
            requeue Prot_Global.Entry_1;                       -- OK.
         end;

         declare
            procedure Nested_Callable_Construct is
            begin
               requeue Prot_Global.Entry_1;                    -- ERROR:
                     -- entry body must be innermost callable construct.
            end Nested_Callable_Construct;
         begin
            Nested_Callable_Construct;
         end;

         declare
            package Enclosing_Body is
               procedure P;
            end Enclosing_Body;
            package body Enclosing_Body is
               procedure P is
               begin null; end P;
            begin  -- package init
               requeue Prot_Global.Entry_1;                    -- ERROR:
                     -- entry body must be innermost enclosing body.
            end Enclosing_Body;
         begin
            Enclosing_Body.P;
         end;
      end Entry_3;

      requeue Prot_Global.Entry_1;                             -- ERROR:
                                        -- not within an entry or accept
   end ATask;

begin
   requeue Prot_Global.Entry_1;                                -- ERROR:
                                        -- not within an entry or accept
end B954004;
