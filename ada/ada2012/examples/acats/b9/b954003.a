-- B954003.A
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
--      Check that the accessibility level of the target task object of
--      a requeue_statement is not equal to or statically deeper than any
--      enclosing accept_statement of the task unit.
--      Check that for a requeue statement of an entry_body the target
--      object is either a formal parameter of the entry_body or the
--      accessibility level of the target object is not statically
--      deeper that that of the entry_declaration.
--
-- TEST DESCRIPTION:
--      The note in paragraph 9.5.4(6a);6.0 through 9.5.4(6c);6.0 points
--      out the illegal cases we check for.
--
--
-- CHANGE HISTORY:
--      20 OCT 95   SAIC    ACVC 2.1
--       5 JUL 12   RLB     Fixed to eliminate requeues on constant objects
--                          (illegal by 9.5(7.1/2)).
--
--!


procedure B954003 is

   protected type POT1 is
      entry PE11;
   end POT1;

   protected body POT1 is
      entry PE11 when True is
      begin
         null;
      end PE11;
   end POT1;

   Prot_Global : POT1;

   ---------------

   protected type POT2 is
      entry PE21 (Prot_Parm : in out POT1; Int : Integer);
   private
      Prot_Component : POT1;
   end POT2;

   protected body POT2 is
      entry PE21 (Prot_Parm : in out POT1; Int : Integer) when True is
         Prot_Local : POT1;
      begin
         case Int is
           when 1 => requeue Prot_Global.PE11;                       -- OK.
           when 2 => requeue Prot_Parm.PE11;                         -- OK.
           when 3 => requeue Prot_Component.PE11;                    -- OK.
           when 4 => requeue Prot_Local.PE11;                     -- ERROR:
                    -- The target of the requeue is statically deeper than
                    -- than the entry_declaration.
           when others => null;
         end case;
      end PE21;
   end POT2;

   ----------------

   task ATask is
      entry Entry_1 (Prot_Parm : in out POT1);
      entry Note_8b_Case;
      entry Note_8c_Case (Prot_Parm : in out POT1);
   end ATask;

   task body ATask is
      Prot_Task_Local : POT1;
      Int : Integer;
   begin
      accept Entry_1 (Prot_Parm : in out POT1) do
         case Int is
           when 1 => requeue Prot_Global.PE11;                       -- OK.
           when 2 => requeue Prot_Parm.PE11;                         -- OK.
           when 3 => requeue Prot_Task_Local.PE11;                   -- OK.
           when 4 =>
                     declare
                        Prot_Local : POT1;
                     begin
                        requeue Prot_Local.PE11;                  -- ERROR:
                        -- The target of the requeue is statically deeper
                        -- than the enclosing accept_statement.
                     end;
           when others => null;
         end case;

          -- nested accept
          accept Note_8b_Case do
             requeue Prot_Parm.PE11;                              -- ERROR:
             -- The target of the requeue is not a formal parameter of
             -- this accept statement and it is statically deeper than
             -- some enclosing accept statement.
          end Note_8b_Case;
      end Entry_1;

      accept Note_8c_Case (Prot_Parm : in out POT1) do
         declare
            task Inside_Accept is
               entry IAE;
            end Inside_Accept;
            task body Inside_Accept is
            begin
               accept IAE do
                  requeue Prot_Parm.PE11;                            -- OK.
               end IAE;
            end Inside_Accept;
         begin
            Inside_Accept.IAE;
         end;
      end Note_8c_Case;
   end ATask;

begin
   null;
end B954003;
