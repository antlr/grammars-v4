-- CXDB004.A
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
--      Check that if a calling task is Held while waiting for a rendezvous
--      to complete the active priority of the receiver is unaffected.
--
-- TEST DESCRIPTION:
--      The Caller task calls the entry Long_Rendezvous in the Receiver.
--      Internal to the rendezvous there is another entry (Handshake) and
--      the rendezvous waits there.  While at that point the test driver
--      Holds the Caller then calls the Handshake entry thus allowing the
--      rendezvous to complete since its priority should not be affected
--      by the Hold.  The Receiver should go all the way through to
--      termination.  The Caller should not progress after the rendezvous.
--      The driver then Continues the Caller which should now progress
--      accordingly.  The driver checks for timely passage through
--      rendezvous completion, progression of the Receiver and proper
--      progression/non-progression of the Caller by means of flags in
--      the Protected Object TC_PO.
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable to implementations supporting:
--         the Real-Time Annex and
--         the package Asynchronous_Task_Control.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      09 Nov 95   SAIC    ACVC 2.0.1 - added applicability criteria
--      03 Feb 17   RLB     Added missing N/A error tag.
--
--!


with Report;
with ImpDef;
with Ada.Task_Identification;
with Ada.Asynchronous_Task_Control;         -- N/A => ERROR. {1}


procedure CXDB004 is

   package AATC renames Ada.Asynchronous_Task_Control;
   package ATI  renames Ada.Task_Identification;

begin

   Report.Test ("CXDB004", "Asynchronous Task Control. " &
                                          "In rendezvous ,caller is Held");

   declare  -- encapsulate the test

      Caller_Id : ATI.Task_Id;

      protected TC_PO is
         function  In_Rendezvous return Boolean;
         function  Receiver_Complete return Boolean;
         function  Caller_Progressing return Boolean;
         procedure Set_In_Rendezvous;
         procedure Set_Receiver_Complete;
         procedure Set_Caller_Progressing;
      private
         In_Rendezvous_Flag     : Boolean := false;
         Receiver_Complete_Flag : Boolean := false;
         Caller_Progress_Flag   : Boolean := false;
      end TC_PO;

      protected body TC_PO is
         function  In_Rendezvous return Boolean is
         begin
            return In_Rendezvous_Flag;
         end  In_Rendezvous;

         function  Receiver_Complete return Boolean is
         begin
            return Receiver_Complete_Flag;
         end Receiver_Complete;

         function Caller_Progressing return Boolean is
         begin
            return Caller_Progress_Flag;
         end Caller_Progressing;

         procedure Set_In_Rendezvous is
         begin
            In_Rendezvous_Flag := true;
         end Set_In_Rendezvous;

         procedure Set_Receiver_Complete is
         begin
            Receiver_Complete_Flag := true;
         end Set_Receiver_Complete;

         procedure Set_Caller_Progressing is
         begin
            Caller_Progress_Flag := true;
         end Set_Caller_Progressing;

      end TC_PO;

      --===================================

      task Receiver is
         entry Long_Rendezvous;
         entry Handshake;
      end Receiver;

      task body Receiver is
      begin

         accept Long_Rendezvous do

            TC_PO.Set_In_Rendezvous;
            -- This accept is here for handshaking only.  Once we reach
            -- this point we know the caller is waiting within the rendezvous.
            -- We arrange to Hold the caller.
            accept Handshake;
         end Long_Rendezvous;

         -- Set flag to indicate that the Receiver is progressing
         TC_PO.Set_Receiver_Complete;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Receiver Task");
      end Receiver;


      task Caller is
         entry Get_Id(Task_Id : out ATI.Task_Id);
      end Caller;

      task body Caller is
      begin
         accept Get_Id (Task_Id : out ATI.Task_Id)  do
            -- Get the system assigned Task_Id for this task and
            -- "return" it to the caller
            Task_Id := ATI.Current_Task;
         end Get_Id;

         -- Call the receiver.  Once in the rendezvous both tasks
         -- will wait.
         Receiver.Long_Rendezvous;

         -- When we first get back from the rendezvous the Caller should
         -- be held and thus we should not execute the following.  Only
         -- when the Caller is Continued should we execute it.
         TC_PO.Set_Caller_Progressing;

      exception
         when others =>
            Report.Failed ("Unexpected Exception in Caller Task");
      end Caller;

   begin    -- encapsulation

      -- Get Caller's ID for the Hold/Continue operations
      Caller.Get_Id ( Caller_Id );

      -- when Caller has reached the handshake, Hold Caller
      while not TC_PO.In_Rendezvous loop
         delay ImpDef.Minimum_Task_Switch;
      end loop;
      --
      AATC.Hold ( Caller_Id);

      -- We have Held the caller while it was in the rendezvous. We
      -- now call the handshake entry. The Hold operation should not affect
      -- the priority of the Receiver so the rendezvous should complete.
      Receiver.Handshake;

      --
      -- The test will hang here if the Receiver does not complete the
      -- rendezvous
      while not TC_PO.Receiver_Complete loop
         delay ImpDef.Minimum_Task_Switch;
      end loop;

      -- Prove that the Caller does not progress after the rendezvous
      -- Delay enough to allow the Caller to (erroneously) progress
      --
      delay 3*ImpDef.Minimum_Task_Switch;
      --
      if TC_PO.Caller_Progressing then
         Report.Failed ("Held task progressed after rendezvous");
      end if;

      -- Continue the Caller and prove that it goes.
      AATC.Continue ( Caller_Id);
      --
      --  The test will hang here if the caller does not Continue
      while not TC_PO.Caller_Progressing loop
         delay ImpDef.Minimum_Task_Switch;
      end loop;


   end;     -- encapsulation

   Report.Result;

end CXDB004;
