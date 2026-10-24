-- B954001.A
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
--      Check for error if requeue is not type conformant with the call or
--      if requeue has parameters. Check requeues with/without abort.
--
-- TEST DESCRIPTION:
--      Code two versions of the "Distributor" task.  In one,
--      requeue using the same format as a normal call (with parameter).
--      In the other, requeue to an accept which has different parameter
--      profile than the current call.  Repeat the above with two more 
--      versions of the "Distributor" but use requeue_with_abort
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      08 May 96   SAIC    ACVC 2.1  Added new cases
--
--!

procedure B954001 is

   type Transaction_Record;
   type acc_Transaction_Record is access Transaction_Record;
   type Transaction_Record is 
      record
         ID               : integer := 0;
         Account_Number   : integer := 0;
      end record;


   task Distributor_1 is
      entry Input (Transaction : acc_Transaction_Record);
   end Distributor_1;
   
   task Distributor_2 is
      entry Input (Transaction : acc_Transaction_Record);
   end Distributor_2;

   task Distributor_3 is
      entry Input (Transaction : acc_Transaction_Record);
   end Distributor_3;

   task Distributor_4 is
      entry Input (Transaction : acc_Transaction_Record);
   end Distributor_4;
   
   task Debit_Computation is
      entry Input (Transaction : acc_Transaction_Record);
   end Debit_Computation;
   
   task Credit_Computation is
      entry Input (Account_Number : Integer);
   end Credit_Computation;
   

   task Error_Router is
      entry Unknown_Error;   -- no parameters
      entry Input_Error (Transaction : acc_Transaction_Record);
   end Error_Router;


   task body Distributor_1 is
   -- requeue with parameters
   begin 
      accept Input (Transaction : acc_Transaction_Record) do
         -- Very likely error: use requeue in the same way as we
         -- would a normal call (the same parameters as incoming)
         requeue Debit_Computation.Input (Transaction);              -- ERROR:
                                                             -- has parameters
      end Input;
   end Distributor_1;

   task body Distributor_2 is
   -- requeue type not conformant
   begin 
      accept Input (Transaction : acc_Transaction_Record) do
         -- The current call is for access type, 
         -- target accept is integer
         requeue Credit_Computation.Input;                           -- ERROR:
                                                        -- type not conformant
      end Input;
   end Distributor_2;

   task body Distributor_3 is
   -- requeue (with abort) with parameters 
   begin 
      accept Input (Transaction : acc_Transaction_Record) do
         -- Very likely error: use requeue in the same way as we
         -- would a normal call (the same parameters as incoming)
         requeue Debit_Computation.Input (Transaction) with abort;   -- ERROR:
                                                             -- has parameters
      end Input;
   end Distributor_3;

   task body Distributor_4 is
   -- requeue (with abort) type not conformant
   begin 
      accept Input (Transaction : acc_Transaction_Record) do
         -- The current call is for access type, target
         -- accept is integer
         requeue Credit_Computation.Input with abort;                -- ERROR:
                                                        -- type not conformant
      end Input;
   end Distributor_4;

   task body Debit_Computation is
   begin 
      accept Input (Transaction : acc_Transaction_Record) do
         null;
      end Input;
   end Debit_Computation;

   task body Credit_Computation is
   begin 
      accept Input (Account_Number : Integer) do
         null;
      end Input;
   end Credit_Computation;


   task body Error_Router is
   begin
      select
        accept Unknown_Error do
           -- a reasonable, but wrong, assumption is that we could requeue
           -- back to where we came from.  The fact that this entry does 
           -- not have any parameters prevents us from going back to an
           -- entry with parameters.
           requeue Input_Error;                                   -- ERROR:
                                  -- target entry is not subtype conformant
        end Unknown_Error;
      or
        accept Input_Error (Transaction : acc_Transaction_Record) do
           requeue Unknown_Error;                                    -- OK.
        end Input_Error;
      end select;
   end Error_Router;


begin
   null;

end B954001;
