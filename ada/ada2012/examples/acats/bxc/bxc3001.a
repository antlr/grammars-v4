-- BXC3001.A
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
--      Check that pragmas Interrupt_Handler and Attach_Handler are
--      recognized.  Check that the handler is a parameterless protected
--      procedure; check that the pragmas are allowed only immediately in
--      a protected definition.
--      Check that Attach_Handler will accept an expression only of type
--      Interrupts.Interrupt_ID.
--
-- TEST DESCRIPTION:
--      This test defines protected objects with various interfaces and
--      uses them to determine the correct recognition of pragmas
--      Interrupt_Handler and Attach_Handler.
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for a compiler attempting validation
--      for the Systems Programming Annex.  Note that additional warnings
--      may occur in an implementation where Ada.Interrupts.Interrupt_ID
--      contains only a single value.
--
--      This test is not applicable for an implementation that takes
--      advantage of C.3.1(19) and provides implementations of
--      Attach_Handler and Interrupt_Handler other than those explicitly
--      defined in C.3.1.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      13 Nov 95   SAIC    Split for ACVC 2.1
--
--!

with Ada.Interrupts;
package BXC3001 is
  protected Object_0 is
    procedure Parameterless_Procedure;
    pragma Interrupt_Handler( Parameterless_Procedure );           -- OK
    pragma Attach_Handler( Parameterless_Procedure,
                           Ada.Interrupts.Interrupt_ID'First );    -- OK
  end Object_0;

  protected type Type_0 is
    procedure Parameterless_Procedure;
    pragma Interrupt_Handler( Parameterless_Procedure );           -- OK
    pragma Attach_Handler( Parameterless_Procedure,
                           Ada.Interrupts.Interrupt_ID'Last );     -- OK
  end Type_0;

  Legal_Object : Type_0;                                           -- OK

  protected Object_1 is

    entry Parameterless_Entry;
    pragma Interrupt_Handler( Parameterless_Entry );              -- ERROR:
                                         -- must be parameterless procedure

    pragma Attach_Handler( Parameterless_Entry,
                           Ada.Interrupts.Interrupt_ID'First );   -- ERROR:
                                         -- must be parameterless procedure

    entry Parameter_Entry( Parameter: in out Natural );
    pragma Interrupt_Handler( Parameter_Entry );                  -- ERROR:
                                         -- must be parameterless procedure

    pragma Attach_Handler( Parameter_Entry,
                           Ada.Interrupts.Interrupt_ID'First );   -- ERROR:
                                         -- must be parameterless procedure

    procedure Parameter_Procedure( Parameter: in out Natural);
    pragma Interrupt_Handler( Parameter_Procedure );              -- ERROR:
                                         -- must be parameterless procedure
    pragma Attach_Handler( Parameter_Procedure,
                           Ada.Interrupts.Interrupt_ID'First );   -- ERROR:
                                         -- must be parameterless procedure

    function Parameterless_Function return Natural;
    pragma Interrupt_Handler( Parameterless_Function );           -- ERROR:
                                         -- must be parameterless procedure
    pragma Attach_Handler( Parameterless_Function,
                           Ada.Interrupts.Interrupt_ID'First );   -- ERROR:
                                         -- must be parameterless procedure

    function Parameter_Function( Parameter: Natural ) return Natural;
    pragma Interrupt_Handler( Parameter_Function );               -- ERROR:
                                         -- must be parameterless procedure
    pragma Attach_Handler( Parameter_Function,
                           Ada.Interrupts.Interrupt_ID'First );   -- ERROR:
                                         -- must be parameterless procedure
  end Object_1;

  protected Object_2 is
    procedure Parameterless_Procedure;
  end Object_2;

    pragma Interrupt_Handler( Object_2.Parameterless_Procedure ); -- ERROR:
                                         -- must be within protected object

    pragma Attach_Handler( Object_2.Parameterless_Procedure,
                           Ada.Interrupts.Interrupt_ID'First );   -- ERROR:
                                         -- must be within protected object


  procedure Parameterless_Procedure;
    pragma Interrupt_Handler( Parameterless_Procedure );          -- ERROR:
                                         -- must be within protected object

    pragma Attach_Handler( Parameterless_Procedure,
                           Ada.Interrupts.Interrupt_ID'First );   -- ERROR:
                                         -- must be within protected object

  type Interrupt_ID is new Integer;
  Int_Handler_Var : Interrupt_ID := 1;

  protected Object_3 is
    procedure Parameterless;
      pragma Interrupt_Handler( Parameterless );
      pragma Attach_Handler( Parameterless, Int_Handler_Var );    -- ERROR:
                                    -- must be type Interrupts.Interrupt_ID
  end Object_3;

end BXC3001;
