-- B3A0002.A
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
--      Check that subtype conformance is required for actual values of access
--      to subprogram types.  Check that the mode, number and subtype of
--      parameters must statically match.  Check that the calling convention
--      of the value must not be Intrinsic.  Check that corresponding subtypes
--      of the profiles must statically match.
--
--      Check that a generic formal subprogram may not be the actual value of
--      an access to subprogram type because it cannot subtype-conform with
--      anything.
--
-- TEST DESCRIPTION:
--      This test defines several subprograms of similar yet distinct
--      profiles.  Attempts are made to incorrectly designate these
--      subprograms using access to subprogram types.
--
--
-- CHANGE HISTORY:
--      08 JUN 95   SAIC    Initial version
--      20 FEB 96   SAIC    Added case for 2.1
--
--!

------------------------------------------------------------------- B3A0002

procedure B3A0002 is

  type Access_Integer is access Integer;

  type Access_Parameterless_Procedure is
       access procedure;

  type Access_Protected_Procedure is
       access protected procedure;

  type Access_Proc_W_In_Parameter is
       access procedure(A: in Integer);

  type Access_Proc_W_Out_Parameter is
       access procedure(B: out Integer);

  type Access_Proc_W_InOut_Parameter is
       access procedure(C: in out Integer);

  type Access_Proc_W_Access_Parameter is
       access procedure(D: access Integer);

  type Access_Proc_W_Access_T_Parameter is
       access procedure(D: Access_Integer);

  type Access_Integer_Function is
       access function return Integer;

  type Access_Func_W_Parameter is
       access function(E: in Integer) return Integer;

  procedure Parameterless_Procedure is
    begin null; end;

  procedure Proc_W_In_Integer(F: in Integer) is
    begin null; end;

  procedure Proc_W_Def_Integer(G: Integer) is
    begin null; end;

  procedure Proc_W_Def_In_Integer(Val: in Integer := 0) is
    begin null; end;

  procedure Proc_W_Out_Integer(H: out Integer) is
    begin null; end;

  procedure Proc_W_InOut_Integer(I: in out Integer) is
    begin null; end;

  procedure Proc_W_Access_Integer(Val: access Integer) is
    begin null; end;

  procedure Proc_Int_Acc(Parm: Access_Integer) is
    begin null; end;

  function Integer_Function return Integer is begin
    return 0; end;

  function Integer_Pred(J: in Integer) return Integer
    renames Integer'Pred;

  function Func_W_Integer(K: in Integer) return Integer is
    begin return 0; end;

  procedure Proc_W_In_Float(L: in Float) is
    begin null; end;

  procedure Proc_W_Out_Float(M: out Float) is
    begin null; end;

  procedure Proc_W_InOut_Float(N: in out Float) is
    begin null; end;

  procedure Proc_W_Access_Float(Val: access Float) is
    begin null; end;

  function Float_Function return Float is
    begin return 0.0; end;

  function Func_W_Float(O: in Float) return Float is
    begin return 0.0; end;

  protected Prot_Obj is
    procedure Proc;
    function Func return Integer;
  end Prot_Obj;

  protected body Prot_Obj is
    procedure Proc is begin null; end;
    function Func return Integer is begin return 0; end;
 end;

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  A_Parameterless_Procedure: Access_Parameterless_Procedure;

  A_Protected_Procedure    : Access_Protected_Procedure;

  A_Proc_W_In_Parameter    : Access_Proc_W_In_Parameter;

  A_Proc_W_Out_Parameter   : Access_Proc_W_Out_Parameter;

  A_Proc_W_InOut_Parameter : Access_Proc_W_InOut_Parameter;

  A_Proc_W_Access_Parameter: Access_Proc_W_Access_Parameter;

  A_Integer_Function       : Access_Integer_Function;

  A_Func_W_Parameter       : Access_Func_W_Parameter;

  generic
    with procedure P;
  procedure Gen_Proc;

  procedure Gen_Proc is
    Gen_Local_Access_Proc : Access_Parameterless_Procedure;
  begin
    Gen_Local_Access_Proc := P'Access;                               -- ERROR:
                        -- generic formal subprogram is not subtype conformant
  end Gen_Proc;

begin  -- Main test procedure.

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- The positive tests

  A_Parameterless_Procedure := Parameterless_Procedure'Access;        -- OK

  A_Protected_Procedure     := Prot_Obj.Proc'Access;                  -- OK

  A_Proc_W_In_Parameter     := Proc_W_In_Integer'Access;              -- OK

  A_Proc_W_In_Parameter     := Proc_W_Def_Integer'Access;             -- OK

  A_Proc_W_Out_Parameter    := Proc_W_Out_Integer'Access;             -- OK

  A_Proc_W_InOut_Parameter  := Proc_W_InOut_Integer'Access;           -- OK

  A_Proc_W_Access_Parameter := Proc_W_Access_Integer'Access;          -- OK

  A_Integer_Function        := Integer_Function'Access;               -- OK

  A_Func_W_Parameter        := Func_W_Integer'Access;                 -- OK

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- A sampling of possible errors.

  A_Parameterless_Procedure := Proc_W_In_Integer'Access;             -- ERROR:
                                  -- parameter and result profile do not match

  A_Parameterless_Procedure := Integer_Function'Access;              -- ERROR:
                                  -- parameter and result profile do not match

  A_Parameterless_Procedure := Prot_Obj.Proc'Access;                 -- ERROR:
                                              -- different calling conventions

  A_Parameterless_Procedure := Prot_Obj.Func'Access;                 -- ERROR:
                                  -- parameter and result profile do not match
                                           -- calling conventions do not match

  A_Parameterless_Procedure := Proc_W_Def_In_Integer'Access;         -- ERROR:
                                                 -- wrong number of parameters

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  A_Proc_W_In_Parameter := Parameterless_Procedure'Access;           -- ERROR:
                                      -- designated procedure is parameterless


  A_Proc_W_In_Parameter := Proc_W_Out_Integer'Access;                -- ERROR:
                                              -- parameter mode does not match

  A_Proc_W_In_Parameter := Func_W_Integer'Access;                    -- ERROR:
                                                          -- must be procedure

  A_Proc_W_In_Parameter := Proc_W_In_Float'Access;                   -- ERROR:
                                              -- parameter type does not match

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  A_Proc_W_Out_Parameter := Parameterless_Procedure'Access;          -- ERROR:
                                           -- parameter profile does not match

  A_Proc_W_Out_Parameter := Proc_W_InOut_Integer'Access;             -- ERROR:
                                              -- parameter mode does not match

  A_Proc_W_Out_Parameter := Integer_Function'Access;                 -- ERROR:
                                                            -- not a procedure

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  A_Proc_W_InOut_Parameter := Proc_W_In_Integer'Access;              -- ERROR:
                                              -- parameter mode does not match

  A_Proc_W_Access_Parameter := Proc_W_Access_Float'Access;           -- ERROR:
                                          -- designated subtype does not match

  A_Proc_W_Access_Parameter := Proc_Int_Acc'Access;                  -- ERROR:
                                          -- designated subtype does not match

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  A_Integer_Function := Parameterless_Procedure'Access;              -- ERROR:
                                                             -- not a function

  A_Integer_Function := Proc_W_Out_Integer'Access;                   -- ERROR:
                                                             -- not a function

  A_Integer_Function := Func_W_Integer'Access;                       -- ERROR:
                                             -- parameter profile do not match

  A_Integer_Function := Float_Function'Access;                       -- ERROR:
                                             -- result type does not match

  A_Integer_Function := Prot_Obj.Func'Access;                        -- ERROR:
                                              -- different calling conventions

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  A_Func_W_Parameter := Integer_Pred'Access;                         -- ERROR:
                                            -- may not be convention Intrinsic

end B3A0002;
