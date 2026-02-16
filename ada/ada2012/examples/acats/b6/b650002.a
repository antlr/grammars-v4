-- B650002
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--
--     Check that an extended return statement cannot be used to return
--     from a procedure body, entry body, or accept statement.
--
--     Check that an extended return statement cannot be used to return
--     from an outer extended return statement.
--
--     Check that a simple return statement that applies to a procedure body,
--     entry body, accept statement, or extended return statement cannot have
--     an expression.
--
--     Check that a simple return statement that applies to a function body
--     must have an expression.
--
-- CHANGE HISTORY:
--      21 Sep 2007   RLB   Created test from Ada 83 tests.
--      15 Nov 2007   RLB   Updated objective per ARG resolution.
--!
procedure B650002 is

    Obj : Character := 'A';

    function Func (P : in Positive) return Natural;

    function Func (P : in Positive) return Natural is

        procedure Proc (N : in out Natural) is
        begin
            case N is
                when 1 =>
                    return N - 1;                               -- ERROR: 3
                when 2 =>
                    return R : Natural do                       -- ERROR: 1
                        R := N + 1;
                    end return;
                when 3 =>
                    return R : Natural := N * 2;                -- ERROR: 1
                when 4 =>
                    return;                                     -- OK.
                when others =>
                    declare
                        P : Natural := N + 1;
                    begin
                        if P < 4 then
                            return (P);                         -- ERROR: 3
                        elsif P in 5..8 then
                            return R : Natural do               -- ERROR: 1
                                R := P + 1;
                            end return;
                        else
                            return;                             -- OK.
                        end if;
                    end;
            end case;
        end Proc;

        generic
        procedure Gen (N : in out Natural);

        procedure Gen (N : in out Natural) is
        begin
            case N is
                when 1 =>
                    return N / 2;                               -- ERROR: 3
                when 2 =>
                    return R : Natural do                       -- ERROR: 1
                        R := N * 3;
                    end return;
                when 3 =>
                    return R : Natural := N - 2;                -- ERROR: 1
                when others =>
                    return;                                     -- OK.
            end case;
        end Gen;

        generic
        function GenF (N : Natural) return Positive;

        function GenF (N : Natural) return Positive is
        begin
            if N = 1 then
                return N * 2;                                   -- OK.
            elsif N = 2 then
                return;                                         -- ERROR: 4
            else
                return R : Positive do                          -- OK.
                    R := N - 1;
                    if N = 3 then
                        return R : Positive := 0;               -- ERROR: 2
                    elsif N = 4 then
                        return 4;                               -- ERROR: 3
                    else
                        return;                                 -- OK.
                    end if;
                end return;
            end if;
        end GenF;

        protected type Prot is
            procedure Proc (P : in out Positive);
            entry Ent (P : in out Positive);
        private
            Count : Natural := 0;
        end Prot;

        protected body Prot is
            procedure Proc (P : in out Positive) is
            begin
                case P is
                    when 1 =>
                        return P * 3;                           -- ERROR: 3
                    when 2 =>
                        return R : Positive do                  -- ERROR: 1
                            R := P - 1;
                        end return;
                    when 3 =>
                        return R : Natural := P + 2;            -- ERROR: 1
                    when others =>
                        return;                                 -- OK.
                end case;
            end Proc;

            entry Ent (P : in out Positive) when True is
            begin
                case P is
                    when 1 =>
                        return P * 3;                           -- ERROR: 3
                    when 2 =>
                        return R : Positive do                  -- ERROR: 1
                            R := P - 1;
                        end return;
                    when 3 =>
                        return R : Natural := P + 2;            -- ERROR: 1
                    when 4 =>
                        return;                                 -- OK.
                    when others =>
                        declare
                            N : Natural := P - 1;
                        begin
                            if N < 4 then
                                return (P);                     -- ERROR: 3
                            elsif N in 5..8 then
                                return R : Positive do          -- ERROR: 1
                                    R := P + 1;
                                end return;
                            else
                                return;                         -- OK.
                            end if;
                        end;
                end case;
            end Ent;

        end Prot;

        task type Tsk is
            entry Ent (P : in out Positive);
        end Tsk;

        task body Tsk is
        begin
            accept Ent (P : in out Positive) do
                case P is
                    when 1 =>
                        return P * 4;                           -- ERROR: 3
                    when 2 =>
                        return R : Positive do                  -- ERROR: 1
                            R := P + 3;
                        end return;
                    when 3 =>
                        return R : Natural := P * 2;            -- ERROR: 1
                    when 4 =>
                        return;                                 -- OK.
                    when others =>
                        declare
                            N : Natural := P - 1;
                        begin
                            if N < 4 then
                                return (P);                     -- ERROR: 3
                            elsif N in 5..8 then
                                return R : Positive do          -- ERROR: 1
                                    R := P + 1;
                                end return;
                            else
                                return;                         -- OK.
                            end if;
                        end;
                end case;
            end Ent;
        end Tsk;

    begin
        if P = 1 then
            return P * 2;                                       -- OK.
        elsif P = 2 then
            return;                                             -- ERROR: 4
        elsif P in 3 .. 10 then
            return R : Natural do                               -- OK.
                R := P - 1;
                if P = 3 then
                    return R : Natural := 0;                    -- ERROR: 2
                elsif P = 4 then
                    return 4;                                   -- ERROR: 3
                else
                    return;                                     -- OK.
                end if;
            end return;
        else
            declare
                N : Natural := P - 1;
            begin
                if N < 4 then
                    return (P);                                 -- OK.
                elsif N in 5..8 then
                    return;                                     -- ERROR: 4
                else
                    return R : Natural do                       -- OK.
                        R := N + 2;
                        if N = 13 then
                            return R : Natural := 0;            -- ERROR: 2
                        elsif N = 14 then
                            return (N + 2);                     -- ERROR: 3
                        else
                            return;                             -- OK.
                        end if;
                    end return;
                end if;
            end;
        end if;
    end Func;

begin
    if Obj = 'R' then
        return Func (4);                                        -- ERROR: 3
    elsif Obj = 'L' then
        return R : Character do                                 -- ERROR: 1
            R := 'B';
        end return;
    else
        return;                                                 -- OK.
    end if;
end B650002;

