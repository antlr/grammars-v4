--------------------------------------------------------------------------
--                                                                      --
-- Copyright (c) 1990,1991,1992 by Synopsys, Inc.  All rights reserved. --
--                                                                      --
-- This source file may be used and distributed without restriction     --
-- provided that this copyright statement is not removed from the file  --
-- and that any derivative work contains this copyright notice.         --
--                                                                      --
--    Package name: STD_LOGIC_ARITH                                     --
--                                                                      --
--    Purpose:                                                          --
--     A set of arithemtic, conversion, and comparison functions        --
--     for SIGNED, UNSIGNED, SMALL_INT, INTEGER,                        --
--     STD_ULOGIC, STD_LOGIC, and STD_LOGIC_VECTOR.                     --
--                                                                      --
--------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

package std_logic_arith is

    type UNSIGNED is array (NATURAL range <>) of STD_LOGIC;
    type SIGNED is array (NATURAL range <>) of STD_LOGIC;
    subtype SMALL_INT is INTEGER range 0 to 1;

    ----------------
    -- add operators
    ----------------
    function "+"(L: UNSIGNED; R: UNSIGNED) return UNSIGNED;

    function "+"(L: SIGNED; R: SIGNED) return SIGNED;

    function "+"(L: UNSIGNED; R: SIGNED) return SIGNED;

    function "+"(L: SIGNED; R: UNSIGNED) return SIGNED;

    function "+"(L: UNSIGNED; R: INTEGER) return UNSIGNED;

    function "+"(L: INTEGER; R: UNSIGNED) return UNSIGNED;

    function "+"(L: SIGNED; R: INTEGER) return SIGNED;

    function "+"(L: INTEGER; R: SIGNED) return SIGNED;

    function "+"(L: UNSIGNED; R: STD_ULOGIC) return UNSIGNED;

    function "+"(L: STD_ULOGIC; R: UNSIGNED) return UNSIGNED;

    function "+"(L: SIGNED; R: STD_ULOGIC) return SIGNED;

    function "+"(L: STD_ULOGIC; R: SIGNED) return SIGNED;

    function "+"(L: UNSIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "+"(L: SIGNED; R: SIGNED) return STD_LOGIC_VECTOR;

    function "+"(L: UNSIGNED; R: SIGNED) return STD_LOGIC_VECTOR;

    function "+"(L: SIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "+"(L: UNSIGNED; R: INTEGER) return STD_LOGIC_VECTOR;

    function "+"(L: INTEGER; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "+"(L: SIGNED; R: INTEGER) return STD_LOGIC_VECTOR;

    function "+"(L: INTEGER; R: SIGNED) return STD_LOGIC_VECTOR;

    function "+"(L: UNSIGNED; R: STD_ULOGIC) return STD_LOGIC_VECTOR;

    function "+"(L: STD_ULOGIC; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "+"(L: SIGNED; R: STD_ULOGIC) return STD_LOGIC_VECTOR;

    function "+"(L: STD_ULOGIC; R: SIGNED) return STD_LOGIC_VECTOR;

    ---------------------
    -- subtract operators
    ---------------------
    function "-"(L: UNSIGNED; R: UNSIGNED) return UNSIGNED;

    function "-"(L: SIGNED; R: SIGNED) return SIGNED;

    function "-"(L: UNSIGNED; R: SIGNED) return SIGNED;

    function "-"(L: SIGNED; R: UNSIGNED) return SIGNED;

    function "-"(L: UNSIGNED; R: INTEGER) return UNSIGNED;

    function "-"(L: INTEGER; R: UNSIGNED) return UNSIGNED;

    function "-"(L: SIGNED; R: INTEGER) return SIGNED;

    function "-"(L: INTEGER; R: SIGNED) return SIGNED;

    function "-"(L: UNSIGNED; R: STD_ULOGIC) return UNSIGNED;

    function "-"(L: STD_ULOGIC; R: UNSIGNED) return UNSIGNED;

    function "-"(L: SIGNED; R: STD_ULOGIC) return SIGNED;

    function "-"(L: STD_ULOGIC; R: SIGNED) return SIGNED;

    function "-"(L: UNSIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "-"(L: SIGNED; R: SIGNED) return STD_LOGIC_VECTOR;

    function "-"(L: UNSIGNED; R: SIGNED) return STD_LOGIC_VECTOR;

    function "-"(L: SIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "-"(L: UNSIGNED; R: INTEGER) return STD_LOGIC_VECTOR;

    function "-"(L: INTEGER; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "-"(L: SIGNED; R: INTEGER) return STD_LOGIC_VECTOR;

    function "-"(L: INTEGER; R: SIGNED) return STD_LOGIC_VECTOR;

    function "-"(L: UNSIGNED; R: STD_ULOGIC) return STD_LOGIC_VECTOR;

    function "-"(L: STD_ULOGIC; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "-"(L: SIGNED; R: STD_ULOGIC) return STD_LOGIC_VECTOR;

    function "-"(L: STD_ULOGIC; R: SIGNED) return STD_LOGIC_VECTOR;

    ------------------
    -- unary operators
    ------------------
    function "+"(L: UNSIGNED) return UNSIGNED;

    function "+"(L: SIGNED) return SIGNED;

    function "-"(L: SIGNED) return SIGNED;

    function "ABS"(L: SIGNED) return SIGNED;

    function "+"(L: UNSIGNED) return STD_LOGIC_VECTOR;

    function "+"(L: SIGNED) return STD_LOGIC_VECTOR;

    function "-"(L: SIGNED) return STD_LOGIC_VECTOR;

    function "ABS"(L: SIGNED) return STD_LOGIC_VECTOR;

    ---------------------------
    -- multiplication operators
    ---------------------------
    function "*"(L: UNSIGNED; R: UNSIGNED) return UNSIGNED;

    function "*"(L: SIGNED; R: SIGNED) return SIGNED;

    function "*"(L: SIGNED; R: UNSIGNED) return SIGNED;

    function "*"(L: UNSIGNED; R: SIGNED) return SIGNED;

    function "*"(L: UNSIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "*"(L: SIGNED; R: SIGNED) return STD_LOGIC_VECTOR;

    function "*"(L: SIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR;

    function "*"(L: UNSIGNED; R: SIGNED) return STD_LOGIC_VECTOR;


    -----------------------
    -- less_than comparison
    -----------------------
    function "<"(L: UNSIGNED; R: UNSIGNED) return BOOLEAN;

    function "<"(L: SIGNED; R: SIGNED) return BOOLEAN;

    function "<"(L: UNSIGNED; R: SIGNED) return BOOLEAN;

    function "<"(L: SIGNED; R: UNSIGNED) return BOOLEAN;

    function "<"(L: UNSIGNED; R: INTEGER) return BOOLEAN;

    function "<"(L: INTEGER; R: UNSIGNED) return BOOLEAN;

    function "<"(L: SIGNED; R: INTEGER) return BOOLEAN;

    function "<"(L: INTEGER; R: SIGNED) return BOOLEAN;


    --------------------------------
    -- less_than_or_equal comparison
    --------------------------------
    function "<="(L: UNSIGNED; R: UNSIGNED) return BOOLEAN;

    function "<="(L: SIGNED; R: SIGNED) return BOOLEAN;

    function "<="(L: UNSIGNED; R: SIGNED) return BOOLEAN;

    function "<="(L: SIGNED; R: UNSIGNED) return BOOLEAN;

    function "<="(L: UNSIGNED; R: INTEGER) return BOOLEAN;

    function "<="(L: INTEGER; R: UNSIGNED) return BOOLEAN;

    function "<="(L: SIGNED; R: INTEGER) return BOOLEAN;

    function "<="(L: INTEGER; R: SIGNED) return BOOLEAN;


    --------------------------
    -- greater_than comparison
    --------------------------
    function ">"(L: UNSIGNED; R: UNSIGNED) return BOOLEAN;

    function ">"(L: SIGNED; R: SIGNED) return BOOLEAN;

    function ">"(L: UNSIGNED; R: SIGNED) return BOOLEAN;

    function ">"(L: SIGNED; R: UNSIGNED) return BOOLEAN;

    function ">"(L: UNSIGNED; R: INTEGER) return BOOLEAN;

    function ">"(L: INTEGER; R: UNSIGNED) return BOOLEAN;

    function ">"(L: SIGNED; R: INTEGER) return BOOLEAN;

    function ">"(L: INTEGER; R: SIGNED) return BOOLEAN;


    -----------------------------------
    -- greater_than_or_equal comparison
    -----------------------------------
    function ">="(L: UNSIGNED; R: UNSIGNED) return BOOLEAN;

    function ">="(L: SIGNED; R: SIGNED) return BOOLEAN;

    function ">="(L: UNSIGNED; R: SIGNED) return BOOLEAN;

    function ">="(L: SIGNED; R: UNSIGNED) return BOOLEAN;

    function ">="(L: UNSIGNED; R: INTEGER) return BOOLEAN;

    function ">="(L: INTEGER; R: UNSIGNED) return BOOLEAN;

    function ">="(L: SIGNED; R: INTEGER) return BOOLEAN;

    function ">="(L: INTEGER; R: SIGNED) return BOOLEAN;


    -------------------
    -- equal comparison
    -------------------
    function "="(L: UNSIGNED; R: UNSIGNED) return BOOLEAN;

    function "="(L: SIGNED; R: SIGNED) return BOOLEAN;

    function "="(L: UNSIGNED; R: SIGNED) return BOOLEAN;

    function "="(L: SIGNED; R: UNSIGNED) return BOOLEAN;

    function "="(L: UNSIGNED; R: INTEGER) return BOOLEAN;

    function "="(L: INTEGER; R: UNSIGNED) return BOOLEAN;

    function "="(L: SIGNED; R: INTEGER) return BOOLEAN;

    function "="(L: INTEGER; R: SIGNED) return BOOLEAN;


    -----------------------
    -- not equal comparison
    -----------------------
    function "/="(L: UNSIGNED; R: UNSIGNED) return BOOLEAN;

    function "/="(L: SIGNED; R: SIGNED) return BOOLEAN;

    function "/="(L: UNSIGNED; R: SIGNED) return BOOLEAN;

    function "/="(L: SIGNED; R: UNSIGNED) return BOOLEAN;

    function "/="(L: UNSIGNED; R: INTEGER) return BOOLEAN;

    function "/="(L: INTEGER; R: UNSIGNED) return BOOLEAN;

    function "/="(L: SIGNED; R: INTEGER) return BOOLEAN;

    function "/="(L: INTEGER; R: SIGNED) return BOOLEAN;


    ------------------
    -- shift operators
    ------------------
    function SHL(ARG: UNSIGNED; COUNT: UNSIGNED) return UNSIGNED;

    function SHL(ARG: SIGNED; COUNT: UNSIGNED) return SIGNED;

    function SHR(ARG: UNSIGNED; COUNT: UNSIGNED) return UNSIGNED;

    function SHR(ARG: SIGNED; COUNT: UNSIGNED) return SIGNED;


    -----------------------
    -- conversion operators
    -----------------------
    function CONV_INTEGER(ARG: INTEGER) return INTEGER;

    function CONV_INTEGER(ARG: UNSIGNED) return INTEGER;

    function CONV_INTEGER(ARG: SIGNED) return INTEGER;

    function CONV_INTEGER(ARG: STD_ULOGIC) return SMALL_INT;

    function CONV_UNSIGNED(ARG: INTEGER; SIZE: INTEGER) return UNSIGNED;

    function CONV_UNSIGNED(ARG: UNSIGNED; SIZE: INTEGER) return UNSIGNED;

    function CONV_UNSIGNED(ARG: SIGNED; SIZE: INTEGER) return UNSIGNED;

    function CONV_UNSIGNED(ARG: STD_ULOGIC; SIZE: INTEGER) return UNSIGNED;

    function CONV_SIGNED(ARG: INTEGER; SIZE: INTEGER) return SIGNED;

    function CONV_SIGNED(ARG: UNSIGNED; SIZE: INTEGER) return SIGNED;

    function CONV_SIGNED(ARG: SIGNED; SIZE: INTEGER) return SIGNED;

    function CONV_SIGNED(ARG: STD_ULOGIC; SIZE: INTEGER) return SIGNED;

    function CONV_STD_LOGIC_VECTOR(ARG: INTEGER; SIZE: INTEGER) return STD_LOGIC_VECTOR;

    function CONV_STD_LOGIC_VECTOR(ARG: UNSIGNED; SIZE: INTEGER) return STD_LOGIC_VECTOR;

    function CONV_STD_LOGIC_VECTOR(ARG: SIGNED; SIZE: INTEGER) return STD_LOGIC_VECTOR;

    function CONV_STD_LOGIC_VECTOR(ARG: STD_ULOGIC; SIZE: INTEGER) return STD_LOGIC_VECTOR;


    ----------------------------------------------
    -- zero extend STD_LOGIC_VECTOR (ARG) to SIZE, 
    -- SIZE < 0 is same as SIZE = 0
    -- returns STD_LOGIC_VECTOR(SIZE-1 downto 0)
    ----------------------------------------------
    function EXT(ARG: STD_LOGIC_VECTOR; SIZE: INTEGER) return STD_LOGIC_VECTOR;

    ----------------------------------------------
    -- sign extend STD_LOGIC_VECTOR (ARG) to SIZE, 
    -- SIZE < 0 is same as SIZE = 0
    -- return STD_LOGIC_VECTOR(SIZE-1 downto 0)
    ----------------------------------------------
    function SXT(ARG: STD_LOGIC_VECTOR; SIZE: INTEGER) return STD_LOGIC_VECTOR;

end Std_logic_arith;



library IEEE;
use IEEE.std_logic_1164.all;

package body std_logic_arith is

    function max(L, R: INTEGER) return INTEGER is
    begin
    if L > R then
        return L;
    else
        return R;
    end if;
    end;


    function min(L, R: INTEGER) return INTEGER is
    begin
    if L < R then
        return L;
    else
        return R;
    end if;
    end;

    -- synopsys synthesis_off
    type tbl_type is array (STD_ULOGIC) of STD_ULOGIC;
    constant tbl_BINARY : tbl_type :=
    ('X', 'X', '0', '1', 'X', 'X', '0', '1', 'X');
    -- synopsys synthesis_on

    -- synopsys synthesis_off
    type tbl_mvl9_boolean is array (STD_ULOGIC) of boolean;
    constant IS_X : tbl_mvl9_boolean :=
        (true, true, false, false, true, true, false, false, true);
    -- synopsys synthesis_on



    function MAKE_BINARY(A : STD_ULOGIC) return STD_ULOGIC is
    -- synopsys built_in SYN_FEED_THRU
    begin
    -- synopsys synthesis_off
        if (IS_X(A)) then
        assert false 
        report "There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, the result will be 'X'(es)."
        severity warning;
            return ('X');
        end if;
        return tbl_BINARY(A);
    -- synopsys synthesis_on
    end;

    function MAKE_BINARY(A : UNSIGNED) return UNSIGNED is
    -- synopsys built_in SYN_FEED_THRU
    variable one_bit : STD_ULOGIC;
    variable result : UNSIGNED (A'range);
    begin
    -- synopsys synthesis_off
        for i in A'range loop
            if (IS_X(A(i))) then
            assert false 
            report "There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, the result will be 'X'(es)."
            severity warning;
            result := (others => 'X');
                return result;
            end if;
        result(i) := tbl_BINARY(A(i));
        end loop;
        return result;
    -- synopsys synthesis_on
    end;

    function MAKE_BINARY(A : UNSIGNED) return SIGNED is
    -- synopsys built_in SYN_FEED_THRU
    variable one_bit : STD_ULOGIC;
    variable result : SIGNED (A'range);
    begin
    -- synopsys synthesis_off
        for i in A'range loop
            if (IS_X(A(i))) then
            assert false 
            report "There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, the result will be 'X'(es)."
            severity warning;
            result := (others => 'X');
                return result;
            end if;
        result(i) := tbl_BINARY(A(i));
        end loop;
        return result;
    -- synopsys synthesis_on
    end;

    function MAKE_BINARY(A : SIGNED) return UNSIGNED is
    -- synopsys built_in SYN_FEED_THRU
    variable one_bit : STD_ULOGIC;
    variable result : UNSIGNED (A'range);
    begin
    -- synopsys synthesis_off
        for i in A'range loop
            if (IS_X(A(i))) then
            assert false 
            report "There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, the result will be 'X'(es)."
            severity warning;
            result := (others => 'X');
                return result;
            end if;
        result(i) := tbl_BINARY(A(i));
        end loop;
        return result;
    -- synopsys synthesis_on
    end;

    function MAKE_BINARY(A : SIGNED) return SIGNED is
    -- synopsys built_in SYN_FEED_THRU
    variable one_bit : STD_ULOGIC;
    variable result : SIGNED (A'range);
    begin
    -- synopsys synthesis_off
        for i in A'range loop
            if (IS_X(A(i))) then
            assert false 
            report "There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, the result will be 'X'(es)."
            severity warning;
            result := (others => 'X');
                return result;
            end if;
        result(i) := tbl_BINARY(A(i));
        end loop;
        return result;
    -- synopsys synthesis_on
    end;

    function MAKE_BINARY(A : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    -- synopsys built_in SYN_FEED_THRU
    variable one_bit : STD_ULOGIC;
    variable result : STD_LOGIC_VECTOR (A'range);
    begin
    -- synopsys synthesis_off
        for i in A'range loop
            if (IS_X(A(i))) then
            assert false 
            report "There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, the result will be 'X'(es)."
            severity warning;
            result := (others => 'X');
                return result;
            end if;
        result(i) := tbl_BINARY(A(i));
        end loop;
        return result;
    -- synopsys synthesis_on
    end;

    function MAKE_BINARY(A : UNSIGNED) return STD_LOGIC_VECTOR is
    -- synopsys built_in SYN_FEED_THRU
    variable one_bit : STD_ULOGIC;
    variable result : STD_LOGIC_VECTOR (A'range);
    begin
    -- synopsys synthesis_off
        for i in A'range loop
            if (IS_X(A(i))) then
            assert false 
            report "There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, the result will be 'X'(es)."
            severity warning;
            result := (others => 'X');
                return result;
            end if;
        result(i) := tbl_BINARY(A(i));
        end loop;
        return result;
    -- synopsys synthesis_on
    end;

    function MAKE_BINARY(A : SIGNED) return STD_LOGIC_VECTOR is
    -- synopsys built_in SYN_FEED_THRU
    variable one_bit : STD_ULOGIC;
    variable result : STD_LOGIC_VECTOR (A'range);
    begin
    -- synopsys synthesis_off
        for i in A'range loop
            if (IS_X(A(i))) then
            assert false 
            report "There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, the result will be 'X'(es)."
            severity warning;
            result := (others => 'X');
                return result;
            end if;
        result(i) := tbl_BINARY(A(i));
        end loop;
        return result;
    -- synopsys synthesis_on
    end;



    -- Type propagation function which returns a signed type with the
    -- size of the left arg.
    function LEFT_SIGNED_ARG(A,B: SIGNED) return SIGNED is
      variable Z: SIGNED (A'left downto 0);
      -- pragma return_port_name Z
    begin
      return(Z);
    end;
    
    -- Type propagation function which returns an unsigned type with the
    -- size of the left arg.
    function LEFT_UNSIGNED_ARG(A,B: UNSIGNED) return UNSIGNED is
      variable Z: UNSIGNED (A'left downto 0);
      -- pragma return_port_name Z
    begin
      return(Z);
    end;
    
    -- Type propagation function which returns a signed type with the
    -- size of the result of a signed multiplication
    function MULT_SIGNED_ARG(A,B: SIGNED) return SIGNED is
      variable Z: SIGNED ((A'length+B'length-1) downto 0);
      -- pragma return_port_name Z
    begin
      return(Z);
    end;
    
    -- Type propagation function which returns an unsigned type with the
    -- size of the result of a unsigned multiplication
    function MULT_UNSIGNED_ARG(A,B: UNSIGNED) return UNSIGNED is
      variable Z: UNSIGNED ((A'length+B'length-1) downto 0);
      -- pragma return_port_name Z
    begin
      return(Z);
    end;



    function mult(A,B: SIGNED) return SIGNED is

      variable BA: SIGNED((A'length+B'length-1) downto 0);
      variable PA: SIGNED((A'length+B'length-1) downto 0);
      variable AA: SIGNED(A'length downto 0);
      variable neg: STD_ULOGIC;
      constant one : UNSIGNED(1 downto 0) := "01";
      
      -- pragma map_to_operator MULT_TC_OP
      -- pragma type_function MULT_SIGNED_ARG
      -- pragma return_port_name Z

      begin
    if (A(A'left) = 'X' or B(B'left) = 'X') then
            PA := (others => 'X');
            return(PA);
    end if;
        PA := (others => '0');
        neg := B(B'left) xor A(A'left);
        BA := CONV_SIGNED(('0' & ABS(B)),(A'length+B'length));
        AA := '0' & ABS(A);
        for i in 0 to A'length-1 loop
          if AA(i) = '1' then
            PA := PA+BA;
          end if;
          BA := SHL(BA,one);
        end loop;
        if (neg= '1') then
          return(-PA);
        else 
          return(PA);
        end if;
      end;

    function mult(A,B: UNSIGNED) return UNSIGNED is

      variable BA: UNSIGNED((A'length+B'length-1) downto 0);
      variable PA: UNSIGNED((A'length+B'length-1) downto 0);
      constant one : UNSIGNED(1 downto 0) := "01";
      
      -- pragma map_to_operator MULT_UNS_OP
      -- pragma type_function MULT_UNSIGNED_ARG
      -- pragma return_port_name Z

      begin
    if (A(A'left) = 'X' or B(B'left) = 'X') then
            PA := (others => 'X');
            return(PA);
    end if;
        PA := (others => '0');
        BA := CONV_UNSIGNED(B,(A'length+B'length));
        for i in 0 to A'length-1 loop
          if A(i) = '1' then
            PA := PA+BA;
          end if;
          BA := SHL(BA,one);
        end loop;
        return(PA);
      end;

    -- subtract two signed numbers of the same length
    -- both arrays must have range (msb downto 0)
    function minus(A, B: SIGNED) return SIGNED is
    variable carry: STD_ULOGIC;
    variable BV: STD_ULOGIC_VECTOR (A'left downto 0);
    variable sum: SIGNED (A'left downto 0);

    -- pragma map_to_operator SUB_TC_OP

    -- pragma type_function LEFT_SIGNED_ARG
        -- pragma return_port_name Z

    begin
    if (A(A'left) = 'X' or B(B'left) = 'X') then
            sum := (others => 'X');
            return(sum);
    end if;
    carry := '1';
    BV := not STD_ULOGIC_VECTOR(B);

    for i in 0 to A'left loop
        sum(i) := A(i) xor BV(i) xor carry;
        carry := (A(i) and BV(i)) or
            (A(i) and carry) or
            (carry and BV(i));
    end loop;
    return sum;
    end;

    -- add two signed numbers of the same length
    -- both arrays must have range (msb downto 0)
    function plus(A, B: SIGNED) return SIGNED is
    variable carry: STD_ULOGIC;
    variable BV, sum: SIGNED (A'left downto 0);

    -- pragma map_to_operator ADD_TC_OP
    -- pragma type_function LEFT_SIGNED_ARG
        -- pragma return_port_name Z

    begin
    if (A(A'left) = 'X' or B(B'left) = 'X') then
            sum := (others => 'X');
            return(sum);
    end if;
    carry := '0';
    BV := B;

    for i in 0 to A'left loop
        sum(i) := A(i) xor BV(i) xor carry;
        carry := (A(i) and BV(i)) or
            (A(i) and carry) or
            (carry and BV(i));
    end loop;
    return sum;
    end;


    -- subtract two unsigned numbers of the same length
    -- both arrays must have range (msb downto 0)
    function unsigned_minus(A, B: UNSIGNED) return UNSIGNED is
    variable carry: STD_ULOGIC;
    variable BV: STD_ULOGIC_VECTOR (A'left downto 0);
    variable sum: UNSIGNED (A'left downto 0);

    -- pragma map_to_operator SUB_UNS_OP
    -- pragma type_function LEFT_UNSIGNED_ARG
        -- pragma return_port_name Z

    begin
    if (A(A'left) = 'X' or B(B'left) = 'X') then
            sum := (others => 'X');
            return(sum);
    end if;
    carry := '1';
    BV := not STD_ULOGIC_VECTOR(B);

    for i in 0 to A'left loop
        sum(i) := A(i) xor BV(i) xor carry;
        carry := (A(i) and BV(i)) or
            (A(i) and carry) or
            (carry and BV(i));
    end loop;
    return sum;
    end;

    -- add two unsigned numbers of the same length
    -- both arrays must have range (msb downto 0)
    function unsigned_plus(A, B: UNSIGNED) return UNSIGNED is
    variable carry: STD_ULOGIC;
    variable BV, sum: UNSIGNED (A'left downto 0);

    -- pragma map_to_operator ADD_UNS_OP
    -- pragma type_function LEFT_UNSIGNED_ARG
        -- pragma return_port_name Z

    begin
    if (A(A'left) = 'X' or B(B'left) = 'X') then
            sum := (others => 'X');
            return(sum);
    end if;
    carry := '0';
    BV := B;

    for i in 0 to A'left loop
        sum(i) := A(i) xor BV(i) xor carry;
        carry := (A(i) and BV(i)) or
            (A(i) and carry) or
            (carry and BV(i));
    end loop;
    return sum;
    end;



    function "*"(L: SIGNED; R: SIGNED) return SIGNED is
    -- pragma label_applies_to mult
    begin
          return     mult(CONV_SIGNED(L, L'length),
                  CONV_SIGNED(R, R'length)); -- pragma label mult 
    end;
      
    function "*"(L: UNSIGNED; R: UNSIGNED) return UNSIGNED is
    -- pragma label_applies_to mult
    begin
      return   mult(CONV_UNSIGNED(L, L'length),
                    CONV_UNSIGNED(R, R'length)); -- pragma label mult 
end;
    
    function "*"(L: UNSIGNED; R: SIGNED) return SIGNED is
    -- pragma label_applies_to plus
    begin
     return       mult(CONV_SIGNED(L, L'length+1),
                  CONV_SIGNED(R, R'length)); -- pragma label mult 
    end;

    function "*"(L: SIGNED; R: UNSIGNED) return SIGNED is
    -- pragma label_applies_to plus
    begin
    return      mult(CONV_SIGNED(L, L'length),
                 CONV_SIGNED(R, R'length+1)); -- pragma label mult 
    end;


    function "*"(L: SIGNED; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to mult
    begin
          return STD_LOGIC_VECTOR (mult(CONV_SIGNED(L, L'length),
                  CONV_SIGNED(R, R'length))); -- pragma label mult 
    end;
      
    function "*"(L: UNSIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to mult
    begin
          return STD_LOGIC_VECTOR (mult(CONV_UNSIGNED(L, L'length),
                        CONV_UNSIGNED(R, R'length))); -- pragma label mult 
    end;
        
    function "*"(L: UNSIGNED; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    begin
     return STD_LOGIC_VECTOR (mult(CONV_SIGNED(L, L'length+1),
                  CONV_SIGNED(R, R'length))); -- pragma label mult 
    end;

    function "*"(L: SIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    begin
    return STD_LOGIC_VECTOR (mult(CONV_SIGNED(L, L'length),
                 CONV_SIGNED(R, R'length+1))); -- pragma label mult 
    end;


    function "+"(L: UNSIGNED; R: UNSIGNED) return UNSIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := max(L'length, R'length);
    begin
    return unsigned_plus(CONV_UNSIGNED(L, length),
                 CONV_UNSIGNED(R, length)); -- pragma label plus
    end;


    function "+"(L: SIGNED; R: SIGNED) return SIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := max(L'length, R'length);
    begin
    return plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label plus
    end;


    function "+"(L: UNSIGNED; R: SIGNED) return SIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label plus
    end;


    function "+"(L: SIGNED; R: UNSIGNED) return SIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label plus
    end;


    function "+"(L: UNSIGNED; R: INTEGER) return UNSIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := L'length + 1;
    begin
    return CONV_UNSIGNED(
        plus( -- pragma label plus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1);
    end;


    function "+"(L: INTEGER; R: UNSIGNED) return UNSIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := R'length + 1;
    begin
    return CONV_UNSIGNED(
        plus( -- pragma label plus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1);
    end;


    function "+"(L: SIGNED; R: INTEGER) return SIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := L'length;
    begin
    return plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label plus
    end;


    function "+"(L: INTEGER; R: SIGNED) return SIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := R'length;
    begin
    return plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label plus
    end;


    function "+"(L: UNSIGNED; R: STD_ULOGIC) return UNSIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := L'length;
    begin
    return unsigned_plus(CONV_UNSIGNED(L, length),
             CONV_UNSIGNED(R, length)) ; -- pragma label plus
    end;


    function "+"(L: STD_ULOGIC; R: UNSIGNED) return UNSIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := R'length;
    begin
    return unsigned_plus(CONV_UNSIGNED(L, length),
             CONV_UNSIGNED(R, length)); -- pragma label plus
    end;


    function "+"(L: SIGNED; R: STD_ULOGIC) return SIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := L'length;
    begin
    return plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label plus
    end;


    function "+"(L: STD_ULOGIC; R: SIGNED) return SIGNED is
    -- pragma label_applies_to plus
    constant length: INTEGER := R'length;
    begin
    return plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label plus
    end;



    function "+"(L: UNSIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := max(L'length, R'length);
    begin
    return STD_LOGIC_VECTOR (unsigned_plus(CONV_UNSIGNED(L, length),
                 CONV_UNSIGNED(R, length))); -- pragma label plus
    end;


    function "+"(L: SIGNED; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := max(L'length, R'length);
    begin
    return STD_LOGIC_VECTOR (plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length))); -- pragma label plus
    end;


    function "+"(L: UNSIGNED; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return STD_LOGIC_VECTOR (plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length))); -- pragma label plus
    end;


    function "+"(L: SIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return STD_LOGIC_VECTOR (plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length))); -- pragma label plus
    end;


    function "+"(L: UNSIGNED; R: INTEGER) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := L'length + 1;
    begin
    return STD_LOGIC_VECTOR (CONV_UNSIGNED(
        plus( -- pragma label plus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1));
    end;


    function "+"(L: INTEGER; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := R'length + 1;
    begin
    return STD_LOGIC_VECTOR (CONV_UNSIGNED(
        plus( -- pragma label plus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1));
    end;


    function "+"(L: SIGNED; R: INTEGER) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := L'length;
    begin
    return STD_LOGIC_VECTOR (plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length))); -- pragma label plus
    end;


    function "+"(L: INTEGER; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := R'length;
    begin
    return STD_LOGIC_VECTOR (plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length))); -- pragma label plus
    end;


    function "+"(L: UNSIGNED; R: STD_ULOGIC) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := L'length;
    begin
    return STD_LOGIC_VECTOR (unsigned_plus(CONV_UNSIGNED(L, length),
             CONV_UNSIGNED(R, length))) ; -- pragma label plus
    end;


    function "+"(L: STD_ULOGIC; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := R'length;
    begin
    return STD_LOGIC_VECTOR (unsigned_plus(CONV_UNSIGNED(L, length),
             CONV_UNSIGNED(R, length))); -- pragma label plus
    end;


    function "+"(L: SIGNED; R: STD_ULOGIC) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := L'length;
    begin
    return STD_LOGIC_VECTOR (plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length))); -- pragma label plus
    end;


    function "+"(L: STD_ULOGIC; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to plus
    constant length: INTEGER := R'length;
    begin
    return STD_LOGIC_VECTOR (plus(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length))); -- pragma label plus
    end;



    function "-"(L: UNSIGNED; R: UNSIGNED) return UNSIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := max(L'length, R'length);
    begin
    return unsigned_minus(CONV_UNSIGNED(L, length),
                        CONV_UNSIGNED(R, length)); -- pragma label minus
    end;


    function "-"(L: SIGNED; R: SIGNED) return SIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := max(L'length, R'length);
    begin
    return minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length)); -- pragma label minus
    end;


    function "-"(L: UNSIGNED; R: SIGNED) return SIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length)); -- pragma label minus
    end;


    function "-"(L: SIGNED; R: UNSIGNED) return SIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length)); -- pragma label minus
    end;


    function "-"(L: UNSIGNED; R: INTEGER) return UNSIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := L'length + 1;
    begin
    return CONV_UNSIGNED(
        minus( -- pragma label minus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1);
    end;


    function "-"(L: INTEGER; R: UNSIGNED) return UNSIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := R'length + 1;
    begin
    return CONV_UNSIGNED(
        minus( -- pragma label minus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1);
    end;


    function "-"(L: SIGNED; R: INTEGER) return SIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := L'length;
    begin
    return minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length)); -- pragma label minus
    end;


    function "-"(L: INTEGER; R: SIGNED) return SIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := R'length;
    begin
    return minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length)); -- pragma label minus
    end;


    function "-"(L: UNSIGNED; R: STD_ULOGIC) return UNSIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := L'length + 1;
    begin
    return CONV_UNSIGNED(
        minus( -- pragma label minus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1);
    end;


    function "-"(L: STD_ULOGIC; R: UNSIGNED) return UNSIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := R'length + 1;
    begin
    return CONV_UNSIGNED(
        minus( -- pragma label minus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1);
    end;


    function "-"(L: SIGNED; R: STD_ULOGIC) return SIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := L'length;
    begin
    return minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length)); -- pragma label minus
    end;


    function "-"(L: STD_ULOGIC; R: SIGNED) return SIGNED is
    -- pragma label_applies_to minus
    constant length: INTEGER := R'length;
    begin
    return minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length)); -- pragma label minus
    end;




    function "-"(L: UNSIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := max(L'length, R'length);
    begin
    return STD_LOGIC_VECTOR (unsigned_minus(CONV_UNSIGNED(L, length),
                        CONV_UNSIGNED(R, length))); -- pragma label minus
    end;


    function "-"(L: SIGNED; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := max(L'length, R'length);
    begin
    return STD_LOGIC_VECTOR (minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length))); -- pragma label minus
    end;


    function "-"(L: UNSIGNED; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return STD_LOGIC_VECTOR (minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length))); -- pragma label minus
    end;


    function "-"(L: SIGNED; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return STD_LOGIC_VECTOR (minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length))); -- pragma label minus
    end;


    function "-"(L: UNSIGNED; R: INTEGER) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := L'length + 1;
    begin
    return STD_LOGIC_VECTOR (CONV_UNSIGNED(
        minus( -- pragma label minus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1));
    end;


    function "-"(L: INTEGER; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := R'length + 1;
    begin
    return STD_LOGIC_VECTOR (CONV_UNSIGNED(
        minus( -- pragma label minus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1));
    end;


    function "-"(L: SIGNED; R: INTEGER) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := L'length;
    begin
    return STD_LOGIC_VECTOR (minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length))); -- pragma label minus
    end;


    function "-"(L: INTEGER; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := R'length;
    begin
    return STD_LOGIC_VECTOR (minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length))); -- pragma label minus
    end;


    function "-"(L: UNSIGNED; R: STD_ULOGIC) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := L'length + 1;
    begin
    return STD_LOGIC_VECTOR (CONV_UNSIGNED(
        minus( -- pragma label minus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1));
    end;


    function "-"(L: STD_ULOGIC; R: UNSIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := R'length + 1;
    begin
    return STD_LOGIC_VECTOR (CONV_UNSIGNED(
        minus( -- pragma label minus
            CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)),
        length-1));
    end;


    function "-"(L: SIGNED; R: STD_ULOGIC) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := L'length;
    begin
    return STD_LOGIC_VECTOR (minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length))); -- pragma label minus
    end;


    function "-"(L: STD_ULOGIC; R: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    constant length: INTEGER := R'length;
    begin
    return STD_LOGIC_VECTOR (minus(CONV_SIGNED(L, length),
             CONV_SIGNED(R, length))); -- pragma label minus
    end;




    function "+"(L: UNSIGNED) return UNSIGNED is
    begin
    return L;
    end;


    function "+"(L: SIGNED) return SIGNED is
    begin
    return L;
    end;


    function "-"(L: SIGNED) return SIGNED is
    -- pragma label_applies_to minus
    begin
    return 0 - L; -- pragma label minus
    end;


    function "ABS"(L: SIGNED) return SIGNED is
    begin
    if (L(L'left) = '0' or L(L'left) = 'L') then
        return L;
    else
        return 0 - L;
    end if;
    end;


    function "+"(L: UNSIGNED) return STD_LOGIC_VECTOR is
    begin
    return STD_LOGIC_VECTOR (L);
    end;


    function "+"(L: SIGNED) return STD_LOGIC_VECTOR is
    begin
    return STD_LOGIC_VECTOR (L);
    end;


    function "-"(L: SIGNED) return STD_LOGIC_VECTOR is
    -- pragma label_applies_to minus
    variable tmp: SIGNED(L'length-1 downto 0);
    begin
    tmp := 0 - L;  -- pragma label minus
    return STD_LOGIC_VECTOR (tmp); 
    end;


    function "ABS"(L: SIGNED) return STD_LOGIC_VECTOR is
    variable tmp: SIGNED(L'length-1 downto 0);
    begin
    if (L(L'left) = '0' or L(L'left) = 'L') then
        return STD_LOGIC_VECTOR (L);
    else
        tmp := 0 - L;
        return STD_LOGIC_VECTOR (tmp);
    end if;
    end;


    -- Type propagation function which returns the type BOOLEAN
    function UNSIGNED_RETURN_BOOLEAN(A,B: UNSIGNED) return BOOLEAN is
      variable Z: BOOLEAN;
      -- pragma return_port_name Z
    begin
      return(Z);
    end;
    
    -- Type propagation function which returns the type BOOLEAN
    function SIGNED_RETURN_BOOLEAN(A,B: SIGNED) return BOOLEAN is
      variable Z: BOOLEAN;
      -- pragma return_port_name Z
    begin
      return(Z);
    end;
    

    -- compare two signed numbers of the same length
    -- both arrays must have range (msb downto 0)
    function is_less(A, B: SIGNED) return BOOLEAN is
    constant sign: INTEGER := A'left;
    variable a_is_0, b_is_1, result : boolean;

    -- pragma map_to_operator LT_TC_OP
    -- pragma type_function SIGNED_RETURN_BOOLEAN
        -- pragma return_port_name Z

    begin
    if A(sign) /= B(sign) then
        result := A(sign) = '1';
    else
        result := FALSE;
        for i in 0 to sign-1 loop
        a_is_0 := A(i) = '0';
        b_is_1 := B(i) = '1';
        result := (a_is_0 and b_is_1) or
              (a_is_0 and result) or
              (b_is_1 and result);
        end loop;
    end if;
    return result;
    end;


    -- compare two signed numbers of the same length
    -- both arrays must have range (msb downto 0)
    function is_less_or_equal(A, B: SIGNED) return BOOLEAN is
    constant sign: INTEGER := A'left;
    variable a_is_0, b_is_1, result : boolean;

    -- pragma map_to_operator LEQ_TC_OP
    -- pragma type_function SIGNED_RETURN_BOOLEAN
        -- pragma return_port_name Z

    begin
    if A(sign) /= B(sign) then
        result := A(sign) = '1';
    else
        result := TRUE;
        for i in 0 to sign-1 loop
        a_is_0 := A(i) = '0';
        b_is_1 := B(i) = '1';
        result := (a_is_0 and b_is_1) or
              (a_is_0 and result) or
              (b_is_1 and result);
        end loop;
    end if;
    return result;
    end;



    -- compare two unsigned numbers of the same length
    -- both arrays must have range (msb downto 0)
    function unsigned_is_less(A, B: UNSIGNED) return BOOLEAN is
    constant sign: INTEGER := A'left;
    variable a_is_0, b_is_1, result : boolean;

    -- pragma map_to_operator LT_UNS_OP
    -- pragma type_function UNSIGNED_RETURN_BOOLEAN
        -- pragma return_port_name Z

    begin
    result := FALSE;
    for i in 0 to sign loop
        a_is_0 := A(i) = '0';
        b_is_1 := B(i) = '1';
        result := (a_is_0 and b_is_1) or
              (a_is_0 and result) or
              (b_is_1 and result);
    end loop;
    return result;
    end;


    -- compare two unsigned numbers of the same length
    -- both arrays must have range (msb downto 0)
    function unsigned_is_less_or_equal(A, B: UNSIGNED) return BOOLEAN is
    constant sign: INTEGER := A'left;
    variable a_is_0, b_is_1, result : boolean;

    -- pragma map_to_operator LEQ_UNS_OP
    -- pragma type_function UNSIGNED_RETURN_BOOLEAN
        -- pragma return_port_name Z

    begin
    result := TRUE;
    for i in 0 to sign loop
        a_is_0 := A(i) = '0';
        b_is_1 := B(i) = '1';
        result := (a_is_0 and b_is_1) or
              (a_is_0 and result) or
              (b_is_1 and result);
    end loop;
    return result;
    end;




    function "<"(L: UNSIGNED; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to lt
    constant length: INTEGER := max(L'length, R'length);
    begin
    return unsigned_is_less(CONV_UNSIGNED(L, length),
                CONV_UNSIGNED(R, length)); -- pragma label lt
    end;


    function "<"(L: SIGNED; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to lt
    constant length: INTEGER := max(L'length, R'length);
    begin
    return is_less(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label lt
    end;


    function "<"(L: UNSIGNED; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to lt
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return is_less(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label lt
    end;


    function "<"(L: SIGNED; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to lt
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return is_less(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label lt
    end;


    function "<"(L: UNSIGNED; R: INTEGER) return BOOLEAN is
    -- pragma label_applies_to lt
    constant length: INTEGER := L'length + 1;
    begin
    return is_less(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label lt
    end;


    function "<"(L: INTEGER; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to lt
    constant length: INTEGER := R'length + 1;
    begin
    return is_less(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label lt
    end;


    function "<"(L: SIGNED; R: INTEGER) return BOOLEAN is
    -- pragma label_applies_to lt
    constant length: INTEGER := L'length;
    begin
    return is_less(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label lt
    end;


    function "<"(L: INTEGER; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to lt
    constant length: INTEGER := R'length;
    begin
    return is_less(CONV_SIGNED(L, length),
            CONV_SIGNED(R, length)); -- pragma label lt
    end;




    function "<="(L: UNSIGNED; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to leq
    constant length: INTEGER := max(L'length, R'length);
    begin
    return unsigned_is_less_or_equal(CONV_UNSIGNED(L, length),
                 CONV_UNSIGNED(R, length)); -- pragma label leq
    end;


    function "<="(L: SIGNED; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to leq
    constant length: INTEGER := max(L'length, R'length);
    begin
    return is_less_or_equal(CONV_SIGNED(L, length),
                CONV_SIGNED(R, length)); -- pragma label leq
    end;


    function "<="(L: UNSIGNED; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to leq
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return is_less_or_equal(CONV_SIGNED(L, length),
                CONV_SIGNED(R, length)); -- pragma label leq
    end;


    function "<="(L: SIGNED; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to leq
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return is_less_or_equal(CONV_SIGNED(L, length),
                CONV_SIGNED(R, length)); -- pragma label leq
    end;


    function "<="(L: UNSIGNED; R: INTEGER) return BOOLEAN is
    -- pragma label_applies_to leq
    constant length: INTEGER := L'length + 1;
    begin
    return is_less_or_equal(CONV_SIGNED(L, length),
                CONV_SIGNED(R, length)); -- pragma label leq
    end;


    function "<="(L: INTEGER; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to leq
    constant length: INTEGER := R'length + 1;
    begin
    return is_less_or_equal(CONV_SIGNED(L, length),
                CONV_SIGNED(R, length)); -- pragma label leq
    end;


    function "<="(L: SIGNED; R: INTEGER) return BOOLEAN is
    -- pragma label_applies_to leq
    constant length: INTEGER := L'length;
    begin
    return is_less_or_equal(CONV_SIGNED(L, length),
                CONV_SIGNED(R, length)); -- pragma label leq
    end;


    function "<="(L: INTEGER; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to leq
    constant length: INTEGER := R'length;
    begin
    return is_less_or_equal(CONV_SIGNED(L, length),
                CONV_SIGNED(R, length)); -- pragma label leq
    end;




    function ">"(L: UNSIGNED; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to gt
    constant length: INTEGER := max(L'length, R'length);
    begin
    return unsigned_is_less(CONV_UNSIGNED(R, length),
                CONV_UNSIGNED(L, length)); -- pragma label gt
    end;


    function ">"(L: SIGNED; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to gt
    constant length: INTEGER := max(L'length, R'length);
    begin
    return is_less(CONV_SIGNED(R, length),
               CONV_SIGNED(L, length)); -- pragma label gt
    end;


    function ">"(L: UNSIGNED; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to gt
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return is_less(CONV_SIGNED(R, length),
               CONV_SIGNED(L, length)); -- pragma label gt
    end;


    function ">"(L: SIGNED; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to gt
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return is_less(CONV_SIGNED(R, length),
               CONV_SIGNED(L, length)); -- pragma label gt
    end;


    function ">"(L: UNSIGNED; R: INTEGER) return BOOLEAN is
    -- pragma label_applies_to gt
    constant length: INTEGER := L'length + 1;
    begin
    return is_less(CONV_SIGNED(R, length),
               CONV_SIGNED(L, length)); -- pragma label gt
    end;


    function ">"(L: INTEGER; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to gt
    constant length: INTEGER := R'length + 1;
    begin
    return is_less(CONV_SIGNED(R, length),
               CONV_SIGNED(L, length)); -- pragma label gt
    end;


    function ">"(L: SIGNED; R: INTEGER) return BOOLEAN is
    -- pragma label_applies_to gt
    constant length: INTEGER := L'length;
    begin
    return is_less(CONV_SIGNED(R, length),
               CONV_SIGNED(L, length)); -- pragma label gt
    end;


    function ">"(L: INTEGER; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to gt
    constant length: INTEGER := R'length;
    begin
    return is_less(CONV_SIGNED(R, length),
               CONV_SIGNED(L, length)); -- pragma label gt
    end;




    function ">="(L: UNSIGNED; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to geq
    constant length: INTEGER := max(L'length, R'length);
    begin
    return unsigned_is_less_or_equal(CONV_UNSIGNED(R, length),
                 CONV_UNSIGNED(L, length)); -- pragma label geq
    end;


    function ">="(L: SIGNED; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to geq
    constant length: INTEGER := max(L'length, R'length);
    begin
    return is_less_or_equal(CONV_SIGNED(R, length),
                CONV_SIGNED(L, length)); -- pragma label geq
    end;


    function ">="(L: UNSIGNED; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to geq
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return is_less_or_equal(CONV_SIGNED(R, length),
                CONV_SIGNED(L, length)); -- pragma label geq
    end;


    function ">="(L: SIGNED; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to geq
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return is_less_or_equal(CONV_SIGNED(R, length),
                CONV_SIGNED(L, length)); -- pragma label geq
    end;


    function ">="(L: UNSIGNED; R: INTEGER) return BOOLEAN is
    -- pragma label_applies_to geq
    constant length: INTEGER := L'length + 1;
    begin
    return is_less_or_equal(CONV_SIGNED(R, length),
                CONV_SIGNED(L, length)); -- pragma label geq
    end;


    function ">="(L: INTEGER; R: UNSIGNED) return BOOLEAN is
    -- pragma label_applies_to geq
    constant length: INTEGER := R'length + 1;
    begin
    return is_less_or_equal(CONV_SIGNED(R, length),
                CONV_SIGNED(L, length)); -- pragma label geq
    end;


    function ">="(L: SIGNED; R: INTEGER) return BOOLEAN is
    -- pragma label_applies_to geq
    constant length: INTEGER := L'length;
    begin
    return is_less_or_equal(CONV_SIGNED(R, length),
                CONV_SIGNED(L, length)); -- pragma label geq
    end;


    function ">="(L: INTEGER; R: SIGNED) return BOOLEAN is
    -- pragma label_applies_to geq
    constant length: INTEGER := R'length;
    begin
    return is_less_or_equal(CONV_SIGNED(R, length),
                CONV_SIGNED(L, length)); -- pragma label geq
    end;




    -- for internal use only.  Assumes SIGNED arguments of equal length.
    function bitwise_eql(L: STD_ULOGIC_VECTOR; R: STD_ULOGIC_VECTOR)
                        return BOOLEAN is
    -- pragma built_in SYN_EQL
    begin
    for i in L'range loop
        if L(i) /= R(i) then
        return FALSE;
        end if;
    end loop;
    return TRUE;
    end;

    -- for internal use only.  Assumes SIGNED arguments of equal length.
    function bitwise_neq(L: STD_ULOGIC_VECTOR; R: STD_ULOGIC_VECTOR)
                        return BOOLEAN is
    -- pragma built_in SYN_NEQ
    begin
    for i in L'range loop
        if L(i) /= R(i) then
        return TRUE;
        end if;
    end loop;
    return FALSE;
    end;


    function "="(L: UNSIGNED; R: UNSIGNED) return BOOLEAN is
    constant length: INTEGER := max(L'length, R'length);
    begin
    return bitwise_eql( STD_ULOGIC_VECTOR( CONV_UNSIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_UNSIGNED(R, length) ) );
    end;


    function "="(L: SIGNED; R: SIGNED) return BOOLEAN is
    constant length: INTEGER := max(L'length, R'length);
    begin
    return bitwise_eql( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "="(L: UNSIGNED; R: SIGNED) return BOOLEAN is
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return bitwise_eql( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "="(L: SIGNED; R: UNSIGNED) return BOOLEAN is
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return bitwise_eql( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "="(L: UNSIGNED; R: INTEGER) return BOOLEAN is
    constant length: INTEGER := L'length + 1;
    begin
    return bitwise_eql( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "="(L: INTEGER; R: UNSIGNED) return BOOLEAN is
    constant length: INTEGER := R'length + 1;
    begin
    return bitwise_eql( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "="(L: SIGNED; R: INTEGER) return BOOLEAN is
    constant length: INTEGER := L'length;
    begin
    return bitwise_eql( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "="(L: INTEGER; R: SIGNED) return BOOLEAN is
    constant length: INTEGER := R'length;
    begin
    return bitwise_eql( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;




    function "/="(L: UNSIGNED; R: UNSIGNED) return BOOLEAN is
    constant length: INTEGER := max(L'length, R'length);
    begin
    return bitwise_neq( STD_ULOGIC_VECTOR( CONV_UNSIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_UNSIGNED(R, length) ) );
    end;


    function "/="(L: SIGNED; R: SIGNED) return BOOLEAN is
    constant length: INTEGER := max(L'length, R'length);
    begin
    return bitwise_neq( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "/="(L: UNSIGNED; R: SIGNED) return BOOLEAN is
    constant length: INTEGER := max(L'length + 1, R'length);
    begin
    return bitwise_neq( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "/="(L: SIGNED; R: UNSIGNED) return BOOLEAN is
    constant length: INTEGER := max(L'length, R'length + 1);
    begin
    return bitwise_neq( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "/="(L: UNSIGNED; R: INTEGER) return BOOLEAN is
    constant length: INTEGER := L'length + 1;
    begin
    return bitwise_neq( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "/="(L: INTEGER; R: UNSIGNED) return BOOLEAN is
    constant length: INTEGER := R'length + 1;
    begin
    return bitwise_neq( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "/="(L: SIGNED; R: INTEGER) return BOOLEAN is
    constant length: INTEGER := L'length;
    begin
    return bitwise_neq( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;


    function "/="(L: INTEGER; R: SIGNED) return BOOLEAN is
    constant length: INTEGER := R'length;
    begin
    return bitwise_neq( STD_ULOGIC_VECTOR( CONV_SIGNED(L, length) ),
        STD_ULOGIC_VECTOR( CONV_SIGNED(R, length) ) );
    end;



    function SHL(ARG: UNSIGNED; COUNT: UNSIGNED) return UNSIGNED is
    constant control_msb: INTEGER := COUNT'length - 1;
    variable control: UNSIGNED (control_msb downto 0);
    constant result_msb: INTEGER := ARG'length-1;
    subtype rtype is UNSIGNED (result_msb downto 0);
    variable result, temp: rtype;
    begin
    control := MAKE_BINARY(COUNT);
    if (control(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := ARG;
    for i in 0 to control_msb loop
        if control(i) = '1' then
        temp := rtype'(others => '0');
        if 2**i <= result_msb then
            temp(result_msb downto 2**i) := 
                    result(result_msb - 2**i downto 0);
        end if;
        result := temp;
        end if;
    end loop;
    return result;
    end;

    function SHL(ARG: SIGNED; COUNT: UNSIGNED) return SIGNED is
    constant control_msb: INTEGER := COUNT'length - 1;
    variable control: UNSIGNED (control_msb downto 0);
    constant result_msb: INTEGER := ARG'length-1;
    subtype rtype is SIGNED (result_msb downto 0);
    variable result, temp: rtype;
    begin
    control := MAKE_BINARY(COUNT);
    if (control(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := ARG;
    for i in 0 to control_msb loop
        if control(i) = '1' then
        temp := rtype'(others => '0');
        if 2**i <= result_msb then
            temp(result_msb downto 2**i) := 
                    result(result_msb - 2**i downto 0);
        end if;
        result := temp;
        end if;
    end loop;
    return result;
    end;


    function SHR(ARG: UNSIGNED; COUNT: UNSIGNED) return UNSIGNED is
    constant control_msb: INTEGER := COUNT'length - 1;
    variable control: UNSIGNED (control_msb downto 0);
    constant result_msb: INTEGER := ARG'length-1;
    subtype rtype is UNSIGNED (result_msb downto 0);
    variable result, temp: rtype;
    begin
    control := MAKE_BINARY(COUNT);
    if (control(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := ARG;
    for i in 0 to control_msb loop
        if control(i) = '1' then
        temp := rtype'(others => '0');
        if 2**i <= result_msb then
            temp(result_msb - 2**i downto 0) := 
                    result(result_msb downto 2**i);
        end if;
        result := temp;
        end if;
    end loop;
    return result;
    end;

    function SHR(ARG: SIGNED; COUNT: UNSIGNED) return SIGNED is
    constant control_msb: INTEGER := COUNT'length - 1;
    variable control: UNSIGNED (control_msb downto 0);
    constant result_msb: INTEGER := ARG'length-1;
    subtype rtype is SIGNED (result_msb downto 0);
    variable result, temp: rtype;
    variable sign_bit: STD_ULOGIC;
    begin
    control := MAKE_BINARY(COUNT);
    if (control(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := ARG;
    sign_bit := ARG(ARG'left);
    for i in 0 to control_msb loop
        if control(i) = '1' then
        temp := rtype'(others => sign_bit);
        if 2**i <= result_msb then
            temp(result_msb - 2**i downto 0) := 
                    result(result_msb downto 2**i);
        end if;
        result := temp;
        end if;
    end loop;
    return result;
    end;




    function CONV_INTEGER(ARG: INTEGER) return INTEGER is
    begin
    return ARG;
    end;

    function CONV_INTEGER(ARG: UNSIGNED) return INTEGER is
    variable result: INTEGER;
    variable tmp: STD_ULOGIC;
    -- synopsys built_in SYN_UNSIGNED_TO_INTEGER
    begin
    -- synopsys synthesis_off
    assert ARG'length <= 31
        report "ARG is too large in CONV_INTEGER"
        severity FAILURE;
    result := 0;
    for i in ARG'range loop
        result := result * 2;
        tmp := tbl_BINARY(ARG(i));
        if tmp = '1' then
        result := result + 1;
        elsif tmp = 'X' then
        assert false
        report "CONV_INTEGER: There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, and it has been converted to 0."
        severity WARNING;
        end if;
    end loop;
    return result;
    -- synopsys synthesis_on
    end;


    function CONV_INTEGER(ARG: SIGNED) return INTEGER is
    variable result: INTEGER;
    variable tmp: STD_ULOGIC;
    -- synopsys built_in SYN_SIGNED_TO_INTEGER
    begin
    -- synopsys synthesis_off
    assert ARG'length <= 32
        report "ARG is too large in CONV_INTEGER"
        severity FAILURE;
    result := 0;
    for i in ARG'range loop
        if i /= ARG'left then
        result := result * 2;
            tmp := tbl_BINARY(ARG(i));
            if tmp = '1' then
            result := result + 1;
            elsif tmp = 'X' then
            assert false
            report "CONV_INTEGER: There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, and it has been converted to 0."
            severity WARNING;
            end if;
        end if;
    end loop;
    tmp := MAKE_BINARY(ARG(ARG'left));
    if tmp = '1' then
        if ARG'length = 32 then
        result := (result - 2**30) - 2**30;
        else
        result := result - (2 ** (ARG'length-1));
        end if;
    end if;
    return result;
    -- synopsys synthesis_on
    end;


    function CONV_INTEGER(ARG: STD_ULOGIC) return SMALL_INT is
    variable tmp: STD_ULOGIC;
    -- synopsys built_in SYN_FEED_THRU
    begin
    -- synopsys synthesis_off
    tmp := tbl_BINARY(ARG);
    if tmp = '1' then
        return 1;
    elsif tmp = 'X' then
        assert false
        report "CONV_INTEGER: There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, and it has been converted to 0."
        severity WARNING;
        return 0;
    else
        return 0;
    end if;
    -- synopsys synthesis_on
    end;


    -- convert an integer to a unsigned STD_ULOGIC_VECTOR
    function CONV_UNSIGNED(ARG: INTEGER; SIZE: INTEGER) return UNSIGNED is
    variable result: UNSIGNED(SIZE-1 downto 0);
    variable temp: integer;
    -- synopsys built_in SYN_INTEGER_TO_UNSIGNED
    begin
    -- synopsys synthesis_off
    temp := ARG;
    for i in 0 to SIZE-1 loop
        if (temp mod 2) = 1 then
        result(i) := '1';
        else 
        result(i) := '0';
        end if;
        if temp > 0 then
        temp := temp / 2;
        else
        temp := (temp - 1) / 2; -- simulate ASR
        end if;
    end loop;
    return result;
    -- synopsys synthesis_on
    end;


    function CONV_UNSIGNED(ARG: UNSIGNED; SIZE: INTEGER) return UNSIGNED is
    constant msb: INTEGER := min(ARG'length, SIZE) - 1;
    subtype rtype is UNSIGNED (SIZE-1 downto 0);
    variable new_bounds: UNSIGNED (ARG'length-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_ZERO_EXTEND
    begin
    -- synopsys synthesis_off
    new_bounds := MAKE_BINARY(ARG);
    if (new_bounds(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := rtype'(others => '0');
    result(msb downto 0) := new_bounds(msb downto 0);
    return result;
    -- synopsys synthesis_on
    end;


    function CONV_UNSIGNED(ARG: SIGNED; SIZE: INTEGER) return UNSIGNED is
    constant msb: INTEGER := min(ARG'length, SIZE) - 1;
    subtype rtype is UNSIGNED (SIZE-1 downto 0);
    variable new_bounds: UNSIGNED (ARG'length-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_SIGN_EXTEND
    begin
    -- synopsys synthesis_off
    new_bounds := MAKE_BINARY(ARG);
    if (new_bounds(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := rtype'(others => new_bounds(new_bounds'left));
    result(msb downto 0) := new_bounds(msb downto 0);
    return result;
    -- synopsys synthesis_on
    end;


    function CONV_UNSIGNED(ARG: STD_ULOGIC; SIZE: INTEGER) return UNSIGNED is
    subtype rtype is UNSIGNED (SIZE-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_ZERO_EXTEND
    begin
    -- synopsys synthesis_off
    result := rtype'(others => '0');
    result(0) := MAKE_BINARY(ARG);
    if (result(0) = 'X') then
        result := rtype'(others => 'X');
    end if;
    return result;
    -- synopsys synthesis_on
    end;


    -- convert an integer to a 2's complement STD_ULOGIC_VECTOR
    function CONV_SIGNED(ARG: INTEGER; SIZE: INTEGER) return SIGNED is
    variable result: SIGNED (SIZE-1 downto 0);
    variable temp: integer;
    -- synopsys built_in SYN_INTEGER_TO_SIGNED
    begin
    -- synopsys synthesis_off
    temp := ARG;
    for i in 0 to SIZE-1 loop
        if (temp mod 2) = 1 then
        result(i) := '1';
        else 
        result(i) := '0';
        end if;
        if temp > 0 then
        temp := temp / 2;
        else
        temp := (temp - 1) / 2; -- simulate ASR
        end if;
    end loop;
    return result;
    -- synopsys synthesis_on
    end;


    function CONV_SIGNED(ARG: UNSIGNED; SIZE: INTEGER) return SIGNED is
    constant msb: INTEGER := min(ARG'length, SIZE) - 1;
    subtype rtype is SIGNED (SIZE-1 downto 0);
    variable new_bounds : SIGNED (ARG'length-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_ZERO_EXTEND
    begin
    -- synopsys synthesis_off
    new_bounds := MAKE_BINARY(ARG);
    if (new_bounds(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := rtype'(others => '0');
    result(msb downto 0) := new_bounds(msb downto 0);
    return result;
    -- synopsys synthesis_on
    end;

    function CONV_SIGNED(ARG: SIGNED; SIZE: INTEGER) return SIGNED is
    constant msb: INTEGER := min(ARG'length, SIZE) - 1;
    subtype rtype is SIGNED (SIZE-1 downto 0);
    variable new_bounds : SIGNED (ARG'length-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_SIGN_EXTEND
    begin
    -- synopsys synthesis_off
    new_bounds := MAKE_BINARY(ARG);
    if (new_bounds(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := rtype'(others => new_bounds(new_bounds'left));
    result(msb downto 0) := new_bounds(msb downto 0);
    return result;
    -- synopsys synthesis_on
    end;


    function CONV_SIGNED(ARG: STD_ULOGIC; SIZE: INTEGER) return SIGNED is
    subtype rtype is SIGNED (SIZE-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_ZERO_EXTEND
    begin
    -- synopsys synthesis_off
    result := rtype'(others => '0');
    result(0) := MAKE_BINARY(ARG);
    if (result(0) = 'X') then
        result := rtype'(others => 'X');
    end if;
    return result;
    -- synopsys synthesis_on
    end;


    -- convert an integer to an STD_LOGIC_VECTOR
    function CONV_STD_LOGIC_VECTOR(ARG: INTEGER; SIZE: INTEGER) return STD_LOGIC_VECTOR is
    variable result: STD_LOGIC_VECTOR (SIZE-1 downto 0);
    variable temp: integer;
    -- synopsys built_in SYN_INTEGER_TO_SIGNED
    begin
    -- synopsys synthesis_off
    temp := ARG;
    for i in 0 to SIZE-1 loop
        if (temp mod 2) = 1 then
        result(i) := '1';
        else 
        result(i) := '0';
        end if;
        if temp > 0 then
        temp := temp / 2;
        else
        temp := (temp - 1) / 2; -- simulate ASR
        end if;
    end loop;
    return result;
    -- synopsys synthesis_on
    end;


    function CONV_STD_LOGIC_VECTOR(ARG: UNSIGNED; SIZE: INTEGER) return STD_LOGIC_VECTOR is
    constant msb: INTEGER := min(ARG'length, SIZE) - 1;
    subtype rtype is STD_LOGIC_VECTOR (SIZE-1 downto 0);
    variable new_bounds : STD_LOGIC_VECTOR (ARG'length-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_ZERO_EXTEND
    begin
    -- synopsys synthesis_off
    new_bounds := MAKE_BINARY(ARG);
    if (new_bounds(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := rtype'(others => '0');
    result(msb downto 0) := new_bounds(msb downto 0);
    return result;
    -- synopsys synthesis_on
    end;

    function CONV_STD_LOGIC_VECTOR(ARG: SIGNED; SIZE: INTEGER) return STD_LOGIC_VECTOR is
    constant msb: INTEGER := min(ARG'length, SIZE) - 1;
    subtype rtype is STD_LOGIC_VECTOR (SIZE-1 downto 0);
    variable new_bounds : STD_LOGIC_VECTOR (ARG'length-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_SIGN_EXTEND
    begin
    -- synopsys synthesis_off
    new_bounds := MAKE_BINARY(ARG);
    if (new_bounds(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := rtype'(others => new_bounds(new_bounds'left));
    result(msb downto 0) := new_bounds(msb downto 0);
    return result;
    -- synopsys synthesis_on
    end;


    function CONV_STD_LOGIC_VECTOR(ARG: STD_ULOGIC; SIZE: INTEGER) return STD_LOGIC_VECTOR is
    subtype rtype is STD_LOGIC_VECTOR (SIZE-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_ZERO_EXTEND
    begin
    -- synopsys synthesis_off
    result := rtype'(others => '0');
    result(0) := MAKE_BINARY(ARG);
    if (result(0) = 'X') then
        result := rtype'(others => 'X');
    end if;
    return result;
    -- synopsys synthesis_on
    end;

    function EXT(ARG: STD_LOGIC_VECTOR; SIZE: INTEGER) 
                        return STD_LOGIC_VECTOR is
    constant msb: INTEGER := min(ARG'length, SIZE) - 1;
    subtype rtype is STD_LOGIC_VECTOR (SIZE-1 downto 0);
    variable new_bounds: STD_LOGIC_VECTOR (ARG'length-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_ZERO_EXTEND
    begin
    -- synopsys synthesis_off
    new_bounds := MAKE_BINARY(ARG);
    if (new_bounds(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := rtype'(others => '0');
    result(msb downto 0) := new_bounds(msb downto 0);
    return result;
    -- synopsys synthesis_on
    end;


    function SXT(ARG: STD_LOGIC_VECTOR; SIZE: INTEGER) return STD_LOGIC_VECTOR is
    constant msb: INTEGER := min(ARG'length, SIZE) - 1;
    subtype rtype is STD_LOGIC_VECTOR (SIZE-1 downto 0);
    variable new_bounds : STD_LOGIC_VECTOR (ARG'length-1 downto 0);
    variable result: rtype;
    -- synopsys built_in SYN_SIGN_EXTEND
    begin
    -- synopsys synthesis_off
    new_bounds := MAKE_BINARY(ARG);
    if (new_bounds(0) = 'X') then
        result := rtype'(others => 'X');
        return result;
    end if;
    result := rtype'(others => new_bounds(new_bounds'left));
    result(msb downto 0) := new_bounds(msb downto 0);
    return result;
    -- synopsys synthesis_on
    end;


end std_logic_arith;
