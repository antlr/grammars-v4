--------------------------------------------------------------------------
--                                                                      --
-- Copyright (c) 1990, 1991, 1992 by Synopsys, Inc.                     --
--                                             All rights reserved.     --
--                                                                      --
-- This source file may be used and distributed without restriction     --
-- provided that this copyright statement is not removed from the file  --
-- and that any derivative work contains this copyright notice.         --
--                                                                      --
--	Package name: STD_LOGIC_UNSIGNED                                --
--                                 					--
--									--
--      Date:        	09/11/92	KN				--
--			10/08/92 	AMT				--
--									--
--	Purpose: 							--
--	 A set of unsigned arithemtic, conversion,                      --
--           and comparision functions for STD_LOGIC_VECTOR.            --
--									--
--	Note:  comparision of same length discrete arrays is defined	--
--		by the LRM.  This package will "overload" those 	--
--		definitions						--
--									--
--------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

package STD_LOGIC_UNSIGNED is

    function "+"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function "+"(L: STD_LOGIC_VECTOR; R: INTEGER) return STD_LOGIC_VECTOR;

    function "+"(L: INTEGER; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function "+"(L: STD_LOGIC_VECTOR; R: STD_LOGIC) return STD_LOGIC_VECTOR;

    function "+"(L: STD_LOGIC; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function "-"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function "-"(L: STD_LOGIC_VECTOR; R: INTEGER) return STD_LOGIC_VECTOR;

    function "-"(L: INTEGER; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function "-"(L: STD_LOGIC_VECTOR; R: STD_LOGIC) return STD_LOGIC_VECTOR;

    function "-"(L: STD_LOGIC; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function "+"(L: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function "*"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function "<"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function "<"(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN;

    function "<"(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function "<="(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function "<="(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN;

    function "<="(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function ">"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function ">"(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN;

    function ">"(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function ">="(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function ">="(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN;

    function ">="(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function "="(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function "="(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN;

    function "="(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function "/="(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function "/="(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN;

    function "/="(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN;

    function CONV_INTEGER(ARG: STD_LOGIC_VECTOR) return INTEGER;

    function SHL(ARG:STD_LOGIC_VECTOR;COUNT: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function SHR(ARG:STD_LOGIC_VECTOR;COUNT: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;

end STD_LOGIC_UNSIGNED;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

package body STD_LOGIC_UNSIGNED is


    function maximum(L, R: INTEGER) return INTEGER is
    begin
        if L > R then
            return L;
        else
            return R;
        end if;
    end;


    function "+"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
        constant length: INTEGER := maximum(L'length, R'length);
        variable result  : STD_LOGIC_VECTOR (length-1 downto 0);
    begin
        result  := UNSIGNED(L) + UNSIGNED(R);
        return   std_logic_vector(result);
    end;

    function "+"(L: STD_LOGIC_VECTOR; R: INTEGER) return STD_LOGIC_VECTOR is
        variable result  : STD_LOGIC_VECTOR (L'range);
    begin
        result  := UNSIGNED(L) + R;
        return   std_logic_vector(result);
    end;

    function "+"(L: INTEGER; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
        variable result  : STD_LOGIC_VECTOR (R'range);
    begin
        result  := L + UNSIGNED(R);
        return   std_logic_vector(result);
    end;

    function "+"(L: STD_LOGIC_VECTOR; R: STD_LOGIC) return STD_LOGIC_VECTOR is
        variable result  : STD_LOGIC_VECTOR (L'range);
    begin
        result  := UNSIGNED(L) + R;
        return   std_logic_vector(result);
    end;

    function "+"(L: STD_LOGIC; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
        variable result  : STD_LOGIC_VECTOR (R'range);
    begin
        result  := L + UNSIGNED(R);
        return   std_logic_vector(result);
    end;

    function "-"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
        constant length: INTEGER := maximum(L'length, R'length);
        variable result  : STD_LOGIC_VECTOR (length-1 downto 0);
    begin
        result  := UNSIGNED(L) - UNSIGNED(R);
        return   std_logic_vector(result);
    end;

    function "-"(L: STD_LOGIC_VECTOR; R: INTEGER) return STD_LOGIC_VECTOR is
        variable result  : STD_LOGIC_VECTOR (L'range);
    begin
        result  := UNSIGNED(L) - R;
        return   std_logic_vector(result);
    end;

    function "-"(L: INTEGER; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
        variable result  : STD_LOGIC_VECTOR (R'range);
    begin
        result  := L - UNSIGNED(R);
        return   std_logic_vector(result);
    end;

    function "-"(L: STD_LOGIC_VECTOR; R: STD_LOGIC) return STD_LOGIC_VECTOR is
        variable result  : STD_LOGIC_VECTOR (L'range);
    begin
        result  := UNSIGNED(L) - R;
        return   std_logic_vector(result);
    end;

    function "-"(L: STD_LOGIC; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
        variable result  : STD_LOGIC_VECTOR (R'range);
    begin
        result  := L - UNSIGNED(R);
        return   std_logic_vector(result);
    end;

    function "+"(L: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
        variable result  : STD_LOGIC_VECTOR (L'range);
    begin
        result  := + UNSIGNED(L);
        return   std_logic_vector(result);
    end;

    function "*"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
        constant length: INTEGER := maximum(L'length, R'length);
        variable result  : STD_LOGIC_VECTOR ((L'length+R'length-1) downto 0);
    begin
        result  := UNSIGNED(L) * UNSIGNED(R);
        return   std_logic_vector(result);
    end;
        
    function "<"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN is
        constant length: INTEGER := maximum(L'length, R'length);
    begin
        return   UNSIGNED(L) < UNSIGNED(R);
    end;

    function "<"(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN is
    begin
        return   UNSIGNED(L) < R;
    end;

    function "<"(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   L < UNSIGNED(R);
    end;

    function "<="(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   UNSIGNED(L) <= UNSIGNED(R);
    end;

    function "<="(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN is
    begin
        return   UNSIGNED(L) <= R;
    end;

    function "<="(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   L <= UNSIGNED(R);
    end;

    function ">"(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   UNSIGNED(L) > UNSIGNED(R);
    end;

    function ">"(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN is
    begin
        return   UNSIGNED(L) > R;
    end;

    function ">"(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   L > UNSIGNED(R);
    end;

    function ">="(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   UNSIGNED(L) >= UNSIGNED(R);
    end;

    function ">="(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN is
    begin
        return   UNSIGNED(L) >= R;
    end;

    function ">="(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   L >= UNSIGNED(R);
    end;

    function "="(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   UNSIGNED(L) = UNSIGNED(R);
    end;

    function "="(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN is
    begin
        return   UNSIGNED(L) = R;
    end;

    function "="(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   L = UNSIGNED(R);
    end;

    function "/="(L: STD_LOGIC_VECTOR; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   UNSIGNED(L) /= UNSIGNED(R);
    end;

    function "/="(L: STD_LOGIC_VECTOR; R: INTEGER) return BOOLEAN is
    begin
        return   UNSIGNED(L) /= R;
    end;

    function "/="(L: INTEGER; R: STD_LOGIC_VECTOR) return BOOLEAN is
    begin
        return   L /= UNSIGNED(R);
    end;

    function CONV_INTEGER(ARG: STD_LOGIC_VECTOR) return INTEGER is
        variable result    : UNSIGNED(ARG'range);
    begin
        result    := UNSIGNED(ARG);
        return   CONV_INTEGER(result);
    end;

    function SHL(ARG:STD_LOGIC_VECTOR;COUNT: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    begin
       return STD_LOGIC_VECTOR(SHL(UNSIGNED(ARG),UNSIGNED(COUNT)));
    end;
 
    function SHR(ARG:STD_LOGIC_VECTOR;COUNT: STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    begin
       return STD_LOGIC_VECTOR(SHR(UNSIGNED(ARG),UNSIGNED(COUNT)));
    end;

-- remove this since it is already in std_logic_arith
    --function CONV_STD_LOGIC_VECTOR(ARG: INTEGER; SIZE: INTEGER) return STD_LOGIC_VECTOR is
        --variable result1 : UNSIGNED (SIZE-1 downto 0);
        --variable result2 : STD_LOGIC_VECTOR (SIZE-1 downto 0);
    --begin
        --result1 := CONV_UNSIGNED(ARG,SIZE);
        --return   std_logic_vector(result1);
    --end;


end STD_LOGIC_UNSIGNED;
