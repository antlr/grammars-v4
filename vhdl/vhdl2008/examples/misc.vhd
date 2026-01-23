--------------------------------------------------------------------------
--
-- Copyright (c) 1990, 1991, 1992 by Synopsys, Inc.  All rights reserved.
-- 
-- This source file may be used and distributed without restriction 
-- provided that this copyright statement is not removed from the file 
-- and that any derivative work contains this copyright notice.
--
--	Package name: std_logic_misc
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions for the Std_logic_1164 Package.
--
--	Author:  GWH
--
--------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
--library SYNOPSYS;
--use SYNOPSYS.attributes.all;



package std_logic_misc is

    -- output-strength types

    type STRENGTH is (strn_X01, strn_X0H, strn_XL1, strn_X0Z, strn_XZ1, 
		      strn_WLH, strn_WLZ, strn_WZH, strn_W0H, strn_WL1);


--synopsys synthesis_off

    type MINOMAX is array (1 to 3) of TIME;
    

    ---------------------------------------------------------------------
    --
    -- functions for mapping the STD_(U)LOGIC according to STRENGTH
    --
    ---------------------------------------------------------------------

    function strength_map(input: STD_ULOGIC; strn: STRENGTH) return STD_LOGIC;

    function strength_map_z(input:STD_ULOGIC; strn:STRENGTH) return STD_LOGIC;

    ---------------------------------------------------------------------
    --
    -- conversion functions for STD_ULOGIC_VECTOR and STD_LOGIC_VECTOR
    --
    ---------------------------------------------------------------------

--synopsys synthesis_on
    function Drive (V: STD_ULOGIC_VECTOR) return STD_LOGIC_VECTOR;

    function Drive (V: STD_LOGIC_VECTOR) return STD_ULOGIC_VECTOR;
--synopsys synthesis_off

    
    --attribute CLOSELY_RELATED_TCF of Drive: function is TRUE;

    ---------------------------------------------------------------------
    --
    -- conversion functions for sensing various types
    -- (the second argument allows the user to specify the value to
    --  be returned when the network is undriven)
    --
    ---------------------------------------------------------------------

    function Sense (V: STD_ULOGIC; vZ, vU, vDC: STD_ULOGIC) return STD_LOGIC;

    function Sense (V: STD_ULOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					   return STD_LOGIC_VECTOR;
    function Sense (V: STD_ULOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					   return STD_ULOGIC_VECTOR;

    function Sense (V: STD_LOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					   return STD_LOGIC_VECTOR;
    function Sense (V: STD_LOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					   return STD_ULOGIC_VECTOR;

--synopsys synthesis_on


    ---------------------------------------------------------------------
    --
    --	Function: STD_LOGIC_VECTORtoBIT_VECTOR STD_ULOGIC_VECTORtoBIT_VECTOR
    --
    --	Purpose: Conversion fun. from STD_(U)LOGIC_VECTOR to BIT_VECTOR
    --
    --	Mapping:	0, L --> 0
    --			1, H --> 1
    --			X, W --> vX if Xflag is TRUE
    --			X, W --> 0  if Xflag is FALSE
    --			Z --> vZ if Zflag is TRUE
    --			Z --> 0  if Zflag is FALSE
    --			U --> vU if Uflag is TRUE
    --			U --> 0  if Uflag is FALSE
    --			- --> vDC if DCflag is TRUE
    --			- --> 0  if DCflag is FALSE
    --
    ---------------------------------------------------------------------

    function STD_LOGIC_VECTORtoBIT_VECTOR (V: STD_LOGIC_VECTOR
--synopsys synthesis_off
    	; vX, vZ, vU, vDC: BIT := '0'; 
    	  Xflag, Zflag, Uflag, DCflag: BOOLEAN := FALSE
--synopsys synthesis_on
    	) return BIT_VECTOR;

    function STD_ULOGIC_VECTORtoBIT_VECTOR (V: STD_ULOGIC_VECTOR
--synopsys synthesis_off
    	; vX, vZ, vU, vDC: BIT := '0'; 
    	  Xflag, Zflag, Uflag, DCflag: BOOLEAN := FALSE
--synopsys synthesis_on
    	) return BIT_VECTOR;
    

    ---------------------------------------------------------------------
    --
    --	Function: STD_ULOGICtoBIT
    --
    --	Purpose: Conversion function from STD_(U)LOGIC to BIT
    --
    --	Mapping:	0, L --> 0
    --			1, H --> 1
    --			X, W --> vX if Xflag is TRUE
    --			X, W --> 0  if Xflag is FALSE
    --			Z --> vZ if Zflag is TRUE
    --			Z --> 0  if Zflag is FALSE
    --			U --> vU if Uflag is TRUE
    --			U --> 0  if Uflag is FALSE
    --			- --> vDC if DCflag is TRUE
    --			- --> 0  if DCflag is FALSE
    --
    ---------------------------------------------------------------------

    function STD_ULOGICtoBIT (V: STD_ULOGIC
--synopsys synthesis_off
    	; vX, vZ, vU, vDC: BIT := '0'; 
    	  Xflag, Zflag, Uflag, DCflag: BOOLEAN := FALSE
--synopsys synthesis_on
    	) return BIT;

        --------------------------------------------------------------------
        function AND_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function NAND_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function OR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function NOR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function XOR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
        function XNOR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01;
    
        function AND_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function NAND_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function OR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function NOR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function XOR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
        function XNOR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01;
    
--synopsys synthesis_off
	
        function fun_BUF3S(Input, Enable: UX01; Strn: STRENGTH) return STD_LOGIC;
        function fun_BUF3SL(Input, Enable: UX01; Strn: STRENGTH) return STD_LOGIC;
        function fun_MUX2x1(Input0, Input1, Sel: UX01) return UX01;

        function fun_MAJ23(Input0, Input1, Input2: UX01) return UX01;
        function fun_WiredX(Input0, Input1: std_ulogic) return STD_LOGIC;

--synopsys synthesis_on
	
end;


package body std_logic_misc is

--synopsys synthesis_off

    type STRN_STD_ULOGIC_TABLE is array (STD_ULOGIC,STRENGTH) of STD_ULOGIC;

    --------------------------------------------------------------------
    --
    -- Truth tables for output strength --> STD_ULOGIC lookup
    --
    --------------------------------------------------------------------

    -- truth table for output strength --> STD_ULOGIC lookup
    constant tbl_STRN_STD_ULOGIC: STRN_STD_ULOGIC_TABLE := 
    --  ------------------------------------------------------------------
    --  | X01  X0H  XL1  X0Z  XZ1  WLH  WLZ  WZH  W0H  WL1 | strn/ output|
    --  ------------------------------------------------------------------
        (('U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U'),  -- |   U   |
         ('X', 'X', 'X', 'X', 'X', 'W', 'W', 'W', 'W', 'W'),  -- |   X   |
         ('0', '0', 'L', '0', 'Z', 'L', 'L', 'Z', '0', 'L'),  -- |   0   |
         ('1', 'H', '1', 'Z', '1', 'H', 'Z', 'H', 'H', '1'),  -- |   1   |
         ('X', 'X', 'X', 'X', 'X', 'W', 'W', 'W', 'W', 'W'),  -- |   Z   |
         ('X', 'X', 'X', 'X', 'X', 'W', 'W', 'W', 'W', 'W'),  -- |   W   |
         ('0', '0', 'L', '0', 'Z', 'L', 'L', 'Z', '0', 'L'),  -- |   L   |
         ('1', 'H', '1', 'Z', '1', 'H', 'Z', 'H', 'H', '1'),  -- |   H   |
         ('X', 'X', 'X', 'X', 'X', 'W', 'W', 'W', 'W', 'W')); -- |   -   |



    --------------------------------------------------------------------
    --
    -- Truth tables for strength --> STD_ULOGIC mapping ('Z' pass through)
    --
    --------------------------------------------------------------------

    -- truth table for output strength --> STD_ULOGIC lookup
    constant tbl_STRN_STD_ULOGIC_Z: STRN_STD_ULOGIC_TABLE := 
    --  ------------------------------------------------------------------
    --  | X01  X0H  XL1  X0Z  XZ1  WLH  WLZ  WZH  W0H  WL1 | strn/ output|
    --  ------------------------------------------------------------------
        (('U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U'),  -- |   U   |
         ('X', 'X', 'X', 'X', 'X', 'W', 'W', 'W', 'W', 'W'),  -- |   X   |
         ('0', '0', 'L', '0', 'Z', 'L', 'L', 'Z', '0', 'L'),  -- |   0   |
         ('1', 'H', '1', 'Z', '1', 'H', 'Z', 'H', 'H', '1'),  -- |   1   |
         ('Z', 'Z', 'Z', 'Z', 'Z', 'Z', 'Z', 'Z', 'Z', 'Z'),  -- |   Z   |
         ('X', 'X', 'X', 'X', 'X', 'W', 'W', 'W', 'W', 'W'),  -- |   W   |
         ('0', '0', 'L', '0', 'Z', 'L', 'L', 'Z', '0', 'L'),  -- |   L   |
         ('1', 'H', '1', 'Z', '1', 'H', 'Z', 'H', 'H', '1'),  -- |   H   |
         ('X', 'X', 'X', 'X', 'X', 'W', 'W', 'W', 'W', 'W')); -- |   -   |



    ---------------------------------------------------------------------
    --
    -- functions for mapping the STD_(U)LOGIC according to STRENGTH
    --
    ---------------------------------------------------------------------

    function strength_map(input: STD_ULOGIC; strn: STRENGTH) return STD_LOGIC is
	-- pragma subpgm_id 387
    begin
    	return tbl_STRN_STD_ULOGIC(input, strn);
    end strength_map;


    function strength_map_z(input:STD_ULOGIC; strn:STRENGTH) return STD_LOGIC is
	-- pragma subpgm_id 388
    begin
    	return tbl_STRN_STD_ULOGIC_Z(input, strn);
    end strength_map_z;


    ---------------------------------------------------------------------
    --
    -- conversion functions for STD_LOGIC_VECTOR and STD_ULOGIC_VECTOR
    --
    ---------------------------------------------------------------------

--synopsys synthesis_on
    function Drive (V: STD_LOGIC_VECTOR) return STD_ULOGIC_VECTOR is
      -- pragma built_in SYN_FEED_THRU
      -- pragma subpgm_id 389
--synopsys synthesis_off
        alias Value: STD_LOGIC_VECTOR (V'length-1 downto 0) is V;
--synopsys synthesis_on
    begin
--synopsys synthesis_off
    	return STD_ULOGIC_VECTOR(Value);
--synopsys synthesis_on
    end Drive;


    function Drive (V: STD_ULOGIC_VECTOR) return STD_LOGIC_VECTOR is
      -- pragma built_in SYN_FEED_THRU
      -- pragma subpgm_id 390
--synopsys synthesis_off
        alias Value: STD_ULOGIC_VECTOR (V'length-1 downto 0) is V;
--synopsys synthesis_on
    begin
--synopsys synthesis_off
    	return STD_LOGIC_VECTOR(Value);
--synopsys synthesis_on
    end Drive;
--synopsys synthesis_off


    ---------------------------------------------------------------------
    --
    -- conversion functions for sensing various types
    --
    -- (the second argument allows the user to specify the value to
    --  be returned when the network is undriven)
    --
    ---------------------------------------------------------------------

    function Sense (V: STD_ULOGIC; vZ, vU, vDC: STD_ULOGIC) 
    					        return STD_LOGIC is
	-- pragma subpgm_id 391
    begin
    	if V = 'Z' then
    		return vZ;
	elsif V = 'U' then
		return vU;
	elsif V = '-' then
		return vDC;
    	else
    		return V;
    	end if;
    end Sense;


    function Sense (V: STD_ULOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					return STD_LOGIC_VECTOR is
	-- pragma subpgm_id 392
    	alias Value: STD_ULOGIC_VECTOR (V'length-1 downto 0) is V;
    	variable Result: STD_LOGIC_VECTOR (V'length-1 downto 0);
    begin
    	for i in Value'range loop
    		if ( Value(i) = 'Z' ) then
    			Result(i) := vZ;
		elsif Value(i) = 'U' then
			Result(i) :=  vU;
		elsif Value(i) = '-' then
			Result(i) := vDC;
    		else
    			Result(i) := Value(i);
    		end if;
    	end loop;
    	return Result;
    end Sense;


    function Sense (V: STD_ULOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					return STD_ULOGIC_VECTOR is
	-- pragma subpgm_id 393
    	alias Value: STD_ULOGIC_VECTOR (V'length-1 downto 0) is V;
    	variable Result: STD_ULOGIC_VECTOR (V'length-1 downto 0);
    begin
    	for i in Value'range loop
    		if ( Value(i) = 'Z' ) then
    			Result(i) := vZ;
		elsif Value(i) = 'U' then
			Result(i) :=  vU;
		elsif Value(i) = '-' then
			Result(i) := vDC;
    		else
    			Result(i) := Value(i);
    		end if;
    	end loop;
    	return Result;
    end Sense;


    function Sense (V: STD_LOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					return STD_LOGIC_VECTOR is
	-- pragma subpgm_id 394
    	alias Value: STD_LOGIC_VECTOR (V'length-1 downto 0) is V;
    	variable Result: STD_LOGIC_VECTOR (V'length-1 downto 0);
    begin
    	for i in Value'range loop
    		if ( Value(i) = 'Z' ) then
    			Result(i) := vZ;
		elsif Value(i) = 'U' then
			Result(i) :=  vU;
		elsif Value(i) = '-' then
			Result(i) := vDC;
    		else
    			Result(i) := Value(i);
    		end if;
    	end loop;
    	return Result;
    end Sense;


    function Sense (V: STD_LOGIC_VECTOR; vZ, vU, vDC: STD_ULOGIC) 
    					return STD_ULOGIC_VECTOR is
	-- pragma subpgm_id 395
    	alias Value: STD_LOGIC_VECTOR (V'length-1 downto 0) is V;
    	variable Result: STD_ULOGIC_VECTOR (V'length-1 downto 0);
    begin
    	for i in Value'range loop
    		if ( Value(i) = 'Z' ) then
    			Result(i) := vZ;
		elsif Value(i) = 'U' then
			Result(i) :=  vU;
		elsif Value(i) = '-' then
			Result(i) := vDC;
    		else
    			Result(i) := Value(i);
    		end if;
    	end loop;
    	return Result;
    end Sense;

    ---------------------------------------------------------------------
    --
    --	Function: STD_LOGIC_VECTORtoBIT_VECTOR
    --
    --	Purpose: Conversion fun. from STD_LOGIC_VECTOR to BIT_VECTOR
    --
    --	Mapping:	0, L --> 0
    --			1, H --> 1
    --			X, W --> vX if Xflag is TRUE
    --			X, W --> 0  if Xflag is FALSE
    --			Z --> vZ if Zflag is TRUE
    --			Z --> 0  if Zflag is FALSE
    --			U --> vU if Uflag is TRUE
    --			U --> 0  if Uflag is FALSE
    --			- --> vDC if DCflag is TRUE
    --			- --> 0  if DCflag is FALSE
    --
    ---------------------------------------------------------------------

--synopsys synthesis_on
    function STD_LOGIC_VECTORtoBIT_VECTOR (V: STD_LOGIC_VECTOR
--synopsys synthesis_off
    	; vX, vZ, vU, vDC: BIT := '0'; 
    	  Xflag, Zflag, Uflag, DCflag: BOOLEAN := FALSE
--synopsys synthesis_on
    		   ) return BIT_VECTOR is
      -- pragma built_in SYN_FEED_THRU
      -- pragma subpgm_id 396
--synopsys synthesis_off
    	alias Value: STD_LOGIC_VECTOR (V'length-1 downto 0) is V;
    	variable Result: BIT_VECTOR (V'length-1 downto 0);
--synopsys synthesis_on
    begin
--synopsys synthesis_off
    	for i in Value'range loop
    		case Value(i) is
    			when '0' | 'L' =>
    				Result(i) := '0';
    			when '1' | 'H' =>
    				Result(i) := '1';
	    		when 'X' =>
    				if ( Xflag ) then
    					Result(i) := vX;
    				else
    					Result(i) := '0';
	    		 		assert FALSE
    				 	    report "STD_LOGIC_VECTORtoBIT_VECTOR: X --> 0"
    				 	    severity WARNING;
    				end if;
	    		when 'W' =>
    				if ( Xflag ) then
    					Result(i) := vX;
    				else
    					Result(i) := '0';
	    		 		assert FALSE
    				 	    report "STD_LOGIC_VECTORtoBIT_VECTOR: W --> 0"
    				 	    severity WARNING;
    				end if;
	    		when 'Z' =>
    				if ( Zflag ) then
    					Result(i) := vZ;
	    			else
    					Result(i) := '0';
    					assert FALSE
    					    report "STD_LOGIC_VECTORtoBIT_VECTOR: Z --> 0"
    					    severity WARNING;
	    			end if;
    			when 'U' =>
    				if ( Uflag ) then
    					Result(i) := vU;
	    			else
    					Result(i) := '0';
    					assert FALSE
    					    report "STD_LOGIC_VECTORtoBIT_VECTOR: U --> 0"
    					    severity WARNING;
				end if;
    			when '-' =>
    				if ( DCflag ) then
    					Result(i) := vDC;
	    			else
    					Result(i) := '0';
    					assert FALSE
    					    report "STD_LOGIC_VECTORtoBIT_VECTOR: - --> 0"
    					    severity WARNING;
	    			end if;
    			end case;
	    	end loop;
    	return Result;
--synopsys synthesis_on
    end STD_LOGIC_VECTORtoBIT_VECTOR;




    ---------------------------------------------------------------------
    --
    --	Function: STD_ULOGIC_VECTORtoBIT_VECTOR
    --
    --	Purpose: Conversion fun. from STD_ULOGIC_VECTOR to BIT_VECTOR
    --
    --	Mapping:	0, L --> 0
    --			1, H --> 1
    --			X, W --> vX if Xflag is TRUE
    --			X, W --> 0  if Xflag is FALSE
    --			Z --> vZ if Zflag is TRUE
    --			Z --> 0  if Zflag is FALSE
    --			U --> vU if Uflag is TRUE
    --			U --> 0  if Uflag is FALSE
    --			- --> vDC if DCflag is TRUE
    --			- --> 0  if DCflag is FALSE
    --
    ---------------------------------------------------------------------

    function STD_ULOGIC_VECTORtoBIT_VECTOR (V: STD_ULOGIC_VECTOR
--synopsys synthesis_off
    	; vX, vZ, vU, vDC: BIT := '0'; 
    	  Xflag, Zflag, Uflag, DCflag: BOOLEAN := FALSE
--synopsys synthesis_on
    		   ) return BIT_VECTOR is
      -- pragma built_in SYN_FEED_THRU
      -- pragma subpgm_id 397
--synopsys synthesis_off
    	alias Value: STD_ULOGIC_VECTOR (V'length-1 downto 0) is V;
    	variable Result: BIT_VECTOR (V'length-1 downto 0);
--synopsys synthesis_on
    begin
--synopsys synthesis_off
    	for i in Value'range loop
    		case Value(i) is
    			when '0' | 'L' =>
    				Result(i) := '0';
    			when '1' | 'H' =>
    				Result(i) := '1';
	    		when 'X' =>
    				if ( Xflag ) then
    					Result(i) := vX;
	    			else
    					Result(i) := '0';
	    		 		assert FALSE
    				 	    report "STD_ULOGIC_VECTORtoBIT_VECTOR: X --> 0"
    				 	    severity WARNING;
	    			end if;
    			when 'W' =>
	    			if ( Xflag ) then
    					Result(i) := vX;
    				else
    					Result(i) := '0';
	    		 		assert FALSE
    				 	    report "STD_ULOGIC_VECTORtoBIT_VECTOR: W --> 0"
    				 	    severity WARNING;
	    			end if;
    			when 'Z' =>
    				if ( Zflag ) then
	    				Result(i) := vZ;
    				else
    					Result(i) := '0';
    					assert FALSE
    					    report "STD_ULOGIC_VECTORtoBIT_VECTOR: Z --> 0"
	    				    severity WARNING;
    				end if;
	    		when 'U' =>
    				if ( Uflag ) then
    					Result(i) := vU;
	    			else
    					Result(i) := '0';
    					assert FALSE
    					    report "STD_ULOGIC_VECTORtoBIT_VECTOR: U --> 0"
    					    severity WARNING;
				end if;
    			when '-' =>
    				if ( DCflag ) then
    					Result(i) := vDC;
	    			else
    					Result(i) := '0';
    					assert FALSE
    					    report "STD_ULOGIC_VECTORtoBIT_VECTOR: - --> 0"
    					    severity WARNING;
	    			end if;
    			end case;
    	end loop;
    	return Result;
--synopsys synthesis_on
    end STD_ULOGIC_VECTORtoBIT_VECTOR;




    ---------------------------------------------------------------------
    --
    --	Function: STD_ULOGICtoBIT
    --
    --	Purpose: Conversion function from STD_ULOGIC to BIT
    --
    --	Mapping:	0, L --> 0
    --			1, H --> 1
    --			X, W --> vX if Xflag is TRUE
    --			X, W --> 0  if Xflag is FALSE
    --			Z --> vZ if Zflag is TRUE
    --			Z --> 0  if Zflag is FALSE
    --			U --> vU if Uflag is TRUE
    --			U --> 0  if Uflag is FALSE
    --			- --> vDC if DCflag is TRUE
    --			- --> 0  if DCflag is FALSE
    --
    ---------------------------------------------------------------------

    function STD_ULOGICtoBIT (V: STD_ULOGIC
--synopsys synthesis_off
    	; vX, vZ, vU, vDC: BIT := '0'; 
    	  Xflag, Zflag, Uflag, DCflag: BOOLEAN := FALSE
--synopsys synthesis_on
    		   ) return BIT is
      -- pragma built_in SYN_FEED_THRU
      -- pragma subpgm_id 398
    	variable Result: BIT;
    begin
--synopsys synthesis_off
    	case V is
    		when '0' | 'L' =>
    			Result := '0';
    		when '1' | 'H' =>
    			Result := '1';
    		when 'X' =>
    			if ( Xflag ) then
    				Result := vX;
    			else
    				Result := '0';
    		 		assert FALSE
    			 	    report "STD_ULOGICtoBIT: X --> 0"
    			 	    severity WARNING;
    			end if;
    		when 'W' =>
    			if ( Xflag ) then
    				Result := vX;
    			else
    				Result := '0';
    		 		assert FALSE
    			 	    report "STD_ULOGICtoBIT: W --> 0"
    			 	    severity WARNING;
    			end if;
    		when 'Z' =>
    			if ( Zflag ) then
    				Result := vZ;
    			else
    				Result := '0';
    				assert FALSE
    				    report "STD_ULOGICtoBIT: Z --> 0"
    				    severity WARNING;
    			end if;
    		when 'U' =>
    			if ( Uflag ) then
    				Result := vU;
    			else
    				Result := '0';
    				assert FALSE
    				    report "STD_ULOGICtoBIT: U --> 0"
    				    severity WARNING;
			end if;
    		when '-' =>
    			if ( DCflag ) then
    				Result := vDC;
    			else
    				Result := '0';
    				assert FALSE
    				    report "STD_ULOGICtoBIT: - --> 0"
    				    severity WARNING;
    			end if;
    	end case;
    	return Result;
--synopsys synthesis_on
    end STD_ULOGICtoBIT;


    --------------------------------------------------------------------------

    function AND_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 399
	variable result: STD_LOGIC;
    begin
	result := '1';
	for i in ARG'range loop
	    result := result and ARG(i);
	end loop;
        return result;
    end;

    function NAND_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 400
    begin
        return not AND_REDUCE(ARG);
    end;

    function OR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 401
	variable result: STD_LOGIC;
    begin
	result := '0';
	for i in ARG'range loop
	    result := result or ARG(i);
	end loop;
        return result;
    end;

    function NOR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 402
    begin
        return not OR_REDUCE(ARG);
    end;

    function XOR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 403
	variable result: STD_LOGIC;
    begin
	result := '0';
	for i in ARG'range loop
	    result := result xor ARG(i);
	end loop;
        return result;
    end;

    function XNOR_REDUCE(ARG: STD_LOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 404
    begin
        return not XOR_REDUCE(ARG);
    end;

    function AND_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 405
	variable result: STD_LOGIC;
    begin
	result := '1';
	for i in ARG'range loop
	    result := result and ARG(i);
	end loop;
        return result;
    end;

    function NAND_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 406
    begin
        return not AND_REDUCE(ARG);
    end;

    function OR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 407
	variable result: STD_LOGIC;
    begin
	result := '0';
	for i in ARG'range loop
	    result := result or ARG(i);
	end loop;
        return result;
    end;

    function NOR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 408
    begin
        return not OR_REDUCE(ARG);
    end;

    function XOR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 409
	variable result: STD_LOGIC;
    begin
	result := '0';
	for i in ARG'range loop
	    result := result xor ARG(i);
	end loop;
        return result;
    end;

    function XNOR_REDUCE(ARG: STD_ULOGIC_VECTOR) return UX01 is
	-- pragma subpgm_id 410
    begin
        return not XOR_REDUCE(ARG);
    end;

--synopsys synthesis_off
	
    function fun_BUF3S(Input, Enable: UX01; Strn: STRENGTH) return STD_LOGIC is
	-- pragma subpgm_id 411
	type TRISTATE_TABLE is array(STRENGTH, UX01, UX01) of STD_LOGIC;

	-- truth table for tristate "buf" function (Enable active Low)
          constant tbl_BUF3S: TRISTATE_TABLE := 
          -- ----------------------------------------------------
          -- | Input   U    X    0    1       | Enable Strength |
          -- ---------------------------------|-----------------|
        	   ((('U', 'U', 'U', 'U'),  --|   U       X01   |
        	     ('U', 'X', 'X', 'X'),  --|   X       X01   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       X01   |
        	     ('U', 'X', '0', '1')), --|   1       X01   |
        	    (('U', 'U', 'U', 'U'),  --|   U       X0H   |
        	     ('U', 'X', 'X', 'X'),  --|   X       X0H   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       X0H   |
        	     ('U', 'X', '0', 'H')), --|   1       X0H   |
        	    (('U', 'U', 'U', 'U'),  --|   U       XL1   |
        	     ('U', 'X', 'X', 'X'),  --|   X       XL1   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       XL1   |
        	     ('U', 'X', 'L', '1')), --|   1       XL1   |
        	    (('U', 'U', 'U', 'Z'),  --|   U       X0Z   |
        	     ('U', 'X', 'X', 'Z'),  --|   X       X0Z   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       X0Z   |
        	     ('U', 'X', '0', 'Z')), --|   1       X0Z   |
        	    (('U', 'U', 'U', 'U'),  --|   U       XZ1   |
        	     ('U', 'X', 'X', 'X'),  --|   X       XZ1   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       XZ1   |
        	     ('U', 'X', 'Z', '1')), --|   1       XZ1   |
        	    (('U', 'U', 'U', 'U'),  --|   U       WLH   |
        	     ('U', 'W', 'W', 'W'),  --|   X       WLH   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       WLH   |
        	     ('U', 'W', 'L', 'H')), --|   1       WLH   |
        	    (('U', 'U', 'U', 'U'),  --|   U       WLZ   |
        	     ('U', 'W', 'W', 'Z'),  --|   X       WLZ   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       WLZ   |
        	     ('U', 'W', 'L', 'Z')), --|   1       WLZ   |
        	    (('U', 'U', 'U', 'U'),  --|   U       WZH   |
        	     ('U', 'W', 'W', 'W'),  --|   X       WZH   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       WZH   |
        	     ('U', 'W', 'Z', 'H')), --|   1       WZH   |
        	    (('U', 'U', 'U', 'U'),  --|   U       W0H   |
        	     ('U', 'W', 'W', 'W'),  --|   X       W0H   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       W0H   |
        	     ('U', 'W', '0', 'H')), --|   1       W0H   |
        	    (('U', 'U', 'U', 'U'),  --|   U       WL1   |
        	     ('U', 'W', 'W', 'W'),  --|   X       WL1   |
        	     ('Z', 'Z', 'Z', 'Z'),  --|   0       WL1   |
        	     ('U', 'W', 'L', '1')));--|   1       WL1   |
    begin
	return tbl_BUF3S(Strn, Enable, Input);
    end fun_BUF3S;


    function fun_BUF3SL(Input, Enable: UX01; Strn: STRENGTH) return STD_LOGIC is
	-- pragma subpgm_id 412
	type TRISTATE_TABLE is array(STRENGTH, UX01, UX01) of STD_LOGIC;

	-- truth table for tristate "buf" function (Enable active Low)
          constant tbl_BUF3SL: TRISTATE_TABLE := 
          -- ----------------------------------------------------
          -- | Input   U    X    0    1       | Enable Strength |
          -- ---------------------------------|-----------------|
        	   ((('U', 'U', 'U', 'U'),  --|   U       X01   |
        	     ('U', 'X', 'X', 'X'),  --|   X       X01   |
        	     ('U', 'X', '0', '1'),  --|   0       X01   |
        	     ('Z', 'Z', 'Z', 'Z')), --|   1       X01   |
        	    (('U', 'U', 'U', 'U'),  --|   U       X0H   |
        	     ('U', 'X', 'X', 'X'),  --|   X       X0H   |
        	     ('U', 'X', '0', 'H'),  --|   0       X0H   |
        	     ('Z', 'Z', 'Z', 'Z')), --|   1       X0H   |
        	    (('U', 'U', 'U', 'U'),  --|   U       XL1   |
        	     ('U', 'X', 'X', 'X'),  --|   X       XL1   |
        	     ('U', 'X', 'L', '1'),  --|   0       XL1   |
        	     ('Z', 'Z', 'Z', 'Z')), --|   1       XL1   |
        	    (('U', 'U', 'U', 'Z'),  --|   U       X0Z   |
        	     ('U', 'X', 'X', 'Z'),  --|   X       X0Z   |
        	     ('U', 'X', '0', 'Z'),  --|   0       X0Z   |
        	     ('Z', 'Z', 'Z', 'Z')), --|   1       X0Z   |
        	    (('U', 'U', 'U', 'U'),  --|   U       XZ1   |
        	     ('U', 'X', 'X', 'X'),  --|   X       XZ1   |
        	     ('U', 'X', 'Z', '1'),  --|   0       XZ1   |
        	     ('Z', 'Z', 'Z', 'Z')), --|   1       XZ1   |
        	    (('U', 'U', 'U', 'U'),  --|   U       WLH   |
        	     ('U', 'W', 'W', 'W'),  --|   X       WLH   |
        	     ('U', 'W', 'L', 'H'),  --|   0       WLH   |
        	     ('Z', 'Z', 'Z', 'Z')), --|   1       WLH   |
        	    (('U', 'U', 'U', 'U'),  --|   U       WLZ   |
        	     ('U', 'W', 'W', 'Z'),  --|   X       WLZ   |
        	     ('U', 'W', 'L', 'Z'),  --|   0       WLZ   |
        	     ('Z', 'Z', 'Z', 'Z')), --|   1       WLZ   |
        	    (('U', 'U', 'U', 'U'),  --|   U       WZH   |
        	     ('U', 'W', 'W', 'W'),  --|   X       WZH   |
        	     ('U', 'W', 'Z', 'H'),  --|   0       WZH   |
        	     ('Z', 'Z', 'Z', 'Z')), --|   1       WZH   |
        	    (('U', 'U', 'U', 'U'),  --|   U       W0H   |
        	     ('U', 'W', 'W', 'W'),  --|   X       W0H   |
        	     ('U', 'W', '0', 'H'),  --|   0       W0H   |
        	     ('Z', 'Z', 'Z', 'Z')), --|   1       W0H   |
        	    (('U', 'U', 'U', 'U'),  --|   U       WL1   |
        	     ('U', 'W', 'W', 'W'),  --|   X       WL1   |
        	     ('U', 'W', 'L', '1'),  --|   0       WL1   |
        	     ('Z', 'Z', 'Z', 'Z')));--|   1       WL1   |
    begin
	return tbl_BUF3SL(Strn, Enable, Input);
    end fun_BUF3SL;


    function fun_MUX2x1(Input0, Input1, Sel: UX01) return UX01 is
	-- pragma subpgm_id 413
	type MUX_TABLE is array (UX01, UX01, UX01) of UX01;

	-- truth table for "MUX2x1" function
	constant tbl_MUX2x1: MUX_TABLE :=
	--------------------------------------------
        --| In0  'U'  'X'  '0'  '1'      | Sel In1 |
	--------------------------------------------
	      ((('U', 'U', 'U', 'U'),  --| 'U' 'U' |
	        ('U', 'U', 'U', 'U'),  --| 'X' 'U' |
		('U', 'X', '0', '1'),  --| '0' 'U' |
		('U', 'U', 'U', 'U')), --| '1' 'U' |
	       (('U', 'X', 'U', 'U'),  --| 'U' 'X' |
	        ('U', 'X', 'X', 'X'),  --| 'X' 'X' |
		('U', 'X', '0', '1'),  --| '0' 'X' |
		('X', 'X', 'X', 'X')), --| '1' 'X' |
	       (('U', 'U', '0', 'U'),  --| 'U' '0' |
	        ('U', 'X', '0', 'X'),  --| 'X' '0' |
		('U', 'X', '0', '1'),  --| '0' '0' |
		('0', '0', '0', '0')), --| '1' '0' |
	       (('U', 'U', 'U', '1'),  --| 'U' '1' |
	        ('U', 'X', 'X', '1'),  --| 'X' '1' |
		('U', 'X', '0', '1'),  --| '0' '1' |
		('1', '1', '1', '1')));--| '1' '1' |
    begin
	return tbl_MUX2x1(Input1, Sel, Input0);
    end fun_MUX2x1;


    function fun_MAJ23(Input0, Input1, Input2: UX01) return UX01 is
	-- pragma subpgm_id 414
	type MAJ23_TABLE is array (UX01, UX01, UX01) of UX01;

	----------------------------------------------------------------------------
	--	The "tbl_MAJ23" truth table return 1 if the majority of three
	--	inputs is 1, a 0 if the majority is 0, a X if unknown, and a U if
	--	uninitialized.
	----------------------------------------------------------------------------
	constant tbl_MAJ23: MAJ23_TABLE :=
        --------------------------------------------
        --| In0  'U'  'X'  '0'  '1'      | In1 In2 |
        --------------------------------------------
              ((('U', 'U', 'U', 'U'),  --| 'U' 'U' |
                ('U', 'U', 'U', 'U'),  --| 'X' 'U' |
                ('U', 'U', '0', 'U'),  --| '0' 'U' |
                ('U', 'U', 'U', '1')), --| '1' 'U' |
               (('U', 'U', 'U', 'U'),  --| 'U' 'X' |
                ('U', 'X', 'X', 'X'),  --| 'X' 'X' |
                ('U', 'X', '0', 'X'),  --| '0' 'X' |
                ('U', 'X', 'X', '1')), --| '1' 'X' |
               (('U', 'U', '0', 'U'),  --| 'U' '0' |
                ('U', 'X', '0', 'X'),  --| 'X' '0' |
                ('0', '0', '0', '0'),  --| '0' '0' |
                ('U', 'X', '0', '1')), --| '1' '0' |
               (('U', 'U', 'U', '1'),  --| 'U' '1' |
                ('U', 'X', 'X', '1'),  --| 'X' '1' |
                ('U', 'X', '0', '1'),  --| '0' '1' |
                ('1', '1', '1', '1')));--| '1' '1' |

    begin
	return tbl_MAJ23(Input0, Input1, Input2);
    end fun_MAJ23;


    function fun_WiredX(Input0, Input1: STD_ULOGIC) return STD_LOGIC is
	-- pragma subpgm_id 415
        TYPE stdlogic_table IS ARRAY(STD_ULOGIC, STD_ULOGIC) OF STD_LOGIC;

	-- truth table for "WiredX" function
        -------------------------------------------------------------------    
        -- resolution function
        -------------------------------------------------------------------    
        CONSTANT resolution_table : stdlogic_table := (
        --      ---------------------------------------------------------
        --      |  U    X    0    1    Z    W    L    H    -        |   |  
        --      ---------------------------------------------------------
                ( 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U' ), -- | U |
                ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ), -- | X |
                ( 'U', 'X', '0', 'X', '0', '0', '0', '0', 'X' ), -- | 0 |
                ( 'U', 'X', 'X', '1', '1', '1', '1', '1', 'X' ), -- | 1 |
                ( 'U', 'X', '0', '1', 'Z', 'W', 'L', 'H', 'X' ), -- | Z |
                ( 'U', 'X', '0', '1', 'W', 'W', 'W', 'W', 'X' ), -- | W |
                ( 'U', 'X', '0', '1', 'L', 'W', 'L', 'W', 'X' ), -- | L |
                ( 'U', 'X', '0', '1', 'H', 'W', 'W', 'H', 'X' ), -- | H |
                ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ));-- | - |
    begin
	return resolution_table(Input0, Input1);
    end fun_WiredX;

--synopsys synthesis_on
	
end;
