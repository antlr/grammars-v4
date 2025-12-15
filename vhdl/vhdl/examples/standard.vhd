-- This is Package STANDARD as defined in the VHDL 1992 Language Reference Manual.

package standard is 
	type boolean is (false,true); 
	type bit is ('0', '1'); 
	type character is (
		nul, soh, stx, etx, eot, enq, ack, bel, 
		bs,  ht,  lf,  vt,  ff,  cr,  so,  si, 
		dle, dc1, dc2, dc3, dc4, nak, syn, etb, 
		can, em,  sub, esc, fsp, gsp, rsp, usp, 

		' ', '!', '"', '#', '$', '%', '&', ''', 
		'(', ')', '*', '+', ',', '-', '.', '/', 
		'0', '1', '2', '3', '4', '5', '6', '7', 
		'8', '9', ':', ';', '<', '=', '>', '?', 

		'@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 
		'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 
		'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 
		'X', 'Y', 'Z', '[', '\', ']', '^', '_', 

		'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 
		'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 
		'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 
		'x', 'y', 'z', '{', '|', '}', '~', del,

		c128, c129, c130, c131, c132, c133, c134, c135,
		c136, c137, c138, c139, c140, c141, c142, c143,
		c144, c145, c146, c147, c148, c149, c150, c151,
		c152, c153, c154, c155, c156, c157, c158, c159

		-- the character code for 160 is there (NBSP), 
		-- but prints as no char 
		); 

	type severity_level is (note, warning, error, failure); 
	type integer is range -2147483647 to 2147483647; 
	type real is range -1.0E308 to 1.0E308; 
	type time is range -2147483647 to 2147483647 
		units 
			fs;
			ps = 1000 fs;
			ns = 1000 ps;
			us = 1000 ns; 
			ms = 1000 us; 
			sec = 1000 ms; 
			min = 60 sec; 
			hr = 60 min; 
		end units; 
	subtype delay_length is time range 0 fs to time'high;
	impure function now return delay_length; 
	subtype natural is integer range 0 to integer'high; 
	subtype positive is integer range 1 to integer'high; 
	type string is array (positive range <>) of character; 
	type bit_vector is array (natural range <>) of bit; 
	type file_open_kind is (
		read_mode,
		write_mode,
		append_mode);
	type file_open_status is (
		open_ok,
		status_error,
		name_error,
		mode_error);
	attribute foreign : string;
end standard; 
