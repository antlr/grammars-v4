--[[
	PCRE2-based RegEx implemention for Luau
	Version 1.0.0a2
	BSD 2-Clause Licence
	Copyright © 2020 - Blockzez (devforum /u/Blockzez and github.com/Blockzez)
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:

	1. Redistributions of source code must retain the above copyright notice, this
	   list of conditions and the following disclaimer.

	2. Redistributions in binary form must reproduce the above copyright notice,
	   this list of conditions and the following disclaimer in the documentation
	   and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
	FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
	DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
	SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
	CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
]]
--[[ Settings ]]--
-- You can change them here
local options = {
	-- The maximum cache size for regex so the patterns are cached so it doesn't recompile the pattern
	-- The only accepted value are number values >= 0, strings that can be automatically coered to numbers that are >= 0, false and nil
	-- Do note that empty regex patterns (comment-only patterns included) are never cached regardless
	-- The default is 256
	cacheSize = 256,

	-- A boolean that determines whether this use unicode data
	-- If this value evalulates to false, you can remove _unicodechar_category, _scripts and _xuc safely and it'll now error if:
	-- - You try to compile a RegEx with unicode flag
	-- - You try to use the \p pattern
	-- The default is true
	unicodeData = false,
};

--
local u_categories = options.unicodeData and require(script:WaitForChild("_unicodechar_category"));
local chr_scripts = options.unicodeData and require(script:WaitForChild("_scripts"));
local xuc_chr = options.unicodeData and require(script:WaitForChild("_xuc"));
local proxy = setmetatable({ }, { __mode = 'k' });
local re, re_m, match_m = { }, { }, { };
local lockmsg;

--[[ Functions ]]--
local function to_str_arr(self, init)
	if init then
		self = string.sub(self, utf8.offset(self, init));
	end;
	local len = utf8.len(self);
	if len <= 1999 then
		return { n = len, s = self, utf8.codepoint(self, 1, #self) };
	end;
	local clen = math.ceil(len / 1999);
	local ret = table.create(len);
	local p = 1;
	for i = 1, clen do
		local c = table.pack(utf8.codepoint(self, utf8.offset(self, i * 1999 - 1998), utf8.offset(self, i * 1999 - (i == clen and 1998 - ((len - 1) % 1999 + 1) or - 1)) - 1));
		table.move(c, 1, c.n, p, ret);
		p += c.n;
	end;
	ret.s, ret.n = self, len;
	return ret;
end;

local function from_str_arr(self)
	local len = self.n or #self;
	if len <= 7997 then
		return utf8.char(table.unpack(self));
	end;
	local clen = math.ceil(len / 7997);
	local r = table.create(clen);
	for i = 1, clen do
		r[i] = utf8.char(table.unpack(self, i * 7997 - 7996, i * 7997 - (i == clen and 7997 - ((len - 1) % 7997 + 1) or 0)));
	end;
	return table.concat(r);
end;

local function utf8_sub(self, i, j)
	j = utf8.offset(self, j);
	return string.sub(self, utf8.offset(self, i), j and j - 1);
end;

--
local flag_map = {
	a = 'anchored', i = 'caseless', m = 'multiline', s = 'dotall', u = 'unicode', U = 'ungreedy', x ='extended',
};

local posix_class_names = {
	alnum = true, alpha = true, ascii = true, blank = true, cntrl = true, digit = true, graph = true, lower = true, print = true, punct = true, space = true, upper = true, word = true, xdigit = true,
};

local escape_chars = {
	-- grouped
	-- digit, spaces and words
	[0x44] = { "class", "digit", true }, [0x53] = { "class", "space", true }, [0x57] = { "class", "word", true },
	[0x64] = { "class", "digit", false }, [0x73] = { "class", "space", false }, [0x77] = { "class", "word", false },
	-- horizontal/vertical whitespace and newline
	[0x48] = { "class", "blank", true }, [0x56] = { "class", "vertical_tab", true },
	[0x68] = { "class", "blank", false }, [0x76] = { "class", "vertical_tab", false },
	[0x4E] = { 0x4E }, [0x52] = { 0x52 },

	-- not grouped
	[0x42] = 0x08,
	[0x6E] = 0x0A, [0x72] = 0x0D, [0x74] = 0x09,
};

local b_escape_chars = {
	-- word boundary and not word boundary
	[0x62] = { 0x62, { "class", "word", false } }, [0x42] = { 0x42, { "class", "word", false } },

	-- keep match out
	[0x4B] = { 0x4B },

	-- start & end of string
	[0x47] = { 0x47 }, [0x4A] = { 0x4A }, [0x5A] = { 0x5A }, [0x7A] = { 0x7A },
};

local valid_categories = {
	C = true, Cc = true, Cf = true, Cn = true, Co = true, Cs = true,
	L = true, Ll = true, Lm = true, Lo = true, Lt = true, Lu = true,
	M = true, Mc = true, Me = true, Mn = true,
	N = true, Nd = true, Nl = true, No = true,
	P = true, Pc = true, Pd = true, Pe = true, Pf = true, Pi = true, Po = true, Ps = true,
	S = true, Sc = true, Sk = true, Sm = true, So = true,
	Z = true, Zl = true, Zp = true, Zs = true,

	Xan = true, Xps = true, Xsp = true, Xuc = true, Xwd = true,
};

local class_ascii_punct = {
	[0x21] = true, [0x22] = true, [0x23] = true, [0x24] = true, [0x25] = true, [0x26] = true, [0x27] = true, [0x28] = true, [0x29] = true, [0x2A] = true, [0x2B] = true, [0x2C] = true, [0x2D] = true, [0x2E] = true, [0x2F] = true,
	[0x3A] = true, [0x3B] = true, [0x3C] = true, [0x3D] = true, [0x3E] = true, [0x3F] = true, [0x40] = true, [0x5B] = true, [0x5C] = true, [0x5D] = true, [0x5E] = true, [0x5F] = true, [0x60] = true, [0x7B] = true, [0x7C] = true,
	[0x7D] = true, [0x7E] = true,
};

local end_str = { 0x24 };
local dot = { 0x2E };
local beginning_str = { 0x5E };
local alternation = { 0x7C };

local function check_re(re_type, name, func)
	if re_type == "Match" then
		return function(...)
			local arg_n = select('#', ...);
			if arg_n < 1 then
				error("missing argument #1 (Match expected)", 2);
			end;
			local arg0, arg1 = ...;
			if not (proxy[arg0] and proxy[arg0].name == "Match") then
				error(string.format("invalid argument #1 to %q (Match expected, got %s)", name, typeof(arg0)), 2);
			else
				arg0 = proxy[arg0];
			end;
			if name == "group" or name == "span" then
				if arg1 == nil then
					arg1 = 0;
				end;
			end;
			return func(arg0, arg1);
		end;
	end;
	return function(...)
		local arg_n = select('#', ...);
		if arg_n < 1 then
			error("missing argument #1 (RegEx expected)", 2);
		elseif arg_n < 2 then
			error("missing argument #2 (string expected)", 2);
		end;
		local arg0, arg1, arg2, arg3, arg4, arg5 = ...;
		if not (proxy[arg0] and proxy[arg0].name == "RegEx") then
			if type(arg0) ~= "string" and type(arg0) ~= "number" then
				error(string.format("invalid argument #1 to %q (RegEx expected, got %s)", name, typeof(arg0)), 2);
			end;
			arg0 = re.fromstring(arg0);
		elseif name == "sub" then
			if type(arg2) == "number" then
				arg2 ..= '';
			elseif type(arg2) ~= "string" then
				error(string.format("invalid argument #3 to 'sub' (string expected, got %s)", typeof(arg2)), 2);
			end;
		elseif type(arg1) == "number" then
			arg1 ..= '';
		elseif type(arg1) ~= "string" then
			error(string.format("invalid argument #2 to %q (string expected, got %s)", name, typeof(arg1)), 2);
		end;
		if name ~= "sub" and name ~= "split" then
			local init_type = typeof(arg2);
			if init_type ~= 'nil' then
				arg2 = tonumber(arg2);
				if not arg2 then
					error(string.format("invalid argument #3 to %q (number expected, got %s)", name, init_type), 2);
				elseif arg2 < 0 then
					arg2 = #arg1 + math.floor(arg2 + 0.5) + 1;
				else
					arg2 = math.max(math.floor(arg2 + 0.5), 1);
				end;
			end;
		end;
		arg0 = proxy[arg0];
		if name == "match" or name == "matchiter" then
			arg3 = ...;
		elseif name == "sub" then
			arg5 = ...;
		end;
		return func(arg0, arg1, arg2, arg3, arg4, arg5);
	end;
end;

--[[ Matches ]]--
local function match_tostr(self)
	local spans = proxy[self].spans;
	local s_start, s_end = spans[0][1], spans[0][2];
	if s_end <= s_start then
		return string.format("Match (%d..%d, empty)", s_start, s_end - 1);
	end;
	return string.format("Match (%d..%d): %s", s_start, s_end - 1, utf8_sub(spans.input, s_start, s_end));
end;

local function new_match(span_arr, group_id, re, str)
	span_arr.source, span_arr.input = re, str;
	local object = newproxy(true);
	local object_mt = getmetatable(object);
	object_mt.__metatable = lockmsg;
	object_mt.__index = setmetatable(span_arr, match_m);
	object_mt.__tostring = match_tostr;

	proxy[object] = { name = "Match", spans = span_arr, group_id = group_id };
	return object;
end;

match_m.group = check_re('Match', 'group', function(self, group_id)
	local span = self.spans[type(group_id) == "number" and group_id or self.group_id[group_id]];
	if not span then
		return nil;
	end;
	return utf8_sub(self.spans.input, span[1], span[2]);
end);

match_m.span = check_re('Match', 'span', function(self, group_id)
	local span = self.spans[type(group_id) == "number" and group_id or self.group_id[group_id]];
	if not span then
		return nil;
	end;
	return span[1], span[2] - 1;
end);

match_m.groups = check_re('Match', 'groups', function(self)
	local spans = self.spans;
	if spans.n > 0 then
		local ret = table.create(spans.n);
		for i = 0, spans.n do
			local v = spans[i];
			if v then
				ret[i] = utf8_sub(spans.input, v[1], v[2]);
			end;
		end;
		return table.unpack(ret, 1, spans.n);
	end;
	return utf8_sub(spans.input, spans[0][1], spans[0][2]);
end);

match_m.groupdict = check_re('Match', 'groupdict', function(self)
	local spans = self.spans;
	local ret = { };
	for k, v in pairs(self.group_id) do
		v = spans[v];
		if v then
			ret[k] = utf8_sub(spans.input, v[1], v[2]);
		end;
	end;
	return ret;
end);

match_m.grouparr = check_re('Match', 'groupdict', function(self)
	local spans = self.spans;
	local ret = table.create(spans.n);
	for i = 0, spans.n do
		local v = spans[i];
		if v then
			ret[i] = utf8_sub(spans.input, v[1], v[2]);
		end;
	end;
	ret.n = spans.n;
	return ret;
end);

--
local line_verbs = {
	CR = 0, LF = 1, CRLF = 2, ANYRLF = 3, ANY = 4, NUL = 5,
};
local function is_newline(str_arr, i, verb_flags)
	local line_verb_n = verb_flags.newline;
	local chr = str_arr[i];
	if line_verb_n == 0 then
		-- carriage return
		return chr == 0x0D;
	elseif line_verb_n == 2 then
		-- carriage return followed by line feed
		return chr == 0x0A and str_arr[i - 1] == 0x20;
	elseif line_verb_n == 3 then
		-- any of the above
		return chr == 0x0A or chr == 0x0D;
	elseif line_verb_n == 4 then
		-- any of Unicode newlines
		return chr == 0x0A or chr == 0x0B or chr == 0x0C or chr == 0x0D or chr == 0x85 or chr == 0x2028 or chr == 0x2029;
	elseif line_verb_n == 5 then
		-- null
		return chr == 0;
	end;
	-- linefeed
	return chr == 0x0A;
end;


local function tkn_char_match(tkn_part, str_arr, i, flags, verb_flags)
	local chr = str_arr[i];
	if not chr then
		return false;
	elseif flags.ignoreCase and chr >= 0x61 and chr <= 0x7A then
		chr -= 0x20;
	end;
	if type(tkn_part) == "number" then
		return tkn_part == chr;
	elseif tkn_part[1] == "charset" then
		for _, v in ipairs(tkn_part[3]) do
			if tkn_char_match(v, str_arr, i, flags, verb_flags) then
				return not tkn_part[2];
			end;
		end;
		return tkn_part[2];
	elseif tkn_part[1] == "range" then
		return chr >= tkn_part[2] and chr <= tkn_part[3] or flags.ignoreCase and chr >= 0x41 and chr <= 0x5A and (chr + 0x20) >= tkn_part[2] and (chr + 0x20) <= tkn_part[3];
	elseif tkn_part[1] == "class" then
		local char_class = tkn_part[2];
		local negate = tkn_part[3];
		local match = false;
		-- if and elseifs :(
		-- Might make these into tables in the future
		if char_class == "xdigit" then
			match = chr >= 0x30 and chr <= 0x39 or chr >= 0x41 and chr <= 0x46 or chr >= 0x61 and chr <= 0x66;
		elseif char_class == "ascii" then
			match = chr <= 0x7F;
		-- cannot be accessed through POSIX classes
		elseif char_class == "vertical_tab" then
			match = chr >= 0x0A and chr <= 0x0D or chr == 0x2028 or chr == 0x2029;
		--
		elseif flags.unicode then
			local current_category = u_categories[chr] or 'Cn';
			local first_category = current_category:sub(1, 1);
			if char_class == "alnum" then
				match = first_category == 'L' or current_category == 'Nl' or current_category == 'Nd';
			elseif char_class == "alpha" then
				match = first_category == 'L' or current_category == 'Nl';
			elseif char_class == "blank" then
				match = current_category == 'Zs' or chr == 0x09;
			elseif char_class == "cntrl" then
				match = current_category == 'Cc';
			elseif char_class == "digit" then
				match = current_category == 'Nd';
			elseif char_class == "graph" then
				match = first_category ~= 'P' and first_category ~= 'C';
			elseif char_class == "lower" then
				match = current_category == 'Ll';
			elseif char_class == "print" then
				match = first_category ~= 'C';
			elseif char_class == "punct" then
				match = first_category == 'P';
			elseif char_class == "space" then
				match = first_category == 'Z' or chr >= 0x09 and chr <= 0x0D;
			elseif char_class == "upper" then
				match = current_category == 'Lu';
			elseif char_class == "word" then
				match = first_category == 'L' or current_category == 'Nl' or current_category == 'Nd' or current_category == 'Pc';
			end;
		elseif char_class == "alnum" then
			match = chr >= 0x30 and chr <= 0x39 or chr >= 0x41 and chr <= 0x5A or chr >= 0x61 and chr <= 0x7A;
		elseif char_class == "alpha" then
			match = chr >= 0x41 and chr <= 0x5A or chr >= 0x61 and chr <= 0x7A;
		elseif char_class == "blank" then
			match = chr == 0x09 or chr == 0x20;
		elseif char_class == "cntrl" then
			match = chr <= 0x1F or chr == 0x7F;
		elseif char_class == "digit" then
			match = chr >= 0x30 and chr <= 0x39;
		elseif char_class == "graph" then
			match = chr >= 0x21 and chr <= 0x7E;
		elseif char_class == "lower" then
			match = chr >= 0x61 and chr <= 0x7A;
		elseif char_class == "print" then
			match = chr >= 0x20 and chr <= 0x7E;
		elseif char_class == "punct" then
			match = class_ascii_punct[chr];
		elseif char_class == "space" then
			match = chr >= 0x09 and chr <= 0x0D or chr == 0x20;
		elseif char_class == "upper" then
			match = chr >= 0x41 and chr <= 0x5A;
		elseif char_class == "word" then
			match = chr >= 0x30 and chr <= 0x39 or chr >= 0x41 and chr <= 0x5A or chr >= 0x61 and chr <= 0x7A or chr == 0x5F;
		end;
		if negate then
			return not match;
		end;
		return match;
	elseif tkn_part[1] == "category" then
		local chr_category = u_categories[chr] or 'Cn';
		local category_v = tkn_part[3];
		local category_len = #category_v;
		if category_len == 3 then
			local match = false;
			if category_v == "Xan" or category_v == "Xwd" then
				match = chr_category:find("^[LN]") or category_v == "Xwd" and chr == 0x5F;
			elseif category_v == "Xps" or category_v == "Xsp" then
				match = chr_category:sub(1, 1) == 'Z' or chr >= 0x09 and chr <= 0x0D;
			elseif category_v == "Xuc" then
				match = tkn_char_match(xuc_chr, str_arr, i, flags, verb_flags);
			end;
			if tkn_part[2] then
				return not match;
			end
			return match;
		elseif chr_category:sub(1, category_len) == category_v then
			return not tkn_part[2];
		end;
		return tkn_part[2];
	elseif tkn_part[1] == 0x2E then
		return flags.dotAll or not is_newline(str_arr, i, verb_flags);
	elseif tkn_part[1] == 0x4E then
		return not is_newline(str_arr, i, verb_flags);
	elseif tkn_part[1] == 0x52 then
		if verb_flags.newline_seq == 0 then
			-- CR, LF or CRLF
			return chr == 0x0A or chr == 0x0D;
		end;
		-- any unicode newline
		return chr == 0x0A or chr == 0x0B or chr == 0x0C or chr == 0x0D or chr == 0x85 or chr == 0x2028 or chr == 0x2029;
	end;
	return false;
end;

local function find_alternation(token, i, count)
	while true do
		local v = token[i];
		local is_table = type(v) == "table";
		if v == alternation then
			return i, count;
		elseif is_table and v[1] == 0x28 then
			if count then
				count += v.count;
			end;
			i = v[3];
		elseif is_table and v[1] == "quantifier" and type(v[5]) == "table" and v[5][1] == 0x28 then
			if count then
				count += v[5].count;
			end;
			i = v[5][3];
		elseif not v or is_table and v[1] == 0x29 then
			return nil, count;
		elseif count then
			if is_table and v[1] == "quantifier" then
				count += v[3];
			else
				count += 1;
			end;
		end;
		i += 1;
	end;
end;

local function re_rawfind(token, str_arr, init, flags, verb_flags, as_bool)
	local tkn_i, str_i, start_i = 0, init, init;
	local states = { };
	while tkn_i do
		if tkn_i == 0 then
			tkn_i += 1;
			local next_alt = find_alternation(token, tkn_i);
			if next_alt then
				table.insert(states, 1, { "alternation", next_alt, str_i });
			end;
			continue;
		end;
		local ctkn = token[tkn_i];
		local tkn_type = type(ctkn) == "table" and ctkn[1];
		if not ctkn then
			break;
		elseif ctkn == "ACCEPT" then
			local not_lookaround = true;
			local close_i = tkn_i;
			repeat
				close_i += 1;
				local is_table = type(token[close_i]) == "table";
				local close_i_tkn = token[close_i];
				if is_table and (close_i_tkn[1] == 0x28 or close_i_tkn[1] == "quantifier" and type(close_i_tkn[5]) == "table" and close_i_tkn[5][1] == 0x28) then
					close_i = close_i_tkn[1] == "quantifier" and close_i_tkn[5][3] or close_i_tkn[3];
				elseif is_table and close_i_tkn[1] == 0x29 and (close_i_tkn[4] == 0x21 or close_i_tkn[4] == 0x3D) then
					not_lookaround = false;
					tkn_i = close_i;
					break;
				end;
			until not close_i_tkn;
			if not_lookaround then
				break;
			end;
		elseif ctkn == "PRUNE" or ctkn == "SKIP" then
			table.insert(states, 1, { ctkn, str_i });
			tkn_i += 1;
		elseif tkn_type == 0x28 then
			table.insert(states, 1, { "group", tkn_i, str_i, nil, ctkn[2], ctkn[3], ctkn[4] });
			tkn_i += 1;
			local next_alt, count = find_alternation(token, tkn_i, (ctkn[4] == 0x21 or ctkn[4] == 0x3D) and ctkn[5] and 0);
			if next_alt then
				table.insert(states, 1, { "alternation", next_alt, str_i });
			end;
			if count then
				str_i -= count;
			end;
		elseif tkn_type == 0x29 and ctkn[4] ~= 0x21 then
			if ctkn[4] == 0x21 or ctkn[4] == 0x3D then
				while true do
					local selected_match_start;
					local selected_state = table.remove(states, 1);
					if selected_state[1] == "group" and selected_state[2] == ctkn[3] then
						if (ctkn[4] == 0x21 or ctkn[4] == 0x3D) and not ctkn[5] then
							str_i = selected_state[3];
						end;
						if selected_match_start then
							table.insert(states, 1, selected_match_start);
						end;
						break;
					elseif selected_state[1] == "matchStart" and not selected_match_start and ctkn[4] == 0x3D then
						selected_match_start = selected_state;
					end;
				end;
			elseif ctkn[4] == 0x3E then
				repeat
					local selected_state = table.remove(states, 1);
				until not selected_state or selected_state[1] == "group" and selected_state[2] == ctkn[3];
			else
				for i, v in ipairs(states) do
					if v[1] == "group" and v[2] == ctkn[3] then
						if v.jmp then
							-- recursive match
							tkn_i = v.jmp;
						end;
						v[4] = str_i;
						if v[7] == "quantifier" and v[10] + 1 < v[9] then
							if token[ctkn[3]][4] ~= "lazy" or v[10] + 1 < v[8] then
								tkn_i = ctkn[3];
							end;
							local ctkn1 = token[ctkn[3]];
							local new_group = { "group", v[2], str_i, nil, ctkn1[5][2], ctkn1[5][3], "quantifier", ctkn1[2], ctkn1[3], v[10] + 1, v[11], ctkn1[4] };
							table.insert(states, 1, new_group);
							if v[11] then
								table.insert(states, 1, { "alternation", v[11], str_i });
							end;
						end;
						break;
					end;
				end;
			end;
			tkn_i += 1;
		elseif tkn_type == 0x4B then
			table.insert(states, 1, { "matchStart", str_i });
			tkn_i += 1;
		elseif tkn_type == 0x7C then
			local close_i = tkn_i;
			repeat
				close_i += 1;
				local is_table = type(token[close_i]) == "table";
				local close_i_tkn = token[close_i];
				if is_table and (close_i_tkn[1] == 0x28 or close_i_tkn[1] == "quantifier" and type(close_i_tkn[5]) == "table" and close_i_tkn[5][1] == 0x28) then
					close_i = close_i_tkn[1] == "quantifier" and close_i_tkn[5][3] or close_i_tkn[3];
				end;
			until is_table and close_i_tkn[1] == 0x29 or not close_i_tkn;
			if token[close_i] then
				for _, v in ipairs(states) do
					if v[1] == "group" and v[6] == close_i then
						tkn_i = v[6];
						break;
					end;
				end;
			else
				tkn_i = close_i;
			end;
		elseif tkn_type == "recurmatch" then
			table.insert(states, 1, { "group", ctkn[3], str_i, nil, nil, token[ctkn[3]][3], nil, jmp = tkn_i });
			tkn_i = ctkn[3] + 1;
			local next_alt, count = find_alternation(token, tkn_i);
			if next_alt then
				table.insert(states, 1, { "alternation", next_alt, str_i });
			end;
		else
			local match;
			if ctkn == "FAIL" then
				match = false;
			elseif tkn_type == 0x29 then
				repeat
					local selected_state = table.remove(states, 1);
				until selected_state[1] == "group" and selected_state[2] == ctkn[3];
			elseif tkn_type == "quantifier" then
				if type(ctkn[5]) == "table" and ctkn[5][1] == 0x28 then
					local next_alt = find_alternation(token, tkn_i + 1);
					if next_alt then
						table.insert(states, 1, { "alternation", next_alt, str_i });
					end;
					table.insert(states, next_alt and 2 or 1, { "group", tkn_i, str_i, nil, ctkn[5][2], ctkn[5][3], "quantifier", ctkn[2], ctkn[3], 0, next_alt, ctkn[4] });
					if ctkn[4] == "lazy" and ctkn[2] == 0 then
						tkn_i = ctkn[5][3];
					end;
					match = true;
				else
					local start_i, end_i;
					local pattern_count = 1;
					local is_backref = type(ctkn[5]) == "table" and ctkn[5][1] == "backref";
					if is_backref then
						pattern_count = 0;
						local group_n = ctkn[5][2];
						for _, v in ipairs(states) do
							if v[1] == "group" and v[5] == group_n then
								start_i, end_i = v[3], v[4];
								pattern_count = end_i - start_i;
								break;
							end;
						end;
					end;
					local min_max_i = str_i + ctkn[2] * pattern_count;
					local mcount = 0;
					while mcount < ctkn[3] do
						if is_backref then
							if start_i and end_i then
								local org_i = str_i;
								if utf8_sub(str_arr.s, start_i, end_i) ~= utf8_sub(str_arr.s, org_i, str_i + pattern_count) then
									break;
								end;
							else
								break;
							end;
						elseif not tkn_char_match(ctkn[5], str_arr, str_i, flags, verb_flags) then
							break;
						end;
						str_i += pattern_count;
						mcount += 1;
					end;
					match = mcount >= ctkn[2];
					if match and ctkn[4] ~= "possessive" then
						if ctkn[4] == "lazy" then
							min_max_i, str_i = str_i, min_max_i;
						end;
						table.insert(states, 1, { "quantifier", tkn_i, str_i, math.min(min_max_i, str_arr.n + 1), (ctkn[4] == "lazy" and 1 or -1) * pattern_count });
					end;
				end;
			elseif tkn_type == "backref" then
				local start_i, end_i;
				local group_n = ctkn[2];
				for _, v in ipairs(states) do
					if v[1] == "group" and v[5] == group_n then
						start_i, end_i = v[3], v[4];
						break;
					end;
				end;
				if start_i and end_i then
					local org_i = str_i;
					str_i += end_i - start_i;
					match = utf8_sub(str_arr.s, start_i, end_i) == utf8_sub(str_arr.s, org_i, str_i);
				end;
			else
				local chr = str_arr[str_i];
				if tkn_type == 0x24 or tkn_type == 0x5A or tkn_type == 0x7A then
					match = str_i == str_arr.n + 1 or tkn_type == 0x24 and flags.multiline and is_newline(str_arr, str_i + 1, verb_flags) or tkn_type == 0x5A and str_i == str_arr.n and is_newline(str_arr, str_i, verb_flags);
				elseif tkn_type == 0x5E or tkn_type == 0x41 or tkn_type == 0x47 then
					match = str_i == 1 or tkn_type == 0x5E and flags.multiline and is_newline(str_arr, str_i - 1, verb_flags) or tkn_type == 0x47 and str_i == init;
				elseif tkn_type == 0x42 or tkn_type == 0x62 then
					local start_m = str_i == 1 or flags.multiline and is_newline(str_arr, str_i - 1, verb_flags);
					local end_m = str_i == str_arr.n + 1 or flags.multiline and is_newline(str_arr, str_i, verb_flags);
					local w_m = tkn_char_match(ctkn[2], str_arr[str_i - 1], flags) and 0 or tkn_char_match(ctkn[2], chr, flags) and 1;
					if w_m == 0 then
						match = end_m or not tkn_char_match(ctkn[2], chr, flags);
					elseif w_m then
						match = start_m or not tkn_char_match(ctkn[2], str_arr[str_i - 1], flags);
					end;
					if tkn_type == 0x42 then
						match = not match;
					end;
				else
					match = tkn_char_match(ctkn, str_arr, str_i, flags, verb_flags);
					str_i += 1;
				end;
			end;
			if not match then
				while true do
					local prev_type, prev_state = states[1] and states[1][1], states[1];
					if not prev_type or prev_type == "PRUNE" or prev_type == "SKIP" then
						if prev_type then
							table.clear(states);
						end;
						if start_i > str_arr.n then
							if as_bool then
								return false;
							end;
							return nil;
						end;
						start_i = prev_type == "SKIP" and prev_state[2] or start_i + 1;
						tkn_i, str_i = 0, start_i;
						break;
					elseif prev_type == "alternation" then
						tkn_i, str_i = prev_state[2], prev_state[3];
						local next_alt, count = find_alternation(token, tkn_i + 1);
						if next_alt then
							prev_state[2] = next_alt;
						else
							table.remove(states, 1);
						end;
						if count then
							str_i -= count;
						end;
						break;
					elseif prev_type == "group" then
						if prev_state[7] == "quantifier" then
							if prev_state[12] == "greedy" and prev_state[10] >= prev_state[8]
								or prev_state[12] == "lazy" and prev_state[10] < prev_state[9] and not prev_state[13] then
								tkn_i, str_i = prev_state[12] == "greedy" and prev_state[6] or prev_state[2], prev_state[3];
								if prev_state[12] == "greedy" then
									table.remove(states, 1);
									break;
								elseif prev_state[10] >= prev_state[8] then
									prev_state[13] = true;
									break;
								end;
							end;
						elseif prev_state[7] == 0x21 then
							table.remove(states, 1);
							tkn_i, str_i = prev_state[6], prev_state[3];
							break;
						end;
					elseif prev_type == "quantifier" then
						if math.sign(prev_state[4] - prev_state[3]) == math.sign(prev_state[5]) then
							prev_state[3] += prev_state[5];
							tkn_i, str_i = prev_state[2], prev_state[3];
							break;
						end;
					end;
					-- keep match out state and recursive state, can be safely removed
					-- prevents infinite loop
					table.remove(states, 1);
				end;
			end;
			tkn_i += 1;
		end;
	end;
	if as_bool then
		return true;
	end;
	local match_start_ran = false;
	local span = table.create(token.group_n);
	span[0], span.n = { start_i, str_i }, token.group_n;
	for _, v in ipairs(states) do
		if v[1] == "matchStart" and not match_start_ran then
			span[0][1], match_start_ran = v[2], true;
		elseif v[1] == "group" and v[5] and not span[v[5]] then
			span[v[5]] = { v[3], v[4] };
		end;
	end;
	return span;
end;

--[[ Methods ]]--
re_m.test = check_re('RegEx', 'test', function(self, str, init)
	return re_rawfind(self.token, to_str_arr(str, init), 1, self.flags, self.verb_flags, true);
end);

re_m.match = check_re('RegEx', 'match', function(self, str, init, source)
	local span = re_rawfind(self.token, to_str_arr(str, init), 1, self.flags, self.verb_flags, false);
	if not span then
		return nil;
	end;
	return new_match(span, self.group_id, source, str);
end);

re_m.matchall = check_re('RegEx', 'matchall', function(self, str, init, source)
	str = to_str_arr(str, init);
	local i = 1;
	return function()
		local span = i <= str.n + 1 and re_rawfind(self.token, str, i, self.flags, self.verb_flags, false);
		if not span then
			return nil;
		end;
		i = span[0][2] + (span[0][1] >= span[0][2] and 1 or 0);
		return new_match(span, self.group_id, source, str.s);
	end;
end);

local function insert_tokenized_sub(repl_r, str, span, tkn)
	for _, v in ipairs(tkn) do
		if type(v) == "table" then
			if v[1] == "condition" then
				if span[v[2]] then
					if v[3] then
						insert_tokenized_sub(repl_r, str, span, v[3]);
					else
						table.move(str, span[v[2]][1], span[v[2]][2] - 1, #repl_r + 1, repl_r);
					end;
				elseif v[4] then
					insert_tokenized_sub(repl_r, str, span, v[4]);
				end;
			else
				table.move(v, 1, #v, #repl_r + 1, repl_r);
			end;
		elseif span[v] then
			table.move(str, span[v][1], span[v][2] - 1, #repl_r + 1, repl_r);
		end;
	end;
	repl_r.n = #repl_r;
	return repl_r;
end;

re_m.sub = check_re('RegEx', 'sub', function(self, repl, str, n, repl_flag_str, source)
	if repl_flag_str ~= nil and type(repl_flag_str) ~= "number" and type(repl_flag_str) ~= "string" then
		error(string.format("invalid argument #5 to 'sub' (string expected, got %s)", typeof(repl_flag_str)), 3);
	end
	local repl_flags = {
		l = false, o = false, u = false,
	};
	for f in string.gmatch(repl_flag_str or '', utf8.charpattern) do
		if repl_flags[f] ~= false then
			error("invalid regular expression substitution flag " .. f, 3);
		end;
		repl_flags[f] = true;
	end;
	local repl_type = type(repl);
	if repl_type == "number" then
		repl ..= '';
	elseif repl_type ~= "string" and repl_type ~= "function" and (not repl_flags.o or repl_type ~= "table") then
		error(string.format("invalid argument #2 to 'sub' (string/function%s expected, got %s)", repl_flags.o and "/table" or '', typeof(repl)), 3);
	end;
	if tonumber(n) then
		n = tonumber(n);
		if n <= -1 or n ~= n then
			n = math.huge;
		end;
	elseif n ~= nil then
		error(string.format("invalid argument #4 to 'sub' (number expected, got %s)", typeof(n)), 3);
	else
		n = math.huge;
	end;
	if n < 1 then
		return str, 0;
	end;
	local min_repl_n = 0;
	if repl_type == "string" then
		repl = to_str_arr(repl);
		if not repl_flags.l then
			local i1 = 0;
			local repl_r = table.create(3);
			local group_n = self.token.group_n;
			local conditional_c = { };
			while i1 < repl.n do
				local i2 = i1;
				repeat
					i2 += 1;
				until not repl[i2] or repl[i2] == 0x24 or repl[i2] == 0x5C or (repl[i2] == 0x3A or repl[i2] == 0x7D) and conditional_c[1];
				min_repl_n += i2 - i1 - 1;
				if i2 - i1 > 1 then
					table.insert(repl_r, table.move(repl, i1 + 1, i2 - 1, 1, table.create(i2 - i1 - 1)));
				end;
				if repl[i2] == 0x3A then
					local current_conditional_c = conditional_c[1];
					if current_conditional_c[2] then
						error("malformed substitution pattern", 3);
					end;
					current_conditional_c[2] = table.move(repl_r, current_conditional_c[3], #repl_r, 1, table.create(#repl_r + 1 - current_conditional_c[3]));
					for i3 = #repl_r, current_conditional_c[3], -1 do
						repl_r[i3] = nil;
					end;
				elseif repl[i2] == 0x7D then
					local current_conditional_c = table.remove(conditional_c, 1);
					local second_c = table.move(repl_r, current_conditional_c[3], #repl_r, 1, table.create(#repl_r + 1 - current_conditional_c[3]));
					for i3 = #repl_r, current_conditional_c[3], -1 do
						repl_r[i3] = nil;
					end;
					table.insert(repl_r, { "condition", current_conditional_c[1], current_conditional_c[2] ~= true and (current_conditional_c[2] or second_c), current_conditional_c[2] and second_c });
				elseif repl[i2] then
					i2 += 1;
					local subst_c = repl[i2];
					if not subst_c then
						if repl[i2 - 1] == 0x5C then
							error("replacement string must not end with a trailing backslash", 3);
						end;
						local prev_repl_f = repl_r[#repl_r];
						if type(prev_repl_f) == "table" then
							table.insert(prev_repl_f, repl[i2 - 1]);
						else
							table.insert(repl_r, { repl[i2 - 1] });
						end;
					elseif subst_c == 0x5C and repl[i2 - 1] == 0x24 then
						local prev_repl_f = repl_r[#repl_r];
						if type(prev_repl_f) == "table" then
							table.insert(prev_repl_f, 0x24);
						else
							table.insert(repl_r, { 0x24 });
						end;
						i2 -= 1;
						min_repl_n += 1;
					elseif subst_c == 0x30 then
						table.insert(repl_r, 0);
					elseif subst_c > 0x30 and subst_c <= 0x39 then
						local start_i2 = i2;
						local group_i = subst_c - 0x30;
						while repl[i2 + 1] and repl[i2 + 1] >= 0x30 and repl[i2 + 1] <= 0x39 do
							group_i ..= repl[i2 + 1] - 0x30;
							i2 += 1;
						end;
						group_i = tonumber(group_i);
						if not repl_flags.u and group_i > group_n then
							error("reference to non-existent subpattern", 3);
						end;
						table.insert(repl_r, group_i);
					elseif subst_c == 0x7B and repl[i2 - 1] == 0x24 then
						i2 += 1;
						local start_i2 = i2;
						while repl[i2] and
							(repl[i2] >= 0x30 and repl[i2] <= 0x39
								or repl[i2] >= 0x41 and repl[i2] <= 0x5A
								or repl[i2] >= 0x61 and repl[i2] <= 0x7A
								or repl[i2] == 0x5F) do
							i2 += 1;
						end;
						if (repl[i2] == 0x7D or repl[i2] == 0x3A and (repl[i2 + 1] == 0x2B or repl[i2 + 1] == 0x2D)) and i2 ~= start_i2 then
							local group_k = utf8_sub(repl.s, start_i2, i2);
							if repl[start_i2] >= 0x30 and repl[start_i2] <= 0x39 then
								group_k = tonumber(group_k);
								if not repl_flags.u and group_k > group_n then
									error("reference to non-existent subpattern", 3);
								end;
							else
								group_k = self.group_id[group_k];
								if not repl_flags.u and (not group_k or group_k > group_n) then
									error("reference to non-existent subpattern", 3);
								end;
							end;
							if repl[i2] == 0x3A then
								i2 += 1;
								table.insert(conditional_c, { group_k, repl[i2] == 0x2D, #repl_r + 1 });
							else
								table.insert(repl_r, group_k);
							end;
						else
							error("malformed substitution pattern", 3);
						end;
					else
						local c_escape_char;
						if repl[i2 - 1] == 0x24 then
							if subst_c ~= 0x24 then
								local prev_repl_f = repl_r[#repl_r];
								if type(prev_repl_f) == "table" then
									table.insert(prev_repl_f, 0x24);
								else
									table.insert(repl_r, { 0x24 });
								end;
							end;
						else
							c_escape_char = escape_chars[repl[i2]];
							if type(c_escape_char) ~= "number" then
								c_escape_char = nil;
							end;
						end;
						local prev_repl_f = repl_r[#repl_r];
						if type(prev_repl_f) == "table" then
							table.insert(prev_repl_f, c_escape_char or repl[i2]);
						else
							table.insert(repl_r, { c_escape_char or repl[i2] });
						end;
						min_repl_n += 1;
					end;
				end;
				i1 = i2;
			end;
			if conditional_c[1] then
				error("malformed substitution pattern", 3);
			end;
			if not repl_r[2] and type(repl_r[1]) == "table" and repl_r[1][1] ~= "condition" then
				repl, repl.n = repl_r[1], #repl_r[1];
			else
				repl, repl_type = repl_r, "subst_string";
			end;
		end;
	end;
	str = to_str_arr(str);
	local incr, i0, count = 0, 1, 0;
	while i0 <= str.n + incr + 1 do
		local span = re_rawfind(self.token, str, i0, self.flags, self.verb_flags, false);
		if not span then
			break;
		end;
		local repl_r;
		if repl_type == "string" then
			repl_r = repl;
		elseif repl_type == "subst_string" then
			repl_r = insert_tokenized_sub(table.create(min_repl_n), str, span, repl);
		else
			local re_match;
			local repl_c;
			if repl_type == "table" then
				re_match = utf8_sub(str.s, span[0][1], span[0][2]);
				repl_c = repl[re_match];
			else
				re_match = new_match(span, self.group_id, source, str.s);
				repl_c = repl(re_match);
			end;
			if repl_c == re_match or repl_flags.o and not repl_c then
				local repl_n = span[0][2] - span[0][1];
				repl_r = table.move(str, span[0][1], span[0][2] - 1, 1, table.create(repl_n));
				repl_r.n = repl_n;
			elseif type(repl_c) == "string" then
				repl_r = to_str_arr(repl_c);
			elseif type(repl_c) == "number" then
				repl_r = to_str_arr(repl_c .. '');
			elseif repl_flags.o then
				error(string.format("invalid replacement value (a %s)", type(repl_c)), 3);
			else
				repl_r = { n = 0 };
			end;
		end;
		local match_len = span[0][2] - span[0][1];
		local repl_len = math.min(repl_r.n, match_len);
		for i1 = 0, repl_len - 1 do
			str[span[0][1] + i1] = repl_r[i1 + 1];
		end;
		local i1 = span[0][1] + repl_len;
		i0 = span[0][2];
		if match_len > repl_r.n then
			for i2 = 1, match_len - repl_r.n do
				table.remove(str, i1);
				incr -= 1;
				i0 -= 1;
			end;
		elseif repl_r.n > match_len then
			for i2 = 1, repl_r.n - match_len do
				table.insert(str, i1 + i2 - 1, repl_r[repl_len + i2]);
				incr += 1;
				i0 += 1;
			end;
		end;
		if match_len <= 0 then
			i0 += 1;
		end;
		count += 1;
		if n < count + 1 then
			break;
		end;
	end;
	return from_str_arr(str), count;
end);

re_m.split = check_re('RegEx', 'split', function(self, str, n)
	if tonumber(n) then
		n = tonumber(n);
		if n <= -1 or n ~= n then
			n = math.huge;
		end;
	elseif n ~= nil then
		error(string.format("invalid argument #3 to 'split' (number expected, got %s)", typeof(n)), 3);
	else
		n = math.huge;
	end;
	str = to_str_arr(str);
	local i, count = 1, 0;
	local ret = { };
	local prev_empty = 0;
	while i <= str.n + 1 do
		count += 1;
		local span = n >= count and re_rawfind(self.token, str, i, self.flags, self.verb_flags, false);
		if not span then
			break;
		end;
		table.insert(ret, utf8_sub(str.s, i - prev_empty, span[0][1]));
		prev_empty = span[0][1] >= span[0][2] and 1 or 0;
		i = span[0][2] + prev_empty;
	end;
	table.insert(ret, string.sub(str.s, utf8.offset(str.s, i - prev_empty)));
	return ret;
end);

--
local function re_index(self, index)
	return re_m[index] or proxy[self].flags[index];
end;

local function re_tostr(self)
	return proxy[self].pattern_repr .. proxy[self].flag_repr;
end;
--

local other_valid_group_char = {
	-- non-capturing group
	[0x3A] = true,
	-- lookarounds
	[0x21] = true, [0x3D] = true,
	-- atomic
	[0x3E] = true,
	-- branch reset
	[0x7C] = true,
};

local function tokenize_ptn(codes, flags)
	if flags.unicode and not options.unicodeData then
		return "options.unicodeData cannot be turned off while having unicode flag";
	end;
	local i, len = 1, codes.n;
	local group_n = 0;
	local outln, group_id, verb_flags = { }, { }, {
		newline = 1, newline_seq = 1, not_empty = 0,
	};
	while i <= len do
		local c = codes[i];
		if c == 0x28 then
			-- Match
			local ret;
			if codes[i + 1] == 0x2A then
				i += 2;
				local start_i = i;
				while codes[i]
					and (codes[i] >= 0x30 and codes[i] <= 0x39
					or codes[i] >= 0x41 and codes[i] <= 0x5A
					or codes[i] >= 0x61 and codes[i] <= 0x7A
					or codes[i] == 0x5F or codes[i] == 0x3A) do
					i += 1;
				end;
				if codes[i] ~= 0x29 and codes[i - 1] ~= 0x3A then
					-- fallback as normal and ( can't be repeated
					return "quantifier doesn't follow a repeatable pattern";
				end;
				local selected_verb = utf8_sub(codes.s, start_i, i);
				if selected_verb == "positive_lookahead:" or selected_verb == "negative_lookhead:"
					or selected_verb == "positive_lookbehind:" or selected_verb == "negative_lookbehind:"
					or selected_verb:find("^[pn]l[ab]:$") then
					ret = { 0x28, nil, nil, selected_verb:find('^n') and 0x21 or 0x3D, selected_verb:find('b', 3, true) and 1 };
				elseif selected_verb == "atomic:" then
					ret = { 0x28, nil, nil, 0x3E, nil };
				elseif selected_verb == "ACCEPT" or selected_verb == "FAIL" or selected_verb == 'F' or selected_verb == "PRUNE" or selected_verb == "SKIP" then
					ret = selected_verb == 'F' and "FAIL" or selected_verb;
				else
					if line_verbs[selected_verb] then
						verb_flags.newline = selected_verb;
					elseif selected_verb == "BSR_ANYCRLF" or selected_verb == "BSR_UNICODE" then
						verb_flags.newline_seq = selected_verb == "BSR_UNICODE" and 1 or 0;
					elseif selected_verb == "NOTEMPTY" or selected_verb == "NOTEMPTY_ATSTART" then
						verb_flags.not_empty = selected_verb == "NOTEMPTY" and 1 or 2;
					else
						return "unknown or malformed verb";
					end;
					if outln[1] then
						return "this verb must be placed at the beginning of the regex";
					end;
				end;
			elseif codes[i + 1] == 0x3F then
				-- ? syntax
				i += 2;
				if codes[i] == 0x23 then
					-- comments
					i = table.find(codes, 0x29, i);
					if not i then
						return "unterminated parenthetical";
					end;
					i += 1;
					continue;
				elseif not codes[i] then
					return "unterminated parenthetical";
				end;
				ret = { 0x28, nil, nil, codes[i], nil };
				if codes[i] == 0x30 and codes[i + 1] == 0x29 then
					-- recursive match entire pattern
					ret[1], ret[2], ret[3], ret[5] = "recurmatch", 0, 0, nil;
				elseif codes[i] > 0x30 and codes[i] <= 0x39 then
					-- recursive match
					local org_i = i;
					i += 1;
					while codes[i] >= 0x30 and codes[i] <= 0x30 do
						i += 1;
					end;
					if codes[i] ~= 0x29 then
						return "invalid group structure";
					end;
					ret[1], ret[2], ret[4] = "recurmatch", tonumber(utf8_sub(codes.s, org_i, i)), nil;
				elseif codes[i] == 0x3C and codes[i + 1] == 0x21 or codes[i + 1] == 0x3D then
					-- lookbehinds
					i += 1;
					ret[4], ret[5] = codes[i], 1;
				elseif codes[i] == 0x7C then
					-- branch reset
					ret[5] = group_n;
				elseif codes[i] == 0x50 or codes[i] == 0x3C or codes[i] == 0x27 then
					if codes[i] == 0x50 then
						i += 1;
					end;
					if codes[i] == 0x3D then
						-- backref
						local start_i = i + 1;
						while codes[i] and
							(codes[i] >= 0x30 and codes[i] <= 0x39
								or codes[i] >= 0x41 and codes[i] <= 0x5A
								or codes[i] >= 0x61 and codes[i] <= 0x7A
								or codes[i] == 0x5F) do
							i += 1;
						end;
						if not codes[i] then
							return "unterminated parenthetical";
						elseif codes[i] ~= 0x29 or i == start_i then
							return "invalid group structure";
						end;
						ret = { "backref", utf8_sub(codes.s, start_i, i) };
					elseif codes[i] == 0x3C or codes[i - 1] ~= 0x50 and codes[i] == 0x27 then
						-- named capture
						local delimiter = codes[i] == 0x27 and 0x27 or 0x3E;
						local start_i = i + 1;
						i += 1;
						if codes[i] == 0x29 then
							return "missing character in subpattern";
						elseif codes[i] >= 0x30 and codes[i] <= 0x39 then
							return "subpattern name must not begin with a digit";
						elseif not (codes[i] >= 0x41 and codes[i] <= 0x5A or codes[i] >= 0x61 and codes[i] <= 0x7A or codes[i] == 0x5F) then
							return "invalid character in subpattern";
						end;
						i += 1;
						while codes[i] and
							(codes[i] >= 0x30 and codes[i] <= 0x39
								or codes[i] >= 0x41 and codes[i] <= 0x5A
								or codes[i] >= 0x61 and codes[i] <= 0x7A
								or codes[i] == 0x5F) do
							i += 1;
						end;
						if not codes[i] then
							return "unterminated parenthetical";
						elseif codes[i] ~= delimiter then
							return "invalid character in subpattern";
						end;
						local name = utf8_sub(codes.s, start_i, i);
						group_n += 1;
						if (group_id[name] or group_n) ~= group_n then
							return "subpattern name already exists";
						end;
						for name1, group_n1 in pairs(group_id) do
							if name ~= name1 and group_n == group_n1 then
								return "different names for subpatterns of the same number aren't permitted";
							end;
						end;
						group_id[name] = group_n;
						ret[2], ret[4] = group_n, nil;
					else
						return "invalid group structure";
					end;
				elseif not other_valid_group_char[codes[i]] then
					return "invalid group structure";
				end;
			else
				group_n += 1;
				ret = { 0x28, group_n, nil, nil };
			end;
			if ret then
				table.insert(outln, ret);
			end;
		elseif c == 0x29 then
			-- Close parenthesis
			local i1 = #outln + 1;
			local lookbehind_c = -1;
			local current_lookbehind_c = 0;
			local max_c, group_c = 0, 0;
			repeat
				i1 -= 1;
				local v, is_table = outln[i1], type(outln[i1]) == "table";
				if is_table and v[1] == 0x28 then
					group_c += 1;
					if current_lookbehind_c and v.count then
						current_lookbehind_c += v.count;
					end;
					if not v[3] then
						if v[4] == 0x7C then
							group_n = v[5] + math.max(max_c, group_c);
						end;
						if current_lookbehind_c ~= lookbehind_c and lookbehind_c ~= -1 then
							lookbehind_c = nil;
						else
							lookbehind_c = current_lookbehind_c;
						end;
						break;
					end;
				elseif v == alternation then
					if current_lookbehind_c ~= lookbehind_c and lookbehind_c ~= -1 then
						lookbehind_c, current_lookbehind_c = nil, nil;
					else
						lookbehind_c, current_lookbehind_c = current_lookbehind_c, 0;
					end;
					max_c, group_c = math.max(max_c, group_c), 0;
				elseif current_lookbehind_c then
					if is_table and v[1] == "quantifier" then
						if v[2] == v[3] then
							current_lookbehind_c += v[2];
						else
							current_lookbehind_c = nil;
						end;
					else
						current_lookbehind_c += 1;
					end;
				end;
			until i1 < 1;
			if i1 < 1 then
				return "unmatched ) in regular expression";
			end;
			local v = outln[i1];
			local outln_len_p_1 = #outln + 1;
			local ret = { 0x29, v[2], i1, v[4], v[5], count = lookbehind_c };
			if (v[4] == 0x21 or v[4] == 0x3D) and v[5] and not lookbehind_c then
				return "lookbehind assertion is not fixed width";
			end;
			v[3] = outln_len_p_1;
			table.insert(outln, ret);
		elseif c == 0x2E then
			table.insert(outln, dot);
		elseif c == 0x5B then
			-- Character set
			local negate, char_class = false, nil;
			i += 1;
			local start_i = i;
			if codes[i] == 0x5E then
				negate = true;
				i += 1;
			elseif codes[i] == 0x2E or codes[i] == 0x3A or codes[i] == 0x3D then
				-- POSIX character classes
				char_class = codes[i];
			end;
			local ret;
			if codes[i] == 0x5B or codes[i] == 0x5C then
				ret = { };
			else
				ret = { codes[i] };
				i += 1;
			end;
			while codes[i] ~= 0x5D do
				if not codes[i] then
					return "unterminated character class";
				elseif codes[i] == 0x2D and ret[1] and type(ret[1]) == "number" then
					if codes[i + 1] == 0x5D then
						table.insert(ret, 1, 0x2D);
					else
						i += 1;
						local ret_c = codes[i];
						if ret_c == 0x5B then
							if codes[i + 1] == 0x2E or codes[i + 1] == 0x3A or codes[i + 1] == 0x3D then
								-- Check for POSIX character class, name does not matter
								local i1 = i + 2;
								repeat
									i1 = table.find(codes, 0x5D, i1);
								until not i1 or codes[i1 - 1] ~= 0x5C;
								if not i1 then
									return "unterminated character class";
								elseif codes[i1 - 1] == codes[i + 1] and i1 - 1 ~= i + 1 then
									return "invalid range in character class";
								end;
							end;
							if ret[1] > 0x5B then
								return "invalid range in character class";
							end;
						elseif ret_c == 0x5C then
							i += 1;
							if codes[i] == 0x78 then
								local radix0, radix1;
								i += 1;
								if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x39 or codes[i] >= 0x41 and codes[i] <= 0x46 or codes[i] >= 0x61 and codes[i] <= 0x66 then
									radix0 = codes[i] - ((codes[i] >= 0x41 and codes[i] <= 0x5A) and 0x37 or (codes[i] >= 0x61 and codes[i] <= 0x7A) and 0x57 or 0x30);
									i += 1;
									if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x39 or codes[i] >= 0x41 and codes[i] <= 0x46 or codes[i] >= 0x61 and codes[i] <= 0x66 then
										radix1 = codes[i] - ((codes[i] >= 0x41 and codes[i] <= 0x5A) and 0x37 or (codes[i] >= 0x61 and codes[i] <= 0x7A) and 0x57 or 0x30);
									else
										i -= 1;
									end;
								else
									i -= 1;
								end;
								ret_c = radix0 and (radix1 and 16 * radix0 + radix1 or radix0) or 0;
							elseif codes[i] >= 0x30 and codes[i] <= 0x37 then
								local radix0, radix1, radix2 = codes[i] - 0x30, nil, nil;
								i += 1;
								if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x37 then
									radix1 = codes[i] - 0x30;
									i += 1;
									if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x37 then
										radix2 = codes[i] - 0x30;
									else
										i -= 1;
									end;
								else
									i -= 1;
								end;
								ret_c = radix1 and (radix2 and 64 * radix0 + 8 * radix1 + radix2 or 8 * radix0 + radix1) or radix0;
							else
								ret_c = escape_chars[codes[i]] or codes[i];
								if type(ret_c) ~= "number" then
									return "invalid range in character class";
								end;
							end;
						elseif ret[1] > ret_c then
							return "invalid range in character class";
						end;
						ret[1] = { "range", ret[1], ret_c };
					end;
				elseif codes[i] == 0x5B then
					if codes[i + 1] == 0x2E or codes[i + 1] == 0x3A or codes[i + 1] == 0x3D then
						local i1 = i + 2;
						repeat
							i1 = table.find(codes, 0x5D, i1);
						until not i1 or codes[i1 - 1] ~= 0x5C;
						if not i1 then
							return "unterminated character class";
						elseif codes[i1 - 1] ~= codes[i + 1] or i1 - 1 == i + 1 then
							table.insert(ret, 1, 0x5B);
						elseif codes[i1 - 1] == 0x2E or codes[i1 - 1] == 0x3D then
							return "POSIX collating elements aren't supported";
						elseif codes[i1 - 1] == 0x3A then
							-- I have no plans to support escape codes (\) in character class names
							local negate = codes[i + 3] == 0x5E;
							local class_name = utf8_sub(codes.s, i + (negate and 3 or 2), i1 - 1);
							--  If not valid then throw an error
							if not posix_class_names[class_name] then
								return "unknown POSIX class name";
							end;
							table.insert(ret, 1, { "class", class_name, negate });
							i = i1;
						end;
					else
						table.insert(ret, 1, 0x5B);
					end;
				elseif codes[i] == 0x5C then
					i += 1;
					if codes[i] == 0x78 then
						local radix0, radix1;
						i += 1;
						if codes[i] == 0x7B then
							i += 1;
							local org_i = i;
							while codes[i] and
								(codes[i] >= 0x30 and codes[i] <= 0x39
									or codes[i] >= 0x41 and codes[i] <= 0x46
									or codes[i] >= 0x61 and codes[i] <= 0x66) do
								i += 1;
							end;
							if codes[i] ~= 0x7D or i == org_i then
								return "malformed hexadecimal character";
							elseif i - org_i > 4 then
								return "character offset too large";
							end;
							table.insert(ret, 1, tonumber(utf8_sub(codes.s, org_i, i), 16));
						else
							if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x39 or codes[i] >= 0x41 and codes[i] <= 0x46 or codes[i] >= 0x61 and codes[i] <= 0x66 then
								radix0 = codes[i] - ((codes[i] >= 0x41 and codes[i] <= 0x5A) and 0x37 or (codes[i] >= 0x61 and codes[i] <= 0x7A) and 0x57 or 0x30);
								i += 1;
								if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x39 or codes[i] >= 0x41 and codes[i] <= 0x46 or codes[i] >= 0x61 and codes[i] <= 0x66 then
									radix1 = codes[i] - ((codes[i] >= 0x41 and codes[i] <= 0x5A) and 0x37 or (codes[i] >= 0x61 and codes[i] <= 0x7A) and 0x57 or 0x30);
								else
									i -= 1;
								end;
							else
								i -= 1;
							end;
							table.insert(ret, 1, radix0 and (radix1 and 16 * radix0 + radix1 or radix0) or 0);
						end;
					elseif codes[i] >= 0x30 and codes[i] <= 0x37 then
						local radix0, radix1, radix2 = codes[i] - 0x30, nil, nil;
						i += 1;
						if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x37 then
							radix1 = codes[i] - 0x30;
							i += 1;
							if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x37 then
								radix2 = codes[i] - 0x30;
							else
								i -= 1;
							end;
						else
							i -= 1;
						end;
						table.insert(ret, 1, radix1 and (radix2 and 64 * radix0 + 8 * radix1 + radix2 or 8 * radix0 + radix1) or radix0);
					elseif codes[i] == 0x45 then
						-- intentionally left blank, \E that's not preceded \Q is ignored
					elseif codes[i] == 0x51 then
						local start_i = i + 1;
						repeat
							i = table.find(codes, 0x5C, i + 1);
						until not i or codes[i + 1] == 0x45;
						table.move(codes, start_i, i and i - 1 or #codes, #outln + 1, outln);
						if not i then
							break;
						end;
						i += 1;
					elseif codes[i] == 0x4E then
						if codes[i + 1] == 0x7B and codes[i + 2] == 0x55 and codes[i + 3] == 0x2B and flags.unicode then
							i += 4;
							local start_i = i;
							while codes[i] and
								(codes[i] >= 0x30 and codes[i] <= 0x39
									or codes[i] >= 0x41 and codes[i] <= 0x46
									or codes[i] >= 0x61 and codes[i] <= 0x66) do
								i += 1;
							end;
							if codes[i] ~= 0x7D or i == start_i then
								return "malformed Unicode code point";
							end;
							local code_point = tonumber(utf8_sub(codes.s, start_i, i));
							table.insert(ret, 1, code_point);
						else
							return "invalid escape sequence";
						end;
					elseif codes[i] == 0x50 or codes[i] == 0x70 then
						if not options.unicodeData then
							return "options.unicodeData cannot be turned off when using \\p";
						end;
						i += 1;
						if codes[i] ~= 0x7B then
							local c_name = utf8.char(codes[i] or 0);
							if not valid_categories[c_name] then
								return "unknown or malformed script name";
							end;
							table.insert(ret, 1, { "category", false, c_name });
						else
							local negate = codes[i] == 0x50;
							i += 1;
							if codes[i] == 0x5E then
								i += 1;
								negate = not negate;
							end;
							local start_i = i;
							while codes[i] and
								(codes[i] >= 0x30 and codes[i] <= 0x39
									or codes[i] >= 0x41 and codes[i] <= 0x5A
									or codes[i] >= 0x61 and codes[i] <= 0x7A
									or codes[i] == 0x5F) do
								i += 1;
							end;
							if codes[i] ~= 0x7D then
								return "unknown or malformed script name";
							end;
							local c_name = utf8_sub(codes.s, start_i, i);
							local script_set = chr_scripts[c_name];
							if script_set then
								table.insert(ret, 1, { "charset", negate, script_set });
							elseif not valid_categories[c_name] then
								return "unknown or malformed script name";
							else
								table.insert(ret, 1, { "category", negate, c_name });
							end;
						end;
					elseif codes[i] == 0x6F then
						i += 1;
						if codes[i] ~= 0x7B then
							return "malformed octal code";
						end;
						i += 1;
						local org_i = i;
						while codes[i] and codes[i] >= 0x30 and codes[i] <= 0x37 do
							i += 1;
						end;
						if codes[i] ~= 0x7D or i == org_i then
							return "malformed octal code";
						end;
						local ret_chr = tonumber(utf8_sub(codes.s, org_i, i), 8);
						if ret_chr > 0xFFFF then
							return "character offset too large";
						end;
						table.insert(ret, 1, ret_chr);
					else
						local esc_char = escape_chars[codes[i]];
						table.insert(ret, 1, type(esc_char) == "string" and { "class", esc_char, false } or esc_char or codes[i]);
					end;
				elseif flags.ignoreCase and codes[i] >= 0x61 and codes[i] <= 0x7A then
					table.insert(ret, 1, codes[i] - 0x20);
				else
					table.insert(ret, 1, codes[i]);
				end;
				i += 1;
			end;
			if codes[i - 1] == char_class and i - 1 ~= start_i then
				return char_class == 0x3A and "POSIX named classes are only support within a character set" or "POSIX collating elements aren't supported";
			end;
			if not ret[2] and not negate then
				table.insert(outln, ret[1]);
			else
				table.insert(outln, { "charset", negate, ret });
			end;
		elseif c == 0x5C then
			-- Escape char
			i += 1;
			local escape_c = codes[i];
			if not escape_c then
				return "pattern may not end with a trailing backslash";
			elseif escape_c >= 0x30 and escape_c <= 0x39 then
				local org_i = i;
				while codes[i + 1] and codes[i + 1] >= 0x30 and codes[i + 1] <= 0x39 do
					i += 1;
				end;
				local escape_d = tonumber(utf8_sub(codes.s, org_i, i + 1));
				if escape_d > group_n and i ~= org_i then
					i = org_i;
					local radix0, radix1, radix2;
					if codes[i] <= 0x37 then
						radix0 = codes[i] - 0x30;
						i += 1;
						if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x37 then
							radix1 = codes[i] - 0x30;
							i += 1;
							if codes[i] and codes[i] >= 0x30 and codes[i] <= 0x37 then
								radix2 = codes[i] - 0x30;
							else
								i -= 1;
							end;
						else
							i -= 1;
						end;
					end;
					table.insert(outln, radix0 and (radix1 and (radix2 and 64 * radix0 + 8 * radix1 + radix2 or 8 * radix0 + radix1) or radix0) or codes[org_i]);
				else
					table.insert(outln, { "backref", escape_d });
				end;
			elseif escape_c == 0x45 then
				-- intentionally left blank, \E that's not preceded \Q is ignored
			elseif escape_c == 0x51 then
				local start_i = i + 1;
				repeat
					i = table.find(codes, 0x5C, i + 1);
				until not i or codes[i + 1] == 0x45;
				table.move(codes, start_i, i and i - 1 or #codes, #outln + 1, outln);
				if not i then
					break;
				end;
				i += 1;
			elseif escape_c == 0x4E then
				if codes[i + 1] == 0x7B and codes[i + 2] == 0x55 and codes[i + 3] == 0x2B and flags.unicode then
					i += 4;
					local start_i = i;
					while codes[i] and
						(codes[i] >= 0x30 and codes[i] <= 0x39
							or codes[i] >= 0x41 and codes[i] <= 0x46
							or codes[i] >= 0x61 and codes[i] <= 0x66) do
						i += 1;
					end;
					if codes[i] ~= 0x7D or i == start_i then
						return "malformed Unicode code point";
					end;
					local code_point = tonumber(utf8_sub(codes.s, start_i, i));
					table.insert(outln, code_point);
				else
					table.insert(outln, escape_chars[0x4E]);
				end;
			elseif escape_c == 0x50 or escape_c == 0x70 then
				if not options.unicodeData then
					return "options.unicodeData cannot be turned off when using \\p";
				end;
				i += 1;
				if codes[i] ~= 0x7B then
					local c_name = utf8.char(codes[i] or 0);
					if not valid_categories[c_name] then
						return "unknown or malformed script name";
					end;
					table.insert(outln, { "category", false, c_name });
				else
					local negate = escape_c == 0x50;
					i += 1;
					if codes[i] == 0x5E then
						i += 1;
						negate = not negate;
					end;
					local start_i = i;
					while codes[i] and
						(codes[i] >= 0x30 and codes[i] <= 0x39
							or codes[i] >= 0x41 and codes[i] <= 0x5A
							or codes[i] >= 0x61 and codes[i] <= 0x7A
							or codes[i] == 0x5F) do
						i += 1;
					end;
					if codes[i] ~= 0x7D then
						return "unknown or malformed script name";
					end;
					local c_name = utf8_sub(codes.s, start_i, i);
					local script_set = chr_scripts[c_name];
					if script_set then
						table.insert(outln, { "charset", negate, script_set });
					elseif not valid_categories[c_name] then
						return "unknown or malformed script name";
					else
						table.insert(outln, { "category", negate, c_name });
					end;
				end;
			elseif escape_c == 0x67 and (codes[i + 1] == 0x7B or codes[i + 1] >= 0x30 and codes[i + 1] <= 0x39) then
				local is_grouped = false;
				i += 1;
				if codes[i] == 0x7B then
					i += 1;
					is_grouped = true;
				elseif codes[i] < 0x30 or codes[i] > 0x39 then
					return "malformed reference code";
				end;
				local org_i = i;
				while codes[i] and
					(codes[i] >= 0x30 and codes[i] <= 0x39
						or codes[i] >= 0x41 and codes[i] <= 0x46
						or codes[i] >= 0x61 and codes[i] <= 0x66) do
					i += 1;
				end;
				if is_grouped and codes[i] ~= 0x7D then
					return "malformed reference code";
				end;
				local ref_name = tonumber(utf8_sub(codes.s, org_i, i + (is_grouped and 0 or 1)));
				table.insert(outln, { "backref", ref_name });
				if not is_grouped then
					i -= 1;
				end;
			elseif escape_c == 0x6F then
				i += 1;
				if codes[i + 1] ~= 0x7B then
					return "malformed octal code";
				end
				i += 1;
				local org_i = i;
				while codes[i] and codes[i] >= 0x30 and codes[i] <= 0x37 do
					i += 1;
				end;
				if codes[i] ~= 0x7D or i == org_i then
					return "malformed octal code";
				end;
				local ret_chr = tonumber(utf8_sub(codes.s, org_i, i), 8);
				if ret_chr > 0xFFFF then
					return "character offset too large";
				end;
				table.insert(outln, ret_chr);
			elseif escape_c == 0x78 then
				local radix0, radix1;
				i += 1;
				if codes[i] == 0x7B then
					i += 1;
					local org_i = i;
					while codes[i] and
						(codes[i] >= 0x30 and codes[i] <= 0x39
							or codes[i] >= 0x41 and codes[i] <= 0x46
							or codes[i] >= 0x61 and codes[i] <= 0x66) do
						i += 1;
					end;
					if codes[i] ~= 0x7D or i == org_i then
						return "malformed hexadecimal code";
					elseif i - org_i > 4 then
						return "character offset too large";
					end;
					table.insert(outln, tonumber(utf8_sub(codes.s, org_i, i), 16));
				else
					if codes[i] and (codes[i] >= 0x30 and codes[i] <= 0x39 or codes[i] >= 0x41 and codes[i] <= 0x46 or codes[i] >= 0x61 and codes[i] <= 0x66) then
						radix0 = codes[i] - ((codes[i] >= 0x41 and codes[i] <= 0x5A) and 0x37 or (codes[i] >= 0x61 and codes[i] <= 0x7A) and 0x57 or 0x30);
						i += 1;
						if codes[i] and (codes[i] >= 0x30 and codes[i] <= 0x39 or codes[i] >= 0x41 and codes[i] <= 0x46 or codes[i] >= 0x61 and codes[i] <= 0x66) then
							radix1 = codes[i] - ((codes[i] >= 0x41 and codes[i] <= 0x5A) and 0x37 or (codes[i] >= 0x61 and codes[i] <= 0x7A) and 0x57 or 0x30);
						else
							i -= 1;
						end;
					else
						i -= 1;
					end;
					table.insert(outln, radix0 and (radix1 and 16 * radix0 + radix1 or radix0) or 0);
				end;
			else
				local esc_char = b_escape_chars[escape_c] or escape_chars[escape_c];
				table.insert(outln, esc_char or escape_c);
			end;
		elseif c == 0x2A or c == 0x2B or c == 0x3F or c == 0x7B then
			-- Quantifier
			local start_q, end_q;
			if c == 0x7B then
				local org_i = i + 1;
				local start_i;
				while codes[i + 1] and (codes[i + 1] >= 0x30 and codes[i + 1] <= 0x39 or codes[i + 1] == 0x2C and not start_i and i + 1 ~= org_i) do
					i += 1;
					if codes[i] == 0x2C then
						start_i = i;
					end;
				end;
				if codes[i + 1] == 0x7D then
					i += 1;
					if not start_i then
						start_q = tonumber(utf8_sub(codes.s, org_i, i));
						end_q = start_q;
					else
						start_q, end_q = tonumber(utf8_sub(codes.s, org_i, start_i)), start_i + 1 == i and math.huge or tonumber(utf8_sub(codes.s, start_i + 1, i));
						if end_q < start_q then
							return "numbers out of order in {} quantifier";
						end;
					end;
				else
					table.move(codes, org_i - 1, i, #outln + 1, outln);
				end;
			else
				start_q, end_q = c == 0x2B and 1 or 0, c == 0x3F and 1 or math.huge;
			end;
			if start_q then
				local quantifier_type = flags.ungreedy and "lazy" or "greedy";
				if codes[i + 1] == 0x2B or codes[i + 1] == 0x3F then
					i += 1;
					quantifier_type = codes[i] == 0x2B and "possessive" or flags.ungreedy and "greedy" or "lazy";
				end;
				local outln_len = #outln;
				local last_outln_value = outln[outln_len];
				if not last_outln_value or type(last_outln_value) == "table" and (last_outln_value[1] == "quantifier" or last_outln_value[1] == 0x28 or b_escape_chars[last_outln_value[1]])
					or last_outln_value == alternation or type(last_outln_value) == "string" then
					return "quantifier doesn't follow a repeatable pattern";
				end;
				if end_q == 0 then
					table.remove(outln);
				elseif start_q ~= 1 or end_q ~= 1 then
					if type(last_outln_value) == "table" and last_outln_value[1] == 0x29 then
						outln_len = last_outln_value[3];
					end;
					outln[outln_len] = { "quantifier", start_q, end_q, quantifier_type, outln[outln_len] };
				end;
			end;
		elseif c == 0x7C then
			-- Alternation
			table.insert(outln, alternation);
			local i1 = #outln;
			repeat
				i1 -= 1;
				local v1, is_table = outln[i1], type(outln[i1]) == "table";
				if is_table and v1[1] == 0x29 then
					i1 = outln[i1][3];
				elseif is_table and v1[1] == 0x28 then
					if v1[4] == 0x7C then
						group_n = v1[5];
					end;
					break;
				end;
			until not v1;
		elseif c == 0x24 or c == 0x5E then
			table.insert(outln, c == 0x5E and beginning_str or end_str);
		elseif flags.ignoreCase and c >= 0x61 and c <= 0x7A then
			table.insert(outln, c - 0x20);
		elseif flags.extended and (c >= 0x09 and c <= 0x0D or c == 0x20 or c == 0x23) then
			if c == 0x23 then
				repeat
					i += 1;
				until not codes[i] or codes[i] == 0x0A or codes[i] == 0x0D;
			end;
		else
			table.insert(outln, c);
		end;
		i += 1;
	end;
	local max_group_n = 0;
	for i, v in ipairs(outln) do
		if type(v) == "table" and (v[1] == 0x28 or v[1] == "quantifier" and type(v[5]) == "table" and v[5][1] == 0x28) then
			if v[1] == "quantifier" then
				v = v[5];
			end;
			if not v[3] then
				return "unterminated parenthetical";
			elseif v[2] then
				max_group_n = math.max(max_group_n, v[2]);
			end;
		elseif type(v) == "table" and (v[1] == "backref" or v[1] == "recurmatch") then
			if not group_id[v[2]] and (type(v[2]) ~= "number" or v[2] > group_n) then
				return "reference to a non-existent or invalid subpattern";
			elseif v[1] == "recurmatch" and v[2] ~= 0 then
				for i1, v1 in ipairs(outln) do
					if type(v1) == "table" and v1[1] == 0x28 and v1[2] == v[2] then
						v[3] = i1;
						break;
					end;
				end;
			elseif type(v[2]) == "string" then
				v[2] = group_id[v[2]];
			end;
		end;
	end;
	outln.group_n = max_group_n;
	return outln, group_id, verb_flags;
end;

if not tonumber(options.cacheSize) then
	error(string.format("expected number for options.cacheSize, got %s", typeof(options.cacheSize)), 2);
end;
local cacheSize = math.floor(options.cacheSize or 0) ~= 0 and tonumber(options.cacheSize);
local cache_pattern, cache_pattern_names;
if not cacheSize then
elseif cacheSize < 0 or cacheSize ~= cacheSize then
	error("cache size cannot be a negative number or a NaN", 2);
elseif cacheSize == math.huge then
	cache_pattern, cache_pattern_names = { nil }, { nil };
elseif cacheSize >= 2 ^ 32 then
	error("cache size too large", 2);
else
	cache_pattern, cache_pattern_names = table.create(options.cacheSize), table.create(options.cacheSize);
end;
if cacheSize then
	function re.pruge()
		table.clear(cache_pattern_names);
		table.clear(cache_pattern);
	end;
end;

local function new_re(str_arr, flags, flag_repr, pattern_repr)
	local tokenized_ptn, group_id, verb_flags;
	local cache_format = cacheSize and string.format("%s|%s", str_arr.s, flag_repr);
	local cached_token = cacheSize and cache_pattern[table.find(cache_pattern_names, cache_format)];
	if cached_token then
		tokenized_ptn, group_id, verb_flags = table.unpack(cached_token, 1, 3);
	else
		tokenized_ptn, group_id, verb_flags = tokenize_ptn(str_arr, flags);
		if type(tokenized_ptn) == "string" then
			error(tokenized_ptn, 2);
		end;
		if cacheSize and tokenized_ptn[1] then
			table.insert(cache_pattern_names, 1, cache_format);
			table.insert(cache_pattern, 1, { tokenized_ptn, group_id, verb_flags });
			if cacheSize ~= math.huge then
				table.remove(cache_pattern_names, cacheSize + 1);
				table.remove(cache_pattern, cacheSize + 1);
			end;
		end;
	end;

	local object = newproxy(true);
	proxy[object] = { name = "RegEx", flags = flags, flag_repr = flag_repr, pattern_repr = pattern_repr, token = tokenized_ptn, group_id = group_id, verb_flags = verb_flags };
	local object_mt = getmetatable(object);
	object_mt.__index = setmetatable(flags, re_m);
	object_mt.__tostring = re_tostr;
	object_mt.__metatable = lockmsg;

	return object;
end;

local function escape_fslash(pre)
	return (#pre % 2 == 0 and '\\' or '') .. pre .. '.';
end;

local function sort_flag_chr(a, b)
	return a:lower() < b:lower();
end;

function re.new(...)
	if select('#', ...) == 0 then
		error("missing argument #1 (string expected)", 2);
	end;
	local ptn, flags_str = ...;
	if type(ptn) == "number" then
		ptn ..= '';
	elseif type(ptn) ~= "string" then
		error(string.format("invalid argument #1 (string expected, got %s)", typeof(ptn)), 2);
	end;
	if type(flags_str) ~= "string" and type(flags_str) ~= "number" and flags_str ~= nil then
		error(string.format("invalid argument #2 (string expected, got %s)", typeof(flags_str)), 2);
	end;

	local flags = {
		anchored = false, caseless = false, multiline = false, dotall = false, unicode = false, ungreedy = false, extended = false,
	};
	local flag_repr = { };
	for f in string.gmatch(flags_str or '', utf8.charpattern) do
		if flags[flag_map[f]] ~= false then
			error("invalid regular expression flag " .. f, 3);
		end;
		flags[flag_map[f]] = true;
		table.insert(flag_repr, f);
	end;
	table.sort(flag_repr, sort_flag_chr);
	flag_repr = table.concat(flag_repr);
	return new_re(to_str_arr(ptn), flags, flag_repr, string.format("/%s/", ptn:gsub("(\\*)/", escape_fslash)));
end;

function re.fromstring(...)
	if select('#', ...) == 0 then
		error("missing argument #1 (string expected)", 2);
	end;
	local ptn = ...;
	if type(ptn) == "number" then
		ptn ..= '';
	elseif type(ptn) ~= "string" then
		error(string.format("invalid argument #1 (string expected, got %s)", typeof(ptn), 2));
	end;
	local str_arr = to_str_arr(ptn);
	local delimiter = str_arr[1];
	if not delimiter then
		error("empty regex", 2);
	elseif delimiter == 0x5C or (delimiter >= 0x30 and delimiter <= 0x39) or (delimiter >= 0x41 and delimiter <= 0x5A) or (delimiter >= 0x61 and delimiter <= 0x7A) then
		error("delimiter must not be alphanumeric or a backslash", 2);
	end;

	local i0 = 1;
	repeat
		i0 = table.find(str_arr, delimiter, i0 + 1);
		if not i0 then
			error(string.format("no ending delimiter ('%s') found", utf8.char(delimiter)), 2);
		end;
		local escape_count = 1;
		while str_arr[i0 - escape_count] == 0x5C do
			escape_count += 1;
		end;
	until escape_count % 2 == 1;

	local flags = {
		anchored = false, caseless = false, multiline = false, dotall = false, unicode = false, ungreedy = false, extended = false,
	};
	local flag_repr = { };
	while str_arr.n > i0 do
		local f = utf8.char(table.remove(str_arr));
		str_arr.n -= 1;
		if flags[flag_map[f]] ~= false then
			error("invalid regular expression flag " .. f, 3);
		end;
		flags[flag_map[f]] = true;
		table.insert(flag_repr, f);
	end;
	table.sort(flag_repr, sort_flag_chr);
	flag_repr = table.concat(flag_repr);
	table.remove(str_arr, 1);
	table.remove(str_arr);
	str_arr.n -= 2;
	str_arr.s = string.sub(str_arr.s, 2, 1 + str_arr.n);
	return new_re(str_arr, flags, flag_repr, string.sub(ptn, 1, 2 + str_arr.n));
end;

local re_escape_line_chrs = {
	['\0'] = '\\x00', ['\n'] = '\\n', ['\t'] = '\\t', ['\r'] = '\\r', ['\f'] = '\\f',
};

function re.escape(...)
	if select('#', ...) == 0 then
		error("missing argument #1 (string expected)", 2);
	end;
	local str, extended, delimiter = ...;
	if type(str) == "number" then
		str ..= '';
	elseif type(str) ~= "string" then
		error(string.format("invalid argument #1 to 'escape' (string expected, got %s)", typeof(str)), 2);
	end;
	if delimiter == nil then
		delimiter = '';
	elseif type(delimiter) == "number" then
		delimiter ..= '';
	elseif type(delimiter) ~= "string" then
		error(string.format("invalid argument #3 to 'escape' (string expected, got %s)", typeof(delimiter)), 2);
	end;
	if utf8.len(delimiter) > 1 or delimiter:match("^[%a\\]$") then
		error("delimiter have not be alphanumeric", 2);
	end;
	return (string.gsub(str, "[\0\f\n\r\t]", re_escape_line_chrs):gsub(string.format("[\\%s#()%%%%*+.?[%%]^{|%s]", extended and '%s' or '', (delimiter:find'^[%%%]]$' and '%' or '') .. delimiter), "\\%1"));
end;

function re.type(...)
	if select('#', ...) == 0 then
		error("missing argument #1", 2);
	end;
	return proxy[...] and proxy[...].name;
end;

for k, f in pairs(re_m) do
	re[k] = f;
end;

re_m = { __index = re_m };

lockmsg = re.fromstring([[/The\s*metatable\s*is\s*(?:locked|inaccessible)(?#Nice try :])/i]]);
getmetatable(lockmsg).__metatable = lockmsg;

local function readonly_table()
	error("Attempt to modify a readonly table", 2);
end;

match_m = {
	__index = match_m,
	__metatable = lockmsg,
	__newindex = readonly_table,
};

re.Match = setmetatable({ }, match_m);

return setmetatable({ }, {
	__index = re,
	__metatable = lockmsg,
	__newindex = readonly_table,
});
