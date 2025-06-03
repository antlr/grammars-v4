local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

	local base64 = {}

	local extract = bit32.extract

	function base64.makeencoder( s62, s63, spad )
		local encoder = {}
		for b64code, char in pairs{[0]='A','B','C','D','E','F','G','H','I','J',
			'K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y',
			'Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n',
			'o','p','q','r','s','t','u','v','w','x','y','z','0','1','2',
			'3','4','5','6','7','8','9',s62 or '+',s63 or'/',spad or'='} do
			encoder[b64code] = char:byte()
		end
		return encoder
	end

	function base64.makedecoder( s62, s63, spad )
		local decoder = {}
		for b64code, charcode in pairs( base64.makeencoder( s62, s63, spad )) do
			decoder[charcode] = b64code
		end
		return decoder
	end

	local DEFAULT_ENCODER = base64.makeencoder()
	local DEFAULT_DECODER = base64.makedecoder()

	local char, concat, byte = string.char, table.concat, string.byte

	function base64.decode( b64, decoder, usecaching )
		decoder = decoder or DEFAULT_DECODER
		local cache = usecaching and {}
		local t, k = {}, 1
		local n = #b64
		local padding = b64:sub(-2) == '==' and 2 or b64:sub(-1) == '=' and 1 or 0
		for i = 1, padding > 0 and n-4 or n, 4 do
			local a, b, c, d = byte( b64, i, i+3 )
			local s
			if usecaching then
				local v0 = a*0x1000000 + b*0x10000 + c*0x100 + d
				s = cache[v0]
				if not s then
					local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40 + decoder[d]
					s = char( extract(v,16,8), extract(v,8,8), extract(v,0,8))
					cache[v0] = s
				end
			else
				local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40 + decoder[d]
				s = char( extract(v,16,8), extract(v,8,8), extract(v,0,8))
			end
			t[k] = s
			k = k + 1
		end
		if padding == 1 then
			local a, b, c = byte( b64, n-3, n-1 )
			local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40
			t[k] = char( extract(v,16,8), extract(v,8,8))
		elseif padding == 2 then
			local a, b = byte( b64, n-3, n-2 )
			local v = decoder[a]*0x40000 + decoder[b]*0x1000
			t[k] = char( extract(v,16,8))
		end
		return concat( t )
	end

	local ts0 = os.clock()

	for i = 1, 2000 do
		base64.decode("TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "base64")