local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

	local band = bit32.band
	local bnot = bit32.bnot
	local bxor = bit32.bxor

	local rrotate = bit32.rrotate
	local rshift = bit32.rshift

	local primes = 
	{
		0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
		0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
		0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
		0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
		0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
		0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
		0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
		0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
		0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
		0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
		0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
		0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
		0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
		0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
		0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
		0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
	}

	local function toHex(str)
		local result = str:gsub('.', function (char)
			return string.format("%02x", char:byte())
		end)
		
		return result
	end

	local function toBytes(value, length)
		local str = ""
		
		for i = 1, length do
			local rem = value % 256
			str = string.char(rem) .. str
			value = (value - rem) / 256
		end
		
		return str
	end

	local function digestBlock(msg, i, hash, digest)
		for j = 1, 16 do 
			local offset = i + (j - 1) * 4
			local a, b, c, d = string.byte(msg, offset, offset + 3)
			digest[j] = ((a * 256 + b) * 256 + c) * 256 + d
		end
		
		for j = 17, 64 do
			local v = digest[j - 15]
			local s0 = bxor(rrotate(v, 7), rrotate(v, 18), rshift(v, 3))
			
			v = digest[j - 2]
			digest[j] = digest[j - 16] + s0 + digest[j - 7] + bxor(rrotate(v, 17), rrotate(v, 19), rshift(v, 10))
		end
		
		local a, b, c, d, e, f, g, h = table.unpack(hash)
		
		for i = 1, 64 do
			local s0 = bxor(rrotate(a, 2), rrotate(a, 13), rrotate(a, 22))
			local maj = bxor(band(a, b), band(a, c), band(b, c))
			
			local t2 = s0 + maj
			local s1 = bxor(rrotate(e, 6), rrotate(e, 11), rrotate(e, 25))
			
			local ch = bxor(band(e, f), band(bnot(e), g))
			local t1 = h + s1 + ch + primes[i] + digest[i]
			
			h, g, f, e, d, c, b, a = g, f, e, d + t1, c, b, a, t1 + t2
		end
		
		hash[1] = band(hash[1] + a)
		hash[2] = band(hash[2] + b)
		hash[3] = band(hash[3] + c)
		hash[4] = band(hash[4] + d)
		hash[5] = band(hash[5] + e)
		hash[6] = band(hash[6] + f)
		hash[7] = band(hash[7] + g)
		hash[8] = band(hash[8] + h)
	end

	local function sha256(msg)
		do
			local extra = 64 - ((#msg + 9) % 64)
			local len = toBytes(8 * #msg, 8)
			
			msg = msg .. '\128' .. string.rep('\0', extra) .. len
			assert(#msg % 64 == 0)
		end
		
		local hash = 
		{
			0x6a09e667,
			0xbb67ae85,
			0x3c6ef372,
			0xa54ff53a,
			0x510e527f,
			0x9b05688c,
			0x1f83d9ab,
			0x5be0cd19,	
		}

		local digest = {}
		
		for i = 1, #msg, 64 do 
			digestBlock(msg, i, hash, digest)
		end
		
		local result = ""
		
		for i = 1, 8 do
			local value = hash[i]
			result = result .. toBytes(value, 4)
		end
		
		return toHex(result)
	end

	local input = string.rep(".", 1e3)

	local ts0 = os.clock()

	for i = 1, 100 do
		sha256(input)
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "sha256")