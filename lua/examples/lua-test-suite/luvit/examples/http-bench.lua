--  A simple tool to benchmark test speed-http.lua
local net = require('net')
local setInterval = require('timer').setInterval

local PORT = 8080
local HOST = "127.0.0.1"
local CONCURRENT = 100
local BYTES = 44 -- the number of bytes in the expected response

local request = "GET / HTTP/1.1\r\n\r\n"

-- TODO: do an initial call using http-parser to validate output and count bytes

local num = 0
local function chain()
	local client
	client = net.createConnection(PORT, HOST, function ()
		local size
		local function makeRequest()
			size = 0
			client:write(request)
		end
		client:on("data", function (chunk)
			size = size + #chunk
			num = num + 1
			makeRequest()
		end)
		makeRequest()
	end)
end

for i = 1,CONCURRENT do
	chain()
end

setInterval(1000, function ()
	print(num)
	num = 0
end)

