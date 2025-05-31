local Emitter = require('core').Emitter
local http = require('http')

local errorsCaught = 0

require('tap')(function(test)
  test('http-response-error-propagation', function(expect)
    -- Mock socket object
    socket = Emitter:new()

    -- Verify that Response object correctly propagates errors from the underlying
    -- socket (e.g. EPIPE, ECONNRESET, etc.)
    res = http.ServerResponse:new(socket)
    res:on('error', function(err)
      errorsCaught = errorsCaught + 1
      assert(errorsCaught==err)
    end)

    res:emit('error', 1)
    res:emit('error', 2)
    res:emit('error', 3)
  end)
end)
