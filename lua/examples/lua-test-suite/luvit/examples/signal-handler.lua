local uv = require('uv')

-- Create a new signal handler
local sigint = uv.new_signal()
-- Define a handler function
uv.signal_start(sigint, "sigint", function(signal)
  print("got " .. signal .. ", shutting down")
  os.exit(1)
end)

-- Use timer to keep loop running
local timer = uv.new_timer()
timer:start(1000, 1000, function(t)
  print("Still running...")
end)

print("Running...")
