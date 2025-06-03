local uv = require 'uv'

require('weblit-app').bind({
    host = "0.0.0.0",
    port = 8080
}).use(require('weblit-logger')).use(require('weblit-auto-headers')).route({
    method = "GET",
    path = "/"
}, function(req, res, go)
    res.code = 200
    res.body = "Hello World\n"
end).start()

uv.new_signal():start('sigint', function(signal)
    print("\nReceived " .. signal .. ", shutting down...")
    os.exit()
end)

print "Test by making http requests to http://localhost/ and http://localhost/404"
print "Then finish test by pressing control+c"
uv.run()
