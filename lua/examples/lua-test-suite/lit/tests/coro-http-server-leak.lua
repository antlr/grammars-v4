require('coro-http').createServer("127.0.0.1", 8080, function(req)
    p(req)
    return {
        code = 200,
        reason = "OK",
        {"Content-Length", "6"},
        {"Connection", "close"},
    }, "hello\n"
end)
p 'http://localhost:8080/'
