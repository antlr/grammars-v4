local exec = require('childprocess').exec
local los = require('los')
local net = require('net')
local uv = require('uv')

require('tap')(function(test)

  test('environment subprocess', function(expect)
    local child, options, onStdout, onExit, onEnd, data

    options = {
      env = { TEST1 = 1 }
    }

    data = ''

    if los.type() == 'win32' then
      child = exec('cmd.exe /C set', options)
    else
      child = exec('env', options)
    end

    function onStdout(chunk)
      p('stdout', chunk)
      data = data .. chunk
    end

    function onExit(code, signal)
      p('exit')
      assert(code == 0)
      assert(signal == 0)
    end

    function onEnd()
      assert(data:find('TEST1=1'))
      p('found')
      child.stdin:destroy()
    end

    child:on('error', function(err)
      p(err)
      child:close()
    end)
    child.stdout:once('end', expect(onEnd))
    child.stdout:on('data', onStdout)
    child:on('exit', expect(onExit))
    child:on('close', expect(onExit))
  end)

  test('invalid command', function(expect)
    -- disable on windows, bug in libuv
    if los.type() == 'win32' then return end

    exec('skfjsldkfjskdfjdsklfj', function() end)
  end)

  test('invalid command verify callback', function(expect)
    local child, onExit, onClose

    -- disable on windows, bug in libuv
    if los.type() == 'win32' then return end

    function onExit() p('exit') end
    function onClose() p('close') end

    child = exec('skfjsldkfjskdfjdsklfj', expect(function (err, stdout, stderr)
      assert(err)
      assert(err.code ~= 0)
    end))
    child:on('exit', expect(onExit))
    child:on('close', expect(onClose))
  end)

  test('child process no stdin', function(expect)
    local child, onData, options

    options = {
      stdio = {
        nil,
        net.Socket:new({ handle = uv.new_pipe(false) }),
        net.Socket:new({ handle = uv.new_pipe(false) })
      }
    }

    function onData(data) end

    if los.type() == 'win32' then
      child = exec('cmd.exe /C set', options)
    else
      child = exec('env', options)
    end
    child:on('data', onData)
    child:on('exit', expect(function(exitCode)
      options.stdio[2]:destroy()
      options.stdio[3]:destroy()
      assert(exitCode == 0)
    end))
    child:on('close', expect(function() end))
    child:on('error', function(err)
      p(err)
      child:close()
    end)
  end)

  test('child process (no stdin, stderr, stdout) with close', function(expect)
    local child, onData, options

    options = {
      stdio = {
        nil,
        net.Socket:new({ handle = uv.new_pipe(false) }),
        net.Socket:new({ handle = uv.new_pipe(false) })
      }
    }

    function onData(data) p(data) end

    if los.type() == 'win32' then
      child = exec('cmd.exe /C set', options)
    else
      child = exec('env', options)
    end
    child:on('data', onData)
    child:on('close', expect(function(exitCode) assert(exitCode == 0) end))
    child:on('exit', expect(function(exitCode)
      options.stdio[2]:destroy()
      options.stdio[3]:destroy()
      assert(exitCode == 0)
    end))
    child:on('error', function(err)
      p(err)
      child:close()
    end)
  end)

end)
