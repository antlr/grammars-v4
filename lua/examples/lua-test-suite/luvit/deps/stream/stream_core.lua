--[[

Copyright 2015 The Luvit Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS-IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--]]
local core = require('core')


local Stream = core.Emitter:extend()

--[[
// old-style streams.  Note that the pipe method (the only relevant
// part of this class) is overridden in the Readable class.
--]]

function Stream:pipe(dest, options)

  local ondata, ondrain, onend, onclose, onerror, cleanup

  local source = self

  function ondata(chunk)
    if dest.writable then
      if false == dest:write(chunk) and source.pause then
        source:pause()
      end
    end
  end

  source:on('data', ondata)

  function ondrain()
    if source.readable and source.resume then
      source:resume()
    end
  end

  dest:on('drain', ondrain)

  local didOnEnd = false
  function onend()
    if didOnEnd then return end
    didOnEnd = true

    dest:_end()
  end


  function onclose()
    if didOnEnd then return end
    didOnEnd = true

    if type(dest.destroy) == 'function' then
      dest:destroy()
    end
  end

  --[[
  // If the 'end' option is not supplied, dest.end() will be called when
  // source gets the 'end' or 'close' events.  Only dest.end() once.
  --]]
  if not dest._isStdio and (not options or options._end ~= false) then
    source:on('end', onend)
    source:on('close', onclose)
  end

  --[[
  // don't leave dangling pipes when there are errors.
  --]]
  function onerror(er)
    cleanup()
    if core.Emitter.listenerCount(self, 'error') == 0 then
      error(er) -- // Unhandled stream error in pipe.
    end
  end

  source:on('error', onerror)
  dest:on('error', onerror)

  --[[
  // remove all the event listeners that were added.
  --]]
  function cleanup()
    source:removeListener('data', ondata)
    dest:removeListener('drain', ondrain)

    source:removeListener('end', onend)
    source:removeListener('close', onclose)

    source:removeListener('error', onerror)
    dest:removeListener('error', onerror)

    source:removeListener('end', cleanup)
    source:removeListener('close', cleanup)

    dest:removeListener('close', cleanup)
  end

  source:on('end', cleanup)
  source:on('close', cleanup)

  dest:on('close', cleanup)

  dest:emit('pipe', source)

  --[[
  // Allow for unix-like usage: A.pipe(B).pipe(C)
  --]]
  return dest
end

return { Stream = Stream }
