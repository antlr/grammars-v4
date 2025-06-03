return function ()
  local core = require('core')()
  local uv = require('uv')
  local log = require('log').log
  local query = require('pkg').query
  local fs = require('coro-fs')

  -- Try to look up local metadata in case we're inside a luvi app.
  local meta = query(fs, ".")
  local opts = meta and meta.luvi or {}

  -- Allow command-line overrides
  local i = 2
  while i < #args do
    local arg = args[i]
    if arg == "-o" then
      opts.output = assert(args[i + 1], "missing output value")
      i = i + 2
    elseif arg == "-f" then
      opts.flavor = assert(args[i + 1], "missing flavor value")
      i = i + 2
    elseif arg == "-u" then
      opts.url = assert(args[i + 1], "missing url value")
      i = i + 2
    elseif arg == "-v" then
      opts.version = assert(args[i + 1], "missing version value")
      i = i + 2
    else
      error("Unknown option: " .. arg)
    end
  end

  -- Ensure we have the right luvi.
  local path = core.getLuvi(opts)

  if opts.output then
    -- Copy to the output file if requested
    local fd2 = assert(uv.fs_open(opts.output, "w", 493)) -- 0755
    local fd = assert(uv.fs_open(path, "r", 420)) -- 0644
    local size = assert(uv.fs_fstat(fd)).size
    uv.fs_sendfile(fd2, fd, 0, size)
    uv.fs_close(fd)
    uv.fs_close(fd2)
    log("luvi extracted", opts.output)
  else
    -- Otherwise just emit path
    log("luvi cached", path)
  end
end
