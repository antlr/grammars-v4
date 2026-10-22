--[[

Copyright 2014 The Luvit Authors. All Rights Reserved.

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

local Object = require('core').Object

local Path = Object:extend()

function Path:initialize(root, sep)
  self.root = root
  self.sep = sep
end

function Path:_get(key)
  return self[key]
end

function Path:getRoot(filepath)
  return self.root
end

function Path:getSep()
  return self.sep
end

function Path:pathsEqual(a, b)
  return a == b
end

-- Split a filename into [root, dir, basename]
function Path:_splitPath(filename)
  filename = self:normalizeSeparators(filename)
  local root = ""
  if self:isAbsolute(filename) or self:isDriveRelative(filename) then
    root = self:getRoot(filename)
    filename = filename:sub(root:len()+1)
  end
  local trailing_slashes = filename:match("["..self.sep.."]*$")
  if trailing_slashes then
    filename = filename:sub(1, -trailing_slashes:len()-1)
  end
  local basename = filename:match("[^" .. self.sep .. "]+$") or ""
  local dir = basename and filename:sub(1, -basename:len()-1) or filename
  return root, dir, basename
end

-- Modifies an array of path parts in place by interpreting "." and ".." segments
function Path:_normalizeArray(parts, isrelative)
  local skip = 0
  for i = #parts, 1, -1 do
    local part = parts[i]
    if part == "." then
      table.remove(parts, i)
    elseif part == ".." then
      table.remove(parts, i)
      skip = skip + 1
    elseif skip > 0 then
      table.remove(parts, i)
      skip = skip - 1
    end
  end
  if isrelative then
    while skip > 0 do
      table.insert(parts, 1, "..")
      skip = skip - 1
    end
  end
end

function Path:_splitBySeparators(filepath)
  local parts = {}
  for part in filepath:gmatch("[^" .. self.sep .. "]+") do
    parts[#parts + 1] = part
  end
  return parts
end

function Path:normalize(filepath)
  filepath = self:normalizeSeparators(filepath)
  local is_absolute = self:isAbsolute(filepath)
  local root = is_absolute and self:getRoot(filepath) or nil
  local trailing_slash = filepath:sub(#filepath) == self.sep

  if root then
    filepath = filepath:sub(root:len()+1)
  end

  local parts = self:_splitBySeparators(filepath)
  self:_normalizeArray(parts, not is_absolute)
  filepath = table.concat(parts, self.sep)

  if #filepath == 0 then
    if is_absolute then
      return root
    end
    return "."
  end
  if trailing_slash then
    filepath = filepath .. self.sep
  end
  if is_absolute then
    filepath = root .. filepath
  end
  return filepath
end

function Path:_filterparts(parts)
  local filteredparts = {}
  -- filter out empty parts
  for i, part in ipairs(parts) do
    if part and part ~= "" then
      table.insert(filteredparts, part)
    end
  end
  for i, part in ipairs(filteredparts) do
    -- Strip leading slashes on all but first item
    if i > 1 then
      while part:sub(1, 1) == self.sep do
        part = part:sub(2)
      end
    end
    -- Strip trailing slashes on all but last item
    if i < #filteredparts then
      while part:sub(#part) == self.sep do
        part = part:sub(1, #part - 1)
      end
    end
    filteredparts[i] = part
  end
  return filteredparts
end

function Path:_rawjoin(parts)
  return table.concat(parts, self.sep)
end

function Path:_filteredjoin(...)
  local parts = {...}
  for i,part in ipairs(parts) do
    parts[i] = self:normalizeSeparators(part)
  end
  local filteredparts = self:_filterparts(parts)
  local joined = self:_rawjoin(filteredparts)
  return joined, filteredparts
end

function Path:join(...)
  local joined = self:_filteredjoin(...)
  return self:normalize(joined)
end

-- Works backwards, joining the arguments until it resolves to an absolute path.
-- If an absolute path is not resolved, then the current working directory is
-- prepended
function Path:resolve(...)
  local paths = {...}
  local resolvedpath = ""
  local resolveddrive
  local isabsolute = false
  for i=#paths, 1, -1 do
    local path = paths[i]
    if path and path ~= "" then
      local root = resolveddrive and self:getRoot(path)
      if self:isDriveRelative(path) then
        root = root or self:getRoot(path)
        resolveddrive = resolveddrive or root
        path = path:sub(root:len()+1)
      end
      if not root or resolveddrive:sub(1,2) == root:sub(1,2) then
        resolvedpath = self:join(self:normalize(path), resolvedpath)
        if self:isAbsolute(resolvedpath) then
          isabsolute = true
          break
        end
      end
    end
  end
  if not isabsolute then
    if resolveddrive then
      local drivecwd = process.env["="..resolveddrive]
      if drivecwd and self:pathsEqual(drivecwd:sub(1,2), resolveddrive) then
        resolvedpath = self:join(drivecwd, resolvedpath)
      else
        resolvedpath = self:join(resolveddrive, resolvedpath)
      end
    else
      resolvedpath = self:join(process.cwd(), resolvedpath)
    end
  end
  return resolvedpath
end

-- Returns the common parts of the given paths or {} if no
-- common parts were found.
function Path:_commonParts(...)
  local common_parts = {}
  local paths = {...}
  local split_paths = {}
  for _,path in ipairs(paths) do
    table.insert(split_paths, self:_splitBySeparators(path))
  end
  for part_i=1,#split_paths[1] do
    local test_part = split_paths[1][part_i]
    for path_i=2,#split_paths do
      local part = split_paths[path_i][part_i]
      if not self:pathsEqual(test_part, part) then
        return common_parts
      end
    end
    table.insert(common_parts, test_part)
  end
  return common_parts
end

-- Returns the relative path from 'from' to 'to'
-- If no relative path can be solved, then 'to' is returned
function Path:relative(from, to)
  from = self:resolve(from)
  to = self:resolve(to)

  local from_root, from_dir, from_basename = self:_splitPath(from)
  local to_root, to_dir, to_basename = self:_splitPath(to)

  if not self:pathsEqual(from_root, to_root) then
    return to
  end

  local from_path, to_path = from_dir..from_basename, to_dir..to_basename
  local common_parts = self:_commonParts(from_path, to_path)
  local from_parts = self:_splitBySeparators(from_path)
  local to_parts = self:_splitBySeparators(to_path)

  local relative_parts = {}
  for i=#common_parts,#from_parts-1 do
    table.insert(relative_parts, "..")
  end
  for i=#common_parts+1,#to_parts do
    table.insert(relative_parts, to_parts[i])
  end

  return self:_rawjoin(relative_parts)
end

function Path:dirname(filepath)
  filepath = self:normalizeSeparators(filepath)
  if filepath:sub(filepath:len()) == self.sep then
    filepath = filepath:sub(1, -2)
  end

  local root, dir = self:_splitPath(filepath)

  if #dir > 0 then
    dir = dir:sub(1, #dir - 1)
    return root .. dir
  end
  if #root > 0 then
    return root
  end
  return "."

end

function Path:basename(filepath, expected_ext)
  local _, _, base = self:_splitPath(filepath)
  if expected_ext then
     local ext_pos = base:find(expected_ext:gsub('%.', '%.') .. '$')
     if ext_pos then base = base:sub(1, ext_pos - 1) end
  end
  return base
end

function Path:extname(filepath)
  local basename = self:basename(filepath)
  if basename == ".." then
    return ""
  else
    return basename:match(".(%.[^.]*)$") or ""
  end
end

local PosixPath = Path:extend()

function PosixPath:initialize()
  Path.initialize(self, '/', '/')
end

function PosixPath:isAbsolute(filepath)
  return filepath:sub(1, self.root:len()) == self.root
end

function PosixPath:isUNC(filepath)
  return false
end

function PosixPath:isDriveRelative(filepath)
  return false
end

function PosixPath:normalizeSeparators(filepath)
  return filepath
end

function PosixPath:_makeLong(filepath)
  return filepath
end


local WindowsPath = Path:extend()

function WindowsPath:initialize()
  Path.initialize(self, 'c:\\', '\\')
end

-- Windows paths are case-insensitive
function WindowsPath:pathsEqual(a, b)
  return a and b and a:lower() == b:lower()
end

function WindowsPath:isAbsolute(filepath)
  return filepath and not self:isDriveRelative(filepath) and self:getRoot(filepath) ~= nil
end

function WindowsPath:isUNC(filepath)
  return filepath and filepath:match("^[\\/][\\/][^?\\/.]") ~= nil
end

-- Drive-relative paths are unique to Windows and use the format <letter>:filepath
function WindowsPath:isDriveRelative(filepath)
  return filepath and filepath:match("^[%a]:[^\\/]")
end

-- if filepath is not specified, returns the default root (c:\)
-- if filepath is specified, returns one of the following:
--   the UNC server and sharename in the format "\\server\" or "\\server\share\"
--   the drive letter in the format "d:\"
--   nil if the neither could be found (meaning the filepath is relative)
function WindowsPath:getRoot(filepath)
  if filepath then
    if self:isUNC(filepath) then
      local server = filepath:match("^[\\/][\\/]([^?\\/.][^\\/]*)")
      -- share name is optional
      local share = filepath:sub(server:len()+3):match("^[\\/]([^\\/.][^\\/]*)")
      local root = self.sep .. self.sep .. server .. (share and (self.sep .. share) or "")
      -- always append trailing slash
      return root .. self.sep
    else
      local drive = filepath:match("^[%a]:")
      -- only append trailing slash if it's not a drive relative path
      local sep = (drive and not self:isDriveRelative(filepath)) and self.sep or ""
      return drive and (drive .. sep)
    end
  else
    return self.meta.super.getRoot(self, filepath)
  end
end

function WindowsPath:join(...)
  local joined, filteredparts = self:_filteredjoin(...)

  -- the joined path may be interpretted as a UNC path, so we need to
  -- make sure that a UNC path was intended. if the first filtered part
  -- looks like a UNC path, then it is probably a safe assumption.
  -- if not, then consolidate any initial slashes to avoid ambiguity
  if not self:isUNC(filteredparts[1]) then
    joined = joined:gsub("^["..self.sep.."]+", self.sep)
  end

  return self:normalize(joined)
end

function WindowsPath:normalizeSeparators(filepath)
  return filepath:gsub("/", self.sep)
end

function WindowsPath:_makeLong(filepath)
  if self:isUNC(filepath) then
    return "\\\\?\\UNC\\" .. self:resolve(filepath)
  elseif self:isAbsolute(filepath) then
    return "\\\\?\\" .. self:resolve(filepath)
  else
    return filepath
  end
end

return {
  nt = WindowsPath:new(),
  posix = PosixPath:new(),
}
