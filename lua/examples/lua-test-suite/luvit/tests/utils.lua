local function deep_equal(expected, actual, path)
  if expected == actual then
    return true
  end
  local prefix = path and (path .. ": ") or ""
  local expectedType = type(expected)
  local actualType = type(actual)
  if expectedType ~= actualType then
    return false, prefix .. "Expected type " .. expectedType .. " but found " .. actualType
  end
  if expectedType ~= "table" then
    return false, prefix .. "Expected " .. tostring(expected) .. " but found " .. tostring(actual)
  end
  local expectedLength = #expected
  local actualLength = #actual
  for key in pairs(expected) do
    if actual[key] == nil then
      return false, prefix .. "Missing table key " .. key
    end
    local newPath = path and (path .. '.' .. key) or key
    local same, message = deep_equal(expected[key], actual[key], newPath)
    if not same then
      return same, message
    end
  end
  if expectedLength ~= actualLength then
    return false, prefix .. "Expected table length " .. expectedLength .. " but found " .. actualLength
  end
  for key in pairs(actual) do
    if expected[key] == nil then
      return false, prefix .. "Unexpected table key " .. key
    end
  end
  return true
end

exports.deep_equal = deep_equal
