Helpers.expect.match = MiniTest.new_expectation('string matching', function(str, pattern)
  return str:find(pattern) ~= nil
end, function(str, pattern)
  return string.format('Pattern: %s\nObserved string: %s', vim.inspect(pattern), str)
end)
