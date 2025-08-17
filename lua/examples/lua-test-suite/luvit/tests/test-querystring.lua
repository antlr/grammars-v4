local qs = require('querystring')
local deepEqual = require('deep-equal')

require('tap')(function(test)
  -- Format: { { canonicalQS }, { arbitraryQS }, parsed, sep = sep, eq = eq }
  local tests = {
    {
      { 'foo=1&bar=2', 'bar=2&foo=1' },
      {},
      { ['foo'] = '1', ['bar'] = '2' },
    },
    {
      { 'foo=1&foo=2' },
      {},
      { ['foo'] = { '1', '2' } },
    },
    {
      { 'a%20=%20%60%7E%21%40%23%24%25%5E%26*%28%29-_%3D%2B%5B%5D%5C%7B%7D%7C%3B%3A%27%22%2C.%2F%3C%3E%3F%0A%0D%0A%00%7F' },
      {},
      { ['a '] = ' `~!@#$%^&*()-_=+[]\\{}|;:\'",./<>?\n\r\n\000\127' }
    },
    {
      { 'f=' },
      { 'f' },
      { ['f'] = '' },
    },
    {
      { '%25%25s%251G=' },
      { '%25%s%1G' },
      { ['%%s%1G'] = '' },
    },
    {
      { 'f>u+u>f', 'u>f+f>u' },
      {},
      { u = 'f', f = 'u' },
      sep = '+', eq = '>',
    },
  }

  test('parse', function(expect)
    local function testParse(num, input, output, sep, eq)
      local parsed = qs.parse(input, sep, eq)
      if not deepEqual(output, parsed) then
        p("Expected", output)
        p("But got", parsed)
        error("Test #" .. num .. " failed: " .. input)
      end
    end

    for num, testCase in ipairs(tests) do
      local canonicalInputs = testCase[1]
      local arbitraryInputs = testCase[2]
      local output = testCase[3]

      for _, input in ipairs(canonicalInputs) do
        testParse(num, input, output, testCase.sep, testCase.eq)
      end

      for _, input in ipairs(arbitraryInputs) do
        testParse(num, input, output, testCase.sep, testCase.eq)
      end
    end
  end)

  test('stringify', function(expect)
    for num, testCase in ipairs(tests) do
      local input = testCase[3]
      local outputs = testCase[1]
      local stringified = qs.stringify(input, testCase.sep, testCase.eq)

      -- Since the order of the keys in the stringified string is not
      -- deterministic, we must test all the possibilities
      local foundValid = false
      for _, output in ipairs(outputs) do
        if stringified == output then
          foundValid = true
          break
        end
      end

      if not foundValid then
        p("Expected one of", outputs)
        p("But got", stringified)
        error("Test #" .. num .. " failed")
      end
    end
  end)
end)
