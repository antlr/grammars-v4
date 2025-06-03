local url = require("url")
local deepEqual = require('deep-equal')

local parseTests = {
  ["http://localhost"] = {href = 'http://localhost/', protocol = 'http', host = 'localhost', hostname = 'localhost', path = '/', pathname = '/'},
  ["http://localhost/test"] = {href = 'http://localhost/test', protocol = 'http', host = 'localhost', hostname = 'localhost', path = '/test', pathname = '/test'},
  ["http://localhost.local"] = {href = 'http://localhost.local/', protocol = 'http', host = 'localhost.local', hostname = 'localhost.local', path = '/', pathname = '/'},
  ["http://localhost:9000"] = {href = 'http://localhost:9000/', protocol = 'http', host = 'localhost:9000', hostname = 'localhost', path = '/', pathname = '/', port = '9000'},
  ["https://creationix.com/foo/bar?this=sdr"] = {href = 'https://creationix.com/foo/bar?this=sdr', protocol = 'https', host = 'creationix.com', hostname = 'creationix.com', path = '/foo/bar?this=sdr', pathname = '/foo/bar', search = '?this=sdr', query = 'this=sdr'},
  ["https://GabrielNicolasAvellaneda:s3cr3t@github.com:443/GabrielNicolasAvellaneda/luvit"] = {href = 'https://GabrielNicolasAvellaneda:s3cr3t@github.com:443/GabrielNicolasAvellaneda/luvit', protocol = 'https', auth = 'GabrielNicolasAvellaneda:s3cr3t', host = 'github.com:443', hostname = 'github.com', port = '443', path = '/GabrielNicolasAvellaneda/luvit', pathname = '/GabrielNicolasAvellaneda/luvit'},
  ["creationix.com/"] = {href = 'creationix.com/', path = 'creationix.com/', pathname = 'creationix.com/'},
  ["https://www.google.com.br/test#q=luvit"] = {href = 'https://www.google.com.br/test#q=luvit', protocol = 'https', host = 'www.google.com.br', hostname = 'www.google.com.br', path = '/test', pathname = '/test', hash = '#q=luvit'},
  ["/dir/@file"] = {href = '/dir/@file', path = '/dir/@file', pathname = '/dir/@file'},
  ["http://user:passwd@host:81/"] = {href = 'http://user:passwd@host:81/', protocol = 'http', auth = 'user:passwd', host = 'host:81', hostname = 'host', port = '81', path = '/', pathname = '/'},
}
local parseTestsWithQueryString = {
  ["/somepath?test=bar&ponies=foo"] = { pathname = '/somepath', query = {test = 'bar', ponies = 'foo'},href='/somepath?test=bar&ponies=foo',path='/somepath?test=bar&ponies=foo',search='?test=bar&ponies=foo'},
}
local relativeTests = {
  {'http://example.com/a/b/c', '/d', 'http://example.com/d'},
  {'http://example.com/a/b/c', 'd', 'http://example.com/a/b/d'},
  {'http://example.com/a/b/c/', 'd', 'http://example.com/a/b/c/d'},
  {'http://example.com/b//c//d;p?q#blarg', 'https://p/a/t/h?s#hash2', 'https://p/a/t/h?s#hash2'},
  {'http://example.com/b//c//d;p?q#blarg', 'https://u:p@h.com/p/a/t/h?s#hash2', 'https://u:p@h.com/p/a/t/h?s#hash2'},
  {'http://example.com/b//c//d;p?q#blarg', 'https://a/b/c/d', 'https://a/b/c/d'},
  {'http://example.com/b//c//d;p?q#blarg', 'http://u:p@h.com/p/a/t/h?s#hash2', 'http://u:p@h.com/p/a/t/h?s#hash2'},
  {'/foo/bar/baz', '/../etc/passwd', '/etc/passwd'},
  {'http://localhost', 'file://foo/Users', 'file://foo/Users'},
  -- from node
  {'/foo/bar/baz', 'quux', '/foo/bar/quux'},
  {'/foo/bar/baz', 'quux/asdf', '/foo/bar/quux/asdf'},
  {'/foo/bar/baz', 'quux/baz', '/foo/bar/quux/baz'},
  {'/foo/bar/baz', '../quux/baz', '/foo/quux/baz'},
  {'/foo/bar/baz', '/bar', '/bar'},
  {'/foo/bar/baz/', 'quux', '/foo/bar/baz/quux'},
  {'/foo/bar/baz/', 'quux/baz', '/foo/bar/baz/quux/baz'},
  {'/foo/bar/baz', '../../../../../../../../quux/baz', '/quux/baz'},
  {'/foo/bar/baz', '../../../../../../../quux/baz', '/quux/baz'},
  {'/foo', '.', '/'},
  {'/foo', '..', '/'},
  {'/foo/', '.', '/foo/'},
  {'/foo/', '..', '/'},
  {'/foo/bar', '.', '/foo/'},
  {'/foo/bar', '..', '/'},
  {'/foo/bar/', '.', '/foo/bar/'},
  {'/foo/bar/', '..', '/foo/'},
  {'foo/bar', '../../../baz', '../../baz'},
  {'foo/bar/', '../../../baz', '../baz'},
}

require('tap')(function(test)
  for testUrl, expected in pairs(parseTests) do
    test('should parse url '..testUrl, function ()
      local parsed = url.parse(testUrl)
      assert(deepEqual(expected, parsed))

      local formatted = url.format(expected)
      assert(formatted == expected.href, formatted .. ' should equal '.. expected.href)
    end)
  end

  for testUrl, expected in pairs(parseTestsWithQueryString) do
    test('should parse url '..testUrl..' with querystring', function ()
      local parsed = url.parse(testUrl, true)
      assert(deepEqual(expected, parsed))

      local formatted = url.format(expected)
      assert(formatted == expected.href, formatted .. ' should equal '.. expected.href)
    end)
  end

  test('format should extract port from host', function()
    local parsed = url.parse("http://localhost")
    parsed.host = parsed.host .. ":9000"
    assert(not parsed.port)

    local formatted = url.format(parsed)
    assert(formatted == "http://localhost:9000/")
  end)

  for testNo,relativeTest in ipairs(relativeTests) do
    local source = relativeTest[1]
    local relative = relativeTest[2]
    local expected = relativeTest[3]
    test('should resolve ' .. source .. ' to ' .. relative, function()
      local actual = url.resolve(source, relative)

      assert(actual == expected, 'relative test #' .. testNo .. ': ' .. actual .. ' should equal '.. expected)
    end)
  end
end)
