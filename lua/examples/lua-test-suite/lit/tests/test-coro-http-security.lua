local http = require 'coro-http'


require('tap')(function (test)

  local function expectNoConnection(endpoints)
    for name, endpoint in pairs(endpoints) do
      coroutine.wrap(function()
        -- TODO: do we want to match against the error message?
        local connectionSucceeded = pcall(http.getConnection, endpoints.expired, 443, true)
        if connectionSucceeded then
          error(string.format('a bad ceritifcate was accepted as trusted: %s - https://%s', name, endpoint))
        end
      end)()
    end
  end

  local function expectConnection(endpoints)
    for name, endpoint in pairs(endpoints) do
      coroutine.wrap(function()
        local connectionSucceeded, err = pcall(http.getConnection, endpoints.expired, 443, true)
        if not connectionSucceeded then
          error(string.format('a good certificate was rejected as untrusted: %s - https://%s - %s', name, endpoint, err))
        end
      end)()
    end
  end

  test('Certificate Validation', function ()
    local endpoints = {
      expired = 'expired.badssl.com',
      wrong_host = 'wrong.host.badssl.com',
      self_signed = 'self-signed.badssl.com',
      untrusted_root = 'untrusted-root.badssl.com',
    }
    expectNoConnection(endpoints)
  end)

  test('Interception Certificates', function ()
    local endpoints = {
      superfish = 'superfish.badssl.com',
      edellroot = 'edellroot.badssl.com',
      preact_cli = 'preact-cli.badssl.com',
      dsdtestprovider = 'dsdtestprovider.badssl.com',
      webpack_dev_server = 'webpack-dev-server.badssl.com',
    }
    expectNoConnection(endpoints)
  end)

  test('Broken Cryptography', function ()
    local endpoints = {
      rc4 = 'rc4.badssl.com',
      rc4_md5 = 'rc4-md5.badssl.com',
      dh480 = 'dh480.badssl.com',
      dh512 = 'dh512.badssl.com',
      dh1024 = 'dh1024.badssl.com',
      null = 'null.badssl.com',
    }
    expectNoConnection(endpoints)
  end)

  -- might require some workarounds
  -- test('Secure', function ()
  --   local endpoints = {
  --     tls_v1_2 = 'tls-v1-2.badssl.com:1012',
  --     sha256 = 'sha256.badssl.com',
  --     rsa2048 = 'rsa2048.badssl.com',
  --     ecc256 = 'ecc256.badssl.com',
  --     ecc384 = 'ecc384.badssl.com',
  --     -- extended_validation = "extended-validation.badssl.com/", -- bad certificate
  --     mozilla_modern = 'mozilla-modern.badssl.com',
  --   }
  --   expectConnection(endpoints)
  -- end)
end)
