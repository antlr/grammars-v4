local messages = {
  GET = "Sure thing!",
  PUT = "Thanks!",
  DELETE = "Are you sure?",
}

-- This is a configurable stack middleware
return function (method)
  return function (req, res, continue)
      if req.method == method then
        res:finish(method .. ", " .. messages[method] .. "\n")
      else
        continue()
      end
  end
end
