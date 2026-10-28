local erroring = function(x)
  return function()
    error(x, 0)
  end
end
