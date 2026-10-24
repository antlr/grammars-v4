local metricsBase = {}

return {
  -- Returns all metrics as they're known so far.
  all = function()
    return metricsBase
  end,

  -- Resets to initial conditions (no metrics defined).
  reset = function()
    metricsBase = {}
  end,

  -- Defines a new metric, and sets its initial value.  If no value is provided,
  -- it's assumed to be an integer value 0.  This function returns nothing.
  -- If the metric already exists, an error is raised.  To capture the error,
  -- be sure to invoke this function with pcall().
  define = function(name, value)
    value = value or 0
    if metricsBase[name] == nil then
      metricsBase[name] = value
    else
      error("Metric already defined")
    end
  end,

  -- Sets an existing metric to a new value.  This metric must already exist,
  -- or an error will be raised.  Use pcall() to capture this error if needed.
  set = function(name, value)
    value = value or 0
    if metricsBase[name] == nil then
      error("Metric does not exist yet")
    else
      metricsBase[name] = value
    end
  end,

  -- Retrieves the current value of a metric.  This metric must already exist,
  -- or nil will be returned.  
  get = function(name)
    return metricsBase[name]
  end,

  -- Increments a metric.  Typically used to increment a value by one, but it
  -- may also be used to increment by any arbitrary amount.  Returns nothing.
  -- If the metric isn't yet defined, an error is raised, which you can
  -- capture with pcall().
  --
  -- Use a negative number to decrement a value instead.
  increment = function(name, by)
    by = by or 1
    if metricsBase[name] == nil then
      error("Attempt to increment a non-existing metric")
    else
      metricsBase[name] = metricsBase[name] + by
    end
  end,
}
