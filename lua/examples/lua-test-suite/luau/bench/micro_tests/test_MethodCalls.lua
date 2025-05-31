local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

  -- $Id: methcall.lua,v 1.2 2004-06-12 16:19:43 bfulgham Exp $
  -- http://shootout.alioth.debian.org
  -- contributed by Roberto Ierusalimschy

  --------------------------------------------------------------
  -- Toggle class
  --------------------------------------------------------------

  Toggle = {}

  function Toggle:value ()
    return self.state
  end

  function Toggle:activate ()
    self.state = not self.state
    return self
  end

  function Toggle:new (start_state)
    local o = {state = start_state}
    self.__index =self
    setmetatable(o, self)
    return o
  end


  --------------------------------------------------------------
  -- NthToggle class
  --------------------------------------------------------------

  NthToggle = Toggle:new()

  function NthToggle:activate ()
    self.counter = self.counter + 1
    if self.counter >= self.count_max then
      Toggle.activate(self)
      self.counter = 0
    end
    return self
  end

  function NthToggle:new (start_state, max_counter)
    local o = Toggle.new(self, start_state)
    o.count_max = max_counter
    o.counter = 0
    return o
  end


  -----------------------------------------------------------
  -- main
  -----------------------------------------------------------

  function main ()
    local start = os.clock()
    local N = 30000

    local val = 1
    local toggle = Toggle:new(val)
    for i=1,N do
      val = toggle:activate():value()
      val = toggle:activate():value()
      val = toggle:activate():value()
      val = toggle:activate():value()
      val = toggle:activate():value()
      val = toggle:activate():value()
      val = toggle:activate():value()
      val = toggle:activate():value()
      val = toggle:activate():value()
      val = toggle:activate():value()
    end
    print(val and "true" or "false")

    val = 1
    local ntoggle = NthToggle:new(val, 3)
    for i=1,N do
      val = ntoggle:activate():value()
      val = ntoggle:activate():value()
      val = ntoggle:activate():value()
      val = ntoggle:activate():value()
      val = ntoggle:activate():value()
      val = ntoggle:activate():value()
      val = ntoggle:activate():value()
      val = ntoggle:activate():value()
      val = ntoggle:activate():value()
      val = ntoggle:activate():value()
    end
    print(val and "true" or "false")
    return os.clock() - start
  end

  return main()
end

bench.runCode(test, "MethodCalls")