-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print("testing if-else expressions")

function True()
  return true
end

function False()
  return false
end

function EvalElseifChain(condition1, condition2, condition3)
  return if condition1 then 10 elseif condition2 then 20 elseif condition3 then 30 else 40
end

function EvalElse_IfChain(condition1, condition2, condition3)
  return if condition1 then 10 else if condition2 then 20 else if condition3 then 30 else 40
end

function CheckForConditionalEvaluation(condition)
    local counter = 0

    local function AddToCounter(count)
      counter += count
      return counter
    end

    local result = if condition then AddToCounter(7) else AddToCounter(17)
    if condition then
      assert(result == 7)
    else
      assert(result == 17)
    end
    -- ensure the counter value matches the result of the clause that was evaluated
    assert(counter == result)
end
  
-- Test expression using only constants
assert(if true then true else false)
assert(if false then false else true)
assert(if nil then false else true)
assert((7 + if true then 10 else 20) == 17)

-- Test expression using non-constant condition
assert(if True() then true else false)
assert(if False() then false else true)

-- Test evaluation of a "chain" of if/elseif/else in an expression
assert(EvalElseifChain(false, false, false) == 40)
assert(EvalElseifChain(false, false, true) == 30)
assert(EvalElseifChain(false, true, false) == 20)
assert(EvalElseifChain(false, true, true) == 20)
assert(EvalElseifChain(true, false, false) == 10)
assert(EvalElseifChain(true, false, true) == 10)
assert(EvalElseifChain(true, true, false) == 10)
assert(EvalElseifChain(true, true, true) == 10)

-- Test evaluation of a "chain" of if/"else if"/else in an expression
assert(EvalElse_IfChain(false, false, false) == 40)
assert(EvalElse_IfChain(false, false, true) == 30)
assert(EvalElse_IfChain(false, true, false) == 20)
assert(EvalElse_IfChain(false, true, true) == 20)
assert(EvalElse_IfChain(true, false, false) == 10)
assert(EvalElse_IfChain(true, false, true) == 10)
assert(EvalElse_IfChain(true, true, false) == 10)
assert(EvalElse_IfChain(true, true, true) == 10)

-- Test nesting of if-else expressions inside the condition of an if-else expression
assert((if (if True() then false else true) then 10 else 20) == 20)
assert((if if True() then false else true then 10 else 20) == 20)


-- Ensure that if/else expressions are conditionally evaluated
-- i.e. verify the evaluated expression doesn't evaluate the true and false expressions and
-- merely select the proper value.
CheckForConditionalEvaluation(true)
CheckForConditionalEvaluation(false)

return('OK')
