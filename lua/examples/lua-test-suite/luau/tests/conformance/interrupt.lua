-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing interrupts")

function foo()
    for i=1,10 do end
    return
end

foo()

return "OK"
