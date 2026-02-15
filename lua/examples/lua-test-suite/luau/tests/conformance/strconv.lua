-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print("testing string-number conversion")

-- zero
assert(tostring(0) == "0")
assert(tostring(0/-1) == "-0")

-- specials
assert(tostring(1/0) == "inf")
assert(tostring(-1/0) == "-inf")
assert(tostring(0/0) == "nan")

-- integers
assert(tostring(1) == "1")
assert(tostring(42) == "42")
assert(tostring(-4294967296) == "-4294967296")
assert(tostring(9007199254740991) == "9007199254740991")

-- decimals
assert(tostring(0.5) == "0.5")
assert(tostring(0.1) == "0.1")
assert(tostring(-0.17) == "-0.17")
assert(tostring(math.pi) == "3.141592653589793")

-- fuzzing corpus
assert(tostring(5.4536123983019448e-311) == "5.453612398302e-311")
assert(tostring(5.4834368411298348e-311) == "5.48343684113e-311")
assert(tostring(4.4154895841930002e-305) == "4.415489584193e-305")
assert(tostring(1125968630513728) == "1125968630513728")
assert(tostring(3.3951932655938423e-313) == "3.3951932656e-313")
assert(tostring(1.625) == "1.625")
assert(tostring(4.9406564584124654e-324) == "5.e-324")
assert(tostring(2.0049288280105384) == "2.0049288280105384")
assert(tostring(3.0517578125e-05) == "0.000030517578125")
assert(tostring(1.383544921875) == "1.383544921875")
assert(tostring(3.0053350932691001) == "3.0053350932691")
assert(tostring(0.0001373291015625) == "0.0001373291015625")
assert(tostring(-1.9490628022799998e+289) == "-1.94906280228e+289")
assert(tostring(-0.00610404721867928) == "-0.00610404721867928")
assert(tostring(0.00014495849609375) == "0.00014495849609375")
assert(tostring(0.453125) == "0.453125")
assert(tostring(-4.2375343999999997e+73) == "-4.2375344e+73")
assert(tostring(1.3202313930270133e-192) == "1.3202313930270133e-192")
assert(tostring(3.6984408976312836e+19) == "36984408976312840000")
assert(tostring(2.0563000527063302) == "2.05630005270633")
assert(tostring(4.8970527433648997e-260) == "4.8970527433649e-260")
assert(tostring(1.62890625) == "1.62890625")
assert(tostring(1.1295093211933533e+65) == "1.1295093211933533e+65")

return "OK"
