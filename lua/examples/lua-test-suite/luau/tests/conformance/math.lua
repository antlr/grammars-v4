-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print("testing numbers and math lib")

do
  local a,b,c = "2", " 3e0 ", " 10  "
  assert(a+b == 5 and -b == -3 and b+"2" == 5 and "10"-c == 0)
  assert(type(a) == 'string' and type(b) == 'string' and type(c) == 'string')
  assert(a == "2" and b == " 3e0 " and c == " 10  " and -c == -"  10 ")
  assert(c%a == 0 and a^b == 8)
end


do
  local a,b = math.modf(3.5)
  assert(a == 3 and b == 0.5)
  assert(math.huge > 10e30)
  assert(-math.huge < -10e30)
end

function f(...)
  if select('#', ...) == 1 then
    return (...)
  else
    return "***"
  end
end

assert(pcall(tonumber) == false)
assert(tonumber{} == nil)
assert(tonumber'+0.01' == 1/100 and tonumber'+.01' == 0.01 and
       tonumber'.01' == 0.01    and tonumber'-1.' == -1 and
       tonumber'+1.' == 1)
assert(tonumber'+ 0.01' == nil and tonumber'+.e1' == nil and
       tonumber'1e' == nil     and tonumber'1.0e+' == nil and
       tonumber'.' == nil)
assert(tonumber('-12') == -10-2)
assert(tonumber('-1.2e2') == - - -120)
assert(f(tonumber('1  a')) == nil)
assert(f(tonumber('e1')) == nil)
assert(f(tonumber('e  1')) == nil)
assert(f(tonumber(' 3.4.5 ')) == nil)
assert(f(tonumber('')) == nil)
assert(f(tonumber('', 8)) == nil)
assert(f(tonumber('  ')) == nil)
assert(f(tonumber('  ', 9)) == nil)
assert(f(tonumber('99', 8)) == nil)
assert(tonumber('  1010  ', 2) == 10)
assert(tonumber('10', 36) == 36)
--assert(tonumber('\n  -10  \n', 36) == -36)
--assert(tonumber('-fFfa', 16) == -(10+(16*(15+(16*(15+(16*15)))))))
assert(tonumber('fFfa', 15) == nil)
--assert(tonumber(string.rep('1', 42), 2) + 1 == 2^42)
assert(tonumber(string.rep('1', 32), 2) + 1 == 2^32)
--assert(tonumber('-fffffFFFFF', 16)-1 == -2^40)
assert(tonumber('ffffFFFF', 16)+1 == 2^32)

assert(1.1 == 1.+.1)
assert(100.0 == 1E2 and .01 == 1e-2)
assert(1111111111111111-1111111111111110== 1000.00e-03)
--     1234567890123456
assert(1.1 == '1.'+'.1')
assert('1111111111111111'-'1111111111111110' == tonumber"  +0.001e+3 \n\t")

function eq (a,b,limit)
  if not limit then limit = 10E-10 end
  return math.abs(a-b) <= limit
end

assert(0.1e-30 > 0.9E-31 and 0.9E30 < 0.1e31)

assert(0.123456 > 0.123455)

assert(tonumber('+1.23E30') == 1.23*10^30)

-- testing order operators
assert(not(1<1) and (1<2) and not(2<1))
assert(not('a'<'a') and ('a'<'b') and not('b'<'a'))
assert((1<=1) and (1<=2) and not(2<=1))
assert(('a'<='a') and ('a'<='b') and not('b'<='a'))
assert(not(1>1) and not(1>2) and (2>1))
assert(not('a'>'a') and not('a'>'b') and ('b'>'a'))
assert((1>=1) and not(1>=2) and (2>=1))
assert(('a'>='a') and not('a'>='b') and ('b'>='a'))

-- testing mod operator
assert(-4%3 == 2)
assert(4%-3 == -2)
assert(math.pi - math.pi % 1 == 3)
assert(math.pi - math.pi % 0.001 == 3.141)

do
  local a = 3 % 0;
  assert(a ~= a) -- Expect NaN
  assert(((2^53+1) % 2) == 0)
  assert((1234 % (2^53+1)) == 1234)
end

local function testbit(a, n)
  return a/2^n % 2 >= 1
end

assert(eq(math.sin(-9.8)^2 + math.cos(-9.8)^2, 1))
assert(eq(math.tan(math.pi/4), 1))
assert(eq(math.sin(math.pi/2), 1) and eq(math.cos(math.pi/2), 0))
assert(eq(math.atan(1), math.pi/4) and eq(math.acos(0), math.pi/2) and
       eq(math.asin(1), math.pi/2))
assert(eq(math.deg(math.pi/2), 90) and eq(math.rad(90), math.pi/2))
assert(math.abs(-10) == 10)
assert(eq(math.atan2(1,0), math.pi/2))
assert(math.ceil(4.5) == 5.0)
assert(math.floor(4.5) == 4.0)
assert(10 % 3 == 1)
assert(eq(math.sqrt(10)^2, 10))
assert(eq(math.log10(2), math.log(2)/math.log(10)))
assert(eq(math.log(2, 2), 1))
assert(eq(math.log(9, 3), 2))
assert(eq(math.log(100, 10), 2))
assert(eq(math.exp(0), 1))
assert(eq(math.sin(10), math.sin(10%(2*math.pi))))
local v,e = math.frexp(math.pi)
assert(eq(math.ldexp(v,e), math.pi))

assert(eq(math.tanh(3.5), math.sinh(3.5)/math.cosh(3.5)))

assert(tonumber(' 1.3e-2 ') == 1.3e-2)
assert(tonumber(' -1.00000000000001 ') == -1.00000000000001)

-- testing constant limits
-- 2^23 = 8388608
assert(8388609 + -8388609 == 0)
assert(8388608 + -8388608 == 0)
assert(8388607 + -8388607 == 0)

if rawget(_G, "_soft") then return end

f = "a = {"
i = 1
repeat
  f = f .. "{" .. math.sin(i) .. ", " .. math.cos(i) .. ", " .. (i/3) .. "},\n"
  i=i+1
until i > 1000
f = f .. "}"
assert(loadstring(f))()

assert(eq(a[300][1], math.sin(300)))
assert(eq(a[600][1], math.sin(600)))
assert(eq(a[500][2], math.cos(500)))
assert(eq(a[800][2], math.cos(800)))
assert(eq(a[200][3], 200/3))
assert(eq(a[1000][3], 1000/3, 0.001))
print('+')

do   -- testing NaN
  local NaN = 10e500 - 10e400
  assert(NaN ~= NaN)
  assert(not (NaN < NaN))
  assert(not (NaN <= NaN))
  assert(not (NaN > NaN))
  assert(not (NaN >= NaN))
  assert(not (0 < NaN))
  assert(not (NaN < 0))
  local a = {}
  assert(not pcall(function () a[NaN] = 1 end))
  assert(a[NaN] == nil)
  a[1] = 1
  assert(not pcall(function () a[NaN] = 1 end))
  assert(a[NaN] == nil)
end

-- require "checktable"
-- stat(a)

a = nil

-- testing implicit conversions

local a,b = '10', '20'
assert(a*b == 200 and a+b == 30 and a-b == -10 and a/b == 0.5 and -b == -20)
assert(a == '10' and b == '20')


math.randomseed(0)

local i = 0
local Max = 0
local Min = 2
repeat
  local t = math.random()
  Max = math.max(Max, t)
  Min = math.min(Min, t)
  i=i+1
  flag = eq(Max, 1, 0.001) and eq(Min, 0, 0.001)
until flag or i>10000
assert(0 <= Min and Max<1)
assert(flag);

for i=1,10 do
  local t = math.random(5)
  assert(1 <= t and t <= 5)
end

i = 0
Max = -200
Min = 200
repeat
  local t = math.random(-10,0)
  Max = math.max(Max, t)
  Min = math.min(Min, t)
  i=i+1
  flag = (Max == 0 and Min == -10)
until flag or i>10000
assert(-10 <= Min and Max<=0)
assert(flag);

assert(select(2, pcall(math.random, 1, 2, 3)):match("wrong number of arguments"))

-- noise
assert(math.noise(0.5) == 0)
assert(math.noise(0.5, 0.5) == -0.25)
assert(math.noise(0.5, 0.5, -0.5) == 0.125)

local inf = math.huge * 2
local nan = 0 / 0

-- sign
assert(math.sign(0) == 0)
assert(math.sign(42) == 1)
assert(math.sign(-42) == -1)
assert(math.sign(inf) == 1)
assert(math.sign(-inf) == -1)
assert(math.sign(nan) == 0)

-- clamp
assert(math.clamp(-1, 0, 1) == 0)
assert(math.clamp(0.5, 0, 1) == 0.5)
assert(math.clamp(2, 0, 1) == 1)
assert(math.clamp(4, 0, 0) == 0)

-- round
assert(math.round(0) == 0)
assert(math.round(0.4) == 0)
assert(math.round(0.5) == 1)
assert(math.round(3.5) == 4)
assert(math.round(-0.4) == 0)
assert(math.round(-0.5) == -1)
assert(math.round(-3.5) == -4)
assert(math.round(math.huge) == math.huge)

-- fmod
assert(math.fmod(3, 2) == 1)
assert(math.fmod(-3, 2) == -1)
assert(math.fmod(3, -2) == 1)
assert(math.fmod(-3, -2) == -1)

-- most of the tests above go through fastcall path
-- to make sure the basic implementations are also correct we test these functions with string->number coercions
assert(math.abs("-4") == 4)
assert(math.acos("1") == 0)
assert(math.asin("0") == 0)
assert(math.atan2("0", "0") == 0)
assert(math.atan("0") == 0)
assert(math.ceil("1.5") == 2)
assert(math.cosh("0") == 1)
assert(math.cos("0") == 1)
assert(math.deg("0") == 0)
assert(math.exp("0") == 1)
assert(math.floor("1.5") == 1)
assert(math.fmod("1.5", 1) == 0.5)
local v,e = math.frexp("1.5")
assert(v == 0.75 and e == 1)
assert(math.ldexp("0.75", 1) == 1.5)
assert(math.log10("10") == 1)
assert(math.log("0") == -inf)
assert(math.log("8", 2) == 3)
assert(math.log("10", 10) == 1)
assert(math.log("9", 3) == 2)
assert(math.max("1", 2) == 2)
assert(math.max(2, "1") == 2)
assert(math.min("1", 2) == 1)
assert(math.min(2, "1") == 1)
local v,f = math.modf("1.5")
assert(v == 1 and f == 0.5)
assert(math.pow("2", 2) == 4)
assert(math.rad("0") == 0)
assert(math.sinh("0") == 0)
assert(math.sin("0") == 0)
assert(math.sqrt("4") == 2)
assert(math.tanh("0") == 0)
assert(math.tan("0") == 0)
assert(math.clamp("0", 2, 3) == 2)
assert(math.clamp("4", 2, 3) == 3)
assert(math.sign("2") == 1)
assert(math.sign("-2") == -1)
assert(math.sign("0") == 0)
assert(math.round("1.8") == 2)

return('OK')
