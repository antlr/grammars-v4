--[[
MIT License

Copyright (c) 2017 Gabriel de Quadros Ligneul

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
]]
local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

local Complex={type="package"}

local function complex(x,y)
 return setmetatable({ re=x, im=y }, Complex.metatable)
end

function Complex.conj(x,y)
 return complex(x.re,-x.im)
end

function Complex.norm2(x)
 local n=Complex.mul(x,Complex.conj(x))
 return n.re
end

function Complex.abs(x)
 return sqrt(Complex.norm2(x))
end

function Complex.add(x,y)
 return complex(x.re+y.re,x.im+y.im)
end

function Complex.mul(x,y)
 return complex(x.re*y.re-x.im*y.im,x.re*y.im+x.im*y.re)
end

Complex.metatable={
	__add = Complex.add,
	__mul = Complex.mul,
}

local function abs(x)
 return math.sqrt(Complex.norm2(x))
end

xmin=-2.0	xmax=2.0	ymin=-2.0	ymax=2.0
N=(arg and arg[1]) or 64

function level(x,y)
 local c=complex(x,y)
 local l=0
 local z=c
 repeat
  z=z*z+c
  l=l+1
 until abs(z)>2.0 or l>255
 return l-1
end

dx=(xmax-xmin)/N
dy=(ymax-ymin)/N

print("P2")
print("# mandelbrot set",xmin,xmax,ymin,ymax,N)
print(N,N,255)
local S = 0
for i=1,N do
 local x=xmin+(i-1)*dx
 for j=1,N do
  local y=ymin+(j-1)*dy
  S = S + level(x,y)
 end
 -- if i % 10 == 0 then print(collectgarbage("count")) end
end
print(S)

end

bench.runCode(test, "mandel")
