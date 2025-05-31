--[[
 * Copyright (C) 2007 Apple Inc.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
]]

local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

local loops = 15
local nx = 120
local nz = 120

local function morph(a, f)
    local PI2nx = math.pi * 8/nx
    local sin = math.sin
    local f30 = -(50 * sin(f*math.pi*2))
    
    for i = 0,nz-1 do
        for j = 0,nx-1 do
            a[3*(i*nx+j)+1]    = sin((j-1) * PI2nx ) * -f30
        end
    end
end

    
local a = {}
for i = 0,nx*nz*3-1 do 
    a[i] = 0
end

for i = 0,loops-1 do
    morph(a, i/loops)
end

testOutput = 0;
for i = 0,nx-1 do
    testOutput = testOutput + a[3*(i*nx+i)+1];
end

a = nil;

-- This has to be an approximate test since ECMAscript doesn't formally specify
-- what sin() returns. Even if it did specify something like for example what Java 7
-- says - that sin() has to return a value within 1 ulp of exact - then we still
-- would not be able to do an exact test here since that would allow for just enough
-- low-bit slop to create possibly big errors due to testOutput being a sum.
local epsilon = 1e-13;
if (math.abs(testOutput) >= epsilon) then
    assert(false, "Error: bad test output: expected magnitude below " .. epsilon .. " but got " .. testOutput);
end

end

bench.runCode(test, "3d-morph")
