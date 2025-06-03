--[[
 * Copyright (C) Rich Moore.  All rights reserved.
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
 * THIS SOFTWARE IS PROVIDED BY CONTRIBUTORS ``AS IS'' AND ANY
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

--. Start CORDIC

local AG_CONST = 0.6072529350;

local function FIXED(X)
  return X * 65536.0;
end

local function FLOAT(X)
  return X / 65536.0;
end

local function DEG2RAD(X)
  return 0.017453 * (X);
end

local Angles = {
  FIXED(45.0), FIXED(26.565), FIXED(14.0362), FIXED(7.12502),
  FIXED(3.57633), FIXED(1.78991), FIXED(0.895174), FIXED(0.447614),
  FIXED(0.223811), FIXED(0.111906), FIXED(0.055953),
  FIXED(0.027977) 
};

local Target = 28.027;

local function cordicsincos(Target)
    local X;
    local Y;
    local TargetAngle;
    local CurrAngle;

    X = FIXED(AG_CONST);         -- AG_CONST * cos(0)
    Y = 0;                       -- AG_CONST * sin(0)

    TargetAngle = FIXED(Target);
    CurrAngle = 0;
    for Step = 0,11 do
        local NewX;
        if (TargetAngle > CurrAngle) then
            NewX = X - bit32.rshift(math.floor(Y), Step) -- (Y >> Step);
            Y = bit32.rshift(math.floor(X), Step) + Y;
            X = NewX;
            CurrAngle = CurrAngle + Angles[Step + 1];
        else
            NewX = X + bit32.rshift(math.floor(Y), Step)
            Y = -bit32.rshift(math.floor(X), Step) + Y;
            X = NewX;
            CurrAngle = CurrAngle - Angles[Step + 1];
        end
    end

    return FLOAT(X) * FLOAT(Y);
end

-- End CORDIC

local total = 0;

local function cordic( runs )
  for i = 1,runs do
      total = total + cordicsincos(Target);
  end
end

cordic(25000);

local expected = 10362.570468755888;

if (total ~= expected) then
    assert(false, "ERROR: bad result: expected " .. expected .. " but got " .. total);
end

end

bench.runCode(test, "math-cordic")
