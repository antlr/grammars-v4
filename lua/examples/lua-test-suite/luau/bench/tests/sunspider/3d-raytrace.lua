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

local size = 30

local function createVector(x,y,z)
    return { x,y,z };
end

local function sqrLengthVector(self)
    return self[1] * self[1] + self[2] * self[2] + self[3] * self[3];
end

local function lengthVector(self)
    return math.sqrt(self[1] * self[1] + self[2] * self[2] + self[3] * self[3]);
end

local function addVector(self, v)
    self[1] = self[1] + v[1];
    self[2] = self[2] + v[2];
    self[3] = self[3] + v[3];
    return self;
end

local function subVector(self, v)
    self[1] = self[1] - v[1];
    self[2] = self[2] - v[2];
    self[3] = self[3] - v[3];
    return self;
end

local function scaleVector(self, scale)
    self[1] = self[1] * scale;
    self[2] = self[2] * scale;
    self[3] = self[3] * scale;
    return self;
end

local function normaliseVector(self)
    local len = math.sqrt(self[1] * self[1] + self[2] * self[2] + self[3] * self[3]);
    self[1] = self[1] / len;
    self[2] = self[2] / len;
    self[3] = self[3] / len;
    return self;
end

local function add(v1, v2)
    return { v1[1] + v2[1], v1[2] + v2[2], v1[3] + v2[3] };
end

local function sub(v1, v2)
    return { v1[1] - v2[1], v1[2] - v2[2], v1[3] - v2[3] };
end

local function scalev(v1, v2)
    return { v1[1] * v2[1], v1[2] * v2[2], v1[3] * v2[3] };
end

local function dot(v1, v2)
    return v1[1] * v2[1] + v1[2] * v2[2] + v1[3] * v2[3];
end

local function scale(v, scale)
    return { v[1] * scale, v[2] * scale, v[3] * scale };
end

local function cross(v1, v2)
    return { v1[2] * v2[3] - v1[3] * v2[2], 
            v1[3] * v2[1] - v1[1] * v2[3],
            v1[1] * v2[2] - v1[2] * v2[1] };

end

local function normalise(v)
    local len = lengthVector(v);
    return { v[1] / len, v[2] / len, v[3] / len };
end

local function transformMatrix(self, v)
    local vals = self;
    local x  = vals[1] * v[1] + vals[2] * v[2] + vals[3] * v[3] + vals[4];
    local y  = vals[5] * v[1] + vals[6] * v[2] + vals[7] * v[3] + vals[8];
    local z  = vals[9] * v[1] + vals[10] * v[2] + vals[11] * v[3] + vals[12];
    return { x, y, z };
end

local function invertMatrix(self)
    local temp = {}
    local tx = -self[4];
    local ty = -self[8];
    local tz = -self[12];
    for h = 0,2 do
        for v = 0,2 do 
            temp[h + v * 4 + 1] = self[v + h * 4 + 1];
        end
    end

    for i = 0,10 do
        self[i + 1] = temp[i + 1];
    end

    self[4] = tx * self[1] + ty * self[2] + tz * self[3];
    self[8] = tx * self[5] + ty * self[6] + tz * self[7];
    self[12] = tx * self[9] + ty * self[10] + tz * self[11];
    return self;
end

-- Triangle intersection using barycentric coord method
local function Triangle(p1, p2, p3)
    local this = {}

    local edge1 = sub(p3, p1);
    local edge2 = sub(p2, p1);
    local normal = cross(edge1, edge2);
    if (math.abs(normal[1]) > math.abs(normal[2])) then
        if (math.abs(normal[1]) > math.abs(normal[3])) then
            this.axis = 0; 
        else 
            this.axis = 2;
        end
    else
        if (math.abs(normal[2]) > math.abs(normal[3])) then
            this.axis = 1;
        else 
            this.axis = 2;
        end
    end

    local u = (this.axis + 1) % 3;
    local v = (this.axis + 2) % 3;
    local u1 = edge1[u + 1];
    local v1 = edge1[v + 1];
    
    local u2 = edge2[u + 1];
    local v2 = edge2[v + 1];
    this.normal = normalise(normal);
    this.nu = normal[u + 1] / normal[this.axis + 1];
    this.nv = normal[v + 1] / normal[this.axis + 1];
    this.nd = dot(normal, p1) / normal[this.axis + 1];
    local det = u1 * v2 - v1 * u2;
    this.eu = p1[u + 1];
    this.ev = p1[v + 1]; 
    this.nu1 = u1 / det;
    this.nv1 = -v1 / det;
    this.nu2 = v2 / det;
    this.nv2 = -u2 / det; 
    this.material = { 0.7, 0.7, 0.7 };

    
    this.intersect = function(self, orig, dir, near, far)
        local u = (self.axis + 1) % 3;
        local v = (self.axis + 2) % 3;
        local d = dir[self.axis + 1] + self.nu * dir[u + 1] + self.nv * dir[v + 1];
        local t = (self.nd - orig[self.axis + 1] - self.nu * orig[u + 1] - self.nv * orig[v + 1]) / d;

        if (t < near or t > far) then
            return nil;
        end

        local Pu = orig[u + 1] + t * dir[u + 1] - self.eu;
        local Pv = orig[v + 1] + t * dir[v + 1] - self.ev;
        local a2 = Pv * self.nu1 + Pu * self.nv1;

        if (a2 < 0) then
            return nil;
        end

        local a3 = Pu * self.nu2 + Pv * self.nv2;
        if (a3 < 0) then
            return nil;
        end

        if ((a2 + a3) > 1) then
            return nil;
        end

        return t;
    end

    return this
end

local function Scene(a_triangles)
    local this = {}
    this.triangles = a_triangles;
    this.lights = {};
    this.ambient = {0,0,0};
    this.background = {0.8,0.8,1};

    this.intersect = function(self, origin, dir, near, far)
        local closest = nil;
        for i = 0,#self.triangles-1 do
            local triangle = self.triangles[i + 1];   
            local d = triangle:intersect(origin, dir, near, far);
            if (d == nil or d > far or d < near) then
                -- continue;
            else
                far = d;
                closest = triangle;
            end
        end
        
        if (not closest) then
            return { self.background[1],self.background[2],self.background[3] };
        end

        local normal = closest.normal;
        local hit = add(origin, scale(dir, far)); 
        if (dot(dir, normal) > 0) then
            normal = { -normal[1], -normal[2], -normal[3] };
        end

        local colour = nil;
        if (closest.shader) then
            colour = closest.shader(closest, hit, dir);
        else
            colour = closest.material;
        end
        
        -- do reflection
        local reflected = nil;
        if (colour.reflection or 0 > 0.001) then
            local reflection = addVector(scale(normal, -2*dot(dir, normal)), dir);
            reflected = self:intersect(hit, reflection, 0.0001, 1000000);
            if (colour.reflection >= 0.999999) then
                return reflected;
            end
        end
        
        local l = { self.ambient[1], self.ambient[2], self.ambient[3] };

        for i = 0,#self.lights-1 do
            local light = self.lights[i + 1];
            local toLight = sub(light, hit);
            local distance = lengthVector(toLight);
            scaleVector(toLight, 1.0/distance);
            distance = distance - 0.0001;

            if (self:blocked(hit, toLight, distance)) then
                -- continue;
            else
                local nl = dot(normal, toLight);
                if (nl > 0) then
                    addVector(l, scale(light.colour, nl));
                end
            end
        end

        l = scalev(l, colour);
        if (reflected) then
            l = addVector(scaleVector(l, 1 - colour.reflection), scaleVector(reflected, colour.reflection));
        end

        return l;
    end

    this.blocked = function(self, O, D, far)
        local near = 0.0001;
        local closest = nil;
        for i = 0,#self.triangles-1 do
            local triangle = self.triangles[i + 1];   
            local d = triangle:intersect(O, D, near, far);
            if (d == nil or d > far or d < near) then
                --continue;
            else
                return true;
            end
        end
        
        return false;
    end

    return this
end

local zero = { 0,0,0 };

-- this camera code is from notes i made ages ago, it is from *somewhere* -- i cannot remember where
-- that somewhere is
local function Camera(origin, lookat, up)
    local this = {}

    local zaxis = normaliseVector(subVector(lookat, origin));
    local xaxis = normaliseVector(cross(up, zaxis));
    local yaxis = normaliseVector(cross(xaxis, subVector({ 0,0,0 }, zaxis)));
    local m = {};
    m[1] = xaxis[1]; m[2] = xaxis[2]; m[3] = xaxis[3];
    m[5] = yaxis[1]; m[6] = yaxis[2]; m[7] = yaxis[3];
    m[9] = zaxis[1]; m[10] = zaxis[2]; m[11] = zaxis[3];
    m[4] = 0; m[8] = 0; m[12] = 0;
    invertMatrix(m);
    m[4] = 0; m[8] = 0; m[12] = 0;
    this.origin = origin;
    this.directions = {};
    this.directions[1] = normalise({ -0.7,  0.7, 1 });
    this.directions[2] = normalise({ 0.7,  0.7, 1 });
    this.directions[3] = normalise({ 0.7, -0.7, 1 });
    this.directions[4] = normalise({ -0.7, -0.7, 1 });
    this.directions[1] = transformMatrix(m, this.directions[1]);
    this.directions[2] = transformMatrix(m, this.directions[2]);
    this.directions[3] = transformMatrix(m, this.directions[3]);
    this.directions[4] = transformMatrix(m, this.directions[4]);

    this.generateRayPair = function(self, y)
        rays = { {}, {} }
        rays[1].origin = self.origin;
        rays[2].origin = self.origin;
        rays[1].dir = addVector(scale(self.directions[1], y), scale(self.directions[4], 1 - y));
        rays[2].dir = addVector(scale(self.directions[2], y), scale(self.directions[3], 1 - y));
        return rays;
    end

    function renderRows(camera, scene, pixels, width, height, starty, stopy)
        for y = starty,stopy-1 do
            local rays = camera:generateRayPair(y / height);
            for x = 0,width-1 do
                local xp = x / width;
                local origin = addVector(scale(rays[1].origin, xp), scale(rays[2].origin, 1 - xp));
                local dir = normaliseVector(addVector(scale(rays[1].dir, xp), scale(rays[2].dir, 1 - xp)));
                local l = scene:intersect(origin, dir, 0, math.huge);
                pixels[y + 1][x + 1] = l;
            end
        end
    end

    this.render = function(self, scene, pixels, width, height)
        local cam = self;
        local row = 0;
        renderRows(cam, scene, pixels, width, height, 0, height);
    end

    return this
end

local function raytraceScene()
    local startDate = 13154863;
    local numTriangles = 2 * 6;
    local triangles = {}; -- numTriangles);
    local tfl = createVector(-10,  10, -10);
    local tfr = createVector( 10,  10, -10);
    local tbl = createVector(-10,  10,  10);
    local tbr = createVector( 10,  10,  10);
    local bfl = createVector(-10, -10, -10);
    local bfr = createVector( 10, -10, -10);
    local bbl = createVector(-10, -10,  10);
    local bbr = createVector( 10, -10,  10);
    
    -- cube!!!
    -- front
    local i = 0;
    
    triangles[i + 1] = Triangle(tfl, tfr, bfr); i = i + 1;
    triangles[i + 1] = Triangle(tfl, bfr, bfl); i = i + 1;
    -- back
    triangles[i + 1] = Triangle(tbl, tbr, bbr); i = i + 1;
    triangles[i + 1] = Triangle(tbl, bbr, bbl); i = i + 1;
    --        triangles[i-1].material = [0.7,0.2,0.2];
    --            triangles[i-1].material.reflection = 0.8;
    -- left
    triangles[i + 1] = Triangle(tbl, tfl, bbl); i = i + 1;
    --            triangles[i-1].reflection = 0.6;
    triangles[i + 1] = Triangle(tfl, bfl, bbl); i = i + 1;
    --            triangles[i-1].reflection = 0.6;
    -- right
    triangles[i + 1] = Triangle(tbr, tfr, bbr); i = i + 1;
    triangles[i + 1] = Triangle(tfr, bfr, bbr); i = i + 1;
    -- top
    triangles[i + 1] = Triangle(tbl, tbr, tfr); i = i + 1;
    triangles[i + 1] = Triangle(tbl, tfr, tfl); i = i + 1;
    -- bottom
    triangles[i + 1] = Triangle(bbl, bbr, bfr); i = i + 1;
    triangles[i + 1] = Triangle(bbl, bfr, bfl); i = i + 1;
    
    -- Floor!!!!
    local green = createVector(0.0, 0.4, 0.0);
    green.reflection = 0; --
    local grey = createVector(0.4, 0.4, 0.4);
    grey.reflection = 1.0;
    local floorShader = function(tri, pos, view)
        local x = ((pos[1]/32) % 2 + 2) % 2;
        local z = ((pos[3]/32 + 0.3) % 2 + 2) % 2;
        if ((x < 1) ~= (z < 1)) then
            --in the real world we use the fresnel term...
            --    local angle = 1-dot(view, tri.normal);
            --   angle *= angle;
            --  angle *= angle;
            -- angle *= angle;
            --grey.reflection = angle;
            return grey;
        else 
            return green;
        end
    end

    local ffl = createVector(-1000, -30, -1000);
    local ffr = createVector( 1000, -30, -1000);
    local fbl = createVector(-1000, -30,  1000);
    local fbr = createVector( 1000, -30,  1000);
    triangles[i + 1] = Triangle(fbl, fbr, ffr); i = i + 1;
    triangles[i-1 + 1].shader = floorShader;
    triangles[i + 1] = Triangle(fbl, ffr, ffl); i = i + 1;
    triangles[i-1 + 1].shader = floorShader;
    
    local _scene = Scene(triangles);
    _scene.lights[1] = createVector(20, 38, -22);
    _scene.lights[1].colour = createVector(0.7, 0.3, 0.3);
    _scene.lights[2] = createVector(-23, 40, 17);
    _scene.lights[2].colour = createVector(0.7, 0.3, 0.3);
    _scene.lights[3] = createVector(23, 20, 17);
    _scene.lights[3].colour = createVector(0.7, 0.7, 0.7);
    _scene.ambient = createVector(0.1, 0.1, 0.1);
    --  _scene.background = createVector(0.7, 0.7, 1.0);
    
    local pixels = {};
    for y = 0,size-1 do
        pixels[y + 1] = {};
        for x = 0,size-1 do
            pixels[y + 1][x + 1] = 0;
        end
    end

    local _camera = Camera(createVector(-40, 40, 40), createVector(0, 0, 0), createVector(0, 1, 0));
    _camera:render(_scene, pixels, size, size);

    return pixels;
end

local function arrayToCanvasCommands(pixels)
    local s = {};
    table.insert(s, '<!DOCTYPE html><html><head><title>Test</title></head><body><canvas id="renderCanvas" width="' .. size .. 'px" height="' .. size .. 'px"></canvas><scr' .. 'ipt>\nvar pixels = [');
    for y = 0,size-1 do
        table.insert(s, "[");
        for x = 0,size-1 do
            table.insert(s, "[" .. math.floor(pixels[y + 1][x + 1][1] * 255) .. "," .. math.floor(pixels[y + 1][x + 1][2] * 255) .. "," .. math.floor(pixels[y + 1][x + 1][3] * 255) .. "],");
        end
        table.insert(s, "],");
    end
    table.insert(s, '];\n    var canvas = document.getElementById("renderCanvas").getContext("2d");\n\
\n\
\n\
    var size = ' .. size .. ';\n\
    canvas.fillStyle = "red";\n\
    canvas.fillRect(0, 0, size, size);\n\
    canvas.scale(1, -1);\n\
    canvas.translate(0, -size);\n\
\n\
    if (!canvas.setFillColor)\n\
        canvas.setFillColor = function(r, g, b, a) {\n\
            this.fillStyle = "rgb("+[Math.floor(r), Math.floor(g), Math.floor(b)]+")";\n\
    }\n\
\n\
for (var y = 0; y < size; y++) {\n\
  for (var x = 0; x < size; x++) {\n\
    var l = pixels[y][x];\n\
    canvas.setFillColor(l[0], l[1], l[2], 1);\n\
    canvas.fillRect(x, y, 1, 1);\n\
  }\n\
}</script></body></html>');

    return table.concat(s);
end

local testOutput = arrayToCanvasCommands(raytraceScene());

--local f = io.output("output.html")
--f:write(testOutput)
--f:close()

local expectedLength = 11599;
local testLength = #testOutput

if (testLength ~= expectedLength) then
    assert(false, "Error: bad result: expected length " .. expectedLength .. " but got " .. testLength);
end

end

bench.runCode(test, "3d-raytrace")
