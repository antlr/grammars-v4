local bench = script and require(script.Parent.bench_support) or require("bench_support")

local PI = 3.141592653589793
local SOLAR_MASS = 4 * PI * PI
local DAYS_PER_YEAR = 365.24

local Body = {}
Body.__index = Body

function Body.new(x, y, z, vx, vy, vz, mass)
    local self = {}
    self.x = x
    self.y = y
    self.z = z
    self.vx = vx
    self.vy = vy
    self.vz = vz
    self.mass = mass
    return setmetatable(self, Body)
end

function Body:offsetMomentum(px, py, pz)
    self.vx = -px / SOLAR_MASS
    self.vy = -py / SOLAR_MASS
    self.vz = -pz / SOLAR_MASS

    return self
end

local function Jupiter()
    return Body.new(
      4.841431442464721e0,
      -1.1603200440274284e0,
      -1.036220444711231e-1,
      1.660076642744037e-3 * DAYS_PER_YEAR,
      7.6990111841974045e-3 * DAYS_PER_YEAR,
      -6.90460016972063e-5 * DAYS_PER_YEAR,
      9.547919384243267e-4 * SOLAR_MASS
    )
end
local function Saturn()
    return Body.new(8.34336671824458e0, 4.124798564124305e0, -4.035234171143213e-1, -2.767425107268624e-3 * DAYS_PER_YEAR, 4.998528012349173e-3 * DAYS_PER_YEAR, 2.3041729757376395e-5 * DAYS_PER_YEAR, 2.8588598066613082e-4 * SOLAR_MASS)
end
local function Uranus()
    return Body.new(1.2894369562139132e1, -1.511115140169863e1, -2.2330757889265573e-1, 2.964601375647616e-3 * DAYS_PER_YEAR, 2.3784717395948096e-3 * DAYS_PER_YEAR, -2.9658956854023755e-5 * DAYS_PER_YEAR, 4.366244043351563e-5 * SOLAR_MASS)
end
local function Neptune()
    return Body.new(1.5379697114850917e1, -2.5919314609987962e1, 1.7925877295037118e-1, 2.680677724903893e-3 * DAYS_PER_YEAR, 1.628241700382423e-3 * DAYS_PER_YEAR, -9.515922545197158e-5 * DAYS_PER_YEAR, 5.151389020466114e-5 * SOLAR_MASS)
end
local function Sun()
    return Body.new(0, 0, 0, 0, 0, 0, SOLAR_MASS)
end

local NBodySystem = {}
NBodySystem.__index = NBodySystem


function NBodySystem.new(bodies)
    local self = {}
    self.bodies = bodies

    local px = 0
    local py = 0
    local pz = 0
    local size = #self.bodies

    for i=1, size do
        local b = self.bodies[i]
        local m = b.mass

        px = px + b.vx * m
        py = py + b.vy * m
        pz = pz + b.vz * m
    end

    self.bodies[1]:offsetMomentum(px, py, pz)

    return setmetatable(self, NBodySystem)
end

function NBodySystem:advance(dt)
    local dx, dy, dz, distance, mag
    local size = #self.bodies

    for i=1, size do
      local bodyi = self.bodies[i]
      for j=i+1, size do
         local bodyj = self.bodies[j]
         dx = bodyi.x - bodyj.x
         dy = bodyi.y - bodyj.y
         dz = bodyi.z - bodyj.z

         distance = math.sqrt(dx*dx + dy*dy + dz*dz)
         mag = dt / (distance * distance * distance)

         bodyi.vx -= dx * bodyj.mass * mag
         bodyi.vy -= dy * bodyj.mass * mag
         bodyi.vz -= dz * bodyj.mass * mag

         bodyj.vx += dx * bodyi.mass * mag
         bodyj.vy += dy * bodyi.mass * mag
         bodyj.vz += dz * bodyi.mass * mag
      end
    end
    for i=1, size do
        local body = self.bodies[i]

        body.x = body.x + dt * body.vx
        body.y = body.y + dt * body.vy
        body.z = body.z + dt * body.vz
    end
end

function NBodySystem:energy()
    local dx, dy, dz, distance
    local e = 0.0
    local size = #self.bodies

    for i=1, size do
        local bodyi = self.bodies[i]

        e = e + 0.5 * bodyi.mass * (bodyi.vx * bodyi.vx + bodyi.vy * bodyi.vy + bodyi.vz * bodyi.vz)

      for j=i+1, size do
         local bodyj = self.bodies[j]
         dx = bodyi.x - bodyj.x
         dy = bodyi.y - bodyj.y
         dz = bodyi.z - bodyj.z

         distance = math.sqrt(dx*dx + dy*dy + dz*dz)
         e -= (bodyi.mass * bodyj.mass) / distance
      end
    end

    return e
end

local function run()
    local ret = 0
    local n = 3
    while n <= 24 do
        (function()
            local bodies = NBodySystem.new({ 
               Sun(),Jupiter(),Saturn(),Uranus(),Neptune()
            })
            local max = n * 100
            
            ret += bodies:energy()
            for i=1, max do
                bodies:advance(0.01)
            end 
            ret += bodies:energy()
        end)()
        n *= 2
    end
    local expected = -1.3524862408537381

    if ret ~= expected then
        error('ERROR: bad result: expected ' .. expected .. ' but got ' .. ret)
    end
end

function runIteration()
    for i=1, 5 do
        run()
    end
end

bench.runCode(runIteration, "n-body-oop")
