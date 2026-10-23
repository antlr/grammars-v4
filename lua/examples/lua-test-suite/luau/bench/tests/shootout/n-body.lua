local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    --The Computer Language Benchmarks Game
    -- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
    --contributed by Mike Pall

    local PI = 3.141592653589793
    local SOLAR_MASS = 4 * PI * PI
    local DAYS_PER_YEAR = 365.24
    local bodies = {
        { --Sun
        x = 0,
        y = 0,
        z = 0,
        vx = 0,
        vy = 0,
        vz = 0,
        mass = SOLAR_MASS
        },
        { --Jupiter
        x = 4.84143144246472090e+00,
        y = -1.16032004402742839e+00,
        z = -1.03622044471123109e-01,
        vx = 1.66007664274403694e-03 * DAYS_PER_YEAR,
        vy = 7.69901118419740425e-03 * DAYS_PER_YEAR,
        vz = -6.90460016972063023e-05 * DAYS_PER_YEAR,
        mass = 9.54791938424326609e-04 * SOLAR_MASS
        },
        { --Saturn
        x = 8.34336671824457987e+00,
        y = 4.12479856412430479e+00,
        z = -4.03523417114321381e-01,
        vx = -2.76742510726862411e-03 * DAYS_PER_YEAR,
        vy = 4.99852801234917238e-03 * DAYS_PER_YEAR,
        vz = 2.30417297573763929e-05 * DAYS_PER_YEAR,
        mass = 2.85885980666130812e-04 * SOLAR_MASS
        },
        { --Uranus
        x = 1.28943695621391310e+01,
        y = -1.51111514016986312e+01,
        z = -2.23307578892655734e-01,
        vx = 2.96460137564761618e-03 * DAYS_PER_YEAR,
        vy = 2.37847173959480950e-03 * DAYS_PER_YEAR,
        vz = -2.96589568540237556e-05 * DAYS_PER_YEAR,
        mass = 4.36624404335156298e-05 * SOLAR_MASS
        },
        { --Neptune
        x = 1.53796971148509165e+01,
        y = -2.59193146099879641e+01,
        z = 1.79258772950371181e-01,
        vx = 2.68067772490389322e-03 * DAYS_PER_YEAR,
        vy = 1.62824170038242295e-03 * DAYS_PER_YEAR,
        vz = -9.51592254519715870e-05 * DAYS_PER_YEAR,
        mass = 5.15138902046611451e-05 * SOLAR_MASS
        }
    }

    local function advance(bodies, nbody, dt)
        for i = 1, nbody do
            local bi = bodies[i]
            local bix, biy, biz, bimass = bi.x, bi.y, bi.z, bi.mass
            local bivx, bivy, bivz = bi.vx, bi.vy, bi.vz
            for j = i + 1, nbody do
                local bj = bodies[j]
                local dx, dy, dz = bix - bj.x, biy - bj.y, biz - bj.z
                local distance = math.sqrt(dx*dx + dy*dy + dz*dz)
                local mag = dt / (distance * distance * distance)
                local bim, bjm = bimass*mag, bj.mass*mag
                bivx = bivx - (dx * bjm)
                bivy = bivy - (dy * bjm)
                bivz = bivz - (dz * bjm)
                bj.vx = bj.vx + (dx * bim)
                bj.vy = bj.vy + (dy * bim)
                bj.vz = bj.vz + (dz * bim)
            end
            bi.vx = bivx
            bi.vy = bivy
            bi.vz = bivz
        end
        for i = 1, nbody do
            local bi = bodies[i]
            bi.x = bi.x + (dt * bi.vx)
            bi.y = bi.y + (dt * bi.vy)
            bi.z = bi.z + (dt * bi.vz)
        end
    end

    local function energy(bodies, nbody)
        local e = 0
        for i = 1, nbody do
            local bi = bodies[i]
            local vx, vy, vz, bim = bi.vx, bi.vy, bi.vz, bi.mass
            e = e + (0.5 * bim * (vx*vx + vy*vy + vz*vz))
            for j = i + 1, nbody do
                local bj = bodies[j]
                local dx, dy, dz = bi.x - bj.x, bi.y - bj.y, bi.z - bj.z
                local distance = math.sqrt(dx*dx + dy*dy + dz*dz)
                e = e - ((bim * bj.mass) / distance)
            end
        end
        return e
    end

    local function offsetMomentum(b, nbody)
        local px, py, pz = 0, 0, 0
        for i = 1, nbody do
            local bi = b[i]
            local bim = bi.mass
            px = px + (bi.vx * bim)
            py = py + (bi.vy * bim)
            pz = pz + (bi.vz * bim)
        end
        b[1].vx = -px / SOLAR_MASS
        b[1].vy = -py / SOLAR_MASS
        b[1].vz = -pz / SOLAR_MASS
    end

    local N = 20000
    local nbody = #bodies

    local ts0 = os.clock()
    offsetMomentum(bodies, nbody)
    for i = 1, N do advance(bodies, nbody, 0.01) end
    local ts1 = os.clock()

    return ts1 - ts0
end

bench.runCode(test, "n-body")