local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    function updateTransforms(matrixArray, amount, offset, scale, time)
        local i = 0

        for x=0,amount-1 do
        for y=0,amount-1 do
        for z=0,amount-1 do
            local tx = offset - x
            local ty = offset - y
            local tz = offset - z

            local rx = 0
            local ry = ( math.sin( x / 4 + time ) + math.sin( y / 4 + time ) + math.sin( z / 4 + time ) )
            local rz = ry * 2

            local ch = math.cos(rx)
            local sh = math.sin(rx)
            local ca = math.cos(ry)
            local sa = math.sin(ry)
            local cb = math.cos(rz)
            local sb = math.sin(rz)

            local m00 = ch * ca
            local m01 = sh*sb - ch*sa*cb
            local m02 = ch*sa*sb + sh*cb
            local m10 = sa
            local m11 = ca*cb
            local m12 = -ca*sb
            local m20 = -sh*ca
            local m21 = sh*sa*cb + ch*sb
            local m22 = -sh*sa*sb + ch*cb

            matrixArray[i * 16 + 1] = m00 * scale
            matrixArray[i * 16 + 2] = m01 * scale
            matrixArray[i * 16 + 3] = m02 * scale
            matrixArray[i * 16 + 4] = 0
            matrixArray[i * 16 + 5] = m10 * scale
            matrixArray[i * 16 + 6] = m11 * scale
            matrixArray[i * 16 + 7] = m12 * scale
            matrixArray[i * 16 + 8] = 0
            matrixArray[i * 16 + 9] = m20 * scale
            matrixArray[i * 16 + 10] = m21 * scale
            matrixArray[i * 16 + 11] = m22 * scale
            matrixArray[i * 16 + 12] = 0
            matrixArray[i * 16 + 13] = tx
            matrixArray[i * 16 + 14] = ty
            matrixArray[i * 16 + 15] = tz
            matrixArray[i * 16 + 16] = 1

            i = i + 1
        end
        end
        end
    end

    local N = 40
    local array = table.create(N*N*N*16)

    local ts0 = os.clock()

    updateTransforms(array, N, -N/2, 0.5, 1/60)

    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "trig")