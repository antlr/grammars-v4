local Comparator = functiontype<T>(T, T): boolean

local function mysort<A>(arr: {A}, cmp: Comparator<A>)
   -- ...
end