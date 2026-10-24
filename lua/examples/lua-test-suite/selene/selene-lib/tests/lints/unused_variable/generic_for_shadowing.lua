return function(foo, ...)
    for _, foo in pairs({ foo, ... }) do
        print(foo)
    end
end
