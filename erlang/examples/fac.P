-file("src/fac.erl", 1).

-module(fac).

-export([fac/1]).

-spec fac(integer()) -> integer().

fac(Num) ->
    F = fun F(0) ->
                1;
            F(N) when N >= 0 ->
                N * F(N - 1)
        end,
    F(Num).



