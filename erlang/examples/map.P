-file("src/map.erl", 1).

-module(map).

-export([main/0]).

main() ->
    Map1 = #{1 => $1, "abc" => 0},
    Map2 = Map1#{1 := "1"},
    $1 = maps:get(1, maps:update(1, $1, Map2)),
    1 = maps:size(maps:remove("abc", Map2)),
    ok.



