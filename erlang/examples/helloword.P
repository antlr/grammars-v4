-file("src/helloword.erl", 1).

-module(hello).

-export([hello_world/0]).

hello_world() ->
    io:fwrite("hello, world\n").



