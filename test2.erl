-module(test2).
-export([main/0, man_me/1]).


man_me(A) ->
    io:format("~w~n", [A]),
    timer:sleep(1000),
    man_me(A).


main() ->
  lists:foreach(fun(X) -> Pid = spawn(test2, man_me, [X]) end, [1,2,3]).
