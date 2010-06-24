-module(test).
-export([test/1, blah/0, main/0]).

test(A) ->
    receive
        hi -> A,
            test(A + 1);
        bye -> 
            io:format("bye2 ~w~n", [A])
    end.

blah() ->
    Pid = spawn(test, test, [1]),
    Pid ! hi,
    Pid ! hi,
    Pid ! hi,
    Pid ! hi,
    Pid ! hi,
    Pid ! bye.
    
grab_buddies(Nodes, CurPos, NumBuddies) when CurPos > length(Nodes) -> ok;
grab_buddies(Nodes, CurPos, NumBuddies) when CurPos =< length(Nodes) ->
    Buddies = lists:sublist(Nodes, CurPos + 1, NumBuddies), % At the last node.
    if
        length(Buddies) < NumBuddies ->
            NumBuddiesOffBy = NumBuddies - length(Buddies),
            Buddies2 = Buddies ++ lists:sublist(Nodes, 1, NumBuddiesOffBy),
            Client = lists:nth(CurPos, Nodes),
           % Client ! {buddies, Buddies2}, % Assign buddies
           io:format("~w ~w~n", [Client, Buddies2]),
            grab_buddies(Nodes, CurPos + 1, NumBuddies);
        true ->
            Client = lists:nth(CurPos, Nodes),
           io:format("~w ~w~n", [Client, Buddies]),
            %Client ! {buddies, Buddies}, % Assign buddies
            grab_buddies(Nodes, CurPos + 1, NumBuddies)
    end.

main() ->
    grab_buddies([1,2,3,4,5,6,7,8,9,10], 1, 5).
