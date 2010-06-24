-module(test).
-export([test/1, blah/0, main/0, main2/0]).

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
    
get_node(NodeTuple) -> element(1, NodeTuple).
get_state(NodeTuple) -> element(2, NodeTuple).
get_buddies(NodeTuple) -> element(3, NodeTuple).
node_tuple(Client, State, Buddies) -> {Client, State, Buddies}.


grab_buddies(Nodes, CurPos, _) when CurPos > length(Nodes) -> [];
grab_buddies(Nodes, CurPos, NumBuddies) when CurPos =< length(Nodes) ->
    Buddies = lists:map(fun(X) -> get_node(X) end, lists:sublist(Nodes, CurPos + 1, NumBuddies)),
    if
        length(Buddies) < NumBuddies ->
            NumBuddiesOffBy = NumBuddies - length(Buddies),
            Buddies2 = Buddies ++ lists:map(fun(X) -> get_node(X) end, lists:sublist(Nodes, 1, NumBuddiesOffBy)),
            NodeTuple = lists:nth(CurPos, Nodes),
            [node_tuple(get_node(NodeTuple), get_state(NodeTuple), Buddies2)] ++ grab_buddies(Nodes, CurPos + 1, NumBuddies);
        true ->
            NodeTuple = lists:nth(CurPos, Nodes),
            [node_tuple(get_node(NodeTuple), get_state(NodeTuple), Buddies)] ++ grab_buddies(Nodes, CurPos + 1, NumBuddies)
    end.

main() ->
    grab_buddies([{1,on},{2,on},{3,on},{4, on},{5, on},{6,on},{7,on},{8,on},{9,on},{10,on}], 1, 5).


update_node_state(Nodes, Client, State) ->
    lists:map(fun(NodeTuple) -> 
                if 
                    Client == element(1, NodeTuple) -> {Client, State};
                    true -> NodeTuple
                end
              end, Nodes).

main2() ->
    update_node_state([{a,off},{b,on},{c,on}], b, off).
