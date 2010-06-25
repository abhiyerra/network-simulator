-module(simulator).
-export([start/2, client_node/2, server_node/1, client_handler/1]).


% Data Structure

get_node(NodeTuple) -> element(1, NodeTuple).
get_state(NodeTuple) -> element(2, NodeTuple).
get_buddies(NodeTuple) -> element(3, NodeTuple).
node_tuple(Client, State, Buddies) -> {Client, State, Buddies}.


% A extremely even distribution of buddies per client.
grab_buddies(Nodes, CurPos, _) when CurPos > length(Nodes) -> [];
grab_buddies(Nodes, CurPos, NumBuddies) when CurPos =< length(Nodes) ->
    Buddies = lists:map(fun(X) -> {get_node(X), get_state(X)} end, lists:sublist(Nodes, CurPos + 1, NumBuddies)),
    if
        length(Buddies) < NumBuddies ->
            NumBuddiesOffBy = NumBuddies - length(Buddies),
            Buddies2 = Buddies ++ lists:map(fun(X) -> {get_node(X), get_state(X)} end, lists:sublist(Nodes, 1, NumBuddiesOffBy)),
            NodeTuple = lists:nth(CurPos, Nodes),
            [node_tuple(get_node(NodeTuple), get_state(NodeTuple), Buddies2)] ++ grab_buddies(Nodes, CurPos + 1, NumBuddies);
        true ->
            NodeTuple = lists:nth(CurPos, Nodes),
            [node_tuple(get_node(NodeTuple), get_state(NodeTuple), Buddies)] ++ grab_buddies(Nodes, CurPos + 1, NumBuddies)
    end.

nodes_create(Control, 0, NumBuddies) -> [];
nodes_create(Control, NumNodes, NumBuddies) -> 
    Node = spawn(simulator, client_node, [Control, none]),
    nodes_create(Control, NumNodes - 1, NumBuddies) ++ [node_tuple(Node, none, none)].





client_handler(Node) ->
%    io:format("Node ~w~n", [Node]),

    % Sleep for a random amount of minutes
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3), 
    SleepTime = (random:uniform(4001) - 1), % Seconds to sleep (0..4000).
 %   io:format("Sleep ~w ~w~n", [Node, SleepTime]),
    timer:sleep(SleepTime),

    Node ! update_state,
    Node ! print_state,

    client_handler(Node).



client_node(Control, NodeTuple) ->
    receive
        start ->
 %           io:format("~w~n", [NodeTuple]),
            client_node(Control, NodeTuple);
        {set_node_tuple, NodeTuple2} ->
            client_node(Control, NodeTuple2);
        update_state -> 
  %          io:format("update_state ~w~n", [NodeTuple]),
            CurrentState = get_state(NodeTuple),

            NodeTuple2 = case CurrentState of
                online -> % Make it offline
                    node_tuple(get_node(NodeTuple), offline, get_buddies(NodeTuple));
                offline -> % Make it online
                    node_tuple(get_node(NodeTuple), online, get_buddies(NodeTuple))
            end,
            Control ! {update_state, NodeTuple2},
            client_node(Control, NodeTuple2);
        {update_state, State} -> 
            NodeTuple2 = node_tuple(get_node(NodeTuple), State, get_buddies(NodeTuple)),
            Control ! {update_state, NodeTuple2},
            client_node(Control, NodeTuple2);
        {update_buddy_state, Buddy, State} ->
            Buddies = get_buddies(NodeTuple),
            Buddies2 = lists:map(fun(X) ->
                                    BuddyX = element(1, X),
                                    if 
                                        BuddyX == Buddy -> {Buddy, State};
                                        true -> X
                                    end
                                  end, Buddies),
            NodeTuple2 = node_tuple(get_node(NodeTuple), get_state(NodeTuple), Buddies2),
            client_node(Control, NodeTuple2);
        _ ->
            io:format("me ~w ~w~n", [Control, NodeTuple]),
            client_node(Control, NodeTuple)
            
    end.



server_node(Clients) ->
    receive
        {clients, Clients2} ->
            server_node(Clients2);
        simulate ->
  %          io:format("Size of Clients ~w~n~n", [Clients]),
            lists:foreach(fun(X) -> Node = get_node(X), Pid = spawn(simulator, client_handler, [Node]) end, Clients),
            server_node(Clients);
        {update_state, NodeTuple} ->
            % Update the buddy state
            Node = get_node(NodeTuple),
            State = get_state(NodeTuple),
            Buddies = get_buddies(NodeTuple),
            lists:foreach(fun(X) ->
                            BuddyX = element(1, X),
                            BuddyX ! {update_buddy_state, Node, State}
                          end, Buddies),

            % Update the clients list
            Clients2 = lists:map(fun(X) ->
                                    ClientX = get_node(X),
                                    if
                                        ClientX == Node -> NodeTuple;
                                        true -> X
                                    end
                                  end, Clients),
           io:format("~w~n", [Clients2]),
           server_node(Clients2)
    end.



% main
start(NumNodes, NumBuddies) -> 
    Control = spawn(simulator, server_node, [none]),
    Nodes = nodes_create(Control, NumNodes, NumBuddies),

    % Add Control to the Client Node.
    io:format("Loaded control node with: ~w~n", [Nodes]),

    % Set the node state
    NodesWithState = lists:map(fun(X) -> 
                                Client = get_node(X),
                                {A1,A2,A3} = now(),
                                random:seed(A1, A2, A3), 
                                State = lists:nth(random:uniform(2), [online, offline]), % Start with a random state.
                                %io:format("~w~n", [State]),
                                node_tuple(Client, State, none)
                              end, Nodes),
   % io:format("NodesWithState: ~w~n", [NodesWithState]),

    % Set the Buddies.
    NodesWithBuddies = grab_buddies(NodesWithState, 1, NumBuddies),
    io:format("~w~n", [NodesWithBuddies]),

    lists:foreach(fun(X) -> Node = get_node(X), Node ! {set_node_tuple, X} end, NodesWithBuddies),
    Control ! {clients, NodesWithBuddies},
    Control ! simulate.



%man_me(A) ->
%    io:format("~w~n", [A]),
%    timer:sleep(1000),
%    man_me(A).
%
%
%main() ->
%  lists:foreach(fun(X) -> Pid = spawn(test2, man_me, [X]) end, [1,2,3]).


  %  control_node(NodesWithBuddies).



%    Control ! {populate, NumBuddies},
 %   Control ! simulate.




















