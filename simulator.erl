-module(simulator).
-export([simulator/2, client_node/2, control_node/1, client_simulation/2]).


get_node(NodeTuple) -> element(1, NodeTuple).
get_state(NodeTuple) -> element(2, NodeTuple).
get_buddies(NodeTuple) -> element(3, NodeTuple).
node_tuple(Client, State, Buddies) -> {Client, State, Buddies}.


% Nodes

% Done.
nodes_create(0, NumBuddies) -> [];
nodes_create(NumNodes, NumBuddies) -> 
    Node = spawn(simulator, client_node, [1, [], none]),
    nodes_create(NumNodes - 1, NumBuddies) ++ [Node].
  

client_node(NodeTuple, Control) -> 
    receive
        {control, Control2} ->
            client_node(NodeTuple, Control2);
        {buddies, Buddies2} ->
            io:format("~w: ~w~n", [self(), Buddies2]),
            client_node(State, Buddies2, Control); % Update the buddies
        {state, State2} -> 
            client_node(State2, Buddies, Control);
        update_buddies ->
            Control ! {pong_update_buddies, Buddies},
            client_node(State, Buddies, Control);
        {send_my_state} ->
            Control ! {update_client_status, self(), Buddies, State}
    end.



% A extremely even distribution of buddies per client.
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



client_simulation(NodeTuple, Control) ->
    Client = element(1, NodeTuple),
    State = element(2, NodeTuple),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3), % Need to do this otherwise same random accross processes.
    SleepTime = (random:uniform(4001) - 1), % Seconds to sleep (0..4000).
    io:format("Started Client ~w ~w ~w~n", [Client, SleepTime, State]),
    timer:sleep(SleepTime),
    case State of
        offline -> Control ! {state_update, Client, online};
        online -> Control ! {state_update, Client, offline}
    end.

% Server
control_node(Nodes) ->
    receive
        {start, NumBuddies} -> 
            io:format("Starting nodes: ~w~n", [Nodes]),
            % Send info about control node.
            lists:foreach(fun(Client) -> Client ! {control, self()} end, Nodes), 
            io:format("Loaded control node~n"),
            % Set the node state
            NodesWithState = lists:map(fun(Client) -> 
                                        {A1,A2,A3} = now(),
                                        random:seed(A1, A2, A3), 
                                        State = lists:nth(random:uniform(2), [online, offline]), % Start with a random state.
                                        io:format("~w~n", [State]),
                                        Client ! {state, State},
                                        {Client, State}
                                      end, Nodes),
            io:format("NodesWIthState: ~w~n", [NodesWithState]),
            % Load the Buddies.
            NodesWithBuddies = grab_buddies(Nodes, 1, NumBuddies),
            io:format("Loaded buddies~n"),
            control_node(NodesWithBuddies);
        simulate -> 
            io:format("NodesWIthState simulate: ~w~n", [Nodes]),
            lists:foreach(fun(Client) -> spawn(simulator, client_simulation, [Client, self()]) end, Nodes),
            control_node(Nodes);
        {state_update, Client, State} -> 
            io:format("Entered state_update"),
            Nodes2 = update_node_state(Nodes, Client, State),
            io:format("Entered state_update: ~w~n"),
            Client ! update_buddies,
            control_node(Nodes2);
        {pong_update_buddies, Buddies} -> ok
    end.

% NodeTuple is {Node, State}
update_node_state(Nodes, Client, State) ->
    lists:map(fun(NodeTuple) -> 
                if 
                    Client == get_client(NodeTuple) -> {Client, State};
                    true -> NodeTuple
                end
              end, Nodes).    

simulator(NumNodes, NumBuddies) -> 
    Nodes = nodes_create(NumNodes, NumBuddies),
    io:format("Length: ~w~n", [length(Nodes)]),
    Control = spawn(simulator, control_node, [Nodes]),
    Control ! {start, NumBuddies},
    Control ! simulate.

% Done: Start the nodes returning pids for each node which has been started.
% Done: Start the control center for routing info.
% Done: For each of the nodes notify it of the Control.
    % Done: The Node should send the buddies. The Control should keep a dictionary of (buddy) -> clients.
% start Pids of nodes to send/receive messages.

