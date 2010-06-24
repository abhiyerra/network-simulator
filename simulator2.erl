-module(simulator2).
-export([simulator/2, client_node/2, control_node/1, client_simulation/2]).


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


nodes_create(0, NumBuddies) -> [];
nodes_create(NumNodes, NumBuddies) -> 
    Node = spawn(simulator2, client_node, [none, none]),
    nodes_create(NumNodes - 1, NumBuddies) ++ [node_tuple(Node, none, none)].












% Client Node
client_node(NodeTuple, Control) -> 
    receive
        {control, Control2} ->
            io:format("Adding control to client ~w ~w~n", [NodeTuple, Control2]),
            client_node(NodeTuple, Control2);
        {state_update, NodeTuple2} ->
            Control ! {notify_buddies, NodeTuple2},
            client_node(NodeTuple2, Control);
        {update_buddy_status, BuddyNodeTuple} ->
            Buddy = get_node(BuddyNodeTuple),
            BuddyState = get_state(BuddyNodeTuple),
            lists:map(fun(X) -> 
                        if
                            element(1, X) == Buddy -> {Buddy, BuddyState};
                            true -> X
                        end
                      end, get_buddies(NodeTuple))
    end.





% Server Node
control_node(Nodes) ->
    receive
        {populate, NumBuddies} -> 
            % Add Control to the Client Node.
            io:format("Loaded control node with: ~w~n", [Nodes]),
            lists:foreach(fun(X) -> get_node(X) ! {control, self()} end, Nodes),

            % Set the node state
            NodesWithState = lists:map(fun(X) -> 
                                        Client = get_node(X),
                                        {A1,A2,A3} = now(),
                                        random:seed(A1, A2, A3), 
                                        State = lists:nth(random:uniform(2), [online, offline]), % Start with a random state.
                                        %io:format("~w~n", [State]),
                                        node_tuple(Client, State, none)
                                      end, Nodes),
%            io:format("NodesWithState: ~w~n", [NodesWithState]),

            % Set the Buddies.
            NodesWithBuddies = grab_buddies(NodesWithState, 1, NumBuddies),
            io:format("NodesWithBuddies: ~w~n", [NodesWithBuddies]),

            control_node(NodesWithBuddies);

        simulate -> 
            io:format("NodesWithState simulate: ~w~n", [Nodes]),
            lists:foreach(fun(NodeTuple) -> spawn(simulator2, client_simulation, [NodeTuple, self()]) end, Nodes),
            control_node(Nodes);
        {state_update, NodeTuple, State} -> 
            io:format("Entered state_update~n"),
            NodeTuple2 = node_tuple(get_node(NodeTuple), State, get_buddies(NodeTuple)),
            get_node(NodeTuple2) ! {state_update, NodeTuple2},
            Nodes2 = lists:map(fun(X) -> 
                                    Node = get_node(NodeTuple2),
                                    NodeTupleX = get_node(X),
                                    if
                                        Node == NodeTupleX -> NodeTuple2;
                                        true -> X
                                    end
                               end, Nodes),
            io:format("Updated state_change ~w~n", [Nodes2]),
            control_node(Nodes2);
        {notify_buddies, NodeTuple} ->
            Buddies = get_buddies(NodeTuple),
            State = get_state(NodeTuple),
            lists:foreach(fun(X) -> 
                            Buddy = element(1, X),
                            Buddy ! {update_buddy_status, NodeTuple} % Send each of the buddies a status of the new update.
                                                                     % TODO: Can't send if offline
                    end, Buddies),
            control_node(Nodes)
    end.


%            Nodes2 = update_node_state(Nodes, Client, State),
%            io:format("Entered state_update: ~w~n"),
%            Client ! update_buddies,
%        {pong_update_buddies, Buddies} -> ok

client_simulation(NodeTuple, Control) ->
    Node = get_node(NodeTuple),
    State = get_state(NodeTuple),
    Buddies = get_buddies(NodeTuple),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3), 
    SleepTime = (random:uniform(4001) - 1), % Seconds to sleep (0..4000).
    io:format("Started Client ~w ~w ~w~n", [NodeTuple, SleepTime, State]),
    timer:sleep(SleepTime),
    io:format("Client Arose ~w ~w~n", [NodeTuple, SleepTime]),
    case State of
        offline -> 
            io:format("Client online ~w ~w~n", [NodeTuple, SleepTime]),
            Control ! {state_update, NodeTuple, online},
            client_simulation(node_tuple(Node, online, Buddies), Control);
        online -> 
            io:format("Client offline ~w ~w~n", [NodeTuple, SleepTime]),
            Control ! {state_update, NodeTuple, offline}, % Go offline
            client_simulation(node_tuple(Node, offline, Buddies), Control)
    end.










% main
simulator(NumNodes, NumBuddies) -> 
    EmptyNodes = nodes_create(NumNodes, NumBuddies),
    Control = spawn(simulator2, control_node, [EmptyNodes]),
    Control ! {populate, NumBuddies},
    Control ! simulate,
    io:format("Length: ~w~n", [length(EmptyNodes)]).























% Server

% NodeTuple is {Node, State}
%update_node_state(Nodes, Client, State) ->
%    lists:map(fun(NodeTuple) -> 
%                if 
%                    Client == get_client(NodeTuple) -> {Client, State};
%                    true -> NodeTuple
%                end
%              end, Nodes).    
%

