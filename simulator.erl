-module(simulator).
-export([simulator/2, client_node/3, control_node/1]).



% Nodes
nodes_start(0, NumBuddies) -> [];
nodes_start(NumNodes, NumBuddies) -> 
    Node = spawn(simulator, client_node, [1, [], none]),
    nodes_start(NumNodes - 1, NumBuddies) ++ [Node].
  
client_simulation(State, Buddies, Control) ->
    io:format(
    timer:sleep(
    ok.

client_node(State, Buddies, Control) -> 
    receive
        {control, Control2} ->
            client_node(State, Buddies, Control2);
        {buddies, Buddies2} ->
            io:format("~w: ~w~n", [self(), Buddies2]),
            client_node(State, Buddies2, Control); % Update the buddies
        {state, State2} -> 
            client_node(State2, Buddies, Control);
        simulate ->
            client_simulation(State, Buddies, Control)
    end.



% A extremely even distribution of buddies per client.
grab_buddies(Nodes, CurPos, NumBuddies) when CurPos > length(Nodes) -> ok;
grab_buddies(Nodes, CurPos, NumBuddies) when CurPos =< length(Nodes) ->
    Buddies = lists:sublist(Nodes, CurPos + 1, NumBuddies),
    if
        length(Buddies) < NumBuddies ->
            NumBuddiesOffBy = NumBuddies - length(Buddies),
            Buddies2 = Buddies ++ lists:sublist(Nodes, 1, NumBuddiesOffBy),
            Client = lists:nth(CurPos, Nodes),
            Client ! {buddies, Buddies2}, % Assign buddies
            grab_buddies(Nodes, CurPos + 1, NumBuddies);
        true ->
            Client = lists:nth(CurPos, Nodes),
            Client ! {buddies, Buddies}, % Assign buddies
            grab_buddies(Nodes, CurPos + 1, NumBuddies)
    end.





% Server
control_node(Nodes) ->
    receive
        start -> 
            lists:foreach(fun(Node) -> Node ! {control, self()} end, Nodes), 
            control_node(Nodes);
        {assign_buddies, NumBuddies} -> 
            grab_buddies(Nodes, 1, NumBuddies),
            control_node(Nodes);
        {offline, Node} -> [];
        {online, Node} -> []
    end.

    

simulator(NumNodes, NumBuddies) -> 
    Nodes = nodes_start(NumNodes, NumBuddies),
    Control = spawn(simulator, control_node, [Nodes]),
    Control ! start,
    Control ! {assign_buddies, NumBuddies},
    lists:foreach(fun(Client) -> 
                    State = lists:nth(random:uniform(2), [online, offline]), % Start with a random state.
                    io:format("~w~n", [State]),
                    Client ! {state, State}
                  end, Nodes),
    lists:foreach(fun(Client) ->
                    Client ! simulate
                  end, Nodes).

% Done: Start the nodes returning pids for each node which has been started.
% Done: Start the control center for routing info.
% Done: For each of the nodes notify it of the Control.
    % Done: The Node should send the buddies. The Control should keep a dictionary of (buddy) -> clients.
% start Pids of nodes to send/receive messages.

