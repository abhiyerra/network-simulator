% Simulator by Abhi Yerra <abhi@berkeley.edu>

-module(simulator).
-export([start/4, client_node/4, server_node/5, simulated_node/1, client_handler/1, simulation_timer/2]).


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

nodes_create(Simulated, Control, 0, NumBuddies) -> [];
nodes_create(Simulated, Control, NumNodes, NumBuddies) -> 
    TimeDict = dict:new(),
    Node = spawn(simulator, client_node, [Simulated, Control, none, TimeDict]),
    nodes_create(Simulated, Control, NumNodes - 1, NumBuddies) ++ [node_tuple(Node, none, none)].





client_handler(Node) ->
%    io:format("Node ~w~n", [Node]),

    % Sleep for a random amount of minutes
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3), 
    SleepTime = (random:uniform(4001) - 1) * 1000, % Seconds to sleep (0..4000).
 %   io:format("Sleep ~w ~w~n", [Node, SleepTime]),
    timer:sleep(SleepTime),

    StartTime = now(),
    Node ! {update_state, StartTime},
    %Node ! print_state,

    client_handler(Node).



client_node(Simulated, Control, NodeTuple, TimeDict) ->
    receive
        start ->
 %           io:format("~w~n", [NodeTuple]),
            client_node(Simulated, Control, NodeTuple, TimeDict);
        {set_node_tuple, NodeTuple2} ->
            client_node(Simulated, Control, NodeTuple2, TimeDict);
        {update_state, StartTime} -> 
            %io:format("update_state ~w ~w~n ", [NodeTuple, dict:to_list(TimeDict)]),
            CurrentState = get_state(NodeTuple),
            NodeTuple2 = case CurrentState of
                online -> % Make it offline
                    node_tuple(get_node(NodeTuple), offline, get_buddies(NodeTuple));
                offline -> % Make it online
                    node_tuple(get_node(NodeTuple), online, get_buddies(NodeTuple))
            end,

            % TODO: Check if sent more than 5 time this min.
            {Hour,Min,_} = erlang:time(),
            T = {Hour,Min},
            TimeDict2 = dict:update_counter(T, 1, TimeDict),
            {ok, NumReqs} = dict:find(T, TimeDict2),

            if
                % Only notify 5 times per minute. Don't want to handle erlang error so adding + 1.
                NumReqs =< (5 + 1) -> Control ! {update_state, NodeTuple2, StartTime};
                true -> ok
            end,
            client_node(Simulated, Control, NodeTuple2, TimeDict2);
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
            client_node(Simulated, Control, NodeTuple2, TimeDict);
        _ ->
            io:format("me ~w ~w~n", [Control, NodeTuple]),
            client_node(Simulated, Control, NodeTuple, TimeDict)
    end.


get_buddies_from_client([First|Clients], Buddy) ->
    BuddyPid = element(1,First),
    BuddyState = element(2,First),
    if
        BuddyPid == Buddy -> {BuddyPid, BuddyState};
        true -> get_buddies_from_client(Clients, Buddy)
    end.



server_node(Simulated, Clients, PacketLoss, MessagesSent, MessagesFailed) ->
    receive
        {clients, Clients2} ->
            server_node(Simulated, Clients2, PacketLoss, MessagesSent, MessagesFailed);
        simulate ->
  %          io:format("Size of Clients ~w~n~n", [Clients]),
            lists:foreach(fun(X) -> Node = get_node(X), Pid = spawn(simulator, client_handler, [Node]) end, Clients),
            server_node(Simulated, Clients, PacketLoss, MessagesSent, MessagesFailed);
        {update_state, NodeTuple, StartTime} ->
            % Update the buddy state
            Node = get_node(NodeTuple),
            State = get_state(NodeTuple),

            Buddies0 = lists:map(fun(X) -> element(1, X) end, get_buddies(NodeTuple)), % Change buddies here.
            Buddies = lists:map(fun(X) -> get_buddies_from_client(Clients, X) end, Buddies0),

            NodeTuple2 = node_tuple(Node, State, Buddies),

            % Ignore for the first 100 messages simulate failure after that.
            Fail = (MessagesSent > 100) and (element(1, PacketLoss) / element(2, PacketLoss) > MessagesFailed / MessagesSent),
        %    io:format("0) ~w ~w ~w ~n", [element(1, PacketLoss) / element(2, PacketLoss), MessagesFailed / MessagesSent, Fail]),

            lists:foreach(fun(X) ->
                            BuddyX = element(1, X),
                            BuddyXState = element(2, X),
                            if % Only want to notify if the Buddy is online.
                                BuddyXState == online -> 
                                 case Fail of
                                     true -> BuddyX ! {update_buddy_state, Node, State};
                                     false -> ok
                                 end;
                                true -> ok
                            end
                          end, Buddies),


            % Update the clients list
            Clients2 = lists:map(fun(X) ->
                                    ClientX = get_node(X),
                                    if
                                        ClientX == Node -> NodeTuple2;
                                        true -> X
                                    end
                                  end, Clients),

            EndTime = now(),
            DiffTime = element(2, EndTime) - element(2, StartTime),
    %        io:format("~w~n", [DiffTime]),
            Simulated ! {add, DiffTime},

            %io:format("1) ~w ~w ~w ~w~n", [Fail, MessagesSent, MessagesFailed, Clients2]),
            case Fail of
                true -> server_node(Simulated, Clients2, PacketLoss, MessagesSent, MessagesFailed + 1);
                false -> server_node(Simulated, Clients2, PacketLoss, MessagesSent + 1, MessagesFailed)
            end
    end.

get_message_per_minute([], D) -> dict:to_list(D);
get_message_per_minute([First|Rest], D) -> 
    D2 = dict:update_counter(First, 1, D),
    get_message_per_minute(Rest, D2).
    
    

simulated_node(MessageTimes) ->
    receive
        {add, Duration} -> 
            Time = erlang:time(),
    %        io:format("Message ~w ~w ~w~n", [Time, Duration, length(MessageTimes)]),
            simulated_node(MessageTimes ++ [{Time, Duration}]);
        report -> 
            AverageMessageTime = lists:sum(lists:map(fun(X) -> element(2, X) end, MessageTimes)) / length(MessageTimes),
            io:format("Average Message Send Time: ~w~n", [AverageMessageTime]),

            MessagesPerMinuteTimes = lists:map(fun(X) -> 
                                                {Hour, Minute, _} = element(1, X), 
                                                {Hour, Minute} 
                                              end, MessageTimes),
            D = dict:new(),
            MessagePerMinute = get_message_per_minute(MessagesPerMinuteTimes, D),
            io:format("Messages per Minute : ~w~n", [MessagePerMinute]),
            timer:sleep(10),
            erlang:halt()
    end.

simulation_timer(Simulated, 0) -> Simulated ! report;
simulation_timer(Simulated, RunFor) ->
    timer:sleep(1000),
    simulation_timer(Simulated, RunFor - 1).


% start - RunFor should be in seconds, FailPercent should be a tuple ex. {5,100} for 5%.
start(NumNodes, NumBuddies, RunFor, FailPercent) -> 
    Simulated = spawn(simulator, simulated_node, [[]]),

    Control = spawn(simulator, server_node, [Simulated, none, FailPercent, 1, 1]),
    Nodes = nodes_create(Simulated, Control, NumNodes, NumBuddies),

    % Add Control to the Client Node.
    %io:format("Loaded control node with: ~w~n", [Nodes]),

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
    %io:format("~w~n", [NodesWithBuddies]),

    % Start the simulation runner now.
    SimulationTimerPid = spawn(simulator, simulation_timer, [Simulated, RunFor]),

    lists:foreach(fun(X) -> Node = get_node(X), Node ! {set_node_tuple, X} end, NodesWithBuddies),
    Control ! {clients, NodesWithBuddies},
    Control ! simulate.
