-module(actors).
-import(math, [sqrt/1, pow/2]).
-import(crypto, [hash/2]).
-import(rng, [rnd_number/1]).
-import(utils, [writeKV/2, mod/2]).

-export([init/3, gossipWorker/3]).

rumorLimit() ->
    10.

gossipWorker(SupervisorPid, Neighbors, RumorsHeard) ->
    receive
        {health_check} ->
            io:format("Worker is up! ~p~n", [self()]),
            gossipWorker(SupervisorPid, Neighbors, RumorsHeard);
        {setup, SetupNeighbors} ->
            io:format("Setting up the neighbours of worker ~p~n", [self()]),
            gossipWorker(SupervisorPid, SetupNeighbors, RumorsHeard);
        {share_gossip, Rumor} ->
            RumorLimit = rumorLimit() + 1,
            if
                RumorsHeard < RumorLimit ->
                    LenNeighbors = length(Neighbors),
                    NeighborPicked = lists:nth(rand:uniform(LenNeighbors), Neighbors),
                    io:format("~p sharing rumor with neighbor: ~p~n", [self(), NeighborPicked]),
                    NeighborPicked ! {share_gossip, Rumor},
                    gossipWorker(SupervisorPid, Neighbors, RumorsHeard + 1);
                true ->
                    io:format("Node ~p has heard rumor ~p times.~n", [self(), RumorsHeard]),
                    SupervisorPid ! {terminated, self()},
                    false
            end
    end.

findSupervisorPid(SupervisorName) ->
    global:send(SupervisorName, {health_check, self()}),
    R =
        receive
            {health_response, M, Pid} ->
                io:format("~p~n", [M]),
                [ok, M, Pid]
        after 5000 -> {error, "no answer!"}
        end,
    lists:nth(3, R).

spawnMultipleWorkers(SupervisorPid, NumberOfActorsToSpawn, Algorithm) ->
    spawnMultipleWorkers(SupervisorPid, NumberOfActorsToSpawn, Algorithm, []).
spawnMultipleWorkers(_, 0, _, ListOfPid) ->
    ListOfPid;
spawnMultipleWorkers(SupervisorPid, NumberOfActorsToSpawn, Algorithm, ListOfPid) ->
    spawnMultipleWorkers(SupervisorPid, NumberOfActorsToSpawn - 1, Algorithm, [
        spawn(actors, gossipWorker, [SupervisorPid, [], 0]) | ListOfPid
    ]).

findNeighboursLine(Map, N, I, _) when I > N ->
    Map;
findNeighboursLine(Map, N, I, Pids) ->
    if
        I == 1 ->
            Neighbors = [lists:nth(I + 1, Pids)],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursLine(NewMap, N, I + 1, Pids);
        I == N ->
            Neighbors = [lists:nth(I - 1, Pids)],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursLine(NewMap, N, I + 1, Pids);
        true ->
            Neighbors = [lists:nth(I - 1, Pids), lists:nth(I + 1, Pids)],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursLine(NewMap, N, I + 1, Pids)
    end.

findNeighboursGrid(Map, _, N, I, Pids) when I > N ->
    Map;
findNeighboursGrid(Map, Side, N, I, Pids) ->
    RowNum = ceil(I / Side),
    ElNum = mod(I, Side),
    if
        ElNum == 0 ->
            ColNum = Side;
        true ->
            ColNum = ElNum
    end,
    % Not considering diagonals.
    % TODO: Consider cases when lower neighbor doesn't exist as N < Side * Side.
    if
        I == 1 ->
            Neighbors = [lists:nth(I + 1, Pids), lists:nth(I + Side, Pids)],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursGrid(NewMap, Side, N, I + 1, Pids);
        % perfect square or end of list - previous neighbor and upper neighbor.
        (I == Side * Side) or (I == N) ->
            Neighbors = [lists:nth(I - 1, Pids), lists:nth(I - Side, Pids)],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursGrid(NewMap, Side, N, I + 1, Pids);
        (ColNum == Side) and (RowNum == 1) ->
            Neighbors = [
                lists:nth(I - 1, Pids), lists:nth(I + Side, Pids)
            ],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursGrid(NewMap, Side, N, I + 1, Pids);
        % rightmost column - upper, previous and lower are neighbors.
        (ColNum == Side) and (RowNum > 1) ->
            Neighbors = [
                lists:nth(I - Side, Pids), lists:nth(I - 1, Pids), lists:nth(I + Side, Pids)
            ],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursGrid(NewMap, Side, N, I + 1, Pids);
        % bottom row - previous, upper and next are neighbors.
        RowNum == Side ->
            Neighbors = [lists:nth(I - 1, Pids), lists:nth(I - Side, Pids), lists:nth(I + 1, Pids)],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursGrid(NewMap, Side, N, I + 1, Pids);
        % top row - previous, lower and next are neighbors.
        RowNum == 1 ->
            Neighbors = [lists:nth(I - 1, Pids), lists:nth(I + Side, Pids), lists:nth(I + 1, Pids)],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursGrid(NewMap, Side, N, I + 1, Pids);
        % leftmost column - upper, next and lower are neighbors.
        ColNum == 1 ->
            Neighbors = [
                lists:nth(I - Side + 1, Pids), lists:nth(I + 1, Pids), lists:nth(I + Side, Pids)
            ],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursGrid(NewMap, Side, N, I + 1, Pids);
        % middle elements - previous, upper, next, and lower are neighbors.
        true ->
            Neighbors = [
                lists:nth(I - 1, Pids),
                lists:nth(I - Side, Pids),
                lists:nth(I + 1, Pids),
                lists:nth(I + Side, Pids)
            ],
            NewMap = maps:put(I, Neighbors, Map),
            findNeighboursGrid(NewMap, Side, N, I + 1, Pids)
    end.
% TODO: Consider an alternative approach. Loop over the four neighbors of every cell,
% and only append those which are valid. Combine the map created using this function
% with all others to build the overall neighbor map.
basicCheck(N, NeighborI) ->
    if
        (NeighborI > 0) and (NeighborI < N) ->
            true;
        true ->
            false
    end.

checkIfValidNeighbor(Side, N, I, NeighborI) ->
    ElNum = mod(I, Side),
    if
        ElNum == 0 ->
            ColNum = Side;
        true ->
            ColNum = ElNum
    end,
    IsBasicValid = basicCheck(N, NeighborI),
    if
        IsBasicValid == false ->
            false;
        true ->
            if
                (ColNum == 0) and (I - NeighborI == 1) ->
                    false;
                NeighborI + Side > N ->
                    false;
                (ColNum == Side) and (NeighborI - I == 1) ->
                    false;
                true ->
                    true
            end
    end.

findNeighborsOf3DCell(ListOfNeighbors, _, _, _, 0, _) ->
    ListOfNeighbors;
findNeighborsOf3DCell(ListOfNeighbors, Side, N, I, NumNeighborsToCheck, Pids) ->
    if
        NumNeighborsToCheck == 1 ->
            Neighbor = I - 1;
        NumNeighborsToCheck == 2 ->
            Neighbor = I + 1;
        NumNeighborsToCheck == 3 ->
            Neighbor = I - Side;
        NumNeighborsToCheck == 4 ->
            Neighbor = I + Side;
        NumNeighborsToCheck == 5 ->
            Neighbor = I - (Side * Side);
        NumNeighborsToCheck == 6 ->
            Neighbor = I + (Side * Side);
        true ->
            Neighbor = I - 1
    end,
    IsValidNeigh = checkIfValidNeighbor(Side, N, I, Neighbor),
    if
        IsValidNeigh == true ->
            NewListOfNeighbors = [lists:nth(Neighbor, Pids) | ListOfNeighbors];
        true ->
            NewListOfNeighbors = ListOfNeighbors
    end,
    findNeighborsOf3DCell(NewListOfNeighbors, Side, N, I, NumNeighborsToCheck - 1, Pids).

% findNeighborsOf3DCell(ListOfNeighbors, Side, N, I, NumNeighborsToCheck, Pids) ->
%     if
%         NumNeighborsToCheck == 1 ->
%             Neighbor = I - 1,
%             IsValidNeigh = checkIfValidNeighbor(Side, N, I, Neighbor),
%             if
%                 IsValidNeigh == true ->
%                     NewListOfNeighbors = [lists:nth(Neighbor, Pids) | ListOfNeighbors];
%                 true ->
%                     NewListOfNeighbors = ListOfNeighbors
%             end,
%             findNeighborsOf3DCell(NewListOfNeighbors, Side, N, I, NumNeighborsToCheck - 1, Pids);
%         NumNeighborsToCheck == 2 ->
%             Neighbor = I + 1,
%             IsValidNeigh = checkIfValidNeighbor(Side, N, I, Neighbor),
%             if
%                 IsValidNeigh == true ->
%                     NewListOfNeighbors = [lists:nth(Neighbor, Pids) | ListOfNeighbors];
%                 true ->
%                     NewListOfNeighbors = ListOfNeighbors
%             end,
%             findNeighborsOf3DCell(NewListOfNeighbors, Side, N, I, NumNeighborsToCheck - 1, Pids);
%         NumNeighborsToCheck == 3 ->
%             Neighbor = I - Side,
%             IsValidNeigh = checkIfValidNeighbor(Side, N, I, Neighbor),
%             if
%                 IsValidNeigh == true ->
%                     NewListOfNeighbors = [lists:nth(Neighbor, Pids) | ListOfNeighbors];
%                 true ->
%                     NewListOfNeighbors = ListOfNeighbors
%             end,
%             findNeighborsOf3DCell(NewListOfNeighbors, Side, N, I, NumNeighborsToCheck - 1, Pids);
%         NumNeighborsToCheck == 4 ->
%             Neighbor = I + Side,
%             IsValidNeigh = checkIfValidNeighbor(Side, N, I, Neighbor),
%             if
%                 IsValidNeigh == true ->
%                     NewListOfNeighbors = [lists:nth(Neighbor, Pids) | ListOfNeighbors];
%                 true ->
%                     NewListOfNeighbors = ListOfNeighbors
%             end,
%             findNeighborsOf3DCell(NewListOfNeighbors, Side, N, I, NumNeighborsToCheck - 1, Pids);
%         NumNeighborsToCheck == 5 ->
%             Neighbor = I - (Side * Side),
%             IsValidNeigh = checkIfValidNeighbor(Side, N, I, Neighbor),
%             if
%                 IsValidNeigh == true ->
%                     NewListOfNeighbors = [lists:nth(Neighbor, Pids) | ListOfNeighbors];
%                 true ->
%                     NewListOfNeighbors = ListOfNeighbors
%             end,
%             findNeighborsOf3DCell(NewListOfNeighbors, Side, N, I, NumNeighborsToCheck - 1, Pids);
%         NumNeighborsToCheck == 6 ->
%             Neighbor = I + (Side * Side),
%             IsValidNeigh = checkIfValidNeighbor(Side, N, I, Neighbor),
%             if
%                 IsValidNeigh == true ->
%                     NewListOfNeighbors = [lists:nth(Neighbor, Pids) | ListOfNeighbors];
%                 true ->
%                     NewListOfNeighbors = ListOfNeighbors
%             end,
%             findNeighborsOf3DCell(NewListOfNeighbors, Side, N, I, NumNeighborsToCheck - 1, Pids);
%         true ->  % middle elements - previous, upper, next, and lower are neighbors.
%             findNeighborsOf3DCell(ListOfNeighbors, Side, N, I, NumNeighborsToCheck - 1, Pids)
%     end.

findNeighbours3D(Map, _, N, I, _) when I > N ->
    Map;
findNeighbours3D(Map, Side, N, I, Pids) ->
    % Not considering diagonals.
    Neighbors = findNeighborsOf3DCell([], Side, N, I, 6, Pids),
    NewMap = maps:put(I, Neighbors, Map),
    findNeighbours3D(NewMap, Side, N, I + 1, Pids).

setupWorkers(_, _, N, I) when I > N ->
    ok;
setupWorkers(WorkerPids, NeighboursMap, N, I) ->
    Neighbors = maps:get(I, NeighboursMap),
    Pid = lists:nth(I, WorkerPids),
    Pid ! {setup, Neighbors},
    setupWorkers(WorkerPids, NeighboursMap, N, I + 1).

setupTopology(Topology, Dims, NumNodes, WorkerPids) ->
    if
        Topology == "Line" ->
            io:format("Setting up workers for Line topology~n"),
            NeighboursMap = findNeighboursLine(maps:new(), NumNodes, 1, WorkerPids),
            io:format("Topology neighbors: ~p~n", [NeighboursMap]),
            setupWorkers(WorkerPids, NeighboursMap, NumNodes, 1);
        Topology == "2D" -> % not working atm
            io:format("Setting up workers for 2D topology~n"),
            NeighboursMap = findNeighboursGrid(
                maps:new(), lists:nth(2, Dims), NumNodes, 1, WorkerPids
            ),
            io:format("Topology neighbors: ~p~n", [NeighboursMap]),
            setupWorkers(WorkerPids, NeighboursMap, NumNodes, 1);
        (Topology == "3D") or (Topology == "Imp3D") ->
            io:format("Setting up workers for 3D topology~n"),
            NeighboursMap = findNeighbours3D(
                maps:new(), lists:nth(2, Dims), NumNodes, 1, WorkerPids
            ),
            io:format("Topology neighbors: ~p~n", [NeighboursMap]),
            setupWorkers(WorkerPids, NeighboursMap, NumNodes, 1);
        true ->
            false
    end.

determineTopologyDims(Topology, NumNodes) ->
    if
        Topology == "2D" ->
            Side = ceil(sqrt(NumNodes)),
            [-1, Side, Side];
        Topology == "3D" ->
            Side = ceil(pow(NumNodes, 0.333)),
            [-1, Side, Side, Side];
        Topology == "Imp3D" ->
            Side = ceil(pow(NumNodes, 0.333)),
            [-1, Side, Side, Side];
        Topology == "Line" ->
            [-1, NumNodes];
        Topology == "Full" ->
            [-1, NumNodes];
        true ->
            false
    end.

init(NumNodes, Topology, Algorithm) ->
    SupervisorPid = findSupervisorPid(server),
    io:format("~p~n", [SupervisorPid]),

    % NumberOfActorsToSpawn = 10,
    WorkerPids = spawnMultipleWorkers(SupervisorPid, NumNodes, Algorithm),
    io:format("~p\n", [WorkerPids]),
    Dims = determineTopologyDims(Topology, NumNodes),
    setupTopology(Topology, Dims, NumNodes, WorkerPids),
    ok.
