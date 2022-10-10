-module(actors).
-import(math, [sqrt/1, pow/2]).
-import(crypto, [hash/2]).
-import(rng, [rnd_number/1]).
-import(utils, [writeKV/2]).

-export([init/3, gossipWorker/2]).

rumorLimit() ->
    10.

gossipWorker(Neighbors, RumorsHeard) ->
    receive
        {health_check} ->
            io:format("Worker is up! ~p~n", [self()]),
            gossipWorker(Neighbors, RumorsHeard);
        {setup, SetupNeighbors} ->
            io:format("Setting up the neighbours of worker ~p~n", [self()]),
            gossipWorker(SetupNeighbors, RumorsHeard);
        {share_gossip, Rumor} ->
            RumorLimit = rumorLimit() + 1,
            if
                RumorsHeard < RumorLimit ->
                    LenNeighbors = length(Neighbors),
                    NeighborPicked = lists:nth(rand:uniform(LenNeighbors), Neighbors),
                    io:format("~p sharing rumor with neighbor: ~p~n", [self(), NeighborPicked]),
                    NeighborPicked ! {share_gossip, Rumor},
                    gossipWorker(Neighbors, RumorsHeard + 1);
                true ->
                    io:format("Node ~p has heard rumor ~p times.~n", [self(), RumorsHeard]),
                    false
            end
    end.

% findSupervisorPid(SupervisorName) ->
%     global:send(SupervisorName, {health_check, self()}),
%     R =
%         receive
%             {health_response, M, Pid} ->
%                 io:format("~p~n", [M]),
%                 [ok, M, Pid]
%         after 5000 -> {error, "no answer!"}
%         end,
%     lists:nth(3, R).

spawnMultipleWorkers(NumberOfActorsToSpawn, Algorithm) ->
    spawnMultipleWorkers(NumberOfActorsToSpawn, Algorithm, []).
spawnMultipleWorkers(0, _, ListOfPid) ->
    ListOfPid;
spawnMultipleWorkers(NumberOfActorsToSpawn, Algorithm, ListOfPid) ->
    spawnMultipleWorkers(NumberOfActorsToSpawn - 1, Algorithm, [
        spawn(worker, gossipWorker, [[], 0]) | ListOfPid
    ]).

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
            setupWorkers(WorkerPids, NeighboursMap, NumNodes, 1);
        true ->
            false
    end.

init(NumNodes, Topology, Algorithm) ->
    % SupervisorPid = findSupervisorPid(SupervisorName),
    % io:format("~p~n", [SupervisorPid]),

    NumberOfActorsToSpawn = 10,
    WorkerPids = spawnMultipleWorkers(NumberOfActorsToSpawn, Algorithm),
    io:format("~p\n", [WorkerPids]),
    Dims = determineTopologyDims(Topology, NumNodes),
    setupTopology(Topology, Dims, NumNodes, WorkerPids),
    ok.
