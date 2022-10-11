-module(server).

-import(rng, [rnd_chars/1]).
-import(utils, [mod/2]).

-export([init/0, superVisorListener/6]).

superVisorListener(
    NumWorkers, WorkerPids, StartTime, Count, NodesTerminatedList, NodesConvergedList
) ->
    receive
        % {} ->
        %     Remainder = mod(Count, 10000),
        %     if
        %         Remainder == 0 ->
        %             % Logic to send rumor
        %             superVisorListener(Count);
        %         true ->
        %             superVisorListener(Count)
        %     end;
        {health_check, Pid} ->
            io:format("Process: ~p~n", [Pid]),
            Pid ! {health_response, "successful health check", self()},
            superVisorListener(
                NumWorkers, WorkerPids, StartTime, Count, NodesTerminatedList, NodesConvergedList
            );
        {check_state} ->
            io:format(
                "NumWorkers: ~p, WorkerPids: ~p, StartTime: ~p, Count: ~p, NodesTerminatedList: ~p, NodesConvergedList: ~p~n",
                [NumWorkers, WorkerPids, StartTime, Count, NodesTerminatedList, NodesConvergedList]
            ),
            superVisorListener(
                NumWorkers, WorkerPids, StartTime, Count, NodesTerminatedList, NodesConvergedList
            );
        {add_worker, Pid} ->
            Pids = [Pid | WorkerPids],
            superVisorListener(
                NumWorkers + 1, Pids, StartTime, Count, NodesTerminatedList, NodesConvergedList
            );
        {restart_timer, _} ->
            NewStartTime = erlang:monotonic_time(),
            superVisorListener(
                NumWorkers, WorkerPids, NewStartTime, Count, NodesTerminatedList, NodesConvergedList
            );
        {start_gossip} ->
            WorkerPicked = lists:nth(rand:uniform(length(WorkerPids)), WorkerPids),
            NewStartTime = erlang:monotonic_time(),
            WorkerPicked ! {share_gossip, "Rumor 1"},
            superVisorListener(
                NumWorkers, WorkerPids, NewStartTime, Count, NodesTerminatedList, NodesConvergedList
            );
        {start_push_sum} ->
            WorkerPicked = lists:nth(rand:uniform(length(WorkerPids)), WorkerPids),
            NewStartTime = erlang:monotonic_time(),
            Delta = math:pow(10, -10),
            WorkerPicked ! {start_push_sum, Delta},
            superVisorListener(
                NumWorkers, WorkerPids, NewStartTime, Count, NodesTerminatedList, NodesConvergedList
            );
        {terminated, Pid} ->
            io:format("Actor ~p terminated~n", [Pid]),
            NewNodesTerminatedList = [Pid | NodesTerminatedList],
            superVisorListener(
                NumWorkers, WorkerPids, StartTime, Count, NewNodesTerminatedList, NodesConvergedList
            );
        % convergence for Push Sum algorithm.
        {converged, Pid, Sum, Weight} ->
            io:format("Actor ~p converged value of Push Sum, with Sum=~p and Weight=~p ~n", [
                Pid, Sum, Weight
            ]),
            NewNodesConvergedList = [Pid | NodesConvergedList],
            if
                length(NewNodesConvergedList) > length(WorkerPids) * 0.25 ->
                    EndTime = erlang:monotonic_time(),
                    REALTIME = erlang:convert_time_unit(EndTime - StartTime, native, millisecond),
                    REALTIME_SECS = erlang:convert_time_unit(EndTime - StartTime, native, second),
                    io:format("REAL TIME OF PROGRAM in milliseconds:~p~n", [REALTIME]),
                    io:format("REAL TIME OF PROGRAM in seconds:~p~n", [REALTIME_SECS]);
                true ->
                    superVisorListener(
                        NumWorkers,
                        WorkerPids,
                        StartTime,
                        Count,
                        NodesTerminatedList,
                        NewNodesConvergedList
                    )
            end
    end.

init() ->
    % NumWorkers, WorkerPids, StartTime, Count, NodesTerminatedList, NodesConvergedList
    StartTime = erlang:monotonic_time(),
    Pid = spawn(server, superVisorListener, [0, [], StartTime, 0, [], []]),
    % we keep track of the process id
    global:register_name(server, Pid),
    io:format("~p~n", [Pid]),
    ok.
