-module(server).

-import(rng, [rnd_chars/1]).
-import(utils, [mod/2]).

-export([init/0, superVisorListener/2]).

superVisorListener(Count, NodesTerminatedList) ->
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
            superVisorListener(Count, NodesTerminatedList);
        {terminated, Pid} ->
            io:format("Actor ~p terminated~n", [Pid]),
            NewNodesTerminatedList = [Pid | NodesTerminatedList],
            superVisorListener(Count, NewNodesTerminatedList)
    end.

init() ->
    Pid = spawn(server, superVisorListener, [0, []]),
    % we keep track of the process id
    global:register_name(server, Pid),
    io:format("~p~n", [Pid]),
    ok.
