-module(supervisorActor).

-import(rng, [rnd_chars/1]).
-import(utils, [mod/2]).

-export([init/0, superVisorListener/1]).

superVisorListener(Count) ->
    receive
        {} ->
            Remainder = mod(Count, 10000),
            if
                Remainder == 0 ->
                    % Logic to send rumor
                    superVisorListener(Count);
                true ->
                    superVisorListener(Count)
            end;
        {health_check, Pid} ->
            io:format("Process: ~p~n", [Pid]),
            Pid ! {health_response, "successful health check", self()},
            superVisorListener(Count)
    end.

init() ->
    Pid = spawn(fun() -> superVisorListener(1) end),
    % we keep track of the process id
    global:register_name(server, Pid),
    io:format("~p~n", [Pid]),
    ok.
