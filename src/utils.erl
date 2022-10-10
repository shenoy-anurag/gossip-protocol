-module(utils).
-export([writeKV/2, mod/2]).

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _) -> 0.

writeKV(String, Value) ->
    io:format("~p = ~p~n", [String, Value]).
