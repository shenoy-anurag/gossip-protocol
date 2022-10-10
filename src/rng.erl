-module(rng).

-export([rnd_chars/1, rnd_number/1, rnd_alphanumeric_str/1]).

rnd_chars(L)            -> get_rnd(L, chars).
rnd_number(L)           -> get_rnd(L, numbers).
rnd_alphanumeric_str(L) -> get_rnd(L, alphanumeric).

get_rnd(L, chars)         -> gen_rnd(L, "abcdefghijklmnopqrstuvwxyz");
get_rnd(L, numbers)       -> gen_rnd(L, "1234567890");
get_rnd(L, alphanumeric) -> gen_rnd(L, "abcdefghijklmnopqrstuvwxyz1234567890").

gen_rnd(Length, AllowedChars) ->
  MaxLength = length(AllowedChars),
  lists:foldl(
    fun(_, Acc) -> [lists:nth(rand:uniform(MaxLength), AllowedChars)] ++ Acc end,
    [], lists:seq(1, Length)
  ).
