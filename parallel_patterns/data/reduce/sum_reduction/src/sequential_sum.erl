-module(sequential_sum).
-export([sum/1]).

sum(Binary) when is_binary(Binary) ->
    sum(Binary, 0).

sum(<<>>, Acc) ->
    Acc;
sum(<<Int:32, Rest/binary>>, Acc) ->
    sum(Rest, Acc + Int).
