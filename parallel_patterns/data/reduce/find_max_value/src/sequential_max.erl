-module(sequential_max).
-compile({no_auto_import, [max/2]}).
-export([max/1]).

max(Binary) when is_binary(Binary) ->
    case Binary of
        <<>> -> -9223372036854775807; % Handle empty binary case
        _ -> max(Binary, -9223372036854775807)
    end.

max(<<>>, MaxSoFar) ->
    MaxSoFar;
max(<<Value:32/integer-signed, Rest/binary>>, MaxSoFar) ->
    NewMax = find_max(Value, MaxSoFar),
    max(Rest, NewMax).

find_max(X, Y) when X >= Y -> X;
find_max(_, Y) -> Y.
