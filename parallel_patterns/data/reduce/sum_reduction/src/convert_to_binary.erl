-module(convert_to_binary).
-export([list_to_binary/1]).

list_to_binary(List) when is_list(List) ->
    list_to_binary(List, <<>>).

list_to_binary([], Acc) ->
    Acc;
list_to_binary([H | T], Acc) when is_integer(H) ->
    list_to_binary(T, <<Acc/binary, H:32>>).
