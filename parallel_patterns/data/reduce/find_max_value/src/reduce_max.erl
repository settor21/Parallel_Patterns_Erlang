-module(reduce_max).
-compile({no_auto_import, [max/2]}).
-export([max/1, max_chunk/2, collect_results/2]).

max(Binary) when is_binary(Binary) ->
    NumCores = erlang:system_info(logical_processors_online),
    ChunkSize = (byte_size(Binary) div max(1, NumCores div 4)) * 4, % Ensure chunk size is multiple of 4 bytes (32 bits)
    Binaries = split_binary(Binary, ChunkSize, []),
    Parent = self(),
    Pids = [spawn(?MODULE, max_chunk, [Parent, Chunk]) || Chunk <- Binaries],
    collect_results(Pids, -9223372036854775807).

max_chunk(Parent, Binary) ->
    Result = sequential_max:max(Binary),
    Parent ! {self(), Result}.

collect_results([], MaxSoFar) ->
    MaxSoFar;
collect_results([Pid | Pids], MaxSoFar) ->
    receive
        {Pid, Result} ->
            NewMax = max(MaxSoFar, Result),
            collect_results(Pids, NewMax)
    end.

split_binary(<<>>, _, Acc) ->
    lists:reverse(Acc);
split_binary(Binary, ChunkSize, Acc) when byte_size(Binary) =< ChunkSize ->
    lists:reverse([Binary | Acc]);
split_binary(Binary, ChunkSize, Acc) ->
    <<Chunk:ChunkSize/binary, Rest/binary>> = Binary,
    split_binary(Rest, ChunkSize, [Chunk | Acc]).

max(X, Y) when X >= Y -> X;
max(_, Y) -> Y.
