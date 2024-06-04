-module(reduce_sum).
-export([sum/1, sum_chunk/2, collect_results/2]).

sum(Binary) when is_binary(Binary) ->
    NumCores = erlang:system_info(logical_processors_online),
    ChunkSize = (byte_size(Binary) div NumCores) div 4 * 4, % Ensure chunk size is multiple of 4 bytes (32 bits)
    Binaries = split_binary(Binary, ChunkSize, []),
    Parent = self(),
    Pids = [spawn(?MODULE, sum_chunk, [Parent, Chunk]) || Chunk <- Binaries],
    collect_results(Pids, 0).

split_binary(<<>>, _, Acc) ->
    lists:reverse(Acc);
split_binary(Binary, ChunkSize, Acc) when byte_size(Binary) =< ChunkSize ->
    lists:reverse([Binary | Acc]);
split_binary(Binary, ChunkSize, Acc) ->
    <<Chunk:ChunkSize/binary, Rest/binary>> = Binary,
    split_binary(Rest, ChunkSize, [Chunk | Acc]).

sum_chunk(Parent, Binary) ->
    Result = sequential_sum:sum(Binary),
    Parent ! {self(), Result}.

collect_results([], Acc) ->
    Acc;
collect_results([Pid | Pids], Acc) ->
    receive
        {Pid, Result} ->
            collect_results(Pids, Acc + Result)
    end.
