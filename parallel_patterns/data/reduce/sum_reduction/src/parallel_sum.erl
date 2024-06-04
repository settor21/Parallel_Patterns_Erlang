-module(parallel_sum).
-export([sum/1, sum_chunk/2]).

sum(Binary) when is_binary(Binary) ->
    ChunkSize = 8 * 1024, % 8 KB chunks for example
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