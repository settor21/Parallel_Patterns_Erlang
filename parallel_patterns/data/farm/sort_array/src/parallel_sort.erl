-module(parallel_sort).
-export([sort/2]).

% Parallel sorting using farm pattern with multiple workers
sort(Array, NumWorkers) ->
    ChunkSize = length(Array) div NumWorkers,
    Chunks = split_into_chunks(Array, ChunkSize),
    Workers = spawn_workers(Chunks),
    SortedChunks = gather_results(Workers),
    merge_sorted_chunks(SortedChunks).

split_into_chunks(Array, ChunkSize) ->
    split_into_chunks(Array, ChunkSize, []).

split_into_chunks([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
split_into_chunks(Array, ChunkSize, Acc) ->
    {Chunk, Rest} = lists:split(ChunkSize, Array),
    split_into_chunks(Rest, ChunkSize, [Chunk | Acc]).

spawn_workers(Chunks) ->
    lists:map(fun(Chunk) -> spawn_sort_worker(Chunk) end, Chunks).

spawn_sort_worker(Chunk) ->
    spawn(fun() -> sort_worker:sort_chunk(Chunk) end).

gather_results(Workers) ->
    lists:map(fun(Pid) -> receive {Pid, SortedChunk} -> SortedChunk end end, Workers).

merge_sorted_chunks(SortedChunks) ->
    lists:flatten(SortedChunks).
