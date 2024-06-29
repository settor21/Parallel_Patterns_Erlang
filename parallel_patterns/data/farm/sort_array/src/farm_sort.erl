-module(farm_sort).
-export([sort/2]).

% Number of elements below which sorting is done sequentially
-define(THRESHOLD, 1000).

sort(Array, NumWorkers) when NumWorkers > 0 ->
    case length(Array) > ?THRESHOLD of
        true ->
            ChunkSize = length(Array) div NumWorkers,
            spawn_workers(Array, NumWorkers, ChunkSize, []);
        false ->
            sequential_sort:sort(Array)
    end;
sort(_, _) ->
    io:format("Number of workers must be greater than 0.~n", []).

spawn_workers([], _, _, Pids) ->
    collect_results(Pids, []);
spawn_workers(Array, NumWorkers, ChunkSize, Pids) ->
    {Chunk, Rest} = lists:split(ChunkSize, Array),
    Pid = spawn(farm_worker, start, [{self(), Chunk}]),
    spawn_workers(Rest, NumWorkers - 1, ChunkSize, [Pid | Pids]).

collect_results([], SortedChunks) ->
    merge_all(SortedChunks);
collect_results([Pid | Rest], SortedChunks) ->
    receive
        {Pid, SortedChunk} ->
            collect_results(Rest, [SortedChunk | SortedChunks])
    end.

merge_all(Chunks) ->
    lists:foldl(fun sequential_sort:merge/2, [], Chunks).
