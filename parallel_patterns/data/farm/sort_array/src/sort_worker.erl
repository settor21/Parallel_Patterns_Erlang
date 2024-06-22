-module(sort_worker).
-export([sort_chunk/1]).

% Function to sort a chunk of an array
sort_chunk(Chunk) ->
    lists:merge_sort(Chunk).
