-module(sequential_sort).
-export([sort/1]).

% Sequential sorting using merge sort
sort(Array) ->
    lists:merge_sort(Array).
