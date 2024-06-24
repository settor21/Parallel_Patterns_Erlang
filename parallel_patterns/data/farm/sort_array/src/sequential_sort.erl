-module(sequential_sort).
-export([sort/1]).

% Import the lists module to use merge sort
-import(lists, [merge/2]).

% Sequential sorting using merge sort
sort(Array) ->
    merge_sort(Array).

merge_sort([]) ->
    [];
merge_sort([X]) ->
    [X];
merge_sort(List) ->
    {L1, L2} = lists:split(length(List) div 2, List),
    merge(merge_sort(L1), merge_sort(L2)).
