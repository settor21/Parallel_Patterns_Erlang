-module(parallel_sort).
-export([sort/1]).

% Number of elements below which sorting is done sequentially
-define(THRESHOLD, 1000).

sort(Array) ->
    merge_sort(Array).

merge_sort(List) when length(List) < ?THRESHOLD ->
    sequential_sort:sort(List);
merge_sort(List) ->
    {L1, L2} = lists:split(length(List) div 2, List),
    Pid1 = spawn(fun() -> self() ! {self(), sequential_sort:sort(L1)} end),
    Pid2 = spawn(fun() -> self() ! {self(), sequential_sort:sort(L2)} end),
    SortedL1 = get_result(Pid1),
    SortedL2 = get_result(Pid2),
    merge(SortedL1, SortedL2).

get_result(Pid) ->
    receive
        {Pid, Result} -> Result
    end.

merge([], L) ->
    L;
merge(L, []) ->
    L;
merge([H1|T1] = L1, [H2|T2] = L2) ->
    if
        H1 =< H2 ->
            [H1|merge(T1, L2)];
        true ->
            [H2|merge(L1, T2)]
    end.
