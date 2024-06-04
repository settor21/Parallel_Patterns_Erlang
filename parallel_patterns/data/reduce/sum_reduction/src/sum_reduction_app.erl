%%%-------------------------------------------------------------------
%% @doc sum_reduction public API
%% @end
%%%-------------------------------------------------------------------

-module(sum_reduction_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sum_reduction_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
