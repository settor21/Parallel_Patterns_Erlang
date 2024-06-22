%%%-------------------------------------------------------------------
%% @doc find_max_value public API
%% @end
%%%-------------------------------------------------------------------

-module(find_max_value_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    find_max_value_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
