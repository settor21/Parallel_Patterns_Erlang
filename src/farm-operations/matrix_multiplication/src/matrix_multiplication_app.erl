%%%-------------------------------------------------------------------
%% @doc matrix_multiplication public API
%% @end
%%%-------------------------------------------------------------------

-module(matrix_multiplication_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Matrix Multiplication Application started~n"),
    {ok, self()}.

stop(_State) ->
    io:format("Matrix Multiplication Application stopped~n"),
    ok.


%% internal functions
