%%%-------------------------------------------------------------------
%% @doc pokemongo public API
%% @end
%%%-------------------------------------------------------------------

-module(pokemongo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pokemongo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
