% The supervisor for PoGo API (Pokémon Go database).

-module(pogo_api_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILDREN, []).

start_link() ->
    SupName = {local, ?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

init([]) ->
    Flags = {one_for_one, 1, 5},
    Workers = lists:map(fun spec/1, ?CHILDREN),
    {ok, {Flags, Workers}}.

spec(Mod) ->
    {Mod,
     {Mod, start_link, []},
     permanent,
     1000,
     worker,
     [Mod]}.
