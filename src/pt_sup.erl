% The top supervisor for the Pokémon Toolkit application.

-module(pt_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILDREN, []).

start_link() ->
    SupName = {local, ?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

init([]) ->
    Flags = {one_for_one, 1, 5},
    ChildSpecs = lists:map(fun spec/1, ?CHILDREN),
    {ok, {Flags, ChildSpecs}}.

spec(Mod) ->
    {Mod,
     {Mod, start_link, []},
     permanent,
     infinity,
     supervisor,
     [Mod]}.
