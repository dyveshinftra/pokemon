% Pokémon Toolkit

-module(pt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    init(),
    pt_sup:start_link().

stop(_State) ->
    ok.

% initialise the application
init() ->
    init_httpc().

% initialise the HTTP client application
init_httpc() ->
    {ok, Application} = application:get_application(),

    % proxy (from config)
    case application:get_env(Application, proxy) of
        {ok, ProxyConfig} -> httpc:set_options([{proxy, ProxyConfig}]);
        undefined -> ok
    end.