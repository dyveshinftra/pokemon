% The benefits of caching (web) data on the local hard-disk are:
%   - off-line support
%   - reduce bandwidth use
%   - lower webserver load

-module(hd_cache).
-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([get_path/0]). % get path of cached files

-record(state, {path}).

start_link() ->
    Application = application:get_application(),
    start_link(Application).
start_link(Application) ->
    ServerName = {local, ?MODULE},
    Module     = ?MODULE,
    Args       = [Application],
    Options    = [],
    gen_server:start_link(ServerName, Module, Args, Options).

init([Application]) ->
    PathType = user_cache,
    Path = filename:basedir(PathType, Application),
    State = #state{path=Path},
    {ok, State}.

handle_call(get_path, _From, State) ->
    Reply = State#state.path,
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    Reason = normal,
    Reply  = ok,
    {stop, Reason, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = error,
    {reply, Reply, State}.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

stop()     -> gen_server:call(?MODULE, stop).
get_path() -> gen_server:call(?MODULE, get_path).