% The benefits of caching (web) data on the local hard-disk are:
%   - off-line support
%   - reduce bandwidth use
%   - lower webserver load
-module(hd_cache).
-behaviour(gen_server).

% called by supervisor
-export([start_link/0]).

% gen_server behaviour
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% stop server
-export([stop/0]).

% calls
-export([path/0]). % cache path where files are stored

% called by supervisor
start_link() ->
    ServerName = {local, ?MODULE},
    Module     = ?MODULE,
    Args       = [],
    Options    = [],
    gen_server:start_link(ServerName, Module, Args, Options).

% initialise generic server state
init([]) ->
    State = [],
    {ok, State}.

% return the path used to store the cache files
handle_call(path, _From, State) ->
    Reply = "TODO: must come from state!",
    {reply, Reply, State};

% handle stop gracefully
handle_call(stop, _From, State) ->
    Reason = normal,
    Reply  = ok,
    {stop, Reason, Reply, State};

% unknown call
handle_call(_Request, _From, State) ->
    Reply = error,
    {reply, Reply, State}.

% unknown cast
handle_cast(_Request, State) ->
    {noreply, State}.

% unknown info
handle_info(_Info, State) ->
    {noreply, State}.

% terminate server
terminate(_Reason, _State) ->
    ok.

% no change in state on code change
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% stop server
stop() ->
    gen_server:call(?MODULE, stop).

% which path is used to store the cache files
path() ->
    gen_server:call(?MODULE, path).