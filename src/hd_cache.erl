% The benefits of caching (web) data on the local hard-disk are:
%   - off-line support
%   - reduce bandwidth use
%   - lower webserver load
-module(hd_cache).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

% start/stop server
-export([start_link/0, start_link/1, stop/0]).

% generic server behaviour
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% exported functionality
-export([get_path/0,    % where cached files are stored
         write_file/2,  % write a cache file
         read_file/1,   % read a cache file
         md5/1,         % calculate MD5 of cache file
         age/1          % age of cache file in seconds
        ]).

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

% path where cached files are stored defaults to the application's user cache
% but can be from config
init_path(Application) ->
    case application:get_env(Application, cache_path) of
        {ok, Path} -> Path;
        undefined  -> filename:basedir(user_cache, Application)
    end.

init([Application]) ->
    Path = init_path(Application),
    State = #state{path=Path},
    {ok, State}.

% construct absolute filename based and filename and state's path
absname(Filename, State) ->
    Path = State#state.path,
    filename: absname_join(Path, Filename).

% return the path where cached files are stored
handle_call(get_path, _From, State) ->
    Reply = State#state.path,
    {reply, Reply, State};

% write a cache file
% this also creates path when necessary
handle_call({write_file, Filename, Bytes}, _From, State) ->
    AbsoluteFilename = absname(Filename, State),
    Reply = case filelib:ensure_dir(AbsoluteFilename) of
        ok -> file:write_file(AbsoluteFilename, Bytes);
        {error, Reason} -> {error, Reason}
    end,
    {reply, Reply, State};

% read a cache file
handle_call({read_file, Filename}, _From, State) ->
    Reply = file:read_file(absname(Filename, State)),
    {reply, Reply, State};

% calculate MD5 on cached file
handle_call({md5, Filename}, _From, State) ->
    {ok, Bytes} = file:read_file(absname(Filename, State)),
    Reply = binary_ext:to_hex(erlang:md5(Bytes)),
    {reply, Reply, State};

% age of cache file in seconds
handle_call({age, Filename}, _From, State) ->
    {ok, FileInfo} = file:read_file_info(absname(Filename, State), [{time, posix}]),
    Reply = erlang:system_time(second) - FileInfo#file_info.mtime,
    {reply, Reply, State};

% stop the server
handle_call(stop, _From, State) ->
    Reason = normal,
    Reply  = ok,
    {stop, Reason, Reply, State}.

% stubs for unused generic server behaviour
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% public API
stop()              -> gen_server:call(?MODULE, stop).
get_path()          -> gen_server:call(?MODULE, get_path).
read_file(Filename) -> gen_server:call(?MODULE, {read_file, Filename}).
md5(Filename)       -> gen_server:call(?MODULE, {md5, Filename}).
age(Filename)       -> gen_server:call(?MODULE, {age, Filename}).
write_file(Filename, Bytes) ->
    gen_server:call(?MODULE, {write_file, Filename, Bytes}).

