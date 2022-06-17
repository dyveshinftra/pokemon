% PoGo API (https://pogoapi.net/)
%
% The PoGo API is a website dedicated to various machine readable data about
% Pokemon Go.
%
% We aim to provide useful information that you can use to produce tools and
% information for the Pokemon Go community.

-module(pogo_api).
-behaviour(gen_server).

% called by supervisor
-export([start_link/0]).

% gen_server behaviour
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-export([type_effectiveness/0]).

start_link() ->
    ServerName = {local, ?MODULE},
    Module     = ?MODULE,
    Args       = [],
    Options    = [],
    gen_server:start_link(ServerName, Module, Args, Options).

init([]) ->
    io:format("TODO: check hashes in cache~n"),
    {ok, {}}.

terminate(_Reason, _State) ->
    ok.

handle_call(type_effectiveness, _From, State) ->
    {reply, ok, State};
handle_call(Request, _From, State) ->
    io:format("TODO: remove for real handle call~n"),
    {reply, Request, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Request, State) ->
    {noreply, Request, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% what follows must be refactored

%-define(BASE_URL, "https://pogoapi.net/").
%-define(HASHES_URL, ?BASE_URL ++ "/api/v1/api_hashes.json").
%-define(TYPE_EFFECTIVENESS, ?BASE_URL ++ "/api/v1/type_effectiveness.json").

%fetch(Url) ->
    %{ok, {{_Version, 200, _Reason}, _Headers, Body}} = httpc:request(Url),
    %Md5 = erlang:md5(Body),
    %BodyDecoded = jsone:decode(list_to_binary(Body), [{keys, atom}]),
    %{Md5, BodyDecoded}.

%hashes() -> fetch(?HASHES_URL).
%type_effectiveness() -> fetch(?TYPE_EFFECTIVENESS).

type_effectiveness() ->
    gen_server:call(?MODULE, type_effectiveness).
