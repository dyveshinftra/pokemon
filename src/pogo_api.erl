% PoGo API (https://pogoapi.net/)
%
% The PoGo API is a website dedicated to various machine readable data about
% Pokemon Go.
%
% We aim to provide useful information that you can use to produce tools and
% information for the Pokemon Go community.

-module(pogo_api).

-export([hashes/0]).

-define(BASE_URL, "https://pogoapi.net/").
-define(HASHES_URL, ?BASE_URL ++ "/api/v1/api_hashes.json").

fetch(Url) ->
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} = httpc:request(Url),
    Md5 = erlang:md5(Body),
    BodyDecoded = jsone:decode(list_to_binary(Body), [{keys, atom}]),
    {Md5, BodyDecoded}.

hashes() -> fetch(?HASHES_URL).