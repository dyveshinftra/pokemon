-module(hd_cache_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, "eunit-pt").

configured_path_test() ->
    ConfiguredCachePath = "Yes, this is my cache path!",
    application:set_env(?APPLICATION, cache_path, ConfiguredCachePath),
    hd_cache:start_link(?APPLICATION),
    ConfiguredCachePath = hd_cache:get_path(),
    hd_cache:stop(),
    application:unset_env(?APPLICATION, cache_path).

setup() ->
    hd_cache:start_link(?APPLICATION).

cleanup({ok, _Pid}) ->
    hd_cache:stop().

hd_cache_test_() ->
    Filename = "eunit.test",
    {setup, fun setup/0, fun cleanup/1,
     [?_assert(is_list(hd_cache:get_path()) =:= true),
      ?_assert(hd_cache:write_file(Filename, "eunit write test") =:= ok),
      ?_assert(hd_cache:read_file(Filename) =:= "eunit write test")
     ]
    }.
