-module(hd_cache_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, "eunit-pt").

default_path_test() ->
    hd_cache:start_link(?APPLICATION),
    true = is_list(hd_cache:get_path()),
    hd_cache:stop().

configured_path_test() ->
    ConfiguredCachePath = "Yes, this is my cache path!",
    application:set_env(?APPLICATION, cache_path, ConfiguredCachePath),
    hd_cache:start_link(?APPLICATION),
    ConfiguredCachePath = hd_cache:get_path(),
    hd_cache:stop().
