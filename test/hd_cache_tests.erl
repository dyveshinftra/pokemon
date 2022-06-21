-module(hd_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, "eunit-pt").
-define(FILENAME,    "eunit.test").
-define(MSG1,        "eunit write test").
-define(MSG2,        "eunit double write test").

% weird path so seperate from other tests
configured_path_test() ->
    ConfiguredCachePath = "Yes, this is my cache path!",
    application:set_env(?APPLICATION, cache_path, ConfiguredCachePath),
    hd_cache:start_link(?APPLICATION),
    ConfiguredCachePath = hd_cache:get_path(),
    hd_cache:stop(),
    application:unset_env(?APPLICATION, cache_path).

% actual API tests
setup()             -> hd_cache:start_link(?APPLICATION).
cleanup({ok, _Pid}) -> hd_cache:stop().
hd_cache_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [
      % differs per platform, just make sure it is "sane"
      ?_assert(is_list(hd_cache:get_path()) =:= true),

      % write + read test, it should match
      ?_assert(hd_cache:write_file(?FILENAME, ?MSG1) =:= ok),
      ?_assert(hd_cache:read_file (?FILENAME) =:= {ok, list_to_binary(?MSG1)}),

      % make sure write_file "overrides"
      ?_assert(hd_cache:write_file(?FILENAME, ?MSG2) =:= ok),
      ?_assert(hd_cache:read_file (?FILENAME) =:= {ok, list_to_binary(?MSG2)}),

      % check md5
      ?_assert(hd_cache:md5(?FILENAME) =:= <<"185EB09397F65A765B81A13DE396FB79">>)
     ]
    }.
