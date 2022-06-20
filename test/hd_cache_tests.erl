-module(hd_cache_tests).
-include_lib("eunit/include/eunit.hrl").

path_test() ->
    Application = "pt",

    % default path
    hd_cache:start_link(Application),
    true = is_list(hd_cache:get_path()),
    hd_cache:stop().
