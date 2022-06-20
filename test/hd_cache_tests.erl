-module(hd_cache_tests).
-include_lib("eunit/include/eunit.hrl").

path_test() ->
    % default path
    hd_cache:start_link("pt"),
    true = is_list(hd_cache:get_path()),
    hd_cache:stop().