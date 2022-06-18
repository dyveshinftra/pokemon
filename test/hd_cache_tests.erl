-module(hd_cache_tests).
-include_lib("eunit/include/eunit.hrl").

path_test() ->
    % default path
    hd_cache:start_link(),
    true = is_list(hd_cache:path()),
    hd_cache:stop().