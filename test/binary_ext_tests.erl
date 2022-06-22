-module(binary_ext_tests).
-include_lib("eunit/include/eunit.hrl").

to_hex_test_() ->
	[
	 ?_assert(binary_ext:to_hex(<<"">>) =:= <<"">>)
	].
