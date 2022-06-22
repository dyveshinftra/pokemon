-module(binary_ext_tests).
-include_lib("eunit/include/eunit.hrl").

to_hex_test_() ->
	[
	 ?_assertEqual(<<"">>, binary_ext:to_hex(<<"">>)),
	 ?_assertEqual(<<"185eb09397f65a765b81a13de396fb79">>, binary_ext:to_hex(<<24,94,176,147,151,246,90,118,91,129,161,61,227,150,251, 121>>))
	].
