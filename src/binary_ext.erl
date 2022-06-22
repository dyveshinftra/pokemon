% binary extensions
-module(binary_ext).
-export([to_hex/1]).

% to_hex helper: 0-15 -> 0-9,a-f
hex(N) when is_integer(N), N >= 0,  N < 10 -> $0 + N;
hex(N) when is_integer(N), N >= 10, N < 16 -> $a + N - 10.

to_hex(Binary) -> << <<(hex(N1)),(hex(N2))>> || <<N1:4,N2:4>> <= Binary >>.
