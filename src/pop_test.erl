-module(pop_test).
-export([test/0]).

test() ->
    Source = "
        fun add(x, y) {
            x + y;
        }

        fun main() {
            result = add(5, 3);
            if (result == 8) {
                io_format(\"Test passed!\\n\");
            } else {
                io_format(\"Test failed!\\n\");
            }
        }
    ",
    case pop_compiler:compile(Source) of
        {ok, Module} ->
            io:format("Compilation successful!~n"),
            Module:main();
        {error, Reason} ->
            io:format("Compilation failed: ~p~n", [Reason])
    end.
