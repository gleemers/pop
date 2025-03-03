-module(pop).
-export([main/1]).

usage() ->
    io:format("Usage:~n"),
    io:format("  pop compile <filename>  - Compile a Pop source file~n"),
    io:format("  pop run <filename>      - Run a Pop file~n"),
    io:format("  pop <filename>          - Compile and run a Pop file~n"),
    io:format("~nExamples:~n"),
    io:format("  pop compile examples/hello.pop~n"),
    io:format("  pop run examples/hello.pop~n"),
    io:format("  pop examples/hello.pop~n"),
    halt(1).

main([]) ->
    usage();

main(["compile", Filename]) ->
    case compile_file(Filename) of
        ok ->
            io:format("Compilation successful: ~p~n", [Filename]);
        {error, Reason} ->
            io:format("Compilation failed: ~p~n", [Reason]),
            halt(1)
    end;

main(["run", Filename]) ->
    % For simplicity, just compile and run
    main([Filename]);

main([Filename]) ->
    % Compile and run in one step
    case compile_file(Filename) of
        ok ->
            % Run the compiled module
            ModuleName = pop_module,
            try
                ModuleName:main()
            catch
                Error:Reason:Stacktrace ->
                    io:format("Runtime error: ~p:~p~n", [Error, Reason]),
                    io:format("Stacktrace: ~p~n", [Stacktrace]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("Compilation failed: ~p~n", [Reason]),
            halt(1)
    end;

main(_) ->
    usage().

% Helper function to compile a file
compile_file(Filename) ->
    case pop_compiler:compile_file(Filename) of
        {ok, _Module} ->
            ok;
        Error ->
            Error
    end.
