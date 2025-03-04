-module(pop_compiler).
-export([compile/1, compile_file/1]).

-spec compile(string()) -> {ok, module()} | {ok, module(), list()} | {error, term()}.
compile(Source) ->
    case pop_lexer:string(Source) of
        {ok, Tokens, _} ->
            case pop_parser:parse(Tokens) of
                {ok, AST} ->
                    % Run semantic analysis
                    case pop_semantic_analyzer:analyze(AST) of
                        {[], []} ->
                            % No errors or warnings, proceed with compilation
                            compile_ast(AST);
                        {[], Warnings} ->
                            % No errors but has warnings, proceed with compilation
                            case compile_ast(AST) of
                                {ok, Module} ->
                                    {ok, Module, Warnings};
                                Error ->
                                    Error
                            end;
                        {Errors, Warnings} ->
                            % Return semantic errors and warnings
                            {error, Errors, Warnings}
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec compile_file(string()) -> {ok, module()} | {ok, module(), list()} | {error, term()}.
compile_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            compile(binary_to_list(Binary));
        Error ->
            Error
    end.

compile_ast(AST) ->
    ModuleName = pop_module,
    Forms = to_beam_forms(ModuleName, AST),
    case compile:forms(Forms, [return_errors]) of
        {ok, ModuleName, Beam} ->
            code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".beam", Beam),
            {ok, ModuleName};
        Error ->
            Error
    end.

to_beam_forms(ModuleName, AST) ->
    % Module declaration
    ModuleDecl = {attribute, 1, module, ModuleName},
    ExportDecl = {attribute, 2, export, [{main,0}]},
    
    % Convert our AST to BEAM abstract format
    Functions = lists:map(fun convert_to_beam/1, AST),
    
    % Add a main function if one doesn't exist
    Functions2 = case has_main_function(Functions) of
        true -> Functions;
        false -> Functions ++ [create_main_function()]
    end,
    
    [ModuleDecl, ExportDecl] ++ Functions2.

has_main_function(Functions) ->
    lists:any(fun(F) ->
        case F of
            {function, _, main, 0, _} -> true;
            _ -> false
        end
    end, Functions).

convert_to_beam({function, Name, Params, Body}) ->
    Arity = length(Params),
    Line = get_line_number(Body),
    {function, Line, Name, Arity,
     [{clause, Line,
       [param_to_beam(P, Line) || P <- Params],
       [],
       [statements_to_beam(S) || S <- Body]}]};

convert_to_beam({expr, Expr}) ->
    Line = get_line_number([Expr]),
    expr_to_beam(Expr, Line).

% Get line number from AST node or default to 1
get_line_number(_) ->
    % In a real implementation, this would extract line info from tokens
    % For now, we'll use 1 as a default
    1.

statements_to_beam({expr, Expr}) ->
    Line = get_line_number([Expr]),
    expr_to_beam(Expr, Line);
statements_to_beam({if_stmt, Condition, ThenBlock}) ->
    Line = get_line_number([Condition]),
    {'case', Line,
     expr_to_beam(Condition, Line),
     [{clause, Line, [{atom, Line, true}], [], [statements_to_beam(S) || S <- ThenBlock]},
      {clause, Line, [{var, Line, '_'}], [], [{atom, Line, ok}]}]};
statements_to_beam({if_else_stmt, Condition, ThenBlock, ElseBlock}) ->
    Line = get_line_number([Condition]),
    {'case', Line,
     expr_to_beam(Condition, Line),
     [{clause, Line, [{atom, Line, true}], [], [statements_to_beam(S) || S <- ThenBlock]},
      {clause, Line, [{var, Line, '_'}], [], [statements_to_beam(S) || S <- ElseBlock]}]};
statements_to_beam({return_stmt, nil}) ->
    {atom, 1, ok};
statements_to_beam({return_stmt, Value}) ->
    Line = get_line_number([Value]),
    expr_to_beam(Value, Line);
statements_to_beam(Other) ->
    Line = get_line_number([Other]),
    expr_to_beam(Other, Line).

expr_to_beam({op, '+', Left, Right}, Line) ->
    {op, Line, '+',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({op, '-', Left, Right}, Line) ->
    {op, Line, '-',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({op, '*', Left, Right}, Line) ->
    {op, Line, '*',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({op, '/', Left, Right}, Line) ->
    {op, Line, '/',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};
expr_to_beam({op, '%', Left, Right}, Line) ->
    {op, Line, 'rem',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({op, '==', Left, Right}, Line) ->
    {op, Line, '==',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({op, '!=', Left, Right}, Line) ->
    {op, Line, '/=',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({op, '>', Left, Right}, Line) ->
    {op, Line, '>',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({op, '<', Left, Right}, Line) ->
    {op, Line, '<',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({op, '>=', Left, Right}, Line) ->
    {op, Line, '>=',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({op, '<=', Left, Right}, Line) ->
    {op, Line, '=<',
     expr_to_beam(Left, Line),
     expr_to_beam(Right, Line)};

expr_to_beam({assign, Name, Value}, Line) ->
    {match, Line,
     {var, Line, Name},
     expr_to_beam(Value, Line)};

expr_to_beam({call, print, Args}, Line) ->
    % Special handling for print calls
    case Args of
        [Value] ->
            case Value of
                Value when is_list(Value) ->
                    % If it's a string, use ~s to avoid quotes
                    {call, Line,
                     {remote, Line, {atom, Line, io}, {atom, Line, format}},
                     [{string, Line, "~s"}, {cons, Line, expr_to_beam(Value, Line), {nil, Line}}]};
                _ ->
                    % Otherwise use ~p
                    {call, Line,
                     {remote, Line, {atom, Line, io}, {atom, Line, format}},
                     [{string, Line, "~p"}, {cons, Line, expr_to_beam(Value, Line), {nil, Line}}]}
            end;
        _ ->
            % Format multiple arguments with spaces between them
            % Use ~p for all arguments for consistency
            Format = string:join(lists:duplicate(length(Args), "~p"), " "),
            {call, Line,
             {remote, Line, {atom, Line, io}, {atom, Line, format}},
             [{string, Line, Format}, {cons, Line, expr_to_beam(hd(Args), Line), format_args_tail(tl(Args), Line)}]}
    end;

expr_to_beam({call, println, Args}, Line) ->
    % Special handling for println calls (print with newline)
    case Args of
        [Value] ->
            case Value of
                Value when is_list(Value) ->
                    % If it's a string, use ~s to avoid quotes
                    {call, Line,
                     {remote, Line, {atom, Line, io}, {atom, Line, format}},
                     [{string, Line, "~s~n"}, {cons, Line, expr_to_beam(Value, Line), {nil, Line}}]};
                _ ->
                    % Otherwise use ~p
                    {call, Line,
                     {remote, Line, {atom, Line, io}, {atom, Line, format}},
                     [{string, Line, "~p~n"}, {cons, Line, expr_to_beam(Value, Line), {nil, Line}}]}
            end;
        _ ->
            % Format multiple arguments with spaces between them
            % Use ~p for all arguments for consistency
            Format = string:join(lists:duplicate(length(Args), "~p"), " ") ++ "~n",
            {call, Line,
             {remote, Line, {atom, Line, io}, {atom, Line, format}},
             [{string, Line, Format}, {cons, Line, expr_to_beam(hd(Args), Line), format_args_tail(tl(Args), Line)}]}
    end;

expr_to_beam({call, io_format, Args}, Line) ->
    % Special handling for io:format calls
    case Args of
        [Format] ->
            % Process escape sequences in the format string
            ProcessedFormat = process_string_escapes(Format),
            {call, Line,
             {remote, Line, {atom, Line, io}, {atom, Line, format}},
             [{string, Line, ProcessedFormat}, {nil, Line}]};
        [Format, Arg] when is_atom(Arg) ->
            % Process escape sequences in the format string
            ProcessedFormat = process_string_escapes(Format),
            {call, Line,
             {remote, Line, {atom, Line, io}, {atom, Line, format}},
             [{string, Line, ProcessedFormat}, {cons, Line, {var, Line, Arg}, {nil, Line}}]};
        [Format, Arg] ->
            % Process escape sequences in the format string
            ProcessedFormat = process_string_escapes(Format),
            {call, Line,
             {remote, Line, {atom, Line, io}, {atom, Line, format}},
             [{string, Line, ProcessedFormat}, {cons, Line, expr_to_beam(Arg, Line), {nil, Line}}]};
        [Format, Arg1, Arg2] ->
            % Process escape sequences in the format string
            ProcessedFormat = process_string_escapes(Format),
            {call, Line,
             {remote, Line, {atom, Line, io}, {atom, Line, format}},
             [{string, Line, ProcessedFormat}, 
              {cons, Line, expr_to_beam(Arg1, Line), 
               {cons, Line, expr_to_beam(Arg2, Line), {nil, Line}}}]}
    end;

expr_to_beam({call, Name, Args}, Line) ->
    {call, Line,
     {atom, Line, Name},
     [expr_to_beam(Arg, Line) || Arg <- Args]};

expr_to_beam({integer, Value}, Line) ->
    {integer, Line, Value};

expr_to_beam(Value, Line) when is_integer(Value) ->
    {integer, Line, Value};

expr_to_beam(Value, Line) when is_float(Value) ->
    {float, Line, Value};

expr_to_beam(Value, Line) when is_list(Value) ->
    {string, Line, process_string_escapes(Value)};

expr_to_beam(Value, Line) when is_atom(Value) ->
    {var, Line, Value};

expr_to_beam(nil, Line) ->
    {atom, Line, undefined}.

param_to_beam(Name, Line) ->
    {var, Line, Name}.

% Format the tail of argument list for io:format
format_args_tail([], _Line) ->
    {nil, 1};
format_args_tail([Arg | Rest], Line) ->
    {cons, Line, expr_to_beam(Arg, Line), format_args_tail(Rest, Line)}.

% Process escape sequences in strings
process_string_escapes(String) ->
    % Replace \n with actual newlines, etc.
    lists:flatten(process_escapes(String)).

process_escapes([]) ->
    [];
process_escapes([$\\, $n | Rest]) ->
    [$\n | process_escapes(Rest)];
process_escapes([$\\, $t | Rest]) ->
    [$\t | process_escapes(Rest)];
process_escapes([$\\, $r | Rest]) ->
    [$\r | process_escapes(Rest)];
process_escapes([$\\, $\\ | Rest]) ->
    [$\\ | process_escapes(Rest)];
process_escapes([C | Rest]) ->
    [C | process_escapes(Rest)].

% Create a default main function
create_main_function() ->
    {function, 1, main, 0,
     [{clause, 1, [], [],
       [{call, 1,
         {remote, 1, {atom, 1, io}, {atom, 1, format}},
         [{string, 1, "Hello from Pop!~n"}, {nil, 1}]}]}]}.
