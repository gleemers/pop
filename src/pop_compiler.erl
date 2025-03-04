-module(pop_compiler).
-export([compile/1, compile_file/1]).

-spec compile(string()) -> {ok, module()} | {error, term()}.
compile(Source) ->
    case pop_lexer:string(Source) of
        {ok, Tokens, _} ->
            case pop_parser:parse(Tokens) of
                {ok, AST} ->
                    compile_ast(AST);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec compile_file(string()) -> {ok, module()} | {error, term()}.
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
    Line = 1,
    {function, Line, Name, Arity,
     [{clause, Line,
       [param_to_beam(P) || P <- Params],
       [],
       [statements_to_beam(S) || S <- Body]}]};

convert_to_beam({expr, Expr}) ->
    expr_to_beam(Expr).

statements_to_beam({expr, Expr}) ->
    expr_to_beam(Expr);
statements_to_beam({if_stmt, Condition, ThenBlock}) ->
    {'case', 1,
     expr_to_beam(Condition),
     [{clause, 1, [{atom, 1, true}], [], [statements_to_beam(S) || S <- ThenBlock]},
      {clause, 1, [{var, 1, '_'}], [], [{atom, 1, ok}]}]};
statements_to_beam({if_else_stmt, Condition, ThenBlock, ElseBlock}) ->
    {'case', 1,
     expr_to_beam(Condition),
     [{clause, 1, [{atom, 1, true}], [], [statements_to_beam(S) || S <- ThenBlock]},
      {clause, 1, [{var, 1, '_'}], [], [statements_to_beam(S) || S <- ElseBlock]}]};
statements_to_beam({return_stmt, nil}) ->
    {atom, 1, ok};
statements_to_beam({return_stmt, Value}) ->
    expr_to_beam(Value);
statements_to_beam(Other) ->
    expr_to_beam(Other).

expr_to_beam({op, '+', Left, Right}) ->
    {op, 1, '+',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({op, '-', Left, Right}) ->
    {op, 1, '-',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({op, '*', Left, Right}) ->
    {op, 1, '*',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({op, '/', Left, Right}) ->
    {op, 1, '/',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({op, '%', Left, Right}) ->
    {op, 1, 'rem',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({op, '==', Left, Right}) ->
    {op, 1, '==',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({op, '>', Left, Right}) ->
    {op, 1, '>',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({op, '<', Left, Right}) ->
    {op, 1, '<',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({op, '>=', Left, Right}) ->
    {op, 1, '>=',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({op, '<=', Left, Right}) ->
    {op, 1, '=<',
     expr_to_beam(Left),
     expr_to_beam(Right)};

expr_to_beam({assign, Name, Value}) ->
    {match, 1,
     {var, 1, Name},
     expr_to_beam(Value)};

expr_to_beam({call, print, Args}) ->
    % Special handling for print calls
    case Args of
        [Value] ->
            case Value of
                Value when is_list(Value) ->
                    % If it's a string, use ~s to avoid quotes
                    {call, 1,
                     {remote, 1, {atom, 1, io}, {atom, 1, format}},
                     [{string, 1, "~s"}, {cons, 1, expr_to_beam(Value), {nil, 1}}]};
                _ ->
                    % Otherwise use ~p
                    {call, 1,
                     {remote, 1, {atom, 1, io}, {atom, 1, format}},
                     [{string, 1, "~p"}, {cons, 1, expr_to_beam(Value), {nil, 1}}]}
            end;
        _ ->
            % Format multiple arguments with spaces between them
            % Use ~p for all arguments for consistency
            Format = string:join(lists:duplicate(length(Args), "~p"), " "),
            {call, 1,
             {remote, 1, {atom, 1, io}, {atom, 1, format}},
             [{string, 1, Format}, {cons, 1, expr_to_beam(hd(Args)), format_args_tail(tl(Args))}]}
    end;

expr_to_beam({call, println, Args}) ->
    % Special handling for println calls
    case Args of
        [Value] ->
            case Value of
                Value when is_list(Value) ->
                    % If it's a string, use ~s to avoid quotes
                    {call, 1,
                     {remote, 1, {atom, 1, io}, {atom, 1, format}},
                     [{string, 1, "~s\n"}, {cons, 1, expr_to_beam(Value), {nil, 1}}]};
                _ ->
                    % Otherwise use ~p
                    {call, 1,
                     {remote, 1, {atom, 1, io}, {atom, 1, format}},
                     [{string, 1, "~p\n"}, {cons, 1, expr_to_beam(Value), {nil, 1}}]}
            end;
        [] ->
            % Just print a newline
            {call, 1,
             {remote, 1, {atom, 1, io}, {atom, 1, format}},
             [{string, 1, "\n"}, {nil, 1}]};
        _ ->
            % Format multiple arguments with spaces between them
            % Use ~p for all arguments for consistency
            Format = string:join(lists:duplicate(length(Args), "~p"), " ") ++ "\n",
            {call, 1,
             {remote, 1, {atom, 1, io}, {atom, 1, format}},
             [{string, 1, Format}, {cons, 1, expr_to_beam(hd(Args)), format_args_tail(tl(Args))}]}
    end;

expr_to_beam({call, io_format, Args}) ->
    % Special handling for io:format calls
    case Args of
        [Format] ->
            % Process escape sequences in the format string
            ProcessedFormat = process_string_escapes(Format),
            {call, 1,
             {remote, 1, {atom, 1, io}, {atom, 1, format}},
             [{string, 1, ProcessedFormat}, {nil, 1}]};
        [Format, Arg] when is_atom(Arg) ->
            % Process escape sequences in the format string
            ProcessedFormat = process_string_escapes(Format),
            {call, 1,
             {remote, 1, {atom, 1, io}, {atom, 1, format}},
             [{string, 1, ProcessedFormat}, {cons, 1, {var, 1, Arg}, {nil, 1}}]};
        [Format, Arg] ->
            % Process escape sequences in the format string
            ProcessedFormat = process_string_escapes(Format),
            {call, 1,
             {remote, 1, {atom, 1, io}, {atom, 1, format}},
             [{string, 1, ProcessedFormat}, {cons, 1, expr_to_beam(Arg), {nil, 1}}]};
        [Format, Arg1, Arg2] ->
            % Process escape sequences in the format string
            ProcessedFormat = process_string_escapes(Format),
            {call, 1,
             {remote, 1, {atom, 1, io}, {atom, 1, format}},
             [{string, 1, ProcessedFormat}, 
              {cons, 1, expr_to_beam(Arg1), 
               {cons, 1, expr_to_beam(Arg2), {nil, 1}}}]}
    end;

expr_to_beam({call, Name, Args}) ->
    {call, 1,
     {atom, 1, Name},
     [expr_to_beam(Arg) || Arg <- Args]};

expr_to_beam({integer, Value}) ->
    {integer, 1, Value};

expr_to_beam(Value) when is_integer(Value) ->
    {integer, 1, Value};

expr_to_beam(Value) when is_float(Value) ->
    {float, 1, Value};

expr_to_beam(Value) when is_list(Value) ->
    {string, 1, process_string_escapes(Value)};

expr_to_beam(Value) when is_atom(Value) ->
    {var, 1, Value};

expr_to_beam(nil) ->
    {atom, 1, undefined}.

param_to_beam(Name) ->
    {var, 1, Name}.

create_main_function() ->
    {function, 1, main, 0,
     [{clause, 1, [], [],
       [{call, 1,
         {remote, 1, {atom, 1, io}, {atom, 1, format}},
         [{string, 1, "Hello from Pop!~n"}, {nil, 1}]}]}]}.

% Process escape sequences in strings
process_string_escapes(String) ->
    lists:flatten(process_string_escapes(String, [])).

process_string_escapes([], Acc) ->
    lists:reverse(Acc);
process_string_escapes([$\\, $n | Rest], Acc) ->
    process_string_escapes(Rest, [$\n | Acc]);
process_string_escapes([$\\, $t | Rest], Acc) ->
    process_string_escapes(Rest, [$\t | Acc]);
process_string_escapes([$\\, $r | Rest], Acc) ->
    process_string_escapes(Rest, [$\r | Acc]);
process_string_escapes([$\\, $\\ | Rest], Acc) ->
    process_string_escapes(Rest, [$\\ | Acc]);
process_string_escapes([$\\, $" | Rest], Acc) ->
    process_string_escapes(Rest, [$" | Acc]);
process_string_escapes([C | Rest], Acc) ->
    process_string_escapes(Rest, [C | Acc]).

% Helper function to format the tail of arguments
format_args_tail([]) ->
    {nil, 1};
format_args_tail([Arg | Rest]) ->
    {cons, 1, expr_to_beam(Arg), format_args_tail(Rest)}.
