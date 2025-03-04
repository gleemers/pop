-module(pop_semantic_analyzer).
-export([analyze/1]).

%% Analyze the AST for semantic errors
analyze(AST) ->
    % Initialize environment with built-in functions
    Env = init_environment(),
    
    % Analyze each top-level function
    {NewEnv, Errors, Warnings} = analyze_functions(AST, Env, [], []),
    
    % Check for unused variables in the final environment
    UnusedWarnings = check_unused_variables(NewEnv),
    
    % Return all errors and warnings
    {Errors, Warnings ++ UnusedWarnings}.

%% Initialize environment with built-in functions
init_environment() ->
    BuiltIns = [
        {print, 1},
        {println, 1},
        {println, 2},
        {io_format, 1},
        {io_format, 2},
        {io_format, 3}
    ],
    lists:foldl(fun({Name, Arity}, Acc) ->
        add_function(Name, Arity, Acc)
    end, #{}, BuiltIns).

%% Analyze all functions in the AST
analyze_functions([], Env, Errors, Warnings) ->
    {Env, Errors, Warnings};
analyze_functions([{function, Name, Params, Body} | Rest], Env, Errors, Warnings) ->
    % Add function to environment
    Env1 = add_function(Name, length(Params), Env),
    
    % Create a new scope for function body with parameters
    FuncEnv = lists:foldl(fun(Param, Acc) ->
        add_variable(Param, Acc)
    end, Env1, Params),
    
    % Analyze function body
    {_, BodyErrors, BodyWarnings} = analyze_statements(Body, FuncEnv, [], []),
    
    % Continue with next function
    analyze_functions(Rest, Env1, Errors ++ BodyErrors, Warnings ++ BodyWarnings);
analyze_functions([_ | Rest], Env, Errors, Warnings) ->
    analyze_functions(Rest, Env, Errors, Warnings).

%% Analyze a list of statements
analyze_statements([], Env, Errors, Warnings) ->
    {Env, Errors, Warnings};
analyze_statements([Statement | Rest], Env, Errors, Warnings) ->
    {NewEnv, StmtErrors, StmtWarnings} = analyze_statement(Statement, Env, [], []),
    analyze_statements(Rest, NewEnv, Errors ++ StmtErrors, Warnings ++ StmtWarnings).

%% Analyze a single statement
analyze_statement({expr, Expr}, Env, Errors, Warnings) ->
    analyze_expression(Expr, Env, Errors, Warnings);
analyze_statement({if_stmt, Condition, ThenBlock}, Env, Errors, Warnings) ->
    % Analyze condition
    {Env1, CondErrors, CondWarnings} = analyze_expression(Condition, Env, [], []),
    
    % Check for constant conditions
    ConstWarnings = check_constant_condition(Condition, CondWarnings),
    
    % Analyze then block
    {Env2, ThenErrors, ThenWarnings} = analyze_statements(ThenBlock, Env1, [], []),
    
    % Combine errors and warnings
    {Env2, Errors ++ CondErrors ++ ThenErrors, Warnings ++ ConstWarnings ++ ThenWarnings};
analyze_statement({if_else_stmt, Condition, ThenBlock, ElseBlock}, Env, Errors, Warnings) ->
    % Analyze condition
    {Env1, CondErrors, CondWarnings} = analyze_expression(Condition, Env, [], []),
    
    % Check for constant conditions
    ConstWarnings = check_constant_condition(Condition, CondWarnings),
    
    % Analyze then block
    {Env2, ThenErrors, ThenWarnings} = analyze_statements(ThenBlock, Env1, [], []),
    
    % Analyze else block
    {Env3, ElseErrors, ElseWarnings} = analyze_statements(ElseBlock, Env2, [], []),
    
    % Combine errors and warnings
    {Env3, Errors ++ CondErrors ++ ThenErrors ++ ElseErrors, 
          Warnings ++ ConstWarnings ++ ThenWarnings ++ ElseWarnings};
analyze_statement({return_stmt, nil}, Env, Errors, Warnings) ->
    {Env, Errors, Warnings};
analyze_statement({return_stmt, Value}, Env, Errors, Warnings) ->
    analyze_expression(Value, Env, Errors, Warnings);
analyze_statement(Other, Env, Errors, Warnings) ->
    % For any other statement type, try to analyze as expression
    analyze_expression(Other, Env, Errors, Warnings).

%% Analyze an expression
analyze_expression({op, _, Left, Right}, Env, Errors, Warnings) ->
    % Analyze left operand
    {Env1, LeftErrors, LeftWarnings} = analyze_expression(Left, Env, [], []),
    
    % Analyze right operand
    {Env2, RightErrors, RightWarnings} = analyze_expression(Right, Env1, [], []),
    
    % Combine errors and warnings
    {Env2, Errors ++ LeftErrors ++ RightErrors, Warnings ++ LeftWarnings ++ RightWarnings};
analyze_expression({assign, Name, Value}, Env, Errors, Warnings) ->
    % Analyze the value being assigned
    {Env1, ValueErrors, ValueWarnings} = analyze_expression(Value, Env, [], []),
    
    % Add or update variable in environment
    NewEnv = add_variable(Name, Env1),
    
    % Return updated environment and any errors from value
    {NewEnv, Errors ++ ValueErrors, Warnings ++ ValueWarnings};
analyze_expression({call, Name, Args}, Env, Errors, Warnings) ->
    % Check if function exists
    case is_function_defined(Name, length(Args), Env) of
        true ->
            % Analyze each argument
            {NewEnv, ArgErrors, ArgWarnings} = analyze_args(Args, Env, [], []),
            {NewEnv, Errors ++ ArgErrors, Warnings ++ ArgWarnings};
        false ->
            % Function not defined, add error
            FuncError = {error, {undefined_function, Name, length(Args)}},
            {Env, Errors ++ [FuncError], Warnings}
    end;
analyze_expression(Value, Env, Errors, Warnings) when is_atom(Value) ->
    % Check if variable is defined
    case is_variable_defined(Value, Env) of
        true ->
            % Mark variable as used
            NewEnv = mark_variable_used(Value, Env),
            {NewEnv, Errors, Warnings};
        false ->
            % Variable not defined, add error
            VarError = {error, {undefined_variable, Value}},
            {Env, Errors ++ [VarError], Warnings}
    end;
analyze_expression(_Value, Env, Errors, Warnings) ->
    % Literals (integers, floats, strings, etc.) are always valid
    {Env, Errors, Warnings}.

%% Analyze function arguments
analyze_args([], Env, Errors, Warnings) ->
    {Env, Errors, Warnings};
analyze_args([Arg | Rest], Env, Errors, Warnings) ->
    {Env1, ArgErrors, ArgWarnings} = analyze_expression(Arg, Env, [], []),
    analyze_args(Rest, Env1, Errors ++ ArgErrors, Warnings ++ ArgWarnings).

%% Check for constant conditions (e.g., if (4 == 5) { ... })
check_constant_condition({op, '==', Left, Right}, Warnings) when is_integer(Left), is_integer(Right) ->
    case Left == Right of
        true -> Warnings;
        false -> [{warning, {constant_condition, false, {op, '==', Left, Right}}} | Warnings]
    end;
check_constant_condition({op, '!=', Left, Right}, Warnings) when is_integer(Left), is_integer(Right) ->
    case Left /= Right of
        true -> Warnings;
        false -> [{warning, {constant_condition, false, {op, '!=', Left, Right}}} | Warnings]
    end;
check_constant_condition({op, '>', Left, Right}, Warnings) when is_integer(Left), is_integer(Right) ->
    case Left > Right of
        true -> Warnings;
        false -> [{warning, {constant_condition, false, {op, '>', Left, Right}}} | Warnings]
    end;
check_constant_condition({op, '<', Left, Right}, Warnings) when is_integer(Left), is_integer(Right) ->
    case Left < Right of
        true -> Warnings;
        false -> [{warning, {constant_condition, false, {op, '<', Left, Right}}} | Warnings]
    end;
check_constant_condition({op, '>=', Left, Right}, Warnings) when is_integer(Left), is_integer(Right) ->
    case Left >= Right of
        true -> Warnings;
        false -> [{warning, {constant_condition, false, {op, '>=', Left, Right}}} | Warnings]
    end;
check_constant_condition({op, '<=', Left, Right}, Warnings) when is_integer(Left), is_integer(Right) ->
    case Left =< Right of
        true -> Warnings;
        false -> [{warning, {constant_condition, false, {op, '<=', Left, Right}}} | Warnings]
    end;
check_constant_condition(_, Warnings) ->
    Warnings.

%% Check for unused variables in the environment
check_unused_variables(Env) ->
    maps:fold(fun
        (Name, {variable, false}, Acc) ->
            [{warning, {unused_variable, Name}} | Acc];
        (_, _, Acc) ->
            Acc
    end, [], Env).

%% Environment management functions

% Add a function to the environment
add_function(Name, Arity, Env) ->
    maps:put({Name, Arity}, function, Env).

% Check if a function is defined
is_function_defined(Name, Arity, Env) ->
    maps:is_key({Name, Arity}, Env).

% Add a variable to the environment (initially unused)
add_variable(Name, Env) ->
    maps:put(Name, {variable, false}, Env).

% Check if a variable is defined
is_variable_defined(Name, Env) ->
    maps:is_key(Name, Env).

% Mark a variable as used
mark_variable_used(Name, Env) ->
    case maps:get(Name, Env, undefined) of
        {variable, _} ->
            maps:put(Name, {variable, true}, Env);
        _ ->
            Env
    end.
