Nonterminals
    program
    statements
    statement
    expression
    function_def
    param_list
    params
    block
    if_statement
    return_statement
    args
    .

Terminals
    integer float string boolean
    '+' '-' '*' '/' '%' '=' '=='
    '>' '<' '>=' '<='
    '(' ')' '{' '}' ';' ','
    fun_kw if_kw else_kw return_kw
    identifier
    .

Rootsymbol program.

Left 100 '=='.
Left 200 '>' '<' '>=' '<='.
Left 300 '+' '-'.
Left 400 '*' '/' '%'.

program -> statements : '$1'.

statements -> statement : ['$1'].
statements -> statement statements : ['$1'|'$2'].

statement -> expression ';' : {expr, '$1'}.
statement -> function_def : '$1'.
statement -> if_statement : '$1'.
statement -> return_statement : '$1'.

expression -> integer : extract_token('$1').
expression -> float : extract_token('$1').
expression -> string : extract_token('$1').
expression -> boolean : extract_token('$1').
expression -> identifier : extract_token('$1').
expression -> '-' expression : {op, '-', {integer, 0}, '$2'}.
expression -> expression '+' expression : {op, '+', '$1', '$3'}.
expression -> expression '-' expression : {op, '-', '$1', '$3'}.
expression -> expression '*' expression : {op, '*', '$1', '$3'}.
expression -> expression '/' expression : {op, '/', '$1', '$3'}.
expression -> expression '%' expression : {op, '%', '$1', '$3'}.
expression -> expression '==' expression : {op, '==', '$1', '$3'}.
expression -> expression '>' expression : {op, '>', '$1', '$3'}.
expression -> expression '<' expression : {op, '<', '$1', '$3'}.
expression -> expression '>=' expression : {op, '>=', '$1', '$3'}.
expression -> expression '<=' expression : {op, '<=', '$1', '$3'}.
expression -> identifier '=' expression : {assign, extract_token('$1'), '$3'}.
expression -> identifier '(' ')' : {call, extract_token('$1'), []}.
expression -> identifier '(' args ')' : {call, extract_token('$1'), '$3'}.
expression -> '(' expression ')' : '$2'.

function_def -> fun_kw identifier '(' param_list ')' block :
    {function, extract_token('$2'), '$4', '$6'}.

param_list -> '$empty' : [].
param_list -> params : '$1'.

params -> identifier : [extract_token('$1')].
params -> identifier ',' params : [extract_token('$1')|'$3'].

args -> expression : ['$1'].
args -> expression ',' args : ['$1'|'$3'].

block -> '{' statements '}' : '$2'.

if_statement -> if_kw '(' expression ')' block : {if_stmt, '$3', '$5'}.
if_statement -> if_kw '(' expression ')' block else_kw block : {if_else_stmt, '$3', '$5', '$7'}.

return_statement -> return_kw expression ';' : {return_stmt, '$2'}.
return_statement -> return_kw ';' : {return_stmt, nil}.

Erlang code.

extract_token({_Token, _Line, Value}) -> Value;
extract_token({Token, _Line}) -> Token.
