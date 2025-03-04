Definitions.

D = [0-9]
L = [a-zA-Z_]
WS = [\s\t\n\r]
COMMENT = \/\/[^\n]*
DOC_COMMENT = \/\*\*[\s\S]*?\*\/
MODULE_COMMENT = \/\*\*[\s\S]*?\*\/

Rules.

{D}+                   : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{D}+\.{D}+             : {token, {float, TokenLine, list_to_float(TokenChars)}}.
\"[^\"]*\"             : {token, {string, TokenLine, strip_quotes(TokenChars)}}.
\+                     : {token, {'+', TokenLine}}.
\-                     : {token, {'-', TokenLine}}.
\*                     : {token, {'*', TokenLine}}.
\/                     : {token, {'/', TokenLine}}.
\%                     : {token, {'%', TokenLine}}.
=                      : {token, {'=', TokenLine}}.
==                     : {token, {'==', TokenLine}}.
!=                     : {token, {'!=', TokenLine}}.
>                      : {token, {'>', TokenLine}}.
<                      : {token, {'<', TokenLine}}.
>=                     : {token, {'>=', TokenLine}}.
<=                     : {token, {'<=', TokenLine}}.
\(                     : {token, {'(', TokenLine}}.
\)                     : {token, {')', TokenLine}}.
{                      : {token, {'{', TokenLine}}.
}                      : {token, {'}', TokenLine}}.
;                      : {token, {';', TokenLine}}.
,                      : {token, {',', TokenLine}}.
fun                    : {token, {fun_kw, TokenLine}}.
if                     : {token, {if_kw, TokenLine}}.
else                   : {token, {else_kw, TokenLine}}.
return                 : {token, {return_kw, TokenLine}}.
true                   : {token, {boolean, TokenLine, true}}.
false                  : {token, {boolean, TokenLine, false}}.
{L}+                   : {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
{COMMENT}              : skip_token.
{DOC_COMMENT}          : skip_token.
{MODULE_COMMENT}       : skip_token.
{WS}+                  : skip_token.

Erlang code.

strip_quotes(Chars) ->
    lists:sublist(Chars, 2, length(Chars) - 2).
