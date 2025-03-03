%%%-------------------------------------------------------------------
%% @doc pop public API
%% @end
%%%-------------------------------------------------------------------

-module(pop_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pop_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
