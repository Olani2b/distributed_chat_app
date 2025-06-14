%%%-------------------------------------------------------------------
%% @doc distributed_chat public API
%% @end
%%%-------------------------------------------------------------------

-module(distributed_chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    distributed_chat_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
