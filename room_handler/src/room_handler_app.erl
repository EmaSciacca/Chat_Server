%%%-------------------------------------------------------------------
%% @doc room_handler public API
%% @end
%%%-------------------------------------------------------------------

-module(room_handler_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    room_handler_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
