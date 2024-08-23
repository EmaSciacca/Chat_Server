%%%-------------------------------------------------------------------
%% @doc first_release public API
%% @end
%%%-------------------------------------------------------------------

-module(first_release_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    first_release_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
