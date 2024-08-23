%%%-------------------------------------------------------------------
%% @doc first_release top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(first_release_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
     ChildSpecs = [
        #{id => main,
          start => {first_release_serv, start_link, []}}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
