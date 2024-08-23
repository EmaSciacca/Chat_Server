%%%-------------------------------------------------------------------
%% @doc tcp_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Server = {tcp_server,
              {tcp_server, start_link, []},
              permanent,
              5000,
              worker,
              [tcp_server]},
    {ok, {{one_for_one, 5, 10}, [Server]}}.


%% internal functions
