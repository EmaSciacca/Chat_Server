-module(room_handler_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Define the child processes to be supervised
    ServerChild = {room_handler_server,
                   {room_handler_server, start_link, []},
                   permanent, 5000, worker, [room_handler_server]},

    RoomManagerChild = {room_handler_room_manager,
                        {room_handler_room_manager, start_link, []},
                        permanent, 5000, worker, [room_handler_room_manager]},

    % Define the supervisor strategy
    {ok, {{one_for_one, 5, 10}, [ServerChild, RoomManagerChild]}}.
