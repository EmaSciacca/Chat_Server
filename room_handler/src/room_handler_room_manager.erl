-module(room_handler_room_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, handle_request/3]).

%% gen_server callbacks
-export([init/1, handle_call/3]).

-record(room, {name, owner, users}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = #{}, % Initialize State as an empty map
    {ok, State}.


handle_request(Name, "Create", Room) ->
    gen_server:call(?MODULE, {create, Name, Room});
handle_request(Name, "Destroy", Room) ->
    gen_server:call(?MODULE, {destroy, Name, Room});
handle_request(Name, "Join", Room) ->
    gen_server:call(?MODULE, {join, Name, Room});
handle_request(Name, "Leave", Room) ->
    gen_server:call(?MODULE, {leave, Name, Room});
handle_request(Name, "List", undefined) ->
    gen_server:call(?MODULE, list).

handle_call({create, Name, Room}, _From, State) ->
    case maps:is_key(Room, State) of
        true -> 
            % Room already exists
            Response = io_lib:format("Room ~s already exists~n", [Room]),
            % {reply, lists:flatten(Response), maps:put(Room, RoomData, State)}
            % {reply, {error, room_exists}, State};
            {reply, lists:flatten(Response), State};
        false ->
            % Room does not exist, create it
            RoomData = #room{name=Room, owner=Name, users=[Name]},
            io:format("Room created: ~p~n", [RoomData#room.name]),
            io:format("Room owner: ~p~n", [RoomData#room.owner]),
            io:format("Users in room: ~p~n", [RoomData#room.users]),
            Response = io_lib:format("Room ~s created by user ~s~n", [Room, Name]),
            {reply, lists:flatten(Response), maps:put(Room, RoomData, State)}
    end;

handle_call({destroy, Name, Room}, _From, State) ->
    case maps:find(Room, State) of
        {ok, #room{owner=Name}} ->
            % Room exists and is owned by the user
            Response = io_lib:format("Room ~s destroyed by owner ~s~n", [Room, Name]),
            {reply, lists:flatten(Response), maps:remove(Room, State)};
        {ok, #room{owner=_OtherOwner}} ->
            % Room exists but is not owned by the user
            Response = io_lib:format("Room ~s cannot be destroyed by user ~s because it has another owner~n", [Room, Name]),
            {reply, lists:flatten(Response), State};
        error ->
            % Room does not exist
            Response = io_lib:format("Room ~s cannot be destroyed because it does not exist~n", [Room]),
            {reply, lists:flatten(Response), State}
    end;

handle_call({join, Name, Room}, _From, State) ->
    case maps:find(Room, State) of
        {ok, RoomData = #room{users=Users}} ->
            % Check if the user is already in the room
            case lists:member(Name, Users) of
                true ->
                    % User is already in the room
                    Response = io_lib:format("User ~s is already in room ~s~n", [Name, Room]),
                    {reply, lists:flatten(Response), State};
                false ->
                    % User is not in the room, proceed to add them
                    UpdatedRoom = RoomData#room{users=lists:usort([Name | Users])},
                    Response = io_lib:format("User ~s added to room ~s~n", [Name, Room]),
                    {reply, lists:flatten(Response), maps:put(Room, UpdatedRoom, State)}
            end;
        error ->
            % Room does not exist
            Response = io_lib:format("Room ~s does not exist~n", [Room]),
            {reply, lists:flatten(Response), State}
    end;
handle_call({leave, Name, Room}, _From, State) ->
    case maps:find(Room, State) of
        {ok, RoomData = #room{users=Users}} ->
            % Check if the user is in the room
            case lists:member(Name, Users) of
                true ->
                    % User is in the room, proceed to remove them
                    UpdatedUsers = lists:delete(Name, Users),
                    UpdatedRoom = RoomData#room{users=UpdatedUsers},
                    
                    % Check if the room is empty after the user leaves
                    case UpdatedUsers of
                        [] ->
                            % If the room is empty, remove the room entirely
                            Response = io_lib:format("User ~s left room ~s. Room is now empty and removed.~n", [Name, Room]),
                            {reply, lists:flatten(Response), maps:remove(Room, State)};
                        _ ->
                            % Room still has users, update the room state
                            Response = io_lib:format("User ~s left room ~s~n", [Name, Room]),
                            {reply, lists:flatten(Response), maps:put(Room, UpdatedRoom, State)}
                    end;
                false ->
                    % User is not in the room
                    Response = io_lib:format("User ~s is not in room ~s~n", [Name, Room]),
                    {reply, lists:flatten(Response), State}
            end;
        error ->
            % Room does not exist
            Response = io_lib:format("Room ~s does not exist~n", [Room]),
            {reply, lists:flatten(Response), State}
    end;
handle_call(list, _From, State) ->
    % Retrieve the list of rooms from the state
    RoomList = maps:keys(State),
    % Format each room name into "{room}"
    FormattedRooms = lists:map(fun(Room) -> io_lib:format("~s", [Room]) end, RoomList),
    % Join the formatted room names with ", " and add the "List of Rooms: " prefix
    Response = io_lib:format("List of Rooms: ~s~n", [string:join(FormattedRooms, ", ")]),  
    {reply, lists:flatten(Response), State}.


