-module(room_handler_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).

-define(SERVER_PORT, 12345).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Starting room_handler_server~n"),
    case gen_tcp:listen(?SERVER_PORT, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            io:format("Listening on port ~p~n", [?SERVER_PORT]),
            Result = inet:setopts(ListenSocket, [{recbuf, 1024}]),
            io:format("Set options result: ~p~n", [Result]),
            spawn(fun() -> accept_loop(ListenSocket) end),
            {ok, ListenSocket};
        {error, Reason} ->
            io:format("Failed to listen on port ~p: ~p~n", [?SERVER_PORT, Reason]),
            {stop, Reason}
    end.


accept_loop(Socket) ->
    case gen_tcp:accept(Socket) of
        {ok, Client} ->
            spawn(fun() -> handle_client(Client) end),
            accept_loop(Socket);
        {error, closed} ->
            gen_tcp:close(Socket)
    end.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            CleanData = remove_trailing_newlines(Data),
            Response = process_message(CleanData),
            gen_tcp:send(Socket, Response),
            handle_client(Socket);
        {error, closed} ->
            gen_tcp:close(Socket)
    end.

process_message(Data) ->
    %% Convert binary data to list
    Message = binary_to_list(Data),
    %% Debug: Print the message
    io:format("Processing message: ~s~n", [Message]),

    %% Parse the message
    case parse_message(Message) of
        {ok, {Name, Action, Room}} ->
            %% Check if the action is "List" and handle accordingly
            case Action of
                "List" ->
                    %% If Action is "List", Room should be ignored
                    io:format("handling List: ~s~n", [Action]),
                    room_handler_room_manager:handle_request(Name, Action, undefined);
                _ ->
                    %% For other actions, Room should be provided
                    io:format("handling other cases: Name ~s | Action ~s | Room ~s~n", [Name, Action, Room]),
                    room_handler_room_manager:handle_request(Name, Action, Room)
            end;
        error ->
            %% Handle error
            <<"Invalid message format">>
    end.


parse_message(Message) ->
    %% Split the message by "|"
    Parts = re:split(Message, "\\|", [{return, list}]),
    %% Convert the list of parts to a string for logging
    PartsString = lists:flatten(io_lib:format("~p", [Parts])),
    
    %% Log the parts as a string
    io:format("Parsed parts: ~s~n", [PartsString]),

    %% Pattern match based on the number of parts
    case Parts of
        [Name, Action] ->
            io:format("Action: ~s~n", [Action]),
            %% Handle case where Action is "List" and no Room is present
            if
                Action =:= "ACTION:List" ->
                    {ok, {extract_value(Name), extract_value(Action), undefined}};
                true ->
                    error
            end;
        [Name, Action, Room] ->
            %% Handle the case where all three parts are present
            {ok, {extract_value(Name), extract_value(Action), extract_value(Room)}};
        _ ->
            %% Handle invalid format
            error
    end.

extract_value(Pair) ->
    %% Extract value from a "Key:Value" pair
    case string:split(Pair, ":", all) of
        [_, Value] -> Value;
        _ -> undefined
    end.


remove_trailing_newlines(Binary) ->
    %% Helper function to remove trailing newlines from binary
    remove_trailing_newlines(Binary, byte_size(Binary)).

remove_trailing_newlines(Binary, Size) when Size > 0 ->
    %% Check the last byte
    LastByte = binary:part(Binary, {Size - 1, 1}),
    %% If it's a newline or carriage return, remove it
    case LastByte of
        <<10>> -> remove_trailing_newlines(binary:part(Binary, {0, Size - 1}), Size - 1); % \n
        <<13>> -> remove_trailing_newlines(binary:part(Binary, {0, Size - 1}), Size - 1); % \r
        _ -> Binary % No trailing newline or carriage return
    end;
remove_trailing_newlines(Binary, 0) ->
    Binary.
