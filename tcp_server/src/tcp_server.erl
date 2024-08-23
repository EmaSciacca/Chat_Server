-module(tcp_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([stop/0, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PORT, 12345).

-record(state, {listen_socket}).

%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

send(Name, Message) ->
    gen_server:cast(?SERVER, {send, Name, Message}).

%% gen_server callbacks
init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    {ok, #state{listen_socket = ListenSocket}, 0}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

handle_cast({send, Name, Message}, State) ->
    %% This is where you'd send a message to a connected client
    io:format("Sending ~p to ~p~n", [Message, Name]),
    {noreply, State}.

handle_info(timeout, #state{listen_socket = ListenSocket} = State) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_connection(Socket) end),
    {noreply, State, 0};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listen_socket = ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_connection(Socket) ->
    %% Continuously handle messages from the client
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            %% Remove trailing newlines from binary data
            CleanData = remove_trailing_newlines(Data),
            
            %% Check if the data starts with the "NAME:" prefix
            case binary:part(CleanData, {0, 5}) of
                <<"NAME:">> ->
                    %% Extract the name
                    Name = binary:part(CleanData, {5, byte_size(CleanData) - 5}),
                    io:format("Client connected with name: ~s~n", [binary:bin_to_list(Name)]),
                    %% Start the loop to continue handling messages from the client
                    loop(Socket, Name);
                _ ->
                    %% Handle non-name messages
                    io:format("Received data: ~s~n", [binary:bin_to_list(CleanData)]),
                    %% Continue the loop
                    loop(Socket, <<"unknown">>)
            end;
        {error, closed} ->
            io:format("Connection closed~n"),
            ok
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





loop(Socket, Name) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Received from ~s: ~s~n", [Name, Data]),
            loop(Socket, Name);
        {error, closed} ->
            io:format("Connection closed by ~s~n", [Name]),
            ok
    end.
