-module(chat_ws_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(_Req, _State) ->
    {cowboy_websocket, _Req, #{}}.

websocket_init(State) ->
    io:format("WebSocket connection established~n"),
    {ok, State}.

websocket_handle({text, Message}, State) ->
    %% Clean up the message
    CleanMessage = re:replace(Message, <<"\r\n">>, <<>>, [global, {return, binary}]),
    CleanMessage = re:replace(CleanMessage, <<"\n">>, <<>>, [global, {return, binary}]),
    io:format("Received Cleaned WebSocket message: ~p~n", [CleanMessage]),

    try
        %% Decode JSON
        MessageMap = jsx:decode(CleanMessage, [return_maps]),
        io:format("Decoded message: ~p~n", [MessageMap]),

        %% Extract JWT token
        Token = maps:get(<<"token">>, MessageMap, undefined),

        %% Verify JWT and extract inner claims map
        case chat_auth:verify_token(Token) of
            {ok, {jose_jwt, ClaimsMap}} ->
                %% Extract claims (override message-provided userId for safety)
                UserId = maps:get(<<"sub">>, ClaimsMap, <<"unknown">>),
                Username = maps:get(<<"username">>, MessageMap, <<"unknown">>),
                Action = maps:get(<<"action">>, MessageMap, undefined),
                ChatRoomId = maps:get(<<"chatRoomId">>, MessageMap, undefined),
                Msg = maps:get(<<"message">>, MessageMap, <<>>),
                ImageUrl = maps:get(<<"imageUrl">>, MessageMap, <<"">>),

                %% Process action
                case Action of
                    <<"join">> when ChatRoomId =/= undefined ->
                        chat_clients:add_client(ChatRoomId, self()),
                        io:format("User ~p joined room ~p~n", [UserId, ChatRoomId]),
                        {reply, {text, <<"User joined the room">>}, State};

                    <<"message">> when ChatRoomId =/= undefined ->
                        TimestampBin = unicode:characters_to_binary(
                            calendar:system_time_to_rfc3339(erlang:system_time(millisecond),
                            [{unit, millisecond}, {offset, 0}])
                        ),
                        chat_storage:save_message(UserId, ChatRoomId, Msg),
                        chat_server:broadcast_message(ChatRoomId, self(), #{
                            message => Msg,
                            userId => UserId,
                            username => Username,
                            imageUrl => ImageUrl,
                            timestamp => TimestampBin
                        }),
                        {reply, {text, <<"Message sent">>}, State};

                    _ ->
                        io:format("Unknown or invalid action: ~p~n", [Action]),
                        {reply, {text, <<"Invalid action or data">>}, State}
                end;

            {error, _Reason} ->
                io:format("Invalid or missing token~n"),
                {reply, {text, <<"Unauthorized: Invalid token">>}, State}
        end
    catch
        _:Reason ->
            io:format("Error processing WebSocket message: ~p~n", [Reason]),
            {reply, {text, <<"Error processing message">>}, State}
    end.

websocket_info({text, Msg}, State) ->
    io:format("Sending broadcast message to client: ~p~n", [Msg]),
    {reply, {text, Msg}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    io:format("WebSocket closed~n"),
    ok.
