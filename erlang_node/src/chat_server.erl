-module(chat_server).
-export([start_ws/1, broadcast_message/3]).

-import(chat_clients, [add_client/2, remove_client/2, get_clients/1]).

%% Start the WebSocket Server
start_ws(Port) ->
    chat_clients:init(),
    chat_storage:init(),

    %% Start Ranch and Cowboy explicitly
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowboy),
    chat_sync:start_sync(10000),

    io:format("Starting Cowboy WebSocket server on port ~p...~n", [Port]),
    {ok, _} = cowboy:start_clear(chat_server, [{port, Port}], #{
        env => #{dispatch => cowboy_router:compile([
            {'_', [
                {"/websocket", chat_ws_handler, []}
            ]}
        ])}
    }),
    io:format("Cowboy WebSocket server started on port ~p~n", [Port]).
    
%% Broadcast message to all clients in the same chat room
broadcast_message(ChatRoomId, _SenderPid, MsgMap) ->
    Clients = chat_clients:get_clients(ChatRoomId),
    FormattedMessage = jsx:encode(MsgMap),
    lists:foreach(fun(ClientPid) ->
        ClientPid ! {text, FormattedMessage}
    end, Clients).

