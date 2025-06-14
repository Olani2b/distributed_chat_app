-module(chat_clients).
-export([init/0, add_client/2, remove_client/1, get_clients/1]).

%% Initialize ETS tables for connected clients and client-room mapping
init() ->
    ets:new(clients, [named_table, bag, public]),
    ets:new(client_rooms, [named_table, set, public]).

%% Add a new client socket to a specific chat room
add_client(RoomId, Socket) ->
    ets:insert(clients, {RoomId, Socket}),
    ets:insert(client_rooms, {Socket, RoomId}).

%% Remove a disconnected client socket from any chat room
remove_client(Socket) ->
    case ets:lookup(client_rooms, Socket) of
        [{Socket, RoomId}] ->
            ets:delete(clients, {RoomId, Socket}),
            ets:delete(client_rooms, Socket),
            io:format("Client removed from room ~p: ~p~n", [RoomId, Socket]);
        [] -> ok
    end.

%% Get all clients in a specific chat room
get_clients(RoomId) ->
    [Socket || {StoredRoomId, Socket} <- ets:tab2list(clients), StoredRoomId =:= RoomId].

