-module(chat_storage).
-export([init/0, save_message/3, get_all/0, clear/0, remove_message/4]).

%% Initialize ETS table for storing messages
init() ->
    ets:new(messages, [named_table, ordered_set, public]).

%% Save a message with timestamp, user ID, and chat room ID
save_message(UserId, RoomId, Message) ->
    Timestamp = erlang:system_time(millisecond),
    ets:insert(messages, {Timestamp, UserId, RoomId, Message}).

%% Get all messages stored in the ETS table
get_all() ->
    ets:tab2list(messages).

%% Clear all stored messages
clear() ->
    ets:delete_all_objects(messages).
%% Remove a specific message after successful sync
remove_message(Timestamp, UserId, RoomId, Message) ->
    ets:delete_object(messages, {Timestamp, UserId, RoomId, Message}).

