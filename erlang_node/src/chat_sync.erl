-module(chat_sync).
-export([start_sync/1]).


start_sync(Interval) ->
    inets:start(),
    spawn(fun() -> sync_loop(Interval) end).

sync_loop(Interval) ->
    timer:sleep(Interval),
    Messages = chat_storage:get_all(),
    io:format("Attempting to sync messages: ~p~n", [Messages]),
    case Messages of
        [] ->
            io:format("No messages to sync.~n"),
            ok;
        _ ->
            %% Iterate over each message and sync individually
            lists:foreach(fun({Timestamp, UserId, ChatRoomId, Message}) ->
                %% Prepare JSON for a single message
                ISOTime = unicode:characters_to_binary(
    calendar:system_time_to_rfc3339(
        erlang:convert_time_unit(Timestamp, millisecond, native),
        [{unit, native}, {offset, 0}]
    )
),
SingleMessage = jsx:encode([#{
    userId => UserId,
    chatRoomId => ChatRoomId,
    message => unicode:characters_to_binary(Message, utf8),
    timestamp => ISOTime
}]),


                Url = "http://localhost:8080/messages/save",
                RawApiKey = os:getenv("API_KEY"),
                ApiKey = string:trim(RawApiKey),
                Headers = [
                    {"Content-Type", "application/json"},
                    {"X-API-Key", ApiKey}
                ],


                %% Send single message
                case httpc:request(post, {Url, Headers, "application/json", SingleMessage}, [], []) of
                    {ok, {{_,200,_}, _, Body}} ->
                        io:format("Message synced successfully: ~p~n", [Body]),
                        %% Remove the message from ETS upon successful sync
                        chat_storage:remove_message(Timestamp, UserId, ChatRoomId, Message);
                    {ok, {{_,Status,_}, _, Body}} ->
                        io:format("Sync failed with status ~p: ~p~nMessage: ~p~n", [Status, Body, SingleMessage]);
                    {error, Reason} ->
                        io:format("Sync error: ~p~nMessage: ~p~n", [Reason, SingleMessage])
                end
            end, Messages)
    end,
    sync_loop(Interval).


% Convert ETS messages to JSON format
encode_messages(Messages) ->
    MessagesList = [#{
        userId => UserId,
        chatRoomId => ChatRoomId,
        message => unicode:characters_to_binary(Message, utf8),  %%  FIX: Ensure proper string encoding
        timestamp => Timestamp
    } || {Timestamp, UserId, ChatRoomId, Message} <- Messages],
    jsx:encode(MessagesList).
% encode_messages(Messages) ->
%     MessagesList = [#{
%         userId => UserId,
%         chatRoomId => ChatRoomId,
%         message => binary_to_list(Message),  %% Convert binary message to string correctly
%         timestamp => Timestamp
%     } || {Timestamp, UserId, ChatRoomId, Message} <- Messages],

%     jsx:encode(MessagesList).  %%  This ensures JSON is correctly formatted as a list `[ {...}, {...} ]`



