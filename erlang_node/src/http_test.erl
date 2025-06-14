-module(http_test).
-export([send_test_message/0]).

send_test_message() ->
    %% Prepare your JSON payload
    MessagesJson = <<"[{\"message\":\"hello from Erlang\"}]">>,
    
    %% Perform HTTP POST request
    Result = httpc:request(post,
        {"http://localhost:8080/messages/1/send/2", %% Adjust endpoint clearly as needed
         [],
         "application/json",
         MessagesJson},
        [],
        []),
        
    io:format("HTTP POST Result: ~p~n", [Result]).
