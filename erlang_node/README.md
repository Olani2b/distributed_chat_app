distributed_chat
=====

An OTP application

Build
-----

    $ rebar3 compile
    $ spawn(fun() -> chat_server:start(4000) end).

------
telnet localhost 4000
chat_server:start_ws(4000).
export $(cat .env | xargs) && rebar3 shell


