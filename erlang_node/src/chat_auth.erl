-module(chat_auth).
-export([verify_token/1]).

verify_token(TokenBinary) ->
    SecretString = os:getenv("JWT_SECRET"),
    Secret = jose_jwk:from_oct(list_to_binary(SecretString)),
    case jose_jwt:verify(Secret, TokenBinary) of
        {true, JWTMap, _JWS} ->
            io:format(" Verified. Claims: ~p~n", [JWTMap]),
            {ok, JWTMap};
        false ->
            io:format(" Invalid signature or tampered token~n"),
            {error, invalid_signature}
    end.

