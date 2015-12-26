%%%-------------------------------------------------------------------
%%% @author dasudian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十二月 2015 下午4:07
%%%-------------------------------------------------------------------
-module(riak_cs_s3_token).
-author("dasudian").

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([verify_token/1]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

% {ok, {AppId, UserId, ExpiryDate, TokenScret}}
verify_token(Token) ->
        decode_token(Token).

decode_token(Token) ->
        try
                {Expire, SecretInfo} = binary_to_term(base64:decode(Token)),
                case Expire >= calendar:local_time() of
                        true ->
                                {AppId, UserId, ClientId, Random, TokenSecret} = decrypt(SecretInfo),
                                {ok, {AppId, UserId, ClientId, Random, TokenSecret}};
                        false ->
                                {error, expired}
                end
        catch
                _:_ ->
                        {error,invalid}
        end.

decrypt(Value) ->
        {ok,AUTH_SALT} = application:get_env(riak_cs,auth_salt),
        {ok,ENC_IV} = application:get_env(riak_cs,enc_iv),
        {ok,ENC_KEY} = application:get_env(riak_cs,enc_key),
        State = crypto:stream_init(aes_ctr, ENC_KEY, ENC_IV),
        {_NewState, VV} = crypto:stream_decrypt(State, Value),
        [V, AUTH_SALT] = binary_to_term(VV),
        V.


