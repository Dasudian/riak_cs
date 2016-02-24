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

-export([verify_token/2, delete/2]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

% {ok, {AppId, UserId, ExpiryDate, TokenScret}}
verify_token(Token, RD) ->
    decode_token(Token, RD).

decode_token(Token, RD) ->
    try
        PathInfo = wrq:path_info(RD),
        Object = proplists:get_value(object, PathInfo),

        io:format("object :~p ~n", [Object]),

        L = re:split(Object, "%2F"),

        %% 1 deleted upon retrieved the first time, 0 store forever, integer > 0, seconds to be deleted

        %% "0"-public, "1"-open for all apps belong to the same customerid,
        %% "2"-open for all users within this same app, "3"-open for specific user of specific app

        AppID = binary_to_list(lists:nth(2, L)),
        UserID = binary_to_list(lists:nth(3, L)),
        Timeout = binary_to_integer(lists:nth(5, L)),
        Pri = binary_to_integer(lists:nth(6, L)),

        {_Expire, SecretInfo} = binary_to_term(base64:decode(Token)),
%%                case Expire >= calendar:local_time() of
%%                        true ->
%%                                {AppId, UserId, ClientId, Random, TokenSecret} = decrypt(SecretInfo),
%%                                {ok, {AppId, UserId, ClientId, Random, TokenSecret}};
%%                        false ->
%%                                {error, expired}
%%                end
        {AppId, UserId, ClientId, Random, TokenSecret} = decrypt(SecretInfo),

        ok = verify(Pri, AppID, AppId, UserID, UserId),
        Key = re:replace(Object, "%2F", "/", [global, {return, list}]),
        timeout(Timeout, Key),
        io:format("decrypt : ~p ~n", [{AppId, UserId, ClientId, Random, TokenSecret}]),
        {ok, {AppId, UserId, ClientId, Random, TokenSecret}}
    catch
        _:Why ->
            io:format("get error why :~p ~n", [Why]),
            {error, invalid}
    end.

decrypt(Value) ->
    {ok, AUTH_SALT} = application:get_env(riak_cs, auth_salt),
    {ok, ENC_IV} = application:get_env(riak_cs, enc_iv),
    {ok, ENC_KEY} = application:get_env(riak_cs, enc_key),
    State = crypto:stream_init(aes_ctr, ENC_KEY, ENC_IV),
    {_NewState, VV} = crypto:stream_decrypt(State, Value),
    [V, AUTH_SALT] = binary_to_term(VV),
    V.

verify(0, _, _, _, _) ->
    ok;

verify(1, _, _, _, _) ->
    ok;

verify(2, AppID, TAppID, _, _) ->
    if
        AppID == TAppID ->
            ok;
        true ->
            error
    end;

verify(3, AppID, TAppID, UserID, TUserID) ->
    if
        (AppID == TAppID) andalso (UserID == TUserID) ->
            ok;
        true ->
            error
    end.

timeout(-1, Key) ->
    timer:apply_after(1000, riak_cs_s3_token, delete, [<<"cloud-file-bucket">>, list_to_binary(Key)]);
timeout(_, _) -> ok.

delete(Bucket, Key) ->
    io:format("delete : ~p ~n", [Key]),
    {ok, Pid} = riak_cs_riak_client:checkout(request_pool),
    riak_cs_utils:delete_object(Bucket, Key, Pid).
