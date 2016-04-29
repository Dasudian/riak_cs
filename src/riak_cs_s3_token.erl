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
-define(BucketTypeForDownloadTimes, <<"dsd_cf_downloadtimes">>).
-define(BucketNameForDownloadTimes, <<"dsd_cf_downloadtimes">>).
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

        lager:debug("object :~p ~n", [Object]),

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
        %% verify this file's status from CloudFile Server
        {ok, _} = verify_file_status(Key),
        %% update stats
        riak_cs_s3_stats:add_new_download(riak_cs_utils:to_binary(AppId)),
        %% verify success, then counter +1,means this file's download times plus 1;
        %% BucketName:files_info; Key:file's path; Value:[{download_times, 100}]
        {ok, RcPid} = riak_cs_riak_client:checkout(request_pool),
        {ok, Pid} = riak_cs_riak_client:master_pbc(RcPid),
        Result = download_times_plus1(Pid, ?BucketTypeForDownloadTimes, ?BucketNameForDownloadTimes, riak_cs_utils:to_binary(Key)),
        case Result of
            ok ->
                lager:debug("download_times plus 1 success!~n");
            Reason ->
                lager:error("download_times plus 1 fail! Reason = ~p~n", [Reason])
        end,
        io:format("decrypt : ~p ~n", [{AppId, UserId, ClientId, Random, TokenSecret}]),
        {ok, {AppId, UserId, ClientId, Random, TokenSecret}}
    catch
        _:Why ->
            lager:error("get error why :~p ~n", [Why]),
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
    lager:debug("delete : ~p ~n", [Key]),
    {ok, Pid} = riak_cs_riak_client:checkout(request_pool),
    riak_cs_utils:delete_object(Bucket, Key, Pid).

verify_file_status(Key) ->
    Data = riak_cs_utils:to_json([{<<"key">>, Key}]),
    {ok, Cf_host} = application:get_env(riak_cs, cf_host),
    Url = Cf_host ++ "/get_file_status",
    Status =
    case httpc_client:post1(Url, Data) of
        {ok, Res} ->
            Response = riak_cs_utils:from_json(riak_cs_utils:to_binary(Res)),
            lager:debug("Request CF Server Response = ~p", [Response]),
            Result = proplists:get_value(<<"result">>,Response),
            case riak_cs_utils:to_binary(Result) =:= <<"success">> of
                true ->
                    proplists:get_value(<<"status">>,Response);
                _->
                    lager:error("request error ~n"),
                    2
            end;
        {error, Reason} ->
            lager:error("request error:~p~n",[Reason]),
            2
    end,
    lager:debug("verify file status Status = ~p", [Status]),
    case riak_cs_utils:to_integer(Status) of
        0 ->
            {ok, 0};
        1 ->
            lager:warning("File was locked, can not download!"),
            {warning, 1};
        Other ->
            lager:error("Request Error"),
            {error, Other}
    end.

download_times_plus1(Pid, BucketType, BucketName, Key) ->
    lager:debug("download_times_plus1:Pid = ~p,BucketType = ~p,BucketName = ~p,Key = ~p~n", [Pid, BucketType, BucketName, Key]),
    NewObj =
    case riakc_pb_socket:get(Pid, {BucketType, BucketName}, Key) of
        {ok, Obj} ->
            update(Obj);
        {error, notfound} ->
            create(BucketType, BucketName, Key);
        R ->
            lager:error("Error:DownloadTimesPlus1,Reason:~p~n", [R])
    end,
    riakc_pb_socket:put(Pid, NewObj).

update(Obj) ->
    io:format("update~n"),
    OldContentJson = riakc_obj:get_value(Obj),
    OldContent = riak_cs_utils:from_json(OldContentJson),
    Key = proplists:get_value(<<"key">>, OldContent),
    DownloadTimes = proplists:get_value(<<"downloadtimes">>, OldContent),
    NewContent = [{key, Key}, {downloadtimes, DownloadTimes + 1}],
    NewContentJson = riak_cs_utils:to_json(NewContent),
    %NewValue = erlang:binary_to_term(Value) + 1,
    riakc_obj:update_value(Obj, NewContentJson).

create(BucketType, BucketName, Key) ->
    io:format("create~n"),
    Content = [{<<"key">>, Key}, {<<"downloadtimes">>, 1}],
    ContentJson = riak_cs_utils:to_json(Content),
    riakc_obj:new({BucketType, BucketName}, Key, ContentJson, "application/json").