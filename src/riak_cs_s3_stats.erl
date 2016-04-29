%%%-------------------------------------------------------------------
%%% @author dasudian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 四月 2016 下午3:37
%%%-------------------------------------------------------------------
-module(riak_cs_s3_stats).
-author("dasudian").
-define(REQUEST_POOL, request_pool).
-define(BUCKET_TYPE_NEW_DOWNLOAD_FIVE_MIN, <<"cf_new_download_five_minute">>).

%% API
-export([add_new_download/1]).

add_new_download(App_Id) ->
    lager:debug("===============add_new_download:AppId = ~p~n", [App_Id]),
    erlang:spawn(fun() -> add_new_download_five_min(App_Id) end).

add_new_download_five_min(App_Id) ->
    lager:debug("===============add_new_download_five_min:AppId = ~p~n", [App_Id]),
    Ts = riak_cs_utils:to_binary(create_five_min_ts()),
    Data = case get_new_download_five_min(App_Id,Ts) of
               {ok,Res} ->
                   ResList = riak_cs_utils:from_json(Res),
                   DownloadTimes = proplists:get_value(<<"downloadtimes">>,ResList),
                   New_DownloadTimes = riak_cs_utils:to_integer(DownloadTimes) + 1,
                   [{<<"ts">>,Ts},{<<"downloadtimes">>,riak_cs_utils:to_binary(New_DownloadTimes)}];
               {error,notfound} ->
                   [{<<"ts">>,Ts},{<<"downloadtimes">>,<<"1">>}]
           end,
    save_new_download_five_min(App_Id,Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  internal function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% common function
create_five_min_ts() ->
    Seconds = riak_cs_utils:seconds_now(),
    Rem = Seconds rem 300,
    Seconds - Rem.

%%%%%%%%%%%%%%%%%%%%%%%%%%
get_new_download_five_min(AppId,Ts) ->
    {ok, RcPid} = riak_cs_riak_client:checkout(?REQUEST_POOL),
    {ok, Pid} = riak_cs_riak_client:master_pbc(RcPid),
    fetch_new_download_five_min(Pid, AppId, Ts).

fetch_new_download_five_min(RiakPid,AppId,Ts) ->
    fetch(RiakPid,AppId,Ts,?BUCKET_TYPE_NEW_DOWNLOAD_FIVE_MIN).

fetch(RiakPid,AppId,Key,Bucket_type) when is_binary(AppId),is_binary(Key) ->
    case lib_riak:fetch(RiakPid, {Bucket_type,AppId}, Key) of
        {ok, RiakObj} ->
            UserJson = lib_riak:get_value(RiakObj),
            {ok, UserJson};
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
save_new_download_five_min(AppId,Data) ->
    {ok, RcPid} = riak_cs_riak_client:checkout(?REQUEST_POOL),
    {ok, Pid} = riak_cs_riak_client:master_pbc(RcPid),
    save_new_download_five_min(Pid, AppId, Data).

save_new_download_five_min(RiakPid,AppId,Data) ->
    save_ts(RiakPid,AppId,Data,?BUCKET_TYPE_NEW_DOWNLOAD_FIVE_MIN).

save_ts(RiakPid, AppId, Data, Bucket_type) when is_binary(AppId)->
    Key = proplists:get_value(<<"ts">>,Data),
    save(RiakPid, AppId, Key, Data, Bucket_type).

save(RiakPid, AppId, Key, Data, Bucket_type) when is_binary(AppId);is_binary(Key)->
    lager:debug("=============cf_stats_store:save AppId = ~p, Key = ~p, Data = ~p, Bucket_type = ~p~n", [AppId, Key, Data, Bucket_type]),
    NewRiakObj =
        case lib_riak:fetch(RiakPid, {Bucket_type,AppId}, Key) of
            {ok, RiakObj} ->
                lib_riak:update(RiakObj, riak_cs_utils:to_json(Data));
            {error, notfound} ->
                lib_riak:create({Bucket_type,AppId}, Key, riak_cs_utils:to_json(Data))
        end,
    lib_riak:save(RiakPid, NewRiakObj).







