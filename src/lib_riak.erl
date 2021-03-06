%% @author Barco You <barco@dasudian.com>
%% @copyright 2015 Dasudian
%%
-module(lib_riak).

-define(INDEX_KEY, <<"index">>).
-define(INDEX_SUFFIX_INT, <<"_int">>).
-define(INDEX_SUFFIX_BIN, <<"_bin">>).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    connect/1,
    create/3,
    create/4,
    fetch/3,
    delete/3,
    update/2,
    update/3,
    get_value/1,
    save/2,
    get_index/3,
    set_index/4,
    set_indexes/2,
    index/2,
    new_key/0,
    search/4
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @spec connect(connection_info()) -> pid()
%% @doc Create a connection to the specified Riak cluster and
%%      return the Pid associated with the new connection.
connect({IP, Port}) ->
    {ok, RiakPid} = riakc_pb_socket:start_link(IP, Port),
    RiakPid.

%% @spec create(binary, binary, json) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      assumes that the data passed in is JSON and sets
%%      the MIME type to "application/json" for you.
create(Bucket, Key, JsonData) ->
    create(Bucket, Key, JsonData, "application/json").

%% @spec create(binary, binary, term(), string) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      takes arbitrary data and requires the user to
%%      specify the mime type of the data that is being
%%      stored.
create(Bucket, Key, Item, MimeType) ->
    RiakObj = riakc_obj:new(Bucket, Key, Item, MimeType),
    RiakObj.

%% @spec fetch(pid(), binary, binary) -> riakc_obj()
%% @doc Fetches a riakc object from a Riak node/cluster
%%      using the connection given.
fetch(RiakPid, Bucket, Key) ->
    RiakObj = riakc_pb_socket:get(RiakPid, Bucket, Key),
    RiakObj.

%% @spec delete(pid(), binary, binary) -> riakc_obj()
%% @doc Deletes a riakc object from a Riak node/cluster
%%      using the connection given.
delete(RiakPid, Bucket, Key) ->
    ok = riakc_pb_socket:delete(RiakPid, Bucket, Key).

%% @spec update(riakc_obj(), term()) -> riakc_obj()
%% @doc Updates the stored value for a riakc object with
%%      the new one specified.
update(RiakObj, NewValue) ->
    NewRiakObj = riakc_obj:update_value(RiakObj, NewValue),
    NewRiakObj.

%% @doc Set the updated content-type of an object to CT.
%% -spec update_content_type(riakc_obj(),list()|binary()) -> riakc_obj().
update(RiakObj, NewValue, CT) ->
    riakc_obj:update_value(RiakObj, NewValue, CT).

%% @doc Adds an index of the given type and name to the object's
%%      metadata and returns the updated object.
set_index(RiakObj, Type, Name, Value) ->
    Meta = riakc_obj:get_update_metadata(RiakObj),
    Index = case dict:find(?INDEX_KEY, Meta) of
                error -> [];
                {ok, I} -> I
            end,
    NewIndex = dict:to_list(dict:store(index(Type, Name), value(Value), dict:from_list(Index))),
    riakc_obj:update_metadata(RiakObj, dict:store(?INDEX_KEY, NewIndex, Meta)).

%% @doc Adds indexes of the given types and names to the object's
%%      metadata and returns the updated object.
set_indexes(RiakObj, Indexes) ->
    Meta = riakc_obj:get_update_metadata(RiakObj),
    Index = case dict:find(?INDEX_KEY, Meta) of
                error -> [];
                {ok, I} -> I
            end,
    UpdatedIndexes = lists:foldl(fun({T, N, V}, I) ->
        dict:store(index(T, N), value(V), I)
                                 end,
        dict:from_list(Index), Indexes),
    NewIndex = dict:to_list(UpdatedIndexes),
    riakc_obj:update_metadata(RiakObj, dict:store(?INDEX_KEY, NewIndex, Meta)).

%% @spec get_index_int(riakc_obj(), (int|bin), string) -> (int|bin)
%% @doc Queries the object meta data to pull out an index of
%%      integer type. Assumes that the index exists, expect
%%      failure when querying when metadata/index missing.
get_index(RiakObj, Type, Name) ->
    Meta = riakc_obj:get_metadata(RiakObj),
    Indexes = dict:fetch(?INDEX_KEY, Meta),
    IndexKey = index(Type, Name),
    Value = binary_to_list(proplists:get_value(IndexKey, Indexes)),
    case Type of
        int -> list_to_integer(Value);
        bin -> Value
    end.

%% @spec get_value(riakc_obj()) -> term()
%% @doc Retrieves the stored value from within the riakc
%%      object.
get_value(RiakObj) ->
    Value = riakc_obj:get_value(RiakObj),
    Value.

%% @spec save(pid(), riakc_obj()) -> {ok, riakc_obj()} | {error | Reason}
%% @doc Saves the given riak object to the specified Riak node/cluster.
save(RiakPid, RiakObj) ->
    Result = riakc_pb_socket:put(RiakPid, RiakObj),
    Result.

%% @spec new_key() -> key()
%% @doc Generate an close-to-unique key that can be used to identify
%%      an object in riak. We now make use of Flake to do this for us.
new_key() ->
    %% Base-62 key gives us keys with 0-9, a-Z and A-Z
    {ok, Key} = flake_server:id(62),
    list_to_binary(Key).

%% @doc Create an index of a given name based on the type.
index(int, Name) ->
    iolist_to_binary([Name, ?INDEX_SUFFIX_INT]);
index(bin, Name) ->
    iolist_to_binary([Name, ?INDEX_SUFFIX_BIN]).

%% @doc Perform a search on a given index with specified options.
search(RiakPid, Index, Search, Opts) ->
    Result = riakc_pb_socket:search(RiakPid, Index, Search, Opts),
    Result.

%% ------------------------------------------------------------------
%% Private Function Definitions
%% ------------------------------------------------------------------

value(V) when is_list(V) ->
    list_to_binary(V);
value(V) ->
    V.


