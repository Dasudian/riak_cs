%%%-------------------------------------------------------------------
%%% @author dasudian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 十一月 2015 下午7:16
%%%-------------------------------------------------------------------
-module(httpc_client).
-author("dasudian").

-define(WEB_TIMEOUT, 1000 * 20).

%% API
-export([post/2,
		post1/2]).


post(Url, Data) ->
 	httpc:set_options([{cookies, enabled}]),
	post_request(Url, Data,[{"Accept","application/json"}]).

post1(Url, Data) ->
	httpc:set_options([{cookies, enabled}]),
	post_request(Url, Data,[]).

post_request(Url, Data, Opt) ->
	Method = post,
	Request = {Url, Opt, "application/json", Data},
	HTTPOptions = [{ssl, [{verify, 0}]}, {timeout, ?WEB_TIMEOUT}],
	Options = [],
	http_request(Method, Request, HTTPOptions, Options).


http_request(Method, Request, HTTPOptions, Options) ->
	try
		case httpc:request(Method,Request ,HTTPOptions,Options) of
			{ok, {Status_line, Headers, Body}} ->
				handle_result(Status_line, Headers, Body);
			{error, Reason} ->
				{error, Reason}
		end
	catch
		Other_Reason ->
			{error, Other_Reason}
	end.

handle_result(Status_line, _Headers, Body) ->
	{_, Code, _} = Status_line,
	case Code of
		200 ->
			{ok,Body};
		_->
			{error,riak_cs_utils:to_binary(Code)}
	end.
