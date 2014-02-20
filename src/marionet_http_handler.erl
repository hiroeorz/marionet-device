%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 19 Feb 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_http_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([get_json/2]).
-export([update_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[ {<<"application/json">>, get_json} ], Req, State}.

content_types_accepted(Req, State) ->
    {[ {<<"application/json">>, update_json} ], Req, State}.

get_json(Req, State) ->
    Body = <<"{\"device_id\":\"pi001\",
               \"group_id\":\"demo\",
               \"mqtt\":{\"host\":\"test.mosquito.org\",
               \"port\":1883,\"client_id\":\"demo/pi001\"},
               \"subscribes\":[\"/demo/pi002/analog/#\",
                               \"/demo/pi002/digital/#\"]}">>,
    {Body, Req, State}.


update_json(Req, State) ->
    {true, Req, State}.


