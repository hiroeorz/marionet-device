%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@hibiscus>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Nov 2013 by HIROE Shin <shin@hibiscus>
%%%-------------------------------------------------------------------
-module(marionet_device).

%% API
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    lager:start(),
    application:load(gpio),
    application:load(arduino),
    application:start(emqttc),
    application:start(serial),
    application:start(omron_fins),
    application:start(ranch),
    application:start(cowlib),
    application:start(crypto),
    application:start(cowboy),
    application:start(marionet_device).
