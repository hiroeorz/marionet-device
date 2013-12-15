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
    application:start(marionet_device).

