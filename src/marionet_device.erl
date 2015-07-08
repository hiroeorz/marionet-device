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
-export([start/0,
	 all_digital/0,
	 digital_multi_value/1,
	 all_analog/0,
	 analog_multi_value/1]).

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
    application:start(erlzmq),
    application:start(marionet_device).

%%--------------------------------------------------------------------
%% @doc get all digital state.
%% @end
%%--------------------------------------------------------------------
-spec all_digital() -> [0 | 1].
all_digital() ->
    marionet_device_status:all_digital().

%%--------------------------------------------------------------------
%% @doc get digital value list, from digital no list.
%% @end
%%--------------------------------------------------------------------
-spec digital_multi_value([non_neg_integer()]) -> [0 | 1].
digital_multi_value(DigitalNoList) ->
    marionet_device_status:digital_multi_value(DigitalNoList).

%%--------------------------------------------------------------------
%% @doc get all digital state.
%% @end
%%--------------------------------------------------------------------
-spec all_analog() -> [non_neg_integer()].
all_analog() ->
    marionet_device_status:all_analog().

%%--------------------------------------------------------------------
%% @doc get analog value list, from analog no list.
%% @end
%%--------------------------------------------------------------------
-spec analog_multi_value([non_neg_integer()]) -> [non_neg_integer()].
analog_multi_value(AnalogNoList) ->
    marionet_device_status:analog_multi_value(AnalogNoList).
