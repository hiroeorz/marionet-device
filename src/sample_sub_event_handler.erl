%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(sample_sub_event_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DIGITAL_CODE, 1).
-define(ANALOG_CODE, 2).
-define(SERVO_PINNO, 9).

-record(state, {subs :: list()}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([Subscribes]) ->
    ok = arduino:servo_config(?SERVO_PINNO, 0, 180),
    {ok, #state{subs = Subscribes}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({connack_accept}, State=#state{subs=Subscribes}) ->
    ok = emqttc:subscribe(emqttc, Subscribes),
    io:format("sent subscribe request: ~p~n", [Subscribes]),
    {ok, State};

%% digital(QoS=1)
handle_event({publish, <<"/demo/pi002/digital/0">> = Topic,
	      Payload, 1, MsgId}, State) ->
    lager:info("publish: topic(id:~p):~p~n", [MsgId, Topic]),

    [?DIGITAL_CODE,
     _DeviceId, _PortNo, StateList] = marionet_data:unpack(Payload),

    lager:info("publish: state:~p~n", [StateList]),
    [_, _, _, _, S, _, _, _] = StateList,
    gpio_pin:write(25, S),
    emqttc:puback(emqttc, MsgId),
    {ok, State};

%% digital(QoS=0)
handle_event({publish, <<"/demo/pi002/digital/0">> = Topic,
	      Payload}, State) ->
    lager:info("publish: topic:~p~n", [Topic]),

    [?DIGITAL_CODE, 
     _DeviceId, _PortNo, StateList] = marionet_data:unpack(Payload),

    lager:info("sub: state:~p~n", [StateList]),
    [_, _, _, _, S, _, _, _] = StateList,
    gpio_pin:write(25, S),
    {ok, State};

%% analog(pi001, QoS=0)
handle_event({publish, <<"/demo/pi001/analog/", _/binary>> = Topic,
	      Payload}, State) ->
    [?ANALOG_CODE, DeviceId, PinNo, Val] = marionet_data:unpack(Payload),
    lager:debug("sub: pin=~p val=~p~n(topic:~p)", [PinNo, Val, Topic]),
    control_led(DeviceId, PinNo, Val),
    control_servo(DeviceId, PinNo, Val),
    {ok, State};

%% analog(pi002, QoS=0)
handle_event({publish, <<"/demo/pi002/analog/", _/binary>> = Topic,
	      Payload}, State) ->
    [?ANALOG_CODE, DeviceId, PinNo, Val] = marionet_data:unpack(Payload),
    lager:debug("sub: pin=~p val=~p~n(topic:~p)", [PinNo, Val, Topic]),
    control_led(DeviceId, PinNo, Val),
    control_servo(DeviceId, PinNo, Val),
    {ok, State};

%% other(QoS=0)
handle_event({publish, Topic, Payload}, State) ->
    lager:info("sub: topic:~p~n", [Topic]),
    lager:info("sub: payload:~p~n", [Payload]),
    {ok, State};

%% other(QoS=1)
handle_event({publish, Topic, Payload, 1, MsgId}, State) ->
    lager:info("unknown topic received."),
    lager:info("sub: topic(id:~p):~p~n", [MsgId, Topic]),
    lager:info("sub: payload:~p~n", [Payload]),
    emqttc:puback(emqttc, MsgId),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

control_led(2, 0, Val) when Val > 512 ->
    gpio_pin:write(24, 1);

control_led(2, 0, Val) when Val =< 512 ->
    gpio_pin:write(24, 0);

control_led(_DeviceId, _PinNo, _Val) ->
    ok.

control_servo(<<"pi002">>, 0, Val) ->
    Angle = Val * 180 div 1024,
    arduino:analog_write(?SERVO_PINNO, Angle);

control_servo(_DeviceId, _PinNo, _Val) ->
    ok.
