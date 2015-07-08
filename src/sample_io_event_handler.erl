%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Dec 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(sample_io_event_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DIGITAL_CODE, 1).
-define(ANALOG_CODE, 2).
-define(ANALOG_CHANGE_LIMIT_FOR_SEND, 5).  %% 0 - 1023

-record(state, {group_id                        :: binary(),
		device_id                       :: binary(),
		analog_sent_time = dict:new()   :: dict:dict(),
		analog_before_vals = dict:new() :: dict:dict(),
		analog_pub_interval = 3000      :: pos_integer() }).

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
init([GroupId, DeviceId, AnalogPubInterval] = Args) ->
    lager:info("Start I/O event handler: ~p, ~p", [?MODULE, Args]),
    {ok, #state{group_id = GroupId, 
		device_id = DeviceId, 
		analog_pub_interval = AnalogPubInterval}}.

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

%% receive digital port(8bit) changed message.
handle_event({digital_port_changed, PortNo, Status},
	     State=#state{device_id = DeviceId, group_id = GroupId}) ->
    lager:info("digital sent mqtt broker(port:~w): ~p", [PortNo, Status]),
    Payload = marionet_data:pack_io(<<"di">>, DeviceId, PortNo, Status, []),
    Topic = topic(GroupId, DeviceId, <<"digital">>, PortNo),
    lager:debug("Send Topic  : ~p", [Topic]),
    lager:debug("Send Payload: ~p", [Payload]),
    maybe_publish(Topic, Payload, [{qos, 0}, {retain, true}]),
    {ok, State};

%% send analog every 3sec.
handle_event({analog_recv, PinNo, Val},
	     State=#state{device_id=DeviceId, group_id = GroupId, 
			  analog_sent_time = SentTimes,
			  analog_before_vals = BeforeVals,
			  analog_pub_interval = AnalogPubInterval}) ->
    NowTime = current_millisec(),
    BeforeSentTime = case dict:find(PinNo, SentTimes) of
			 error     -> 0;
			 {ok, MilliSec} -> MilliSec
		     end, 

    if AnalogPubInterval =< (NowTime - BeforeSentTime) ->
	    publish_analog(GroupId, DeviceId, PinNo, Val),
	    NewSentTimes = dict:store(PinNo, NowTime, SentTimes),
	    {ok, State#state{analog_sent_time =  NewSentTimes}};
       true ->
	    publish_if_large_changed(GroupId, DeviceId, PinNo, Val, BeforeVals),
	    NewVals = dict:store(PinNo, Val, BeforeVals),
	    {ok, State#state{analog_before_vals = NewVals}}
    end.

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

%%--------------------------------------------------------------------
%% @private
%% @doc crete mqtt topic, that formatted binary.
%% @end
%%--------------------------------------------------------------------
-spec topic(GroupId, DeviceId, DataType, DataNo) -> binary() when
      GroupId :: binary(),
      DeviceId :: binary(),
      DataType :: binary(),
      DataNo :: non_neg_integer().
topic(GroupId, DeviceId, DataType, DataNo) when is_binary(GroupId),
						is_binary(DeviceId),
						is_binary(DataType),
						is_integer(DataNo) ->
    list_to_binary(["/", GroupId, "/", DeviceId, "/", DataType, "/",
		    integer_to_binary(DataNo)]).

%%--------------------------------------------------------------------
%% @private
%% @doc publish analog value if value change is large.
%% @end
%%--------------------------------------------------------------------
-spec publish_if_large_changed(GroupId, DeviceId, PinNo, Val, BeforeVals) -> 
				      sent | ignore when
      GroupId :: binary(),
      DeviceId :: binary(),
      PinNo :: non_neg_integer(),
      Val :: non_neg_integer(),
      BeforeVals :: dict:dict().
publish_if_large_changed(GroupId, DeviceId, PinNo, Val, BeforeVals) ->
    BeforeVal = case dict:find(PinNo, BeforeVals) of
		    error      -> 0;
		    {ok, BVal} -> BVal
		end,
    if abs(BeforeVal - Val) > ?ANALOG_CHANGE_LIMIT_FOR_SEND ->
	    publish_analog(GroupId, DeviceId, PinNo, Val),
	    sent;
       true ->
	    ignore
    end.
       
%%--------------------------------------------------------------------
%% @private
%% @doc publish analog value to MQTT broker.
%% @end
%%--------------------------------------------------------------------
-spec publish_analog(GroupId, DeviceId, PinNo, Val) -> ok when
      GroupId :: binary(),
      DeviceId :: binary(),
      PinNo :: non_neg_integer(),
      Val :: non_neg_integer().
publish_analog(GroupId, DeviceId, PinNo, Val) ->
    lager:info("analog send mqtt broker(PinNo:~w): ~w", [PinNo, Val]),
    Payload = marionet_data:pack_io(<<"ai">>, DeviceId, PinNo, Val, []),
    Topic = topic(GroupId, DeviceId, <<"analog">>, PinNo),
    maybe_publish(Topic, Payload, [{qos, 0}, {retain, true}]).

%%--------------------------------------------------------------------
%% @private
%% @doc get current time(millisecond).
%% @end
%%--------------------------------------------------------------------
-spec current_millisec() -> non_neg_integer().
current_millisec() ->
    {MSec, Sec, MicroSec} = erlang:now(),
    (MSec * 1000000 + Sec) * 1000 + (MicroSec div 1000).

maybe_publish(Topic, Payload, Opts) ->
    case whereis(emqttc) of
	undefined -> 
	    %%lager:warning("MQTT not connected."),
	    ok;
	Pid when is_pid(Pid) ->
	    emqttc:publish(Pid, Topic, Payload, Opts)
    end.
