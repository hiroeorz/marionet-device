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
-define(ANALOG_CODE, 2).
-define(DIGITAL_CODE, 1).

-record(state, {group_id  :: binary(),
		device_id :: binary() }).

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
init([GroupId, DeviceId] = Args) ->
    lager:info("Start I/O event handler: ~p, ~p", [?MODULE, Args]),
    {ok, #state{group_id = GroupId, device_id = DeviceId}}.

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
	     State=#state{device_id=DeviceId}) ->
    lager:info("digital sent mqtt broker(port:~w): ~p", [PortNo, Status]),
    Payload = marionet_data:pack([?DIGITAL_CODE, DeviceId, PortNo, Status]),
    GroupId = State#state.group_id,
    Topic = topic(GroupId, DeviceId, <<"digital">>, PortNo),
    lager:debug("Send Topic  : ~p", [Topic]),
    lager:debug("Send Payload: ~p", [Payload]),
    emqttc:publish(emqttc, Topic, Payload, [{qos, 0}, {retain, 1}]),
    {ok, State};

handle_event({analog_recv, PinNo, Val},
	     State=#state{device_id=DeviceId}) ->
    lager:info("analog send mqtt broker(PinNo:~w): ~w", [PinNo, Val]),
    Payload = marionet_data:pack([?ANALOG_CODE, DeviceId, PinNo, Val]),
    GroupId = State#state.group_id,
    Topic = topic(GroupId, DeviceId, <<"analog">>, PinNo),
    lager:debug("Send Topic  : ~p", [Topic]),
    lager:debug("Send Payload: ~p", [Payload]),
    emqttc:publish(emqttc, Topic, Payload),
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

%% crete mqtt topic, that formatted binary.
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

