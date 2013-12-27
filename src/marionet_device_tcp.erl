%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Dec 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_device_tcp).

-behaviour(gen_fsm).

%% API
-export([start_link/4, send_message/1, open_connection/0]).
-export([closed/2, opened/2, connected/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).
-define(RECONNECT_INTERVAL, 10000).

-define(AUTH_SUCCESS_CODE,              16#02).
-define(ANALOG_IO_PUB_MESSAGE_CODE,     16#E1).
-define(DIGITAL_IO_PUB_MESSAGE_CODE,    16#91).

-record(state, {ip_address :: inet:ip_address(),
                port       :: inet:port_number(),
		device_id  :: pos_integer(),
		token      :: binary(),
                socket     :: gen_tcp:socket()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% @end
%%--------------------------------------------------------------------
-spec start_link(pos_integer(), binary(), 
		 inet:ip_address(), inet:port_number()) -> {ok, pid()}    |
							   ignore         |
							   {error, term()}.
start_link(DeviceId, Token, IPAddress, Port) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, 
		       [DeviceId, Token, IPAddress, Port], []).

%%--------------------------------------------------------------------
%% @doc cast data to server.
%% @end
%%--------------------------------------------------------------------
-spec send_message(binary()) -> ok.
send_message(Bin) ->
    gen_fsm:send_event(?SERVER, {send_message, Bin}).

%%--------------------------------------------------------------------
%% @doc reconnect function called by timer.
%% @end
%%--------------------------------------------------------------------
-spec open_connection() -> ok.
open_connection() ->
    gen_fsm:send_all_state_event(?SERVER, reconnect).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([DeviceId, Token, IPAddress, Port]) ->
    ok = spawn_reconnect_timer(),
    {ok, closed, #state{ip_address = IPAddress, port = Port,
			device_id = DeviceId, token = Token}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%% @end
%%--------------------------------------------------------------------
closed({send_message, Bin}, State) when is_binary(Bin) ->
    error_logger:error_msg("state is closed.: ~p~n", [Bin]),
    {next_state, closed, State}.

opened({send_message, Bin}, State) when is_binary(Bin) ->
    error_logger:error_msg("state is not authenticated.: ~p~n", [Bin]),
    {next_state, opened, State}.

connected({send_message, Bin}, State) when is_binary(Bin) ->
    %%error_logger:info_msg("send: ~p~n", [Bin]),
    case gen_tcp:send(State#state.socket, Bin) of
        ok ->
            {next_state, connected, State};
        {error, closed} ->
            ok = spawn_reconnect_timer(),
            {next_state, closed, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(reconnect, _StateName, State) ->
    IPAddress = State#state.ip_address,
    Port = State#state.port,
    DeviceId = State#state.device_id,
    Token = State#state.token,

    Opts = [binary, {packet, 1},
            {reuseaddr, true},
            {nodelay, true},
            {active, once},
            {send_timeout, 3000}],

    case gen_tcp:connect(IPAddress, Port, Opts) of
        {ok, Socket} ->
            error_logger:info_msg("connected to server: ~p:~p~n",
                                  [IPAddress, Port]),

	    AuthBin = iopack:format(auth_request, {DeviceId, Token}),
	    case gen_tcp:send(Socket, AuthBin) of
		ok ->
		    ok = inet:setopts(Socket, [{active, once}]),
		    {next_state, opened, State#state{socket = Socket}};
		{error, closed} ->
		    ok = spawn_reconnect_timer(),
		    {next_state, closed, State}
	    end;
        {error, Reason} ->
            error_logger:error_msg("connection failure: ~p~n", [Reason]),
            ok = spawn_reconnect_timer(),
            {next_state, closed, State}
    end;

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------
%% tcp message in opened
%%--------------------------------------------------------------

%% authenticate success response from server.
handle_info({tcp, Socket, <<?AUTH_SUCCESS_CODE:8>>}, opened, State) ->
    error_logger:info_msg("authentication success, session connected.~n"),
    ok = inet:setopts(Socket, [{active, once}]),
    {next_state, connected, State};

%%--------------------------------------------------------------
%% tcp message in connected
%%--------------------------------------------------------------

%% analog io message from other device.
handle_info({tcp, Socket, <<?ANALOG_IO_PUB_MESSAGE_CODE:8, _/binary>> = Data},
	    connected, State) ->
    {analog_io_pub_message, {DeviceId, PinNo, Value}} = iopack:parse(Data),
    error_logger:info_msg("sub! analog device_id:~p: pin:~p val:~p~n",
			  [DeviceId, PinNo, Value]),
    ok = inet:setopts(Socket, [{active, once}]),
    {next_state, connected, State};

%% digital io message from other device.
handle_info({tcp, Socket, <<?DIGITAL_IO_PUB_MESSAGE_CODE:8, _/binary>> = Data},
	    connected, State) ->
    {digital_io_pub_message, {DeviceId, PortNo, List}} = iopack:parse(Data),
    error_logger:info_msg("sub! digital device_id:~p: pin:~p val:~p~n",
			  [DeviceId, PortNo, List]),
    ok = inet:setopts(Socket, [{active, once}]),
    {next_state, connected, State};

%%--------------------------------------------------------------
%% close tcp session
%%--------------------------------------------------------------
handle_info({tcp_closed, _} = Info, StateName, State) ->
    error_logger:error_msg("TCP session closed: ~p from ~p~n", 
                           [StateName, Info]),
    ok = spawn_reconnect_timer(),
    {next_state, closed, State};

%%--------------------------------------------------------------
%% another message
%%--------------------------------------------------------------
handle_info(Info, StateName, State) ->
    error_logger:info_msg("~p: unknown info: ~p~n", [StateName, Info]),
    ok = inet:setopts(State#state.socket, [{active, once}]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

spawn_reconnect_timer() ->
    {ok, _TRef} = timer:apply_after(?RECONNECT_INTERVAL, ?MODULE, 
                                    open_connection, []),
    ok.


