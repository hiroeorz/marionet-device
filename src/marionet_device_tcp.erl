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

-define(AES_ENCRYPTED_CODE,             16#01).
-define(CHALLENGE_REQUEST_CODE,         16#11).
-define(ANALOG_IO_PUB_MESSAGE_CODE,     16#E1).
-define(DIGITAL_IO_PUB_MESSAGE_CODE,    16#91).

-record(state, {ip_address      :: inet:ip_address(),
                port            :: inet:port_number(),
		device_id       :: pos_integer(),
		token           :: binary(),
                socket          :: gen_tcp:socket(),
		aes_ivec        :: binary(),
		aes_send_state  :: crypto:opaque(),
		aes_recv_state  :: crypto:opaque()}).

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
    TokenBin = decode(Token),
    {ok, closed, #state{ip_address = IPAddress, port = Port,
			device_id = DeviceId, token = TokenBin}}.

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
    lager:error("state is closed.: ~p", [Bin]),
    {next_state, closed, State}.

opened({send_message, Bin}, State) when is_binary(Bin) ->
    lager:error("state is not authenticated.: ~p", [Bin]),
    {next_state, opened, State}.

connected({send_message, Bin}, State) when is_binary(Bin) ->
    lager:info("send: ~p", [Bin]),
    case send_encrypted_data(State#state.socket, Bin, State) of
	{ok, NewState} ->
            {next_state, connected, NewState};
	{error, NewState} ->
            ok = spawn_reconnect_timer(),
            {next_state, closed, NewState}
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

    Opts = [binary, {packet, 1},
            {reuseaddr, true},
            {nodelay, true},
            {active, once},
            {send_timeout, 3000}],

    case gen_tcp:connect(IPAddress, Port, Opts) of
        {ok, Socket} ->
            lager:info("connected to server: ~p:~p", [IPAddress, Port]),
	    {next_state, opened, State#state{socket = Socket}};
        {error, Reason} ->
            lager:error("connection failure: ~p", [Reason]),
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

%%--------------------------------------------------------------
%% AES encrypted message.
%% Encrypted data is decrypted and call handle_info/3 again.
%%--------------------------------------------------------------
handle_info({tcp, Socket, <<?AES_ENCRYPTED_CODE:8, Encoded/binary>> = _Data},
	    StateName, State) ->
    %%lager:info("encrypted data received: ~p", [Data]),
    AESRecvState = State#state.aes_recv_state,
    {AESRecvState1, Plain} = bulletio_crypto:decrypt(Encoded, AESRecvState),
    NewState = State#state{aes_recv_state = AESRecvState1},
    handle_info({tcp, Socket, Plain}, StateName, NewState);

%% authenticate success response from server.
handle_info({tcp, Socket, <<?CHALLENGE_REQUEST_CODE:8, _/binary>> = Data},
	    opened, State) ->
    {challenge_request, {IVec, Challenge}} = iopack:parse(Data),

    AESKey = State#state.token,
    DeviceId = State#state.device_id,
    AESSendState = bulletio_crypto:init(AESKey, IVec),
    AESRecvState = bulletio_crypto:init(AESKey, IVec),
    {AESSendState1, Encoded} = bulletio_crypto:encrypt(Challenge, AESSendState),

    lager:info("plain     challenge: ~s", [Challenge]),
    lager:info("encrypted challenge: ~s", [Encoded]),

    Bin = iopack:format(challenge_response, {Encoded, DeviceId}),
    ok = gen_tcp:send(State#state.socket, Bin),
    NewState = State#state{aes_ivec = IVec,
			   aes_send_state = AESSendState1,
			   aes_recv_state = AESRecvState},
	
    ok = inet:setopts(Socket, [{active, once}]),
    {next_state, connected, NewState};

%%--------------------------------------------------------------
%% tcp message in connected
%%--------------------------------------------------------------

%% analog io message from other device.
handle_info({tcp, Socket, <<?ANALOG_IO_PUB_MESSAGE_CODE:8, _/binary>> = Data},
	    connected, State) ->
    {analog_io_pub_message, {DeviceId, PinNo, Value}} = iopack:parse(Data),
    lager:info("sub! analog device_id:~p: pin:~p val:~p",
			  [DeviceId, PinNo, Value]),
    ok = inet:setopts(Socket, [{active, once}]),
    {next_state, connected, State};

%% digital io message from other device.
handle_info({tcp, Socket, <<?DIGITAL_IO_PUB_MESSAGE_CODE:8, _/binary>> = Data},
	    connected, State) ->
    {digital_io_pub_message, {DeviceId, PortNo, List}} = iopack:parse(Data),
    lager:info("sub! digital device_id:~p: pin:~p val:~p",
			  [DeviceId, PortNo, List]),
    ok = inet:setopts(Socket, [{active, once}]),
    {next_state, connected, State};

%%--------------------------------------------------------------
%% close tcp session
%%--------------------------------------------------------------
handle_info({tcp_closed, _} = Info, StateName, State) ->
    lager:error("TCP/IP session closed: ~p from ~p", [StateName, Info]),
    ok = spawn_reconnect_timer(),
    {next_state, closed, State};

%%--------------------------------------------------------------
%% another message
%%--------------------------------------------------------------
handle_info(Info, StateName, State) ->
    lager:info("~p: unknown info: ~p", [StateName, Info]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc Encrypted data send to device.
%% @end
%%--------------------------------------------------------------------
-spec send_encrypted_data(Socket, Plain, State) -> {ok, #state{}}    |
						   {error, #state{}} when
      Socket :: gen_tcp:socket(),
      Plain :: binary(),
      State :: #state{}.
send_encrypted_data(Socket, Plain, State) ->
    AESSendState = State#state.aes_send_state,
    {AESSendState1, Encrypted} = bulletio_crypto:encrypt(Plain, AESSendState),
    SendData = iopack:format(aes_encrypt, {Encrypted}),
    case gen_tcp:send(Socket, SendData) of
	ok ->
	    {ok, State#state{aes_send_state = AESSendState1}};
	{error, _Reason} ->
	    {error, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 文字列バイナリを数値バイナリにして返す。
%% @end
%%--------------------------------------------------------------------
-spec decode(list() | binary()) -> binary().
decode(Bin) when is_binary(Bin) ->
    decode(binary_to_list(Bin));

decode(Str) when is_list(Str) ->
    decode(Str, []).

decode(Str, List) ->
    case string:substr(Str, 1, 2) of
	[] ->
	    IntList = [list_to_integer(Unit, 16) || Unit <- List],
	    list_to_binary(lists:reverse(IntList));
	Part -> 
	    decode(string:substr(Str, 3, length(Str)), [Part | List])
    end.
