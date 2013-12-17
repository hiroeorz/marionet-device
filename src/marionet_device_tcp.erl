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
-export([start_link/2]).

-export([closed/2, closed/3,
	 connected/2, connected/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).
-define(RECONNECT_INTERVAL, 10000).

-record(state, {ip_address :: inet:ip_address(),
		port       :: inet:port_number(),
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
-spec start_link(inet:ip_address(), inet:port_number()) -> {ok, pid()}    |
							   ignore         |
							   {error, term()}.
start_link(IPAddress, Port) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [IPAddress, Port], []).

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
init([IPAddress, Port]) ->
    State = #state{ip_address = IPAddress, port = Port},
    {ok, closed, State, ?RECONNECT_INTERVAL}.

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

closed(timeout, State) ->
    case open_connection(State) of
        {ok, Socket} ->
            {next_state, connected, State#state{socket = Socket}};
        {error, _Reason} ->
            {next_state, closed, State, ?RECONNECT_INTERVAL}
    end;

closed({send_message, Bin}, State) when is_binary(Bin) ->
    {next_state, closed, State}.

connected({send_message, Bin}, State) when is_binary(Bin) ->
    error_logger:info_msg("send: ~p", [Bin]),
    {next_state, connected, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc 接続クローズフェーズ。100ミリ秒後に接続待機フェーズに自動的に移行します。
%%
%% 接続待機フェーズに移行した後は、hibernateします。
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%% @end
%%--------------------------------------------------------------------

closed(open, _From, State) ->
    error_logger:info_msg("open session to a server~n"),
    {reply, ok, connected, State};

closed(shutdown, _From, State) ->
    error_logger:info_msg("already closed session to server.~n"),
    Reply = ok,
    {reply, Reply, state_name, State}.

connected(open, _From, State) ->
    error_logger:info_msg("already opend session to a server~n"),
    {reply, ok, connected, State};

connected(shutdown, _From, State) ->
    error_logger:info_msg("shutting down session.~n"),
    Reply = ok,
    {reply, Reply, state_name, State}.

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
handle_info({tcp_closed, ClosedSocket} = Info, StateName, State) ->
    error_logger:info_msg("info: ~p recv from state:~p: ~n", [Info, StateName]),
    ok = gen_tcp:close(ClosedSocket),
    {next_state, closed, State, ?RECONNECT_INTERVAL};

handle_info(Info, StateName, State) ->
    error_logger:info_msg("~p: unknown info: ~p~n", [StateName, Info]),
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
-spec open_connection(#state{}) -> {ok, Socket} | {error, Reason} when
      Socket :: gen_tcp:socket(),
      Reason :: term().
open_connection(State) when is_record(State, state) ->
    IPAddress = State#state.ip_address,
    Port = State#state.port,

    Opts = [binary, {packet, 0},
	    {reuseaddr, true},
	    {nodelay, true},
	    {active, true},
	    {send_timeout, 3000}],

    gen_tcp:connect(IPAddress, Port, Opts).    
