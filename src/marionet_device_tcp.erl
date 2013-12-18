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
-export([start_link/2,
         send_message/1,
         open_connection/0]).

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
init([IPAddress, Port]) ->
    ok = spawn_reconnect_timer(),
    {ok, closed, #state{ip_address = IPAddress, port = Port}}.

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
    {reply, ok, closed, State}.

connected(open, _From, State) ->
    error_logger:info_msg("already opend session to a server~n"),
    {reply, ok, connected, State};

connected(shutdown, _From, State) ->
    error_logger:info_msg("shutting down session.~n"),
    gen_tcp:close(State#state.socket),
    {reply, ok, closed, State}.

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

    Opts = [binary, {packet, 0},
            {reuseaddr, true},
            {nodelay, true},
            {active, true},
            {send_timeout, 3000}],

    case gen_tcp:connect(IPAddress, Port, Opts) of
        {ok, Socket} ->
            error_logger:info_msg("connected to server: ~p:~p~n",
                                  [IPAddress, Port]),
            {next_state, connected, State#state{socket = Socket}};
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
handle_info({tcp_closed, _} = Info, StateName, State) ->
    error_logger:error_msg("TCP session closed: ~p from ~p~n", 
                           [StateName, Info]),
    ok = spawn_reconnect_timer(),
    {next_state, closed, State};

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

spawn_reconnect_timer() ->
    {ok, _TRef} = timer:apply_after(?RECONNECT_INTERVAL, ?MODULE, 
                                    open_connection, []),
    ok.


