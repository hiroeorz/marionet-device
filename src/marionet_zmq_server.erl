%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_zmq_server).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3, close/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(ZMQ_END_POINT, "tcp://127.0.0.1:6789").
-define(SERVER, ?MODULE).
-define(CLOSE_TIMEOUT, 5000).

-record(state, { context     :: binary(),
		 socket      :: {pos_integer, binary()},
		 plc_address :: string() | binary(),
		 plc_port    :: pos_integer() }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(PlcAddress, PlcPort) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(PlcAddress, PlcPort) ->
    start_link(?ZMQ_END_POINT, PlcAddress, PlcPort).

start_link(EndPoint, PlcAddress, PlcPort) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
			  [EndPoint, PlcAddress, PlcPort], []).

close() ->
    gen_server:call(?SERVER, close).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([EndPoint, PlcAddress, PlcPort]) ->
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [rep, {active, true}]),
    ok = erlzmq:bind(Socket, EndPoint),
    {ok, #state{context = Context, socket = Socket,
		plc_address = PlcAddress, plc_port = PlcPort}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(close, _From, State) ->
    Reply = close_socket(State),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%%   ["gpio", "write", [PinNo, 1]]
%%   ["omron_fins", "write_dm_values", [StartAddress, [1,2,3]]]
handle_info({zmq, _S, Msg, []}, State = #socket{socket = Socket}) ->
    lager:debug("zmq rep: ~p", [Msg]),
    case marionet_data:unpack(Msg) of
	[M, F, Args] ->
	    Rep = handle_zmq_request(M, F, Args),
	    erlzmq:send(Socket, Rep);
	Other ->
	    lager:warning("unknown zmq req: ~p", [Other])
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    close_socket(State).

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

handle_zmq_request(M, F, Args) ->
    lager:debug("handle zmq req: ~p : ~p : ~p", [M, F, Args]).

close_socket(_State = #state{socket = Socket, context = Context}) ->
    lager:info("closing ZMQ."),
    ok = erlzmq:close(Socket, ?CLOSE_TIMEOUT),
    ok = erlzmq:term(Context, ?CLOSE_TIMEOUT).


