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
%%   ["digital_write", [2, 1]]
%%   ["digital_read" , [2]]
handle_info({zmq, _S, Payload, []}, State = #state{socket = Socket}) ->
    lager:debug("zmq rep: ~p", [Payload]),
    case marionet_data:unpack_command(Payload) of
	{UUID, Command, Args} when is_binary(Command), is_list(Args) ->
	    Rep = handle_zmq_request(Command, Args),
	    Json = marionet_data:pack([{uuid, UUID} | Rep]),
	    erlzmq:send(Socket, Json);
	Other ->
	    lager:warning("unknown zmq req: ~p", [Other]),
	    Json = marionet_data:pack([{error, <<"invalid_command">>}]),
	    erlzmq:send(Socket, Json)
    end,
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("unknown message: ~p", [Info]),
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
%%% Command Handler
%%%===================================================================

handle_zmq_request(<<"gpio_digital_write">>, [PinNo, Val]) ->
    lager:debug("gpio_digital_write(PinNo:~p, Val:~p)", [PinNo, Val]),
    ok = gpio_pin:write(PinNo, Val),
    [{return, true}];

handle_zmq_request(<<"gpio_digital_read">>, [PinNo]) ->
    lager:debug("gpio_digital_read(PinNo:~p)", [PinNo]),
    [{return, gpio_pin:read(PinNo)}];

handle_zmq_request(Command, Args) ->
    lager:debug("unknown zmq request: ~p : ~p", [Command, Args]),
    [{return, nil}, {error, <<"command_not_found">>}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

close_socket(_State = #state{socket = Socket, context = Context}) ->
    lager:info("closing ZMQ."),
    ok = erlzmq:close(Socket, ?CLOSE_TIMEOUT),
    ok = erlzmq:term(Context, ?CLOSE_TIMEOUT).


