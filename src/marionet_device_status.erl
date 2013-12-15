%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 12 Dec 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_device_status).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 update_digital_port/2,
	 digital/1,
	 all_digital/0,
	 update_analog_value/2,
	 analog/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { gpio :: [tuple()] }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Gpio) -> {ok, pid()} | ignore | {error, term()} when
      Gpio :: [tuple()].
start_link(Gpio) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Gpio], []).

%%--------------------------------------------------------------------
%% @doc update digital port(8bit).
%% @end
%%--------------------------------------------------------------------
-spec update_digital_port(PortNo, Status) -> ok when
      PortNo :: non_neg_integer(),
      Status :: [0 | 1].
update_digital_port(PortNo, Status) when is_integer(PortNo),
					 is_list(Status) ->
    gen_server:cast(?SERVER, {update_digital_port, PortNo, Status}).

%%--------------------------------------------------------------------
%% @doc get digital state of port.
%% @end
%%--------------------------------------------------------------------
-spec digital(PortNo) -> [0 | 1] | undefined when
      PortNo :: non_neg_integer().
digital(PortNo) ->
    case ets:lookup(digital, PortNo) of
	[]  -> undefined;
	[S] -> S
    end.    

%%--------------------------------------------------------------------
%% @doc get all digital state.
%% @end
%%--------------------------------------------------------------------
-spec all_digital() -> [0 | 1].
all_digital() ->
    case ets:first(digital) of 
	'$end_of_table' ->
	    [];
	Key ->
	    all_digital(Key, [])
    end.

all_digital('$end_of_table', Result) ->
    lists:reverse(Result);

all_digital(Key, Result) ->
    [{_PortNo, [X0, X1, X2, X3, X4, X5, X6, X7]}] = 
	ets:lookup(digital, Key),

    NextKey = ets:next(digital, Key),
    all_digital(NextKey, [X7, X6, X5, X4, X3, X2, X1, X0 | Result]).

%%--------------------------------------------------------------------
%% @doc update analog value(14bit).
%% @end
%%--------------------------------------------------------------------
-spec update_analog_value(PinNo, Value) -> ok when
      PinNo :: non_neg_integer(),
      Value :: non_neg_integer().
update_analog_value(PinNo, Value) when is_integer(PinNo),
				       is_integer(Value) ->
    gen_server:cast(?SERVER, {update_analog_value, PinNo, Value}).

%%--------------------------------------------------------------------
%% @doc get digital state of port.
%% @end
%%--------------------------------------------------------------------
-spec analog(PinNo) -> [0 | 1] | undefined when
      PinNo :: non_neg_integer().
analog(PinNo) ->
    case ets:lookup(analog, PinNo) of
	[]  -> undefined;
	[{PinNo, V}] -> V
    end.

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
init([Gpio]) ->
    ets:new(digital, [ordered_set, protected, named_table]),
    ets:new(analog,  [ordered_set, protected, named_table]),
    ok = update_all_status(),
    {ok, #state{gpio = Gpio}}.

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
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

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
handle_cast({update_digital_port, PortNo, Status}, State) ->
    true = ets:insert(digital, {PortNo, Status}),
    {noreply, State};

handle_cast({update_analog_value, PinNo, Value}, State) ->
    true = ets:insert(analog, {PinNo, Value}),
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
%% @doc get all pin's status and store to ets.
%% @end
%%--------------------------------------------------------------------
-spec update_all_status() -> ok.
update_all_status() ->
    get_raspberrypi_status(),
    get_arduino_status().    

%%--------------------------------------------------------------------
%% @private
%% @doc get all arduino pin's status and store to ets.
%% @end
%%--------------------------------------------------------------------
get_arduino_status() ->
    AllStatus = arduino:all_digital(),
    get_status(0, AllStatus).    

%%--------------------------------------------------------------------
%% @private
%% @doc get all RaspberryPi pin's status and store to ets.
%% @end
%%--------------------------------------------------------------------
get_raspberrypi_status() ->
    AllStatus = gpio_pin:all_digital(),
    get_status(0, AllStatus).

get_status(_PortNo, []) ->
    ok;

get_status(PortNo, [X1, X2, X3, X4, X5, X6, X7, X8 | Tail]) ->
    true = ets:insert(digital, {PortNo, [X1, X2, X3, X4, X5, X6, X7, X8]}),
    get_status(PortNo, Tail).

