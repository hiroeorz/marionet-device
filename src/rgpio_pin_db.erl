%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(rgpio_pin_db).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 update_digital_pin/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {digital_tid :: ets:tid(),
		gpio        :: [tuple()] }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(GpioList) -> {ok, pid()} | ignore | {error, term()} when
      GpioList :: [tuple()].
start_link(GpioList) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GpioList], []).

%%--------------------------------------------------------------------
%% @doc update digital pin.
%%
%% GpioPinNo is No of total pin in RaspberryPi GPIO.
%% @end
%%--------------------------------------------------------------------
-spec update_digital_pin(GpioPinNo, PinState) -> {ok, PortNo, Status} when
      GpioPinNo :: non_neg_integer(),
      PinState :: 0 | 1,
      PortNo :: non_neg_integer(),
      Status :: [0 | 1].
update_digital_pin(GpioPinNo, PinState) when is_integer(GpioPinNo),
					     (PinState =:= 0 orelse
					      PinState =:= 1) ->
    gen_server:cast(?SERVER, {update_digital_pin, GpioPinNo, PinState}).

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
init([GpioList]) ->
    Tid = ets:new(rgpio_digital, [ordered_set, private]),
    {ok, #state{digital_tid = Tid, gpio = GpioList}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
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
handle_cast({update_digital_pin, GpioPinNo, PinState},
	    #state{gpio = Gpio} = State) ->

    Tid = State#state.digital_tid,

    case get_pin_position(GpioPinNo, Gpio) of
	noentry ->
	    io:format("pin change report for no entry pin:~p~n", [GpioPinNo]),
	    {noreply, State};
	{PortNo, PinNo} ->
	    OldPortStatus = case ets:lookup(Tid, PortNo) of
				[] ->
				    [0, 0, 0, 0, 0, 0, 0, 0];
				[{PortNo, List}] ->
				    List
			    end,

	    NewPortStatus = update_status(PinNo, PinState, OldPortStatus),
	    true = ets:insert(Tid, {PortNo, NewPortStatus}),
	    gen_event:notify(rgpio_event, {digital_port_changed, 
					   PortNo, NewPortStatus}),
	    {noreply, State}
    end.

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
%% @doc update status of a port.
%% @end
%%--------------------------------------------------------------------
-spec update_status(PinNo, State, List) -> [1 | 0] when
      PinNo :: non_neg_integer(),
      State :: 0 | 1,
      List :: [0 | 1].
update_status(PinNo, State, List) ->
    L = lists:foldl(fun(_S, NewList) when PinNo =:= length(NewList) ->
			    [State | NewList];
		       (S, NewList) ->
			    [S | NewList]
		    end, [], List),
    lists:reverse(L).

%%--------------------------------------------------------------------
%% @private
%% @doc get pin position in a port. PinNo(in arguments) is total No of all pins.
%% @end
%%--------------------------------------------------------------------
-spec get_pin_position(GpioPinNo, GpioList) -> {PortNo, PinNo} when
      GpioPinNo :: non_neg_integer(),
      GpioList :: [tuple()],
      PortNo :: non_neg_integer(),
      PinNo :: non_neg_integer().
get_pin_position(GpioPinNo, GpioList) ->
    GpioPinNoList = [Pin || {Pin, _, _} <- GpioList],
    get_pin_position(GpioPinNo, GpioPinNoList, 0).

get_pin_position(_GpioPinNo, [], _Pos) ->
    noentry;

get_pin_position(GpioPinNo, [GpioPinNo | _Tail], Pos) ->
    PortNo = Pos div 8,
    PinNo = Pos rem 8,
    {PortNo, PinNo};

get_pin_position(GpioPinNo, [_ | Tail], Pos) ->
    get_pin_position(GpioPinNo, Tail, Pos + 1).
