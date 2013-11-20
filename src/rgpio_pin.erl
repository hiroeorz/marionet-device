%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@hibiscus>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Nov 2013 by HIROE Shin <shin@hibiscus>
%%%-------------------------------------------------------------------
-module(rgpio_pin).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 set_pin_mode/2,
	 read/1,
	 write/2,
	 set_int/2,
	 pullup/1,
	 pulldown/1,
	 pullnone/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type mode() :: in | out | dummy.
-type edge() :: falling | rising | both | none.
-type pull() :: up | down | none.

-define(SERVER, ?MODULE).

-record(state, {pin_no      :: non_neg_integer(),
		file_io     :: file:io_device(),
		edge        :: edge(),
		mode        :: mode(),
		pull        :: pull() }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link({PinNo, Mode} | {PinNo, Mode, Edge, Pull}) -> 
			{ok, Pid} |
			ignore    |
			{error, Error} when
      PinNo :: non_neg_integer(),
      Mode :: in | out | dummy,
      Edge :: edge(),
      Pull :: pull(),
      Pid :: pid(),
      Error :: term().
start_link({PinNo, Mode}) when Mode =:= in orelse 
			       Mode =:= out orelse
			       Mode =:= dummy ->
    start_link({PinNo, Mode, none, none});

start_link({PinNo, Mode, Edge, Pull}) when Mode =:= in orelse 
					   Mode =:= out orelse
					   Mode =:= dummy ->
    gen_server:start_link(?MODULE, [PinNo, Mode, Edge, Pull], []).

%%--------------------------------------------------------------------
%% @doc set pin mode, in, out or dummy.
%% @end
%%--------------------------------------------------------------------
-spec set_pin_mode(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: mode().
set_pin_mode(PinNo, Mode) when Mode =:= in orelse 
			   Mode =:= out orelse
			   Mode =:= dummy ->
    gen_server:call(get_child(PinNo), {set_pin_mode, Mode}).

%%--------------------------------------------------------------------
%% @doc read gpio value.
%% @end
%%--------------------------------------------------------------------
-spec read(PinNo) -> Val when
      PinNo :: non_neg_integer(),
      Val :: non_neg_integer().
read(PinNo) when is_integer(PinNo) ->
    gen_server:call(get_child(PinNo), read).

%%--------------------------------------------------------------------
%% @doc write value to gpio.
%% @end
%%--------------------------------------------------------------------
-spec write(PinNo, Val) -> ok when
      PinNo :: non_neg_integer(),
      Val :: non_neg_integer().      
write(PinNo, Val) when is_integer(PinNo) andalso is_integer(Val) ->
    gen_server:call(get_child(PinNo), {write, Val}).

%%--------------------------------------------------------------------
%% @doc set interrupt that fire when gpio's input or output status is chaned.
%% @end
%%--------------------------------------------------------------------
-spec set_int(PinNo, Mode) -> ok | {error, Reason} when
      PinNo :: non_neg_integer(),
      Mode :: edge(),
      Reason :: term().
set_int(PinNo, Mode) ->
    gen_server:call(get_child(PinNo), {set_int, Mode}).    

%%--------------------------------------------------------------------
%% @doc set pullup to a pin.
%% @end
%%--------------------------------------------------------------------
-spec pullup(PinNo) -> ok when
      PinNo :: non_neg_integer().
pullup(PinNo) ->
    rgpio_port:pullup(PinNo).

%%--------------------------------------------------------------------
%% @doc set pulldown to a pin.
%% @end
%%--------------------------------------------------------------------
-spec pulldown(PinNo) -> ok when
      PinNo :: non_neg_integer().
pulldown(PinNo) ->
    rgpio_port:pulldown(PinNo).

%%--------------------------------------------------------------------
%% @doc release pin mode from pullup pulldown.
%% @end
%%--------------------------------------------------------------------
-spec pullnone(PinNo) -> ok when
      PinNo :: non_neg_integer().
pullnone(PinNo) ->
    rgpio_port:pullnone(PinNo).

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
init([PinNo, dummy, _Edge, _Pull]) ->
    {ok, #state{pin_no = PinNo, mode = dummy}};

init([PinNo, Mode, Edge, Pull]) ->
    ok = unexport(PinNo), timer:sleep(300), %% waiting for file deleted...
    ok = export(PinNo),   timer:sleep(300), %% waiting for file created...
    ok = set_mode(PinNo, Mode),
    {ok, FileIO} = open(PinNo, Mode),

    case Edge of
	none -> undefined;
	Edge when Edge =:= rising orelse
		  Edge =:= falling orelse
		  Edge =:= both ->
	    ok  = set_interrupt(PinNo, Edge)
    end,

    case Pull of
	up   -> rgpio_port:pullup(PinNo);
	down -> rgpio_port:pulldown(PinNo);
	none -> rgpio_port:pullnone(PinNo)
    end,

    {ok, #state{pin_no = PinNo, file_io = FileIO, edge = Edge, 
		mode = Mode, pull = Pull}}.

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

handle_call({set_pin_mode, dummy}, _From, #state{mode = dummy} = State) ->
    {reply, ok, State};

handle_call({set_pin_mode, dummy}, _From, State) ->
    PinNo = State#state.pin_no,
    ok = file:close(State#state.file_io),
    {reply, ok, #state{pin_no = PinNo, mode = dummy}};

handle_call({set_pin_mode, Mode}, _From, State) ->
    PinNo = State#state.pin_no,
    ok = set_mode(PinNo, Mode),
    {ok, FileIO} = open(PinNo, Mode),
    {reply, ok, State#state{file_io = FileIO}};

%% read pin state
handle_call(read, _From, #state{mode = dummy} = State) ->
    {reply, 0, State};

handle_call(read, _From, State) ->
    Reply = case read_row(State#state.file_io) of
		{ok, Val} -> list_to_integer(Val);
		{error, Reason} -> {error, Reason}
	    end,

    {reply, Reply, State};

%% write pin state
handle_call({write, _Val}, _From, #state{mode = dummy} = State) ->
    {reply, ok, State};

handle_call({write, Val}, _From, State) ->
    FileIO = State#state.file_io,
    {ok, 0} = file:position(FileIO, 0),

    Reply = case file:write(FileIO, integer_to_list(Val)) of
		ok -> ok;
		{error, Reason} -> {error, Reason}
	    end,

    {reply, Reply, State};

% set interrupt
handle_call({set_int, _Mode}, _From,  #state{mode = dummy} = State) ->
    {reply, ok, State};
    
handle_call({set_int, Mode}, _From, #state{pin_no = PinNo} = State) ->
    Reply = set_interrupt(PinNo, Mode),
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
%% now modifing...
handle_info({changed, {PinNo, Val}}, #state{edge = both} = State) ->
    io:format("change! ~w: ~w~n", [PinNo, Val]),
    {noreply, State};

handle_info({changed, {PinNo, 1}}, #state{edge = rising} = State) ->
    io:format("rising! ~w: ~w~n", [PinNo, 1]),
    {noreply, State};

handle_info({changed, {PinNo, 0}}, #state{edge = falling} = State) ->
    io:format("falling! ~w: ~w~n", [PinNo, 0]),
    {noreply, State};

handle_info({changed, {PinNo, Val}}, State) ->
    io:format("ignore: ~w: ~w~n", [PinNo, Val]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("unknown message received: ~p~n", [Info]),
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
    ok = unexport(State#state.pin_no),
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
%% @doc get child pid from supervisor.
%% @end
%%--------------------------------------------------------------------
-spec get_child(PinNo) -> Pid | {error, not_started} when
      PinNo :: non_neg_integer(),
      Pid :: pid().
get_child(PinNo) ->
    List = supervisor:which_children(rgpio_pin_sup),

    Fun = fun({{rgpio_pin, No}, _, _, _}) ->
		  No =:= PinNo;
	     ({_, _, _, _}) ->
		  false
	  end,

    case lists:filter(Fun, List) of
	[] ->
	    {error, not_started};
	[ {{rgpio_pin, PinNo}, Pid, _, _}] ->
	    Pid
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc read row data from device file.
%% @end
%%--------------------------------------------------------------------
-spec read_row(FileIO) -> ok | {error, Reason} when
      FileIO :: file:device_io(),
      Reason :: term().
read_row(FileIO) ->
    {ok, 0} = file:position(FileIO, 0),
    file:read(FileIO, 1).


%%--------------------------------------------------------------------
%% @private
%% @doc open device file.
%% @end
%%--------------------------------------------------------------------
-spec open(PinNo, Mode) -> ok | {error, Reason} when
      PinNo :: non_neg_integer(),
      Mode :: in | out,
      Reason :: term().
open(PinNo, Mode) ->
    OpenMode = case Mode of
		   out -> read_write;
		   in  -> read
	       end,

    FileName = io_lib:format("/sys/class/gpio/gpio~w/value", [PinNo]),
    file:open(FileName, OpenMode).

%%--------------------------------------------------------------------
%% @private
%% @doc unexport gpio.
%% @end
%%--------------------------------------------------------------------
-spec unexport(PinNo) -> ok when
      PinNo :: non_neg_integer().
unexport(PinNo) ->
    {ok, FileIO} = file:open("/sys/class/gpio/unexport", write),
    case file:write(FileIO, io_lib:format("~w", [PinNo])) of
	ok -> ok;
	{error, einval} -> ok %% not exported.
    end,
    ok = file:close(FileIO).

%%--------------------------------------------------------------------
%% @private
%% @doc export gpio.
%% @end
%%--------------------------------------------------------------------
-spec export(PinNo) -> ok when
      PinNo :: non_neg_integer().
export(PinNo) ->
    {ok, FileIO} = file:open("/sys/class/gpio/export", write),
    ok = file:write(FileIO, io_lib:format("~w", [PinNo])),
    ok = file:close(FileIO).

%%--------------------------------------------------------------------
%% @private
%% @doc set open mode, in or out.
%% @end
%%--------------------------------------------------------------------
-spec set_mode(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: in | out.
set_mode(PinNo, Mode) when Mode =:= in orelse
			   Mode =:= out ->
    FileName = io_lib:format("/sys/class/gpio/gpio~w/direction", [PinNo]),
    {ok, FileIO} = file:open(FileName, write),
    ok = file:write(FileIO, atom_to_list(Mode)),
    ok = file:close(FileIO).    

%%--------------------------------------------------------------------
%% @private
%% @doc set interrupt to gpio pin.
%% @end
%%--------------------------------------------------------------------
-spec set_interrupt(PinNo, Mode) -> ok | {error, Reason} when
      PinNo :: non_neg_integer(),
      Mode :: edge(),      
      Reason :: term().
set_interrupt(PinNo, Mode) ->
    ok = set_int_mode(PinNo, Mode),
    rgpio_port:start_poll(PinNo, Mode).

%%--------------------------------------------------------------------
%% @private
%% @doc export gpio.
%% @end
%%--------------------------------------------------------------------
-spec set_int_mode(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: edge().
set_int_mode(PinNo, Mode) ->
    FileName = io_lib:format("/sys/class/gpio/gpio~w/edge", [PinNo]),
    {ok, FileIO} = file:open(FileName, write),
    ok = file:write(FileIO, atom_to_list(Mode)),
    ok = file:close(FileIO).
