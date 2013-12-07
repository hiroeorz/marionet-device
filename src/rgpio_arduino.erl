%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(rgpio_arduino).

-behaviour(gen_server).

%% API
-export([start_link/5,
	 call/1,
	 cast/1,
	 firmata_version_request/0,
	 initialize/0,
	 digital/0,
	 analog/0,
	 digital_write/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% timer use function
-export([init_pin/1]).

-define(SERVER, ?MODULE).
-define(SYSEX_START_CODE, 16#F0).
-define(SYSEX_END_CODE,   16#F7).

-record(state, {serial_pid                  :: pid(),
		init_flag = true            :: boolean(),
		process_queue = queue:new() :: queue(),
		recv_queue = <<>>           :: binary(),
		digital_conf                :: [tuple()],
		analog_conf                 :: [non_neg_integer()],
		digital_port_reporting_conf :: [non_neg_integer()] }).

-type serial_speed() :: 9600 | 19200 | 38400 | 57600 | 115200. 
-type pin_mode() :: in | out | analog | pwm | servo.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Speed, Device, Digital, Analog, DiPortReporting) -> 
			{ok, pid()}     |
			ignore          |
			{error, term()} when
      Speed :: serial_speed(),
      Device :: string(),
      Digital :: [ {PinNo,  pin_mode(), Opts} ],
      PinNo :: non_neg_integer(),
      Opts :: [tuple()],
      Analog :: [non_neg_integer()],
      DiPortReporting :: [non_neg_integer()].
start_link(Speed, Device, Digital, Analog, DiPortReporting) ->
    gen_server:start_link({local, ?SERVER}, 
			  ?MODULE, [Speed, Device, Digital, Analog, 
				    DiPortReporting], []).

%%--------------------------------------------------------------------
%% @doc get firmata version from arduino.
%% @end
%%--------------------------------------------------------------------
-spec firmata_version_request() -> ok.
firmata_version_request() ->
    cast(rgpio_firmata:format(version_report_request)).

call(Bin) when is_binary(Bin) ->
    gen_server:call(?SERVER, {call, Bin}).

cast(Bin) when is_binary(Bin) ->
    gen_server:cast(?SERVER, {cast, Bin}).

%%--------------------------------------------------------------------
%% @doc get digital state.
%% @end
%%--------------------------------------------------------------------
digital() ->
    case ets:first(arduino_digital) of 
	'$end_of_table' ->
	    [];
	Key ->
	    digital(Key, [])
    end.

digital('$end_of_table', Result) ->
    lists:reverse(Result);

digital(Key, Result) ->
    [{_PortNo, [X0, X1, X2, X3, X4, X5, X6, X7]}] = 
	ets:lookup(arduino_digital, Key),

    NextKey = ets:next(arduino_digital, Key),
    digital(NextKey, [X7, X6, X5, X4, X3, X2, X1, X0 | Result]).

%%--------------------------------------------------------------------
%% @doc get analog state.
%% @end
%%--------------------------------------------------------------------
analog() ->
    case ets:first(arduino_analog) of 
	'$end_of_table' ->
	    [];
	Key ->
	    analog(Key, [])
    end.

analog('$end_of_table', Result) ->
    lists:reverse(Result);

analog(Key, Result) ->
    [{_PinNo, Val}] = ets:lookup(arduino_analog, Key),
    NextKey = ets:next(arduino_analog, Key),
    analog(NextKey, [Val | Result]).

initialize() ->
    gen_server:cast(?SERVER, initialize).

digital_write(PortNo, Vals) ->
    gen_server:cast(?SERVER, {digital_write, [PortNo, Vals]}).

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
init([Speed, Device, Digital, Analog, DiPortReporting]) ->
    case file:open(Device, [read]) of
	{error, eisdir} -> %% device file exist
	    Pid = serial:start([{speed, Speed}, {open, Device}]),
	    ets:new(arduino_digital, [ordered_set, protected, named_table]),
	    ets:new(arduino_analog,  [ordered_set, protected, named_table]),

	    State = #state{serial_pid = Pid, 
			   digital_conf = Digital, 
			   analog_conf = Analog,
			   digital_port_reporting_conf = DiPortReporting},
	    reset_config(State),
	    version_report_request(State),
	    {ok, State};
	{error, enoent} -> %% file not exist
	    ignore
    end.

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
% now thinking...
handle_call({call, Bin}, From, State) ->
    SerialPid = State#state.serial_pid,
    send(Bin, SerialPid),
    NewQueue = queue:in(From, State#state.process_queue),
    {noreply, State#state{process_queue = NewQueue}}.

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
handle_cast({cast, Bin}, State) ->
    SerialPid = State#state.serial_pid,
    send(Bin, SerialPid),
    {noreply, State};

handle_cast(initialize, State) ->
    State1 = init_pin(State),
    {noreply, State1};

handle_cast({dirigal_write, PortNo, Vals}, State) ->
    SerialPid = State#state.serial_pid,
    digital_write(PortNo, Vals, SerialPid),
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

%%--------------------------------------------------------------------------
%% sysex firmata data
%%--------------------------------------------------------------------------
handle_info({data, <<?SYSEX_START_CODE:8, _/binary>> = Queue},
	    #state{recv_queue = <<>>} = State) ->

    State1 = check_first_message(State),
    process_firmata_sysex(Queue, State1);

handle_info({data, Bin},
	    #state{recv_queue = <<?SYSEX_START_CODE:8, _/binary>>} = State) ->

    State1 = check_first_message(State),
    RecvQueue = State1#state.recv_queue,
    Queue = <<RecvQueue/binary, Bin/binary>>,
    process_firmata_sysex(Queue, State1);

%%--------------------------------------------------------------------------
%% normal firmata data
%%--------------------------------------------------------------------------
handle_info({data, <<>>}, #state{recv_queue = <<>>} = State) ->
    {noreply, State};

handle_info({data, Bin}, #state{recv_queue = RecvQueue} = State) ->
    State1 = check_first_message(State),
    Queue = <<RecvQueue/binary, Bin/binary>>,
    <<Code:8, TailOfTotal/binary>> = Queue,
    Size = rgpio_firmata:size(Code),

    if Size =:= unknown ->
	    io:format("code(~p) not matched in getting size~n", [Code]),
	    io:format("data = ~p~n", [Queue]),
	    erlang:error(invalid_firmata_code);
       true -> ok
    end,

    if byte_size(TailOfTotal) >= Size ->
	    <<Code:8, Body:Size/binary, TailBin/binary>> = Queue,
	    Reply = rgpio_firmata:parse(Code, Body),
	    NewState1 = handle_firmata(Reply, State1),
	    NewState2 = reply(Reply, NewState1),

	    handle_info({data, TailBin}, NewState2#state{recv_queue = <<>>});
       true ->
	    {noreply, State#state{recv_queue = Queue}}
    end;

%%--------------------------------------------------------------------------
%% unknown
%%--------------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("unknown info: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc handle firmata message from arduino.
%% @end
%%--------------------------------------------------------------------
handle_firmata({sysex, {name_and_version_report, {_, _, SketchName}}}, State) ->
    io:format("Arduino sketch: ~s~n", [SketchName]),
    State;

handle_firmata({version_report, {MeasureVer, MinorVer}}, State) ->
    io:format("Firmata version: ~w.~w~n", [MeasureVer, MinorVer]),
    State;

handle_firmata({digital_io_message, {PortNo, Status}}, State) ->
    io:format("digital: ~w:~p~n", [PortNo, Status]),
    true = ets:insert(arduino_digital, {PortNo, Status}),
    State;

handle_firmata({analog_io_message, {PinNo, Val}}, State) ->
    %%io:format("analog: ~w:~w~n", [PinNo, Val]),
    true = ets:insert(arduino_analog, {PinNo, Val}),
    State;

handle_firmata(Reply, State) ->
    io:format("haldle unknown message: ~p~n", [Reply]),
    State.

reply(Reply, State) ->
    case queue:out(State#state.process_queue) of
	{{value, From}, NewQueue} ->
	    gen_server:reply(From, Reply),
	    State#state{process_queue = NewQueue};
	{empty, _}->
	    State
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc process firmata sysex protocol.
%% @end
%%--------------------------------------------------------------------
-spec process_firmata_sysex(binary(), #state{}) -> {noreply, #state{}}.
process_firmata_sysex(Queue, State) ->
    case has_sysex_end(Queue) of
	{true, Size} ->

	    <<?SYSEX_START_CODE:8, Body:Size/binary, ?SYSEX_END_CODE:8,
	      TailBin/binary>> = Queue,

	    Reply = rgpio_firmata:parse(?SYSEX_START_CODE, Body),
	    NewState1 = reply(Reply, State),
	    NewState2 = handle_firmata(Reply, NewState1),

	    handle_info({data, TailBin}, NewState2#state{recv_queue = <<>>});
	false ->
	    {noreply, State#state{recv_queue = Queue}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc search sysex end code.
%%
%% when sysex end code is exist, return {true, Size}.
%% when not exist,               return false
%% @end
%%--------------------------------------------------------------------
-spec has_sysex_end(binary()) -> {true, non_neg_integer()} | false.
has_sysex_end(Bin) ->
    has_sysex_end(Bin, 0).

has_sysex_end(<<>>, _) ->
    false;

has_sysex_end(<<?SYSEX_END_CODE:8, _/binary>>, Size) ->
    {true, Size - 1};

has_sysex_end(<<_:8/integer, TailBin/binary>>, Size) ->
    has_sysex_end(<<TailBin/binary>>, Size + 1).

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

%% send message to serial process.
send(Bin, SerialPid) ->
    SerialPid ! {send, Bin}.

%% version report request
version_report_request(State) ->
    SerialPid = State#state.serial_pid,
    send(rgpio_firmata:format(version_report_request), SerialPid).

%% reset arduino config.
reset_config(State) ->
    SerialPid = State#state.serial_pid,
    send(rgpio_firmata:format(syste_reset), SerialPid),
    ok.

%% init all pins if init_flag is true.
check_first_message(#state{init_flag = true} = State) ->
    timer:sleep(1000),
    init_pin(State),
    State#state{init_flag = false};

check_first_message(#state{init_flag = false} = State) ->
    State.

%% set digital pin mode(in or out or analog or pwm or servo).
%% set digital pin reporting.
%% set analog  pin reporting. 
init_pin(State) ->
    io:format("Arduino pin initializing "),
    ok = reset_config(State),
    timer:sleep(10), %%一旦反映させる
    DigitalList = State#state.digital_conf,
    DigitalPortReporting = State#state.digital_port_reporting_conf,
    AnalogList = State#state.analog_conf,

    SerialPid = State#state.serial_pid,
    ok = sampling_interval(30, SerialPid),
    ok = init_pin_mode(DigitalList, SerialPid),
    ok = set_digital_port_reporting(0, DigitalPortReporting, SerialPid),
    ok = set_analog_port_reporting(AnalogList, SerialPid),
    io:format("done.~n"),
    State.

%% set all digital pin mode.
init_pin_mode([], _SerialPid) ->
    ok;

init_pin_mode([{PinNo, Mode} | Tail], SerialPid) ->
    set_digital_pin_mode(PinNo, Mode, SerialPid),
    init_pin_mode(Tail, SerialPid).

%% set digital pin mode.
set_digital_pin_mode(PinNo, Mode, SerialPid) ->
    ModeInt = case Mode of
		  in     -> 0;
		  out    -> 1;
		  analog -> 2;
		  pwm    -> 3;
		  servo  -> 4
	      end,

    Command = rgpio_firmata:format(set_pin_mode, {PinNo, ModeInt}),
    send(Command, SerialPid).

%% set digital pin to reporting.
set_digital_port_reporting(_, [],  _SerialPid) ->
    ok;

set_digital_port_reporting(PortNo, [Mode | Tail], SerialPid) ->
    Vals = [1, 1, 1, 1, 1, 1, 1, 1],
    true = ets:insert(arduino_analog, {PortNo, Vals}),
    Command = rgpio_firmata:format(set_digital_port_reporting, {PortNo, Mode}),
    send(Command, SerialPid),
    digital_write(PortNo, Vals, SerialPid),
    io:format("."),
    set_digital_port_reporting(PortNo + 1, Tail, SerialPid).

%% set analog pin to reporting.
set_analog_port_reporting([], _SerialPid) ->
    ok;

set_analog_port_reporting([PinNo | Tail], SerialPid) ->
    Command = rgpio_firmata:format(set_analogin_reporting, {PinNo, 1}),
    true = ets:insert(arduino_analog, {PinNo, 0}),
    send(Command, SerialPid),
    io:format("."),
    set_analog_port_reporting(Tail, SerialPid).

digital_write(PortNo, Vals, SerialPid) when is_list(Vals) andalso
					    length(Vals) =:= 8 ->
    Command = rgpio_firmata:format(digital_io_message, {PortNo, Vals}),
    send(Command, SerialPid).
    
sampling_interval(Interval, SerialPid) ->
    Command = rgpio_firmata:format(sysex, sampling_interval, Interval),
    send(Command, SerialPid),
    ok.
    
