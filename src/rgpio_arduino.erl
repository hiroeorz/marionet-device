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
-export([start_link/2,
	 call/1,
	 cast/1,
	 firmata_version/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SYSEX_START_CODE, 16#F0).
-define(SYSEX_END_CODE,   16#F7).

-record(state, {serial_pid                  :: pid(),
		process_queue = queue:new() :: queue(),
		recv_queue = <<>>           :: binary(),
		waiting_code                :: non_neg_integer() }).

-type serial_speed() :: 9600 | 19200 | 38400 | 57600 | 115200. 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Speed, Device) -> {ok, pid()} | ignore | {error, term()} when
      Speed :: serial_speed(),
      Device :: string().
start_link(Speed, Device) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Speed, Device], []).

-spec firmata_version() -> {ok, {MeasureVersion, MinorVersion}} when
      MeasureVersion :: non_neg_integer(),
      MinorVersion :: non_neg_integer().
firmata_version() ->
    {version_report, V} = call(rgpio_firmata:format(version_report_request)),
    {ok, V}.

call(Bin) when is_binary(Bin) ->
    gen_server:call(?SERVER, {call, Bin}).

cast(Bin) when is_binary(Bin) ->
    gen_server:cast(?SERVER, {cast, Bin}).

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
init([Speed, Device]) ->
    case file:open(Device, [read]) of
	{error, eisdir} -> %% device file exist
	    Pid = serial:start([{speed, Speed}, {open, Device}]),
	    {ok, #state{serial_pid = Pid}};
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
	    #state{waiting_code = undefined, 
		   recv_queue = <<>>} = State) ->

    process_firmata_sysex(Queue, State);

handle_info({data, Bin},
	    #state{waiting_code = undefined,
		   recv_queue = <<?SYSEX_START_CODE:8, _/binary>>} = State) ->

    RecvQueue = State#state.recv_queue,
    Queue = <<RecvQueue/binary, Bin/binary>>,
    process_firmata_sysex(Queue, State);

%%--------------------------------------------------------------------------
%% normal firmata data
%%--------------------------------------------------------------------------
handle_info({data, Bin}, #state{recv_queue = RecvQueue} = State) ->
    Queue = <<RecvQueue/binary, Bin/binary>>,
    <<Code:8, TailOfTotal/binary>> = Queue,
    Size = rgpio_firmata:size(Code),

    if byte_size(TailOfTotal) >= Size ->
	    <<Code:8, Body:Size/binary, TailBin/binary>> = Queue,
	    Reply = rgpio_firmata:parse(Code, Body),
	    NewState1 = handle_firmata(Reply, State),
	    NewState2 = reply(Reply, NewState1),
	    {noreply, NewState2#state{waiting_code = undefined,
				     recv_queue = TailBin}};
       true ->
	    {noreply, State#state{waiting_code = Code, recv_queue = Queue}}
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
handle_firmata({sysex,{name_and_version_report, _}}, State) ->
    {ok, Config} = application:get_env(rgpio, arduino),
    GpioList = proplists:get_value(gpio, Config),
    MaxPortNo = (length(GpioList) div 8),
    io:format("gpio: ~p~n", [GpioList]),

    SerialPid = State#state.serial_pid,
    ok = init_digital_pin(GpioList, SerialPid),
    ok = set_digital_port_reporting(MaxPortNo, SerialPid),
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
	    io:format("queue ~p~n", [Reply]),
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
	    {noreply, NewState2#state{recv_queue = TailBin}};
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

%% set all digital pin mode.
init_digital_pin([], _SerialPid) ->
    ok;

init_digital_pin([{PinNo, Mode} | Tail], SerialPid) ->
    set_digital_pin_mode(PinNo, Mode, SerialPid),
    init_digital_pin(Tail, SerialPid).

%% set digital pin to reporting.
set_digital_port_reporting(-1, _SerialPid) ->
    ok;

set_digital_port_reporting(PortNo, SerialPid) ->
    io:format("set digital port(~p) to 1~n", [PortNo]),
    Command = rgpio_firmata:format(set_digital_port_reporting, {PortNo, 1}),
    send(Command, SerialPid),
    io:format("set digital port command:~p~n", [Command]),
    set_digital_port_reporting(PortNo - 1, SerialPid).

%% set digital pin mode.
set_digital_pin_mode(PinNo, Mode, SerialPid) ->
    ModeInt = case Mode of
		  in  -> 0;
		  out -> 1
	      end,

    io:format("set digital pin(~p) mode:~p~n", [PinNo, ModeInt]),
    Command = rgpio_firmata:format(set_pin_mode, {PinNo, ModeInt}),
    io:format("set digital pin mode command~p~n", [Command]),
    send(Command, SerialPid).
