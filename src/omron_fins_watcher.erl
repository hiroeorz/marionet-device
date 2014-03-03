%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 23 Jan 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(omron_fins_watcher).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([check_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {dst_address                :: inet:ip_address(),
		port                       :: inet:port_number(),
		timer_ref                  :: timer:tref(),
		analog_offset         = 0  :: non_neg_integer(),
		digital_port_offset   = 0  :: non_neg_integer(),
		analog_dm_list        = [] :: [non_neg_integer()],
		digital_dm_list       = [] :: [non_neg_integer()],
		old_digital_list      = [] :: [non_neg_integer()] }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Config) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

check_status() ->
    gen_server:cast(?SERVER, check_status).

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
init([Config]) ->
    DstAddress = proplists:get_value(dst_address, Config),
    Port = proplists:get_value(port, Config, 9600),
    AnalogOffset = proplists:get_value(analog_offset, Config, 0),
    DigitalOffset = proplists:get_value(digital_port_offset, Config, 0),
    AnalogNoList = proplists:get_value(analog, Config, []),
    DigitalNoList = proplists:get_value(digital, Config, []),
    Interval = proplists:get_value(check_interval, Config, 2900),

    {ok, TRef} = timer:apply_interval(Interval, ?MODULE, check_status, []),
    {ok, #state{dst_address = DstAddress, port = Port, timer_ref = TRef,
		analog_offset=AnalogOffset, digital_port_offset=DigitalOffset,
		analog_dm_list=AnalogNoList, digital_dm_list=DigitalNoList}}.

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
handle_cast(check_status, State) ->
    check_analog_status(State),
    State1 = check_digital_status(State),
    {noreply, State1};

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

check_analog_status(_State=#state{dst_address=DstAddress, port=Port,
				  analog_dm_list=DmList,
				  analog_offset=Offset}) ->
    {ok, ValList} = omron_fins:read_dm_multi_values(DstAddress, Port, DmList),
    ok = report_analog(Offset, ValList).

report_analog(_No, []) ->
    ok;

report_analog(No, [Val | Tail]) ->
    gen_event:notify(omron_fins_event, {analog_recv, No, Val}),
    report_analog(No + 1, Tail).

check_digital_status(State=#state{dst_address=DstAddress, port=Port,
				  digital_dm_list=DmList,
				  digital_port_offset=Offset,
				  old_digital_list=OldValList}) ->
    {ok, ValList} = omron_fins:read_dm_multi_values(DstAddress, Port, DmList),
    ok = report_digital(Offset, ValList, OldValList),
    State#state{old_digital_list = ValList}.

report_digital(_No, [], _) ->
    ok;

report_digital(No, [Val | Tail], [Val | TailOld]) ->
    report_digital(No + 2, Tail, TailOld);

report_digital(No, [Val | Tail], []) ->
    Least = Val div 256,
    Status1 = bit_list(Least),
    gen_event:notify(omron_fins_event, {digital_port_changed, No, Status1}),
    Most = Val rem 256,
    Status2 = bit_list(Most),
    gen_event:notify(omron_fins_event, {digital_port_changed, No+1, Status2}),
    report_digital(No + 2, Tail, []);

report_digital(No, [Val | Tail], [OldVal | TailOld]) ->
    Least = Val div 256,
    OldLeast = OldVal div 256,

    if Least =/= OldLeast ->
	    Status1 = bit_list(Least),
	    gen_event:notify(omron_fins_event,
			     {digital_port_changed, No, Status1});
       true ->
	    ok
    end,

    Most = Val rem 256,
    OldMost = OldVal rem 256,

    if Most =/= OldMost ->
	    Status2 = bit_list(Most),
	    gen_event:notify(omron_fins_event,
			     {digital_port_changed, No+1, Status2});
       true ->
	    ok
    end,

    report_digital(No + 2, Tail, TailOld).

bit_list(Num) when is_integer(Num) ->
    <<X8:1, X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1>> = <<Num:8/integer>>,
    [X1, X2, X3, X4, X5, X6, X7, X8].
