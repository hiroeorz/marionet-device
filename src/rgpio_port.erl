%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(rgpio_port).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start_poll/2,
	 pullup/1,
	 pulldown/1,
	 pullnone/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pid_dict = dict:new() :: dict(),
		c_node                :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_poll(PinNo, Mode) ->
    gen_server:call(?SERVER, {start_poll, PinNo, Mode}).    

pullup(PinNo) ->
    gen_server:call(?SERVER, {pullup_down, PinNo, pullup}).

pulldown(PinNo) ->
    gen_server:call(?SERVER, {pullup_down, PinNo, pulldown}).

pullnone(PinNo) ->
    gen_server:call(?SERVER, {pullup_down, PinNo, none}).

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
init([]) ->
    _Pid = spawn_link(fun() ->
			      os:cmd("./priv/rgpio_lib"),
			      erlang:error(port_process_down)
		      end),

    {ok, C_Node} = application:get_env(rgpio, c_node),
    {ok, #state{c_node = C_Node}}.

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
handle_call({start_poll, PinNo, Mode}, From, State) ->
    Ref = make_ref(),
    C_Node = State#state.c_node,
    {any, C_Node} ! {call, self(), Ref, {start_poll, PinNo, Mode}},

    NewDict = dict:store(Ref, From, State#state.pid_dict),
    {noreply, State#state{pid_dict = NewDict}};

handle_call({pullup_down, PinNo, Mode}, From, State) ->
    Ref = make_ref(),
    C_Node = State#state.c_node,
    {any, C_Node} ! {call, self(), Ref, {pullup_down, PinNo, Mode}},

    NewDict = dict:store(Ref, From, State#state.pid_dict),
    {noreply, State#state{pid_dict = NewDict}}.

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
handle_info({Ref, Reply}, State) ->
    {ok, From} = dict:find(Ref, State#state.pid_dict),
    NewDict = dict:erase(Ref, State#state.pid_dict),
    gen_server:reply(From, Reply),
    {noreply, State#state{pid_dict = NewDict}};

handle_info({gpio_changed, _PinNo, _Mode} = Event, State) ->
    gen_event:notify(rgpio_event, Event),
    {noreply, State};

handle_info(Info, State) ->
    io:format("unknown message recieved: ~p~n", [Info]),
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
