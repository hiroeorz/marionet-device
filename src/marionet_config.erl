%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_config).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 get/1,
	 set/2,
	 get_all_keys/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {dets_name :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(FileName) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(FileName) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FileName], []).

%%--------------------------------------------------------------------
%% @doc Get application config
%% @end
%%--------------------------------------------------------------------
-spec get(Key) -> Val | undefined when
      Key :: term(),
      Val :: term().
get(Key) when is_atom(Key) ->
    gen_server:call(?SERVER, {get, Key}).

%%--------------------------------------------------------------------
%% @doc Set application config
%% @end
%%--------------------------------------------------------------------
-spec set(Key, Val) -> ok when
      Key :: term(),
      Val :: term().
set(Key, Val) ->
    gen_server:cast(?SERVER, {set, Key, Val}).

%%--------------------------------------------------------------------
%% @doc Get all config keys.
%% @end
%%--------------------------------------------------------------------
-spec get_all_keys() -> [ {atom(), term()} ].
get_all_keys() ->
    gen_server:call(?SERVER, {get_all_keys}).
    
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
init([FileName]) ->
    _ = ets:new(config_ets, [named_table, private]),
    {ok, _} = dets:open_file(FileName, []),
    load_config(FileName),
    {ok, #state{dets_name = FileName}}.

load_config(FileName) ->
    case dets:first(FileName) of
	'$end_of_table' ->
	    ok;
	Key ->
	    load_config(FileName, Key)
    end.

load_config(FileName, Key) ->
    [{Key, Val}] = dets:lookup(FileName, Key),
    true = ets:insert(config_ets, {Key, Val}),
    case dets:next(FileName, Key) of
	'$end_of_table' ->
	    ok;
	NextKey ->
	    load_config(FileName, NextKey)
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
handle_call({get, Key}, _From, State) ->
    KeyBin = list_to_binary(atom_to_list(Key)),
    io:format("search config: ~p~n", [KeyBin]),

    case ets:lookup(config_ets, KeyBin) of
	[{_, Val}] = A -> 
	    io:format("-----~n"),
	    io:format("RESULT: ~p~n", [A]),
	    io:format("-----config from db(key=~p)~n", [Key]),
	    io:format("~p~n", [Val]),
	    {reply, Val, State};
	[] ->
	    case application:get_env(marionet_device, Key) of
		{ok, Val1} -> 
		    {reply, Val1, State};
		undefined ->
		    {reply, undefined, State}
	    end
    end;

handle_call({get_all_keys}, _From, State) ->
    {ok, AllKeys} = application:get_all_key(marionet_device),
    Env = proplists:get_value(env, AllKeys),
    {reply, Env, State}.

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
handle_cast({set, Key, Val}, #state{dets_name = Name} = State) ->
    ok = dets:insert(Name, {Key, Val}),
    true = ets:insert(config_ets, {Key, Val}),
    io:format("config saved: ~p~n", [{Key, Val}]),
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
    dets:close(config_dets),
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
