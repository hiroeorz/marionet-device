%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 19 Nov 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(gpio_pin_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(IOList,  EventHandlers) -> {ok, Pid}     |
					    ignore        |
					    {error, Error} when
      IOList :: [ {non_neg_integer(), in | out} ],
      EventHandlers :: [atom()],
      Pid :: pid(),
      Error :: term().
start_link(IOList,  EventHandlers) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [IOList, EventHandlers]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([IOList, EventHandlers]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    Childs = 
	[port_spec()] ++ child_list(IOList) ++
	[db_child(IOList), event_spec(EventHandlers)],

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

event_spec(EventHandlers) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {gpio_pin_event, {gpio_pin_event, start_link, [EventHandlers]},
     Restart, Shutdown, Type, [gpio_port]}.

port_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {gpio_port, {gpio_port, start_link, []},
     Restart, Shutdown, Type, [gpio_port]}.

db_child(IOList) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {gpio_pin_db, {gpio_pin_db, start_link, [IOList]},
     Restart, Shutdown, Type, [gpio_pin_db]}.

child_list(IOList) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    L1 = [ {{gpio_pin, PinNo}, {gpio_pin, start_link, [{PinNo, Mode, Opts}]},
	    Restart, Shutdown, Type, [gpio_pin]}
	   || {PinNo, Mode, Opts} <- IOList ],

    L2 = [ {{gpio_pin, PinNo}, {gpio_pin, start_link, [{PinNo, Mode, []}]},
	    Restart, Shutdown, Type, [gpio_pin]}
	   || {PinNo, Mode} <- IOList ],

    L1 ++ L2.
    
