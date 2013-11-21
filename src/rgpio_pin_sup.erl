%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 19 Nov 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(rgpio_pin_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
-spec start_link(IOList) -> {ok, Pid} | ignore | {error, Error} when
      IOList :: [ {non_neg_integer(), in | out} ], 
      Pid :: pid(),
      Error :: term().
start_link(IOList) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [IOList]).

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
init([IOList]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, child_list(IOList)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child_list(IOList) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    L1 = [ {{rgpio_pin, PinNo}, 
	    {rgpio_pin, start_link, [{PinNo, Mode, Opts}]},
	    Restart, Shutdown, Type, [rgpio]}
	   || {PinNo, Mode, Opts} <- IOList ],

    L2 = [ {{rgpio_pin, PinNo}, 
	    {rgpio_pin, start_link, [{PinNo, Mode, []}]},
	    Restart, Shutdown, Type, [rgpio]}
	   || {PinNo, Mode} <- IOList ],

    L1 ++ L2.
    
