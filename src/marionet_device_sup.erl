%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_device_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CONFIG_FILE, ".marionet-config.db").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [config_spec(), sup2_spec()]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Child spec of marionet_config.
%% @end
%%--------------------------------------------------------------------
-spec config_spec() -> supervisor:child_spec().
config_spec() ->
    lager:debug("user config file: ~s", [conf_file()]),
    {marionet_config, {marionet_config, start_link, [conf_file()]},
     permanent, 2000, supervisor, [marionet_config]}.

%%--------------------------------------------------------------------
%% @private
%% @doc Child spec of marionet_device_sup2.
%% @end
%%--------------------------------------------------------------------
-spec sup2_spec() -> supervisor:child_spec().
sup2_spec() ->
    {marionet_device_sup2, {marionet_device_sup2, start_link, []},
     permanent, 2000, supervisor, [marionet_device_sup2]}.


%%--------------------------------------------------------------------
%% @private
%% @doc Path of config file path.
%% @end
%%--------------------------------------------------------------------
-spec conf_file() -> file:filename_all().
conf_file() ->
    filename:join(home_dir(), ?CONFIG_FILE).

%%--------------------------------------------------------------------
%% @private
%% @doc Path of user home dir.
%% @end
%%--------------------------------------------------------------------
-spec home_dir() -> string().
home_dir() ->
    os:getenv("HOME").
