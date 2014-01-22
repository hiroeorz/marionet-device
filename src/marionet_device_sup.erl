%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@hibiscus>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Nov 2013 by HIROE Shin <shin@hibiscus>
%%%-------------------------------------------------------------------
-module(marionet_device_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid} | ignore | {error, Error} when
      Pid :: pid(),
      Error :: term().
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

    {ok, ArduinoEnable} = application:get_env(arduino_enable),
    {ok, Subscribes} = application:get_env(subscribes),

    emqttc_event:add_handler(marionet_sub_event_handler, [[Subscribes]]),
    Specs = [mqtt_spec(), gpio_sup_spec(), status_spec()],

    Specs1 = case ArduinoEnable of
		 true  -> Specs ++ [arduino_sup_spec()];
		 false -> Specs
	     end,

    {ok, {SupFlags, Specs1}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

mqtt_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {ok, MqttBroker} = application:get_env(mqtt_broker),
    {emqttc, {emqttc, start_link, [MqttBroker]},
     Restart, Shutdown, Type, [emqttc]}.

arduino_sup_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    {ok, Config} = application:get_env(arduino),
    {ok, DeviceId} = application:get_env(device_id),
    Handlers = [ {marionet_device_event,  []},
		 {marionet_log_sender, [DeviceId]} ],

    {arduino_sup, {arduino_sup, start_link, [Config, Handlers]},
     Restart, Shutdown, Type, [arduino_sup]}.

gpio_sup_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    {ok, GpioList} = application:get_env(gpio),
    {ok, DeviceId} = application:get_env(device_id),
    Handlers = [ {marionet_device_event,  []},
		 {marionet_log_sender, [DeviceId]} ],

    {gpio_sup, {gpio_sup, start_link, [GpioList, Handlers]},
     Restart, Shutdown, Type, [gpio_sup]}.

status_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    {ok, GpioList} = application:get_env(gpio),

    {marionet_device_status, {marionet_device_status, start_link, [GpioList]},
     Restart, Shutdown, Type, [marionet_device_status]}.
