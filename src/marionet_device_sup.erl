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
    {ok, OmronFinsEnable} = application:get_env(omron_fins_enable),
    {ok, IOEventHandler} = application:get_env(io_event_handler),
    {ok, SubEventHandler} = application:get_env(subscribe_event_handler),
    {ok, Subscribes} = application:get_env(subscribes),
    {ok, GroupId} = application:get_env(group_id),
    {ok, DeviceId} = application:get_env(device_id),

    Specs = [gpio_sup_spec(),
	     mqtt_spec(),
	     status_spec(),
	     event_sup_spec(emqttc_event, SubEventHandler, Subscribes),
	     event_sup_spec(gpio_pin_event, marionet_device_event, []),
	     event_sup_spec(gpio_pin_event, IOEventHandler, [GroupId, DeviceId])
	    ],

    Specs1 = 
	case ArduinoEnable of
	    true  -> 
		{ok, DeviceId} = application:get_env(device_id),
		Specs ++ [arduino_sup_spec(),
			  event_sup_spec(arduino_event,
					 marionet_device_event, []),
			  event_sup_spec(arduino_event,
					 IOEventHandler, [GroupId, DeviceId]) ];
		 false -> 
		     Specs
	     end,

    Specs2 = 
	case OmronFinsEnable of
	    true  -> 
		Specs1 ++ [fins_port_spec(),
			   fins_event_spec(IOEventHandler),
			   fins_watcher_spec()];
	    false -> Specs1
	end,

    {ok, {SupFlags, Specs2}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

event_sup_spec(EventManager, EventHandler, Args) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Handler = {EventManager, EventHandler, Args},

    {{marionet_event_sup, Handler},
     {marionet_event_sup, start_link, [Handler]},
     Restart, Shutdown, Type, [marionet_event_sup]}.

fins_port_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {ok, Config} = application:get_env(omron_fins),
    IPAddress = proplists:get_value(ip_address, Config),
    Port = proplists:get_value(port, Config, 9600),

    {omron_fins_port, {omron_fins, start_port, [IPAddress, Port]},
     Restart, Shutdown, Type, [omron_fins_port]}.

fins_event_spec(IOEventHandler) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {ok, DeviceId} = application:get_env(device_id),
    Handlers = [ {marionet_device_event,  []},
		 {IOEventHandler, [DeviceId]} ],

    {omron_fins_event, {omron_fins_event, start_link, [Handlers]},
     Restart, Shutdown, Type, [omron_fins_event]}.

fins_watcher_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {ok, Config} = application:get_env(omron_fins),

    {omron_fins_watcher, {omron_fins_watcher, start_link, [Config]},
     Restart, Shutdown, Type, [omron_fins_watcher]}.

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
    %%{ok, DeviceId} = application:get_env(device_id),
    %%Handlers = [ {marionet_device_event,  []},
    %%		 {IOEventHandler, [DeviceId]} ],

    {arduino_sup, {arduino_sup, start_link, [Config, []]},
     Restart, Shutdown, Type, [arduino_sup]}.

gpio_sup_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    {ok, GpioList} = application:get_env(gpio),
    %%{ok, DeviceId} = application:get_env(device_id),
    %%   Handlers = [ {marionet_device_event,  []},
    %%		 {IOEventHandler, [DeviceId]} ],

    {gpio_sup, {gpio_sup, start_link, [GpioList, []]},
     Restart, Shutdown, Type, [gpio_sup]}.

status_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    {ok, GpioList} = application:get_env(gpio),

    {marionet_device_status, {marionet_device_status, start_link, [GpioList]},
     Restart, Shutdown, Type, [marionet_device_status]}.
