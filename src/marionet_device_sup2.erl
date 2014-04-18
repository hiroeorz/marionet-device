%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@hibiscus>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Nov 2013 by HIROE Shin <shin@hibiscus>
%%%-------------------------------------------------------------------
-module(marionet_device_sup2).

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

    ArduinoEnable = marionet_config:get(arduino_enable),
    OmronFinsEnable = marionet_config:get(omron_fins_enable),
    IOEventHandler = marionet_config:get(io_event_handler),
    SubEventHandler = marionet_config:get(subscribe_event_handler),
    Subscribes = marionet_config:get(subscribes),
    GroupId = marionet_config:get(group_id),
    DeviceId = marionet_config:get(device_id),
    AnalogPubInterval = marionet_config:get(analog_publish_interval),
    MqttEnable = marionet_config:get(mqtt_enable),

    GpioSpecs = [gpio_sup_spec()],

    MqttSpecs = 
	case MqttEnable of
	    true  -> [mqtt_spec()];
	    false -> []
	end,

    EventSpecs = [status_spec(),
		  zmq_server_spec(),
		  event_sup_spec(emqttc_event, SubEventHandler, [Subscribes]),
		  event_sup_spec(emqttc_event, marionet_sub_event_handler,
				 [Subscribes]),
		  event_sup_spec(gpio_pin_event, marionet_device_event, []),
		  event_sup_spec(gpio_pin_event, IOEventHandler,
				 [GroupId, DeviceId, AnalogPubInterval])
		 ],

    ArduinoSpecs = 
	case ArduinoEnable of
	    true  -> 
		[arduino_sup_spec(),
		 event_sup_spec(arduino_event, marionet_device_event, []),
		 event_sup_spec(arduino_event, IOEventHandler, 
				[GroupId, DeviceId, AnalogPubInterval])
		];
		 false -> 
		     []
	     end,

    FinsSpecs = 
	case OmronFinsEnable of
	    true  -> 
		[fins_port_spec(),
		 fins_event_spec(),
		 event_sup_spec(omron_fins_event, IOEventHandler,
				[GroupId, DeviceId, AnalogPubInterval]),
		 fins_watcher_spec()
		];
	    false ->
		[]
	end,

    {ok, {SupFlags, 
	  GpioSpecs ++ MqttSpecs ++ EventSpecs ++ ArduinoSpecs ++ FinsSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

zmq_server_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Config = marionet_config:get(omron_fins),
    IPAddress = proplists:get_value(ip_address, Config),
    Port = proplists:get_value(port, Config, 9600),

    {marionet_zmq_server, {marionet_zmq_server, start_link, [IPAddress, Port]},
     Restart, Shutdown, Type, [marionet_zmq_server]}.

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

    Config = marionet_config:get(omron_fins),
    IPAddress = proplists:get_value(ip_address, Config),
    Port = proplists:get_value(port, Config, 9600),

    {omron_fins_port, {omron_fins_port, start_link, [IPAddress, Port]},
     Restart, Shutdown, Type, [omron_fins_port]}.

fins_event_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    
    {omron_fins_event, {omron_fins_event, start_link, [ [] ]},
     Restart, Shutdown, Type, [omron_fins_event]}.

fins_watcher_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Config = marionet_config:get(omron_fins),

    {omron_fins_watcher, {omron_fins_watcher, start_link, [Config]},
     Restart, Shutdown, Type, [omron_fins_watcher]}.

mqtt_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    MqttBroker = marionet_config:get(mqtt_broker),
    {emqttc, {emqttc, start_link, [MqttBroker]},
     Restart, Shutdown, Type, [emqttc]}.

arduino_sup_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    Config = marionet_config:get(arduino),

    {arduino_sup, {arduino_sup, start_link, [Config, []]},
     Restart, Shutdown, Type, [arduino_sup]}.

gpio_sup_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    GpioList = marionet_config:get(gpio),
    AnalogList = marionet_config:get(analog_list),
    AnalogInterval = marionet_config:get(analog_interval),

    {gpio_sup, {gpio_sup, start_link, 
		[GpioList, AnalogList, AnalogInterval, []]},
     Restart, Shutdown, Type, [gpio_sup]}.

status_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    GpioList = marionet_config:get(gpio),

    {marionet_device_status, {marionet_device_status, start_link, [GpioList]},
     Restart, Shutdown, Type, [marionet_device_status]}.
