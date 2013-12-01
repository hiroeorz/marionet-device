%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@hibiscus>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Nov 2013 by HIROE Shin <shin@hibiscus>
%%%-------------------------------------------------------------------
-module(rgpio_sup).

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
    Specs = [rgpio_event_spec(), port_spec(), rgpio_pin_sup_spec()],

    {ok, ArduinoEnable} = application:get_env(arduino_enable),

    Specs1 = case ArduinoEnable of
		 true  -> Specs ++ [arduino_spec()];
		 false -> Specs
	     end,

    {ok, {SupFlags, Specs1}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
arduino_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {ok, Arduino} = application:get_env(arduino),
    Device = proplists:get_value(device, Arduino, "/dev/ttyACM0"),
    Speed = proplists:get_value(speed, Arduino, 57600),

    {rgpio_arduino, 
     {rgpio_arduino, start_link, [Speed, Device]},
     Restart, Shutdown, Type, [rgpio_port]}.

port_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {rgpio_port, 
     {rgpio_port, start_link, []},
     Restart, Shutdown, Type, [rgpio_port]}.

rgpio_pin_sup_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,
    {ok, GpioList} = application:get_env(gpio),

    {rgpio_pin_sup, 
     {rgpio_pin_sup, start_link, [GpioList]},
     Restart, Shutdown, Type, [rgpio_pin_sup]}.

rgpio_event_spec() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,
    {ok, Handlers} = application:get_env(pin_event_handlers),

    {rgpio_event, 
     {rgpio_event, start_link, [Handlers]},
     Restart, Shutdown, Type, [rgpio_event]}.
