-module(rgpio_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, GpioList} = application:get_env(gpio),
    rgpio_sup:start_link(GpioList).

stop(_State) ->
    ok.
