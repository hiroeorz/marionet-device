-module(marionet_device_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:load(arduino),
    application:load(gpio),
    start_http(),
    marionet_device_sup:start_link().

stop(_State) ->
    ok.

start_http() ->
    Routes = [
	      {"/settings/[...]", cowboy_static, 
	       {priv_dir, marionet_device, "www",
		[{mimetypes, cow_mimetypes, all}]}},

	      {"/api/[...]", marionet_http_handler, []}
	     ],

    Dispatch = cowboy_router:compile([ {'_', Routes} ]),
    {ok, _} = cowboy:start_http(http, 10, [{port, 8080}],
				[ {env, [{dispatch, Dispatch}]} ]),
    ok.
