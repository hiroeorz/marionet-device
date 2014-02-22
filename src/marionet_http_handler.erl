%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 19 Feb 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_http_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([get/2, update/2]).
-export([allowed_methods/2]).

%%%===================================================================
%%% REST API
%%%===================================================================

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    lager:debug("Req: ~p", [Req]),
    {[ {<<"application/json">>, get} ], Req, State}.

content_types_accepted(Req, State) ->
    {[ {'*', update} ], Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update(Req, State) ->
    {[<<"config">>, ResourceName], Req1} = cowboy_req:path_info(Req),
    {ok, [{Json, true}], Req2} = cowboy_req:body_qs(8000000, Req1),
    Obj = marionet_json:decode(Json),
    io:format("body: ~p~n", [Obj]),
    ok = update_resource(ResourceName, Obj, Req, State),
    {true, Req2, State}.

update_resource(<<"base.json">>, Obj, _Req, _State) ->
    lists:foreach(fun({Key, Val}) ->
			  ok = marionet_config:set(Key, Val)
		  end, Obj);

update_resource(<<"mqtt_broker.json">>, Obj, _Req, _State) ->
    lists:foreach(fun({Key, Val}) ->
			  ok = marionet_config:set(Key, Val)
		  end, Obj);

update_resource(<<"gpio.json">>, Obj, _Req, _State) ->
    PinNo = proplists:get_value(<<"pin_no">>, Obj),
    Mode = list_to_existing_atom(
	     binary_to_list(proplists:get_value(<<"mode">>, Obj, <<"in">>))
	    ),

    BinOpts = proplists:get_value(<<"opts">>, Obj, []),
    Opts = atom_opt(BinOpts),
    Edge = proplists:get_value(<<"edge">>, Opts),
    Pull = proplists:get_value(<<"pull">>, Opts),
    ActiveLow = proplists:get_value(<<"active_low">>, Opts),

    Val = {PinNo, Mode,  [{edge, Edge}, {pull, Pull}, {active_low, ActiveLow}]},

    GpioList = marionet_config:get(gpio),
    NewGpioList = lists:map(fun({No, _, _}) when No =:= PinNo -> Val;
			       ({_, _, _} = E) -> E
			    end, GpioList),
    io:format("GPIO: ~p~n", [NewGpioList]),
    ok = marionet_config:set(<<"gpio">>, NewGpioList).

get(Req, State) ->
    {[<<"config">>, ResourceName], Req1} = cowboy_req:path_info(Req),
    get_resource(ResourceName, Req1, State).

get_resource(<<"base.json">>, Req, State) ->
    Obj = [{device_id, marionet_config:get(device_id)},
	   {group_id,  marionet_config:get(group_id)}
	  ],
    {marionet_json:encode(Obj), Req, State};

get_resource(<<"mqtt_broker.json">>, Req, State) ->
    Mqtt = marionet_config:get(mqtt_broker),
    io:format("mqtt: ~p~n", [Mqtt]),
    Obj = [{client_id, proplists:get_value(client_id, Mqtt)},
	   {host, list_to_binary(proplists:get_value(host, Mqtt))},
	   {port, proplists:get_value(port, Mqtt)}
	  ],
    {marionet_json:encode(Obj), Req, State};

get_resource(<<"subscribes.json">>, Req, State) ->
    Obj = [{subscribes, marionet_config:get(subscribes)}],
    {marionet_json:encode(Obj), Req, State};

get_resource(<<"gpio.json">>, Req, State) ->
    GpioList = marionet_config:get(gpio),

    Obj = lists:map(fun({PinNo, Mode, Opts}) ->
			    [{pin_no, PinNo}, 
			     {mode, list_to_binary(atom_to_list(Mode))},
			     {opts, binary_opt(Opts)}]
		    end, GpioList),

    {marionet_json:encode(Obj), Req, State};

get_resource(<<"arduino.json">>, Req, State) ->
    Enable = marionet_config:get(arduino_enable),
    Arduino = marionet_config:get(arduino),
    Speed = proplists:get_value(speed, Arduino),
    Device = proplists:get_value(device, Arduino),
    SamplingInterval = proplists:get_value(sampling_interval, Arduino),
    DiReporting = proplists:get_value(digital_port_reporting, Arduino),
    DiPortOffset = proplists:get_value(digital_port_offset, Arduino),
    AiOffset = proplists:get_value(analog_offset, Arduino),
    Analog = proplists:get_value(analog, Arduino),
    Digital = proplists:get_value(digital, Arduino),

    io:format("digital: ~p~n", [Digital]),

    Digital1 = lists:map(fun({PinNo, Mode, Opts}) ->
				 [{pin_no, PinNo}, 
				  {mode, list_to_binary(atom_to_list(Mode))},
				  {opts, binary_opt(Opts)}];
			    ({PinNo, Mode})->
				 [{pin_no, PinNo}, 
				  {mode, list_to_binary(atom_to_list(Mode))},
				  {opts, []}]
			 end, Digital),
    Obj = [
	   {arduino_enable, Enable},
	   {speed, Speed},
	   {device, list_to_binary(Device)},
	   {sampling_interval, SamplingInterval},
	   {digital_port_reporting, DiReporting},
	   {digital_port_offset, DiPortOffset},
	   {analog_offset, AiOffset},
	   {analog, Analog},
	   {digital, Digital1}
	  ],

    {marionet_json:encode(Obj), Req, State}.

atom_opt(List) ->
    atom_opt(List, []).

atom_opt([], Result) ->
    lists:reverse(Result);    

%% boolean() or undefined or null.
atom_opt([{Name, AtomVal} | Tail], Result) when is_atom(AtomVal) ->
    atom_opt(Tail, [{Name, AtomVal} | Result]);

atom_opt([{Name, BinVal} | Tail], Result) ->
    AtomVal = list_to_existing_atom(binary_to_list(BinVal)),
    atom_opt(Tail, [{Name, AtomVal} | Result]).

binary_opt(List) ->
    binary_opt(List, []).

binary_opt([], Result) ->
    lists:reverse(Result);

%% boolean() or undefined or null not change to binary.
binary_opt([{Name, Atom} | Tail], Result) when Atom =:= true;
					       Atom =:= false;
					       Atom =:= undefined;
					       Atom =:= null ->
    binary_opt(Tail, [{Name, Atom} | Result]);

binary_opt([{Name, Atom} | Tail], Result) ->
    binary_opt(Tail, [{Name, list_to_binary(atom_to_list(Atom))} | Result]).
