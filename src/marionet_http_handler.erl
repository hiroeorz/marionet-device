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
%%% REST
%%%===================================================================

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
    ObjList = lists:map(fun({Topic, Qos}) ->
				[{topic, Topic}, {qos, Qos}]
			end, marionet_config:get(subscribes)),

    {marionet_json:encode([{subscribes, ObjList}]), Req, State};

get_resource(<<"gpio.json">>, Req, State) ->
    GpioList = marionet_config:get(gpio),

    Obj = lists:map(fun({PinNo, Mode, Opts}) ->
			    [{pin_no, PinNo}, 
			     {mode, list_to_binary(atom_to_list(Mode))},
			     {opts, binary_opt(Opts)}]
		    end, GpioList),

    {marionet_json:encode([{gpio, Obj}]), Req, State};

get_resource(<<"arduino.json">>, Req, State) ->
    Enable = marionet_config:get(arduino_enable),
    Arduino = marionet_config:get(arduino),
    Speed = proplists:get_value(speed, Arduino),
    SamplingInterval = proplists:get_value(sampling_interval, Arduino),
    DiReporting = proplists:get_value(digital_port_reporting, Arduino),
    DiPortOffset = proplists:get_value(digital_port_offset, Arduino),
    AiOffset = proplists:get_value(analog_offset, Arduino),
    Analog = proplists:get_value(analog, Arduino),
    Digital = proplists:get_value(digital, Arduino),

    Device = case proplists:get_value(device, Arduino) of
		 DBin when is_binary(DBin) ->
		     DBin;
		 DList when is_list(DList) ->
		     list_to_binary(DList)
	     end,
 
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
	   {device, Device},
	   {sampling_interval, SamplingInterval},
	   {digital_port_reporting, DiReporting},
	   {digital_port_offset, DiPortOffset},
	   {analog_offset, AiOffset},
	   {analog, Analog},
	   {digital, Digital1}
	  ],

    {marionet_json:encode(Obj), Req, State};

get_resource(<<"omron_fins.json">>, Req, State) ->
    Enable = marionet_config:get(omron_fins_enable),
    OmronFins = marionet_config:get(omron_fins),
    Obj = [{omron_fins_enable, Enable}, {omron_fins, OmronFins}],
    {marionet_json:encode(Obj), Req, State}.

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

update_resource(<<"subscribes.json">>, Obj, _Req, _State) ->
    NewSubList = proplists:get_value(<<"subscribes">>, Obj),
    NewSubList1 = lists:map(fun(Sub) ->
				    T = proplists:get_value(<<"topic">>, Sub),
				    Q = proplists:get_value(<<"qos">>, Sub),
				    {T, Q}
			    end, NewSubList),

    ok = marionet_config:set(<<"subscribes">>, NewSubList1);

update_resource(<<"gpio.json">>, Obj, _Req, _State) ->
    GpioList = proplists:get_value(<<"gpio">>, Obj),

    NewGpioList = 
	lists:map(fun(Gpio) ->
			  PinNo = proplists:get_value(<<"pin_no">>, Gpio),
			  ModeBin = proplists:get_value(<<"mode">>, Gpio), 
			  Mode = list_to_existing_atom(binary_to_list(ModeBin)),
			  OptsBin = proplists:get_value(<<"opts">>, Gpio, []),
			  Opts = atom_opt(OptsBin),
			  Edge = proplists:get_value(<<"edge">>, Opts),
			  Pull = proplists:get_value(<<"pull">>, Opts),
			  ActiveLow = proplists:get_value(<<"active_low">>,
							  Opts),
			  {PinNo, Mode,  [{edge, Edge},
					  {pull, Pull}, 
					  {active_low, ActiveLow}]}
		  end, GpioList),

    ok = marionet_config:set(<<"gpio">>, NewGpioList);

update_resource(<<"arduino.json">>, Obj, _Req, _State) ->
    DigitalList = proplists:get_value(<<"digital">>, Obj),

    NewDigitalList = 
	lists:map(fun(Gpio) ->
			  PinNo = proplists:get_value(<<"pin_no">>, Gpio),
			  ModeBin = proplists:get_value(<<"mode">>, Gpio), 
			  Mode = list_to_existing_atom(binary_to_list(ModeBin)),
			  OptsBin = proplists:get_value(<<"opts">>, Gpio, []),
			  Opts = atom_opt(OptsBin),
			  Pull = proplists:get_value(<<"pull">>, Opts),

			  case Pull of
			      undefined ->
				  {PinNo, Mode};
			      P when P =:= up; P =:= none ->
				  {PinNo, Mode,  [{pull, Pull}]}
			      end				  
		  end, DigitalList),

    NewConf = [{ speed, proplists:get_value(<<"speed">>, Obj) },
	       { device, proplists:get_value(<<"device">>, Obj) },

	       { sampling_interval, 
		 proplists:get_value(<<"sampling_interval">>, Obj) },

	       { digital_port_reporting,
		 proplists:get_value(<<"digital_port_reporting">>, Obj) },

	       { digital_port_offset, 
		 proplists:get_value(<<"digital_port_offset">>, Obj) },

	       { analog_offset, proplists:get_value(<<"analog_offset">>, Obj) },
	       { analog, proplists:get_value(<<"analog">>, Obj)  },
	       { digital, NewDigitalList}
	      ],
    ok = marionet_config:set(<<"arduino">>, NewConf),
    
    ArduinoEnable = proplists:get_value(<<"arduino_enable">>, Obj),
    ok = marionet_config:set(<<"arduino_enable">>, ArduinoEnable);

update_resource(<<"omron_fins.json">>, Obj, _Req, _State) ->
    FinsEnable = proplists:get_value(<<"omron_fins_enable">>, Obj),
    Conf = proplists:get_value(<<"omron_fins">>, Obj),

    NewConf = [{ ip_address, 
		 proplists:get_value(<<"ip_address">>, Conf) },

	       { dst_address, 
		 proplists:get_value(<<"dst_address">>, Conf) },

	       { port, 
		 proplists:get_value(<<"port">>, Conf) },

	       { check_interval, 
		 proplists:get_value(<<"check_interval">>, Conf) },

	       { analog_offset, 
		 proplists:get_value(<<"analog_offset">>, Conf) },

	       { digital_port_offset, 
		 proplists:get_value(<<"digital_port_offset">>, Conf) },

	       { analog,
		 proplists:get_value(<<"analog">>, Conf) },

	       { digital,
		 proplists:get_value(<<"digital">>, Conf) }
	      ],

    ok = marionet_config:set(<<"omron_fins">>, NewConf),
    ok = marionet_config:set(<<"omron_fins_enable">>, FinsEnable).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
