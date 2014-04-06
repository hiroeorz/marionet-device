%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 21 Jan 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(marionet_data).

%% API
-export([pack_io/5, 
	 unpack_io/1]).

-export([pack/1, unpack/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Format analog status data.
%% @end
%%--------------------------------------------------------------------
-spec pack_io(Type, DeviceId, AnalogNo, Val, Opts) -> Payload when
      Type :: binary(),
      DeviceId :: binary(),
      AnalogNo :: non_neg_integer(),
      Val :: non_neg_integer(),
      Opts :: [tuple()],
      Payload :: binary().
pack_io(Type, DeviceId, AnalogNo, Val, Opts) ->
    Obj = [{type, Type},
	   {id, DeviceId},
	   {no, AnalogNo},
	   {val, Val},
	   {opts, Opts}
	  ],
    pack(Obj).

%%--------------------------------------------------------------------
%% @doc Parse analog status data.
%% @end
%%--------------------------------------------------------------------
-spec unpack_io(Payload) -> {Type, DeviceId, No, Val, Opts} when
      Payload :: binary(),
      Type :: binary(),
      DeviceId :: binary(),
      No :: non_neg_integer(),
      Val :: non_neg_integer(),
      Opts :: [tuple()].
unpack_io(Payload) ->
    Obj = unpack(Payload), <<"ai">> = proplists:get_value(<<"type">>, Obj),
    Type = proplists:get_value(<<"type">>, Obj),
    DeviceId = proplists:get_value(<<"id">>, Obj),
    No = proplists:get_value(<<"no">>, Obj),
    Val = proplists:get_value(<<"val">>, Obj),
    Opts = proplists:get_value(<<"opts">>, Obj),
    {Type, DeviceId, No, Val, Opts}.

%%--------------------------------------------------------------------
%% @doc format using msgpack
%% @end
%%--------------------------------------------------------------------
-spec pack(term()) -> binary().
pack(Obj) ->
    %%msgpack:pack(Obj, [{format, jsx}]).
    jsx:encode(Obj).

-spec unpack(binary) -> term().
unpack(Bin) ->
    %%{ok, Val} = msgpack:unpack(Bin, [{format, jsx}]),
    %%Val.
    jsx:decode(Bin).

%%%===================================================================
%%% Internal functions
%%%===================================================================
