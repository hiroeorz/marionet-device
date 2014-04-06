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
-export([pack_analog/3, pack_digital/3]).
-export([pack/1, unpack/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc format digital status data.
%% @end
%%--------------------------------------------------------------------
-spec pack_analog(DeviceId, AnalogNo, Val) -> Payload when
      DeviceId :: binary(),
      AnalogNo :: non_neg_integer(),
      Val :: non_neg_integer(),
      Payload :: binary().
pack_analog(DeviceId, AnalogNo, Val) ->
    Obj = [{type, <<"ai">>},
	   {id, DeviceId},
	   {no, AnalogNo},
	   {status, Val}],
    pack(Obj).

%%--------------------------------------------------------------------
%% @doc format digital status data.
%% @end
%%--------------------------------------------------------------------
-spec pack_digital(DeviceId, PortNo, Status) -> Payload when
      DeviceId :: binary(),
      PortNo :: non_neg_integer(),
      Status :: [1|0],
      Payload :: binary().
pack_digital(DeviceId, PortNo, Status) ->
    Obj = [{type, <<"di">>},
	   {id, DeviceId},
	   {no, PortNo},
	   {status, Status}],
    pack(Obj).

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
