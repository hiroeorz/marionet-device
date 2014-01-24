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
-export([pack/1, unpack/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc format using msgpack
%% @end
%%--------------------------------------------------------------------
-spec pack(term()) -> binary().
pack(Obj) ->
    msgpack:pack(Obj, [{format, jsx}]).

-spec unpack(binary) -> term().
unpack(Bin) ->
    msgpack:unpack(Bin, [{format, jsx}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
