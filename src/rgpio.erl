%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@hibiscus>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Nov 2013 by HIROE Shin <shin@hibiscus>
%%%-------------------------------------------------------------------
-module(rgpio).

%% API
-export([start/0]).
-export([read/1,
	 write/2,
	 set_int/2]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(rgpio).

%%--------------------------------------------------------------------
%% @doc read gpio value.
%% @end
%%--------------------------------------------------------------------
-spec read(PinNo) -> Val when
      PinNo :: non_neg_integer(),
      Val :: non_neg_integer().
read(PinNo) when is_integer(PinNo) ->
    rgpio_pin:read(PinNo).

%%--------------------------------------------------------------------
%% @doc write value to gpio.
%% @end
%%--------------------------------------------------------------------
-spec write(PinNo, Val) -> ok when
      PinNo :: non_neg_integer(),
      Val :: non_neg_integer().      
write(PinNo, Val) when is_integer(PinNo) andalso is_integer(Val) ->
    rgpio_pin:write(PinNo, Val).

%%--------------------------------------------------------------------
%% @doc set interrupt that fire when gpio's input or output status is chaned.
%% @end
%%--------------------------------------------------------------------
-spec set_int(PinNo, Mode) -> ok | {error, Reason} when
      PinNo :: non_neg_integer(),
      Mode :: falling | rising | both | none,
      Reason :: term().
set_int(PinNo, Mode) ->
    rgpio_pin:set_int(PinNo, Mode).
