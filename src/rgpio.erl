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
-export([set_pin_mode/2,
	 read/1,
	 write/2,
	 set_int/2,
	 pullup/1,
	 pulldown/1,
	 pullnone/1,
	 get_active_low/1,
	 set_active_low/2,
	 add_event_handler/1,
	 status/0,
	 packed_status/0]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(rgpio).

%%--------------------------------------------------------------------
%% @doc set pin mode, in or out.
%% @end
%%--------------------------------------------------------------------
-spec set_pin_mode(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: rgpio_pin:edge().
set_pin_mode(PinNo, Mode) ->
    rgpio_pin:set_pin_mode(PinNo, Mode).

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
      Mode :: rgpio_pin:edge(),
      Reason :: term().
set_int(PinNo, Mode) ->
    rgpio_pin:set_int(PinNo, Mode).

%%--------------------------------------------------------------------
%% @doc set pullup to a pin.
%%
%% RaspberryPi内蔵のプルアップ抵抗を用いてpinをプルアップ有りに設定します
%% 入力無しの状態で常時3.3Vの電圧がかかり、GNDと接地された場合のみ0となります
%% @end
%%--------------------------------------------------------------------
-spec pullup(PinNo) -> ok when
      PinNo :: non_neg_integer().
pullup(PinNo) ->
    rgpio_pin:pullup(PinNo).

%%--------------------------------------------------------------------
%% @doc set pulldown to a pin.
%%
%% RaspberryPi内蔵のプルダウン抵抗を用いてpinをプルダウン有りに設定します
%% 入力無しの状態で常時GND接地の0.0Vとなり、3.3Vと短絡された場合のみ1となります
%% @end
%%--------------------------------------------------------------------
-spec pulldown(PinNo) -> ok when
      PinNo :: non_neg_integer().
pulldown(PinNo) ->
    rgpio_pin:pulldown(PinNo).

%%--------------------------------------------------------------------
%% @doc release pin mode from pullup pulldown.
%%
%% RaspberryPi内蔵のプルアップ、プルダウン抵抗を用いません
%% 入力無しの状態では不安定な電圧となり、外部回路でプルアップまたはプルダウンが必要です
%% @end
%%--------------------------------------------------------------------
-spec pullnone(PinNo) -> ok when
      PinNo :: non_neg_integer().
pullnone(PinNo) ->
    rgpio_pin:pullnone(PinNo).

%%--------------------------------------------------------------------
%% @doc get active low from a pin.
%%
%% Mode=0: 通電時のread/1の結果は 通電->1 解放->0 (デフォルト)
%% Mode=1: 通電時のread/1の結果は 通電->0 解放->1
%% @end
%%--------------------------------------------------------------------
-spec get_active_low(PinNo) -> ok when
      PinNo :: non_neg_integer().
get_active_low(PinNo) ->
    rgpio_pin:get_active_low(PinNo).

%%--------------------------------------------------------------------
%% @doc set active low to a pin.
%%
%% Mode=1: active_lowを1に設定して、通電->0 解放->1 となるようにビット反転します
%% Mode=0: active_lowを0に設定して、通電->1 解放->0 となるようにします
%% @end
%%--------------------------------------------------------------------
-spec set_active_low(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: rgpio_pin:mode().
set_active_low(PinNo, Mode) ->
    rgpio_pin:set_active_low(PinNo, Mode).

%%--------------------------------------------------------------------
%% @doc add event handler to gen_event.
%%
%% event fired when gpio's input or output status is chaned.
%% @end
%%--------------------------------------------------------------------
-spec add_event_handler(Module) -> ok | {'EXIT', Reason} | term() when
      Module :: atom(),
      Reason :: term().
add_event_handler(Module) ->
    rgpio_event:add_handler(Module).

%%--------------------------------------------------------------------
%% @doc get status list.
%%
%% example: [0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0]
%% @end
%%--------------------------------------------------------------------
-spec status() -> [ 1 | 0 ].
status() ->
    {ok, GpioList} = application:get_env(rgpio, gpio),
    status(GpioList, []).

status([], Result) ->
    lists:reverse(Result);

status([{PinNo, _Mode, _Opts} | Tail], Result) ->
    status(Tail, [rgpio:read(PinNo) | Result]).

%%--------------------------------------------------------------------
%% @doc get status list that packed to 16bit unsigned integer.
%%
%% example: [16390,0]
%%
%% 主にサーバーへの送信データサイズを減らす為の関数。
%% 各pinの状態を16点毎にまとめて16bit整数のリストを返します。
%% RaspberryPiのGPIO点数では必要ありませんが、PLCなどもっと点数の多いものと一緒に
%% 使われたときに型を合わせる用。
%% 16ビットに足りない分は0で埋めます。
%% @end
%%--------------------------------------------------------------------
-spec packed_status() -> [non_neg_integer()].
packed_status() ->
    List = case status() of
	       Status1 when (length(Status1) rem 16) =:= 0  ->
		   Status1;
	       Status2 ->
		   Rem = length(Status2) rem 16,
		   Status2 ++ lists:map(fun(_) -> 0 end, lists:seq(1, 16 - Rem))
	   end,

    packed_status(List, []).

packed_status([], Result) ->
    lists:reverse(Result);

packed_status([X1,  X2,  X3,  X4,  X5,  X6,  X7,  X8,
	       X9, X10, X11, X12, X13, X14, X15, X16 | Tail], Result) ->
    <<Int:16/big-unsigned-integer>> = <<X16:1/integer,
					X15:1/integer,
					X14:1/integer,
					X13:1/integer,
					X12:1/integer,
					X11:1/integer,
					X10:1/integer,
					X9:1/integer,
					X8:1/integer,
					X7:1/integer,
					X6:1/integer,
					X5:1/integer,
					X4:1/integer,
					X3:1/integer,
					X2:1/integer,
					X1:1/integer>>,

    packed_status(Tail, [Int | Result]).
