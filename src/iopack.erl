%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 16 Dec 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(iopack).

%% API
-export([format/2]).

-define(MEASURE_VERSION, 0).
-define(MINOR_VERSION,   1).

-define(DIGITAL_IO_MESSAGE_CODE,         16#90).
-define(ANALOG_IO_MESSAGE_CODE,          16#E0).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc create binary data that formatted by firmata protocol format.
%% @end
%%--------------------------------------------------------------------
-spec format(FormatName, tuple()) -> binary() when
      FormatName :: atom().

%--------------------------------------------------------------------
% 1 byte digital data format, 
%--------------------------------------------------------------------
% 0  digital data, 0x90
% 1  port no 0-255
% 2  digital pins 0-7 bitmask
%--------------------------------------------------------------------
format(digital_io_message, {Port, [X0, X1, X2, X3, X4, X5, X6, X7]})
  when is_integer(Port) ->
    
    <<?DIGITAL_IO_MESSAGE_CODE:8,
      Port:8/unsigned-integer,
      X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1>>;

%--------------------------------------------------------------------
% analog 16-bit data format
%--------------------------------------------------------------------
% 0  analog pin, 0xE0
% 1  pin no 0-255
% 2  analog most  significant 8 bits
% 3  analog least significant 8 bits
%--------------------------------------------------------------------
format(analog_io_message, {PinNo, Value}) when is_integer(PinNo),
					       is_integer(Value) ->
    <<?ANALOG_IO_MESSAGE_CODE:8, 
      PinNo:8/unsigned-integer, 
      Value:16/unsigned-integer >>.

%%%===================================================================
%%% Internal functions
%%%===================================================================
