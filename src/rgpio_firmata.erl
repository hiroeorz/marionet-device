%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 29 Nov 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(rgpio_firmata).

%% API
-export([format/2]).

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
% two byte digital data format, 
% second nibble of byte 0 gives the port number
% (e.g. 0x92 is the third port, port 2)
%--------------------------------------------------------------------
% 0  digital data, 0x90-0x9F, (MIDI NoteOn, but different data format)
% 1  digital pins 0-6 bitmask
% 2  digital pin 7 bitmask 
%--------------------------------------------------------------------
format(digital_io_message, {Port, [X0, X1, X2, X3, X4, X5, X6, X7]}) ->
    Code = 16#90 + Port,
    <<Code:8/unsigned-integer,
      0:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1,
      0:7, X7:1 >>;

%--------------------------------------------------------------------
% analog 14-bit data format
%--------------------------------------------------------------------
% 0  analog pin, 0xE0-0xEF, (MIDI Pitch Wheel)
% 1  analog least significant 7 bits
% 2  analog most significant 7 bits
%--------------------------------------------------------------------
format(analog_io_message, {PinNo, Value}) when is_integer(Value) ->
    Code = 16#E0 + PinNo,
    <<0:1, 0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1,
      X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1 >> 
	= <<Value:16/big-unsigned-integer>>,
    
    <<Code:8/unsigned-integer,
      0:1, X6:1,  X5:1,  X4:1,  X3:1,  X2:1, X1:1, X0:1,
      0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1, X7:1 >>.

%%%===================================================================
%%% Internal functions
%%%===================================================================
