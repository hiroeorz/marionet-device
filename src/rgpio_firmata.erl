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
-export([size/1, parse/2, format/1, format/2]).

-define(MEASURE_VERSION, 2).
-define(MINOR_VERSION,   3).

-define(DIGITAL_IO_MESSAGE_CODE,         16#90).
-define(ANALOG_IO_MESSAGE_CODE,          16#E0).
-define(SET_PIN_MODE_CODE,               16#F4).
-define(SET_ANALOG_PIN_REPORTING_CODE,   16#C0).
-define(SET_DIGITAL_PORT_REPORTING_CODE, 16#D0).
-define(SYSEX_START_CODE,                16#F0).
-define(SYSEX_END_CODE,                  16#F7).
-define(VERSION_REPORT_CODE,             16#F9).

-define(SYSEX_NAME_AND_VERSION_CODE, 16#79).


%%%===================================================================
%%% API
%%%===================================================================
-spec size(Code) -> non_neg_integer() | in_sysex when
      Code :: non_neg_integer().

%% digital message
size(Code) when 16#90 =< Code , Code =< 16#9F ->
    2;

%% analog message
size(Code) when 16#E0 =< Code , Code =< 16#EF->
    2;

%% version report
size(?VERSION_REPORT_CODE) ->
    2;

%% set pin mode
size(?SET_PIN_MODE_CODE) ->
    3;

%% toggle analogin reporting by pin
size(Code) when 16#C0 =< Code , Code =< 16#CF ->
    1;

%% toggle digital reporting by port
size(Code) when 16#D0 =< Code , Code =< 16#DF ->
    1;

%% version report
size(?VERSION_REPORT_CODE) ->
    0;

%% sysex start
size(?SYSEX_START_CODE) ->
    in_sysex.

parse(?VERSION_REPORT_CODE, <<MeasureVer:8, MinorVer:8 >>) ->
    {version_report, {MeasureVer, MinorVer}};

parse(Code, Bin) when 16#90 =< Code , Code =< 16#9F ->
    PortNo = Code - ?DIGITAL_IO_MESSAGE_CODE,

    <<0:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1,
      0:7, X7:1>> = Bin,

    {digital_io_message, {PortNo, [X0, X1, X2, X3, X4, X5, X6, X7]}};

parse(?SYSEX_START_CODE, Bin) ->
    {sysex, parse_sysex(Bin)}.

%%--------------------------------------------------------------------
%% @doc parse sysex data
%% @end
%%--------------------------------------------------------------------
-spec parse_sysex(binary()) -> {sysex, {Name, term()}} when
      Name :: atom().

%--------------------------------------------------------------------
% Receive Firmware Name and Version (after query)
%--------------------------------------------------------------------
%  0  START_SYSEX (0xF0)
%  1  queryFirmware (0x79)
%  2  major version (0-127)
%  3  minor version (0-127)
%  4  first 0-6 bit of firmware name
%  5  second 7bit of firmware name @fix
%  x  ...for as many bytes as it needs)
%  6  END_SYSEX (0xF7)
%--------------------------------------------------------------------
parse_sysex(<<?SYSEX_NAME_AND_VERSION_CODE:8,
	    MeasureVer:8/integer, MinorVer:8/integer, Bin/binary>>) ->
    Name = bit7_to_bit8(Bin),
    {name_and_version_report, {MeasureVer, MinorVer, Name}}.

%%--------------------------------------------------------------------
%% @doc create binary data that formatted by firmata protocol format.
%% @end
%%--------------------------------------------------------------------
-spec format(FormatName) -> binary() when
      FormatName :: atom().

%--------------------------------------------------------------------
 % request version report
%--------------------------------------------------------------------
% 0 request version report (0xF9) (MIDI Undefined)
%--------------------------------------------------------------------
format(version_report_request) ->
    <<?VERSION_REPORT_CODE:8 >>;

%--------------------------------------------------------------------
% version report format
%--------------------------------------------------------------------
% 0  version report header (0xF9) (MIDI Undefined)
% 1  major version (0-127)
% 2  minor version (0-127)
%--------------------------------------------------------------------
format(version_report) ->
    <<?VERSION_REPORT_CODE:8, ?MEASURE_VERSION:8, ?MINOR_VERSION:8 >>.

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
format(digital_io_message, {Port, [X0, X1, X2, X3, X4, X5, X6, X7]})
  when is_integer(Port) ->
    Code = ?DIGITAL_IO_MESSAGE_CODE + Port,
    <<Code:8,
      0:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1,
      0:7, X7:1 >>;

%--------------------------------------------------------------------
% analog 14-bit data format
%--------------------------------------------------------------------
% 0  analog pin, 0xE0-0xEF, (MIDI Pitch Wheel)
% 1  analog least significant 7 bits
% 2  analog most significant 7 bits
%--------------------------------------------------------------------
format(analog_io_message, {PinNo, Value}) when is_integer(PinNo),
					       is_integer(Value) ->
    <<0:1, 0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1,
      X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1 >> 
	= <<Value:16/big-unsigned-integer>>,
    
    Code = ?ANALOG_IO_MESSAGE_CODE + PinNo,

    <<Code:8,
      0:1, X6:1,  X5:1,  X4:1,  X3:1,  X2:1, X1:1, X0:1,
      0:1, X13:1, X12:1, X11:1, X10:1, X9:1, X8:1, X7:1 >>;

%--------------------------------------------------------------------
% set pin mode
%--------------------------------------------------------------------
% 1  set digital pin mode (0xF4) (MIDI Undefined)
% 2  pin number (0-127)
% 3  state (INPUT/OUTPUT/ANALOG/PWM/SERVO, 0/1/2/3/4)
%--------------------------------------------------------------------
format(set_pin_mode, {PinNo, State}) when is_integer(PinNo),
					  (State >= 0 andalso State =< 4) ->
    <<?SET_PIN_MODE_CODE:8, PinNo:8, State:8 >>;

%--------------------------------------------------------------------
% toggle analogIn reporting by pin
%--------------------------------------------------------------------
% 0  toggle analogIn reporting (0xC0-0xCF) (MIDI Program Change)
% 1  disable(0)/enable(non-zero) 
%--------------------------------------------------------------------
format(set_analogin_reporting, {PinNo, Enable}) when is_integer(PinNo),
						     is_integer(Enable) ->
    Code = ?SET_ANALOG_PIN_REPORTING_CODE + PinNo,
    <<Code:8, Enable:8 >>;

%--------------------------------------------------------------------
% toggle digital port reporting by port (second nibble of byte 0),
%   e.g. 0xD1 is port 1 is pins 8 to 15,  
%--------------------------------------------------------------------
% 0  toggle digital port reporting (0xD0-0xDF) (MIDI Aftertouch)
% 1  disable(0)/enable(non-zero) 
%--------------------------------------------------------------------
format(set_digital_port_reporting, {PortNo, Enable}) when is_integer(PortNo),
							  is_integer(Enable) ->
    Code = ?SET_DIGITAL_PORT_REPORTING_CODE + PortNo,
    <<Code:8, Enable:8 >>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 7bit binary to 8bit binary.
%%
%% 07654321 00000008 -> 87654321
%% @end
%%--------------------------------------------------------------------
bit7_to_bit8(Bin) ->
    bit7_to_bit8(Bin, []).

bit7_to_bit8(<<>>, Result) ->
    list_to_binary(lists:reverse(Result));

bit7_to_bit8(<<P1:1/binary, P2:1/binary, Tail/binary>>, Result) ->
    <<_:1, X16:1, X15:1, X14:1, X13:1, X12:1, X11:1, X10:1>> = P1,
    <<_:1,   _:1,   _:1,   _:1,   _:1,   _:1,   _:1, X20:1>> = P2,
    Byte = <<X20:1, X16:1, X15:1, X14:1, X13:1, X12:1, X11:1, X10:1>>,    
    bit7_to_bit8(Tail, [Byte | Result]).
