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
-export([format/2, parse/1]).

-define(MEASURE_VERSION, 0).
-define(MINOR_VERSION,   1).

-define(AES_ENCRYPTED_CODE,              16#01).
-define(CHALLENGE_REQUEST_CODE,          16#11).
-define(CHALLENGE_RESPONSE_CODE,         16#12).
-define(DIGITAL_IO_MESSAGE_CODE,         16#90).
-define(ANALOG_IO_MESSAGE_CODE,          16#E0).
-define(ANALOG_IO_PUB_MESSAGE_CODE,      16#E1).
-define(DIGITAL_IO_PUB_MESSAGE_CODE,     16#91).

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
% 0  encrypted data, 0x01
% 1  data
% 2
% .
% .
% n
%--------------------------------------------------------------------
format(aes_encrypt, {Encrypted}) when is_binary(Encrypted) ->
    <<?AES_ENCRYPTED_CODE:8, Encrypted/binary>>;

%--------------------------------------------------------------------
% challenge request 
%--------------------------------------------------------------------
% 0    challenge request, 0x11
% 1 -  16 ivec 
% 17 - 81 challenge
%--------------------------------------------------------------------
format(challenge_request, {IVec, Challenge}) when is_binary(Challenge),
						  is_binary(IVec) ->
    <<?CHALLENGE_REQUEST_CODE:8, IVec:16/binary, Challenge:64/binary>>;

%--------------------------------------------------------------------
% challenge response
%--------------------------------------------------------------------
% 0 challenge response, 0x12
% 1 AES encoded challenge (0)
% 2 AES encoded challenge (1)
% .
% .
% 65 AES encoded challenge (63)
% 66 device id most  significant 8 bits
% 67 device id       significant 8 bits
% 68 device id       significant 8 bits
% 69 device id least significant 8 bits
%--------------------------------------------------------------------
format(challenge_response, {Encoded, DeviceId}) when is_binary(Encoded),
						     is_integer(DeviceId) ->
    <<?CHALLENGE_RESPONSE_CODE:8, 
      Encoded:64/binary, DeviceId:32/big-unsigned-integer>>;

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
      Value:16/unsigned-integer >>;

%--------------------------------------------------------------------
% analog 16-bit data format for publish
%--------------------------------------------------------------------
% 0  analog pin, 0xE0
% 1  device_id most 1byte
% 2  device_id most 2byte
% 3  device_id most 3byte
% 4  device_id most 4byte
% 5  pin no 0-255
% 6  analog most  significant 8 bits
% 7  analog least significant 8 bits
%--------------------------------------------------------------------
format(analog_io_pub_message, {DeviceId, PinNo, Value}) 
  when is_integer(DeviceId), is_integer(PinNo), is_integer(Value) ->
    <<?ANALOG_IO_PUB_MESSAGE_CODE:8,
      DeviceId:32/unsigned-integer,
      PinNo:8/unsigned-integer, 
      Value:16/unsigned-integer >>;

%--------------------------------------------------------------------
% analog 16-bit data format for publish
%--------------------------------------------------------------------
% 0  analog pin, 0xE0
% 1  device_id most 1byte
% 2  device_id most 2byte
% 3  device_id most 3byte
% 4  device_id most 4byte
% 5  port no 0-255
% 6  digital pins 0-7 bitmask
%--------------------------------------------------------------------
format(digital_io_pub_message, 
       {DeviceId, Port, [X0, X1, X2, X3, X4, X5, X6, X7]}) 
  when is_integer(DeviceId), is_integer(Port) ->
    <<?DIGITAL_IO_PUB_MESSAGE_CODE:8,
      DeviceId:32/unsigned-integer,
      Port:8/unsigned-integer,
      X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1>>.

%%--------------------------------------------------------------------
%% @doc create binary data that formatted by firmata protocol format.
%% @end
%%--------------------------------------------------------------------
-spec parse(binary()) -> {FormatName, tuple()} when
      FormatName :: atom().

parse(<<?CHALLENGE_REQUEST_CODE:8, IVec:16/binary, Challenge:64/binary>>) ->
    {challenge_request, {IVec, Challenge}};

parse(<<?ANALOG_IO_MESSAGE_CODE:8,
	PinNo:8/unsigned-integer, Value:16/unsigned-integer >>) ->
    {analog_io_message, {PinNo, Value}};

parse(<<?ANALOG_IO_PUB_MESSAGE_CODE:8,
	DeviceId:32/unsigned-integer,
	PinNo:8/unsigned-integer, Value:16/unsigned-integer >>) ->
    {analog_io_pub_message, {DeviceId, PinNo, Value}};

parse(<<?DIGITAL_IO_MESSAGE_CODE:8,
	Port:8/unsigned-integer,
	X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1>>) ->
    DigitalList = [X0, X1, X2, X3, X4, X5, X6, X7],
    {digital_io_message, {Port, DigitalList}};

parse(<<?DIGITAL_IO_PUB_MESSAGE_CODE:8,
	DeviceId:32/unsigned-integer,
	Port:8/unsigned-integer,
	X7:1, X6:1, X5:1, X4:1, X3:1, X2:1, X1:1, X0:1>>) ->
    DigitalList = [X0, X1, X2, X3, X4, X5, X6, X7],
    {digital_io_pub_message, {DeviceId, Port, DigitalList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
