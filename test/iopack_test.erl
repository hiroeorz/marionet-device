-module(iopack_test).

-include_lib("eunit/include/eunit.hrl").

format_digital_io_message_test() ->
    <<16#90:8, 0:8, 0:8>> =
	iopack:format(digital_io_message, {0, [0,0,0,0,0,0,0,0]} ),

    <<16#90:8, 1:8, 1:8>> =
	iopack:format(digital_io_message, {1, [1,0,0,0,0,0,0,0]} ),

    <<16#90:8, 1:8, 128:8>> =
	iopack:format(digital_io_message, {1, [0,0,0,0,0,0,0,1]} ),

    <<16#90:8, 20:8, 17:8>> =
	iopack:format(digital_io_message, {20, [1,0,0,0,1,0,0,0]} ),

    <<16#90:8, 12:8, 255:8>> =
	iopack:format(digital_io_message, {12, [1,1,1,1,1,1,1,1]} ).

format_analog_io_message_test() ->
    <<16#E0:8, 0:8, 0:16>> = iopack:format(analog_io_message, {0, 0} ),

    <<16#E0:8, 255:8, 65535:16>> = iopack:format(analog_io_message,
						 {255, 65535} ),

    <<16#E0:8, 12:8, 10:16>> = iopack:format(analog_io_message, {12, 10} ).
    
    
