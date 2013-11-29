-module(rgpio_firmata_test).

-include_lib("eunit/include/eunit.hrl").

format_digital_io_message_test() ->
    <<16#90:8, 0:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,0,0,0,0,0,0,0]} ),

    <<16#90:8, 1:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [1,0,0,0,0,0,0,0]} ),
    
    <<16#90:8, 2:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,1,0,0,0,0,0,0]} ),

    <<16#90:8, 3:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [1,1,0,0,0,0,0,0]} ),

    <<16#90:8, 4:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,0,1,0,0,0,0,0]} ),

    <<16#90:8, 5:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [1,0,1,0,0,0,0,0]} ),

    <<16#90:8, 6:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,1,1,0,0,0,0,0]} ),

    <<16#90:8, 7:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [1,1,1,0,0,0,0,0]} ),

    <<16#90:8, 8:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,0,0,1,0,0,0,0]} ),

    <<16#90:8, 9:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [1,0,0,1,0,0,0,0]} ),

    <<16#90:8, 10:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,1,0,1,0,0,0,0]} ),

    <<16#90:8, 11:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [1,1,0,1,0,0,0,0]} ),

    <<16#90:8, 12:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,0,1,1,0,0,0,0]} ),

    <<16#90:8, 13:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [1,0,1,1,0,0,0,0]} ),
    
    <<16#90:8, 14:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,1,1,1,0,0,0,0]} ),

    <<16#90:8, 15:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [1,1,1,1,0,0,0,0]} ),

    <<16#90:8, 16:8, 0:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,0,0,0,1,0,0,0]} ),

    <<16#90:8, 16:8, 1:8>> = 
	rgpio_firmata:format(digital_io_message, {0, [0,0,0,0,1,0,0,1]} ),

    %% port no

    <<16#91:8, 16:8, 1:8>> = 
	rgpio_firmata:format(digital_io_message, {1, [0,0,0,0,1,0,0,1]} ),

    <<16#92:8, 16:8, 1:8>> = 
	rgpio_firmata:format(digital_io_message, {2, [0,0,0,0,1,0,0,1]} ).

format_analog_io_message_test() ->
    <<16#E0:8, 1:8, 0:8>> = rgpio_firmata:format(analog_io_message, {0, 1} ),
    <<16#E0:8, 2:8, 0:8>> = rgpio_firmata:format(analog_io_message, {0, 2} ),
    <<16#E0:8, 0:8, 1:8>> = rgpio_firmata:format(analog_io_message, {0, 128} ),
    <<16#E0:8, 1:8, 1:8>> = rgpio_firmata:format(analog_io_message, {0, 129} ),

    %% pin no

    <<16#E1:8, 1:8, 1:8>> = rgpio_firmata:format(analog_io_message, {1, 129} ),
    <<16#E2:8, 1:8, 1:8>> = rgpio_firmata:format(analog_io_message, {2, 129} ).

    
    
    
