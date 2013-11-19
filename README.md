RaspberryPiGPIO
===============

RaspberryPi GPIO reader, writer, setinterrupter.

Goals
-------------------------------------------------

RaspberryPiGPIO is provide a complete GPIO handler in a small code base.


Getting Started
-------------------------------------------------

Log in your RaspberryPi and fetch the latest version of RaspberryPiGPIO using git.

    $ git clone https://github.com/hiroeorz/RaspberryPiGPIO.git
    $ cd RaspberryPiGPIO
    $ make


Running
-------------------------------------------------

check [default setting of gpio](https://github.com/hiroeorz/RaspberryPiGPIO/blob/master/src/rgpio.app.src).

In default setting, under gpio22 is input pin, other is output pin.

You can change pin setting by modifing above file, if you need.

Start application.

    $ ./start-dev
    
    > rgpio:start().


Read gpio

    rgpio:read(18).
    0

Write

    rgpio:write(18, 1).
    rgpio:read(18).
    1
    
    rgpio:write(18, 0).
    rgpio:read(18).
    0
    

Pullup or pulldown

    rgpio:pullup(18).
    rgpio:pulldown(18).
    rgpio:pullnone(18).

Set interrupt
 
    rgpio:set_int(18, rising).
    rgpio:set_int(18, falling).
    rgpio:set_int(18, both).
    rgpio:set_int(18, none).

Add event handler

    rgpio:add_event_handler(sample_module).

The sample_module is event handler befavior of gen_event.
If gpio18 set interrupt rising and pin status changed 0 to 1 , called event handler.

Get all status list

    rgpio:status().
    [0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0]

Get all status that packed 16bit integer.

    rgpio:packed_status().
    [16390,0]

Todo
-------------------------------------------------

* test
* http rest interface.
* and the other...
