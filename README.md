# MarioNetDevice

## Goals

* Provide a logger interface, that send digital,analog log to a remote server.
* Provide a remote control interface from a server.
* Provide a M2M connection interface to your embeded application.

**CAUTION**

Now, MarioNetDevice supported RaspberryPi (and Arduino extension) only, I will support other devices, that running general GNU/Linux operating system. 

## Getting Started

Log in your RaspberryPi and fetch the latest version of MarioNetDevice using git.

```
$ git clone https://github.com/hiroeorz/MarioNetDevice.git
$ cd MarioNetDevice
$ make
```

Or add "deps" line to your app's rebar.conf.

```erlang
{marionet_device, ".*", {git, "https://github.com/hiroeorz/marionet_device.git",
   {branch, "master"}}},

```

and get deps

```
$ ./rebar get-deps
```

## Running

check [default setting of marionet_device](blob/master/src/marionet_device.app.src).

In default setting, gpio 25,27 is output pin, other is input pin.

You can change pin setting by modifing above file, if you need.

### Start application.

You must run MarioNetDevice as root. Because MarioNetDevice access to /dev/mem. /dev/mem is allowed writable access from root only.

```
$ sudo ./start-dev
```
-------
or start erlang mode as root

```
$ sudo erl -pa ebin deps/*/ebin -boot start_sasl -sname marionet_device \
       -setcookie marionet -s marionet_device start
```
and start MarioNetDevice in erl shell.

```erl-sh
1> application:start(marionet_device).
```

MarioNetDevice will connect to a TCP server when start up Application.
(Default server IP address is '127.0.0.1')

And send self status to a server. Status is digital status and analog values.

* Digital status send to server when status is chaned (e.g.: Switch status changes OFF to ON).
* Analog values send to server when every 0.3 sec.

## General Purpose I/O

### Read gpio value

```erl-sh
1> gpio_pin:read(18).
0
```

### Write value to gpio

```erl-sh
1> gpio_pin:write(25, 1).
ok
3> gpio_pin:write(25, 0).
ok
```    

### Change Pin mode

```erl-sh
1> gpio_pin:set_pin_mode(18, out).
ok
2> gpio_pin:set_pin_mode(25, in).
ok
```

### Pullup or pulldown

```erl-sh
1> gpio_pin:pullup(18).
ok
2> gpio_pin:pulldown(18).
ok
3> gpio_pin:pullnone(18).
ok
```

### Set interrupt
 
```erl-sh
1> gpio_pin:set_int(18, rising).
ok
2> gpio_pin:set_int(18, falling).
ok
3> gpio_pin:set_int(18, both).
ok
4> gpio_pin:set_int(18, none).
ok
```
### Get active low
 
```erl-sh
1> gpio_pin:get_active_low(4).
0
```

### Set active low

```erl-sh
1> gpio_pin:set_active_low(25, 1).
```

example:

```erl-sh
1> gpio_pin:write(25, 1).
ok
2> gpio_pin:read(25).
1
3> gpio_pin:set_active_low(25, 1).
ok
2> gpio_pin:read(25).
0
```

### Add event handler of gpio pin

```erl-sh
1> gpio_pin_event:add_event_handler(sample_module, []).
ok
```
* First argument is module name.
* Second argument is arguments of sample_module:init/1.

The sample_module is event handler befavior of gen_event.
If gpio18 set interrupt rising and pin status changed 0 to 1 , called event handler.

This is sample event handler.
[gpio_pin_event_logger.erl](blob/master/src/gpio_pin_event.erl)


### Get all status list

```erl-sh
1> gpio_pin:status().
[1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
```

## Arduino I/O

You can read write Arduino I/O throuh the Firmata protocol.
You need to send sketch 'Standard Firmata' to your Arduino or implementation Firmata Protocol your own, before start up the marionet.

In default setting, gpio 0,1,2,3,4,5,6,7 is digital input mode,
   8, 9 is digital output mode,
  10,11 is pwm,
  12,13 is servo.

### Get all digital state.

```erl-sh
1> arduino:all_digital()
[1,1,0,0,0,0,0,1]
```

### Get all analog value.

```erl-sh
1> arduino:all_analog().
[343, 211, 375, 111, 0, 343]
```

### Write digital state.

PortNO is unit number of every 8 bit digital status.

```erl-sh
1> arduino:digital_write(0, [1,1,1,1,1,1,1,1]).
ok
```

### Add event handler of arduino

```erl-sh
1> arduino_event:add_event_handler(sample_module, []).
ok
```

* First argument is module name.
* Second argument is arguments of sample_module:init/1.

The sample_module is event handler befavior of gen_event.

This is sample event handler.
[gpio_pin_event_logger.erl](blob/master/src/arduino_event_logger.erl)


## Todo

- Define protocol format between a remote server.
- Test.
- Connecting to m2m server.
- Omron Fins extension.
- and the other...
