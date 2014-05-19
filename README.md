# MarioNetDevice   [![Build Status](https://travis-ci.org/hiroeorz/marionet-device.svg?branch=master)](https://travis-ci.org/hiroeorz/marionet-device)

## Goal

* Provide a MQTT Client Application That executed on RaspberryPi (with arduino if you need).

## Demo

[![ScreenShot](https://dl.dropboxusercontent.com/u/24342163/mqtt_demo.png)](http://youtu.be/j3Vmd_o24oU)

## Getting Started

Download MarioNetDevice on your RaspberryPi.

```
$ wget https://dl.dropboxusercontent.com/u/24342163/marionet-device/marionet-device-001.tar.gz
$ tar xvzf marionet-device-001.tar.gz
```

If your have a second RaspberryPi, Download MarioNetDevice second build.

```
$ wget https://dl.dropboxusercontent.com/u/24342163/marionet-device/marionet-device-002.tar.gz
$ tar xvzf marionet-device-002.tar.gz
```
 
## Running

You need run marionet-device as root.

first RaspberryPi.

```
$ sudo ./marionet-device-001/bin/marionet-device-001 start
```

second RaspberryPi(If you have).

```
$ sudo ./marionet-device-002/bin/marionet-device-002 start
```

##  Settings

Defualt setting is [here](https://github.com/hiroeorz/marionet-device/blob/master/rel/marionet-device-001/files/sys.config)

You can change pin setting by modifing above file, if you need.
Your application's setting file is 

```
$ vi ./marionet-device-001/releases/1/sys.config
```

### MQTT connection settings

marionet-device connet to "test.mosquitto.org" in default. If you want to connect to your own MQTT broker, change "host" setting in "mqtt_broker".

```erl-sh
{mqtt_broker, [{host, "test.mosquitto.org"},
               {port, 1883},
               {client_id, <<"demo/pi001">>}, %% len < 24
               {username, undefined},
               {password, undefined}
              ]},
```

### General Purpose I/O

Marionet-device send self status to a broker. Status is digital status and analog values.

* Digital status send to server when status is chaned (e.g.: Switch status changes OFF to ON).

Default settings is

```
{gpio, [
           { 4, in,  [{edge, both}, {pull, up}, {active_low, true}] },
           {17, in,  [{edge, both}, {pull, up}, {active_low, true}] },
           {18, in,  [{edge, both}, {pull, up}, {active_low, true}] },
           {22, in,  [{edge, both}, {pull, up}, {active_low, true}] },
           {23, in,  [{edge, both}, {pull, up}, {active_low, true}] },
           {24, out, [{edge, both}, {pull, down}] },
           {25, out, [{edge, both}, {pull, down}] }
          ]},
```

In default setting, gpio 4,17,18,22,23 are digital input mode, 24,25 are digital output mode.

## Arduino I/O

You can read write Arduino I/O throuh the Firmata protocol.
You need to send sketch 'Standard Firmata' to your Arduino or implementation Firmata Protocol your own sketch, before start up the marionet.

```
   {arduino_enable, true},
   {arduino, [{speed, 57600},
              {device, "/dev/ttyACM0"},
              {sampling_interval, 3000},
              {digital_port_reporting, [1, 0]},
              {digital_port_offset, 1},
              {analog_offset, 0},
              {analog, [0, 1] },
              {digital, [
                         { 0, in, [{pull, up} ]},
                         { 1, in, [{pull, up} ]},
                         { 2, in, [{pull, up} ]},
                         { 3, in, [{pull, up} ]},
                         { 4, in, [{pull, up} ]},
                         { 5, in, [{pull, up} ]},
                         { 6, in, [{pull, up} ]},
                         { 7, in, [{pull, up} ]},

                         { 8, servo},
                         { 9, servo},
                         {10, pwm},
                         {11, pwm},
                         {12, out},
                         {13, out}
                        ]
              }
             ]},
```

In default setting,
* 0,1,2,3,4,5,6,7 are digital input mode.
* 8,9 are servo.
* 10,11 is pwm.
* 12,13 is output.


## Build from source.

Log in your RaspberryPi and fetch the latest version of MarioNetDevice using git.

```
$ git clone https://github.com/hiroeorz/marionet-device.git
$ cd marionet-device
$ make
$ make generate
```


## Todo

- Omron plc extension (use omron-fins-erlang).
