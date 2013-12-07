

# Module rgpio_arduino #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).

<a name="types"></a>

## Data Types ##




### <a name="type-pin_mode">pin_mode()</a> ###



<pre><code>
pin_mode() = in | out | analog | pwm | servo
</code></pre>





### <a name="type-serial_speed">serial_speed()</a> ###



<pre><code>
serial_speed() = 9600 | 19200 | 38400 | 57600 | 115200
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#analog-0">analog/0</a></td><td>get analog state.</td></tr><tr><td valign="top"><a href="#call-1">call/1</a></td><td></td></tr><tr><td valign="top"><a href="#cast-1">cast/1</a></td><td></td></tr><tr><td valign="top"><a href="#digital-0">digital/0</a></td><td>get digital state.</td></tr><tr><td valign="top"><a href="#digital_write-2">digital_write/2</a></td><td></td></tr><tr><td valign="top"><a href="#firmata_version_request-0">firmata_version_request/0</a></td><td>get firmata version from arduino.</td></tr><tr><td valign="top"><a href="#init_pin-1">init_pin/1</a></td><td></td></tr><tr><td valign="top"><a href="#initialize-0">initialize/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-5">start_link/5</a></td><td>Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="analog-0"></a>

### analog/0 ###

`analog() -> any()`

get analog state.
<a name="call-1"></a>

### call/1 ###

`call(Bin) -> any()`


<a name="cast-1"></a>

### cast/1 ###

`cast(Bin) -> any()`


<a name="digital-0"></a>

### digital/0 ###

`digital() -> any()`

get digital state.
<a name="digital_write-2"></a>

### digital_write/2 ###

`digital_write(PortNo, Vals) -> any()`


<a name="firmata_version_request-0"></a>

### firmata_version_request/0 ###


<pre><code>
firmata_version_request() -&gt; ok
</code></pre>

<br></br>


get firmata version from arduino.
<a name="init_pin-1"></a>

### init_pin/1 ###

`init_pin(State) -> any()`


<a name="initialize-0"></a>

### initialize/0 ###

`initialize() -> any()`


<a name="start_link-5"></a>

### start_link/5 ###


<pre><code>
start_link(Speed, Device, Digital, Analog, DiPortReporting) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Speed = <a href="#type-serial_speed">serial_speed()</a></code></li><li><code>Device = string()</code></li><li><code>Digital = [{PinNo, <a href="#type-pin_mode">pin_mode()</a>, Opts}]</code></li><li><code>PinNo = non_neg_integer()</code></li><li><code>Opts = [tuple()]</code></li><li><code>Analog = [non_neg_integer()]</code></li><li><code>DiPortReporting = [non_neg_integer()]</code></li></ul>

Starts the server
