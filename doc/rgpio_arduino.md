

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




### <a name="type-serial_speed">serial_speed()</a> ###



<pre><code>
serial_speed() = 9600 | 19200 | 38400 | 57600 | 115200
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-1">call/1</a></td><td></td></tr><tr><td valign="top"><a href="#cast-1">cast/1</a></td><td></td></tr><tr><td valign="top"><a href="#firmata_version-0">firmata_version/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-1"></a>

### call/1 ###

`call(Bin) -> any()`


<a name="cast-1"></a>

### cast/1 ###

`cast(Bin) -> any()`


<a name="firmata_version-0"></a>

### firmata_version/0 ###


<pre><code>
firmata_version() -&gt; {ok, {MeasureVersion, MinorVersion}}
</code></pre>

<ul class="definitions"><li><code>MeasureVersion = non_neg_integer()</code></li><li><code>MinorVersion = non_neg_integer()</code></li></ul>


<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(Speed, Device) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Speed = <a href="#type-serial_speed">serial_speed()</a></code></li><li><code>Device = string()</code></li></ul>


Starts the server

