

# Module rgpio_serial #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#send-1">send/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="send-1"></a>

### send/1 ###

`send(Bin) -> any()`


<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(Speed, Device) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Speed = <a href="#type-serial_speed">serial_speed()</a></code></li><li><code>Device = string()</code></li></ul>


Starts the server
