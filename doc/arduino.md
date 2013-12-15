

# Module arduino #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_analog-0">all_analog/0</a></td><td>get analog state.</td></tr><tr><td valign="top"><a href="#all_digital-0">all_digital/0</a></td><td>get digital state.</td></tr><tr><td valign="top"><a href="#cast-1">cast/1</a></td><td>send request to arduino.</td></tr><tr><td valign="top"><a href="#digital_write-2">digital_write/2</a></td><td></td></tr><tr><td valign="top"><a href="#firmata_version_request-0">firmata_version_request/0</a></td><td>get firmata version from arduino.</td></tr><tr><td valign="top"><a href="#init_pin-1">init_pin/1</a></td><td></td></tr><tr><td valign="top"><a href="#initialize-0">initialize/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the server.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_analog-0"></a>

### all_analog/0 ###

`all_analog() -> any()`

get analog state.
<a name="all_digital-0"></a>

### all_digital/0 ###

`all_digital() -> any()`

get digital state.
<a name="cast-1"></a>

### cast/1 ###


<pre><code>
cast(Bin::binary()) -&gt; ok
</code></pre>

<br></br>


send request to arduino
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


<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<br></br>


Starts the server
<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(Conf) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Conf = [tuple()]</code></li></ul>


