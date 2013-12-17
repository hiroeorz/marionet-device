

# Module marionet_device_tcp #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_fsm`](gen_fsm.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connected-2">connected/2</a></td><td></td></tr><tr><td valign="top"><a href="#connected-3">connected/3</a></td><td></td></tr><tr><td valign="top"><a href="#open_connection-0">open_connection/0</a></td><td>reconnect function called by timer.</td></tr><tr><td valign="top"><a href="#send_message-1">send_message/1</a></td><td>cast data to server.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Creates a gen_fsm process which calls Module:init/1 to
initialize.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connected-2"></a>

### connected/2 ###

`connected(X1, State) -> any()`


<a name="connected-3"></a>

### connected/3 ###

`connected(X1, From, State) -> any()`


<a name="open_connection-0"></a>

### open_connection/0 ###


<pre><code>
open_connection() -&gt; ok
</code></pre>

<br></br>


reconnect function called by timer.
<a name="send_message-1"></a>

### send_message/1 ###


<pre><code>
send_message(Bin::binary()) -&gt; ok
</code></pre>

<br></br>


cast data to server.
<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(IPAddress::<a href="inet.md#type-ip_address">inet:ip_address()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<br></br>



Creates a gen_fsm process which calls Module:init/1 to
initialize. To ensure a synchronized start-up procedure, this
function does not return until Module:init/1 has returned.
