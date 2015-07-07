

# Module marionet_zmq_server #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2014, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-0">close/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-0"></a>

### close/0 ###

`close() -> any()`


<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(PlcAddress, PlcPort) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>
<br />


Starts the server

<a name="start_link-3"></a>

### start_link/3 ###

`start_link(EndPoint, PlcAddress, PlcPort) -> any()`


