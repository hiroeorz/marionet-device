

# Module rgpio_port #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#pulldown-1">pulldown/1</a></td><td></td></tr><tr><td valign="top"><a href="#pullnone-1">pullnone/1</a></td><td></td></tr><tr><td valign="top"><a href="#pullup-1">pullup/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#start_poll-2">start_poll/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="pulldown-1"></a>

### pulldown/1 ###

`pulldown(PinNo) -> any()`


<a name="pullnone-1"></a>

### pullnone/1 ###

`pullnone(PinNo) -> any()`


<a name="pullup-1"></a>

### pullup/1 ###

`pullup(PinNo) -> any()`


<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<br></br>



Starts the server

<a name="start_poll-2"></a>

### start_poll/2 ###

`start_poll(PinNo, Mode) -> any()`


