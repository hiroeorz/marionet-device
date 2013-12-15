

# Module marionet_device_status #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_digital-0">all_digital/0</a></td><td>get all digital state.</td></tr><tr><td valign="top"><a href="#analog-1">analog/1</a></td><td>get digital state of port.</td></tr><tr><td valign="top"><a href="#digital-1">digital/1</a></td><td>get digital state of port.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Starts the server.</td></tr><tr><td valign="top"><a href="#update_analog_value-2">update_analog_value/2</a></td><td>update analog value(14bit).</td></tr><tr><td valign="top"><a href="#update_digital_port-2">update_digital_port/2</a></td><td>update digital port(8bit).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_digital-0"></a>

### all_digital/0 ###


<pre><code>
all_digital() -&gt; [0 | 1]
</code></pre>

<br></br>


get all digital state.
<a name="analog-1"></a>

### analog/1 ###


<pre><code>
analog(PinNo) -&gt; [0 | 1] | undefined
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>

get digital state of port.
<a name="digital-1"></a>

### digital/1 ###


<pre><code>
digital(PortNo) -&gt; [0 | 1] | undefined
</code></pre>

<ul class="definitions"><li><code>PortNo = non_neg_integer()</code></li></ul>

get digital state of port.
<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(Gpio) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Gpio = [tuple()]</code></li></ul>

Starts the server
<a name="update_analog_value-2"></a>

### update_analog_value/2 ###


<pre><code>
update_analog_value(PinNo, Value) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Value = non_neg_integer()</code></li></ul>

update analog value(14bit).
<a name="update_digital_port-2"></a>

### update_digital_port/2 ###


<pre><code>
update_digital_port(PortNo, Status) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PortNo = non_neg_integer()</code></li><li><code>Status = [0 | 1]</code></li></ul>

update digital port(8bit).
