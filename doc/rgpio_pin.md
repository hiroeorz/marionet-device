

# Module rgpio_pin #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@hibiscus`](mailto:shin@hibiscus)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#read-1">read/1</a></td><td>read gpio value.</td></tr><tr><td valign="top"><a href="#set_int-2">set_int/2</a></td><td>set interrupt that fire when gpio's input or output status is chaned.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Starts the server.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>write value to gpio.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="read-1"></a>

### read/1 ###


<pre><code>
read(PinNo) -&gt; Val
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Val = non_neg_integer()</code></li></ul>

read gpio value.
<a name="set_int-2"></a>

### set_int/2 ###


<pre><code>
set_int(PinNo, Mode) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = falling | rising | both | none</code></li><li><code>Reason = term()</code></li></ul>

set interrupt that fire when gpio's input or output status is chaned.
<a name="start-0"></a>

### start/0 ###

`start() -> any()`


<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(X1::{PinNo, Mode} | {PinNo, Mode, Edge}) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = in | out | dummy</code></li><li><code>Edge = none | rising | falling | both</code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>

Starts the server
<a name="write-2"></a>

### write/2 ###


<pre><code>
write(PinNo, Val) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Val = non_neg_integer()</code></li></ul>

write value to gpio.
