

# Module rgpio_pin #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@hibiscus`](mailto:shin@hibiscus)).

<a name="types"></a>

## Data Types ##




### <a name="type-edge">edge()</a> ###



<pre><code>
edge() = falling | rising | both | none
</code></pre>





### <a name="type-mode">mode()</a> ###



<pre><code>
mode() = in | out
</code></pre>





### <a name="type-pull">pull()</a> ###



<pre><code>
pull() = up | down | none
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_digital-0">all_digital/0</a></td><td>get status list.</td></tr><tr><td valign="top"><a href="#get_active_low-1">get_active_low/1</a></td><td>get active low to from a pin.</td></tr><tr><td valign="top"><a href="#pulldown-1">pulldown/1</a></td><td>set pulldown to a pin.</td></tr><tr><td valign="top"><a href="#pullnone-1">pullnone/1</a></td><td>release pin mode from pullup pulldown.</td></tr><tr><td valign="top"><a href="#pullup-1">pullup/1</a></td><td>set pullup to a pin.</td></tr><tr><td valign="top"><a href="#read-1">read/1</a></td><td>read gpio value.</td></tr><tr><td valign="top"><a href="#set_active_low-2">set_active_low/2</a></td><td>set active low to a pin.</td></tr><tr><td valign="top"><a href="#set_int-2">set_int/2</a></td><td>set interrupt that fire when gpio's input or output status is chaned.</td></tr><tr><td valign="top"><a href="#set_pin_mode-2">set_pin_mode/2</a></td><td>set pin mode, in or out.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Starts the server.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>write value to gpio.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_digital-0"></a>

### all_digital/0 ###


<pre><code>
all_digital() -&gt; [1 | 0]
</code></pre>

<br></br>



get status list.


example: [0,1,1,0,0,0,0,0]
<a name="get_active_low-1"></a>

### get_active_low/1 ###


<pre><code>
get_active_low(PinNo) -&gt; <a href="#type-mode">mode()</a>
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>

get active low to from a pin.
<a name="pulldown-1"></a>

### pulldown/1 ###


<pre><code>
pulldown(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>

set pulldown to a pin.
<a name="pullnone-1"></a>

### pullnone/1 ###


<pre><code>
pullnone(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>

release pin mode from pullup pulldown.
<a name="pullup-1"></a>

### pullup/1 ###


<pre><code>
pullup(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>

set pullup to a pin.
<a name="read-1"></a>

### read/1 ###


<pre><code>
read(PinNo) -&gt; Val
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Val = <a href="#type-mode">mode()</a></code></li></ul>

read gpio value.
<a name="set_active_low-2"></a>

### set_active_low/2 ###


<pre><code>
set_active_low(PinNo, Mode) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="#type-mode">mode()</a></code></li></ul>


set active low to a pin.


Mode=1: active_lowを1に設定して、通電->0 解放->1 となるようにビット反転します
Mode=0: active_lowを0に設定して、通電->1 解放->0 となるようにします
<a name="set_int-2"></a>

### set_int/2 ###


<pre><code>
set_int(PinNo, Mode) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="#type-edge">edge()</a></code></li><li><code>Reason = term()</code></li></ul>

set interrupt that fire when gpio's input or output status is chaned.
<a name="set_pin_mode-2"></a>

### set_pin_mode/2 ###


<pre><code>
set_pin_mode(PinNo, Mode) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="#type-mode">mode()</a></code></li></ul>

set pin mode, in or out.
<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(X1::{PinNo, Mode} | {PinNo, Mode, Edge, Pull}) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="#type-mode">mode()</a></code></li><li><code>Edge = <a href="#type-edge">edge()</a></code></li><li><code>Pull = <a href="#type-pull">pull()</a></code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>

Starts the server
<a name="write-2"></a>

### write/2 ###


<pre><code>
write(PinNo, Val) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Val = <a href="#type-mode">mode()</a></code></li></ul>

write value to gpio.
