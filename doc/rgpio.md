

# Module rgpio #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Authors:__ HIROE Shin ([`shin@hibiscus`](mailto:shin@hibiscus)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_event_handler-1">add_event_handler/1</a></td><td>add event handler to gen_event.</td></tr><tr><td valign="top"><a href="#get_active_low-1">get_active_low/1</a></td><td>get active low from a pin.</td></tr><tr><td valign="top"><a href="#packed_status-0">packed_status/0</a></td><td>get status list that packed to 16bit unsigned integer.</td></tr><tr><td valign="top"><a href="#pulldown-1">pulldown/1</a></td><td>set pulldown to a pin.</td></tr><tr><td valign="top"><a href="#pullnone-1">pullnone/1</a></td><td>release pin mode from pullup pulldown.</td></tr><tr><td valign="top"><a href="#pullup-1">pullup/1</a></td><td>set pullup to a pin.</td></tr><tr><td valign="top"><a href="#read-1">read/1</a></td><td>read gpio value.</td></tr><tr><td valign="top"><a href="#set_active_low-2">set_active_low/2</a></td><td>set active low to a pin.</td></tr><tr><td valign="top"><a href="#set_int-2">set_int/2</a></td><td>set interrupt that fire when gpio's input or output status is chaned.</td></tr><tr><td valign="top"><a href="#set_pin_mode-2">set_pin_mode/2</a></td><td>set pin mode, in or out.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#status-0">status/0</a></td><td>get status list.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>write value to gpio.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_event_handler-1"></a>

### add_event_handler/1 ###


<pre><code>
add_event_handler(Module) -&gt; ok | {'EXIT', Reason} | term()
</code></pre>

<ul class="definitions"><li><code>Module = atom()</code></li><li><code>Reason = term()</code></li></ul>


add event handler to gen_event.


event fired when gpio's input or output status is chaned.
<a name="get_active_low-1"></a>

### get_active_low/1 ###


<pre><code>
get_active_low(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>


get active low from a pin.


Mode=0: 通電時のread/1の結果は 通電->1 解放->0 (デフォルト)
Mode=1: 通電時のread/1の結果は 通電->0 解放->1
<a name="packed_status-0"></a>

### packed_status/0 ###


<pre><code>
packed_status() -&gt; [non_neg_integer()]
</code></pre>

<br></br>



get status list that packed to 16bit unsigned integer.



example: [16390,0]


主にサーバーへの送信データサイズを減らす為の関数。
各pinの状態を16点毎にまとめて16bit整数のリストを返します。
RaspberryPiのGPIO点数では必要ありませんが、PLCなどもっと点数の多いものと一緒に
使われたときに型を合わせる用。
16ビットに足りない分は0で埋めます。
<a name="pulldown-1"></a>

### pulldown/1 ###


<pre><code>
pulldown(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>


set pulldown to a pin.


RaspberryPi内蔵のプルダウン抵抗を用いてpinをプルダウン有りに設定します
入力無しの状態で常時GND接地の0.0Vとなり、3.3Vと短絡された場合のみ1となります
<a name="pullnone-1"></a>

### pullnone/1 ###


<pre><code>
pullnone(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>


release pin mode from pullup pulldown.


RaspberryPi内蔵のプルアップ、プルダウン抵抗を用いません
入力無しの状態では不安定な電圧となり、外部回路でプルアップまたはプルダウンが必要です
<a name="pullup-1"></a>

### pullup/1 ###


<pre><code>
pullup(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>


set pullup to a pin.


RaspberryPi内蔵のプルアップ抵抗を用いてpinをプルアップ有りに設定します
入力無しの状態で常時3.3Vの電圧がかかり、GNDと接地された場合のみ0となります
<a name="read-1"></a>

### read/1 ###


<pre><code>
read(PinNo) -&gt; Val
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Val = non_neg_integer()</code></li></ul>

read gpio value.
<a name="set_active_low-2"></a>

### set_active_low/2 ###


<pre><code>
set_active_low(PinNo, Mode) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="rgpio_pin.md#type-mode">rgpio_pin:mode()</a></code></li></ul>


set active low to a pin.


Mode=1: active_lowを1に設定して、通電->0 解放->1 となるようにビット反転します
Mode=0: active_lowを0に設定して、通電->1 解放->0 となるようにします
<a name="set_int-2"></a>

### set_int/2 ###


<pre><code>
set_int(PinNo, Mode) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="rgpio_pin.md#type-edge">rgpio_pin:edge()</a></code></li><li><code>Reason = term()</code></li></ul>

set interrupt that fire when gpio's input or output status is chaned.
<a name="set_pin_mode-2"></a>

### set_pin_mode/2 ###


<pre><code>
set_pin_mode(PinNo, Mode) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="rgpio_pin.md#type-edge">rgpio_pin:edge()</a></code></li></ul>

set pin mode, in or out.
<a name="start-0"></a>

### start/0 ###

`start() -> any()`


<a name="status-0"></a>

### status/0 ###


<pre><code>
status() -&gt; [1 | 0]
</code></pre>

<br></br>



get status list.


example: [0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0]
<a name="write-2"></a>

### write/2 ###


<pre><code>
write(PinNo, Val) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Val = non_neg_integer()</code></li></ul>

write value to gpio.
