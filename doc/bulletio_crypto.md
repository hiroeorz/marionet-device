

# Module bulletio_crypto #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decrypt-2">decrypt/2</a></td><td>decrypto  chipertext -> palintext using AES Counter mode.</td></tr><tr><td valign="top"><a href="#encrypt-2">encrypt/2</a></td><td>encrypto palintext -> chipertext using AES Counter mode.</td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td>init crypto state.</td></tr><tr><td valign="top"><a href="#ivec-0">ivec/0</a></td><td>create IVector.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decrypt-2"></a>

### decrypt/2 ###


<pre><code>
decrypt(ChiperText, State) -&gt; {NewState, PlainText}
</code></pre>

<ul class="definitions"><li><code>ChiperText = iodata()</code></li><li><code>State = <a href="crypto.md#type-opaque">crypto:opaque()</a></code></li><li><code>NewState = <a href="cryoto.md#type-opaque">cryoto:opaque()</a></code></li><li><code>PlainText = binary()</code></li></ul>

decrypto  chipertext -> palintext using AES Counter mode.
<a name="encrypt-2"></a>

### encrypt/2 ###


<pre><code>
encrypt(PlainText, State) -&gt; {NewState, ChiperText}
</code></pre>

<ul class="definitions"><li><code>PlainText = iodata()</code></li><li><code>State = <a href="crypto.md#type-opaque">crypto:opaque()</a></code></li><li><code>NewState = <a href="crypto.md#type-opaque">crypto:opaque()</a></code></li><li><code>ChiperText = binary()</code></li></ul>

encrypto palintext -> chipertext using AES Counter mode.
<a name="init-2"></a>

### init/2 ###


<pre><code>
init(AESKey, IVec) -&gt; State
</code></pre>

<ul class="definitions"><li><code>AESKey = iodata()</code></li><li><code>IVec = binary()</code></li><li><code>State = <a href="crypto.md#type-opaque">crypto:opaque()</a></code></li></ul>

init crypto state.
<a name="ivec-0"></a>

### ivec/0 ###

`ivec() -> any()`

create IVector.
