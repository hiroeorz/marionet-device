

# Module marionet_data #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2014, HIROE Shin

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#pack-1">pack/1</a></td><td>format using msgpack.</td></tr><tr><td valign="top"><a href="#pack_io-5">pack_io/5</a></td><td>Format analog status data.</td></tr><tr><td valign="top"><a href="#unpack-1">unpack/1</a></td><td></td></tr><tr><td valign="top"><a href="#unpack_command-1">unpack_command/1</a></td><td>Parse command that send from other application.</td></tr><tr><td valign="top"><a href="#unpack_io-1">unpack_io/1</a></td><td>Parse analog status data.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="pack-1"></a>

### pack/1 ###


<pre><code>
pack(Obj::term()) -&gt; binary()
</code></pre>

<br></br>


format using msgpack
<a name="pack_io-5"></a>

### pack_io/5 ###


<pre><code>
pack_io(Type, DeviceId, AnalogNo, Val, Opts) -&gt; Payload
</code></pre>

<ul class="definitions"><li><code>Type = binary()</code></li><li><code>DeviceId = binary()</code></li><li><code>AnalogNo = non_neg_integer()</code></li><li><code>Val = non_neg_integer()</code></li><li><code>Opts = [tuple()]</code></li><li><code>Payload = binary()</code></li></ul>

Format analog status data.
<a name="unpack-1"></a>

### unpack/1 ###


<pre><code>
unpack(Bin::binary()) -&gt; term()
</code></pre>

<br></br>



<a name="unpack_command-1"></a>

### unpack_command/1 ###


<pre><code>
unpack_command(Payload) -&gt; {UUID, Command, Args}
</code></pre>

<ul class="definitions"><li><code>Payload = binary()</code></li><li><code>UUID = binary()</code></li><li><code>Command = binary()</code></li><li><code>Args = [term()]</code></li></ul>

Parse command that send from other application.
<a name="unpack_io-1"></a>

### unpack_io/1 ###


<pre><code>
unpack_io(Payload) -&gt; {Type, DeviceId, No, Val, Opts}
</code></pre>

<ul class="definitions"><li><code>Payload = binary()</code></li><li><code>Type = binary()</code></li><li><code>DeviceId = binary()</code></li><li><code>No = non_neg_integer()</code></li><li><code>Val = non_neg_integer()</code></li><li><code>Opts = [tuple()]</code></li></ul>

Parse analog status data.
