

# Module marionet_config #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2014, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get application config.</td></tr><tr><td valign="top"><a href="#get_all_keys-0">get_all_keys/0</a></td><td>Get all config keys.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>Set application config.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Key) -&gt; Val | undefined
</code></pre>

<ul class="definitions"><li><code>Key = term()</code></li><li><code>Val = term()</code></li></ul>

Get application config
<a name="get_all_keys-0"></a>

### get_all_keys/0 ###


<pre><code>
get_all_keys() -&gt; [{atom(), term()}]
</code></pre>

<br></br>


Get all config keys.
<a name="set-2"></a>

### set/2 ###


<pre><code>
set(Key, Val) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Key = term()</code></li><li><code>Val = term()</code></li></ul>

Set application config
<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(FileName) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<br></br>



Starts the server

