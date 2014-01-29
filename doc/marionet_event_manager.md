

# Module marionet_event_manager #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2014, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(Handler) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Handler = atom()</code></li><li><code>Pid = pid()</code></li><li><code>Error = atom()</code></li></ul>

Starts the server
