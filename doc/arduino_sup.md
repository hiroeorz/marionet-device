

# Module arduino_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Starts the supervisor.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(Config, EventHandlers) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Config = [tuple()]</code></li><li><code>EventHandlers = [atom()]</code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>


Starts the supervisor

