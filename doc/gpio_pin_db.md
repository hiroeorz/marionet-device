

# Module gpio_pin_db #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#update_digital_pin-2">update_digital_pin/2</a></td><td>update digital pin.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(GpioList) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<ul class="definitions"><li><code>GpioList = [tuple()]</code></li></ul>


Starts the server

<a name="update_digital_pin-2"></a>

### update_digital_pin/2 ###


<pre><code>
update_digital_pin(GpioPinNo, PinState) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>GpioPinNo = non_neg_integer()</code></li><li><code>PinState = 0 | 1</code></li></ul>


update digital pin.


GpioPinNo is No of total pin in RaspberryPi GPIO.
