require "marionet"

io = MarioNet::IO.new
gpio = MarioNet::GPIO.new(io)
arduino = MarioNet::Arduino.new(io)

LED_GALILEO = 24
LED_PLC = 25

###########################################################################
# Device "plc"
###########################################################################

#
# LED status change when analog data received from device "plc".
#
io.remote(device:"plc", type:"analog") do |no, val|
  # some task
end

#
# plc analog(1) received.
#
io.remote(device:"plc", type:"analog", no:1) do |no, val|
   blink_led(gpio, LED_PLC)
end

#
# plc analog(2) received.
#
io.remote(device:"plc", type:"analog", no:2) do |no, val|
   blink_led(gpio, LED_PLC)
end

#
# plc analog(3) received.
#
io.remote(device:"plc", type:"analog", no:3) do |no, val|
   blink_led(gpio, LED_PLC)
end

###########################################################################
# Device "pi002"
###########################################################################

#
# pi002 analog(all) received.
#
io.remote(device:"pi002", type:"analog") do |no, val|
  # some task
end

#
# galileo analog(all) received.
#
io.remote(device:"galileo", type:"analog") do |no, val|
  # some task
end

###########################################################################
# Device "galileo"
###########################################################################

#
# galileo analog(0) -> control servo (connected to arduino digital pin 9).
#
io.remote(device:"galileo", type:"analog", no:0) do |no, val|
  servo_pin = 9
  angle = val * 180 / 4096
  arduino.analog_write(servo_pin, angle) { puts "servo: -> #{angle}" }
  blink_led(gpio, LED_GALILEO)
end

#
# galileo digital(all)
#
io.remote(device:"galileo", type:"digital") do |no, digital|
  puts "galileo digital: #{digital}"
end

###########################################################################
# Private
###########################################################################

def blink_led(gpio, led)
  gpio.digital_read(led) do |state|
    new_state = (state.zero?) ? 1 : 0
    gpio.digital_write(led, new_state)
  end
end

###########################################################################
# Start Application
###########################################################################

io.start

