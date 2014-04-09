require "marionet"

io = MarioNet::IO.new
gpio = MarioNet::GPIO.new(io)
arduino = MarioNet::Arduino.new(io)

#
# LED status change when analog data received from device "plc".
#
io.remote(device:"plc", type:"analog") do |no, val|
  # some task
end

#
# plc analog(2) received.
#
io.remote(device:"plc", type:"analog", no:0) do |no, val|
  gpio.digital_read(25) do |state|
    new_state = (state.zero?) ? 1 : 0
    puts "plc: #{state} -> #{new_state}"
    gpio.digital_write(25, new_state)
  end
end

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

#
# galileo analog(0) -> control servo (connected to arduino digital pin 9).
#
io.remote(device:"galileo", type:"analog", no:0) do |no, val|
  servo_pin = 9
  angle = val * 180 / 4096
  arduino.analog_write(servo_pin, angle) { puts "servo: -> #{angle}" }

  gpio.digital_read(24) do |state|
    new_state = (state.zero?) ? 1 : 0
    puts "galileo: #{state} -> #{new_state}"
    gpio.digital_write(24, new_state)
  end

end

#
# galileo digital(all)
#
io.remote(device:"galileo", type:"digital") do |no, digital_array|
  puts "galileo digital: #{val}"
end

io.start
