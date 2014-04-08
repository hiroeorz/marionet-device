require "em-zeromq"

# URI to ZeroMQ binded port.
ZMQ_URI = "tcp://127.0.0.1:6788"

zmq = EM::ZeroMQ::Context.new(1)
puts "zmq context created."

EM.run {
  print "connecting to #{URI}..."
  sub = zmq.socket(ZMQ::SUB)
  sub.connect(ZMQ_URI); puts "done"
  sub.subscribe       ; puts "subscribe start."

  sub.on(:message) { |part|
    p part.copy_out_string
  }
}
