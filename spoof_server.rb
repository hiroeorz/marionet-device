#!/usr/bin/env ruby


require "socket"

PORT = 9810
socket = TCPServer.open(PORT)

while true
  sock = socket.accept
  puts "accepted client"
  code_bin = sock.read(1)
  code = code_bin.unpack("H*")[0]
  
  case code
  when 0x90
    body = sock.read(2)
    p body
  when 0xE0
    body = sock.read(3)
    p body
  else
    sock.close
  end
end
