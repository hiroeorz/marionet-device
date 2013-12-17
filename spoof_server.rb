#!/usr/bin/env ruby


require "socket"

PORT = 9810
socket = TCPServer.open(PORT)

while true
  sock = socket.accept
  puts "accepted client"

  begin
    while true
      code_bin = sock.read(1)
      break if code_bin.nil?

      code = code_bin.unpack("H*")[0]
      p code
    
      case code
      when "90"
        body = sock.read(2)
        p body
      when "e0"
        body = sock.read(3)
        p body
      else
        puts "unknown code: #{code}"
        sock.close
        break
      end
    end
  rescue => e
    p e
  end
end
