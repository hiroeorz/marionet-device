# -*- coding: utf-8 -*-

require "json"
require "em-zeromq"
require "uuidtools"

module MarioNet
  class Task

    # URI to ZeroMQ binded port.
    ZMQ_SUB_URI = "tcp://127.0.0.1:6788"
    ZMQ_REQ_URI = "tcp://127.0.0.1:6789"

    def initialize
      @req = nil
      @callback = nil
    end

    def event_queue
      @event_queue ||= {}
    end

    def context
      @sub_context ||= EM::ZeroMQ::Context.new(1)
    end

    def callback
      @callback ||= {}
    end

    def event(ev, &block)
      raise ArgumentError.new("Block must given!") unless block_given?
      event_queue[ev] = block
    end

    def call(ev)
      type = ev["type"]
      id   = ev["id"]
      no   = ev["no"]
      val  = ev["val"]
      opts = ev["opts"]

      task1 = event_queue[{:id => id, :type => type, :no => no}]
      task1.call(val, ev) if task1

      task2 = event_queue[{:id => id, :type => type}]
      task2.call(val, ev) if task2
    end

    def send_command(obj, &block)
      obj[:uuid] = uuid

      if @req.send_msg(obj.to_json)
        callback[uuid] = block if block_given?
      end
    end

    def start
      EM.run {
        print "connecting to request port ( #{ZMQ_REQ_URI} )..."
        @req = context.socket(ZMQ::REQ)
        @req.connect(ZMQ_REQ_URI); puts "done"

        print "connecting to subscribe port ( #{ZMQ_SUB_URI} )..."
        sub = context.socket(ZMQ::SUB)
        sub.connect(ZMQ_SUB_URI); puts "done"
        sub.subscribe
        
        @req.on(:message) { |part|
          if !callback.empty?
            result = JSON.parse(part.copy_out_string)

            if result["error"]
              raise MarioNet::CommandError.new(result["error"])
            else
              p result
              p [:callback, callback]
              callback.shift.call(result["return"])
            end
          end
        }

        sub.on(:message) { |part|
          self.call(JSON.parse(part.copy_out_string))
        }
      }
    end

    private

    def uuid
      UUIDTools::UUID.random_create
    end

  end

  class GPIO

    def initialize(task)
      @task = task
    end
    
    def digital_write(pin, val, &block)
      @task.send_command({command:"gpio_digital_write", args:[pin, val]},
                         &block)
    end

    def digital_read(pin, &block)
      @task.send_command({command:"gpio_digital_read", args:[pin]}, 
                         &block)
    end

  end

  class CommandError < StandardError
  end

end

task = MarioNet::Task.new
gpio = MarioNet::GPIO.new(task)

task.event(id:"plc", type:"ai") do |val, ev|
  p [val, ev]
  
#  pin = ev["no"]

#  gpio.digital_read(4) do |state|
#    p [:read, state]
#    new_val = (state.zero?) ? 1 : 0
#    gpio.digital_write(24, new_val) { |result| p [:write, result] }
#  end
end

task.event(id:"plc", type:"ai", no:2) do |val, ev|
  p [val, ev]
end

task.event(id:"pi002", type:"ai") do |val, ev|
  p [val, ev]
end

task.event(id:"galileo", type:"ai") do |val, ev|
  p [val, ev]
end

task.start
